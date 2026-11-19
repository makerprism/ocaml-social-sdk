(** LinkedIn API v2 Provider
    
    This implementation supports OAuth 2.0 for personal posting.
    Tokens expire after 60 days (5,184,000 seconds).
    
    IMPORTANT: Programmatic token refresh is only available for LinkedIn Partner Program apps.
    Standard apps using "Sign In with LinkedIn" or "Share on LinkedIn" products DO NOT
    have access to programmatic refresh - users must re-authorize through OAuth flow.
*)

open Social_core

(* LinkedIn "Little Text Format" escaping for the commentary field.

   The Posts API treats [commentary] as Little Text Format, in which
   the characters [\ | ( ) [ ] { } @ # * ~ < > _] are reserved and must
   be backslash-escaped unless they form a recognised element. Sending
   them raw can corrupt a post (LinkedIn is known to truncate content at
   an unescaped parenthesis) and prevents inline mention annotations from
   parsing.

   [escape_little_text_format] escapes stray reserved characters while
   preserving the three elements we and our users rely on, matched as
   whole spans and passed through:
     - mention annotations  [@\[Name\](urn:li:organization:123)]
     - hashtags             [#word]
     - URLs                 [https://...]
   Inside a mention annotation the structural [@ \[ \] ( )] and the URN
   stay literal; only the display name is escaped (a name may itself
   contain reserved characters). This keeps hashtags clickable and URLs
   intact, so the change is safe for posts that contain no mentions. *)
let little_text_format_reserved = "\\|()[]{}@#*~<>_"

let escape_ltf_segment s =
  let buf = Buffer.create (String.length s + 8) in
  String.iter
    (fun c ->
      if String.contains little_text_format_reserved c then
        Buffer.add_char buf '\\';
      Buffer.add_char buf c)
    s;
  Buffer.contents buf

(* Single whole-span matcher for the elements we preserve, in
   precedence order. One regex (rather than a second one to re-split the
   annotation) keeps the annotation grammar in a single place. Groups:
     1 = annotation display name   2 = annotation URN
     3 = URL                       4 = hashtag
   The URN entity-type class allows digits/underscore (e.g.
   [urn:li:fsd_profile:...]), not just letters. *)
let ltf_preserve_re =
  Re.Pcre.regexp
    "@\\[([^\\]]*)\\]\\((urn:li:[a-zA-Z0-9_]+:[^)]*)\\)|(https?://[^\\s]+)|(#[A-Za-z0-9_]+)"

(* Trailing characters that should not be treated as part of a URL: a
   linkifier (and LinkedIn) stops the link before sentence punctuation
   and closing brackets. Stripping them and escaping the tail keeps the
   link target clean and prevents an unescaped reserved [)]/[]] (the
   greedy-match form of the very truncation-at-paren bug we fix). *)
let url_trailing_punct = ")].,;:!?"

let strip_url_trailing_punct url =
  let n = String.length url in
  let rec back i =
    if i > 0 && String.contains url_trailing_punct url.[i - 1] then back (i - 1)
    else i
  in
  let keep = back n in
  (String.sub url 0 keep, String.sub url keep (n - keep))

let escape_little_text_format text =
  let buf = Buffer.create (String.length text + 16) in
  let pos = ref 0 in
  List.iter
    (fun g ->
      let mstart, mend = Re.Group.offset g 0 in
      if mstart > !pos then
        Buffer.add_string buf
          (escape_ltf_segment (String.sub text !pos (mstart - !pos)));
      (if Re.Group.test g 1 then
         (* Mention annotation: escape only the display name; keep the
            structure and URN literal. *)
         Buffer.add_string buf
           (Printf.sprintf "@[%s](%s)"
              (escape_ltf_segment (Re.Group.get g 1))
              (Re.Group.get g 2))
       else if Re.Group.test g 3 then (
         (* URL: pass the link target through unescaped, but peel off and
            escape any trailing punctuation the greedy match swallowed. *)
         let core, tail = strip_url_trailing_punct (Re.Group.get g 3) in
         Buffer.add_string buf core;
         Buffer.add_string buf (escape_ltf_segment tail))
       else
         (* Hashtag: pass through so it stays a clickable hashtag. *)
         Buffer.add_string buf (Re.Group.get g 0));
      pos := mend)
    (Re.all ltf_preserve_re text);
  if !pos < String.length text then
    Buffer.add_string buf
      (escape_ltf_segment (String.sub text !pos (String.length text - !pos)));
  Buffer.contents buf

(** Profile information from userinfo endpoint (OpenID Connect) *)
type profile_info = {
  sub: string;                    (** Subject identifier (user ID) *)
  name: string option;            (** User's full name *)
  given_name: string option;      (** First name *)
  family_name: string option;     (** Last name *)
  picture: string option;         (** Profile picture URL *)
  email: string option;           (** Email address *)
  email_verified: bool option;    (** Email verification status *)
  locale: string option;
      (** User's locale, e.g. ["en-US"]. Documented as a string; an object
          [{"language": "en", "country": "US"}] is read as ["en-US"]. *)
}

(** Parses a [GET /v2/userinfo] body
    ({{:https://learn.microsoft.com/en-us/linkedin/consumer/integrations/self-serve/sign-in-with-linkedin-v2}}:
    [sub], [name], [given_name], [family_name], [picture], [locale],
    optional [email] / [email_verified]). [Error] without a non-empty
    [sub]. A member of an unexpected type reads as [None] instead of
    failing the whole profile. *)
let parse_userinfo body : (profile_info, string) Stdlib.result =
  match Yojson.Basic.from_string body with
  | exception Yojson.Json_error message -> Stdlib.Error ("userinfo response is not JSON: " ^ message)
  | `Assoc fields -> (
      let text_of = function
        | Some (`String value) when String.trim value <> "" -> Some value
        | Some (`String _ | `Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _) | None -> None
      in
      let text key = text_of (List.assoc_opt key fields) in
      let locale =
        match List.assoc_opt "locale" fields with
        | Some (`Assoc parts) -> (
            match text_of (List.assoc_opt "language" parts), text_of (List.assoc_opt "country" parts) with
            | Some language, Some country -> Some (language ^ "-" ^ country)
            | Some language, None -> Some language
            | None, (Some _ | None) -> None)
        | (Some (`String _ | `Null | `Bool _ | `Int _ | `Float _ | `List _) | None) as value -> text_of value
      in
      match text "sub" with
      | None -> Stdlib.Error "userinfo response has no sub"
      | Some sub ->
          Stdlib.Ok
            { sub;
              name = text "name";
              given_name = text "given_name";
              family_name = text "family_name";
              picture = text "picture";
              email = text "email";
              email_verified =
                (match List.assoc_opt "email_verified" fields with
                 | Some (`Bool verified) -> Some verified
                 | Some (`Null | `Int _ | `Float _ | `String _ | `List _ | `Assoc _) | None -> None);
              locale })
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> Stdlib.Error "userinfo response is not an object"

(** The SDK error for a failed userinfo read, with the package's usual
    mapping: 401 [Token_expired], 403 [Insufficient_permissions [openid;
    profile]], 429 [Rate_limited], otherwise [Api_error] with the body. *)
let userinfo_error ~status_code body =
  let message =
    match Yojson.Basic.from_string body with
    | `Assoc fields -> (
        match List.assoc_opt "message" fields with
        | Some (`String message) -> Some message
        | Some (`Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _) | None -> None)
    | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> None
    | exception Yojson.Json_error _ -> None
  in
  match status_code with
  | 401 -> Error_types.Auth_error Error_types.Token_expired
  | 403 ->
      Error_types.Auth_error
        (Error_types.Insufficient_permissions { required = [ "openid"; "profile" ]; platform_message = message })
  | 429 -> Error_types.make_rate_limited ()
  | _ ->
      Error_types.make_api_error ~platform:Platform_types.LinkedIn ~status_code
        ~message:(Option.value message ~default:"API error") ~raw_response:body ()

(** OAuth 2.0 module for LinkedIn
    
    LinkedIn uses standard OAuth 2.0 WITHOUT PKCE support.
    Access tokens expire after 60 days and typically cannot be programmatically refreshed
    unless you are enrolled in the LinkedIn Partner Program.
    
    IMPORTANT: LinkedIn has TWO separate OAuth products:
    1. "Sign In with LinkedIn using OpenID Connect" + "Share on LinkedIn" 
       - For personal profile posting
       - Standard apps, no programmatic refresh
    2. "Community Management API" 
       - For LinkedIn Page posting
       - Requires SEPARATE app registration
       - Cannot be combined with other products
    
    Required environment variables (or pass directly to functions):
    - LINKEDIN_CLIENT_ID: OAuth 2.0 Client ID from LinkedIn Developer Portal
    - LINKEDIN_CLIENT_SECRET: OAuth 2.0 Client Secret
    - LINKEDIN_REDIRECT_URI: Registered callback URL
*)
module OAuth = struct
  let has_surrounding_whitespace value = value <> String.trim value
  let is_blank value = String.trim value = ""

  let normalize_expires_in seconds =
    if seconds < 0 then 0 else if seconds > 31_536_000 then 31_536_000 else seconds

  let parse_ipv4_host host =
    let octets = String.split_on_char '.' host in
    match octets with
    | [a; b; c; d] ->
        (try
           let ai = int_of_string a in
           let bi = int_of_string b in
           let ci = int_of_string c in
           let di = int_of_string d in
           let in_range x = x >= 0 && x <= 255 in
           if in_range ai && in_range bi && in_range ci && in_range di then
             Some (ai, bi, ci, di)
           else
             None
         with _ -> None)
    | _ -> None

  let is_safe_remote_media_url url =
    try
      let uri = Uri.of_string (String.trim url) in
      match Uri.scheme uri, Uri.host uri with
      | (Some ("http" | "https"), Some host) ->
          let lower_host = String.lowercase_ascii host in
          if lower_host = "localhost" || lower_host = "::1" || String.ends_with ~suffix:".local" lower_host then
            false
          else
            (match parse_ipv4_host lower_host with
             | Some (a, b, _, _) ->
                 not (
                   a = 10
                   || a = 127
                   || (a = 169 && b = 254)
                   || (a = 172 && b >= 16 && b <= 31)
                   || (a = 192 && b = 168)
                 )
             | None -> true)
      | _ -> false
    with _ -> false

  (** Scope definitions for LinkedIn API v2 *)
  module Scopes = struct
    (** Scopes required for read-only operations (profile info) *)
    let read = ["openid"; "profile"; "email"]
    
    (** Scopes required for personal posting (includes read scopes) *)
    let write = ["openid"; "profile"; "email"; "w_member_social"]
    
    (** All available scopes for personal posting apps
        
        Note: Organization/page scopes require separate Community Management API app *)
    let all = [
      "openid"; "profile"; "email"; "w_member_social";
      (* Organization scopes - require Community Management API product *)
      "r_organization_admin"; "w_organization_social"; "rw_organization_admin"
    ]
    
    (** Scopes for LinkedIn Company Pages (requires Community Management API) *)
    let organization = ["r_organization_admin"; "w_organization_social"; "rw_organization_admin"]
    
    (** Operations that can be performed with LinkedIn API *)
    type operation = 
      | Post_text
      | Post_media
      | Post_video
      | Read_profile
      | Read_posts
      | Delete_post
      | Manage_pages  (** LinkedIn Pages - requires separate Community Management API app *)
    
    (** Get scopes required for specific operations *)
    let for_operations ops =
      let base = ["openid"; "profile"; "email"] in
      if List.exists (fun o -> o = Post_text || o = Post_media || o = Post_video || o = Delete_post) ops
      then base @ ["w_member_social"]
      else if List.exists (fun o -> o = Manage_pages) ops
      then base @ ["r_organization_admin"; "w_organization_social"]
      else base
  end
  
  (** Platform metadata for LinkedIn OAuth *)
  module Metadata = struct
    (** LinkedIn does NOT support PKCE *)
    let supports_pkce = false
    
    (** LinkedIn Partner Program apps support refresh; standard apps do NOT *)
    let supports_refresh = false  (* Standard apps: false; Partner Program: true *)
    
    (** LinkedIn access tokens expire after 60 days (5,184,000 seconds) *)
    let token_lifetime_seconds = Some 5184000
    
    (** Recommended buffer before expiry (7 days) for reconnection prompts *)
    let refresh_buffer_seconds = 604800
    
    (** Maximum retry attempts for token refresh (Partner Program only) *)
    let max_refresh_attempts = 5
    
    (** Authorization endpoint *)
    let authorization_endpoint = "https://www.linkedin.com/oauth/v2/authorization"
    
    (** Token endpoint *)
    let token_endpoint = "https://www.linkedin.com/oauth/v2/accessToken"
    
    (** Token introspection endpoint (Partner Program only) *)
    let introspection_endpoint = "https://www.linkedin.com/oauth/v2/introspectToken"
  end
  
  (** Generate authorization URL for LinkedIn OAuth 2.0 flow
      
      Note: LinkedIn does NOT support PKCE, so no code_challenge parameter.
      
      @param client_id OAuth 2.0 Client ID
      @param redirect_uri Registered callback URL
      @param state CSRF protection state parameter (should be stored and verified on callback)
      @param scopes OAuth scopes to request (defaults to Scopes.write)
      @return Full authorization URL to redirect user to
  *)
  let get_authorization_url ~client_id ~redirect_uri ~state ?(scopes=Scopes.write) () =
    let scope_str = String.concat " " scopes in
    let params = [
      ("response_type", "code");
      ("client_id", client_id);
      ("redirect_uri", redirect_uri);
      ("state", state);
      ("scope", scope_str);
    ] in
    let query = Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) params) in
    Printf.sprintf "%s?%s" Metadata.authorization_endpoint query
  
  (** Make functor for OAuth operations that need HTTP client
      
      This separates the pure functions (URL generation, scope selection) from
      functions that need to make HTTP requests (token exchange, refresh).
  *)
  module Make (Http : HTTP_CLIENT) = struct
    (** Exchange authorization code for access token
        
        Note: LinkedIn does NOT support PKCE, so no code_verifier parameter.
        
        @param client_id OAuth 2.0 Client ID
        @param client_secret OAuth 2.0 Client Secret
        @param redirect_uri Registered callback URL (must match authorization request)
        @param code Authorization code from callback
        @param on_success Continuation receiving [(credentials, granted_scope)]
        @param on_error Continuation receiving error message
    *)
    let exchange_code ~client_id ~client_secret ~redirect_uri ~code on_success on_error =
      let client_id = String.trim client_id in
      let client_secret = String.trim client_secret in
      if client_id = "" || client_secret = "" then
        on_error "LinkedIn OAuth credentials not configured"
      else if has_surrounding_whitespace redirect_uri then
        on_error "LinkedIn redirect URI must not contain leading or trailing whitespace"
      else if is_blank redirect_uri then
        on_error "LinkedIn redirect URI is required"
      else if has_surrounding_whitespace code then
        on_error "LinkedIn authorization code must not contain leading or trailing whitespace"
      else if is_blank code then
        on_error "LinkedIn authorization code is required"
      else
      let body = Social_core.form_urlencode_kvs [
        ("grant_type", "authorization_code");
        ("code", code);
        ("redirect_uri", redirect_uri);
        ("client_id", client_id);
        ("client_secret", client_secret);
      ] in
      let url = Metadata.token_endpoint in
      let headers = [
        ("Content-Type", "application/x-www-form-urlencoded");
      ] in

      let parse_error body =
        try
          let json = Yojson.Basic.from_string body in
          let open Yojson.Basic.Util in
          let error = json |> member "error" |> to_string_option in
          let error_desc = json |> member "error_description" |> to_string_option in
          let error_msg =
            match error, error_desc with
            | Some err, Some desc -> Printf.sprintf "%s: %s" err desc
            | Some err, None -> err
            | None, _ -> body
          in
          (error, error_msg)
        with _ ->
          (None, body)
      in

      Http.post ~headers ~body url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let access_token = json |> member "access_token" |> to_string |> String.trim in
              if access_token = "" then
                on_error "LinkedIn OAuth response contains empty access token"
              else
              let refresh_token = 
                try
                  let parsed = json |> member "refresh_token" |> to_string |> String.trim in
                  if parsed = "" then None else Some parsed
                with _ -> None in
              
              (* LinkedIn always includes expires_in in seconds *)
              let expires_in = normalize_expires_in (json |> member "expires_in" |> to_int) in
              let expires_at = 
                let now = Ptime_clock.now () in
                match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
                | Some exp -> Some (Ptime.to_rfc3339 exp)
                | None -> None in
              
              let token_type_str = json |> member "token_type" |> to_string_option |> Option.value ~default:"Bearer" in

              let scope = json |> member "scope" |> to_string_option in
              let creds : credentials = {
                access_token;
                refresh_token;
                expires_at;
                auth_type = auth_type_of_string token_type_str;
                scope;
              } in
              on_success (creds, scope)
            with e ->
              on_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))
          else
            let _, error_msg = parse_error response.body in
            on_error (Printf.sprintf "LinkedIn OAuth exchange failed (%d): %s" response.status error_msg))
        on_error
    
    (** Refresh access token (LinkedIn Partner Program ONLY)
        
        IMPORTANT: Standard LinkedIn apps (Sign In + Share on LinkedIn products)
        do NOT support programmatic token refresh. This function will fail
        unless you are enrolled in the LinkedIn Partner Program.
        
        For standard apps, users must re-authorize through the OAuth flow
        when their token expires (every 60 days).
        
        @param client_id OAuth 2.0 Client ID
        @param client_secret OAuth 2.0 Client Secret
        @param refresh_token The refresh token from previous exchange
        @param on_success Continuation receiving new credentials
        @param on_error Continuation receiving error message
    *)
    let refresh_token ~client_id ~client_secret ~refresh_token on_success on_error =
      let client_id = String.trim client_id in
      let client_secret = String.trim client_secret in
      if client_id = "" || client_secret = "" then
        on_error "LinkedIn OAuth credentials not configured"
      else if has_surrounding_whitespace refresh_token then
        on_error "LinkedIn refresh token must not contain leading or trailing whitespace"
      else if is_blank refresh_token then
        on_error "LinkedIn refresh token is required"
      else
      let body = Social_core.form_urlencode_kvs [
        ("grant_type", "refresh_token");
        ("refresh_token", refresh_token);
        ("client_id", client_id);
        ("client_secret", client_secret);
      ] in

      let headers = [
        ("Content-Type", "application/x-www-form-urlencoded");
      ] in

      Http.post ~headers ~body Metadata.token_endpoint
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let access_token = json |> member "access_token" |> to_string |> String.trim in
              if access_token = "" then
                on_error "LinkedIn OAuth response contains empty access token"
              else
              let new_refresh = 
                try
                  let parsed = json |> member "refresh_token" |> to_string |> String.trim in
                  if parsed = "" then Some refresh_token else Some parsed
                with _ -> Some refresh_token in
              let expires_in = normalize_expires_in (json |> member "expires_in" |> to_int) in
              let expires_at = 
                let now = Ptime_clock.now () in
                match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
                | Some exp -> Some (Ptime.to_rfc3339 exp)
                | None -> None in
              let token_type_str = json |> member "token_type" |> to_string_option |> Option.value ~default:"Bearer" in
              let scope = json |> member "scope" |> to_string_option in
              let creds : credentials = {
                access_token;
                refresh_token = new_refresh;
                expires_at;
                auth_type = auth_type_of_string token_type_str;
                scope;
              } in
              on_success creds
            with e ->
              on_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))
          else
            (* Parse error response for better error messages *)
            let error_msg = 
              try
                let json = Yojson.Basic.from_string response.body in
                let open Yojson.Basic.Util in
                let error = json |> member "error" |> to_string_option |> Option.value ~default:"unknown" in
                let error_desc = json |> member "error_description" |> to_string_option in
                match error, error_desc with
                | "unauthorized_client", _ | "invalid_grant", _ ->
                    "Programmatic refresh not available - your app is not enrolled in LinkedIn Partner Program. User must re-authorize."
                | _, Some desc -> Printf.sprintf "%s: %s" error desc
                | _, None -> error
              with _ -> response.body
            in
            on_error (Printf.sprintf "Token refresh failed (%d): %s" response.status error_msg))
        on_error

    (** The OpenID Connect profile of a token, [GET /v2/userinfo], before
        any account is stored (OAuth callback). Needs [openid] and
        [profile]; [email] also needs [email]. *)
    let get_userinfo ~access_token on_result =
      Http.get
        ~headers:[ ("Authorization", "Bearer " ^ access_token) ]
        "https://api.linkedin.com/v2/userinfo"
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            match parse_userinfo response.body with
            | Stdlib.Ok profile -> on_result (Stdlib.Ok profile)
            | Stdlib.Error message -> on_result (Stdlib.Error (Error_types.Internal_error message))
          else on_result (Stdlib.Error (userinfo_error ~status_code:response.status response.body)))
        (fun err -> on_result (Stdlib.Error (Error_types.Network_error (Error_types.Connection_failed err))))
  end
end

(** {1 Response Types} *)

(** Paging metadata for paginated responses *)
type paging = {
  start: int;        (** Zero-based index of first result *)
  count: int;        (** Number of results in this response *)
  total: int option; (** Total number of results available (if known) *)
}

(** Collection response with pagination *)
type 'a collection_response = {
  elements: 'a list;         (** List of entities in this page *)
  paging: paging option;      (** Paging metadata *)
  metadata: Yojson.Basic.t option; (** Optional response metadata *)
}

(** Post/share information *)
type post_info = {
  id: string;                     (** Post URN/ID *)
  author: string;                 (** Author URN *)
  created_at: string option;      (** Creation timestamp *)
  text: string option;            (** Post text content *)
  visibility: string option;      (** Visibility setting *)
  lifecycle_state: string option; (** Lifecycle state (PUBLISHED, etc) *)
}

(** Type definitions for uploaded media *)
type uploaded_media = {
  asset_urn: string;
  media_type: string;
  alt_text: string option;
}

(** Pagination state for scroller pattern *)
type pagination_state = {
  start: int;
  count: int;
  has_more: bool;
}

(** Search result types *)
type search_criteria = {
  keywords: string option;
  author: string option;
  start: int;
  count: int;
}

(** Engagement information *)
type engagement_info = {
  like_count: int option;
  comment_count: int option;
  share_count: int option;
  impression_count: int option;
}

type account_analytics = {
  entity_urn: string;
  follower_count: int option;
  impression_count: int option;
  unique_impression_count: int option;
  share_count: int option;
  click_count: int option;
  like_count: int option;
  comment_count: int option;
}

(** Comment on a post *)
type comment_info = {
  id: string;
  actor: string;
  text: string;
  created_at: string option;
}

type organization_access_info = {
  organization_urn: string;
  organization_id: string option;
  role: string option;
  state: string option;
}

(** {1 Organization lookup}

    An organization's public names, from
    [GET /rest/organizationsLookup?ids=List(...)]
    ({{:https://learn.microsoft.com/en-us/linkedin/marketing/community-management/organizations/organization-lookup-api}
    Organization Lookup API}, checked 2026-11-17). [organizationsLookup]
    returns the non-admin fields ([id], [localizedName], [vanityName], ...)
    for any organization, without the member being its administrator;
    [GET /rest/organizations?ids=] would answer 403 per organization the
    member does not administer. The batch response is
    [{"results": {"<id>": {...}}, "statuses": {"<id>": 200}, "errors": {"<id>": {...}}}]. *)

type organization = {
  id : string;  (** Numeric organization id, as in [urn:li:organization:<id>]. *)
  localized_name : string option;  (** [localizedName] *)
  vanity_name : string option;  (** [vanityName]: the name in the Page URL. *)
}

(** Ids per [organizationsLookup] request. LinkedIn documents no batch
    limit; this keeps request URLs short. *)
let organizations_lookup_batch_size = 50

(** Parses an [organizationsLookup] (or batch [organizations]) body: one
    {!organization} per entry of [results]. An id LinkedIn could not
    resolve (listed under [errors]) has no entry. [Error] when the body is
    not JSON or has no [results] object. *)
let parse_organizations_lookup body : (organization list, string) Stdlib.result =
  let text = function
    | Some (`String value) when String.trim value <> "" -> Some value
    | Some (`String _ | `Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _) | None -> None
  in
  let organization_of (key, value) =
    match value with
    | `Assoc fields ->
        let id =
          match List.assoc_opt "id" fields with
          | Some (`Int id) -> string_of_int id
          | Some (`String id) when String.trim id <> "" -> String.trim id
          | Some (`String _ | `Null | `Bool _ | `Float _ | `List _ | `Assoc _) | None -> key
        in
        Some
          { id;
            localized_name = text (List.assoc_opt "localizedName" fields);
            vanity_name = text (List.assoc_opt "vanityName" fields) }
    | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> None
  in
  match Yojson.Basic.from_string body with
  | exception Yojson.Json_error message ->
      Stdlib.Error ("organization lookup response is not JSON: " ^ message)
  | `Assoc fields -> (
      match List.assoc_opt "results" fields with
      | Some (`Assoc results) -> Stdlib.Ok (List.filter_map organization_of results)
      | Some (`Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _) | None ->
          Stdlib.Error "organization lookup response has no results object")
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ ->
      Stdlib.Error "organization lookup response is not an object"

(** [true] for a non-empty all-digit organization id, the only shape the
    Rest.li [List(...)] parameter can carry unescaped. *)
let is_organization_id id =
  id <> "" && String.for_all (fun c -> c >= '0' && c <= '9') id

(** {1 Media kinds}

    LinkedIn uploads images, videos and documents through three different
    APIs, so the SDK must know what each media URL points at. Callers that
    already know (from their own media records or a MIME type) pass it to
    {!Make_with_options.post_single} as [?media_kinds]; otherwise the kind is
    read from the downloaded file's [Content-Type] and, failing that, from
    the URL path. *)

type media_kind =
  | Image
  | Video
  | Document

let string_of_media_kind = function
  | Image -> "image"
  | Video -> "video"
  | Document -> "document"

(** [media_kind_of_content_type "video/mp4; codecs=avc1"] is [Some Video].
    Only [image/*] and [video/*] are recognised: anything else (including
    [application/octet-stream], which storage services send for unknown
    files) is [None], so the caller falls back to the URL. Documents are
    never inferred; pass [Document] explicitly. *)
let media_kind_of_content_type content_type =
  let mime =
    match String.index_opt content_type ';' with
    | Some i -> String.sub content_type 0 i
    | None -> content_type
  in
  let mime = String.lowercase_ascii (String.trim mime) in
  if String.starts_with ~prefix:"video/" mime then Some Video
  else if String.starts_with ~prefix:"image/" mime then Some Image
  else None

let video_path_suffixes = [ ".mp4"; ".mov"; ".mpeg"; ".avi" ]

(** The kind a URL's path suggests: [Video] for a video file extension,
    otherwise [Image]. The query string and fragment are ignored, so a
    presigned URL such as [https://bucket.s3.amazonaws.com/a.mp4?X-Amz-...]
    is a video. *)
let media_kind_of_url url =
  let path = String.lowercase_ascii (Uri.path (Uri.of_string (String.trim url))) in
  if List.exists (fun suffix -> Filename.check_suffix path suffix) video_path_suffixes
  then Video
  else Image

(** The kind to upload a media item as: the caller's [declared] kind if
    given, else the downloaded file's [Content-Type], else the URL path. *)
let resolve_media_kind ~declared ~content_type ~url =
  match declared with
  | Some kind -> kind
  | None ->
      (match Option.bind content_type media_kind_of_content_type with
       | Some kind -> kind
       | None -> media_kind_of_url url)

(** Case-insensitive response header lookup (HTTP header names are
    case-insensitive and clients differ in how they report them). *)
let find_response_header headers name =
  let name = String.lowercase_ascii name in
  List.find_map
    (fun (key, value) ->
      if String.lowercase_ascii key = name then Some value else None)
    headers

(** {1 Video upload (Videos API)}

    @see <https://learn.microsoft.com/en-us/linkedin/marketing/community-management/shares/videos-api>

    [initializeUpload] declares the file size and answers with one upload
    instruction per part (4 MiB each, the last one shorter). Each part is
    PUT to its own URL and answers with an [ETag]; [finalizeUpload] then
    joins the parts, given the ETags in instruction order and the upload
    token. Only after that does the video leave [WAITING_UPLOAD]. *)

(** One part of a multi-part video upload: the bytes
    [first_byte..last_byte] (both inclusive) go to [upload_url]. *)
type video_upload_part = {
  upload_url : string;
  first_byte : int;
  last_byte : int;
}

(** The [initializeUpload] response. [upload_token] is opaque and may be
    the empty string; it is sent back unchanged in [finalizeUpload]. *)
type video_upload_session = {
  video_urn : string;
  upload_token : string;
  parts : video_upload_part list;
}

(** Processing state of an uploaded video ([GET /rest/videos/{urn}]). *)
type video_status =
  | Video_waiting_upload
  | Video_processing
  | Video_available
  | Video_processing_failed of string option
      (** [processingFailureReason], when LinkedIn gives one. *)
  | Video_status_other of string
      (** A status this SDK does not know yet. *)

let parse_json_result ~what body f =
  match Yojson.Basic.from_string body with
  | json ->
      (try Ok (f json) with
       | Yojson.Basic.Util.Type_error (msg, _) ->
           Error (Printf.sprintf "Failed to parse %s: %s" what msg))
  | exception Yojson.Json_error msg ->
      Error (Printf.sprintf "Failed to parse %s: %s" what msg)

(** Parse an [initializeUpload] response:
    [{"value": {"video": urn, "uploadToken": t,
    "uploadInstructions": [{"uploadUrl", "firstByte", "lastByte"}, ...]}}]. *)
let parse_video_upload_session body =
  let open Yojson.Basic.Util in
  let parsed =
    parse_json_result ~what:"video initializeUpload response" body (fun json ->
        let value = member "value" json in
        let part instruction =
          {
            upload_url = instruction |> member "uploadUrl" |> to_string;
            first_byte = instruction |> member "firstByte" |> to_number |> Float.to_int;
            last_byte = instruction |> member "lastByte" |> to_number |> Float.to_int;
          }
        in
        {
          video_urn = value |> member "video" |> to_string;
          upload_token =
            value |> member "uploadToken" |> to_string_option
            |> Option.value ~default:"";
          parts = value |> member "uploadInstructions" |> to_list |> List.map part;
        })
  in
  match parsed with
  | Ok { parts = []; _ } ->
      Error "LinkedIn video initializeUpload returned no upload instructions"
  | (Ok _ | Error _) as result -> result

(** Byte offset and length of [part] within a file of [file_size] bytes.
    The last part's [lastByte] may point past the end of the file, so it is
    clamped; a part that starts outside the file is an error. *)
let video_part_slice ~file_size part =
  if part.first_byte < 0 || part.first_byte >= file_size
     || part.last_byte < part.first_byte
  then
    Error
      (Printf.sprintf
         "LinkedIn video upload instruction %d-%d does not fit a %d-byte file"
         part.first_byte part.last_byte file_size)
  else
    let last = min part.last_byte (file_size - 1) in
    Ok (part.first_byte, last - part.first_byte + 1)

(** The part id [finalizeUpload] expects: the part PUT's [ETag] header,
    without the quotes HTTP puts around entity tags. *)
let video_part_id_of_headers headers =
  match find_response_header headers "etag" with
  | None -> None
  | Some raw ->
      let tag = String.trim raw in
      let len = String.length tag in
      let unquoted =
        if len >= 2 && tag.[0] = '"' && tag.[len - 1] = '"' then
          String.sub tag 1 (len - 2)
        else tag
      in
      if unquoted = "" then None else Some unquoted

(** The [finalizeUpload] request body. *)
let video_finalize_body ~(session : video_upload_session) ~part_ids =
  Yojson.Basic.to_string
    (`Assoc
       [
         ( "finalizeUploadRequest",
           `Assoc
             [
               ("video", `String session.video_urn);
               ("uploadToken", `String session.upload_token);
               ("uploadedPartIds", `List (List.map (fun id -> `String id) part_ids));
             ] );
       ])

let video_status_of_string ~failure_reason = function
  | "WAITING_UPLOAD" -> Video_waiting_upload
  | "PROCESSING" -> Video_processing
  | "AVAILABLE" -> Video_available
  | "PROCESSING_FAILED" -> Video_processing_failed failure_reason
  | other -> Video_status_other other

(** Parse a [GET /rest/videos/{urn}] response's [status]. *)
let parse_video_status body =
  let open Yojson.Basic.Util in
  parse_json_result ~what:"video status response" body (fun json ->
      let failure_reason = json |> member "processingFailureReason" |> to_string_option in
      json |> member "status" |> to_string |> video_status_of_string ~failure_reason)

(** A non-blocking wait in the consumer's runtime; see {!Social_core.SLEEP}.
    With Lwt: [Social_lwt.Lwt_adapter.sleep]. *)
type sleep = seconds:float -> (unit -> unit) -> (string -> unit) -> unit

(** Options for {!Make_with_options}. Start from {!Default_options}. *)
module type OPTIONS = sig
  (** Wait between checks of a finalized video's processing status.
      With [Some sleep], {!Make_with_options.post_single} waits (without
      blocking) until LinkedIn reports the video [AVAILABLE] before creating
      the post. [None] creates the post right after [finalizeUpload]; the
      SDK never blocks a thread, so it cannot wait without a timer. *)
  val sleep : sleep option
end

(** Defaults: no timer, so a video post is created right after upload. *)
module Default_options = struct
  let sleep : sleep option = None
end

(** Configuration module type for LinkedIn provider *)
module type CONFIG = sig
  module Http : HTTP_CLIENT
  
  val get_env : string -> string option
  val get_credentials : account_id:string -> (credentials -> unit) -> (string -> unit) -> unit
  val update_credentials : account_id:string -> credentials:credentials -> (unit -> unit) -> (string -> unit) -> unit
  val encrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val decrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val update_health_status : account_id:string -> status:string -> error_message:string option -> (unit -> unit) -> (string -> unit) -> unit
end

(** LinkedIn provider with explicit {!OPTIONS} (e.g. a timer, so video
    posts wait until LinkedIn has processed the video). *)
module type COORDINATED_CONFIG = sig
  include CONFIG

  val with_refresh_lock :
    account_id:string ->
    ((Social_core.credentials -> unit) -> (Error_types.error -> unit) -> unit) ->
    (Social_core.credentials -> unit) ->
    (Error_types.error -> unit) -> unit
end

module Make_coordinated_with_options (Config : COORDINATED_CONFIG) (Options : OPTIONS) = struct
  let linkedin_api_base = "https://api.linkedin.com/v2"
  let linkedin_rest_base = "https://api.linkedin.com/rest"
  let linkedin_auth_url = "https://www.linkedin.com/oauth/v2"
  
  (** Check if token is expired or expiring soon *)
  let is_token_expired_buffer ~buffer_seconds expires_at_opt =
    Social_token_expiry.is_expired ~buffer_seconds expires_at_opt
  
  (** {1 Platform Constants} *)
  
  (** Maximum post length (LinkedIn allows 3000 characters for posts) *)
  let max_text_length = 3000
  
  (** Maximum number of images per post *)
  let max_images = 9
  
  (** Maximum image size (8MB) *)
  let max_image_size_bytes = 8 * 1024 * 1024
  
  (** Maximum video size (200MB for standard accounts) *)
  let max_video_size_bytes = 200 * 1024 * 1024
  
  (** Maximum video duration in seconds (10 minutes) *)
  let max_video_duration_seconds = 600

  let has_surrounding_whitespace value = value <> String.trim value
  let is_blank value = String.trim value = ""

  let normalize_expires_in seconds =
    if seconds < 0 then 0 else if seconds > 31_536_000 then 31_536_000 else seconds

  let has_forbidden_restli_list_chars value =
    String.contains value ',' || String.contains value '(' || String.contains value ')'

  let validate_restli_list_item ~field_name value =
    if is_blank value then
      Some (Printf.sprintf "%s is required" field_name)
    else if has_surrounding_whitespace value then
      Some (Printf.sprintf "%s must not contain leading or trailing whitespace" field_name)
    else if has_forbidden_restli_list_chars value then
      Some (Printf.sprintf "%s contains invalid characters for Rest.li list encoding" field_name)
    else
      None

  let validate_required_urn ~field_name value =
    if is_blank value then
      Some (Printf.sprintf "%s is required" field_name)
    else if has_surrounding_whitespace value then
      Some (Printf.sprintf "%s must not contain leading or trailing whitespace" field_name)
    else if has_forbidden_restli_list_chars value then
      Some (Printf.sprintf "%s contains invalid delimiter characters" field_name)
    else
      None

  let is_valid_linkedin_version version =
    let is_digit c = c >= '0' && c <= '9' in
    let month_in_range mm =
      try
        let m = int_of_string mm in
        m >= 1 && m <= 12
      with _ -> false
    in
    let len = String.length version in
    len = 6
    && String.for_all is_digit version
    && month_in_range (String.sub version 4 2)

  let linkedin_version_header () =
    let configured = Config.get_env "LINKEDIN_VERSION" |> Option.value ~default:"" |> String.trim in
    if configured = "" then "202601"
    else if is_valid_linkedin_version configured then configured
    else "202601"

  let is_person_urn urn = String.starts_with ~prefix:"urn:li:person:" urn
  let is_organization_urn urn = String.starts_with ~prefix:"urn:li:organization:" urn
  let is_organization_brand_urn urn = String.starts_with ~prefix:"urn:li:organizationBrand:" urn

  let validate_author_urn value =
    match validate_required_urn ~field_name:"LinkedIn author URN" value with
    | Some err -> Some err
    | None ->
        if is_person_urn value || is_organization_urn value || is_organization_brand_urn value then None
        else Some "LinkedIn author URN must be a person, organization, or organizationBrand URN"

  let normalize_optional_filter value_opt =
    match value_opt with
    | None -> None
    | Some raw ->
        let trimmed = String.trim raw in
        if trimmed = "" then None else Some trimmed

  let normalize_acl_role_filter value_opt =
    normalize_optional_filter value_opt |> Option.map String.uppercase_ascii

  let normalize_acl_state_filter value_opt =
    normalize_optional_filter value_opt |> Option.map String.uppercase_ascii

  let take_first_n n items =
    let rec loop remaining acc = function
      | _ when remaining <= 0 -> List.rev acc
      | [] -> List.rev acc
      | x :: xs -> loop (remaining - 1) (x :: acc) xs
    in
    loop n [] items

  let role_rank = function
    | Some role ->
        (match String.uppercase_ascii (String.trim role) with
        | "ADMINISTRATOR" -> 5
        | "CONTENT_ADMINISTRATOR" -> 4
        | "DIRECT_SPONSORED_CONTENT_POSTER" -> 3
        | "CURATOR" -> 2
        | "ANALYST" -> 1
        | _ -> 0)
    | None -> 0

  let state_rank = function
    | Some state ->
        (match String.uppercase_ascii (String.trim state) with
        | "APPROVED" -> 3
        | "REQUESTED" -> 2
        | "REJECTED" -> 1
        | "REVOKED" -> 0
        | _ -> 0)
    | None -> 0

  let choose_preferred_org_access a b =
    let a_state = state_rank a.state and b_state = state_rank b.state in
    if a_state <> b_state then if a_state > b_state then a else b
    else
      let a_role = role_rank a.role and b_role = role_rank b.role in
      if a_role > b_role then a else b

  let select_preferred_organization_access organizations =
    match organizations with
    | [] -> None
    | first :: rest -> Some (List.fold_left choose_preferred_org_access first rest)

  let normalize_start start = max 0 start
  let normalize_count ~max_count count = min max_count (max 1 count)

  let rec redact_json_sensitive_fields = function
    | `Assoc fields ->
        `Assoc
          (List.map
             (fun (key, value) ->
               let lower = String.lowercase_ascii key in
               if lower = "access_token" || lower = "refresh_token" || lower = "client_secret"
               then (key, `String "[REDACTED]")
               else (key, redact_json_sensitive_fields value))
             fields)
    | `List values -> `List (List.map redact_json_sensitive_fields values)
    | other -> other

  let redact_sensitive_payload body =
    try
      body
      |> Yojson.Basic.from_string
      |> redact_json_sensitive_fields
      |> Yojson.Basic.to_string
    with _ -> "[REDACTED_NON_JSON_ERROR_BODY]"
  
  (** {1 Validation} *)
  
  (** Validate a single post's content *)
  let validate_post ~text ?(media_count=0) () =
    let errors = ref [] in
    let text_len = String.length text in
    if text_len > max_text_length then
      errors := Error_types.Text_too_long { length = text_len; max = max_text_length } :: !errors;
    if media_count > max_images then
      errors := Error_types.Too_many_media { count = media_count; max = max_images } :: !errors;
    if !errors = [] then Ok ()
    else Error (List.rev !errors)
  
  (** Validate a thread before posting (LinkedIn only posts first item) *)
  let validate_thread ~texts ?(media_counts=[]) () =
    if texts = [] then
      Error [Error_types.Thread_empty]
    else
      (* LinkedIn only posts the first item, so only validate that *)
      let first_text = List.hd texts in
      let first_media_count = match media_counts with [] -> 0 | c :: _ -> c in
      match validate_post ~text:first_text ~media_count:first_media_count () with
      | Ok () -> Ok ()
      | Error errs -> Error [Error_types.Thread_post_invalid { index = 0; errors = errs }]
  
  (** Validate media constraints *)
  let validate_media ~(media : Platform_types.post_media) =
    match media.Platform_types.media_type with
    | Platform_types.Image ->
        if media.file_size_bytes > max_image_size_bytes then
          Error [Error_types.Media_too_large { 
            size_bytes = media.file_size_bytes; 
            max_bytes = max_image_size_bytes 
          }]
        else
          Ok ()
    | Platform_types.Video ->
        let errors = ref [] in
        if media.file_size_bytes > max_video_size_bytes then
          errors := Error_types.Media_too_large { 
            size_bytes = media.file_size_bytes; 
            max_bytes = max_video_size_bytes 
          } :: !errors;
        (match media.duration_seconds with
         | Some duration when duration > float_of_int max_video_duration_seconds ->
             errors := Error_types.Video_too_long { 
               duration_seconds = duration; 
               max_seconds = max_video_duration_seconds 
             } :: !errors
         | _ -> ());
        if !errors = [] then Ok ()
        else Error (List.rev !errors)
    | Platform_types.Gif ->
        if media.file_size_bytes > max_image_size_bytes then
          Error [Error_types.Media_too_large { 
            size_bytes = media.file_size_bytes; 
            max_bytes = max_image_size_bytes 
          }]
        else
          Ok ()
  
  (** Suppress warning for validate_media - exported for public use *)
  let _ = validate_media
  
  (** {1 Internal Helpers} *)
  
  (** Parse API error from response *)
  let parse_api_error ~required_scopes ~status_code ~body =
    let redacted_body = redact_sensitive_payload body in
    let extract_linkedin_message () =
      try
        let json = Yojson.Basic.from_string redacted_body in
        let open Yojson.Basic.Util in
        json |> member "message" |> to_string_option
      with _ -> None
    in
    if status_code = 401 then
      Error_types.Auth_error Error_types.Token_expired
    else if status_code = 403 then
      Error_types.Auth_error (Error_types.Insufficient_permissions {
        required = required_scopes;
        platform_message = extract_linkedin_message ();
      })
    else if status_code = 429 then
      Error_types.make_rate_limited ()
    else
      Error_types.make_api_error
        ~platform:Platform_types.LinkedIn
        ~status_code
        ~message:(try
          let json = Yojson.Basic.from_string redacted_body in
          let open Yojson.Basic.Util in
          let msg = json |> member "message" |> to_string_option in
          let service_code = json |> member "serviceErrorCode" |> to_int_option in
          match msg, service_code with
          | Some m, Some code -> Printf.sprintf "Error %d: %s" code m
          | Some m, None -> m
          | None, Some code -> Printf.sprintf "Service error code: %d" code
          | None, None -> "API error"
        with _ -> "API error")
        ~raw_response:redacted_body ()

  let strip_urn_prefix ~prefix urn =
    if String.starts_with ~prefix urn then
      Some (String.sub urn (String.length prefix) (String.length urn - String.length prefix))
    else None

  let organization_id_of_urn urn =
    match strip_urn_prefix ~prefix:"urn:li:organization:" urn with
    | Some id -> Some id
    | None -> strip_urn_prefix ~prefix:"urn:li:organizationBrand:" urn

  let parse_organization_accesses_from_body body =
    let json = Yojson.Basic.from_string body in
    let open Yojson.Basic.Util in
    let elements =
      try json |> member "elements" |> to_list
      with _ -> []
    in
    List.filter_map
      (fun elem ->
        try
          let normalize_urn_opt = function
            | Some urn ->
                let trimmed = String.trim urn in
                if trimmed = "" then None else Some trimmed
            | None -> None
          in
          let organization_opt = elem |> member "organization" |> to_string_option |> normalize_urn_opt in
          let organization_target_opt = elem |> member "organizationTarget" |> to_string_option |> normalize_urn_opt in
          let valid_organization_urn_opt =
            let is_valid urn = is_organization_urn urn || is_organization_brand_urn urn in
            match organization_opt, organization_target_opt with
            | Some urn, _ when is_valid urn -> Some urn
            | _, Some urn when is_valid urn -> Some urn
            | _ -> None
          in
          match valid_organization_urn_opt with
          | None -> None
          | Some organization_urn ->
              let role = elem |> member "role" |> to_string_option |> normalize_optional_filter in
              let state = elem |> member "state" |> to_string_option |> normalize_optional_filter in
              Some
                {
                  organization_urn;
                  organization_id = organization_id_of_urn organization_urn;
                  role;
                  state;
                }
        with _ -> None)
      elements

  let fetch_organization_accesses_by_token ?role ?state ~access_token on_result =
    let page_size = 100 in
    let max_items = 1000 in
    let max_pages = (max_items / page_size) + 5 in
    let headers =
      [
        ("Authorization", Printf.sprintf "Bearer %s" access_token);
        ("X-Restli-Protocol-Version", "2.0.0");
        ("LinkedIn-Version", linkedin_version_header ());
        ("X-RestLi-Method", "FINDER");
      ]
    in
    let build_url start =
      let params =
        [
          ("q", "roleAssignee");
          ("count", string_of_int page_size);
          ("start", string_of_int start);
        ]
        @
        (match role with Some r -> [ ("role", r) ] | None -> [])
        @
        (match state with Some s -> [ ("state", s) ] | None -> [])
      in
      let query_string = Uri.encoded_of_query (List.map (fun (k, v) -> (k, [ v ])) params) in
      Printf.sprintf "https://api.linkedin.com/rest/organizationAcls?%s" query_string
    in
    let rec fetch_page ~start ~acc ~pages_fetched =
      Config.Http.get ~headers (build_url start)
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let page_accesses = parse_organization_accesses_from_body response.body in
              let merged =
                List.fold_left
                  (fun seen item ->
                    let rec replace_or_add acc_seen = function
                      | [] -> List.rev (item :: acc_seen)
                      | existing :: rest when existing.organization_urn = item.organization_urn ->
                          let preferred = choose_preferred_org_access item existing in
                          List.rev_append acc_seen (preferred :: rest)
                      | existing :: rest -> replace_or_add (existing :: acc_seen) rest
                    in
                    replace_or_add [] seen)
                  acc page_accesses
              in
              let merged_count = List.length merged in
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let raw_elements_count =
                try json |> member "elements" |> to_list |> List.length
                with _ -> List.length page_accesses
              in
              let next_start =
                try
                  let paging = json |> member "paging" in
                  let paging_start = paging |> member "start" |> to_int in
                  let paging_count = paging |> member "count" |> to_int in
                  let page_total = paging |> member "total" |> to_int_option in
                  let candidate = paging_start + paging_count in
                  match page_total with
                  | Some total when candidate >= total -> None
                  | _ when paging_count <= 0 -> None
                  | _ -> Some candidate
                with _ ->
                  if raw_elements_count < page_size then None
                  else Some (start + page_size)
              in
              let pages_fetched = pages_fetched + 1 in
              if merged_count >= max_items then on_result (Ok (take_first_n max_items merged))
              else if pages_fetched >= max_pages then on_result (Ok merged)
              else
                match next_start with
                | Some ns when ns > start -> fetch_page ~start:ns ~acc:merged ~pages_fetched
                | Some _ -> on_result (Ok merged)
                | None -> on_result (Ok merged)
            with e ->
              on_result
                (Error
                   (Error_types.Internal_error
                      (Printf.sprintf "Failed to parse organization access response: %s"
                         (Printexc.to_string e))))
          else
            on_result
              (Error
                 (parse_api_error ~required_scopes:[ "r_organization_admin" ]
                    ~status_code:response.status ~body:response.body)))
        (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err))))
    in
    fetch_page ~start:0 ~acc:[] ~pages_fetched:0

  (** Names of organizations by id ({!organization}): batches of
      {!organizations_lookup_batch_size} ids to
      [GET /rest/organizationsLookup?ids=List(...)], one request after the
      other, results in request order. Duplicate ids are asked once. An id
      LinkedIn cannot resolve is left out of the result; a failed request
      fails the whole call. Ids that are not all digits are a
      [Validation_error] and nothing is sent; an empty list is [Ok []]
      without a request.

      Pairs with {!fetch_organization_accesses_by_token}, whose
      [organization_id]s are the ids this takes. The token needs
      [rw_organization_admin]. *)
  let get_organizations ~access_token ~ids on_result =
    let ids =
      List.fold_left
        (fun acc id -> let id = String.trim id in if List.mem id acc then acc else id :: acc)
        [] ids
      |> List.rev
    in
    match List.find_opt (fun id -> not (is_organization_id id)) ids with
    | Some bad ->
        on_result
          (Error
             (Error_types.Validation_error
                [ Error_types.Invalid_url (Printf.sprintf "not a LinkedIn organization id: %S" bad) ]))
    | None ->
        let headers =
          [ ("Authorization", Printf.sprintf "Bearer %s" access_token);
            ("X-Restli-Protocol-Version", "2.0.0");
            ("LinkedIn-Version", linkedin_version_header ()) ]
        in
        (* Rest.li 2.0 list syntax: the parentheses and commas stay literal;
           the ids are digits, so nothing needs escaping. *)
        let url batch =
          Printf.sprintf "%s/organizationsLookup?ids=List(%s)" linkedin_rest_base
            (String.concat "," batch)
        in
        let rec batches acc current count = function
          | [] -> List.rev (if current = [] then acc else List.rev current :: acc)
          | id :: rest when count = organizations_lookup_batch_size ->
              batches (List.rev current :: acc) [ id ] 1 rest
          | id :: rest -> batches acc (id :: current) (count + 1) rest
        in
        let rec fetch acc = function
          | [] -> on_result (Ok (List.concat (List.rev acc)))
          | batch :: rest ->
              Config.Http.get ~headers (url batch)
                (fun response ->
                  if response.status >= 200 && response.status < 300 then
                    match parse_organizations_lookup response.body with
                    | Ok organizations -> fetch (organizations :: acc) rest
                    | Error msg ->
                        on_result
                          (Error
                             (Error_types.Internal_error
                                (Printf.sprintf "Failed to parse organization lookup response: %s" msg)))
                  else
                    on_result
                      (Error
                         (parse_api_error ~required_scopes:[ "rw_organization_admin" ]
                            ~status_code:response.status ~body:response.body)))
                (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err))))
        in
        fetch [] (batches [] [] 0 ids)
  
  (** Refresh OAuth 2.0 access token (PARTNER PROGRAM ONLY)
      
      IMPORTANT: Programmatic token refresh is only available for LinkedIn Partner Program apps.
      Standard apps using "Sign In with LinkedIn" or "Share on LinkedIn" products DO NOT
      have access to programmatic refresh.
      
      For standard apps:
      - This function will always fail with "unauthorized_client" or similar error
      - Users must re-authorize through the OAuth flow (consent screen bypassed if logged in)
      - The re-authorization should happen automatically when token expires
      
      To check if your app has programmatic refresh:
      - Log in to LinkedIn Developer Portal
      - Check your app's products - if you have "Marketing Developer Platform" or similar
        partner program access, you have programmatic refresh
      - Standard apps only have "Sign In with LinkedIn" and "Share on LinkedIn"
  *)
  let refresh_access_token ~client_id ~client_secret ~refresh_token on_success on_error =
    let enable_programmatic_refresh = 
      Config.get_env "LINKEDIN_ENABLE_PROGRAMMATIC_REFRESH" 
      |> Option.value ~default:"false" 
      |> String.lowercase_ascii = "true" in
    
    if not enable_programmatic_refresh then
      on_error "Programmatic refresh not enabled - user must re-authorize. Set LINKEDIN_ENABLE_PROGRAMMATIC_REFRESH=true if you have LinkedIn Partner Program access."
    else if String.trim client_id = "" || String.trim client_secret = "" then
      on_error "LinkedIn OAuth credentials not configured"
    else if has_surrounding_whitespace client_id || has_surrounding_whitespace client_secret then
      on_error "LinkedIn OAuth credentials must not contain leading or trailing whitespace"
    else if has_surrounding_whitespace refresh_token then
      on_error "LinkedIn refresh token must not contain leading or trailing whitespace"
    else if is_blank refresh_token then
      on_error "LinkedIn refresh token is required"
    else (
      let url = Printf.sprintf "%s/accessToken" linkedin_auth_url in
      
      let body = Social_core.form_urlencode_kvs [
        ("grant_type", "refresh_token");
        ("refresh_token", refresh_token);
        ("client_id", String.trim client_id);
        ("client_secret", String.trim client_secret);
      ] in

      let headers = [
        ("Content-Type", "application/x-www-form-urlencoded")
      ] in

      Config.Http.post ~headers ~body url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let new_access = json |> member "access_token" |> to_string |> String.trim in
              if new_access = "" then
                on_error "LinkedIn OAuth response contains empty access token"
              else
              let new_refresh = 
                try
                  let parsed = json |> member "refresh_token" |> to_string |> String.trim in
                  if parsed = "" then refresh_token else parsed
                with _ -> refresh_token
              in
              (* CRITICAL: Read actual expires_in from LinkedIn refresh response *)
              let expires_in = normalize_expires_in (json |> member "expires_in" |> to_int) in
              let expires_at = 
                let now = Ptime_clock.now () in
                match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
                | Some exp -> Ptime.to_rfc3339 exp
                | None -> Ptime.to_rfc3339 now in
              on_success (new_access, new_refresh, expires_at)
            with e ->
              on_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))
          else
            (* Parse error response for better error messages *)
            let error_msg = 
              try
                let json = Yojson.Basic.from_string response.body in
                let open Yojson.Basic.Util in
                let error = json |> member "error" |> to_string_option |> Option.value ~default:"unknown" in
                let error_desc = json |> member "error_description" |> to_string_option in
                match error, error_desc with
                | "unauthorized_client", _ | "invalid_grant", _ ->
                    "Programmatic refresh not available - your app is not enrolled in LinkedIn Partner Program. User must re-authorize."
                | _, Some desc -> Printf.sprintf "%s: %s" error desc
                | _, None -> error
              with _ -> response.body
            in
            on_error (Printf.sprintf "Token refresh failed (%d): %s" response.status error_msg))
        on_error
    )
  
  (** Ensure valid OAuth 2.0 access token, refreshing if needed *)
  let ensure_valid_token ~account_id on_success on_error =
    let user_friendly_refresh_error err =
      if String.length err > 100
         &&
         (Str.string_match (Str.regexp ".*[Pp]rogrammatic.*") err 0
          || Str.string_match (Str.regexp ".*[Pp]artner.*") err 0)
      then
        "LinkedIn token refresh failed - please reconnect your account"
      else
        Printf.sprintf "LinkedIn token refresh failed: %s - please reconnect your account" err
    in
    let perform_refresh ~credentials on_refresh_success on_refresh_error =
      match credentials.Social_core.refresh_token with
      | None -> on_refresh_error (Error_types.Auth_error Error_types.Token_expired)
      | Some refresh_token ->
          let client_id = Config.get_env "LINKEDIN_CLIENT_ID" |> Option.value ~default:"" in
          let client_secret = Config.get_env "LINKEDIN_CLIENT_SECRET" |> Option.value ~default:"" in
          refresh_access_token ~client_id ~client_secret ~refresh_token
            (fun (new_access, new_refresh, expires_at) ->
              on_refresh_success {
                Social_core.access_token = new_access;
                refresh_token = Some new_refresh;
                expires_at = Some expires_at;
                auth_type = Bearer;
                scope = credentials.Social_core.scope;
              })
            (fun err ->
              let message = user_friendly_refresh_error err in
              on_refresh_error (Error_types.Auth_error (Error_types.Refresh_failed message)))
    in
    Social_refresh.Orchestrator.ensure_valid_access_token
      ~with_refresh_lock:Config.with_refresh_lock
      ~reload_credentials:Config.get_credentials
      ~policy:{ Social_refresh.refresh_window_seconds = 604800 }
      ~account_id
      ~load_credentials:Config.get_credentials
      ~perform_refresh
      ~persist_credentials:Config.update_credentials
      ~update_health:Config.update_health_status
      (fun credentials -> on_success credentials.Social_core.access_token)
      on_error
  
  (** Get person URN for posting *)
  let get_person_urn ~access_token on_success on_error =
    let url = Printf.sprintf "%s/userinfo" linkedin_api_base in
    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
      ("LinkedIn-Version", linkedin_version_header ());
    ] in
    
    Config.Http.get ~headers url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            (* OpenID Connect returns 'sub' (subject) as the user identifier *)
            let raw_person_id = json |> Yojson.Basic.Util.member "sub" |> Yojson.Basic.Util.to_string in
            if has_surrounding_whitespace raw_person_id then
              on_error "LinkedIn userinfo subject identifier must not contain leading or trailing whitespace"
            else if raw_person_id = "" then
              on_error "LinkedIn userinfo response has empty subject identifier"
            else
              let person_urn = Printf.sprintf "urn:li:person:%s" raw_person_id in
              (match validate_restli_list_item ~field_name:"LinkedIn person URN" person_urn with
               | Some err -> on_error err
               | None -> on_success person_urn)
          with e ->
            on_error (Printf.sprintf "Failed to parse person URN: %s" (Printexc.to_string e))
        else
          let redacted = redact_sensitive_payload response.body in
          let body_message =
            try
              let json = Yojson.Basic.from_string redacted in
              Yojson.Basic.Util.(json |> member "message" |> to_string_option)
            with _ -> None
          in
          let message =
            match response.status with
            | 401 ->
                "Failed to get person URN (401): authentication failed; token may be expired"
            | 403 ->
                "Failed to get person URN (403): insufficient permissions (required scopes: openid, profile)"
            | 429 ->
                "Failed to get person URN (429): rate limited"
            | code ->
                (match body_message with
                 | Some msg -> Printf.sprintf "Failed to get person URN (%d): %s" code msg
                 | None -> Printf.sprintf "Failed to get person URN (%d)" code)
          in
          on_error message)
      on_error
  
  let rest_json_headers ~access_token =
    [
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
      ("Content-Type", "application/json");
      ("X-Restli-Protocol-Version", "2.0.0");
      ("LinkedIn-Version", linkedin_version_header ());
    ]

  let binary_upload_headers ~access_token =
    [
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
      ("Content-Type", "application/octet-stream");
      ("LinkedIn-Version", linkedin_version_header ());
    ]

  let is_success_status status = status >= 200 && status < 300

  (* A URN as one path segment: Rest.li wants its colons encoded too
     ([/rest/videos/urn%3Ali%3Avideo%3A...]), which path encoding keeps. *)
  let encode_urn_path_segment urn =
    String.concat "%3A" (String.split_on_char ':' (Uri.pct_encode urn))

  (** Register a single-part image or document upload ([/rest/images] or
      [/rest/documents] [initializeUpload]); answers [(asset_urn, upload_url)]
      for {!upload_binary}. Videos are multi-part and go through
      {!upload_video} instead. *)
  let register_single_part_upload ~access_token ~owner_urn ~kind on_success on_error =
    let endpoint_and_field =
      match kind with
      | Image -> Ok ("images", "image")
      | Document -> Ok ("documents", "document")
      | Video ->
          Error
            "LinkedIn videos are uploaded in parts after declaring their size; use upload_video"
    in
    match endpoint_and_field with
    | Error msg -> on_error msg
    | Ok (endpoint, urn_field) ->
        let body =
          Yojson.Basic.to_string
            (`Assoc [ ("initializeUploadRequest", `Assoc [ ("owner", `String owner_urn) ]) ])
        in
        let url = Printf.sprintf "%s/%s?action=initializeUpload" linkedin_rest_base endpoint in
        Config.Http.post ~headers:(rest_json_headers ~access_token) ~body url
          (fun response ->
            if is_success_status response.status then
              let parsed =
                parse_json_result ~what:"register response" response.body (fun json ->
                    let open Yojson.Basic.Util in
                    let value = member "value" json in
                    ( value |> member urn_field |> to_string,
                      value |> member "uploadUrl" |> to_string ))
              in
              match parsed with
              | Ok asset -> on_success asset
              | Error msg -> on_error msg
            else
              on_error
                (Printf.sprintf "Failed to register upload (%d): %s" response.status
                   (redact_sensitive_payload response.body)))
          on_error

  (** Register an image or document upload with LinkedIn's per-asset-type
      REST APIs ([/rest/images], [/rest/documents]). Returns
      [(asset_urn, upload_url)] for {!upload_binary}.

      [~media_type:"video"] is an error: the Videos API needs the file size
      up front and a multi-part upload, see {!upload_video}. Any other value
      registers an image. *)
  let register_upload ~access_token ~owner_urn ~media_type on_success on_error =
    let kind =
      match media_type with
      | "video" -> Video
      | "document" -> Document
      | _ -> Image
    in
    register_single_part_upload ~access_token ~owner_urn ~kind on_success on_error

  (** Upload binary media data to LinkedIn *)
  let upload_binary ~access_token ~upload_url ~media_data on_success on_error =
    Config.Http.put ~headers:(binary_upload_headers ~access_token) ~body:media_data upload_url
      (fun response ->
        if is_success_status response.status then on_success ()
        else on_error (Printf.sprintf "Failed to upload media binary (%d)" response.status))
      on_error

  (** {2 Videos API} *)

  (** Seconds between video status checks while waiting for [AVAILABLE]. *)
  let video_status_poll_interval_seconds = 5.0

  (** Status checks before the post is created anyway (5 minutes at the
      default interval). LinkedIn publishes a post whose video is still
      processing once processing completes. *)
  let video_status_max_polls = 60

  (** [POST /rest/videos?action=initializeUpload] declaring
      [file_size_bytes]; answers the upload session (video URN, upload token
      and one instruction per part). *)
  let initialize_video_upload ~access_token ~owner_urn ~file_size_bytes on_success on_error =
    let body =
      Yojson.Basic.to_string
        (`Assoc
           [
             ( "initializeUploadRequest",
               `Assoc
                 [
                   ("owner", `String owner_urn);
                   ("fileSizeBytes", `Int file_size_bytes);
                   ("uploadCaptions", `Bool false);
                   ("uploadThumbnail", `Bool false);
                 ] );
           ])
    in
    let url = Printf.sprintf "%s/videos?action=initializeUpload" linkedin_rest_base in
    Config.Http.post ~headers:(rest_json_headers ~access_token) ~body url
      (fun response ->
        if is_success_status response.status then
          match parse_video_upload_session response.body with
          | Ok session -> on_success session
          | Error msg -> on_error msg
        else
          on_error
            (Printf.sprintf "Failed to initialize video upload (%d): %s" response.status
               (redact_sensitive_payload response.body)))
      on_error

  (** PUT every part of [media_data] to its instruction's URL, in order;
      answers the part ids (ETags) in the same order. *)
  let upload_video_parts ~access_token:_ ~(session : video_upload_session) ~media_data on_success
      on_error =
    let file_size = String.length media_data in
    let rec upload_parts parts part_ids =
      match parts with
      | [] -> on_success (List.rev part_ids)
      | part :: rest ->
          (match video_part_slice ~file_size part with
           | Error msg -> on_error msg
           | Ok (offset, length) ->
               Config.Http.put ~headers:[ ("Content-Type", "application/octet-stream") ]
                 ~body:(String.sub media_data offset length) part.upload_url
                 (fun response ->
                   if not (is_success_status response.status) then
                     on_error
                       (Printf.sprintf "Failed to upload video part %d-%d (%d)" part.first_byte
                          part.last_byte response.status)
                   else
                     match video_part_id_of_headers response.headers with
                     | Some part_id -> upload_parts rest (part_id :: part_ids)
                     | None ->
                         on_error
                           (Printf.sprintf
                              "LinkedIn returned no ETag for video part %d-%d; cannot finalize the upload"
                              part.first_byte part.last_byte))
                 on_error)
    in
    upload_parts session.parts []

  (** [POST /rest/videos?action=finalizeUpload] with the upload token and
      the part ids in instruction order. *)
  let finalize_video_upload ~access_token ~(session : video_upload_session) ~part_ids on_success
      on_error =
    let url = Printf.sprintf "%s/videos?action=finalizeUpload" linkedin_rest_base in
    Config.Http.post ~headers:(rest_json_headers ~access_token)
      ~body:(video_finalize_body ~session ~part_ids) url
      (fun response ->
        if is_success_status response.status then on_success ()
        else
          on_error
            (Printf.sprintf "Failed to finalize video upload (%d): %s" response.status
               (redact_sensitive_payload response.body)))
      on_error

  (** [GET /rest/videos/{urn}]: the video's processing status. *)
  let get_video_status ~access_token ~video_urn on_success on_error =
    let url = Printf.sprintf "%s/videos/%s" linkedin_rest_base (encode_urn_path_segment video_urn) in
    let headers =
      [
        ("Authorization", Printf.sprintf "Bearer %s" access_token);
        ("X-Restli-Protocol-Version", "2.0.0");
        ("LinkedIn-Version", linkedin_version_header ());
      ]
    in
    Config.Http.get ~headers url
      (fun response ->
        if is_success_status response.status then
          match parse_video_status response.body with
          | Ok status -> on_success status
          | Error msg -> on_error msg
        else
          on_error
            (Printf.sprintf "Failed to read video status (%d): %s" response.status
               (redact_sensitive_payload response.body)))
      on_error

  (** With {!OPTIONS.sleep}, wait until the video is [AVAILABLE], checking
      every {!video_status_poll_interval_seconds}. [PROCESSING_FAILED] is an
      error. After {!video_status_max_polls} checks (or without a timer) it
      continues anyway and lets the Posts API decide. A failed status read
      only costs one check. *)
  let wait_for_video_available ~access_token ~video_urn on_ready on_error =
    match Options.sleep with
    | None -> on_ready ()
    | Some sleep ->
        let rec check polls_left =
          get_video_status ~access_token ~video_urn
            (function
              | Video_available -> on_ready ()
              | Video_processing_failed reason ->
                  on_error
                    (Printf.sprintf "LinkedIn could not process the video%s"
                       (match reason with Some r -> ": " ^ r | None -> ""))
              | Video_waiting_upload | Video_processing | Video_status_other _ ->
                  wait_and_check polls_left)
            (fun _status_error -> wait_and_check polls_left)
        and wait_and_check polls_left =
          if polls_left <= 1 then on_ready ()
          else
            sleep ~seconds:video_status_poll_interval_seconds
              (fun () -> check (polls_left - 1))
              on_error
        in
        check video_status_max_polls

  (** Upload a video through the Videos API: initialize with the file size,
      PUT each part, finalize with the part ETags, then (with a timer) wait
      until it is [AVAILABLE]. Answers the video URN. *)
  let upload_video ~access_token ~owner_urn ~media_data on_success on_error =
    initialize_video_upload ~access_token ~owner_urn
      ~file_size_bytes:(String.length media_data)
      (fun session ->
        upload_video_parts ~access_token ~session ~media_data
          (fun part_ids ->
            finalize_video_upload ~access_token ~session ~part_ids
              (fun () ->
                wait_for_video_available ~access_token ~video_urn:session.video_urn
                  (fun () -> on_success session.video_urn)
                  on_error)
              on_error)
          on_error)
      on_error

  (** Upload already-downloaded bytes as [kind]; answers the asset URN. *)
  let upload_media_data ~access_token ~owner_urn ~kind ~media_data on_success on_error =
    match kind with
    | Video -> upload_video ~access_token ~owner_urn ~media_data on_success on_error
    | Image | Document ->
        register_single_part_upload ~access_token ~owner_urn ~kind
          (fun (asset_urn, upload_url) ->
            upload_binary ~access_token ~upload_url ~media_data
              (fun () -> on_success asset_urn)
              on_error)
          on_error

  let validate_downloaded_media ~kind ~content_type ~file_size ~alt_text =
    let check media_type ~default_mime =
      validate_media
        ~media:
          {
            Platform_types.media_type;
            mime_type = Option.value content_type ~default:default_mime;
            file_size_bytes = file_size;
            width = None;
            height = None;
            duration_seconds = None;
            alt_text;
          }
    in
    match kind with
    | Video -> check Platform_types.Video ~default_mime:"video/mp4"
    | Image -> check Platform_types.Image ~default_mime:"image/jpeg"
    | Document -> Ok ()

  (** Download [media_url] and upload it to LinkedIn. The kind is
      [declared_kind] if given, else the response's [Content-Type], else the
      URL path (see {!resolve_media_kind}). Answers
      [(asset_urn, kind, alt_text)]. *)
  let upload_media_from_url ~access_token ~owner_urn ~media_url ~declared_kind ~alt_text
      ~validate_before_upload ~on_validation_error on_success on_error =
    if not (OAuth.is_safe_remote_media_url media_url) then
      on_error "Unsafe media URL; only public http(s) URLs are allowed"
    else
      Config.Http.get ~headers:[] media_url
        (fun media_response ->
          if not (is_success_status media_response.status) then
            on_error (Printf.sprintf "Failed to download media (%d)" media_response.status)
          else
            let content_type = find_response_header media_response.headers "content-type" in
            let kind = resolve_media_kind ~declared:declared_kind ~content_type ~url:media_url in
            let validation =
              if validate_before_upload then
                validate_downloaded_media ~kind ~content_type
                  ~file_size:(String.length media_response.body) ~alt_text
              else Ok ()
            in
            match validation with
            | Error errs -> on_validation_error errs
            | Ok () ->
                upload_media_data ~access_token ~owner_urn ~kind ~media_data:media_response.body
                  (fun asset_urn -> on_success (asset_urn, kind, alt_text))
                  on_error)
        on_error

  (** Upload image, video or document to LinkedIn with optional alt text.
      [~media_type] is ["image"], ["video"] or ["document"]; any other value
      is treated as an image. *)
  let upload_media ~access_token ~owner_urn ~media_url ~media_type ~alt_text
      ?(validate_before_upload = false) ~on_validation_error on_success on_error =
    let declared_kind =
      match media_type with
      | "video" -> Video
      | "document" -> Document
      | _ -> Image
    in
    upload_media_from_url ~access_token ~owner_urn ~media_url ~declared_kind:(Some declared_kind)
      ~alt_text ~validate_before_upload ~on_validation_error
      (fun (asset_urn, _kind, alt_text) -> on_success (asset_urn, alt_text))
      on_error

  (** Extract first URL from text for link preview *)
  let extract_first_url text =
    try
      let url_pattern = Re.Pcre.regexp 
        "https?://[a-zA-Z0-9][-a-zA-Z0-9@:%._\\+~#=]{0,256}\\.[a-zA-Z0-9()]{1,6}\\b[-a-zA-Z0-9()@:%_\\+.~#?&/=]*"
      in
      let group = Re.exec url_pattern text in
      Some (Re.Group.get group 0)
    with Not_found -> None
  
  (** Truncate a byte string to at most [max_bytes], but never in the middle
      of a UTF-8 multi-byte sequence. Walks back over any continuation bytes
      (0b10xxxxxx) at the cut point so non-ASCII text (umlauts, emoji, CJK,
      etc.) is never corrupted at the truncation boundary. *)
  let truncate_utf8_safe s max_bytes =
    if String.length s <= max_bytes then s
    else begin
      let cut = ref max_bytes in
      while !cut > 0 && (Char.code s.[!cut] land 0xc0) = 0x80 do
        decr cut
      done;
      String.sub s 0 !cut
    end

  (** Derive a fallback article title from post text.
      LinkedIn rejects article payloads without a title (HTTP 422,
      "/content/article/title :: field is required but not found").
      When the caller does not supply an explicit title, use the first
      non-empty line of the post text, truncated to 200 bytes on a UTF-8
      boundary. As a last resort, fall back to a generic label so
      publishing never fails on this field alone. *)
  let derive_article_title ?title text =
    let trimmed_non_empty s = String.length (String.trim s) > 0 in
    match title with
    | Some t when trimmed_non_empty t -> truncate_utf8_safe (String.trim t) 200
    | _ ->
        let first_line =
          match String.split_on_char '\n' text with
          | line :: _ when trimmed_non_empty line -> String.trim line
          | _ -> String.trim text
        in
        if String.length first_line = 0 then "Link preview"
        else truncate_utf8_safe first_line 200

  (** Post to LinkedIn

      @param account_id The account identifier
      @param text The post text (max 3000 characters)
      @param media_urls List of media URLs to attach (max 9)
      @param author_urn Optional explicit author URN. When omitted, uses current person URN.
             Supported formats: urn:li:person:*, urn:li:organization:*, urn:li:organizationBrand:*
      @param alt_texts Optional alt text for each media item
      @param media_kinds What each media item is ([Image], [Video] or [Document]),
             by position in [media_urls]. Pass it when you know: a missing or [None]
             entry is detected from the download's [Content-Type], then from the URL
             path (query string ignored), defaulting to [Image].
      @param title Optional article title. Only used when text contains a URL and no media
             is attached (LinkedIn article payload). Falls back to the first line of text
             truncated to 200 chars when omitted.
      @param link_preview When [true] (the default) and no media is attached, the first
             URL in [text] becomes an [article] content block, so LinkedIn renders a link
             card. Pass [false] to publish the text alone; the URL stays clickable in the
             commentary. The commentary is Little Text Format-escaped either way.
      @param validate_media_before_upload When true, validates media size after download
             but before upload. LinkedIn limits: 200MB video, 10min duration, 8MB images.
             Default: false
      @param on_result Callback receiving the outcome with post ID
  *)
  let post_single ~account_id ~text ~media_urls ?author_urn ?(alt_texts=[]) ?(media_kinds=[]) ?title ?(link_preview=true) ?(validate_media_before_upload=false) on_result =
    (* Validate before making any API calls *)
    let media_count = List.length media_urls in
    let author_validation =
      match author_urn with
      | None -> Ok None
      | Some explicit_author ->
          (match validate_author_urn explicit_author with
          | Some err -> Error err
          | None -> Ok (Some explicit_author))
    in
    match validate_post ~text ~media_count (), author_validation with
    | Error errs, _ ->
        on_result (Error_types.Failure (Error_types.Validation_error errs))
    | _, Error err ->
        on_result (Error_types.Failure (Error_types.Internal_error err))
    | Ok (), Ok validated_author_urn ->
        ensure_valid_token ~account_id
          (fun access_token ->
            let resolve_author_urn on_success_author on_error_author =
              match validated_author_urn with
              | Some explicit_author -> on_success_author explicit_author
              | None -> get_person_urn ~access_token on_success_author on_error_author
            in
            resolve_author_urn
              (fun resolved_author_urn ->
                (* Upload all media items first *)
                let rec upload_all_media items acc on_complete on_err =
                  match items with
                  | [] -> on_complete (List.rev acc)
                  | (url, declared_kind, alt_text) :: rest ->
                      upload_media_from_url ~access_token ~owner_urn:resolved_author_urn
                        ~media_url:url ~declared_kind ~alt_text
                        ~validate_before_upload:validate_media_before_upload
                        ~on_validation_error:(fun errs ->
                          on_result (Error_types.Failure (Error_types.Validation_error errs)))
                        (fun (asset_urn, kind, alt_text) ->
                          let uploaded =
                            { asset_urn; media_type = string_of_media_kind kind; alt_text }
                          in
                          upload_all_media rest (uploaded :: acc) on_complete on_err)
                        on_err
                in

                (* Pair each URL with its declared kind and alt text *)
                let nth_or_none items i = Option.join (List.nth_opt items i) in
                let urls_with_alt =
                  List.mapi
                    (fun i url -> (url, nth_or_none media_kinds i, nth_or_none alt_texts i))
                    media_urls
                in

                upload_all_media urls_with_alt []
                  (fun uploaded_media ->
                    (* Extract URL from text for link preview, unless the caller
                       opted out of the article card. *)
                    let text_url =
                      if link_preview then extract_first_url text else None
                    in

                    (* Build modern /rest/posts body *)
                    let base_fields = [
                      ("author", `String resolved_author_urn);
                      ("commentary", `String (escape_little_text_format text));
                      ("visibility", `String "PUBLIC");
                      ("distribution", `Assoc [
                        ("feedDistribution", `String "MAIN_FEED");
                        ("targetEntities", `List []);
                        ("thirdPartyDistributionChannels", `List []);
                      ]);
                      ("lifecycleState", `String "PUBLISHED");
                    ] in

                    let content_fields =
                      match uploaded_media, text_url with
                      | [], Some url ->
                          (* URL found, no uploaded media - article for link preview.
                             LinkedIn requires `title` on article payloads. *)
                          let article_title = derive_article_title ?title text in
                          [("content", `Assoc [
                            ("article", `Assoc [
                              ("source", `String url);
                              ("title", `String article_title);
                            ]);
                          ])]
                      | [], None ->
                          (* No media, no URL - text only *)
                          []
                      | uploaded_media, _ ->
                          (* Has uploaded media - use appropriate content type *)
                          let first = List.hd uploaded_media in
                          if first.media_type = "video" then
                            let media = first in
                            let title_field = match media.alt_text with
                              | Some alt when String.length alt > 0 -> [("title", `String alt)]
                              | _ -> []
                            in
                            [("content", `Assoc [
                              ("media", `Assoc ([
                                ("id", `String media.asset_urn);
                              ] @ title_field));
                            ])]
                          else if first.media_type = "document" then
                            let media = first in
                            let title_field = match media.alt_text with
                              | Some alt when String.length alt > 0 -> [("title", `String alt)]
                              | _ -> [("title", `String "Document")]
                            in
                            [("content", `Assoc [
                              ("media", `Assoc ([
                                ("id", `String media.asset_urn);
                              ] @ title_field));
                            ])]
                          else match uploaded_media with
                            | [single_image] ->
                              (* Single image - use media content type (multiImage requires 2-20) *)
                              let title_field = match single_image.alt_text with
                                | Some alt when String.length alt > 0 -> [("altText", `String alt)]
                                | _ -> []
                              in
                              [("content", `Assoc [
                                ("media", `Assoc ([
                                  ("id", `String single_image.asset_urn);
                                ] @ title_field));
                              ])]
                            | _ ->
                              (* Multiple images - use multiImage content *)
                              let images_json = `List (List.map (fun media ->
                                let alt_field = match media.alt_text with
                                  | Some alt when String.length alt > 0 -> [("altText", `String alt)]
                                  | _ -> []
                                in
                                `Assoc ([("id", `String media.asset_urn)] @ alt_field)
                              ) uploaded_media) in
                              [("content", `Assoc [
                                ("multiImage", `Assoc [
                                  ("images", images_json);
                                ]);
                              ])]
                    in

                    let post_body = `Assoc (base_fields @ content_fields) in
                    let url = Printf.sprintf "%s/posts" linkedin_rest_base in
                    let body = Yojson.Basic.to_string post_body in
                    let headers =
                      [
                        ("Authorization", Printf.sprintf "Bearer %s" access_token);
                        ("Content-Type", "application/json");
                        ("X-Restli-Protocol-Version", "2.0.0");
                        ("LinkedIn-Version", linkedin_version_header ());
                      ]
                    in

                    Config.Http.post ~headers ~body url
                      (fun response ->
                        if response.status >= 200 && response.status < 300 then
                          let post_id =
                            try
                              (* Modern API returns id in header or body *)
                              let json = Yojson.Basic.from_string response.body in
                              json |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_string
                            with _ ->
                              (* Try x-restli-id header *)
                              (find_response_header response.headers "x-restli-id"
                               |> Option.value ~default:"unknown")
                          in
                          on_result (Error_types.Success post_id)
                        else
                          let required_scopes =
                            if is_organization_urn resolved_author_urn || is_organization_brand_urn resolved_author_urn
                            then ["w_organization_social"]
                            else ["w_member_social"]
                          in
                          on_result
                            (Error_types.Failure
                               (parse_api_error ~required_scopes
                                  ~status_code:response.status ~body:response.body)))
                      (fun err ->
                        on_result
                          (Error_types.Failure
                             (Error_types.Network_error (Error_types.Connection_failed err)))))
                  (fun err ->
                    on_result
                      (Error_types.Failure
                         (Error_types.Network_error (Error_types.Connection_failed err)))))
              (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err)))
          )
          (fun err -> on_result (Error_types.Failure err))
  
  (** Post thread (LinkedIn doesn't support threads, posts only first item)

      @param account_id The account identifier
      @param texts List of post texts (only first is used)
      @param media_urls_per_post Media URLs for each post
      @param author_urn Optional explicit author URN forwarded to post_single
      @param alt_texts_per_post Alt texts for each post's media
      @param media_kinds_per_post Media kinds for each post's media, forwarded to
             post_single as [?media_kinds]
      @param title Optional article title forwarded to post_single (used only when the
             first post is a link-preview article with no media).
      @param link_preview Forwarded to post_single. Default: true
      @param validate_media_before_upload When true, validates media after download. Default: false
      @param on_result Callback receiving the outcome with thread_result
  *)
  let post_thread ~account_id ~texts ~media_urls_per_post ?author_urn ?(alt_texts_per_post=[]) ?(media_kinds_per_post=[]) ?title ?link_preview ?(validate_media_before_upload=false) on_result =
    let media_counts = List.map List.length media_urls_per_post in
    match validate_thread ~texts ~media_counts () with
    | Error errs ->
        on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        let first_text = List.hd texts in
        let first_or_empty = function [] -> [] | first :: _ -> first in
        let first_media = first_or_empty media_urls_per_post in
        let first_alt_texts = first_or_empty alt_texts_per_post in
        let first_media_kinds = first_or_empty media_kinds_per_post in
        post_single ~account_id ~text:first_text ~media_urls:first_media ?author_urn ~alt_texts:first_alt_texts ~media_kinds:first_media_kinds ?title ?link_preview ~validate_media_before_upload
          (fun outcome ->
            match outcome with
            | Error_types.Success post_id ->
                let result = { 
                  Error_types.posted_ids = [post_id];
                  failed_at_index = None;
                  total_requested = List.length texts;
                } in
                (* If more than one text was provided, add a warning that only first was posted *)
                if List.length texts > 1 then
                  on_result (Error_types.Partial_success { 
                    result; 
                    warnings = [Error_types.Enrichment_skipped 
                      "LinkedIn does not support threads - only first post was published"]
                  })
                else
                  on_result (Error_types.Success result)
            | Error_types.Partial_success { result = post_id; warnings } ->
                let result = { 
                  Error_types.posted_ids = [post_id];
                  failed_at_index = None;
                  total_requested = List.length texts;
                } in
                let extra_warning = 
                  if List.length texts > 1 then
                    [Error_types.Enrichment_skipped 
                      "LinkedIn does not support threads - only first post was published"]
                  else []
                in
                on_result (Error_types.Partial_success { result; warnings = warnings @ extra_warning })
            | Error_types.Failure err ->
                on_result (Error_types.Failure err))
  
  (** OAuth authorization URL.

      @param scopes Override the default OAuth scopes. When provided, this list
        is used as-is and [include_organization_scopes] is ignored.
      @param include_organization_scopes When [scopes] is not provided, add
        organization management scopes to the defaults. Default: false. *)
  let get_oauth_url ?scopes ?(include_organization_scopes=false) ~redirect_uri ~state on_success on_error =
    let raw_client_id = Config.get_env "LINKEDIN_CLIENT_ID" |> Option.value ~default:"" in
    let raw_configured_redirect_uri = Config.get_env "LINKEDIN_REDIRECT_URI" |> Option.value ~default:"" in
    let client_id = String.trim raw_client_id in
    let configured_redirect_uri = String.trim raw_configured_redirect_uri in

    if raw_client_id = "" then
      on_error "LinkedIn client ID not configured"
    else if has_surrounding_whitespace raw_client_id then
      on_error "LinkedIn client ID must not contain leading or trailing whitespace"
    else if has_surrounding_whitespace raw_configured_redirect_uri then
      on_error "Configured LinkedIn redirect URI must not contain leading or trailing whitespace"
    else if has_surrounding_whitespace redirect_uri then
      on_error "LinkedIn redirect URI must not contain leading or trailing whitespace"
    else if is_blank redirect_uri then
      on_error "LinkedIn redirect URI is required"
    else if has_surrounding_whitespace state then
      on_error "LinkedIn OAuth state must not contain leading or trailing whitespace"
    else if is_blank state then
      on_error "LinkedIn OAuth state is required"
    else if configured_redirect_uri <> "" && redirect_uri <> configured_redirect_uri then
      on_error "LinkedIn redirect URI does not match configured LINKEDIN_REDIRECT_URI"
    else (
      let resolved_scopes =
        match scopes with
        | Some s -> s
        | None ->
            (* Default LinkedIn OAuth 2.0 scopes for personal posting.

               Required products in LinkedIn Developer Portal:
               - "Sign In with LinkedIn using OpenID Connect" -> openid, profile, email
               - "Share on LinkedIn" -> w_member_social (post as person)

               Note: For organization/company page posting, use a separate implementation
               which requires the Community Management API product. *)
            if include_organization_scopes then
              ["openid"; "profile"; "email"; "w_member_social";
               "r_organization_admin"; "w_organization_social"]
            else
              ["openid"; "profile"; "email"; "w_member_social"]
      in
      let scope_str = String.concat " " resolved_scopes in
      
      let params = [
        ("response_type", "code");
        ("client_id", client_id);
        ("redirect_uri", redirect_uri);
        ("state", state);
        ("scope", scope_str);
      ] in
      
      let query = Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) params) in
      let url = Printf.sprintf "%s/authorization?%s" linkedin_auth_url query in
      on_success url
    )
  
  (** Exchange OAuth code for access token.
      [on_success] receives [(credentials, granted_scope)] where [granted_scope]
      is the [scope] field from LinkedIn's token response ([None] if absent). *)
  let exchange_code ~code ~redirect_uri on_success on_error =
    let raw_client_id = Config.get_env "LINKEDIN_CLIENT_ID" |> Option.value ~default:"" in
    let raw_client_secret = Config.get_env "LINKEDIN_CLIENT_SECRET" |> Option.value ~default:"" in
    let raw_configured_redirect_uri = Config.get_env "LINKEDIN_REDIRECT_URI" |> Option.value ~default:"" in
    let client_id = String.trim raw_client_id in
    let client_secret = String.trim raw_client_secret in
    let configured_redirect_uri = String.trim raw_configured_redirect_uri in
    
    if raw_client_id = "" || raw_client_secret = "" then
      on_error "LinkedIn OAuth credentials not configured"
    else if has_surrounding_whitespace raw_client_id || has_surrounding_whitespace raw_client_secret then
      on_error "LinkedIn OAuth credentials must not contain leading or trailing whitespace"
    else if has_surrounding_whitespace raw_configured_redirect_uri then
      on_error "Configured LinkedIn redirect URI must not contain leading or trailing whitespace"
    else if has_surrounding_whitespace code then
      on_error "LinkedIn authorization code must not contain leading or trailing whitespace"
    else if is_blank code then
      on_error "LinkedIn authorization code is required"
    else if has_surrounding_whitespace redirect_uri then
      on_error "LinkedIn redirect URI must not contain leading or trailing whitespace"
    else if is_blank redirect_uri then
      on_error "LinkedIn redirect URI is required"
    else if configured_redirect_uri <> "" && redirect_uri <> configured_redirect_uri then
      on_error "LinkedIn redirect URI does not match configured LINKEDIN_REDIRECT_URI"
    else (
      let body = Social_core.form_urlencode_kvs [
        ("grant_type", "authorization_code");
        ("code", code);
        ("redirect_uri", String.trim redirect_uri);
        ("client_id", client_id);
        ("client_secret", client_secret);
      ] in
      let url = Printf.sprintf "%s/accessToken" linkedin_auth_url in
      let headers = [
        ("Content-Type", "application/x-www-form-urlencoded");
      ] in

      let parse_error body =
        try
          let json = Yojson.Basic.from_string body in
          let open Yojson.Basic.Util in
          let error = json |> member "error" |> to_string_option in
          let error_desc = json |> member "error_description" |> to_string_option in
          let error_msg =
            match error, error_desc with
            | Some err, Some desc -> Printf.sprintf "%s: %s" err desc
            | Some err, None -> err
            | None, _ -> body
          in
          (error, error_msg)
        with _ ->
          (None, body)
      in

      Config.Http.post ~headers ~body url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let access_token = json |> member "access_token" |> to_string |> String.trim in
              if access_token = "" then
                on_error "LinkedIn OAuth response contains empty access token"
              else
              let refresh_token = 
                try
                  let parsed = json |> member "refresh_token" |> to_string |> String.trim in
                  if parsed = "" then None else Some parsed
                with _ -> None
              in
              
              (* CRITICAL: Read actual expires_in from LinkedIn response - MUST be present!
                 LinkedIn's token response ALWAYS includes expires_in in seconds.
                 If it's missing, something is seriously wrong with the OAuth response. *)
              let expires_in_result = 
                try Ok (normalize_expires_in (json |> member "expires_in" |> to_int))
                with _ -> Error "LinkedIn OAuth response missing 'expires_in' field"
              in
              
              match expires_in_result with
              | Error err ->
                  on_error err
              | Ok expires_in ->
              
              let expires_at = 
                let now = Ptime_clock.now () in
                match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
                | Some exp -> Ptime.to_rfc3339 exp
                | None -> Ptime.to_rfc3339 now
              in
              
              let scope = json |> member "scope" |> to_string_option in
              let credentials = {
                access_token;
                refresh_token;
                expires_at = Some expires_at;
                auth_type = auth_type_of_string (json |> member "token_type" |> to_string_option |> Option.value ~default:"Bearer");
                scope;
              } in
              on_success (credentials, scope)
            with e ->
              on_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))
          else
            let _, error_msg = parse_error response.body in
            on_error (Printf.sprintf "LinkedIn OAuth exchange failed (%d): %s" response.status error_msg))
        on_error
    )

  let get_organization_access ~account_id ?role ?(acl_state="APPROVED") on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let normalized_role = normalize_acl_role_filter role in
        let normalized_state = normalize_acl_state_filter (Some acl_state) in
        fetch_organization_accesses_by_token ?role:normalized_role ?state:normalized_state
          ~access_token on_result)
      (fun err -> on_result (Error err))

  let get_preferred_organization_access ~account_id ?role ?(acl_state="APPROVED") on_result =
    get_organization_access ~account_id ?role ~acl_state
      (function
        | Ok organizations -> on_result (Ok (select_preferred_organization_access organizations))
        | Error err -> on_result (Error err))

  let exchange_code_and_get_organizations ~code ~redirect_uri ?role ?(acl_state="APPROVED")
      on_success on_error =
    exchange_code ~code ~redirect_uri
      (fun (creds, _scope) ->
        let normalized_role = normalize_acl_role_filter role in
        let normalized_state = normalize_acl_state_filter (Some acl_state) in
        fetch_organization_accesses_by_token ?role:normalized_role ?state:normalized_state
          ~access_token:creds.access_token
          (function
            | Ok organizations -> on_success (creds, organizations)
            | Error err -> on_error (Error_types.error_to_string err)))
      on_error

  let exchange_code_and_get_preferred_organization ~code ~redirect_uri ?role ?(acl_state="APPROVED")
      on_success on_error =
    exchange_code_and_get_organizations ~code ~redirect_uri ?role ~acl_state
      (fun (creds, organizations) ->
        on_success (creds, select_preferred_organization_access organizations))
      on_error

  let validate_oauth_state ~expected ~received on_success on_error =
    let constant_time_equal a b =
      let len = String.length a in
      if len <> String.length b then false
      else
        let diff = ref 0 in
        for i = 0 to len - 1 do
          diff := !diff lor (Char.code a.[i] lxor Char.code b.[i])
        done;
        !diff = 0
    in
    if is_blank expected || is_blank received then
      on_error "OAuth state is required"
    else if has_surrounding_whitespace expected || has_surrounding_whitespace received then
      on_error "OAuth state must not contain leading or trailing whitespace"
    else if String.length expected <> String.length received then
      on_error "OAuth state mismatch"
    else if not (constant_time_equal expected received) then
      on_error "OAuth state mismatch"
    else
      on_success ()
  
  (** Validate content length *)
  let validate_content ~text =
    let len = String.length text in
    if len = 0 then
      Error "Text cannot be empty"
    else if len > 3000 then
      Error (Printf.sprintf "Text too long: %d characters (max 3000)" len)
    else
      Ok ()
  
  (** {1 Profile API} *)
  
  (** Get current user's profile information using OpenID Connect
      
      This uses the /userinfo endpoint which requires the 'openid' and 'profile' scopes.
      Returns basic profile information including user ID, name, email, and profile picture.
  *)
  let get_profile ~account_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let url = Printf.sprintf "%s/userinfo" linkedin_api_base in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
          ("LinkedIn-Version", linkedin_version_header ());
        ] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              (match parse_userinfo response.body with
               | Stdlib.Ok profile -> on_result (Ok profile)
               | Stdlib.Error message ->
                   on_result (Error (Error_types.Internal_error ("Failed to parse profile: " ^ message))))
            else
              on_result (Error (parse_api_error ~required_scopes:["openid"; "profile"] ~status_code:response.status ~body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** {1 Posts API} *)

  (** Parse a post from the modern /rest/posts response format *)
  let parse_post_from_rest json =
    let open Yojson.Basic.Util in
    {
      id = json |> member "id" |> to_string;
      author = json |> member "author" |> to_string;
      created_at = (try
        json |> member "createdAt" |> to_int_option
        |> Option.map string_of_int
      with _ -> None);
      text = json |> member "commentary" |> to_string_option;
      visibility = json |> member "visibility" |> to_string_option;
      lifecycle_state = json |> member "lifecycleState" |> to_string_option;
    }

  let get_post ~account_id ~post_urn on_result =
    match validate_required_urn ~field_name:"LinkedIn post URN" post_urn with
    | Some err -> on_result (Error (Error_types.Internal_error err))
    | None ->
    ensure_valid_token ~account_id
      (fun access_token ->
        let url = Printf.sprintf "%s/posts/%s" linkedin_rest_base (Uri.pct_encode post_urn) in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
          ("X-Restli-Protocol-Version", "2.0.0");
          ("LinkedIn-Version", linkedin_version_header ());
        ] in

        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let post = parse_post_from_rest json in
                on_result (Ok post)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse post: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~required_scopes:["r_member_social"] ~status_code:response.status ~body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Get user's posts with pagination
      
      Fetches posts authored by the current user. Returns a collection with paging support.
      @param start Starting index (default: 0)
      @param count Number of posts to fetch (default: 10, max: 50)
  *)
  let get_posts ~account_id ?(start=0) ?(count=10) on_result =
    let start = normalize_start start in
    let count = normalize_count ~max_count:50 count in
    ensure_valid_token ~account_id
      (fun access_token ->
        get_person_urn ~access_token
          (fun person_urn ->
            match validate_restli_list_item ~field_name:"LinkedIn person URN" person_urn with
            | Some err -> on_result (Error (Error_types.Internal_error err))
            | None ->
            (* Build query parameters for filtering by author *)
            let authors_param = Printf.sprintf "List(%s)" person_urn in
            let query_params = [
              ("q", "authors");
              ("authors", authors_param);
              ("start", string_of_int start);
              ("count", string_of_int count);
            ] in
            let query_string = Uri.encoded_of_query
              (List.map (fun (k, v) -> (k, [v])) query_params) in

            let url = Printf.sprintf "%s/posts?%s" linkedin_rest_base query_string in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_token);
              ("X-Restli-Protocol-Version", "2.0.0");
              ("LinkedIn-Version", linkedin_version_header ());
              ("X-RestLi-Method", "FINDER");
            ] in

            Config.Http.get ~headers url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  try
                    let json = Yojson.Basic.from_string response.body in
                    let open Yojson.Basic.Util in

                    (* Parse elements using modern format *)
                    let elements_json = json |> member "elements" |> to_list in
                    let posts = List.map parse_post_from_rest elements_json in

                    (* Parse paging *)
                    let paging = try
                      let paging_json = json |> member "paging" in
                      Some {
                        start = paging_json |> member "start" |> to_int;
                        count = paging_json |> member "count" |> to_int;
                        total = paging_json |> member "total" |> to_int_option;
                      }
                    with _ -> None in

                    let collection = {
                      elements = posts;
                      paging = paging;
                      metadata = None;
                    } in
                    on_result (Ok collection)
                  with e ->
                    on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse posts: %s" (Printexc.to_string e))))
                else
                  on_result (Error (parse_api_error ~required_scopes:["r_member_social"] ~status_code:response.status ~body:response.body)))
              (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))
  
  (** {1 Scroller Pattern for Pagination} *)
  
  (** Scroller for paginated post fetching
      
      Provides a convenient interface for navigating through pages of posts.
      Usage:
        let scroller = create_posts_scroller ~account_id ~page_size:10 ()
        scroller.scroll_next (fun result -> ...)
  *)
  type 'a scroller = {
    scroll_next: (('a collection_response, Error_types.error) result -> unit) -> unit;
    scroll_back: (('a collection_response, Error_types.error) result -> unit) -> unit;
    current_position: unit -> int;
    has_more: unit -> bool;
  }
  
  (** Create a scroller for user's posts *)
  let create_posts_scroller ~account_id ?(page_size=10) () =
    let page_size = max 1 page_size in
    let current_start = ref 0 in
    let last_total = ref None in
    
    let scroll_next on_result =
      get_posts ~account_id ~start:!current_start ~count:page_size
        (fun result ->
          (match result with
          | Ok collection ->
              (* Update state *)
              (match collection.paging with
              | Some p -> 
                  current_start := p.start + p.count;
                  last_total := p.total
              | None -> 
                  current_start := !current_start + (List.length collection.elements))
          | Error _ -> ());
          on_result result)
    in
    
    let scroll_back on_result =
      let new_start = max 0 (!current_start - page_size) in
      get_posts ~account_id ~start:new_start ~count:page_size
        (fun result ->
          (match result with
          | Ok collection ->
              (match collection.paging with
              | Some p -> current_start := p.start + p.count
              | None -> current_start := new_start + (List.length collection.elements))
          | Error _ -> ());
          on_result result)
    in
    
    let current_position () = !current_start in
    
    let has_more () = 
      match !last_total with
      | Some total -> !current_start < total
      | None -> true  (* Unknown, assume there might be more *)
    in
    
    { scroll_next; scroll_back; current_position; has_more }
  
  (** {1 Batch Operations} *)
  
  (** Batch get posts by URNs
      
      Efficiently fetch multiple posts in a single API call.
      @param post_urns List of post URNs to fetch
  *)
  let batch_get_posts ~account_id ~post_urns on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        if List.length post_urns = 0 then
          on_result (Ok [])
        else if List.exists (fun urn -> is_blank urn || has_surrounding_whitespace urn || has_forbidden_restli_list_chars urn) post_urns then
          on_result
            (Error
               (Error_types.Internal_error
                  "Invalid post URN for batch get: values must be non-blank, trimmed, and must not contain ',', '(' or ')'"))
        else
          let ids_param = Printf.sprintf "List(%s)" (String.concat "," post_urns) in
          let query_string = Uri.encoded_of_query [ ("ids", [ids_param]) ] in
          let url = Printf.sprintf "%s/posts?%s" linkedin_rest_base query_string in
          let headers = [
            ("Authorization", Printf.sprintf "Bearer %s" access_token);
            ("X-Restli-Protocol-Version", "2.0.0");
            ("LinkedIn-Version", linkedin_version_header ());
            ("X-RestLi-Method", "BATCH_GET");
          ] in

          Config.Http.get ~headers url
            (fun response ->
              if response.status >= 200 && response.status < 300 then
                (* Parse response first, then call callback outside try/with *)
                match
                  try
                    let json = Yojson.Basic.from_string response.body in
                    let open Yojson.Basic.Util in
                    let results = json |> member "results" |> to_assoc in

                    Ok (List.filter_map (fun (_id, post_json) ->
                      try Some (parse_post_from_rest post_json)
                      with _ -> None
                    ) results)
                  with e ->
                    Error (Error_types.Internal_error (Printf.sprintf "Failed to parse batch results: %s" (Printexc.to_string e)))
                with
                | Ok posts -> on_result (Ok posts)
                | Error err -> on_result (Error err)
              else
                on_result (Error (parse_api_error ~required_scopes:["r_member_social"] ~status_code:response.status ~body:response.body)))
            (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** {1 Search API (FINDER Pattern)} *)
  
  (** Search posts with custom criteria
      
      Uses LinkedIn's authors finder on /rest/posts.
      Keyword finder search is intentionally rejected because it is not
      supported by the posts finder contract.
      
      @param keywords Optional keyword input (currently unsupported; returns error)
      @param author Optional author URN to filter by; defaults to current member
      @param start Starting index
      @param count Results per page
  *)
  let search_posts ~account_id ?keywords ?author ?(start=0) ?(count=10) on_result =
    let start = normalize_start start in
    let count = normalize_count ~max_count:50 count in
    ensure_valid_token ~account_id
      (fun access_token ->
        let fetch_for_author author_urn =
          match validate_restli_list_item ~field_name:"LinkedIn author URN" author_urn with
          | Some err -> on_result (Error (Error_types.Internal_error err))
          | None ->
              let query_params = [
                ("q", "authors");
                ("authors", Printf.sprintf "List(%s)" author_urn);
                ("start", string_of_int start);
                ("count", string_of_int count);
              ] in
              let query_string =
                Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) query_params)
              in
              let url = Printf.sprintf "%s/posts?%s" linkedin_rest_base query_string in
              let headers = [
                ("Authorization", Printf.sprintf "Bearer %s" access_token);
                ("X-Restli-Protocol-Version", "2.0.0");
                ("LinkedIn-Version", linkedin_version_header ());
                ("X-RestLi-Method", "FINDER");
              ] in
              Config.Http.get ~headers url
                (fun response ->
                  if response.status >= 200 && response.status < 300 then
                    try
                      let json = Yojson.Basic.from_string response.body in
                      let open Yojson.Basic.Util in
                      let elements_json = json |> member "elements" |> to_list in
                      let posts = List.map parse_post_from_rest elements_json in
                      let paging = try
                        let paging_json = json |> member "paging" in
                        Some {
                          start = paging_json |> member "start" |> to_int;
                          count = paging_json |> member "count" |> to_int;
                          total = paging_json |> member "total" |> to_int_option;
                        }
                      with _ -> None in
                      on_result (Ok { elements = posts; paging; metadata = None })
                    with e ->
                      on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse search results: %s" (Printexc.to_string e))))
                  else
                    on_result (Error (parse_api_error ~required_scopes:["r_member_social"] ~status_code:response.status ~body:response.body)))
                (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err))))
        in
        match keywords with
        | Some _ ->
            on_result
              (Error
                 (Error_types.Internal_error
                    "LinkedIn Posts API does not support keyword finder search; use author filtering"))
        | None ->
            (match author with
            | Some auth -> fetch_for_author auth
            | None ->
                get_person_urn ~access_token
                  (fun person_urn -> fetch_for_author person_urn)
                  (fun err -> on_result (Error (Error_types.Internal_error err)))) )
      (fun err -> on_result (Error err))
  
  (** Create a scroller for post search *)
  let create_search_scroller ~account_id ?keywords ?author ?(page_size=10) () =
    let page_size = max 1 page_size in
    let current_start = ref 0 in
    let last_total = ref None in
    
    let scroll_next on_result =
      search_posts ~account_id ?keywords ?author ~start:!current_start ~count:page_size
        (fun result ->
          (match result with
          | Ok collection ->
              (match collection.paging with
              | Some p -> 
                  current_start := p.start + p.count;
                  last_total := p.total
              | None -> 
                  current_start := !current_start + (List.length collection.elements))
          | Error _ -> ());
          on_result result)
    in
    
    let scroll_back on_result =
      let new_start = max 0 (!current_start - page_size) in
      search_posts ~account_id ?keywords ?author ~start:new_start ~count:page_size
        (fun result ->
          (match result with
          | Ok collection ->
              (match collection.paging with
              | Some p -> current_start := p.start + p.count
              | None -> current_start := new_start + (List.length collection.elements))
          | Error _ -> ());
          on_result result)
    in
    
    let current_position () = !current_start in
    let has_more () = 
      match !last_total with
      | Some total -> !current_start < total
      | None -> true
    in
    
    { scroll_next; scroll_back; current_position; has_more }
  
  (** {1 Engagement API} *)
  
  (** Like a post
      
      Adds a like/reaction to the specified post.
      @param post_urn The URN of the post to like
  *)
  let like_post ~account_id ~post_urn on_result =
    match validate_required_urn ~field_name:"LinkedIn post URN" post_urn with
    | Some err -> on_result (Error (Error_types.Internal_error err))
    | None ->
    ensure_valid_token ~account_id
      (fun access_token ->
        get_person_urn ~access_token
          (fun person_urn ->
            let like_body = `Assoc [
              ("actor", `String person_urn);
              ("object", `String post_urn);
            ] in
            
            let url = Printf.sprintf "%s/socialActions/%s/likes"
              linkedin_rest_base (Uri.pct_encode post_urn) in
            let body = Yojson.Basic.to_string like_body in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_token);
              ("Content-Type", "application/json");
              ("X-Restli-Protocol-Version", "2.0.0");
              ("LinkedIn-Version", linkedin_version_header ());
            ] in

            Config.Http.post ~headers ~body url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  on_result (Ok ())
                else
                  on_result (Error (parse_api_error ~required_scopes:["w_member_social"] ~status_code:response.status ~body:response.body)))
              (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  (** Unlike a post
      
      Removes a like/reaction from the specified post.
      @param post_urn The URN of the post to unlike
  *)
  let unlike_post ~account_id ~post_urn on_result =
    match validate_required_urn ~field_name:"LinkedIn post URN" post_urn with
    | Some err -> on_result (Error (Error_types.Internal_error err))
    | None ->
    ensure_valid_token ~account_id
      (fun access_token ->
        get_person_urn ~access_token
          (fun person_urn ->
            (* Build the like URN: urn:li:like:(actor,object) *)
            let like_id = Printf.sprintf "(%s,%s)" person_urn post_urn in
            let url = Printf.sprintf "%s/socialActions/%s/likes/%s"
              linkedin_rest_base
              (Uri.pct_encode post_urn)
              (Uri.pct_encode like_id) in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_token);
              ("X-Restli-Protocol-Version", "2.0.0");
              ("LinkedIn-Version", linkedin_version_header ());
            ] in
            
            Config.Http.delete ~headers url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  on_result (Ok ())
                else
                  on_result (Error (parse_api_error ~required_scopes:["w_member_social"] ~status_code:response.status ~body:response.body)))
              (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))
  
  (** Comment on a post
      
      Adds a comment to the specified post.
      @param post_urn The URN of the post
      @param text The comment text
  *)
  let comment_on_post ~account_id ~post_urn ~text on_result =
    if is_blank text then
      on_result (Error (Error_types.Internal_error "LinkedIn comment text is required"))
    else
    match validate_required_urn ~field_name:"LinkedIn post URN" post_urn with
    | Some err -> on_result (Error (Error_types.Internal_error err))
    | None ->
    ensure_valid_token ~account_id
      (fun access_token ->
        get_person_urn ~access_token
          (fun person_urn ->
            let comment_body = `Assoc [
              ("actor", `String person_urn);
              ("object", `String post_urn);
              ("message", `Assoc [
                ("text", `String text);
              ]);
            ] in
            
            let url = Printf.sprintf "%s/socialActions/%s/comments"
              linkedin_rest_base (Uri.pct_encode post_urn) in
            let body = Yojson.Basic.to_string comment_body in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_token);
              ("Content-Type", "application/json");
              ("X-Restli-Protocol-Version", "2.0.0");
              ("LinkedIn-Version", linkedin_version_header ());
            ] in
            
            Config.Http.post ~headers ~body url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  try
                    let json = Yojson.Basic.from_string response.body in
                    let comment_id = json 
                      |> Yojson.Basic.Util.member "id" 
                      |> Yojson.Basic.Util.to_string in
                    on_result (Ok comment_id)
                  with _ ->
                    (* If we can't parse the ID, just return success *)
                    on_result (Ok "unknown")
                else
                  on_result (Error (parse_api_error ~required_scopes:["w_member_social"] ~status_code:response.status ~body:response.body)))
              (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))
  
  (** Get comments on a post
      
      Fetches comments for a specific post with pagination.
      @param post_urn The URN of the post
      @param start Starting index
      @param count Number of comments to fetch
  *)
  let get_post_comments ~account_id ~post_urn ?(start=0) ?(count=10) on_result =
    match validate_required_urn ~field_name:"LinkedIn post URN" post_urn with
    | Some err -> on_result (Error (Error_types.Internal_error err))
    | None ->
    let start = normalize_start start in
    let count = normalize_count ~max_count:100 count in
    ensure_valid_token ~account_id
      (fun access_token ->
        let query_params = [
          ("start", string_of_int start);
          ("count", string_of_int count);
        ] in
        let query_string = Uri.encoded_of_query 
          (List.map (fun (k, v) -> (k, [v])) query_params) in
        
        let url = Printf.sprintf "%s/socialActions/%s/comments?%s"
          linkedin_rest_base (Uri.pct_encode post_urn) query_string in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
          ("X-Restli-Protocol-Version", "2.0.0");
          ("LinkedIn-Version", linkedin_version_header ());
        ] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let open Yojson.Basic.Util in
                
                let elements_json = json |> member "elements" |> to_list in
                let comments = List.map (fun elem ->
                  {
                    id = elem |> member "id" |> to_string;
                    actor = elem |> member "actor" |> to_string;
                    text = elem |> member "message" |> member "text" |> to_string;
                    created_at = (try elem |> member "created" |> member "time" |> to_string_option with _ -> None);
                  }
                ) elements_json in
                
                let paging = try
                  let paging_json = json |> member "paging" in
                  Some {
                    start = paging_json |> member "start" |> to_int;
                    count = paging_json |> member "count" |> to_int;
                    total = paging_json |> member "total" |> to_int_option;
                  }
                with _ -> None in
                
                let collection = {
                  elements = comments;
                  paging = paging;
                  metadata = None;
                } in
                on_result (Ok collection)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse comments: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~required_scopes:["r_member_social"] ~status_code:response.status ~body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Get engagement statistics for a post
      
      Fetches like count, comment count, and other engagement metrics.
      Note: This may require additional API permissions.
      @param post_urn The URN of the post
  *)
  let get_post_engagement ~account_id ~post_urn on_result =
    match validate_required_urn ~field_name:"LinkedIn post URN" post_urn with
    | Some err -> on_result (Error (Error_types.Internal_error err))
    | None ->
    ensure_valid_token ~account_id
      (fun access_token ->
        let url = Printf.sprintf "%s/socialMetadata/%s"
          linkedin_rest_base (Uri.pct_encode post_urn) in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
          ("X-Restli-Protocol-Version", "2.0.0");
          ("LinkedIn-Version", linkedin_version_header ());
        ] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let open Yojson.Basic.Util in
                
                (* socialMetadata returns reactionSummaries and commentSummary *)
                let total_reactions =
                  try
                    let summaries = json |> member "reactionSummaries" |> to_assoc in
                    let total = List.fold_left (fun acc (_key, v) ->
                      acc + (try v |> member "count" |> to_int with _ -> 0)
                    ) 0 summaries in
                    Some total
                  with _ -> None
                in
                let comment_count =
                  try Some (json |> member "commentSummary" |> member "count" |> to_int)
                  with _ -> None
                in
                let engagement : engagement_info = {
                  like_count = total_reactions;
                  comment_count = comment_count;
                  share_count = None;
                  impression_count = None;
                } in
                on_result (Ok engagement)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse engagement: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~required_scopes:["r_member_social"] ~status_code:response.status ~body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  let get_account_analytics ~account_id ~entity_urn on_result =
    match validate_author_urn entity_urn with
    | Some err -> on_result (Error (Error_types.Internal_error err))
    | None ->
        ensure_valid_token ~account_id
          (fun access_token ->
            let finder_query =
              Uri.encoded_of_query
                [ ("q", ["organizationalEntity"]);
                  ("organizationalEntity", [entity_urn]) ]
            in
            let follower_url =
              Printf.sprintf "%s/organizationalEntityFollowerStatistics?%s"
                linkedin_rest_base
                finder_query
            in
            let share_url =
              Printf.sprintf "%s/organizationalEntityShareStatistics?%s"
                linkedin_rest_base
                finder_query
            in
            let headers =
              [ ("Authorization", Printf.sprintf "Bearer %s" access_token);
                ("X-Restli-Protocol-Version", "2.0.0");
                ("LinkedIn-Version", linkedin_version_header ());
                ("X-RestLi-Method", "FINDER") ]
            in

            let int_of_json_option json =
              let open Yojson.Basic.Util in
              try Some (json |> to_int)
              with _ ->
                try Some (json |> to_float |> int_of_float)
                with _ ->
                  try Some (json |> to_string |> int_of_string)
                  with _ -> None
            in

            let member_path json path =
              List.fold_left
                (fun acc key ->
                  let open Yojson.Basic.Util in
                  acc |> member key)
                json
                path
            in

            let first_element json =
              let open Yojson.Basic.Util in
              match json |> member "elements" |> to_list with
              | first :: _ -> Some first
              | [] -> None
            in

            let parse_follower_count body =
              try
                let json = Yojson.Basic.from_string body in
                match first_element json with
                | None -> None
                | Some elem ->
                    [ ["followerCounts"; "organicFollowerCount"];
                      ["followerCounts"; "followerCounts"];
                      ["followerCounts"; "totalFollowerCounts"];
                      ["followerCounts"; "totalFollowerCount"] ]
                    |> List.find_map (fun path -> int_of_json_option (member_path elem path))
              with _ -> None
            in

            let parse_share_metrics body =
              try
                let json = Yojson.Basic.from_string body in
                match first_element json with
                | None -> (None, None, None, None, None, None)
                | Some elem ->
                    let stats = member_path elem ["totalShareStatistics"] in
                    let impressions = int_of_json_option (member_path stats ["impressionCount"]) in
                    let unique_impressions =
                      int_of_json_option (member_path stats ["uniqueImpressionsCount"])
                    in
                    let shares = int_of_json_option (member_path stats ["shareCount"]) in
                    let clicks = int_of_json_option (member_path stats ["clickCount"]) in
                    let likes = int_of_json_option (member_path stats ["likeCount"]) in
                    let comments = int_of_json_option (member_path stats ["commentCount"]) in
                    (impressions, unique_impressions, shares, clicks, likes, comments)
              with _ -> (None, None, None, None, None, None)
            in

            Config.Http.get ~headers follower_url
              (fun follower_response ->
                if follower_response.status >= 200 && follower_response.status < 300 then
                  let follower_count = parse_follower_count follower_response.body in
                  Config.Http.get ~headers share_url
                    (fun share_response ->
                      if share_response.status >= 200 && share_response.status < 300 then
                        let impression_count, unique_impression_count, share_count, click_count,
                            like_count, comment_count =
                          parse_share_metrics share_response.body
                        in
                        let analytics : account_analytics =
                          {
                            entity_urn;
                            follower_count;
                            impression_count;
                            unique_impression_count;
                            share_count;
                            click_count;
                            like_count;
                            comment_count;
                          }
                        in
                        on_result (Ok analytics)
                      else
                        on_result
                          (Error
                             (parse_api_error
                                ~required_scopes:[ "r_organization_admin"; "r_member_social" ]
                                ~status_code:share_response.status
                                ~body:share_response.body)))
                    (fun err ->
                      on_result
                        (Error (Error_types.Network_error (Error_types.Connection_failed err))))
                else
                  on_result
                    (Error
                       (parse_api_error
                          ~required_scopes:[ "r_organization_admin"; "r_member_social" ]
                          ~status_code:follower_response.status
                          ~body:follower_response.body)))
              (fun err ->
                on_result
                  (Error (Error_types.Network_error (Error_types.Connection_failed err))))
          )
          (fun err -> on_result (Error err))

  (** {1 Post Deletion} *)

  (** Delete a post by URN

      Deletes a post using the modern /rest/posts endpoint.
      @param post_urn The URN of the post to delete (e.g., "urn:li:share:123456")
  *)
  let delete_post ~account_id ~post_urn on_result =
    match validate_required_urn ~field_name:"LinkedIn post URN" post_urn with
    | Some err -> on_result (Error (Error_types.Internal_error err))
    | None ->
    ensure_valid_token ~account_id
      (fun access_token ->
        let url = Printf.sprintf "%s/posts/%s" linkedin_rest_base (Uri.pct_encode post_urn) in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
          ("X-Restli-Protocol-Version", "2.0.0");
          ("LinkedIn-Version", linkedin_version_header ());
        ] in

        Config.Http.delete ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_result (Ok ())
            else
              on_result (Error (parse_api_error ~required_scopes:["w_member_social"] ~status_code:response.status ~body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** {1 Document/Carousel Posts} *)

  (** Post a document (PDF carousel) to LinkedIn

      Uploads a document via /rest/documents and creates a post with document content.
      @param account_id The account identifier
      @param text The post text/commentary
      @param document_url URL of the document (PDF) to upload
      @param document_title Title for the document
      @param author_urn Optional explicit author URN
      @param on_result Callback receiving the outcome with post ID
  *)
  let post_document ~account_id ~text ~document_url ~document_title ?author_urn on_result =
    let author_validation =
      match author_urn with
      | None -> Ok None
      | Some explicit_author ->
          (match validate_author_urn explicit_author with
          | Some err -> Error err
          | None -> Ok (Some explicit_author))
    in
    match validate_post ~text (), author_validation with
    | Error errs, _ ->
        on_result (Error_types.Failure (Error_types.Validation_error errs))
    | _, Error err ->
        on_result (Error_types.Failure (Error_types.Internal_error err))
    | Ok (), Ok validated_author_urn ->
        ensure_valid_token ~account_id
          (fun access_token ->
            let resolve_author_urn on_success_author on_error_author =
              match validated_author_urn with
              | Some explicit_author -> on_success_author explicit_author
              | None -> get_person_urn ~access_token on_success_author on_error_author
            in
            resolve_author_urn
              (fun resolved_author_urn ->
                (* Step 1: Download the document *)
                Config.Http.get ~headers:[] document_url
                  (fun doc_response ->
                    if doc_response.status >= 200 && doc_response.status < 300 then
                      (* Step 2: Register document upload *)
                      register_upload ~access_token ~owner_urn:resolved_author_urn ~media_type:"document"
                        (fun (document_urn, upload_url) ->
                          (* Step 3: Upload the document binary *)
                          upload_binary ~access_token ~upload_url ~media_data:doc_response.body
                            (fun () ->
                              (* Step 4: Create the post with document content *)
                              let post_body = `Assoc [
                                ("author", `String resolved_author_urn);
                                ("commentary", `String (escape_little_text_format text));
                                ("visibility", `String "PUBLIC");
                                ("distribution", `Assoc [
                                  ("feedDistribution", `String "MAIN_FEED");
                                  ("targetEntities", `List []);
                                  ("thirdPartyDistributionChannels", `List []);
                                ]);
                                ("lifecycleState", `String "PUBLISHED");
                                ("content", `Assoc [
                                  ("media", `Assoc [
                                    ("id", `String document_urn);
                                    ("title", `String document_title);
                                  ]);
                                ]);
                              ] in

                              let url = Printf.sprintf "%s/posts" linkedin_rest_base in
                              let body = Yojson.Basic.to_string post_body in
                              let headers = [
                                ("Authorization", Printf.sprintf "Bearer %s" access_token);
                                ("Content-Type", "application/json");
                                ("X-Restli-Protocol-Version", "2.0.0");
                                ("LinkedIn-Version", linkedin_version_header ());
                              ] in

                              Config.Http.post ~headers ~body url
                                (fun response ->
                                  if response.status >= 200 && response.status < 300 then
                                    let post_id =
                                      try
                                        let json = Yojson.Basic.from_string response.body in
                                        json |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_string
                                      with _ ->
                                        (try List.assoc "x-restli-id" response.headers
                                        with Not_found -> "unknown")
                                    in
                                    on_result (Error_types.Success post_id)
                                  else
                                    let required_scopes =
                                      if is_organization_urn resolved_author_urn || is_organization_brand_urn resolved_author_urn
                                      then ["w_organization_social"]
                                      else ["w_member_social"]
                                    in
                                    on_result (Error_types.Failure
                                      (parse_api_error ~required_scopes ~status_code:response.status ~body:response.body)))
                                (fun err ->
                                  on_result (Error_types.Failure
                                    (Error_types.Network_error (Error_types.Connection_failed err)))))
                            (fun err ->
                              on_result (Error_types.Failure
                                (Error_types.Network_error (Error_types.Connection_failed err)))))
                        (fun err ->
                          on_result (Error_types.Failure
                            (Error_types.Network_error (Error_types.Connection_failed err))))
                    else
                      on_result (Error_types.Failure
                        (Error_types.Network_error (Error_types.Connection_failed
                          (Printf.sprintf "Failed to download document from %s (%d)" document_url doc_response.status)))))
                  (fun err ->
                    on_result (Error_types.Failure
                      (Error_types.Network_error (Error_types.Connection_failed err)))))
              (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err)))
          )
          (fun err -> on_result (Error_types.Failure err))

  (** {1 Poll Posts} *)

  (** Create a poll post on LinkedIn

      Creates a post with a poll using the modern /rest/posts endpoint.
      @param account_id The account identifier
      @param text The post text/commentary
      @param question The poll question
      @param options List of poll option strings (2-4 options)
      @param duration Duration in days: 1, 3, 7, or 14
      @param author_urn Optional explicit author URN
      @param on_result Callback receiving the outcome with post ID
  *)
  let post_poll ~account_id ~text ~question ~options ~duration ?author_urn on_result =
    let author_validation =
      match author_urn with
      | None -> Ok None
      | Some explicit_author ->
          (match validate_author_urn explicit_author with
          | Some err -> Error err
          | None -> Ok (Some explicit_author))
    in
    (* Validate poll constraints *)
    let poll_validation =
      let num_options = List.length options in
      if num_options < 2 then Error "Poll must have at least 2 options"
      else if num_options > 4 then Error "Poll must have at most 4 options"
      else if List.exists (fun o -> String.trim o = "") options then Error "Poll options must not be blank"
      else if not (List.mem duration [1; 3; 7; 14]) then Error "Poll duration must be 1, 3, 7, or 14 days"
      else if String.trim question = "" then Error "Poll question must not be blank"
      else Ok ()
    in
    match validate_post ~text (), author_validation, poll_validation with
    | Error errs, _, _ ->
        on_result (Error_types.Failure (Error_types.Validation_error errs))
    | _, Error err, _ ->
        on_result (Error_types.Failure (Error_types.Internal_error err))
    | _, _, Error err ->
        on_result (Error_types.Failure (Error_types.Internal_error err))
    | Ok (), Ok validated_author_urn, Ok () ->
        ensure_valid_token ~account_id
          (fun access_token ->
            let resolve_author_urn on_success_author on_error_author =
              match validated_author_urn with
              | Some explicit_author -> on_success_author explicit_author
              | None -> get_person_urn ~access_token on_success_author on_error_author
            in
            resolve_author_urn
              (fun resolved_author_urn ->
                let duration_str = match duration with
                  | 1 -> "ONE_DAY"
                  | 3 -> "THREE_DAYS"
                  | 7 -> "ONE_WEEK"
                  | 14 -> "TWO_WEEKS"
                  | _ -> "ONE_WEEK"
                in
                let options_json = `List (List.map (fun opt ->
                  `Assoc [("text", `String opt)]
                ) options) in

                let post_body = `Assoc [
                  ("author", `String resolved_author_urn);
                  ("commentary", `String (escape_little_text_format text));
                  ("visibility", `String "PUBLIC");
                  ("distribution", `Assoc [
                    ("feedDistribution", `String "MAIN_FEED");
                    ("targetEntities", `List []);
                    ("thirdPartyDistributionChannels", `List []);
                  ]);
                  ("lifecycleState", `String "PUBLISHED");
                  ("content", `Assoc [
                    ("poll", `Assoc [
                      ("question", `String question);
                      ("options", options_json);
                      ("settings", `Assoc [
                        ("duration", `String duration_str);
                      ]);
                    ]);
                  ]);
                ] in

                let url = Printf.sprintf "%s/posts" linkedin_rest_base in
                let body = Yojson.Basic.to_string post_body in
                let headers = [
                  ("Authorization", Printf.sprintf "Bearer %s" access_token);
                  ("Content-Type", "application/json");
                  ("X-Restli-Protocol-Version", "2.0.0");
                  ("LinkedIn-Version", linkedin_version_header ());
                ] in

                Config.Http.post ~headers ~body url
                  (fun response ->
                    if response.status >= 200 && response.status < 300 then
                      let post_id =
                        try
                          let json = Yojson.Basic.from_string response.body in
                          json |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_string
                        with _ ->
                          (try List.assoc "x-restli-id" response.headers
                          with Not_found -> "unknown")
                      in
                      on_result (Error_types.Success post_id)
                    else
                      let required_scopes =
                        if is_organization_urn resolved_author_urn || is_organization_brand_urn resolved_author_urn
                        then ["w_organization_social"]
                        else ["w_member_social"]
                      in
                      on_result (Error_types.Failure
                        (parse_api_error ~required_scopes ~status_code:response.status ~body:response.body)))
                  (fun err ->
                    on_result (Error_types.Failure
                      (Error_types.Network_error (Error_types.Connection_failed err)))))
              (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err)))
          )
          (fun err -> on_result (Error_types.Failure err))
end

(** LinkedIn provider with {!Default_options}. *)
module Make_with_options (Config : CONFIG) (Options : OPTIONS) =
  Make_coordinated_with_options (struct
  include Config

  let with_refresh_lock ~account_id:_ run on_success on_error =
    run on_success on_error
end) (Options)

module Make (Config : CONFIG) = Make_with_options (Config) (Default_options)

module Make_coordinated (Config : COORDINATED_CONFIG) =
  Make_coordinated_with_options (Config) (Default_options)

(** Coordinated credential operations without legacy account-health writes.
    All typed results, credential persistence, and locking are preserved. *)
module Make_credentials_only (Config : COORDINATED_CONFIG) =
  Make_coordinated (struct
    include Config
    let update_health_status = Health_status.ignore_updates
  end)

(** Explicit provider options without legacy account-health writes. *)
module Make_credentials_only_with_options
    (Config : COORDINATED_CONFIG) (Options : OPTIONS) =
  Make_coordinated_with_options (struct
    include Config
    let update_health_status = Health_status.ignore_updates
  end) (Options)
