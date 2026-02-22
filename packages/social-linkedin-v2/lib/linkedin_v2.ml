(** LinkedIn API v2 Provider
    
    This implementation supports OAuth 2.0 for personal posting.
    Tokens expire after 60 days (5,184,000 seconds).
    
    IMPORTANT: Programmatic token refresh is only available for LinkedIn Partner Program apps.
    Standard apps using "Sign In with LinkedIn" or "Share on LinkedIn" products DO NOT
    have access to programmatic refresh - users must re-authorize through OAuth flow.
*)

open Social_core

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

  let normalize_token_type = function
    | None -> "Bearer"
    | Some raw ->
        let trimmed = String.trim raw in
        if trimmed = "" then "Bearer"
        else if String.lowercase_ascii trimmed = "bearer" then "Bearer"
        else trimmed

  let normalize_expires_in seconds =
    if seconds < 0 then 0 else if seconds > 31_536_000 then 31_536_000 else seconds

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
        @param on_success Continuation receiving credentials
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
      let params = [
        ("grant_type", ["authorization_code"]);
        ("code", [code]);
        ("redirect_uri", [redirect_uri]);
        ("client_id", [client_id]);
        ("client_secret", [client_secret]);
      ] in
      let body = Uri.encoded_of_query params in
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
              
              let token_type = normalize_token_type (json |> member "token_type" |> to_string_option) in
              
              let creds : credentials = {
                access_token;
                refresh_token;
                expires_at;
                token_type;
              } in
              on_success creds
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
      let body = Uri.encoded_of_query [
        ("grant_type", ["refresh_token"]);
        ("refresh_token", [refresh_token]);
        ("client_id", [client_id]);
        ("client_secret", [client_secret]);
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
              let token_type = normalize_token_type (json |> member "token_type" |> to_string_option) in
              let creds : credentials = {
                access_token;
                refresh_token = new_refresh;
                expires_at;
                token_type;
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

(** Profile information from userinfo endpoint (OpenID Connect) *)
type profile_info = {
  sub: string;                    (** Subject identifier (user ID) *)
  name: string option;            (** User's full name *)
  given_name: string option;      (** First name *)
  family_name: string option;     (** Last name *)
  picture: string option;         (** Profile picture URL *)
  email: string option;           (** Email address *)
  email_verified: bool option;    (** Email verification status *)
  locale: string option;          (** User's locale *)
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

(** Make functor to create LinkedIn provider with given configuration *)
module Make (Config : CONFIG) = struct
  let linkedin_api_base = "https://api.linkedin.com/v2"
  let linkedin_rest_base = "https://api.linkedin.com/rest"
  let linkedin_auth_url = "https://www.linkedin.com/oauth/v2"
  
  (** Check if token is expired or expiring soon *)
  let is_token_expired_buffer ~buffer_seconds expires_at_opt =
    match expires_at_opt with
    | None -> false
    | Some expires_at_str ->
        try
          match Ptime.of_rfc3339 expires_at_str with
          | Ok (expires_at, _, _) ->
              let now = Ptime_clock.now () in
              let buffer = Ptime.Span.of_int_s buffer_seconds in
              (match Ptime.add_span now buffer with
               | Some future -> not (Ptime.is_later expires_at ~than:future)
               | None -> false)
          | Error _ -> true
        with _ -> true
  
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

  let normalize_token_type = function
    | None -> "Bearer"
    | Some raw ->
        let trimmed = String.trim raw in
        if trimmed = "" then "Bearer"
        else if String.lowercase_ascii trimmed = "bearer" then "Bearer"
        else trimmed

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
    if status_code = 401 then
      Error_types.Auth_error Error_types.Token_expired
    else if status_code = 403 then
      Error_types.Auth_error (Error_types.Insufficient_permissions required_scopes)
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
      
      let body = Uri.encoded_of_query [
        ("grant_type", ["refresh_token"]);
        ("refresh_token", [refresh_token]);
        ("client_id", [String.trim client_id]);
        ("client_secret", [String.trim client_secret]);
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
    Config.get_credentials ~account_id
      (fun creds ->
        (* Check if token needs refresh (7 days buffer) *)
        if is_token_expired_buffer ~buffer_seconds:604800 creds.expires_at then (
          (* Token expiring soon, refresh it *)
          match creds.refresh_token with
          | None ->
              Config.update_health_status ~account_id ~status:"token_expired" 
                ~error_message:(Some "Token expired - please reconnect (LinkedIn tokens last 60 days)")
                (fun () -> on_error (Error_types.Auth_error Error_types.Token_expired))
                (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err)))
          | Some refresh_token ->
              let client_id = Config.get_env "LINKEDIN_CLIENT_ID" |> Option.value ~default:"" in
              let client_secret = Config.get_env "LINKEDIN_CLIENT_SECRET" |> Option.value ~default:"" in
              
              refresh_access_token ~client_id ~client_secret ~refresh_token
                (fun (new_access, new_refresh, expires_at) ->
                  (* Update stored credentials *)
                  let updated_creds = {
                    access_token = new_access;
                    refresh_token = Some new_refresh;
                    expires_at = Some expires_at;
                    token_type = "Bearer";
                  } in
                  Config.update_credentials ~account_id ~credentials:updated_creds
                    (fun () ->
                      Config.update_health_status ~account_id ~status:"healthy" ~error_message:None
                        (fun () -> on_success new_access)
                        (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err))))
                    (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err))))
                (fun err ->
                  let user_friendly_error = 
                    if String.length err > 100 && 
                       (Str.string_match (Str.regexp ".*[Pp]rogrammatic.*") err 0 ||
                        Str.string_match (Str.regexp ".*[Pp]artner.*") err 0) then
                      "LinkedIn token refresh failed - please reconnect your account"
                    else
                      Printf.sprintf "LinkedIn token refresh failed: %s - please reconnect your account" err
                  in
                  Config.update_health_status ~account_id ~status:"refresh_failed" 
                    ~error_message:(Some user_friendly_error)
                    (fun () -> on_error (Error_types.Auth_error (Error_types.Refresh_failed user_friendly_error)))
                    (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err))))
        ) else (
          (* Token still valid *)
          Config.update_health_status ~account_id ~status:"healthy" ~error_message:None
            (fun () -> on_success creds.access_token)
            (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err)))
        ))
      (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err)))
  
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
  
  (** Register media upload with LinkedIn using modern per-asset-type REST APIs

      Uses /rest/images for images, /rest/videos for videos, /rest/documents for documents.
      Returns (asset_urn, upload_url) for subsequent binary upload.
  *)
  let register_upload ~access_token ~owner_urn ~media_type on_success on_error =
    let endpoint = match media_type with
      | "video" -> Printf.sprintf "%s/videos?action=initializeUpload" linkedin_rest_base
      | "document" -> Printf.sprintf "%s/documents?action=initializeUpload" linkedin_rest_base
      | _ -> Printf.sprintf "%s/images?action=initializeUpload" linkedin_rest_base
    in

    let register_body = `Assoc [
      ("initializeUploadRequest", `Assoc [
        ("owner", `String owner_urn);
      ])
    ] in

    let body = Yojson.Basic.to_string register_body in
    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
      ("Content-Type", "application/json");
      ("X-Restli-Protocol-Version", "2.0.0");
      ("LinkedIn-Version", linkedin_version_header ());
    ] in

    Config.Http.post ~headers ~body endpoint
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let open Yojson.Basic.Util in
            let json = Yojson.Basic.from_string response.body in
            let value = json |> member "value" in
            let asset_urn = match media_type with
              | "video" -> value |> member "video" |> to_string
              | "document" -> value |> member "document" |> to_string
              | _ -> value |> member "image" |> to_string
            in
            let upload_url = value |> member "uploadUrl" |> to_string in
            on_success (asset_urn, upload_url)
          with e ->
            on_error (Printf.sprintf "Failed to parse register response: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "Failed to register upload (%d): %s" response.status response.body))
      on_error
  
  (** Upload binary media data to LinkedIn *)
  let upload_binary ~access_token ~upload_url ~media_data on_success on_error =
    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
      ("Content-Type", "application/octet-stream");
      ("LinkedIn-Version", linkedin_version_header ());
    ] in
    
    Config.Http.put ~headers ~body:media_data upload_url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          on_success ()
        else
          on_error (Printf.sprintf "Failed to upload media binary (%d)" response.status))
      on_error
  
  (** Upload image or video to LinkedIn with optional alt text *)
  let upload_media ~access_token ~owner_urn ~media_url ~media_type ~alt_text ?(validate_before_upload=false) ~on_validation_error on_success on_error =
    (* Download media from URL *)
    Config.Http.get ~headers:[] media_url
      (fun media_response ->
        if media_response.status >= 200 && media_response.status < 300 then
          let file_size = String.length media_response.body in
          
          (* Validate media if requested *)
          let validation_result =
            if validate_before_upload then
              let mime_type = 
                List.assoc_opt "content-type" media_response.headers 
                |> Option.value ~default:(if media_type = "video" then "video/mp4" else "image/jpeg")
              in
              let media_type_enum = 
                if media_type = "video" then Platform_types.Video 
                else Platform_types.Image 
              in
              let media : Platform_types.post_media = {
                media_type = media_type_enum;
                mime_type = mime_type;
                file_size_bytes = file_size;
                width = None;
                height = None;
                duration_seconds = None;
                alt_text = alt_text;
              } in
              validate_media ~media
            else
              Ok ()
          in
          
          (match validation_result with
          | Error errs -> on_validation_error errs
          | Ok () ->
              register_upload ~access_token ~owner_urn ~media_type
                (fun (asset_urn, upload_url) ->
                  upload_binary ~access_token ~upload_url ~media_data:media_response.body
                    (fun () -> on_success (asset_urn, alt_text))
                    on_error)
                on_error)
        else
          on_error (Printf.sprintf "Failed to download media from %s (%d)" media_url media_response.status))
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
  
  (** Post to LinkedIn
      
      @param account_id The account identifier
      @param text The post text (max 3000 characters)
      @param media_urls List of media URLs to attach (max 9)
      @param author_urn Optional explicit author URN. When omitted, uses current person URN.
             Supported formats: urn:li:person:*, urn:li:organization:*, urn:li:organizationBrand:*
      @param alt_texts Optional alt text for each media item
      @param validate_media_before_upload When true, validates media size after download
             but before upload. LinkedIn limits: 200MB video, 10min duration, 8MB images.
             Default: false
      @param on_result Callback receiving the outcome with post ID
  *)
  let post_single ~account_id ~text ~media_urls ?author_urn ?(alt_texts=[]) ?(validate_media_before_upload=false) on_result =
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
                let rec upload_all_media urls_with_alt acc on_complete on_err =
                  match urls_with_alt with
                  | [] -> on_complete (List.rev acc)
                  | (url, alt_text) :: rest ->
                      (* Determine media type from URL or default to image *)
                      let is_video_url url =
                        let lower = String.lowercase_ascii url in
                        Filename.check_suffix lower ".mp4" ||
                        Filename.check_suffix lower ".mov" ||
                        Filename.check_suffix lower ".mpeg" ||
                        Filename.check_suffix lower ".avi"
                      in
                      let media_type = if is_video_url url then "video" else "image" in
                      upload_media ~access_token ~owner_urn:resolved_author_urn
                        ~media_url:url 
                        ~media_type
                        ~alt_text
                        ~validate_before_upload:validate_media_before_upload
                        ~on_validation_error:(fun errs -> 
                          on_result (Error_types.Failure (Error_types.Validation_error errs)))
                        (fun (asset_urn, alt_text) ->
                          let uploaded = { 
                            asset_urn; 
                            media_type; 
                            alt_text 
                          } in
                          upload_all_media rest (uploaded :: acc) on_complete on_err)
                        on_err
                in
                
                (* Pair URLs with alt text *)
                let urls_with_alt = List.mapi (fun i url ->
                  let alt_text = try List.nth alt_texts i with _ -> None in
                  (url, alt_text)
                ) media_urls in
                
                upload_all_media urls_with_alt []
                  (fun uploaded_media ->
                    (* Extract URL from text for link preview *)
                    let text_url = extract_first_url text in

                    (* Build modern /rest/posts body *)
                    let base_fields = [
                      ("author", `String resolved_author_urn);
                      ("commentary", `String text);
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
                          (* URL found, no uploaded media - article for link preview *)
                          [("content", `Assoc [
                            ("article", `Assoc [
                              ("source", `String url);
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
                          else
                            (* Images - use multiImage content *)
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
                              (try
                                List.assoc "x-restli-id" response.headers
                              with Not_found -> "unknown")
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
      @param validate_media_before_upload When true, validates media after download. Default: false
      @param on_result Callback receiving the outcome with thread_result
  *)
  let post_thread ~account_id ~texts ~media_urls_per_post ?author_urn ?(alt_texts_per_post=[]) ?(validate_media_before_upload=false) on_result =
    let media_counts = List.map List.length media_urls_per_post in
    match validate_thread ~texts ~media_counts () with
    | Error errs ->
        on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        let first_text = List.hd texts in
        let first_media = if List.length media_urls_per_post > 0 then List.hd media_urls_per_post else [] in
        let first_alt_texts = if List.length alt_texts_per_post > 0 then List.hd alt_texts_per_post else [] in
        post_single ~account_id ~text:first_text ~media_urls:first_media ?author_urn ~alt_texts:first_alt_texts ~validate_media_before_upload
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
  
  (** OAuth authorization URL *)
  let get_oauth_url ?(include_organization_scopes=false) ~redirect_uri ~state on_success on_error =
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
      (* LinkedIn OAuth 2.0 scopes for personal posting
         
         Required products in LinkedIn Developer Portal:
         - "Sign In with LinkedIn using OpenID Connect" → openid, profile, email
         - "Share on LinkedIn" → w_member_social (post as person)
         
         Note: For organization/company page posting, use a separate implementation
         which requires the Community Management API product. *)
      let scopes =
        if include_organization_scopes then
          [
            "openid";
            "profile";
            "email";
            "w_member_social";
            "r_organization_admin";
            "w_organization_social";
          ]
        else ["openid"; "profile"; "email"; "w_member_social"]
      in
      let scope_str = String.concat " " scopes in
      
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
  
  (** Exchange OAuth code for access token *)
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
      let params = [
        ("grant_type", ["authorization_code"]);
        ("code", [code]);
        ("redirect_uri", [String.trim redirect_uri]);
        ("client_id", [client_id]);
        ("client_secret", [client_secret]);
      ] in

      let body = Uri.encoded_of_query params in
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
              
              let credentials = {
                access_token;
                refresh_token;
                expires_at = Some expires_at;
                token_type = normalize_token_type (json |> member "token_type" |> to_string_option);
              } in
              on_success credentials
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
      (fun creds ->
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
              try
                let json = Yojson.Basic.from_string response.body in
                let open Yojson.Basic.Util in
                let profile = {
                  sub = json |> member "sub" |> to_string;
                  name = json |> member "name" |> to_string_option;
                  given_name = json |> member "given_name" |> to_string_option;
                  family_name = json |> member "family_name" |> to_string_option;
                  picture = json |> member "picture" |> to_string_option;
                  email = json |> member "email" |> to_string_option;
                  email_verified = json |> member "email_verified" |> to_bool_option;
                  locale = json |> member "locale" |> to_string_option;
                } in
                on_result (Ok profile)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse profile: %s" (Printexc.to_string e))))
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
                                ("commentary", `String text);
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
                  ("commentary", `String text);
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
