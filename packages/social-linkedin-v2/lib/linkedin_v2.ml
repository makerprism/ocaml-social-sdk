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
      (* LinkedIn expects parameters in query string, not body *)
      let params = [
        ("grant_type", ["authorization_code"]);
        ("code", [code]);
        ("redirect_uri", [redirect_uri]);
        ("client_id", [String.trim client_id]);
        ("client_secret", [String.trim client_secret]);
      ] in
      let query_string = Uri.encoded_of_query params in
      let url = Printf.sprintf "%s?%s" Metadata.token_endpoint query_string in
      
      (* POST request with parameters in query string, empty body *)
      Http.post ~headers:[] ~body:"" url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let access_token = json |> member "access_token" |> to_string in
              let refresh_token = 
                try Some (json |> member "refresh_token" |> to_string)
                with _ -> None in
              
              (* LinkedIn always includes expires_in in seconds *)
              let expires_in = json |> member "expires_in" |> to_int in
              let expires_at = 
                let now = Ptime_clock.now () in
                match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
                | Some exp -> Some (Ptime.to_rfc3339 exp)
                | None -> None in
              
              let token_type = 
                try json |> member "token_type" |> to_string
                with _ -> "Bearer" in
              
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
            (* Parse error response *)
            let error_msg = 
              try
                let json = Yojson.Basic.from_string response.body in
                let open Yojson.Basic.Util in
                let error = json |> member "error" |> to_string_option |> Option.value ~default:"unknown" in
                let error_desc = json |> member "error_description" |> to_string_option in
                match error_desc with
                | Some desc -> Printf.sprintf "%s: %s" error desc
                | None -> error
              with _ -> response.body
            in
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
      let body = Uri.encoded_of_query [
        ("grant_type", ["refresh_token"]);
        ("refresh_token", [refresh_token]);
        ("client_id", [String.trim client_id]);
        ("client_secret", [String.trim client_secret]);
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
              let access_token = json |> member "access_token" |> to_string in
              let new_refresh = 
                try Some (json |> member "refresh_token" |> to_string) 
                with _ -> Some refresh_token in
              let expires_in = json |> member "expires_in" |> to_int in
              let expires_at = 
                let now = Ptime_clock.now () in
                match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
                | Some exp -> Some (Ptime.to_rfc3339 exp)
                | None -> None in
              let token_type = 
                try json |> member "token_type" |> to_string
                with _ -> "Bearer" in
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

(** Comment on a post *)
type comment_info = {
  id: string;
  actor: string;
  text: string;
  created_at: string option;
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
    else if client_id = "" || client_secret = "" then
      on_error "LinkedIn OAuth credentials not configured"
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
              let new_access = json |> member "access_token" |> to_string in
              let new_refresh = 
                try json |> member "refresh_token" |> to_string 
                with _ -> refresh_token in
              (* CRITICAL: Read actual expires_in from LinkedIn refresh response *)
              let expires_in = json |> member "expires_in" |> to_int in
              Printf.printf "[LinkedIn] Token refreshed, new expiration in %d seconds (%d days)\n%!" 
                expires_in (expires_in / 86400);
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
        (* Log token status for debugging *)
        let expires_info = match creds.expires_at with
          | Some exp_str -> 
              (try
                match Ptime.of_rfc3339 exp_str with
                | Ok (exp_time, _, _) ->
                    let now = Ptime_clock.now () in
                    let diff = Ptime.diff exp_time now in
                    let days = Ptime.Span.to_d_ps diff |> fst in
                    Printf.sprintf "expires in %d days (%s)" days exp_str
                | Error _ -> Printf.sprintf "invalid format: %s" exp_str
              with _ -> Printf.sprintf "parse error: %s" exp_str)
          | None -> "no expiration set"
        in
        
        let has_refresh = match creds.refresh_token with
          | Some _ -> "has refresh_token"
          | None -> "NO refresh_token (LinkedIn standard app limitation)"
        in
        
        Printf.printf "[LinkedIn] Token check for account %s: %s, %s\n%!" 
          account_id expires_info has_refresh;
        
        (* Check if token needs refresh (7 days buffer) *)
        if is_token_expired_buffer ~buffer_seconds:604800 creds.expires_at then (
          Printf.printf "[LinkedIn] Token expiring soon for account %s (within 7 days), attempting refresh...\n%!" account_id;
          
          (* Token expiring soon, refresh it *)
          match creds.refresh_token with
          | None ->
              let error_msg = "LinkedIn token expired but no refresh_token available. " ^
                              "LinkedIn standard apps (with 'Sign In with LinkedIn' and 'Share on LinkedIn' products) " ^
                              "do not support programmatic token refresh. User must reconnect via OAuth flow. " ^
                              "Tokens last 60 days from initial authorization." in
              Printf.printf "[LinkedIn] ERROR for account %s: %s\n%!" account_id error_msg;
              Config.update_health_status ~account_id ~status:"token_expired" 
                ~error_message:(Some "Token expired - please reconnect (LinkedIn tokens last 60 days)")
                (fun () -> on_error "LinkedIn token expired - please reconnect your account")
                on_error
          | Some refresh_token ->
              Printf.printf "[LinkedIn] Attempting programmatic refresh for account %s...\n%!" account_id;
              let client_id = Config.get_env "LINKEDIN_CLIENT_ID" |> Option.value ~default:"" in
              let client_secret = Config.get_env "LINKEDIN_CLIENT_SECRET" |> Option.value ~default:"" in
              
              refresh_access_token ~client_id ~client_secret ~refresh_token
                (fun (new_access, new_refresh, expires_at) ->
                  Printf.printf "[LinkedIn] Successfully refreshed token for account %s, new expiry: %s\n%!" 
                    account_id expires_at;
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
                        on_error)
                    on_error)
                (fun err ->
                  Printf.printf "[LinkedIn] Token refresh FAILED for account %s: %s\n%!" account_id err;
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
                    (fun () -> on_error user_friendly_error)
                    on_error)
        ) else (
          Printf.printf "[LinkedIn] Token valid for account %s (%s)\n%!" account_id expires_info;
          (* Token still valid *)
          Config.update_health_status ~account_id ~status:"healthy" ~error_message:None
            (fun () -> on_success creds.access_token)
            on_error
        ))
      on_error
  
  (** Get person URN for posting *)
  let get_person_urn ~access_token on_success on_error =
    let url = Printf.sprintf "%s/userinfo" linkedin_api_base in
    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
    ] in
    
    Config.Http.get ~headers url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            (* OpenID Connect returns 'sub' (subject) as the user identifier *)
            let person_id = json |> Yojson.Basic.Util.member "sub" |> Yojson.Basic.Util.to_string in
            let person_urn = Printf.sprintf "urn:li:person:%s" person_id in
            on_success person_urn
          with e ->
            on_error (Printf.sprintf "Failed to parse person URN: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "Failed to get person URN (%d): %s" response.status response.body))
      on_error
  
  (** Register image upload with LinkedIn *)
  let register_upload ~access_token ~person_urn ~media_type on_success on_error =
    let recipe = match media_type with
      | "video" -> "urn:li:digitalmediaRecipe:feedshare-video"
      | _ -> "urn:li:digitalmediaRecipe:feedshare-image"
    in
    
    let url = Printf.sprintf "%s/assets?action=registerUpload" linkedin_api_base in
    let register_body = `Assoc [
      ("registerUploadRequest", `Assoc [
        ("recipes", `List [`String recipe]);
        ("owner", `String person_urn);
        ("serviceRelationships", `List [
          `Assoc [
            ("relationshipType", `String "OWNER");
            ("identifier", `String "urn:li:userGeneratedContent");
          ]
        ]);
      ])
    ] in
    
    let body = Yojson.Basic.to_string register_body in
    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
      ("Content-Type", "application/json");
      ("X-Restli-Protocol-Version", "2.0.0");
    ] in
    
    Config.Http.post ~headers ~body url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let open Yojson.Basic.Util in
            let json = Yojson.Basic.from_string response.body in
            let asset = json |> member "value" |> member "asset" |> to_string in
            let upload_url = json 
              |> member "value" 
              |> member "uploadMechanism"
              |> member "com.linkedin.digitalmedia.uploading.MediaUploadHttpRequest"
              |> member "uploadUrl"
              |> to_string in
            on_success (asset, upload_url)
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
    ] in
    
    Config.Http.put ~headers ~body:media_data upload_url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          on_success ()
        else
          on_error (Printf.sprintf "Failed to upload media binary (%d)" response.status))
      on_error
  
  (** Upload image or video to LinkedIn with optional alt text *)
  let upload_media ~access_token ~person_urn ~media_url ~media_type ~alt_text on_success on_error =
    (* Download media from URL *)
    Config.Http.get ~headers:[] media_url
      (fun media_response ->
        if media_response.status >= 200 && media_response.status < 300 then
          register_upload ~access_token ~person_urn ~media_type
            (fun (asset_urn, upload_url) ->
              upload_binary ~access_token ~upload_url ~media_data:media_response.body
                (fun () -> on_success (asset_urn, alt_text))
                on_error)
            on_error
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
  
  (** Post to LinkedIn *)
  let post_single ~account_id ~text ~media_urls ?(alt_texts=[]) on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_person_urn ~access_token
          (fun person_urn ->
            (* Upload all media items first *)
            let rec upload_all_media urls_with_alt acc on_complete =
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
                  upload_media ~access_token ~person_urn 
                    ~media_url:url 
                    ~media_type
                    ~alt_text
                    (fun (asset_urn, alt_text) ->
                      let uploaded = { 
                        asset_urn; 
                        media_type; 
                        alt_text 
                      } in
                      upload_all_media rest (uploaded :: acc) on_complete)
                    on_error
            in
            
            (* Pair URLs with alt text *)
            let urls_with_alt = List.mapi (fun i url ->
              let alt_text = try List.nth alt_texts i with _ -> None in
              (url, alt_text)
            ) media_urls in
            
            upload_all_media urls_with_alt [] (fun uploaded_media ->
                (* Extract URL from text for link preview *)
                let text_url = extract_first_url text in
                
                (* Determine share media category and build specific content *)
                let specific_content = 
                  match uploaded_media, text_url with
                  | [], Some url ->
                      (* URL found, no uploaded media - use ARTICLE for rich link preview *)
                      [
                        ("shareCommentary", `Assoc [("text", `String text)]);
                        ("shareMediaCategory", `String "ARTICLE");
                        ("media", `List [
                          `Assoc [
                            ("status", `String "READY");
                            ("originalUrl", `String url);
                          ]
                        ]);
                      ]
                  | [], None ->
                      (* No media, no URL - text only *)
                      [
                        ("shareCommentary", `Assoc [("text", `String text)]);
                        ("shareMediaCategory", `String "NONE");
                      ]
                  | uploaded_media, _ ->
                      (* Has uploaded media - use existing media handling logic *)
                      let category = match uploaded_media with
                        | first :: _ -> if first.media_type = "video" then "VIDEO" else "IMAGE"
                        | [] -> "NONE"
                      in
                      let media_json = `List (List.map (fun media ->
                        let base_fields = [
                          ("status", `String "READY");
                          ("media", `String media.asset_urn);
                        ] in
                        (* Add description (alt text) if available *)
                        let with_description = match media.alt_text with
                          | Some alt when String.length alt > 0 ->
                              base_fields @ [("description", `Assoc [("text", `String alt)])]
                          | _ -> base_fields
                        in
                        `Assoc with_description
                      ) uploaded_media) in
                      [
                        ("shareCommentary", `Assoc [("text", `String text)]);
                        ("shareMediaCategory", `String category);
                        ("media", media_json);
                      ]
                in
                
                let post_body = `Assoc [
                  ("author", `String person_urn);
                  ("lifecycleState", `String "PUBLISHED");
                  ("specificContent", `Assoc [
                    ("com.linkedin.ugc.ShareContent", `Assoc specific_content)
                  ]);
                  ("visibility", `Assoc [
                    ("com.linkedin.ugc.MemberNetworkVisibility", `String "PUBLIC")
                  ])
                ] in
                
                let url = Printf.sprintf "%s/ugcPosts" linkedin_api_base in
                let body = Yojson.Basic.to_string post_body in
                let headers = [
                  ("Authorization", Printf.sprintf "Bearer %s" access_token);
                  ("Content-Type", "application/json");
                  ("X-Restli-Protocol-Version", "2.0.0");
                ] in
                
                Config.Http.post ~headers ~body url
                  (fun response ->
                    if response.status >= 200 && response.status < 300 then
                      (* LinkedIn returns post ID in X-RestLi-Id header *)
                      let post_id = 
                        try
                          (* Parse from response headers if available *)
                          let json = Yojson.Basic.from_string response.body in
                          json |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_string
                        with _ -> "unknown"
                      in
                      on_success post_id
                    else
                      (* Parse error response *)
                      let error_msg = 
                        try
                          let json = Yojson.Basic.from_string response.body in
                          let open Yojson.Basic.Util in
                          let error = json |> member "message" |> to_string_option in
                          let service_error = json |> member "serviceErrorCode" |> to_int_option in
                          match error, service_error with
                          | Some msg, Some code -> Printf.sprintf "Error %d: %s" code msg
                          | Some msg, None -> msg
                          | None, Some code -> Printf.sprintf "Service error code: %d" code
                          | None, None -> response.body
                        with _ -> response.body
                      in
                      on_error (Printf.sprintf "LinkedIn API error (%d): %s" response.status error_msg))
                  on_error))
          on_error)
      on_error
  
  (** Post thread (LinkedIn doesn't support threads, posts only first item) *)
  let post_thread ~account_id ~texts ~media_urls_per_post ?(alt_texts_per_post=[]) on_success on_error =
    if List.length texts = 0 then
      on_error "No content to post"
    else
      let first_text = List.hd texts in
      let first_media = if List.length media_urls_per_post > 0 then List.hd media_urls_per_post else [] in
      let first_alt_texts = if List.length alt_texts_per_post > 0 then List.hd alt_texts_per_post else [] in
      post_single ~account_id ~text:first_text ~media_urls:first_media ~alt_texts:first_alt_texts
        (fun post_id -> on_success [post_id])
        on_error
  
  (** OAuth authorization URL *)
  let get_oauth_url ~redirect_uri ~state on_success on_error =
    let client_id = Config.get_env "LINKEDIN_CLIENT_ID" |> Option.value ~default:"" in
    
    if client_id = "" then
      on_error "LinkedIn client ID not configured"
    else (
      (* LinkedIn OAuth 2.0 scopes for personal posting
         
         Required products in LinkedIn Developer Portal:
         - "Sign In with LinkedIn using OpenID Connect" → openid, profile, email
         - "Share on LinkedIn" → w_member_social (post as person)
         
         Note: For organization/company page posting, use a separate implementation
         which requires the Community Management API product. *)
      let scopes = ["openid"; "profile"; "email"; "w_member_social"] in
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
    let client_id = Config.get_env "LINKEDIN_CLIENT_ID" |> Option.value ~default:"" in
    let client_secret = Config.get_env "LINKEDIN_CLIENT_SECRET" |> Option.value ~default:"" in
    
    if client_id = "" || client_secret = "" then
      on_error "LinkedIn OAuth credentials not configured"
    else (
      (* LinkedIn expects token exchange parameters as query string in the URL
         NOTE: No PKCE (code_verifier) - LinkedIn does not support it. *)
      let params = [
        ("grant_type", ["authorization_code"]);
        ("code", [code]);
        ("redirect_uri", [redirect_uri]);
        ("client_id", [String.trim client_id]);
        ("client_secret", [String.trim client_secret]);
      ] in
      
      let query_string = Uri.encoded_of_query params in
      let url = Printf.sprintf "%s/accessToken?%s" linkedin_auth_url query_string in
      
      (* POST request with parameters in query string, empty body *)
      Config.Http.post ~headers:[] ~body:"" url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let access_token = json |> member "access_token" |> to_string in
              let refresh_token = 
                try Some (json |> member "refresh_token" |> to_string)
                with _ -> None
              in
              
              (* CRITICAL: Read actual expires_in from LinkedIn response - MUST be present!
                 LinkedIn's token response ALWAYS includes expires_in in seconds.
                 If it's missing, something is seriously wrong with the OAuth response. *)
              let expires_in_result = 
                try Ok (json |> member "expires_in" |> to_int)
                with _ -> Error "LinkedIn OAuth response missing 'expires_in' field"
              in
              
              match expires_in_result with
              | Error err ->
                  let response_preview = if String.length response.body > 200 
                    then String.sub response.body 0 200 ^ "..." 
                    else response.body in
                  Printf.printf "[LinkedIn] ERROR: %s. Response: %s\n%!" err response_preview;
                  on_error err
              | Ok expires_in ->
              
              (* Log refresh_token presence and actual expiration *)
              (match refresh_token with
              | Some rt -> 
                  Printf.printf "[LinkedIn] OAuth exchange successful: access_token received, refresh_token PRESENT (length: %d)\n%!" 
                    (String.length rt);
                  Printf.printf "[LinkedIn] Token expires in %d seconds (%d days) according to LinkedIn response\n%!" 
                    expires_in (expires_in / 86400)
              | None -> 
                  Printf.printf "[LinkedIn] OAuth exchange successful: access_token received, refresh_token ABSENT\n%!";
                  Printf.printf "[LinkedIn] WARNING: LinkedIn standard apps (with 'Sign In' and 'Share' products) typically do NOT provide refresh_token.\n%!";
                  Printf.printf "[LinkedIn] Token expires in %d seconds (%d days) according to LinkedIn response\n%!" 
                    expires_in (expires_in / 86400);
                  Printf.printf "[LinkedIn] User will need to reconnect via OAuth flow when token expires.\n%!";
                  Printf.printf "[LinkedIn] To enable programmatic refresh, apply for LinkedIn Partner Program.\n%!");
              
              let expires_at = 
                let now = Ptime_clock.now () in
                match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
                | Some exp -> Ptime.to_rfc3339 exp
                | None -> Ptime.to_rfc3339 now
              in
              Printf.printf "[LinkedIn] Token expires at: %s\n%!" expires_at;
              
              let credentials = {
                access_token;
                refresh_token;
                expires_at = Some expires_at;
                token_type = "Bearer";
              } in
              on_success credentials
            with e ->
              on_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))
          else
            (* Parse error response *)
            let error_msg = 
              try
                let json = Yojson.Basic.from_string response.body in
                let open Yojson.Basic.Util in
                let error = json |> member "error" |> to_string_option |> Option.value ~default:"unknown" in
                let error_desc = json |> member "error_description" |> to_string_option in
                match error_desc with
                | Some desc -> Printf.sprintf "%s: %s" error desc
                | None -> error
              with _ -> response.body
            in
            on_error (Printf.sprintf "LinkedIn OAuth exchange failed (%d): %s" response.status error_msg))
        on_error
    )
  
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
  let get_profile ~account_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        let url = Printf.sprintf "%s/userinfo" linkedin_api_base in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
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
                on_success profile
              with e ->
                on_error (Printf.sprintf "Failed to parse profile: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Failed to get profile (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** {1 Posts API} *)
  
  (** Get a specific post by URN
      
      Fetches a single post/share using its URN. Requires appropriate permissions.
      @param post_urn The URN of the post (e.g., "urn:li:share:123456")
  *)
  let get_post ~account_id ~post_urn on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        let url = Printf.sprintf "%s/ugcPosts/%s" linkedin_api_base (Uri.pct_encode post_urn) in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
          ("X-Restli-Protocol-Version", "2.0.0");
        ] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let open Yojson.Basic.Util in
                let post = {
                  id = json |> member "id" |> to_string;
                  author = json |> member "author" |> to_string;
                  created_at = (try json |> member "created" |> member "time" |> to_string_option with _ -> None);
                  text = (try
                    json 
                    |> member "specificContent" 
                    |> member "com.linkedin.ugc.ShareContent"
                    |> member "shareCommentary"
                    |> member "text"
                    |> to_string_option
                  with _ -> None);
                  visibility = (try
                    json
                    |> member "visibility"
                    |> member "com.linkedin.ugc.MemberNetworkVisibility"
                    |> to_string_option
                  with _ -> None);
                  lifecycle_state = json |> member "lifecycleState" |> to_string_option;
                } in
                on_success post
              with e ->
                on_error (Printf.sprintf "Failed to parse post: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Failed to get post (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Get user's posts with pagination
      
      Fetches posts authored by the current user. Returns a collection with paging support.
      @param start Starting index (default: 0)
      @param count Number of posts to fetch (default: 10, max: 50)
  *)
  let get_posts ~account_id ?(start=0) ?(count=10) on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_person_urn ~access_token
          (fun person_urn ->
            (* Build query parameters for filtering by author *)
            let query_params = [
              ("q", "authors");
              ("authors", person_urn);
              ("start", string_of_int start);
              ("count", string_of_int (min count 50));
            ] in
            let query_string = Uri.encoded_of_query 
              (List.map (fun (k, v) -> (k, [v])) query_params) in
            
            let url = Printf.sprintf "%s/ugcPosts?%s" linkedin_api_base query_string in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_token);
              ("X-Restli-Protocol-Version", "2.0.0");
            ] in
            
            Config.Http.get ~headers url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  try
                    let json = Yojson.Basic.from_string response.body in
                    let open Yojson.Basic.Util in
                    
                    (* Parse elements *)
                    let elements_json = json |> member "elements" |> to_list in
                    let posts = List.map (fun elem ->
                      {
                        id = elem |> member "id" |> to_string;
                        author = elem |> member "author" |> to_string;
                        created_at = (try elem |> member "created" |> member "time" |> to_string_option with _ -> None);
                        text = (try
                          elem 
                          |> member "specificContent" 
                          |> member "com.linkedin.ugc.ShareContent"
                          |> member "shareCommentary"
                          |> member "text"
                          |> to_string_option
                        with _ -> None);
                        visibility = (try
                          elem
                          |> member "visibility"
                          |> member "com.linkedin.ugc.MemberNetworkVisibility"
                          |> to_string_option
                        with _ -> None);
                        lifecycle_state = elem |> member "lifecycleState" |> to_string_option;
                      }
                    ) elements_json in
                    
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
                    on_success collection
                  with e ->
                    on_error (Printf.sprintf "Failed to parse posts: %s" (Printexc.to_string e))
                else
                  on_error (Printf.sprintf "Failed to get posts (%d): %s" response.status response.body))
              on_error)
          on_error)
      on_error
  
  (** {1 Scroller Pattern for Pagination} *)
  
  (** Scroller for paginated post fetching
      
      Provides a convenient interface for navigating through pages of posts.
      Usage:
        let scroller = create_posts_scroller ~account_id ~page_size:10 ()
        scroller.scroll_next (fun page -> ...) (fun err -> ...)
  *)
  type 'a scroller = {
    scroll_next: (('a collection_response -> unit) -> (string -> unit) -> unit);
    scroll_back: (('a collection_response -> unit) -> (string -> unit) -> unit);
    current_position: unit -> int;
    has_more: unit -> bool;
  }
  
  (** Create a scroller for user's posts *)
  let create_posts_scroller ~account_id ?(page_size=10) () =
    let current_start = ref 0 in
    let last_total = ref None in
    
    let scroll_next on_success on_error =
      get_posts ~account_id ~start:!current_start ~count:page_size
        (fun collection ->
          (* Update state *)
          (match collection.paging with
          | Some p -> 
              current_start := p.start + p.count;
              last_total := p.total
          | None -> 
              current_start := !current_start + (List.length collection.elements));
          on_success collection)
        on_error
    in
    
    let scroll_back on_success on_error =
      let new_start = max 0 (!current_start - page_size - page_size) in
      current_start := new_start;
      get_posts ~account_id ~start:new_start ~count:page_size
        (fun collection ->
          (match collection.paging with
          | Some p -> current_start := p.start + p.count
          | None -> current_start := new_start + (List.length collection.elements));
          on_success collection)
        on_error
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
  let batch_get_posts ~account_id ~post_urns on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        if List.length post_urns = 0 then
          on_success []
        else
          let encoded_ids = List.map Uri.pct_encode post_urns in
          let ids_param = String.concat "," encoded_ids in
          let url = Printf.sprintf "%s/ugcPosts?ids=%s" linkedin_api_base ids_param in
          let headers = [
            ("Authorization", Printf.sprintf "Bearer %s" access_token);
            ("X-Restli-Protocol-Version", "2.0.0");
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
                      try
                        Some {
                          id = post_json |> member "id" |> to_string;
                          author = post_json |> member "author" |> to_string;
                          created_at = (try
                            post_json |> member "created" |> member "time" |> to_string_option
                          with _ -> None);
                          text = (try
                            post_json 
                            |> member "specificContent" 
                            |> member "com.linkedin.ugc.ShareContent"
                            |> member "shareCommentary"
                            |> member "text"
                            |> to_string_option
                          with _ -> None);
                          visibility = (try
                            post_json
                            |> member "visibility"
                            |> member "com.linkedin.ugc.MemberNetworkVisibility"
                            |> to_string_option
                          with _ -> None);
                          lifecycle_state = post_json |> member "lifecycleState" |> to_string_option;
                        }
                      with _ -> None
                    ) results)
                  with e ->
                    Error (Printf.sprintf "Failed to parse batch results: %s" (Printexc.to_string e))
                with
                | Ok posts -> on_success posts
                | Error msg -> on_error msg
              else
                on_error (Printf.sprintf "Batch get failed (%d): %s" response.status response.body))
            on_error)
      on_error
  
  (** {1 Search API (FINDER Pattern)} *)
  
  (** Search posts with custom criteria
      
      Uses LinkedIn's FINDER method to search posts by various criteria.
      This is more flexible than simple listing.
      
      @param keywords Optional keywords to search for
      @param author Optional author URN to filter by
      @param start Starting index
      @param count Results per page
  *)
  let search_posts ~account_id ?keywords ?author ?(start=0) ?(count=10) on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        (* Build query parameters for FINDER *)
        let base_params = [
          ("q", "search");  (* FINDER name *)
          ("start", string_of_int start);
          ("count", string_of_int (min count 50));
        ] in
        
        (* Add optional parameters *)
        let with_keywords = match keywords with
          | Some kw -> ("keywords", kw) :: base_params
          | None -> base_params
        in
        
        let with_author = match author with
          | Some auth -> ("author", auth) :: with_keywords
          | None -> with_keywords
        in
        
        let query_string = Uri.encoded_of_query 
          (List.map (fun (k, v) -> (k, [v])) with_author) in
        
        let url = Printf.sprintf "%s/ugcPosts?%s" linkedin_api_base query_string in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
          ("X-Restli-Protocol-Version", "2.0.0");
        ] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let open Yojson.Basic.Util in
                
                let elements_json = json |> member "elements" |> to_list in
                let posts = List.map (fun elem ->
                  {
                    id = elem |> member "id" |> to_string;
                    author = elem |> member "author" |> to_string;
                    created_at = (try elem |> member "created" |> member "time" |> to_string_option with _ -> None);
                    text = (try
                      elem 
                      |> member "specificContent" 
                      |> member "com.linkedin.ugc.ShareContent"
                      |> member "shareCommentary"
                      |> member "text"
                      |> to_string_option
                    with _ -> None);
                    visibility = (try
                      elem
                      |> member "visibility"
                      |> member "com.linkedin.ugc.MemberNetworkVisibility"
                      |> to_string_option
                    with _ -> None);
                    lifecycle_state = elem |> member "lifecycleState" |> to_string_option;
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
                  elements = posts;
                  paging = paging;
                  metadata = None;
                } in
                on_success collection
              with e ->
                on_error (Printf.sprintf "Failed to parse search results: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Search failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Create a scroller for post search *)
  let create_search_scroller ~account_id ?keywords ?author ?(page_size=10) () =
    let current_start = ref 0 in
    let last_total = ref None in
    
    let scroll_next on_success on_error =
      search_posts ~account_id ?keywords ?author ~start:!current_start ~count:page_size
        (fun collection ->
          (match collection.paging with
          | Some p -> 
              current_start := p.start + p.count;
              last_total := p.total
          | None -> 
              current_start := !current_start + (List.length collection.elements));
          on_success collection)
        on_error
    in
    
    let scroll_back on_success on_error =
      let new_start = max 0 (!current_start - page_size - page_size) in
      current_start := new_start;
      search_posts ~account_id ?keywords ?author ~start:new_start ~count:page_size
        (fun collection ->
          (match collection.paging with
          | Some p -> current_start := p.start + p.count
          | None -> current_start := new_start + (List.length collection.elements));
          on_success collection)
        on_error
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
  let like_post ~account_id ~post_urn on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_person_urn ~access_token
          (fun person_urn ->
            let like_body = `Assoc [
              ("actor", `String person_urn);
              ("object", `String post_urn);
            ] in
            
            let url = Printf.sprintf "%s/socialActions/%s/likes" 
              linkedin_api_base (Uri.pct_encode post_urn) in
            let body = Yojson.Basic.to_string like_body in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_token);
              ("Content-Type", "application/json");
              ("X-Restli-Protocol-Version", "2.0.0");
            ] in
            
            Config.Http.post ~headers ~body url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  on_success ()
                else
                  on_error (Printf.sprintf "Failed to like post (%d): %s" 
                    response.status response.body))
              on_error)
          on_error)
      on_error
  
  (** Unlike a post
      
      Removes a like/reaction from the specified post.
      @param post_urn The URN of the post to unlike
  *)
  let unlike_post ~account_id ~post_urn on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_person_urn ~access_token
          (fun person_urn ->
            (* Build the like URN: urn:li:like:(actor,object) *)
            let like_id = Printf.sprintf "(%s,%s)" person_urn post_urn in
            let url = Printf.sprintf "%s/socialActions/%s/likes/%s" 
              linkedin_api_base 
              (Uri.pct_encode post_urn)
              (Uri.pct_encode like_id) in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_token);
              ("X-Restli-Protocol-Version", "2.0.0");
            ] in
            
            Config.Http.delete ~headers url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  on_success ()
                else
                  on_error (Printf.sprintf "Failed to unlike post (%d): %s" 
                    response.status response.body))
              on_error)
          on_error)
      on_error
  
  (** Comment on a post
      
      Adds a comment to the specified post.
      @param post_urn The URN of the post
      @param text The comment text
  *)
  let comment_on_post ~account_id ~post_urn ~text on_success on_error =
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
              linkedin_api_base (Uri.pct_encode post_urn) in
            let body = Yojson.Basic.to_string comment_body in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_token);
              ("Content-Type", "application/json");
              ("X-Restli-Protocol-Version", "2.0.0");
            ] in
            
            Config.Http.post ~headers ~body url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  try
                    let json = Yojson.Basic.from_string response.body in
                    let comment_id = json 
                      |> Yojson.Basic.Util.member "id" 
                      |> Yojson.Basic.Util.to_string in
                    on_success comment_id
                  with _ ->
                    (* If we can't parse the ID, just return success *)
                    on_success "unknown"
                else
                  on_error (Printf.sprintf "Failed to comment (%d): %s" 
                    response.status response.body))
              on_error)
          on_error)
      on_error
  
  (** Get comments on a post
      
      Fetches comments for a specific post with pagination.
      @param post_urn The URN of the post
      @param start Starting index
      @param count Number of comments to fetch
  *)
  let get_post_comments ~account_id ~post_urn ?(start=0) ?(count=10) on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        let query_params = [
          ("start", string_of_int start);
          ("count", string_of_int (min count 100));
        ] in
        let query_string = Uri.encoded_of_query 
          (List.map (fun (k, v) -> (k, [v])) query_params) in
        
        let url = Printf.sprintf "%s/socialActions/%s/comments?%s" 
          linkedin_api_base (Uri.pct_encode post_urn) query_string in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
          ("X-Restli-Protocol-Version", "2.0.0");
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
                on_success collection
              with e ->
                on_error (Printf.sprintf "Failed to parse comments: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Failed to get comments (%d): %s" 
                response.status response.body))
          on_error)
      on_error
  
  (** Get engagement statistics for a post
      
      Fetches like count, comment count, and other engagement metrics.
      Note: This may require additional API permissions.
      @param post_urn The URN of the post
  *)
  let get_post_engagement ~account_id ~post_urn on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        let url = Printf.sprintf "%s/socialMetadata/%s" 
          linkedin_api_base (Uri.pct_encode post_urn) in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
          ("X-Restli-Protocol-Version", "2.0.0");
        ] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let open Yojson.Basic.Util in
                
                let engagement = {
                  like_count = json |> member "totalLikes" |> to_int_option;
                  comment_count = json |> member "totalComments" |> to_int_option;
                  share_count = json |> member "totalShares" |> to_int_option;
                  impression_count = json |> member "totalImpressions" |> to_int_option;
                } in
                on_success engagement
              with e ->
                on_error (Printf.sprintf "Failed to parse engagement: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Failed to get engagement (%d): %s" 
                response.status response.body))
          on_error)
      on_error
end
