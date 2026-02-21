(** Instagram Graph API v21 Provider
    
    This implementation supports Instagram Business accounts via Graph API.
    
    CRITICAL REQUIREMENTS:
    - Instagram Business or Creator account ONLY
    - Must be linked to a Facebook Page
    - Two-step publishing process: create container, then publish
    - Images must be publicly accessible URLs
    
    Rate Limits:
    - 200 API calls/hour per user
    - 25 container creations/hour
    - 25 posts/day
*)

open Social_core

(** OAuth 2.0 module for Instagram Graph API

    Instagram uses Facebook's OAuth 2.0 infrastructure with some key differences:

    Token types:
    - Short-lived user tokens: ~1-2 hours (from Facebook code exchange)
    - Long-lived user tokens: ~60 days (via fb_exchange_token grant on Facebook Graph API)

    Token refresh:
    - Long-lived tokens can be refreshed using ig_refresh_token grant
    - Must refresh before expiration (within 60 days)
    - Cannot refresh short-lived tokens directly

    IMPORTANT: Instagram Business/Creator accounts ONLY.
    Personal Instagram accounts cannot use the Graph API.

    Required environment variables (or pass directly to functions):
    - FACEBOOK_APP_ID: App ID from Facebook Developer Portal
    - FACEBOOK_APP_SECRET: App Secret (same app as Facebook)
    - INSTAGRAM_REDIRECT_URI: Registered callback URL
*)
module OAuth = struct
  let compute_app_secret_proof ~app_secret ~access_token =
    let digest = Digestif.SHA256.hmac_string ~key:app_secret access_token in
    Digestif.SHA256.to_hex digest

  let parse_credentials_from_json ?(default_expires_in=3600) json =
    let open Yojson.Basic.Util in
    let access_token = json |> member "access_token" |> to_string in
    let expires_in =
      try json |> member "expires_in" |> to_int
      with _ -> default_expires_in
    in
    let expires_at =
      let now = Ptime_clock.now () in
      match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
      | Some exp -> Some (Ptime.to_rfc3339 exp)
      | None -> None
    in
    let token_type =
      try json |> member "token_type" |> to_string
      with _ -> "Bearer"
    in
    ({
      access_token;
      refresh_token = None;
      expires_at;
      token_type;
    } : credentials)

  let parse_api_error ~status_code ~response_body =
    try
      let json = Yojson.Basic.from_string response_body in
      let open Yojson.Basic.Util in
      let error_obj = json |> member "error" in
      let error_code =
        try error_obj |> member "code" |> to_int
        with _ -> 0
      in
      let error_message =
        try error_obj |> member "message" |> to_string
        with _ -> response_body
      in
      match error_code with
      | 190 -> Error_types.Auth_error Error_types.Token_expired
      | 102 -> Error_types.Auth_error Error_types.Token_invalid
      | 4 | 32 | 613 ->
          Error_types.Rate_limited {
            retry_after_seconds = Some 300;
            limit = Some 25;
            remaining = Some 0;
            reset_at = None;
          }
      | 10 | 200 ->
          Error_types.Auth_error (Error_types.Insufficient_permissions ["instagram_content_publish"])
      | _ ->
          Error_types.Api_error {
            status_code;
            message = error_message;
            platform = Platform_types.Instagram;
            raw_response = Some response_body;
            request_id = None;
          }
    with _ ->
      Error_types.Api_error {
        status_code;
        message = response_body;
        platform = Platform_types.Instagram;
        raw_response = Some response_body;
        request_id = None;
      }

  let network_error_of_string err =
    let lower = String.lowercase_ascii err in
    let looks_sensitive =
      List.exists
        (fun needle ->
          try
            ignore (Str.search_forward (Str.regexp_string needle) lower 0);
            true
          with Not_found -> false)
        [ "access_token="; "access_token"; "client_secret"; "authorization" ]
    in
    let safe_err = if looks_sensitive then "[REDACTED_SENSITIVE_TEXT]" else err in
    Error_types.Network_error (Error_types.Connection_failed safe_err)

  (** Scope definitions for Instagram Graph API *)
  module Scopes = struct
    (** Scopes for basic Instagram profile information *)
    let read = [
      "instagram_basic";
      "pages_show_list";
      "pages_read_engagement";
    ]
    
    (** Scopes required for Instagram posting *)
    let write = [
      "instagram_basic";
      "instagram_content_publish";
      "pages_show_list";
      "pages_read_engagement";
    ]
    
    (** All commonly used scopes for Instagram management *)
    let all = [
      "instagram_basic";
      "instagram_content_publish";
      "instagram_manage_comments";
      "instagram_manage_insights";
      "pages_show_list";
      "pages_read_engagement";
      "pages_manage_metadata";
    ]
    
    (** Operations that can be performed with Instagram API *)
    type operation = 
      | Post_image
      | Post_video
      | Post_carousel
      | Post_reel
      | Post_story
      | Read_profile
      | Read_insights
      | Manage_comments
    
    (** Get scopes required for specific operations *)
    let for_operations ops =
      let base = ["instagram_basic"; "pages_show_list"; "pages_read_engagement"] in
      let needs_publish = List.exists (fun o -> 
        o = Post_image || o = Post_video || o = Post_carousel || o = Post_reel || o = Post_story
      ) ops in
      let needs_comments = List.exists (fun o -> o = Manage_comments) ops in
      let needs_insights = List.exists (fun o -> o = Read_insights) ops in
      base @
      (if needs_publish then ["instagram_content_publish"] else []) @
      (if needs_comments then ["instagram_manage_comments"] else []) @
      (if needs_insights then ["instagram_manage_insights"] else [])
  end
  
  (** Platform metadata for Instagram OAuth *)
  module Metadata = struct
    (** Instagram does NOT support PKCE (uses Facebook OAuth) *)
    let supports_pkce = false
    
    (** Instagram supports token refresh via ig_refresh_token grant *)
    let supports_refresh = true
    
    (** Short-lived tokens last ~1-2 hours *)
    let short_lived_token_seconds = Some 3600
    
    (** Long-lived tokens last ~60 days *)
    let long_lived_token_seconds = Some 5184000
    
    (** Recommended buffer before expiry (7 days) *)
    let refresh_buffer_seconds = 604800
    
    (** Maximum retry attempts for token operations *)
    let max_refresh_attempts = 5
    
    (** Authorization endpoint (Facebook's OAuth dialog) *)
    let authorization_endpoint = "https://www.facebook.com/v21.0/dialog/oauth"
    
    (** Token endpoint for initial exchange (Facebook Graph API) *)
    let token_endpoint = "https://graph.facebook.com/v21.0/oauth/access_token"
    
    (** Instagram Graph API token endpoint for long-lived exchange *)
    let instagram_token_endpoint = "https://graph.instagram.com/access_token"
    
    (** Instagram Graph API endpoint for token refresh *)
    let instagram_refresh_endpoint = "https://graph.instagram.com/refresh_access_token"
    
    (** Graph API base URL *)
    let api_base = "https://graph.facebook.com/v21.0"
  end
  
  (** Generate authorization URL for Instagram OAuth 2.0 flow
      
      Note: Instagram uses Facebook's OAuth dialog. The user will:
      1. Log in to Facebook
      2. Select which Instagram Business/Creator account to connect
      3. Grant requested permissions
      
      @param client_id Facebook App ID
      @param redirect_uri Registered callback URL
      @param state CSRF protection state parameter
      @param scopes OAuth scopes to request (defaults to Scopes.write)
      @return Full authorization URL to redirect user to
  *)
  let get_authorization_url ~client_id ~redirect_uri ~state ?(scopes=Scopes.write) () =
    let scope_str = String.concat "," scopes in
    let params = [
      ("client_id", client_id);
      ("redirect_uri", redirect_uri);
      ("state", state);
      ("scope", scope_str);
      ("response_type", "code");
      ("auth_type", "rerequest");
    ] in
    let query = Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) params) in
    Printf.sprintf "%s?%s" Metadata.authorization_endpoint query
  
  (** Make functor for OAuth operations that need HTTP client *)
  module Make (Http : HTTP_CLIENT) = struct
    (** Exchange authorization code for short-lived access token
        
        Note: This returns a SHORT-LIVED token (~1-2 hours).
        Call exchange_for_long_lived_token to get a 60-day token.
        
        @param client_id Facebook App ID
        @param client_secret Facebook App Secret
        @param redirect_uri Registered callback URL
        @param code Authorization code from callback
        @param on_result Continuation receiving api_result with credentials
    *)
    let exchange_code ~client_id ~client_secret ~redirect_uri ~code on_result =
      let params = [
        ("client_id", [client_id]);
        ("client_secret", [client_secret]);
        ("redirect_uri", [redirect_uri]);
        ("code", [code]);
      ] in
      let query = Uri.encoded_of_query params in
      let url = Printf.sprintf "%s?%s" Metadata.token_endpoint query in
      
      Http.get url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let creds = parse_credentials_from_json ~default_expires_in:3600 json in
              on_result (Ok creds)
            with e ->
              on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))))
          else
            on_result (Error (parse_api_error ~status_code:response.status ~response_body:response.body)))
        (fun err -> on_result (Error (network_error_of_string err)))
    
    (** Exchange short-lived token for long-lived token (60 days)

        IMPORTANT: Always call this after exchange_code to get a usable token.
        Uses the fb_exchange_token grant type on the Facebook Graph API endpoint.
        This is for Instagram Graph API tokens obtained via Facebook Login.

        @param client_id Facebook App ID
        @param client_secret Facebook App Secret
        @param short_lived_token The short-lived token from exchange_code
        @param on_result Continuation receiving api_result with long-lived credentials
    *)
    let exchange_for_long_lived_token ~client_id ~client_secret ~short_lived_token on_result =
      let params = [
        ("grant_type", ["fb_exchange_token"]);
        ("client_id", [client_id]);
        ("client_secret", [client_secret]);
        ("fb_exchange_token", [short_lived_token]);
      ] in
      let query = Uri.encoded_of_query params in
      let url = Printf.sprintf "%s?%s" Metadata.token_endpoint query in
      
      Http.get url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let creds = parse_credentials_from_json ~default_expires_in:5184000 json in
              on_result (Ok creds)
            with e ->
              on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse long-lived token response: %s" (Printexc.to_string e))))
          else
            on_result (Error (parse_api_error ~status_code:response.status ~response_body:response.body)))
        (fun err -> on_result (Error (network_error_of_string err)))
    
    (** Refresh a long-lived token to extend its validity
        
        Instagram long-lived tokens can be refreshed to get a new 60-day token.
        You should refresh tokens before they expire (within the 60-day window).
        Uses the ig_refresh_token grant type.
        
        @param access_token The current long-lived token to refresh
        @param on_result Continuation receiving api_result with refreshed credentials
    *)
    let refresh_token ?app_secret ~access_token on_result =
      let params = [
        ("grant_type", ["ig_refresh_token"]);
        ("access_token", [access_token]);
      ] @
      (match app_secret with
       | Some secret -> [("appsecret_proof", [compute_app_secret_proof ~app_secret:secret ~access_token])]
       | None -> [])
      in
      let query = Uri.encoded_of_query params in
      let url = Printf.sprintf "%s?%s" Metadata.instagram_refresh_endpoint query in
      
      Http.get url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let creds = parse_credentials_from_json ~default_expires_in:5184000 json in
              on_result (Ok creds)
            with e ->
              on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse refresh response: %s" (Printexc.to_string e))))
          else
            on_result (Error (parse_api_error ~status_code:response.status ~response_body:response.body)))
        (fun err -> on_result (Error (network_error_of_string err)))
    
    (** Instagram Business Account info from /me/accounts discovery *)
    type instagram_account_info = {
      ig_user_id: string;
      ig_username: string;
      page_id: string;
      page_name: string;
    }
    
    (** Discover Instagram Business accounts linked to user's Facebook Pages
        
        After OAuth, use this to find which Instagram accounts the user can manage.
        Returns all Instagram Business/Creator accounts linked to Facebook Pages
        the user has admin access to.
        
        @param user_access_token Long-lived user access token
        @param on_result Continuation receiving api_result with list of Instagram accounts
    *)
    let get_instagram_accounts ?app_secret ~user_access_token on_result =
      let params = [
        ("fields", ["id,name,instagram_business_account{id,username}"]);
        ("access_token", [user_access_token]);
      ] @
      (match app_secret with
       | Some secret -> [("appsecret_proof", [compute_app_secret_proof ~app_secret:secret ~access_token:user_access_token])]
       | None -> [])
      in
      let query = Uri.encoded_of_query params in
      let url = Printf.sprintf "%s/me/accounts?%s" Metadata.api_base query in
      
      Http.get url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let pages_data = json |> member "data" |> to_list in
              let accounts = List.filter_map (fun page ->
                try
                  let page_id = page |> member "id" |> to_string in
                  let page_name = page |> member "name" |> to_string in
                  let ig_account = page |> member "instagram_business_account" in
                  if ig_account = `Null then None
                  else
                    try
                      let ig_user_id = ig_account |> member "id" |> to_string in
                      let ig_username =
                        try ig_account |> member "username" |> to_string
                        with _ -> "unknown"
                      in
                      Some {
                        ig_user_id;
                        ig_username;
                        page_id;
                        page_name;
                      }
                    with _ -> None
                with _ -> None
              ) pages_data in
              on_result (Ok accounts)
            with e ->
              on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse accounts response: %s" (Printexc.to_string e))))
          else
            on_result (Error (parse_api_error ~status_code:response.status ~response_body:response.body)))
        (fun err -> on_result (Error (network_error_of_string err)))
    
    (** Debug/inspect a token to check its validity and permissions
        
        @param access_token The token to inspect
        @param app_token App access token (client_id|client_secret)
        @param on_result Continuation receiving api_result with token info as JSON
    *)
    let debug_token ~access_token ~app_token on_result =
      let params = [
        ("input_token", [access_token]);
        ("access_token", [app_token]);
      ] in
      let query = Uri.encoded_of_query params in
      let url = Printf.sprintf "%s/debug_token?%s" Metadata.api_base query in
      
      Http.get url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              on_result (Ok json)
            with e ->
              on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse debug response: %s" (Printexc.to_string e))))
          else
            on_result (Error (parse_api_error ~status_code:response.status ~response_body:response.body)))
        (fun err -> on_result (Error (network_error_of_string err)))
  end
end

(** {1 Rate Limiting Types} *)

(** Rate limit usage information from X-App-Usage header *)
type rate_limit_info = {
  call_count : int;
  total_cputime : int;
  total_time : int;
  percentage_used : float;
}

(** Configuration module type for Instagram provider *)
module type CONFIG = sig
  module Http : HTTP_CLIENT
  
  val get_env : string -> string option
  val get_credentials : account_id:string -> (credentials -> unit) -> (string -> unit) -> unit
  val update_credentials : account_id:string -> credentials:credentials -> (unit -> unit) -> (string -> unit) -> unit
  val encrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val decrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val update_health_status : account_id:string -> status:string -> error_message:string option -> (unit -> unit) -> (string -> unit) -> unit
  val get_ig_user_id : account_id:string -> (string -> unit) -> (string -> unit) -> unit
  val sleep : float -> (unit -> 'a) -> 'a

  (** Optional: Called when rate limit info is updated *)
  val on_rate_limit_update : rate_limit_info -> unit
end

(** Make functor to create Instagram provider with given configuration *)
module Make (Config : CONFIG) = struct
  let graph_api_base = "https://graph.facebook.com/v21.0"
  module OAuth_http = OAuth.Make(Config.Http)
  
  (** {1 Platform Constants} *)
  
  let max_caption_length = 2200
  let max_carousel_items = 10
  let max_hashtags = 30
  let min_carousel_items = 2
  
  (** {1 Validation Functions} *)
  
  (** Validate a single post's content *)
  let validate_post ~text ?(media_count=0) () =
    let errors = ref [] in
    
    (* Check caption length *)
    let text_len = String.length text in
    if text_len > max_caption_length then
      errors := Error_types.Text_too_long { length = text_len; max = max_caption_length } :: !errors;
    
    (* Count hashtags *)
    let hashtag_count = 
      let rec count_hashtags str pos acc =
        try
          let idx = String.index_from str pos '#' in
          count_hashtags str (idx + 1) (acc + 1)
        with Not_found -> acc
      in
      count_hashtags text 0 0
    in
    if hashtag_count > max_hashtags then
      (* Reusing Too_many_media for hashtag limit - count/max fields work for this *)
      errors := Error_types.Too_many_media { count = hashtag_count; max = max_hashtags } :: !errors;
    
    (* Check media requirement - Instagram requires at least one media item *)
    if media_count = 0 then
      errors := Error_types.Media_required :: !errors;
    
    (* Check carousel limits *)
    if media_count > max_carousel_items then
      errors := Error_types.Too_many_media { count = media_count; max = max_carousel_items } :: !errors;
    
    if !errors = [] then Ok ()
    else Error (List.rev !errors)
  
  (** Validate thread content *)
  let validate_thread ~texts ?(media_counts=[]) () =
    if List.length texts = 0 then
      Error [Error_types.Thread_empty]
    else
      let errors = ref [] in
      List.iteri (fun i text ->
        let media_count = 
          try List.nth media_counts i 
          with _ -> 0 
        in
        match validate_post ~text ~media_count () with
        | Error post_errors ->
            errors := Error_types.Thread_post_invalid { index = i; errors = post_errors } :: !errors
        | Ok () -> ()
      ) texts;
      if !errors = [] then Ok ()
      else Error (List.rev !errors)
  
  (** Validate media URLs *)
  let validate_media ~media_urls =
    let errors = ref [] in
    List.iter (fun url ->
      if not (String.starts_with ~prefix:"http://" url || String.starts_with ~prefix:"https://" url) then
        errors := Error_types.Invalid_url url :: !errors
    ) media_urls;
    if !errors = [] then Ok ()
    else Error (List.rev !errors)
  
  (** {1 Rate Limiting} *)
  
  (** Parse X-App-Usage header *)
  let parse_rate_limit_header headers =
    let parse_usage_number json field =
      try json |> Yojson.Basic.Util.member field |> Yojson.Basic.Util.to_int
      with _ ->
        try
          json
          |> Yojson.Basic.Util.member field
          |> Yojson.Basic.Util.to_float
          |> int_of_float
        with _ -> 0
    in
    try
      let usage_header = 
        List.find_opt (fun (k, _) -> 
          String.lowercase_ascii k = "x-app-usage"
        ) headers 
      in
      match usage_header with
      | Some (_, value) ->
          let json = Yojson.Basic.from_string value in
          let call_count = parse_usage_number json "call_count" in
          let total_cputime = parse_usage_number json "total_cputime" in
          let total_time = parse_usage_number json "total_time" in
          let percentage_used =
            float_of_int (max call_count (max total_cputime total_time))
          in
          Some {
            call_count;
            total_cputime;
            total_time;
            percentage_used;
          }
      | None -> None
    with _ -> None
  
  (** Update rate limit tracking from response *)
  let update_rate_limits response =
    match parse_rate_limit_header response.headers with
    | Some info -> Config.on_rate_limit_update info
    | None -> ()
  
  (** {1 Security - App Secret Proof} *)
  
  (** Compute HMAC-SHA256 app secret proof *)
  let compute_app_secret_proof ~access_token =
    match Config.get_env "FACEBOOK_APP_SECRET" with
    | Some app_secret ->
        let digest = Digestif.SHA256.hmac_string ~key:app_secret access_token in
        Some (Digestif.SHA256.to_hex digest)
    | None -> None
  
  (** Parse Instagram API error and return user-friendly message *)
  let parse_error_response response_body status_code =
    try
      let json = Yojson.Basic.from_string response_body in
      let open Yojson.Basic.Util in
      let error_obj = json |> member "error" in
      let error_code = 
        try error_obj |> member "code" |> to_int
        with _ -> 0
      in
      let error_message = 
        try error_obj |> member "message" |> to_string
        with _ -> response_body
      in
      
      (* Helper to check if string contains substring (case-insensitive) *)
      let string_contains_s str sub =
        try
          let _ = Str.search_forward (Str.regexp_case_fold (Str.quote sub)) str 0 in
          true
        with Not_found -> false
      in
      
      (* Map common error codes to user-friendly messages *)
      let friendly_message = match error_code with
        (* OAuth/Authentication errors *)
        | 190 -> "Instagram access token expired or invalid. Please reconnect your Instagram account."
        | 102 -> "Instagram session expired. Please reconnect your account."
        
        (* Rate limit errors *)
        | 4 -> "Instagram rate limit exceeded. You can post up to 25 times per day. Please try again later."
        | 32 -> "Instagram page rate limit exceeded. Please wait a few minutes before posting again."
        | 613 -> "Too many API calls. Please wait a few minutes and try again."
        
        (* Content errors *)
        | 100 when string_contains_s (String.lowercase_ascii error_message) "business" ->
            "This Instagram account is not a Business or Creator account. Please convert your account: Instagram Settings → Account → Switch to Professional Account"
        | 100 when string_contains_s (String.lowercase_ascii error_message) "image_url" ->
            "Instagram couldn't access the image URL. Make sure the image is publicly accessible via HTTPS."
        | 100 when string_contains_s (String.lowercase_ascii error_message) "caption" ->
            "Caption is too long. Instagram captions must be 2,200 characters or less."
        | 100 when string_contains_s (String.lowercase_ascii error_message) "creation_id" ->
            "Container not ready for publishing. The image is still being processed. Please wait a moment and try again."
        
        (* Media errors *)
        | 9004 -> "Instagram couldn't download the image. Ensure the URL is publicly accessible via HTTPS."
        | 9005 -> "Invalid image format. Please use JPEG or PNG images."
        | 352 when string_contains_s (String.lowercase_ascii error_message) "size" ->
            "Image file is too large. Instagram images must be 8 MB or less."
        
        (* Permission errors *)
        | 10 -> "Missing Instagram permissions. Please reconnect your account and grant all requested permissions."
        | 200 -> "Missing Instagram content publishing permission. Please reconnect and grant instagram_content_publish."
        
        (* Generic fallback *)
        | _ -> Printf.sprintf "Instagram API error (%d): %s" error_code error_message
      in
      
      friendly_message
    with _ ->
      (* Failed to parse JSON error - return raw response *)
      Printf.sprintf "Instagram API error (%d): %s" status_code response_body
  
  (** Parse API error response and return structured Error_types.error *)
  let parse_api_error ~status_code ~response_body =
    OAuth.parse_api_error ~status_code ~response_body

  let api_error_of_response response =
    parse_api_error ~status_code:response.status ~response_body:response.body

  let network_error_of_string err =
    OAuth.network_error_of_string err
  
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
  
  (** Refresh long-lived token (extends validity by 60 days) *)
  let refresh_token ~access_token on_success on_error =
    let params = [
      ("grant_type", ["ig_refresh_token"]);
      ("access_token", [access_token]);
    ] @
    (match compute_app_secret_proof ~access_token with
     | Some proof -> [("appsecret_proof", [proof])]
     | None -> [])
    in

    let query = Uri.encoded_of_query params in
    let url = Printf.sprintf "https://graph.instagram.com/refresh_access_token?%s" query in

    Config.Http.get url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            let credentials =
              OAuth.parse_credentials_from_json ~default_expires_in:5184000 json
            in
            let credentials = { credentials with token_type = "Bearer" } in
            on_success credentials
          with e ->
            on_error (Printf.sprintf "Failed to parse refresh response: %s" (Printexc.to_string e))
        else
          on_error (Error_types.error_to_string (api_error_of_response response)))
      on_error
  
  (** Ensure valid access token, refreshing if needed *)
  let ensure_valid_token ~account_id on_success on_error =
    Config.get_credentials ~account_id
      (fun creds ->
        (* Check if token needs refresh (7 day buffer before expiry) *)
        if is_token_expired_buffer ~buffer_seconds:(7 * 86400) creds.expires_at then
          (* Token is expired or expiring soon - try to refresh it *)
          refresh_token ~access_token:creds.access_token
            (fun refreshed_creds ->
              (* Update stored credentials with refreshed token *)
              Config.update_credentials ~account_id ~credentials:refreshed_creds
                (fun () ->
                  Config.update_health_status ~account_id ~status:"healthy" ~error_message:None
                    (fun () -> on_success refreshed_creds.access_token)
                    (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err))))
                (fun err ->
                  (* Failed to update credentials in DB *)
                  on_error (Error_types.Network_error (Error_types.Connection_failed (Printf.sprintf "Failed to save refreshed token: %s" err)))))
            (fun refresh_err ->
              (* Token refresh failed - mark as expired and ask user to reconnect *)
              Config.update_health_status ~account_id ~status:"token_expired" 
                ~error_message:(Some "Access token expired - please reconnect")
                (fun () -> on_error (Error_types.Auth_error (Error_types.Refresh_failed refresh_err)))
                (fun _ -> on_error (Error_types.Auth_error (Error_types.Refresh_failed refresh_err))))
        else
          (* Token is still valid *)
          Config.update_health_status ~account_id ~status:"healthy" ~error_message:None
            (fun () -> on_success creds.access_token)
            (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err))))
      (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err)))

  (** {1 Insights (P0 Analytics)} *)

  type insight_point = {
    value : int option;
    end_time : string option;
  }

  type account_audience_insights = {
    account_id : string;
    since : string;
    until : string;
    follower_count : insight_point list;
    reach : insight_point list;
  }

  type account_engagement_insights = {
    account_id : string;
    since : string;
    until : string;
    likes : insight_point list;
    views : insight_point list;
    comments : insight_point list;
    shares : insight_point list;
    saves : insight_point list;
    replies : insight_point list;
  }

  type media_insights = {
    post_id : string;
    views : int option;
    reach : int option;
    saved : int option;
    likes : int option;
    comments : int option;
    shares : int option;
  }

  let account_audience_metric_names = [ "follower_count"; "reach" ]

  let account_engagement_metric_names =
    [ "likes"; "views"; "comments"; "shares"; "saves"; "replies" ]

  let media_insight_metric_names =
    [ "views"; "reach"; "saved"; "likes"; "comments"; "shares" ]

  let account_audience_canonical_metric_keys =
    Analytics_normalization.canonical_metric_keys_of_provider_metrics
      ~provider:Analytics_normalization.Instagram
      account_audience_metric_names

  let account_engagement_canonical_metric_keys =
    Analytics_normalization.canonical_metric_keys_of_provider_metrics
      ~provider:Analytics_normalization.Instagram
      account_engagement_metric_names

  let media_insight_canonical_metric_keys =
    Analytics_normalization.canonical_metric_keys_of_provider_metrics
      ~provider:Analytics_normalization.Instagram
      media_insight_metric_names

  let to_canonical_insight_points points =
    points
    |> List.filter_map (fun point ->
           match point.value with
           | Some value ->
               Some (Analytics_types.make_datapoint ?timestamp:point.end_time value)
           | None -> None)

  let to_canonical_optional_point value_opt =
    match value_opt with
    | Some value -> [ Analytics_types.make_datapoint value ]
    | None -> []

  let to_canonical_instagram_series ?time_range ~scope ~provider_metric points =
    match Analytics_normalization.instagram_metric_to_canonical provider_metric with
    | Some metric ->
        Some
          (Analytics_types.make_series
             ?time_range
             ~metric
             ~scope
             ~provider_metric
             points)
    | None -> None

  let to_canonical_account_audience_insights_series
      (account_insights : account_audience_insights) =
    let time_range =
      {
        Analytics_types.since = Some account_insights.since;
        until_ = Some account_insights.until;
        granularity = Some Analytics_types.Day;
      }
    in
    [ ("follower_count", to_canonical_insight_points account_insights.follower_count);
      ("reach", to_canonical_insight_points account_insights.reach) ]
    |> List.filter_map (fun (provider_metric, points) ->
           to_canonical_instagram_series
             ~time_range
             ~scope:Analytics_types.Profile
             ~provider_metric
             points)

  let to_canonical_account_engagement_insights_series
      (account_insights : account_engagement_insights) =
    let time_range =
      {
        Analytics_types.since = Some account_insights.since;
        until_ = Some account_insights.until;
        granularity = Some Analytics_types.Day;
      }
    in
    [ ("likes", to_canonical_insight_points account_insights.likes);
      ("views", to_canonical_insight_points account_insights.views);
      ("comments", to_canonical_insight_points account_insights.comments);
      ("shares", to_canonical_insight_points account_insights.shares);
      ("saves", to_canonical_insight_points account_insights.saves);
      ("replies", to_canonical_insight_points account_insights.replies) ]
    |> List.filter_map (fun (provider_metric, points) ->
           to_canonical_instagram_series
             ~time_range
             ~scope:Analytics_types.Profile
             ~provider_metric
             points)

  let to_canonical_media_insights_series (media_insights : media_insights) =
    [ ("views", to_canonical_optional_point media_insights.views);
      ("reach", to_canonical_optional_point media_insights.reach);
      ("saved", to_canonical_optional_point media_insights.saved);
      ("likes", to_canonical_optional_point media_insights.likes);
      ("comments", to_canonical_optional_point media_insights.comments);
      ("shares", to_canonical_optional_point media_insights.shares) ]
    |> List.filter_map (fun (provider_metric, points) ->
           to_canonical_instagram_series
             ~scope:Analytics_types.Media
             ~provider_metric
             points)

  let int_of_json = function
    | `Int n -> Some n
    | `Float f -> Some (int_of_float f)
    | `String s -> (try Some (int_of_string s) with _ -> None)
    | _ -> None

  let parse_insight_point json =
    let open Yojson.Basic.Util in
    {
      value = int_of_json (json |> member "value");
      end_time = json |> member "end_time" |> to_string_option;
    }

  let parse_metric_points metric_json =
    let open Yojson.Basic.Util in
    let values =
      match metric_json |> member "values" with
      | `List items -> List.map parse_insight_point items
      | _ -> []
    in
    if values <> [] then values
    else
      match metric_json |> member "total_value" |> member "value" |> int_of_json with
      | Some value -> [ { value = Some value; end_time = None } ]
      | None -> []

  let find_metric_points metric_name metric_items =
    List.find_map
      (fun metric_json ->
        let open Yojson.Basic.Util in
        match metric_json |> member "name" |> to_string_option with
        | Some name when name = metric_name -> Some (parse_metric_points metric_json)
        | _ -> None)
      metric_items
    |> Option.value ~default:[]

  let first_metric_value metric_name metric_items =
    let points = find_metric_points metric_name metric_items in
    List.find_map (fun point -> point.value) points

  let parse_account_audience_insights_response ~account_id ~since ~until response_body =
    try
      let open Yojson.Basic.Util in
      let json = Yojson.Basic.from_string response_body in
      let metric_items = json |> member "data" |> to_list in
      Ok
        {
          account_id;
          since;
          until;
          follower_count = find_metric_points "follower_count" metric_items;
          reach = find_metric_points "reach" metric_items;
        }
    with e ->
      Error
        (Error_types.Internal_error
           (Printf.sprintf "Failed to parse account audience insights: %s"
              (Printexc.to_string e)))

  let parse_account_engagement_insights_response ~account_id ~since ~until response_body =
    try
      let open Yojson.Basic.Util in
      let json = Yojson.Basic.from_string response_body in
      let metric_items = json |> member "data" |> to_list in
      Ok
        {
          account_id;
          since;
          until;
          likes = find_metric_points "likes" metric_items;
          views = find_metric_points "views" metric_items;
          comments = find_metric_points "comments" metric_items;
          shares = find_metric_points "shares" metric_items;
          saves = find_metric_points "saves" metric_items;
          replies = find_metric_points "replies" metric_items;
        }
    with e ->
      Error
        (Error_types.Internal_error
           (Printf.sprintf "Failed to parse account engagement insights: %s"
              (Printexc.to_string e)))

  let parse_media_insights_response ~post_id response_body =
    try
      let open Yojson.Basic.Util in
      let json = Yojson.Basic.from_string response_body in
      let metric_items = json |> member "data" |> to_list in
      Ok
        {
          post_id;
          views = first_metric_value "views" metric_items;
          reach = first_metric_value "reach" metric_items;
          saved = first_metric_value "saved" metric_items;
          likes = first_metric_value "likes" metric_items;
          comments = first_metric_value "comments" metric_items;
          shares = first_metric_value "shares" metric_items;
        }
    with e ->
      Error
        (Error_types.Internal_error
           (Printf.sprintf "Failed to parse media insights: %s" (Printexc.to_string e)))

  let get_account_audience_insights ~id ~access_token ~since ~until on_result =
    let params =
      [
        ("metric", [ String.concat "," account_audience_metric_names ]);
        ("period", [ "day" ]);
        ("since", [ since ]);
        ("until", [ until ]);
        ("access_token", [ access_token ]);
      ]
    in
    let query = Uri.encoded_of_query params in
    let url = Printf.sprintf "%s/%s/insights?%s" graph_api_base id query in
    Config.Http.get url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          on_result
            (parse_account_audience_insights_response ~account_id:id ~since ~until
               response.body)
        else on_result (Error (api_error_of_response response)))
      (fun err -> on_result (Error (network_error_of_string err)))

  let get_account_audience_insights_canonical ~id ~access_token ~since ~until on_result =
    get_account_audience_insights ~id ~access_token ~since ~until
      (function
        | Ok insights ->
            on_result (Ok (to_canonical_account_audience_insights_series insights))
        | Error err -> on_result (Error err))

  let get_account_engagement_insights ~id ~access_token ~since ~until on_result =
    let params =
      [
        ("metric_type", [ "total_value" ]);
        ("metric", [ String.concat "," account_engagement_metric_names ]);
        ("period", [ "day" ]);
        ("since", [ since ]);
        ("until", [ until ]);
        ("access_token", [ access_token ]);
      ]
    in
    let query = Uri.encoded_of_query params in
    let url = Printf.sprintf "%s/%s/insights?%s" graph_api_base id query in
    Config.Http.get url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          on_result
            (parse_account_engagement_insights_response ~account_id:id ~since ~until
               response.body)
        else on_result (Error (api_error_of_response response)))
      (fun err -> on_result (Error (network_error_of_string err)))

  let get_account_engagement_insights_canonical ~id ~access_token ~since ~until on_result =
    get_account_engagement_insights ~id ~access_token ~since ~until
      (function
        | Ok insights ->
            on_result (Ok (to_canonical_account_engagement_insights_series insights))
        | Error err -> on_result (Error err))

  let get_media_insights ~post_id ~access_token on_result =
    let params =
      [
        ("metric", [ String.concat "," media_insight_metric_names ]);
        ("access_token", [ access_token ]);
      ]
    in
    let query = Uri.encoded_of_query params in
    let url = Printf.sprintf "%s/%s/insights?%s" graph_api_base post_id query in
    Config.Http.get url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          on_result (parse_media_insights_response ~post_id response.body)
        else on_result (Error (api_error_of_response response)))
      (fun err -> on_result (Error (network_error_of_string err)))

  let get_media_insights_canonical ~post_id ~access_token on_result =
    get_media_insights ~post_id ~access_token
      (function
        | Ok insights -> on_result (Ok (to_canonical_media_insights_series insights))
        | Error err -> on_result (Error err))

  let get_account_audience_insights_for_account ~account_id ~since ~until on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        Config.get_ig_user_id ~account_id
          (fun id ->
            get_account_audience_insights ~id ~access_token ~since ~until on_result)
          (fun err ->
            on_result
              (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  let get_account_engagement_insights_for_account ~account_id ~since ~until on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        Config.get_ig_user_id ~account_id
          (fun id ->
            get_account_engagement_insights ~id ~access_token ~since ~until on_result)
          (fun err ->
            on_result
              (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  let get_media_insights_for_account ~account_id ~post_id on_result =
    ensure_valid_token ~account_id
      (fun access_token -> get_media_insights ~post_id ~access_token on_result)
      (fun err -> on_result (Error err))

  (** Media type detection from URL *)
  let classify_media_url url =
    let url_lower = String.lowercase_ascii url in
    if Str.string_match (Str.regexp ".*\\.\\(mp4\\|mov\\)$") url_lower 0 then
      `Video
    else if Str.string_match (Str.regexp ".*\\.\\(jpg\\|jpeg\\|png\\)$") url_lower 0 then
      `Image
    else
      `Unsupported

  let detect_media_type url =
    match classify_media_url url with
    | `Video -> "VIDEO"
    | `Image | `Unsupported -> "IMAGE"  (* Keep legacy default; validated earlier in flows *)

  let validate_supported_media_urls ~media_urls =
    let errors =
      List.fold_left (fun acc url ->
        match classify_media_url url with
        | `Image | `Video -> acc
        | `Unsupported -> Error_types.Media_unsupported_format ("Unsupported media format URL: " ^ url) :: acc
      ) [] media_urls
    in
    if errors = [] then Ok () else Error (List.rev errors)
  
  (** {1 Tagging helper} *)

  (** Build optional tagging and location parameters for container creation *)
  let tagging_params ?(user_tags=[]) ?location_id ?(collaborators=[]) () =
    let user_tag_params = match user_tags with
      | [] -> []
      | tags ->
          let json_tags = List.map (fun (username, x, y) ->
            Printf.sprintf {|{"username":"%s","x":%f,"y":%f}|} username x y
          ) tags in
          [("user_tags", [Printf.sprintf "[%s]" (String.concat "," json_tags)])]
    in
    let location_params = match location_id with
      | Some loc -> [("location_id", [loc])]
      | None -> []
    in
    let collab_params = match collaborators with
      | [] -> []
      | collabs -> [("collaborators", [String.concat "," collabs])]
    in
    user_tag_params @ location_params @ collab_params

  (** Step 1a: Create single image container *)
  let create_image_container ~ig_user_id ~access_token ~image_url ~caption ~alt_text ~is_carousel_item ?(user_tags=[]) ?location_id ?(collaborators=[]) on_result =
    let url = Printf.sprintf "%s/%s/media" graph_api_base ig_user_id in

    let base_params = [
      ("image_url", [image_url]);
    ] in

    (* Add alt text if provided *)
    let base_with_alt = match alt_text with
      | Some alt when String.length alt > 0 ->
          ("custom_accessibility_caption", [alt]) :: base_params
      | _ -> base_params
    in

    let params =
      (if is_carousel_item then
        (* Carousel items don't include caption, and mark as carousel item *)
        ("is_carousel_item", ["true"]) :: base_with_alt
      else
        (* Regular posts include caption *)
        ("caption", [caption]) :: base_with_alt) @
      (tagging_params ~user_tags ?location_id ~collaborators ()) @
      (* Add app secret proof if available *)
      (match compute_app_secret_proof ~access_token with
       | Some proof -> [("appsecret_proof", [proof])]
       | None -> [])
    in

    let body = Uri.encoded_of_query params in
    let headers = [
      ("Content-Type", "application/x-www-form-urlencoded");
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
    ] in

    Config.Http.post ~headers ~body url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          try
            let open Yojson.Basic.Util in
            let json = Yojson.Basic.from_string response.body in
            let container_id = json |> member "id" |> to_string in
            on_result (Ok container_id)
          with e ->
            on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse container response: %s" (Printexc.to_string e))))
        else
          on_result (Error (api_error_of_response response)))
      (fun err -> on_result (Error (network_error_of_string err)))
  
  (** Step 1b: Create video container *)
  let create_video_container ~ig_user_id ~access_token ~video_url ~caption ~alt_text ~media_type ~is_carousel_item ?(user_tags=[]) ?location_id ?(collaborators=[]) ?share_to_feed ?cover_url ?thumb_offset ?audio_name on_result =
    let url = Printf.sprintf "%s/%s/media" graph_api_base ig_user_id in

    let base_params = [
      ("media_type", [media_type]); (* "VIDEO" or "REELS" *)
      ("video_url", [video_url]);
    ] in

    (* Add alt text if provided *)
    let base_with_alt = match alt_text with
      | Some alt when String.length alt > 0 ->
          ("custom_accessibility_caption", [alt]) :: base_params
      | _ -> base_params
    in

    (* Add reel-specific parameters *)
    let reel_params =
      (match share_to_feed with
       | Some true -> [("share_to_feed", ["true"])]
       | Some false -> [("share_to_feed", ["false"])]
       | None -> []) @
      (match cover_url with
       | Some u -> [("cover_url", [u])]
       | None -> []) @
      (match thumb_offset with
       | Some ms -> [("thumb_offset", [string_of_int ms])]
       | None -> []) @
      (match audio_name with
       | Some name -> [("audio_name", [name])]
       | None -> [])
    in

    let params =
      (if is_carousel_item then
        (* Carousel items don't include caption, and mark as carousel item *)
        ("is_carousel_item", ["true"]) :: base_with_alt
      else
        (* Regular posts include caption *)
        ("caption", [caption]) :: base_with_alt) @
      (tagging_params ~user_tags ?location_id ~collaborators ()) @
      reel_params @
      (* Add app secret proof if available *)
      (match compute_app_secret_proof ~access_token with
       | Some proof -> [("appsecret_proof", [proof])]
       | None -> [])
    in

    let body = Uri.encoded_of_query params in
    let headers = [
      ("Content-Type", "application/x-www-form-urlencoded");
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
    ] in

    Config.Http.post ~headers ~body url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          try
            let open Yojson.Basic.Util in
            let json = Yojson.Basic.from_string response.body in
            let container_id = json |> member "id" |> to_string in
            on_result (Ok container_id)
          with e ->
            on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse video container response: %s" (Printexc.to_string e))))
        else
          on_result (Error (api_error_of_response response)))
      (fun err -> on_result (Error (network_error_of_string err)))
  
  (** Step 1c: Create carousel container from child containers *)
  let create_carousel_container ~ig_user_id ~access_token ~children_ids ~caption on_result =
    let url = Printf.sprintf "%s/%s/media" graph_api_base ig_user_id in
    
    let params = [
      ("media_type", ["CAROUSEL"]);
      ("children", [String.concat "," children_ids]);
      ("caption", [caption]);
    ] @
    (* Add app secret proof if available *)
    (match compute_app_secret_proof ~access_token with
     | Some proof -> [("appsecret_proof", [proof])]
     | None -> [])
    in
    
    let body = Uri.encoded_of_query params in
    let headers = [
      ("Content-Type", "application/x-www-form-urlencoded");
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
    ] in
    
    Config.Http.post ~headers ~body url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          try
            let open Yojson.Basic.Util in
            let json = Yojson.Basic.from_string response.body in
            let container_id = json |> member "id" |> to_string in
            on_result (Ok container_id)
          with e ->
            on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse carousel container response: %s" (Printexc.to_string e))))
        else
          on_result (Error (api_error_of_response response)))
      (fun err -> on_result (Error (network_error_of_string err)))
  
  (** Step 2: Publish container *)
  let publish_container ~ig_user_id ~access_token ~container_id on_result =
    let url = Printf.sprintf "%s/%s/media_publish" graph_api_base ig_user_id in
    
    let params = [
      ("creation_id", [container_id]);
    ] @
    (* Add app secret proof if available *)
    (match compute_app_secret_proof ~access_token with
     | Some proof -> [("appsecret_proof", [proof])]
     | None -> [])
    in
    
    let body = Uri.encoded_of_query params in
    let headers = [
      ("Content-Type", "application/x-www-form-urlencoded");
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
    ] in
    
    Config.Http.post ~headers ~body url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          try
            let open Yojson.Basic.Util in
            let json = Yojson.Basic.from_string response.body in
            let media_id = json |> member "id" |> to_string in
            on_result (Ok media_id)
          with e ->
            on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse publish response: %s" (Printexc.to_string e))))
        else
          on_result (Error (api_error_of_response response)))
      (fun err -> on_result (Error (network_error_of_string err)))
  
  (** Check container status *)
  let check_container_status ~container_id ~access_token on_result =
    let proof_params = match compute_app_secret_proof ~access_token with
      | Some proof -> [("appsecret_proof", [proof])]
      | None -> []
    in
    let query_params = [("fields", ["status_code,status"])] @ proof_params in
    let query = Uri.encoded_of_query query_params in
    let url = Printf.sprintf "%s/%s?%s" graph_api_base container_id query in
    
    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
    ] in
    
    Config.Http.get ~headers url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            let open Yojson.Basic.Util in
            let status_code = json |> member "status_code" |> to_string in
            let status = json |> member "status" |> to_string_option |> Option.value ~default:"UNKNOWN" in
            on_result (Ok (status_code, status))
          with e ->
            on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse status: %s" (Printexc.to_string e))))
        else
          on_result (Error (api_error_of_response response)))
      (fun err -> on_result (Error (network_error_of_string err)))
  
  (** Poll container status with exponential backoff *)
  let rec poll_container_status ~container_id ~access_token ~ig_user_id ~attempt ~max_attempts on_success on_error =
    if attempt > max_attempts then
      on_error (Printf.sprintf "Container still processing after %d attempts. Try publishing again in a few minutes." max_attempts)
    else
      (* Exponential backoff: 2s, 3s, 5s, 8s, 13s *)
      let delay = match attempt with
        | 1 -> 2.0
        | 2 -> 3.0
        | 3 -> 5.0
        | 4 -> 8.0
        | _ -> 13.0
      in
      
      Config.sleep delay (fun () ->
        check_container_status ~container_id ~access_token
          (function
            | Ok (status_code, status) ->
                (match status_code with
                | "FINISHED" ->
                    (* Container ready - publish it *)
                    publish_container ~ig_user_id ~access_token ~container_id
                      (function
                        | Ok media_id -> on_success media_id
                        | Error e -> on_error (Error_types.error_to_string e))
                | "ERROR" ->
                    (* Container processing failed *)
                    let error_detail = if status <> "UNKNOWN" && status <> "" 
                      then Printf.sprintf ": %s" status 
                      else "" in
                    on_error (Printf.sprintf "Instagram container processing failed%s" error_detail)
                | "IN_PROGRESS" ->
                    (* Still processing - retry with next attempt *)
                    poll_container_status ~container_id ~access_token ~ig_user_id 
                      ~attempt:(attempt + 1) ~max_attempts on_success on_error
                | _ ->
                    (* Unknown status code - keep polling until max attempts *)
                    poll_container_status ~container_id ~access_token ~ig_user_id
                      ~attempt:(attempt + 1) ~max_attempts on_success on_error)
            | Error _err ->
                (* Status check failed - retry until max attempts *)
                poll_container_status ~container_id ~access_token ~ig_user_id
                  ~attempt:(attempt + 1) ~max_attempts on_success on_error))
  
  (** Create child containers for carousel (recursive) *)
  let rec create_carousel_children ~ig_user_id ~access_token ~media_urls_with_alt ~index ~acc on_success on_error =
    match media_urls_with_alt with
    | [] -> on_success (List.rev acc)
    | (url, alt_text) :: rest ->
        let media_type = detect_media_type url in
        
        (match media_type with
        | "VIDEO" ->
            (* Create video carousel item *)
            create_video_container ~ig_user_id ~access_token ~video_url:url 
              ~caption:"" ~alt_text ~media_type:"VIDEO" ~is_carousel_item:true
              (function
                | Ok child_id ->
                    create_carousel_children ~ig_user_id ~access_token 
                      ~media_urls_with_alt:rest ~index:(index + 1) ~acc:(child_id :: acc)
                      on_success on_error
                | Error e -> on_error (Error_types.error_to_string e))
        | _ ->
            (* Create image carousel item *)
            create_image_container ~ig_user_id ~access_token ~image_url:url 
              ~caption:"" ~alt_text ~is_carousel_item:true
              (function
                | Ok child_id ->
                    create_carousel_children ~ig_user_id ~access_token 
                      ~media_urls_with_alt:rest ~index:(index + 1) ~acc:(child_id :: acc)
                      on_success on_error
                | Error e -> on_error (Error_types.error_to_string e)))
  
  (** Post to Instagram with two-step process
      
      Note: Instagram uses URL-based container creation where the platform
      fetches media directly. Client-side file size validation is not possible.
      Instagram enforces its own limits: 8MB images, 1GB video.
  *)
  let post_single ~account_id ~text ~media_urls ?(alt_texts=[]) on_result =
    let media_count = List.length media_urls in
    
    (* Validate content first - includes media count check *)
    match validate_post ~text ~media_count () with
    | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        (* Validate media URLs are accessible *)
        match validate_media ~media_urls with
        | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
        | Ok () ->
            (match validate_supported_media_urls ~media_urls with
             | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
             | Ok () ->
                 if media_count > 1 then
                   (* Carousel post with 2-10 items *)
                   ensure_valid_token ~account_id
                     (fun access_token ->
                       Config.get_ig_user_id ~account_id
                         (fun ig_user_id ->
                           (* Pair URLs with alt text *)
                           let media_urls_with_alt = List.mapi (fun i url ->
                             let alt_text = try List.nth alt_texts i with _ -> None in
                             (url, alt_text)
                           ) media_urls in
                           (* Step 1: Create child containers for each media item *)
                           create_carousel_children ~ig_user_id ~access_token ~media_urls_with_alt
                             ~index:0 ~acc:[]
                             (fun children_ids ->
                               (* Step 2: Create parent carousel container *)
                               create_carousel_container ~ig_user_id ~access_token
                                 ~children_ids ~caption:text
                                 (function
                                   | Ok carousel_id ->
                                       (* Step 3: Poll carousel status and publish when ready *)
                                       poll_container_status ~container_id:carousel_id
                                         ~access_token ~ig_user_id ~attempt:1 ~max_attempts:5
                                         (fun media_id -> on_result (Error_types.Success media_id))
                                         (fun err -> on_result (Error_types.Failure
                                           (Error_types.Internal_error err)))
                                   | Error e -> on_result (Error_types.Failure e)))
                             (fun err -> on_result (Error_types.Failure
                               (Error_types.Internal_error err))))
                         (fun err -> on_result (Error_types.Failure
                           (Error_types.Internal_error err))))
                     (fun err -> on_result (Error_types.Failure err))
                 else
                   (* Single image or video post *)
                   ensure_valid_token ~account_id
                     (fun access_token ->
                       Config.get_ig_user_id ~account_id
                         (fun ig_user_id ->
                           let media_url = List.hd media_urls in
                           let media_type = detect_media_type media_url in
                           let alt_text = try List.nth alt_texts 0 with _ -> None in

                           (* Step 1: Create container based on media type *)
                           match media_type with
                           | "VIDEO" ->
                               create_video_container ~ig_user_id ~access_token ~video_url:media_url
                                 ~caption:text ~alt_text ~media_type:"VIDEO" ~is_carousel_item:false
                                 (function
                                   | Ok container_id ->
                                       (* Step 2: Poll container status and publish when ready *)
                                       poll_container_status ~container_id ~access_token ~ig_user_id
                                         ~attempt:1 ~max_attempts:5
                                         (fun media_id -> on_result (Error_types.Success media_id))
                                         (fun err -> on_result (Error_types.Failure
                                           (Error_types.Internal_error err)))
                                   | Error e -> on_result (Error_types.Failure e))
                           | _ ->
                               create_image_container ~ig_user_id ~access_token ~image_url:media_url
                                 ~caption:text ~alt_text ~is_carousel_item:false
                                 (function
                                   | Ok container_id ->
                                       (* Step 2: Poll container status and publish when ready *)
                                       poll_container_status ~container_id ~access_token ~ig_user_id
                                         ~attempt:1 ~max_attempts:5
                                         (fun media_id -> on_result (Error_types.Success media_id))
                                         (fun err -> on_result (Error_types.Failure
                                           (Error_types.Internal_error err)))
                                   | Error e -> on_result (Error_types.Failure e)))
                         (fun err -> on_result (Error_types.Failure
                           (Error_types.Internal_error err))))
                     (fun err -> on_result (Error_types.Failure err)))
  
  (** {1 Comment Posting (H1)} *)

  (** Post a comment on a media object

      Requires the instagram_manage_comments permission.

      @param media_id The ID of the media to comment on
      @param access_token Valid access token
      @param message The comment text
      @param on_result Continuation receiving api_result with the new comment ID
  *)
  let post_comment ~media_id ~access_token ~message on_result =
    let url = Printf.sprintf "%s/%s/comments" graph_api_base media_id in

    let params = [
      ("message", [message]);
    ] @
    (match compute_app_secret_proof ~access_token with
     | Some proof -> [("appsecret_proof", [proof])]
     | None -> [])
    in

    let body = Uri.encoded_of_query params in
    let headers = [
      ("Content-Type", "application/x-www-form-urlencoded");
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
    ] in

    Config.Http.post ~headers ~body url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          try
            let open Yojson.Basic.Util in
            let json = Yojson.Basic.from_string response.body in
            let comment_id = json |> member "id" |> to_string in
            on_result (Ok comment_id)
          with e ->
            on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse comment response: %s" (Printexc.to_string e))))
        else
          on_result (Error (api_error_of_response response)))
      (fun err -> on_result (Error (network_error_of_string err)))

  (** Post a comment using account credentials (auto-refreshes token)

      @param account_id Internal account identifier
      @param media_id The ID of the media to comment on
      @param message The comment text
      @param on_result Continuation receiving api_result with the new comment ID
  *)
  let post_comment_for_account ~account_id ~media_id ~message on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        post_comment ~media_id ~access_token ~message on_result)
      (fun err -> on_result (Error err))

  (** {1 Media Listing (H2)} *)

  (** Media item returned from the media listing endpoint *)
  type media_item = {
    id : string;
    caption : string option;
    timestamp : string option;
    media_type : string option;
    media_url : string option;
    thumbnail_url : string option;
    permalink : string option;
  }

  (** List media for an Instagram user

      @param ig_user_id Instagram Business Account ID
      @param access_token Valid access token
      @param limit Maximum number of items to return (optional, API default applies)
      @param on_result Continuation receiving api_result with list of media items
  *)
  let get_user_media ~ig_user_id ~access_token ?limit on_result =
    let params = [
      ("fields", ["id,caption,timestamp,media_type,media_url,thumbnail_url,permalink"]);
      ("access_token", [access_token]);
    ] @
    (match limit with
     | Some n -> [("limit", [string_of_int n])]
     | None -> []) @
    (match compute_app_secret_proof ~access_token with
     | Some proof -> [("appsecret_proof", [proof])]
     | None -> [])
    in

    let query = Uri.encoded_of_query params in
    let url = Printf.sprintf "%s/%s/media?%s" graph_api_base ig_user_id query in

    Config.Http.get url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          try
            let open Yojson.Basic.Util in
            let json = Yojson.Basic.from_string response.body in
            let data = json |> member "data" |> to_list in
            let items = List.map (fun item ->
              {
                id = item |> member "id" |> to_string;
                caption = item |> member "caption" |> to_string_option;
                timestamp = item |> member "timestamp" |> to_string_option;
                media_type = item |> member "media_type" |> to_string_option;
                media_url = item |> member "media_url" |> to_string_option;
                thumbnail_url = item |> member "thumbnail_url" |> to_string_option;
                permalink = item |> member "permalink" |> to_string_option;
              }
            ) data in
            on_result (Ok items)
          with e ->
            on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse media listing response: %s" (Printexc.to_string e))))
        else
          on_result (Error (api_error_of_response response)))
      (fun err -> on_result (Error (network_error_of_string err)))

  (** List media for an account (auto-refreshes token)

      @param account_id Internal account identifier
      @param limit Maximum number of items to return (optional)
      @param on_result Continuation receiving api_result with list of media items
  *)
  let get_user_media_for_account ~account_id ?limit on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        Config.get_ig_user_id ~account_id
          (fun ig_user_id ->
            get_user_media ~ig_user_id ~access_token ?limit on_result)
          (fun err ->
            on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** {1 Media Deletion (H3)} *)

  (** Delete a media object

      @param media_id The ID of the media to delete
      @param access_token Valid access token
      @param on_result Continuation receiving api_result with success boolean
  *)
  let delete_media ~media_id ~access_token on_result =
    let proof_params = match compute_app_secret_proof ~access_token with
      | Some proof -> [("appsecret_proof", [proof])]
      | None -> []
    in
    let query = Uri.encoded_of_query proof_params in
    let base_url = Printf.sprintf "%s/%s" graph_api_base media_id in
    let url = if query = "" then base_url else Printf.sprintf "%s?%s" base_url query in

    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
    ] in

    Config.Http.delete ~headers url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          try
            let open Yojson.Basic.Util in
            let json = Yojson.Basic.from_string response.body in
            let success = json |> member "success" |> to_bool in
            on_result (Ok success)
          with _ ->
            (* If parsing fails but status is 2xx, treat as success *)
            on_result (Ok true)
        else
          on_result (Error (api_error_of_response response)))
      (fun err -> on_result (Error (network_error_of_string err)))

  (** Delete a media object using account credentials (auto-refreshes token)

      @param account_id Internal account identifier
      @param media_id The ID of the media to delete
      @param on_result Continuation receiving api_result with success boolean
  *)
  let delete_media_for_account ~account_id ~media_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        delete_media ~media_id ~access_token on_result)
      (fun err -> on_result (Error err))

  (** Post Reel (short-form video) *)
  let post_reel ~account_id ~text ~video_url ?(alt_text=None) ?(share_to_feed=None) ?(cover_url=None) ?(thumb_offset=None) ?(audio_name=None) on_result =
    (* Validate caption *)
    match validate_post ~text ~media_count:1 () with
    | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        (match validate_media ~media_urls:[video_url] with
         | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
         | Ok () ->
             match classify_media_url video_url with
             | `Unsupported | `Image ->
                 on_result (Error_types.Failure (Error_types.Validation_error [Error_types.Media_unsupported_format "Instagram videos must be MP4 or MOV format"]))
             | `Video ->
                 ensure_valid_token ~account_id
                   (fun access_token ->
                     Config.get_ig_user_id ~account_id
                       (fun ig_user_id ->
                         (* Create Reel container with REELS media type *)
                         create_video_container ~ig_user_id ~access_token ~video_url
                           ~caption:text ~alt_text ~media_type:"REELS" ~is_carousel_item:false
                           ?share_to_feed ?cover_url ?thumb_offset ?audio_name
                           (function
                             | Ok container_id ->
                                 (* Poll and publish *)
                                 poll_container_status ~container_id ~access_token ~ig_user_id
                                   ~attempt:1 ~max_attempts:5
                                   (fun media_id -> on_result (Error_types.Success media_id))
                                   (fun err -> on_result (Error_types.Failure
                                     (Error_types.Internal_error err)))
                             | Error e -> on_result (Error_types.Failure e)))
                       (fun err -> on_result (Error_types.Failure
                         (Error_types.Internal_error err))))
                   (fun err -> on_result (Error_types.Failure err)))
  
  (** {1 Instagram Stories} *)
  
  (** Create story container for image
      
      Instagram Stories use a separate media_type "STORIES".
      Stories are full-screen vertical content (9:16 aspect ratio recommended).
      Stories expire after 24 hours.
      
      @param ig_user_id Instagram Business Account ID
      @param access_token Valid access token
      @param image_url Publicly accessible URL of the image
      @param on_result Continuation receiving api_result with container ID
  *)
  let create_story_image_container ~ig_user_id ~access_token ~image_url on_result =
    let url = Printf.sprintf "%s/%s/media" graph_api_base ig_user_id in
    
    let params = [
      ("media_type", ["STORIES"]);
      ("image_url", [image_url]);
    ] @
    (* Add app secret proof if available *)
    (match compute_app_secret_proof ~access_token with
     | Some proof -> [("appsecret_proof", [proof])]
     | None -> [])
    in
    
    let body = Uri.encoded_of_query params in
    let headers = [
      ("Content-Type", "application/x-www-form-urlencoded");
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
    ] in
    
    Config.Http.post ~headers ~body url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          try
            let open Yojson.Basic.Util in
            let json = Yojson.Basic.from_string response.body in
            let container_id = json |> member "id" |> to_string in
            on_result (Ok container_id)
          with e ->
            on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse story container response: %s" (Printexc.to_string e))))
        else
          on_result (Error (api_error_of_response response)))
      (fun err -> on_result (Error (network_error_of_string err)))
  
  (** Create story container for video
      
      Video stories must be:
      - MP4 or MOV format
      - 1-60 seconds duration
      - Recommended: 9:16 aspect ratio (1080x1920)
      
      @param ig_user_id Instagram Business Account ID
      @param access_token Valid access token
      @param video_url Publicly accessible URL of the video
      @param on_result Continuation receiving api_result with container ID
  *)
  let create_story_video_container ~ig_user_id ~access_token ~video_url on_result =
    let url = Printf.sprintf "%s/%s/media" graph_api_base ig_user_id in
    
    let params = [
      ("media_type", ["STORIES"]);
      ("video_url", [video_url]);
    ] @
    (* Add app secret proof if available *)
    (match compute_app_secret_proof ~access_token with
     | Some proof -> [("appsecret_proof", [proof])]
     | None -> [])
    in
    
    let body = Uri.encoded_of_query params in
    let headers = [
      ("Content-Type", "application/x-www-form-urlencoded");
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
    ] in
    
    Config.Http.post ~headers ~body url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          try
            let open Yojson.Basic.Util in
            let json = Yojson.Basic.from_string response.body in
            let container_id = json |> member "id" |> to_string in
            on_result (Ok container_id)
          with e ->
            on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse story video container response: %s" (Printexc.to_string e))))
        else
          on_result (Error (api_error_of_response response)))
      (fun err -> on_result (Error (network_error_of_string err)))
  
  (** Post image story to Instagram
      
      Posts an image as an Instagram Story. Stories are full-screen vertical
      content that expires after 24 hours.
      
      Requirements:
      - Image must be publicly accessible via HTTPS
      - Recommended: 9:16 aspect ratio (1080x1920 pixels)
      - Supported formats: JPEG, PNG
      - Maximum file size: 8 MB
      
      @param account_id Internal account identifier
      @param image_url Publicly accessible URL of the image
      @param on_result Continuation receiving outcome with media ID
  *)
  let post_story_image ~account_id ~image_url on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        Config.get_ig_user_id ~account_id
          (fun ig_user_id ->
            (* Create story container with image *)
            create_story_image_container ~ig_user_id ~access_token ~image_url
              (function
                | Ok container_id ->
                    (* Poll and publish *)
                    poll_container_status ~container_id ~access_token ~ig_user_id 
                      ~attempt:1 ~max_attempts:5 
                      (fun media_id -> on_result (Error_types.Success media_id))
                      (fun err -> on_result (Error_types.Failure 
                        (Error_types.Internal_error err)))
                | Error e -> on_result (Error_types.Failure e)))
          (fun err -> on_result (Error_types.Failure 
            (Error_types.Internal_error err))))
      (fun err -> on_result (Error_types.Failure err))
  
  (** Post video story to Instagram
      
      Posts a video as an Instagram Story. Stories are full-screen vertical
      content that expires after 24 hours.
      
      Requirements:
      - Video must be publicly accessible via HTTPS
      - Duration: 1-60 seconds
      - Recommended: 9:16 aspect ratio (1080x1920 pixels)
      - Supported formats: MP4, MOV
      - Supported codecs: H.264 video, AAC audio
      - Maximum file size: 100 MB (varies by duration)
      
      @param account_id Internal account identifier
      @param video_url Publicly accessible URL of the video
      @param on_result Continuation receiving outcome with media ID
  *)
  let post_story_video ~account_id ~video_url on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        Config.get_ig_user_id ~account_id
          (fun ig_user_id ->
            (* Create story container with video *)
            create_story_video_container ~ig_user_id ~access_token ~video_url
              (function
                | Ok container_id ->
                    (* Poll and publish - videos need more time to process *)
                    poll_container_status ~container_id ~access_token ~ig_user_id 
                      ~attempt:1 ~max_attempts:10 
                      (fun media_id -> on_result (Error_types.Success media_id))
                      (fun err -> on_result (Error_types.Failure 
                        (Error_types.Internal_error err)))
                | Error e -> on_result (Error_types.Failure e)))
          (fun err -> on_result (Error_types.Failure 
            (Error_types.Internal_error err))))
      (fun err -> on_result (Error_types.Failure err))
  
  (** Post story to Instagram (auto-detect media type)
      
      Automatically detects whether the media is an image or video based on
      the file extension and posts it as an Instagram Story.
      
      @param account_id Internal account identifier
      @param media_url Publicly accessible URL of the image or video
      @param on_result Continuation receiving outcome with media ID
  *)
  let post_story ~account_id ~media_url on_result =
    let url_lower = String.lowercase_ascii media_url in
    if not (String.starts_with ~prefix:"http://" url_lower || String.starts_with ~prefix:"https://" url_lower) then
      on_result (Error_types.Failure (Error_types.Validation_error [Error_types.Invalid_url media_url]))
    else
      match classify_media_url media_url with
      | `Video -> post_story_video ~account_id ~video_url:media_url on_result
      | `Image -> post_story_image ~account_id ~image_url:media_url on_result
      | `Unsupported ->
          on_result (Error_types.Failure (Error_types.Validation_error [Error_types.Media_unsupported_format "Story media must be an image (JPEG, PNG) or video (MP4, MOV)"]))
  
  (** Validate story media
      
      Validates that the media URL is appropriate for Instagram Stories.
      
      @param media_url The URL to validate
      @return Ok () if valid, Error message otherwise
  *)
  let validate_story ~media_url =
    let url_lower = String.lowercase_ascii media_url in
    (* Check if URL is accessible (starts with http/https) *)
    if not (String.starts_with ~prefix:"http://" url_lower || String.starts_with ~prefix:"https://" url_lower) then
      Error "Story media URL must be a publicly accessible HTTP(S) URL"
    else
      (* Check for valid image or video extension *)
      let is_image = Str.string_match (Str.regexp ".*\\.\\(jpg\\|jpeg\\|png\\)$") url_lower 0 in
      let is_video = Str.string_match (Str.regexp ".*\\.\\(mp4\\|mov\\)$") url_lower 0 in
      if not (is_image || is_video) then
        Error "Story media must be an image (JPEG, PNG) or video (MP4, MOV)"
      else
        Ok ()
  
  (** Post thread (Instagram doesn't support threads, posts only first item with warning) *)
  let post_thread ~account_id ~texts ~media_urls_per_post ?(alt_texts_per_post=[]) on_result =
    if List.length texts = 0 then
      on_result (Error_types.Failure (Error_types.Validation_error [Error_types.Thread_empty]))
    else
      let first_text = List.hd texts in
      let first_media = if List.length media_urls_per_post > 0 then List.hd media_urls_per_post else [] in
      let first_alt_texts = if List.length alt_texts_per_post > 0 then List.hd alt_texts_per_post else [] in
      let total_posts = List.length texts in
      
      post_single ~account_id ~text:first_text ~media_urls:first_media ~alt_texts:first_alt_texts
        (fun outcome ->
          match outcome with
          | Error_types.Success post_id ->
              let thread_result = {
                Error_types.posted_ids = [post_id];
                failed_at_index = None;
                total_requested = total_posts;
              } in
              if total_posts > 1 then
                (* Warn that only first post was published *)
                on_result (Error_types.Partial_success {
                  result = thread_result;
                  warnings = [Error_types.Enrichment_skipped 
                    "Instagram does not support threads - only the first post was published"];
                })
              else
                on_result (Error_types.Success thread_result)
          | Error_types.Partial_success { result = post_id; warnings } ->
              let thread_result = {
                Error_types.posted_ids = [post_id];
                failed_at_index = None;
                total_requested = total_posts;
              } in
              let all_warnings = 
                if total_posts > 1 then
                  Error_types.Enrichment_skipped 
                    "Instagram does not support threads - only the first post was published" :: warnings
                else warnings
              in
              on_result (Error_types.Partial_success { result = thread_result; warnings = all_warnings })
          | Error_types.Failure err ->
              on_result (Error_types.Failure err))
  
  (** OAuth authorization URL *)
  let get_oauth_url ~redirect_uri ~state on_success on_error =
    let client_id = Config.get_env "FACEBOOK_APP_ID" |> Option.value ~default:"" in
    
    if client_id = "" then
      on_error "Facebook App ID not configured"
    else
      on_success
        (OAuth.get_authorization_url
           ~client_id
           ~redirect_uri
           ~state
           ~scopes:OAuth.Scopes.write
           ())
  
  (** Exchange OAuth code for a long-lived access token (~60 days).

      Internally performs two steps: exchanges the code for a short-lived token,
      then exchanges that for a long-lived token via [exchange_for_long_lived_token].
      Callers do not need to perform a separate long-lived token exchange. *)
  let rec exchange_code ~code ~redirect_uri on_success on_error =
    let client_id = Config.get_env "FACEBOOK_APP_ID" |> Option.value ~default:"" in
    let client_secret = Config.get_env "FACEBOOK_APP_SECRET" |> Option.value ~default:"" in
    
    if client_id = "" || client_secret = "" then
      on_error "Facebook OAuth credentials not configured"
    else
      OAuth_http.exchange_code ~client_id ~client_secret ~redirect_uri ~code
        (function
          | Ok short_lived_creds ->
              (* Immediately exchange for long-lived token (60 days) *)
              exchange_for_long_lived_token ~short_lived_token:short_lived_creds.access_token
                on_success on_error
          | Error err ->
              on_error (Error_types.error_to_string err))
  
  (** Exchange short-lived token for long-lived token (60 days)

      Uses the fb_exchange_token grant type on the Facebook Graph API endpoint.
      This is for Instagram Graph API tokens obtained via Facebook Login.
  *)
  and exchange_for_long_lived_token ~short_lived_token on_success on_error =
    let client_id = Config.get_env "FACEBOOK_APP_ID" |> Option.value ~default:"" in
    let client_secret = Config.get_env "FACEBOOK_APP_SECRET" |> Option.value ~default:"" in

    if client_id = "" || client_secret = "" then
      on_error "Facebook OAuth credentials not configured"
    else
      OAuth_http.exchange_for_long_lived_token ~client_id ~client_secret ~short_lived_token
        (function
          | Ok credentials ->
              let normalized = { credentials with token_type = "Bearer" } in
              on_success normalized
          | Error err -> on_error (Error_types.error_to_string err))
  
  (** Validate content length and hashtags *)
  let validate_content ~text =
    let len = String.length text in
    if len > 2200 then
      Error (Printf.sprintf "Instagram captions must be 2,200 characters or less (current: %d)" len)
    else
      (* Count hashtags *)
      let hashtag_count = 
        let rec count_hashtags str pos acc =
          try
            let idx = String.index_from str pos '#' in
            count_hashtags str (idx + 1) (acc + 1)
          with Not_found -> acc
        in
        count_hashtags text 0 0
      in
      if hashtag_count > 30 then
        Error (Printf.sprintf "Instagram allows maximum 30 hashtags (current: %d)" hashtag_count)
      else
        Ok ()
  
  (** Validate carousel post *)
  let validate_carousel ~media_urls =
    let count = List.length media_urls in
    if count < 2 then
      Error "Instagram carousel posts require at least 2 media items"
    else if count > 10 then
      Error (Printf.sprintf "Instagram carousel posts allow maximum 10 items (current: %d)" count)
    else
      Ok ()
  
  (** Validate video URL *)
  let validate_video ~video_url ~media_type =
    let url_lower = String.lowercase_ascii video_url in
    (* Check if URL has video extension *)
    if not (Str.string_match (Str.regexp ".*\\.\\(mp4\\|mov\\)$") url_lower 0) then
      Error "Instagram videos must be MP4 or MOV format"
    else
      match media_type with
      | "REELS" -> Ok () (* Reels: 3-90 seconds, validated by Instagram *)
      | "VIDEO" -> Ok () (* Feed videos: 3-60 seconds, validated by Instagram *)
      | _ -> Error "Invalid video media type"
  
  (** Validate media URLs for carousel *)
  let validate_carousel_items ~media_urls =
    (* All items must be accessible URLs *)
    let all_valid = List.for_all (fun url ->
      String.length url > 0 && 
      (String.starts_with ~prefix:"http://" url || String.starts_with ~prefix:"https://" url)
    ) media_urls in
    
    if not all_valid then
      Error "All carousel media items must be publicly accessible HTTP(S) URLs"
    else
      Ok ()
end
