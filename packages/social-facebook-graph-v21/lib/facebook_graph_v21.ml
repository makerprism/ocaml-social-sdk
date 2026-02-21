(** Facebook Graph API v21 Provider
    
    This implementation supports Facebook Pages posting via Graph API.
    Page access tokens are long-lived (60 days) and require re-authentication to refresh.
*)

open Social_core

(** OAuth 2.0 module for Facebook
    
    Facebook uses OAuth 2.0 WITHOUT PKCE support.
    
    Token types:
    - Short-lived user tokens: ~1-2 hours (returned by code exchange)
    - Long-lived user tokens: ~60 days (obtained by exchanging short-lived tokens)
    - Page access tokens: Derived from user tokens, can be made permanent
    
    IMPORTANT: For posting to Facebook Pages, you need:
    1. User authenticates with pages_manage_posts permission
    2. Exchange short-lived token for long-lived token
    3. Get Page access token via /me/accounts endpoint
    
    Required environment variables (or pass directly to functions):
    - FACEBOOK_APP_ID: App ID from Facebook Developer Portal
    - FACEBOOK_APP_SECRET: App Secret
    - FACEBOOK_REDIRECT_URI: Registered callback URL
*)
module OAuth = struct
  (** Scope definitions for Facebook Graph API *)
  module Scopes = struct
    (** Scopes required for basic read operations *)
    let read = ["public_profile"; "email"]
    
    (** Scopes required for Facebook Page posting *)
    let write = [
      "pages_read_engagement";
      "pages_manage_posts";
      "pages_show_list";
    ]
    
    (** All commonly used scopes for Pages management *)
    let all = [
      "public_profile"; "email";
      "pages_read_engagement"; "pages_manage_posts";
      "pages_show_list"; "pages_read_user_content";
      "pages_manage_metadata"; "pages_manage_engagement";
    ]
    
    (** Operations that can be performed with Facebook API *)
    type operation = 
      | Post_text
      | Post_media
      | Post_video
      | Post_story
      | Read_profile
      | Read_posts
      | Delete_post
      | Manage_pages
    
    (** Get scopes required for specific operations *)
    let for_operations ops =
      let base = ["public_profile"] in
      if List.exists (fun o -> o = Post_text || o = Post_media || o = Post_video || o = Post_story || o = Delete_post || o = Manage_pages) ops
      then base @ write
      else if List.exists (fun o -> o = Read_profile || o = Read_posts) ops
      then base @ ["pages_read_engagement"; "pages_show_list"]
      else base
  end
  
  (** Platform metadata for Facebook OAuth *)
  module Metadata = struct
    (** Facebook does NOT support PKCE *)
    let supports_pkce = false
    
    (** Facebook doesn't use traditional refresh tokens - use long-lived token exchange *)
    let supports_refresh = false
    
    (** Short-lived tokens last ~1-2 hours *)
    let short_lived_token_seconds = Some 3600
    
    (** Long-lived tokens last ~60 days *)
    let long_lived_token_seconds = Some 5184000
    
    (** Recommended buffer before expiry (7 days) *)
    let refresh_buffer_seconds = 604800
    
    (** Maximum retry attempts *)
    let max_refresh_attempts = 5
    
    (** Authorization endpoint *)
    let authorization_endpoint = "https://www.facebook.com/v21.0/dialog/oauth"
    
    (** Token endpoint *)
    let token_endpoint = "https://graph.facebook.com/v21.0/oauth/access_token"
    
    (** Graph API base URL *)
    let api_base = "https://graph.facebook.com/v21.0"
  end
  
  (** Generate authorization URL for Facebook OAuth 2.0 flow
      
      Note: Facebook does NOT support PKCE.
      
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
        
        Note: The returned token is SHORT-LIVED (~1-2 hours).
        Call exchange_for_long_lived_token to get a 60-day token.
        
        @param client_id Facebook App ID
        @param client_secret Facebook App Secret
        @param redirect_uri Registered callback URL
        @param code Authorization code from callback
        @param on_success Continuation receiving credentials
        @param on_error Continuation receiving error message
    *)
    let exchange_code ~client_id ~client_secret ~redirect_uri ~code on_success on_error =
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
              let open Yojson.Basic.Util in
              let access_token = json |> member "access_token" |> to_string in
              let expires_in = 
                try json |> member "expires_in" |> to_int
                with _ -> 3600 (* Default to 1 hour if not provided *)
              in
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
                refresh_token = None;  (* Facebook doesn't use refresh tokens *)
                expires_at;
                token_type;
              } in
              on_success creds
            with e ->
              on_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "Token exchange failed (%d): %s" response.status response.body))
        on_error
    
    (** Exchange short-lived token for long-lived token (60 days)
        
        IMPORTANT: Always call this after exchange_code to get a usable token.
        
        @param client_id Facebook App ID
        @param client_secret Facebook App Secret
        @param short_lived_token The short-lived token from exchange_code
        @param on_success Continuation receiving long-lived credentials
        @param on_error Continuation receiving error message
    *)
    let exchange_for_long_lived_token ~client_id ~client_secret ~short_lived_token on_success on_error =
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
              let open Yojson.Basic.Util in
              let access_token = json |> member "access_token" |> to_string in
              let expires_in = 
                try json |> member "expires_in" |> to_int
                with _ -> 5184000 (* Default to 60 days *)
              in
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
                refresh_token = None;
                expires_at;
                token_type;
              } in
              on_success creds
            with e ->
              on_error (Printf.sprintf "Failed to parse long-lived token response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "Long-lived token exchange failed (%d): %s" response.status response.body))
        on_error
    
    (** Page token information *)
    type page_info = {
      page_id: string;
      page_name: string;
      page_access_token: string;
      page_category: string option;
    }
    
    (** Get pages the user manages with their Page access tokens
        
        @param user_access_token Long-lived user access token
        @param on_success Continuation receiving list of page info
        @param on_error Continuation receiving error message
    *)
    let get_user_pages ~user_access_token on_success on_error =
      let url = Printf.sprintf "%s/me/accounts?fields=id,name,access_token,category&access_token=%s"
        Metadata.api_base user_access_token in
      
      Http.get url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let pages_data = json |> member "data" |> to_list in
              let pages = List.map (fun page ->
                {
                  page_id = page |> member "id" |> to_string;
                  page_name = page |> member "name" |> to_string;
                  page_access_token = page |> member "access_token" |> to_string;
                  page_category = page |> member "category" |> to_string_option;
                }
              ) pages_data in
              on_success pages
            with e ->
              on_error (Printf.sprintf "Failed to parse pages response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "Get pages failed (%d): %s" response.status response.body))
        on_error
    
    (** Debug/inspect a token to check its validity and permissions
        
        @param access_token The token to inspect
        @param app_token App access token (client_id|client_secret)
        @param on_success Continuation receiving token info as JSON
        @param on_error Continuation receiving error message
    *)
    let debug_token ~access_token ~app_token on_success on_error =
      let url = Printf.sprintf "%s/debug_token?input_token=%s&access_token=%s"
        Metadata.api_base access_token app_token in
      
      Http.get url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              on_success json
            with e ->
              on_error (Printf.sprintf "Failed to parse debug response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "Debug token failed (%d): %s" response.status response.body))
        on_error
  end
end

(** {1 Error Types} *)

(** Facebook error codes *)
type facebook_error_code = 
  | Invalid_token (* 190 - Token expired/invalid *)
  | Rate_limit_exceeded (* 4, 17, 32, 613 - Rate limited *)
  | Permission_denied (* 200, 299, 10 - Permission issues *)
  | Invalid_parameter (* 100 - Invalid API parameter *)
  | Temporarily_unavailable (* 2, 368 - Temporary failure *)
  | Duplicate_post (* 506 - Duplicate content *)
  | Unknown of int

(** Structured Facebook API error *)
type facebook_error = {
  message : string;
  error_type : string;
  code : facebook_error_code;
  subcode : int option;
  fbtrace_id : string option;
  should_retry : bool;
  retry_after_seconds : int option;
}

(** {1 Rate Limiting Types} *)

(** Rate limit usage information from X-App-Usage header *)
type rate_limit_info = {
  call_count : int;
  total_cputime : int;
  total_time : int;
  percentage_used : float;
}

(** {1 Pagination Types} *)

(** Pagination cursors *)
type paging_cursors = {
  before : string option;
  after : string option;
}

(** Paginated response *)
type 'a page_result = {
  data : 'a list;
  paging : paging_cursors option;
  next_url : string option;
  previous_url : string option;
}

(** Configuration module type for Facebook provider *)
module type CONFIG = sig
  module Http : HTTP_CLIENT
  
  val get_env : string -> string option
  val get_credentials : account_id:string -> (credentials -> unit) -> (string -> unit) -> unit
  val update_credentials : account_id:string -> credentials:credentials -> (unit -> unit) -> (string -> unit) -> unit
  val encrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val decrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val update_health_status : account_id:string -> status:string -> error_message:string option -> (unit -> unit) -> (string -> unit) -> unit
  val get_page_id : account_id:string -> (string -> unit) -> (string -> unit) -> unit
  
  (** Optional: Called when rate limit info is updated *)
  val on_rate_limit_update : rate_limit_info -> unit
end

(** Make functor to create Facebook provider with given configuration *)
module Make (Config : CONFIG) = struct
  let graph_api_base = "https://graph.facebook.com/v21.0"
  module OAuth_http = OAuth.Make(Config.Http)
  
  (** {1 Platform Constants} *)
  
  let max_post_length = 63206  (* Facebook's actual limit *)
  let recommended_post_length = 5000  (* Recommended for engagement *)
  let max_photos_per_post = 10
  
  (** {1 Validation Functions} *)
  
  (** Validate a single post's content *)
  let validate_post ~text ?(media_count=0) () =
    let errors = ref [] in
    
    (* Check text length *)
    let text_len = String.length text in
    if text_len > max_post_length then
      errors := Error_types.Text_too_long { length = text_len; max = max_post_length } :: !errors;
    
    (* Check media count *)
    if media_count > max_photos_per_post then
      errors := Error_types.Too_many_media { count = media_count; max = max_photos_per_post } :: !errors;
    
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
  let validate_media_urls ~media_urls =
    let errors = ref [] in
    List.iter (fun url ->
      if not (String.starts_with ~prefix:"http://" url || String.starts_with ~prefix:"https://" url) then
        errors := Error_types.Invalid_url url :: !errors
    ) media_urls;
    if !errors = [] then Ok ()
    else Error (List.rev !errors)
  
  (** Validate media file for Facebook
      
      Facebook limits:
      - Images: 4MB for regular posts
      - Videos: ~1GB for Reels, 10GB for regular video
      - Reels: 90 seconds max duration
  *)
  let validate_media_file ~(media : Platform_types.post_media) =
    match media.Platform_types.media_type with
    | Platform_types.Image ->
        let max_image_bytes = 4 * 1024 * 1024 in (* 4MB *)
        if media.file_size_bytes > max_image_bytes then
          Error [Error_types.Media_too_large { 
            size_bytes = media.file_size_bytes; 
            max_bytes = max_image_bytes 
          }]
        else
          Ok ()
    | Platform_types.Video ->
        let max_video_bytes = 1024 * 1024 * 1024 in (* 1GB for Reels *)
        if media.file_size_bytes > max_video_bytes then
          Error [Error_types.Media_too_large { 
            size_bytes = media.file_size_bytes; 
            max_bytes = max_video_bytes 
          }]
        else
          Ok ()
    | Platform_types.Gif ->
        let max_gif_bytes = 8 * 1024 * 1024 in (* 8MB *)
        if media.file_size_bytes > max_gif_bytes then
          Error [Error_types.Media_too_large { 
            size_bytes = media.file_size_bytes; 
            max_bytes = max_gif_bytes 
          }]
        else
          Ok ()
  
  (* Keep old name for backward compatibility *)
  let validate_media = validate_media_urls
  
  (** {1 Error Handling} *)
  
  (** Parse Facebook error code into typed variant *)
  let parse_error_code code =
    match code with
    | 190 -> Invalid_token
    | 4 | 17 | 32 | 613 | 80000 | 80001 | 80002 | 80003 | 80004 | 80005 | 80006 | 80008 | 80009 | 80014 -> Rate_limit_exceeded
    | 200 | 299 | 10 -> Permission_denied
    | 100 -> Invalid_parameter
    | 2 | 368 -> Temporarily_unavailable
    | 506 -> Duplicate_post
    | n -> Unknown n
  
  (** Determine if error is retryable *)
  let is_retryable = function
    | Rate_limit_exceeded -> true
    | Temporarily_unavailable -> true
    | _ -> false
  
  (** Get recommended retry delay in seconds *)
  let get_retry_delay = function
    | Rate_limit_exceeded -> Some 300 (* 5 minutes *)
    | Temporarily_unavailable -> Some 60 (* 1 minute *)
    | _ -> None
  
  (** Parse Facebook API error from response *)
  let parse_facebook_error response_body =
    try
      let json = Yojson.Basic.from_string response_body in
      let open Yojson.Basic.Util in
      let error_obj = json |> member "error" in
      let message = error_obj |> member "message" |> to_string_option |> Option.value ~default:"Unknown error" in
      let error_type = error_obj |> member "type" |> to_string_option |> Option.value ~default:"UnknownError" in
      let code_int = error_obj |> member "code" |> to_int_option |> Option.value ~default:0 in
      let code = parse_error_code code_int in
      let subcode = error_obj |> member "error_subcode" |> to_int_option in
      let fbtrace_id = error_obj |> member "fbtrace_id" |> to_string_option in
      Some {
        message;
        error_type;
        code;
        subcode;
        fbtrace_id;
        should_retry = is_retryable code;
        retry_after_seconds = get_retry_delay code;
      }
    with _ -> None
  
  (** Parse API error response and return structured Error_types.error *)
  let contains_substring s sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false

  (* Note: publish_video is NOT a real Facebook permission. Video and Reel
     publishing on Pages requires only pages_manage_posts with a Page access
     token. See: https://developers.facebook.com/docs/video-api/guides/publishing *)
  let provider_required_permissions = [
    "pages_read_engagement";
    "pages_manage_posts";
    "pages_show_list";
  ]

  let permissions_for_path path =
    let p = String.lowercase_ascii path in
    if contains_substring p "me/accounts" then
      ["pages_show_list"]
    else if contains_substring p "video_reels" || contains_substring p "video_stories"
         || contains_substring p "/videos" then
      ["pages_manage_posts"]
    else if contains_substring p "photo_stories" || contains_substring p "photos" || contains_substring p "feed" then
      ["pages_manage_posts"]
    else
      provider_required_permissions

  let parse_api_error_with_permissions ~required_permissions ~status_code ~response_body =
    match parse_facebook_error response_body with
    | Some fb_err ->
        (match fb_err.code with
        | Invalid_token ->
            let auth_error =
              match fb_err.subcode with
              | Some 463 | Some 467 -> Error_types.Token_expired
              | _ -> Error_types.Token_invalid
            in
            Error_types.Auth_error auth_error
        | Rate_limit_exceeded ->
            Error_types.Rate_limited { 
              retry_after_seconds = fb_err.retry_after_seconds;
              limit = None;
              remaining = Some 0;
              reset_at = None;
            }
        | Permission_denied -> 
            Error_types.Auth_error (Error_types.Insufficient_permissions required_permissions)
        | Duplicate_post ->
            Error_types.Duplicate_content
        | _ ->
            Error_types.Api_error {
              status_code;
              message = fb_err.message;
              platform = Platform_types.FacebookPage;
              raw_response = Some response_body;
              request_id = fb_err.fbtrace_id;
            })
    | None ->
        Error_types.Api_error {
          status_code;
          message = response_body;
          platform = Platform_types.FacebookPage;
          raw_response = Some response_body;
          request_id = None;
        }

  let parse_api_error ~status_code ~response_body =
    parse_api_error_with_permissions
      ~required_permissions:provider_required_permissions
      ~status_code
      ~response_body

  let contains_substring haystack needle =
    let h_len = String.length haystack in
    let n_len = String.length needle in
    if n_len = 0 then true
    else if n_len > h_len then false
    else
      let rec loop i =
        if i + n_len > h_len then false
        else if String.sub haystack i n_len = needle then true
        else loop (i + 1)
      in
      loop 0

  let redact_sensitive_text_if_needed text =
    let lower = String.lowercase_ascii text in
    if List.exists (contains_substring lower)
         [ "access_token=";
           "access_token";
           "client_secret";
           "authorization" ]
    then "[REDACTED_SENSITIVE_TEXT]"
    else text
  
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
          (* Facebook uses percentage-based limits *)
          let percentage_used = float_of_int (max call_count (max total_cputime total_time)) in
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
  
  (** {1 Pagination} *)
  
  (** Parse pagination info from response *)
  let parse_paging json =
    try
      let open Yojson.Basic.Util in
      let paging_obj = json |> member "paging" in
      let cursors = paging_obj |> member "cursors" in
      let before = cursors |> member "before" |> to_string_option in
      let after = cursors |> member "after" |> to_string_option in
      let next_url = paging_obj |> member "next" |> to_string_option in
      let previous_url = paging_obj |> member "previous" |> to_string_option in
      Some {
        before;
        after;
      }, next_url, previous_url
    with _ -> None, None, None
  
  (** Generic GET request with pagination support *)
  let get_paginated ~path ~access_token ?fields ?cursor on_result =
    let field_params = match fields with
      | Some f -> [("fields", String.concat "," f)]
      | None -> []
    in
    let cursor_params = match cursor with
      | Some c -> [("after", c)]
      | None -> []
    in
    let params = field_params @ cursor_params in
    
    let url = 
      if List.length params > 0 then
        let query = Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) params) in
        Printf.sprintf "%s/%s?%s" graph_api_base path query
      else
        Printf.sprintf "%s/%s" graph_api_base path
    in
    
    let proof = compute_app_secret_proof ~access_token in
    let auth_params = match proof with
      | Some p -> [("appsecret_proof", [p])]
      | None -> []
    in
    
    let final_url = 
      if List.length auth_params > 0 then
        let existing_query = if String.contains url '?' then "&" else "?" in
        url ^ existing_query ^ Uri.encoded_of_query auth_params
      else url
    in
    
    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
    ] in
    
    Config.Http.get ~headers final_url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          on_result (Ok response)
        else
          on_result (Error (parse_api_error_with_permissions
            ~required_permissions:(permissions_for_path path)
            ~status_code:response.status
            ~response_body:response.body)))
      (fun err ->
        on_result
          (Error (Error_types.Internal_error (redact_sensitive_text_if_needed err))))
  
  (** {1 Token Management} *)
  
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
  
  (** Ensure valid access token *)
  let ensure_valid_token ~account_id on_success on_error =
    Config.get_credentials ~account_id
      (fun creds ->
        (* Check if token needs refresh (24 hour buffer) *)
        if is_token_expired_buffer ~buffer_seconds:86400 creds.expires_at then
          (* Token expiring soon - Facebook requires re-authentication *)
          Config.update_health_status ~account_id ~status:"token_expired" 
            ~error_message:(Some "Access token expired - please reconnect")
            (fun () -> on_error (Error_types.Auth_error Error_types.Token_expired))
            (fun _ -> on_error (Error_types.Auth_error Error_types.Token_expired))
        else
          (* Token still valid *)
          Config.update_health_status ~account_id ~status:"healthy" ~error_message:None
            (fun () -> on_success creds.access_token)
            (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err))))
      (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err)))

  let is_recoverable_posting_auth_error = function
    | Error_types.Auth_error (Error_types.Insufficient_permissions _ | Error_types.Token_invalid | Error_types.Token_expired) -> true
    | _ -> false

  let should_try_next_recovery_token = function
    | Error_types.Auth_error (Error_types.Insufficient_permissions _ | Error_types.Token_invalid | Error_types.Token_expired) -> true
    | _ -> false

  let recover_page_access_token ~account_id ~current_access_token on_success on_error =
    Config.get_page_id ~account_id
      (fun page_id ->
        let report_recovery_status status message cont =
          Config.update_health_status ~account_id ~status ~error_message:(Some message)
            cont
            (fun _ -> cont ())
        in
        let dedupe_non_empty tokens =
          let rec loop seen = function
            | [] -> List.rev seen
            | t :: rest when t = "" || List.mem t seen -> loop seen rest
            | t :: rest -> loop (t :: seen) rest
          in
          loop [] tokens
        in
        let fetch_pages_with_user_token ~user_token on_success_fetch on_error_fetch =
          let params = [
            ("fields", ["id,access_token"]);
            ("access_token", [user_token]);
          ] @
          (match compute_app_secret_proof ~access_token:user_token with
           | Some proof -> [("appsecret_proof", [proof])]
           | None -> []) in
          let url = Printf.sprintf "%s/me/accounts?%s" graph_api_base (Uri.encoded_of_query params) in
          Config.Http.get url
            (fun response ->
              if response.status >= 200 && response.status < 300 then
                try
                  let json = Yojson.Basic.from_string response.body in
                  let open Yojson.Basic.Util in
                  let pages = json |> member "data" |> to_list in
                  let maybe_page =
                    List.find_opt (fun page ->
                      try page |> member "id" |> to_string = page_id
                      with _ -> false
                    ) pages
                  in
                  match maybe_page with
                  | Some page ->
                      let page_access_token = page |> member "access_token" |> to_string in
                      if page_access_token = "" then
                        on_error_fetch (Error_types.Auth_error (Error_types.Insufficient_permissions ["pages_show_list"; "pages_manage_posts"]))
                      else
                        on_success_fetch (user_token, page_access_token)
                  | None ->
                      on_error_fetch (Error_types.Auth_error (Error_types.Insufficient_permissions ["pages_show_list"; "pages_manage_posts"]))
                with _ ->
                  on_error_fetch (Error_types.Auth_error Error_types.Token_invalid)
              else
                on_error_fetch (parse_api_error_with_permissions
                  ~required_permissions:["pages_show_list"]
                  ~status_code:response.status
                  ~response_body:response.body))
            (fun _ -> on_error_fetch (Error_types.Auth_error Error_types.Token_invalid))
        in
        let rec try_candidate_tokens ~stored_creds ~last_error = function
          | [] ->
              let final_error =
                match last_error with
                | Some e -> e
                | None -> Error_types.Auth_error Error_types.Token_invalid
              in
              report_recovery_status "token_recovery_failed"
                "Failed to recover Page token from /me/accounts for configured page_id"
                (fun () -> on_error final_error)
          | candidate_user_token :: rest ->
              fetch_pages_with_user_token ~user_token:candidate_user_token
                (fun (user_token_used, page_access_token) ->
                  let complete_success () =
                    report_recovery_status "token_recovered"
                      "Recovered Page access token from /me/accounts after posting auth failure"
                      (fun () -> on_success page_id page_access_token)
                  in
                  match stored_creds with
                  | Some creds ->
                      let preserved_user_token =
                        if user_token_used = page_access_token then creds.refresh_token
                        else Some user_token_used
                      in
                      let updated_credentials = {
                        creds with
                        access_token = page_access_token;
                        refresh_token = preserved_user_token;
                        token_type = "Bearer";
                      } in
                      Config.update_credentials ~account_id ~credentials:updated_credentials
                        complete_success
                        (fun _ -> complete_success ())
                  | None ->
                      complete_success ())
                (fun err ->
                  if should_try_next_recovery_token err then
                    try_candidate_tokens ~stored_creds ~last_error:(Some err) rest
                  else
                    let stop_reason =
                      "Stopped token recovery due to non-auth failure: " ^
                      Error_types.error_to_string err
                    in
                    report_recovery_status "token_recovery_failed"
                      stop_reason
                      (fun () -> on_error err))
        in
        Config.get_credentials ~account_id
          (fun creds ->
            let candidate_user_tokens =
              dedupe_non_empty (current_access_token :: (match creds.refresh_token with Some t -> [t] | None -> []))
            in
            try_candidate_tokens ~stored_creds:(Some creds) ~last_error:None candidate_user_tokens)
          (fun _ ->
            let candidate_user_tokens = dedupe_non_empty [current_access_token] in
            try_candidate_tokens ~stored_creds:None ~last_error:None candidate_user_tokens))
      (fun err -> on_error (Error_types.Internal_error err))

  let with_posting_page_context ~account_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        Config.get_page_id ~account_id
          (fun page_id -> on_success page_id access_token)
          (fun err -> on_error (Error_types.Internal_error err)))
      on_error
  
  (** Upload photo to Facebook Page with optional alt text *)
  let upload_photo ~page_id ~page_access_token ~image_url ~alt_text ?(validate_before_upload=false) on_result =
    (* Download image first *)
    Config.Http.get ~headers:[] image_url
      (fun image_response ->
        if image_response.status >= 200 && image_response.status < 300 then
          let file_size = String.length image_response.body in
          
          (* Validate if requested *)
          let validation_result =
            if validate_before_upload then
              let media : Platform_types.post_media = {
                media_type = Platform_types.Image;
                mime_type = List.assoc_opt "content-type" image_response.headers 
                            |> Option.value ~default:"image/jpeg";
                file_size_bytes = file_size;
                width = None;
                height = None;
                duration_seconds = None;
                alt_text = alt_text;
              } in
              validate_media_file ~media
            else
              Ok ()
          in
          
          (match validation_result with
          | Error errs -> on_result (Error (Error_types.Validation_error errs))
          | Ok () ->
              let url = Printf.sprintf "%s/%s/photos" graph_api_base page_id in
              
              (* Add app secret proof if available *)
              let proof_params = match compute_app_secret_proof ~access_token:page_access_token with
                | Some proof -> [("appsecret_proof", [proof])]
                | None -> []
              in
              
              let final_url = 
                if List.length proof_params > 0 then
                  url ^ "?" ^ Uri.encoded_of_query proof_params
                else url
              in
              
              (* Create multipart form data *)
              let base_parts = [
                {
                  name = "source";
                  filename = Some "image.jpg";
                  content_type = Some "image/jpeg";
                  content = image_response.body;
                };
                {
                  name = "published";
                  filename = None;
                  content_type = None;
                  content = "false";  (* Upload unpublished, attach to post later *)
                };
              ] in
              
              (* Add alt text if provided *)
              let parts = match alt_text with
                | Some alt when String.length alt > 0 ->
                    base_parts @ [{
                      name = "alt_text_custom";
                      filename = None;
                      content_type = None;
                      content = alt;
                    }]
                | _ -> base_parts
              in
              
              let headers = [
                ("Authorization", Printf.sprintf "Bearer %s" page_access_token);
              ] in
              
              Config.Http.post_multipart ~headers ~parts final_url
                (fun response ->
                  update_rate_limits response;
                  if response.status >= 200 && response.status < 300 then
                    try
                      let open Yojson.Basic.Util in
                      let json = Yojson.Basic.from_string response.body in
                      let photo_id = json |> member "id" |> to_string in
                      on_result (Ok photo_id)
                    with e ->
                      on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse photo response: %s" (Printexc.to_string e))))
                  else
                    on_result (Error (parse_api_error_with_permissions
                      ~required_permissions:["pages_manage_posts"]
                      ~status_code:response.status
                      ~response_body:response.body)))
                (fun err -> on_result (Error (Error_types.Internal_error err))))
        else
          on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to download image from %s (%d)" image_url image_response.status))))
      (fun err ->
        on_result
          (Error (Error_types.Internal_error (redact_sensitive_text_if_needed err))))
  
  (** Upload video to Facebook Page for Reels 
      
      Facebook Reels use a two-step process:
      1. Initialize upload to get video_id
      2. Upload the video file
      3. Create the Reel post
      
      @see <https://developers.facebook.com/docs/video-api/guides/reels-publishing>
  *)
  let upload_video_reel_typed ~page_id ~page_access_token ~video_url ~description on_success on_error =
    (* Download video first *)
    Config.Http.get ~headers:[] video_url
      (fun video_response ->
        if video_response.status >= 200 && video_response.status < 300 then
          let video_content = video_response.body in
          let video_size = String.length video_content in
          
          (* Step 1: Initialize the upload session *)
          let init_url = Printf.sprintf "%s/%s/video_reels" graph_api_base page_id in
          
          let init_params = [
            ("upload_phase", ["start"]);
            ("access_token", [page_access_token]);
          ] @
          (match compute_app_secret_proof ~access_token:page_access_token with
           | Some proof -> [("appsecret_proof", [proof])]
           | None -> [])
          in
          
          let init_body = Uri.encoded_of_query init_params in
          let headers = [
            ("Content-Type", "application/x-www-form-urlencoded");
          ] in
          
          Config.Http.post ~headers ~body:init_body init_url
            (fun init_response ->
              update_rate_limits init_response;
              if init_response.status >= 200 && init_response.status < 300 then
                try
                  let open Yojson.Basic.Util in
                  let json = Yojson.Basic.from_string init_response.body in
                  let video_id = json |> member "video_id" |> to_string in
                  let upload_url = json |> member "upload_url" |> to_string in
                  
                  (* Step 2: Upload video file to the upload_url *)
                  let upload_headers = [
                    ("Authorization", Printf.sprintf "OAuth %s" page_access_token);
                    ("file_size", string_of_int video_size);
                  ] in
                  
                  Config.Http.post ~headers:upload_headers ~body:video_content upload_url
                    (fun upload_response ->
                      if upload_response.status >= 200 && upload_response.status < 300 then
                        (* Step 3: Finish the upload and create the Reel *)
                        let finish_url = Printf.sprintf "%s/%s/video_reels" graph_api_base page_id in
                        
                        let finish_params = [
                          ("upload_phase", ["finish"]);
                          ("video_id", [video_id]);
                          ("video_state", ["PUBLISHED"]);
                          ("description", [description]);
                          ("access_token", [page_access_token]);
                        ] @
                        (match compute_app_secret_proof ~access_token:page_access_token with
                         | Some proof -> [("appsecret_proof", [proof])]
                         | None -> [])
                        in
                        
                        let finish_body = Uri.encoded_of_query finish_params in
                        Config.Http.post ~headers ~body:finish_body finish_url
                          (fun finish_response ->
                            update_rate_limits finish_response;
                            if finish_response.status >= 200 && finish_response.status < 300 then
                              try
                                let open Yojson.Basic.Util in
                                let json = Yojson.Basic.from_string finish_response.body in
                                (* Check for success field *)
                                let success = 
                                  try json |> member "success" |> to_bool 
                                  with _ -> true (* Assume success if field not present *)
                                in
                                if success then
                                  on_success video_id
                                else
                                  on_error (Error_types.Internal_error "Facebook Reel upload failed: success=false")
                              with _e ->
                                (* If we can't parse but got 2xx, consider it a success *)
                                on_success video_id
                            else
                              on_error (parse_api_error_with_permissions
                                ~required_permissions:["pages_manage_posts"]
                                ~status_code:finish_response.status
                                ~response_body:finish_response.body))
                          (fun err -> on_error (Error_types.Internal_error err))
                      else
                        on_error (parse_api_error_with_permissions
                          ~required_permissions:["pages_manage_posts"]
                          ~status_code:upload_response.status
                          ~response_body:upload_response.body))
                    (fun err -> on_error (Error_types.Internal_error err))
                with e ->
                  on_error (Error_types.Internal_error (Printf.sprintf "Failed to parse init response: %s" (Printexc.to_string e)))
              else
                on_error (parse_api_error_with_permissions
                  ~required_permissions:["pages_manage_posts"]
                  ~status_code:init_response.status
                  ~response_body:init_response.body))
            (fun err -> on_error (Error_types.Internal_error err))
        else
          on_error (Error_types.Internal_error (Printf.sprintf "Failed to download video from %s (%d)" video_url video_response.status)))
      (fun err -> on_error (Error_types.Internal_error err))

  let upload_video_reel ~page_id ~page_access_token ~video_url ~description on_success on_error =
    upload_video_reel_typed ~page_id ~page_access_token ~video_url ~description
      on_success
      (fun err -> on_error (Error_types.error_to_string err))
  
  (** Post Reel (short-form video) to Facebook Page *)
  let post_reel ~account_id ~text ~video_url on_result =
    (* Validate caption *)
    match validate_post ~text ~media_count:1 () with
    | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        with_posting_page_context ~account_id
          (fun page_id page_access_token ->
            let rec attempt_post ~can_recover token =
              upload_video_reel_typed ~page_id ~page_access_token:token ~video_url ~description:text
                (fun video_id -> on_result (Error_types.Success video_id))
                (fun parsed ->
                  if can_recover && is_recoverable_posting_auth_error parsed then
                    recover_page_access_token ~account_id ~current_access_token:token
                      (fun _resolved_page_id resolved_page_token ->
                        attempt_post ~can_recover:false resolved_page_token)
                      (fun _ -> on_result (Error_types.Failure parsed))
                  else
                    on_result (Error_types.Failure parsed))
            in
            attempt_post ~can_recover:true page_access_token)
          (fun err -> on_result (Error_types.Failure err))
  
  (** {1 Facebook Page Stories} *)
  
  (** Post photo story to Facebook Page
      
      Facebook Page Stories are full-screen vertical content that expires after 24 hours.
      This function posts a photo as a Page story.
      
      Requirements:
      - Page access token with pages_manage_posts permission
      - Image must be publicly accessible via HTTPS
      - Recommended: 9:16 aspect ratio (1080x1920 pixels)
      
      @see <https://developers.facebook.com/docs/graph-api/reference/page/photo_stories/>
      
      @param page_id Facebook Page ID
      @param page_access_token Page access token
      @param image_url Publicly accessible URL of the image
      @param on_result Continuation receiving api_result with story ID
  *)
  let upload_photo_story ~page_id ~page_access_token ~image_url on_result =
    (* Download image first *)
    Config.Http.get ~headers:[] image_url
      (fun image_response ->
        if image_response.status >= 200 && image_response.status < 300 then
          let url = Printf.sprintf "%s/%s/photo_stories" graph_api_base page_id in
          
          (* Add app secret proof if available *)
          let proof_params = match compute_app_secret_proof ~access_token:page_access_token with
            | Some proof -> [("appsecret_proof", [proof])]
            | None -> []
          in
          
          let final_url = 
            if List.length proof_params > 0 then
              url ^ "?" ^ Uri.encoded_of_query proof_params
            else url
          in
          
          (* Create multipart form data with photo *)
          let parts = [
            {
              name = "photo";
              filename = Some "story.jpg";
              content_type = Some "image/jpeg";
              content = image_response.body;
            };
          ] in
          
          let headers = [
            ("Authorization", Printf.sprintf "Bearer %s" page_access_token);
          ] in
          
          Config.Http.post_multipart ~headers ~parts final_url
            (fun response ->
              update_rate_limits response;
              if response.status >= 200 && response.status < 300 then
                try
                  let open Yojson.Basic.Util in
                  let json = Yojson.Basic.from_string response.body in
                  (* Response contains post_id for the story *)
                  let story_id = 
                    try json |> member "post_id" |> to_string
                    with _ -> json |> member "id" |> to_string
                  in
                  on_result (Ok story_id)
                with e ->
                  on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse photo story response: %s" (Printexc.to_string e))))
              else
                on_result (Error (parse_api_error_with_permissions
                  ~required_permissions:["pages_manage_posts"]
                  ~status_code:response.status
                  ~response_body:response.body)))
            (fun err -> on_result (Error (Error_types.Internal_error err)))
        else
          on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to download image from %s (%d)" image_url image_response.status))))
      (fun err ->
        on_result
          (Error (Error_types.Internal_error (redact_sensitive_text_if_needed err))))
  
  (** Post video story to Facebook Page
      
      Facebook Page Stories are full-screen vertical content that expires after 24 hours.
      This function posts a video as a Page story using the resumable upload API.
      
      Requirements:
      - Page access token with pages_manage_posts permission
      - Video must be publicly accessible via HTTPS
      - Duration: 1-120 seconds
      - Recommended: 9:16 aspect ratio (1080x1920 pixels)
      - Supported formats: MP4, MOV
      
      @see <https://developers.facebook.com/docs/graph-api/reference/page/video_stories/>
      
      @param page_id Facebook Page ID
      @param page_access_token Page access token
      @param video_url Publicly accessible URL of the video
      @param on_result Continuation receiving api_result with story ID
  *)
  let upload_video_story ~page_id ~page_access_token ~video_url on_result =
    (* Download video first *)
    Config.Http.get ~headers:[] video_url
      (fun video_response ->
        if video_response.status >= 200 && video_response.status < 300 then
          let video_content = video_response.body in
          let video_size = String.length video_content in
          
          (* Step 1: Initialize the upload session for video story *)
          let init_url = Printf.sprintf "%s/%s/video_stories" graph_api_base page_id in
          
          let init_params = [
            ("upload_phase", ["start"]);
            ("access_token", [page_access_token]);
          ] @
          (match compute_app_secret_proof ~access_token:page_access_token with
           | Some proof -> [("appsecret_proof", [proof])]
           | None -> [])
          in
          
          let init_body = Uri.encoded_of_query init_params in
          let headers = [
            ("Content-Type", "application/x-www-form-urlencoded");
          ] in
          
          Config.Http.post ~headers ~body:init_body init_url
            (fun init_response ->
              update_rate_limits init_response;
              if init_response.status >= 200 && init_response.status < 300 then
                try
                  let open Yojson.Basic.Util in
                  let json = Yojson.Basic.from_string init_response.body in
                  let video_id = json |> member "video_id" |> to_string in
                  let upload_url = json |> member "upload_url" |> to_string in
                  
                  (* Step 2: Upload video file to the upload_url *)
                  let upload_headers = [
                    ("Authorization", Printf.sprintf "OAuth %s" page_access_token);
                    ("file_size", string_of_int video_size);
                  ] in
                  
                  Config.Http.post ~headers:upload_headers ~body:video_content upload_url
                    (fun upload_response ->
                      if upload_response.status >= 200 && upload_response.status < 300 then
                        (* Step 3: Finish the upload and publish the story *)
                        let finish_url = Printf.sprintf "%s/%s/video_stories" graph_api_base page_id in
                        
                        let finish_params = [
                          ("upload_phase", ["finish"]);
                          ("video_id", [video_id]);
                          ("access_token", [page_access_token]);
                        ] @
                        (match compute_app_secret_proof ~access_token:page_access_token with
                         | Some proof -> [("appsecret_proof", [proof])]
                         | None -> [])
                        in
                        
                        let finish_body = Uri.encoded_of_query finish_params in
                        Config.Http.post ~headers ~body:finish_body finish_url
                          (fun finish_response ->
                            update_rate_limits finish_response;
                            if finish_response.status >= 200 && finish_response.status < 300 then
                              try
                                let open Yojson.Basic.Util in
                                let json = Yojson.Basic.from_string finish_response.body in
                                (* Check for success and extract post_id *)
                                let success = 
                                  try json |> member "success" |> to_bool 
                                  with _ -> true
                                in
                                if success then
                                  let story_id =
                                    try json |> member "post_id" |> to_string
                                    with _ -> video_id
                                  in
                                  on_result (Ok story_id)
                                else
                                  on_result (Error (Error_types.Internal_error "Facebook video story upload failed: success=false"))
                              with _e ->
                                (* If we can't parse but got 2xx, consider it a success *)
                                on_result (Ok video_id)
                            else
                              on_result (Error (parse_api_error_with_permissions
                                ~required_permissions:["pages_manage_posts"]
                                ~status_code:finish_response.status
                                ~response_body:finish_response.body)))
                          (fun err -> on_result (Error (Error_types.Internal_error err)))
                      else
                        on_result (Error (parse_api_error_with_permissions
                          ~required_permissions:["pages_manage_posts"]
                          ~status_code:upload_response.status
                          ~response_body:upload_response.body)))
                    (fun err -> on_result (Error (Error_types.Internal_error err)))
                with e ->
                  on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse video story init response: %s" (Printexc.to_string e))))
              else
                on_result (Error (parse_api_error_with_permissions
                  ~required_permissions:["pages_manage_posts"]
                  ~status_code:init_response.status
                  ~response_body:init_response.body)))
            (fun err -> on_result (Error (Error_types.Internal_error err)))
        else
          on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to download video from %s (%d)" video_url video_response.status))))
      (fun err ->
        on_result
          (Error (Error_types.Internal_error (redact_sensitive_text_if_needed err))))
  
  (** Post photo story to Facebook Page (high-level)
      
      @param account_id Internal account identifier
      @param image_url Publicly accessible URL of the image
      @param on_result Continuation receiving outcome with story ID
  *)
  let post_story_photo ~account_id ~image_url on_result =
    with_posting_page_context ~account_id
      (fun page_id page_access_token ->
        let rec attempt_post can_recover token =
          upload_photo_story ~page_id ~page_access_token:token ~image_url
            (function
              | Ok story_id -> on_result (Error_types.Success story_id)
              | Error e when can_recover && is_recoverable_posting_auth_error e ->
                  recover_page_access_token ~account_id ~current_access_token:token
                    (fun _resolved_page_id resolved_page_token ->
                      attempt_post false resolved_page_token)
                    (fun _ -> on_result (Error_types.Failure e))
              | Error e -> on_result (Error_types.Failure e))
        in
        attempt_post true page_access_token)
      (fun err -> on_result (Error_types.Failure err))
  
  (** Post video story to Facebook Page (high-level)
      
      @param account_id Internal account identifier
      @param video_url Publicly accessible URL of the video
      @param on_result Continuation receiving outcome with story ID
  *)
  let post_story_video ~account_id ~video_url on_result =
    with_posting_page_context ~account_id
      (fun page_id page_access_token ->
        let rec attempt_post can_recover token =
          upload_video_story ~page_id ~page_access_token:token ~video_url
            (function
              | Ok story_id -> on_result (Error_types.Success story_id)
              | Error e when can_recover && is_recoverable_posting_auth_error e ->
                  recover_page_access_token ~account_id ~current_access_token:token
                    (fun _resolved_page_id resolved_page_token ->
                      attempt_post false resolved_page_token)
                    (fun _ -> on_result (Error_types.Failure e))
              | Error e -> on_result (Error_types.Failure e))
        in
        attempt_post true page_access_token)
      (fun err -> on_result (Error_types.Failure err))
  
  (** Detect media type from URL extension *)
  let detect_media_type url =
    let url_lower = String.lowercase_ascii url in
    if Str.string_match (Str.regexp ".*\\.\\(mp4\\|mov\\|avi\\)$") url_lower 0 then
      "VIDEO"
    else if Str.string_match (Str.regexp ".*\\.\\(jpg\\|jpeg\\|png\\|gif\\)$") url_lower 0 then
      "IMAGE"
    else
      "IMAGE" (* Default to image *)
  
  (** Post story to Facebook Page (auto-detect media type)
      
      Automatically detects whether the media is an image or video based on
      the file extension and posts it as a Facebook Page Story.
      
      @param account_id Internal account identifier
      @param media_url Publicly accessible URL of the image or video
      @param on_result Continuation receiving outcome with story ID
  *)
  let post_story ~account_id ~media_url on_result =
    let media_type = detect_media_type media_url in
    match media_type with
    | "VIDEO" -> post_story_video ~account_id ~video_url:media_url on_result
    | _ -> post_story_photo ~account_id ~image_url:media_url on_result
  
  (** Validate story media
      
      Validates that the media URL is appropriate for Facebook Page Stories.
      
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
      let is_image = Str.string_match (Str.regexp ".*\\.\\(jpg\\|jpeg\\|png\\|gif\\)$") url_lower 0 in
      let is_video = Str.string_match (Str.regexp ".*\\.\\(mp4\\|mov\\)$") url_lower 0 in
      if not (is_image || is_video) then
        Error "Story media must be an image (JPEG, PNG) or video (MP4, MOV)"
      else
        Ok ()
  
  (** Post to Facebook Page
      
      @param validate_media_before_upload When true, validates media file size after 
             download but before upload. Facebook limits: 4MB images, 1GB video.
             Default: false
  *)
  let post_single ~account_id ~text ~media_urls ?(alt_texts=[]) ?link ?scheduled_publish_time ?(validate_media_before_upload=false) on_result =
    let media_count = List.length media_urls in
    
    (* Validate content first *)
    match validate_post ~text ~media_count () with
    | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        (* Validate media URLs *)
        match validate_media ~media_urls with
        | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
        | Ok () ->
            with_posting_page_context ~account_id
              (fun page_id page_access_token ->
                  let rec attempt_post can_recover token =
                    let cleanup_uploaded_photos photo_ids on_done =
                      let rec loop = function
                        | [] -> on_done ()
                        | photo_id :: rest ->
                            let proof_params =
                              match compute_app_secret_proof ~access_token:token with
                              | Some proof -> [ ("appsecret_proof", [ proof ]) ]
                              | None -> []
                            in
                            let base_url = Printf.sprintf "%s/%s" graph_api_base photo_id in
                            let delete_url =
                              if List.length proof_params > 0 then
                                base_url ^ "?" ^ Uri.encoded_of_query proof_params
                              else
                                base_url
                            in
                            let headers = [ ("Authorization", Printf.sprintf "Bearer %s" token) ] in
                            Config.Http.delete ~headers delete_url
                              (fun _ -> loop rest)
                              (fun _ -> loop rest)
                      in
                      loop photo_ids
                    in
                    (* Upload all photos first *)
                    let rec upload_all_photos urls_with_alt acc on_complete =
                      match urls_with_alt with
                      | [] -> on_complete (Ok (List.rev acc))
                      | (url, alt_text) :: rest ->
                          upload_photo ~page_id ~page_access_token:token ~image_url:url ~alt_text
                            ~validate_before_upload:validate_media_before_upload
                            (function
                              | Ok photo_id -> upload_all_photos rest (photo_id :: acc) on_complete
                              | Error e -> on_complete (Error (e, List.rev acc)))
                    in
                    
                    (* Pair URLs with alt text - use None if alt text list is shorter *)
                    let urls_with_alt = List.mapi (fun i url ->
                      let alt_text = try List.nth alt_texts i with _ -> None in
                      (url, alt_text)
                    ) media_urls in
                    
                    upload_all_photos urls_with_alt [] 
                      (function
                      | Error (e, uploaded_photo_ids) ->
                        cleanup_uploaded_photos uploaded_photo_ids (fun () ->
                          if can_recover && is_recoverable_posting_auth_error e then
                            recover_page_access_token ~account_id ~current_access_token:token
                              (fun _resolved_page_id resolved_page_token ->
                                attempt_post false resolved_page_token)
                              (fun _ -> on_result (Error_types.Failure e))
                          else
                            on_result (Error_types.Failure e))
                      | Ok photo_ids ->
                        (* Create Facebook Page post *)
                        let url = Printf.sprintf "%s/%s/feed" graph_api_base page_id in
                        let attached_media_params =
                          List.mapi (fun i photo_id ->
                            (Printf.sprintf "attached_media[%d]" i, [
                              Yojson.Basic.to_string (`Assoc [("media_fbid", `String photo_id)])
                            ])
                          ) photo_ids
                        in
                        
                        let params =
                          [
                            ("message", [text]);
                          ] @
                          attached_media_params @
                          (* Add link parameter for link posts *)
                          (match link with
                           | Some l -> [("link", [l])]
                           | None -> []) @
                          (* Add scheduled publishing parameters *)
                          (match scheduled_publish_time with
                           | Some t -> [("scheduled_publish_time", [string_of_int t]); ("published", ["false"])]
                           | None -> []) @
                          (* Add app secret proof *)
                          (match compute_app_secret_proof ~access_token:token with
                           | Some proof -> [("appsecret_proof", [proof])]
                           | None -> [])
                        in
                        
                        let body = Uri.encoded_of_query params in
                        let headers = [
                          ("Content-Type", "application/x-www-form-urlencoded");
                          ("Authorization", Printf.sprintf "Bearer %s" token);
                        ] in
                        
                        Config.Http.post ~headers ~body url
                          (fun response ->
                            update_rate_limits response;
                            if response.status >= 200 && response.status < 300 then
                              try
                                let open Yojson.Basic.Util in
                                let json = Yojson.Basic.from_string response.body in
                                let post_id = json |> member "id" |> to_string in
                                on_result (Error_types.Success post_id)
                              with _e ->
                                (* Post succeeded but couldn't parse ID *)
                                on_result (Error_types.Success "unknown")
                            else
                              let parsed_error =
                                parse_api_error_with_permissions
                                  ~required_permissions:["pages_manage_posts"]
                                  ~status_code:response.status
                                  ~response_body:response.body
                              in
                              cleanup_uploaded_photos photo_ids (fun () ->
                                if can_recover && is_recoverable_posting_auth_error parsed_error then
                                  recover_page_access_token ~account_id ~current_access_token:token
                                    (fun _resolved_page_id resolved_page_token ->
                                      attempt_post false resolved_page_token)
                                    (fun _ -> on_result (Error_types.Failure parsed_error))
                                else
                                  on_result (Error_types.Failure parsed_error)))
                          (fun err ->
                            let parsed_error = Error_types.Internal_error err in
                            cleanup_uploaded_photos photo_ids (fun () ->
                              on_result (Error_types.Failure parsed_error))))
                  in
                  attempt_post true page_access_token)
              (fun err -> on_result (Error_types.Failure err))
  
  (** Post thread (Facebook doesn't support threads, posts only first item with warning)
      
      @param validate_media_before_upload When true, validates media file size after 
             download but before upload. Facebook limits: 4MB images, 1GB video.
             Default: false
  *)
  let post_thread ~account_id ~texts ~media_urls_per_post ?(alt_texts_per_post=[]) ?(validate_media_before_upload=false) on_result =
    if List.length texts = 0 then
      on_result (Error_types.Failure (Error_types.Validation_error [Error_types.Thread_empty]))
    else
      let first_text = List.hd texts in
      let first_media = if List.length media_urls_per_post > 0 then List.hd media_urls_per_post else [] in
      let first_alt_texts = if List.length alt_texts_per_post > 0 then List.hd alt_texts_per_post else [] in
      let total_posts = List.length texts in
      
      post_single ~account_id ~text:first_text ~media_urls:first_media ~alt_texts:first_alt_texts
        ~validate_media_before_upload
        (fun outcome ->
          match outcome with
          | Error_types.Success post_id ->
              let thread_result = {
                Error_types.posted_ids = [post_id];
                failed_at_index = None;
                total_requested = total_posts;
              } in
              if total_posts > 1 then
                on_result (Error_types.Partial_success {
                  result = thread_result;
                  warnings = [Error_types.Enrichment_skipped 
                    "Facebook does not support threads - only the first post was published"];
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
                    "Facebook does not support threads - only the first post was published" :: warnings
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
    else (
      (* Facebook OAuth scopes for Pages management *)
      let scopes = [
        "pages_read_engagement";
        "pages_manage_posts";
        "pages_show_list";
        "business_management";
      ] in
      
      let scope_str = String.concat "," scopes in
      let params = [
        ("client_id", client_id);
        ("redirect_uri", redirect_uri);
        ("state", state);
        ("scope", scope_str);
        ("response_type", "code");
        ("auth_type", "rerequest");  (* Force re-authentication for account selection *)
      ] in
      
      let query = Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) params) in
      let url = Printf.sprintf "https://www.facebook.com/v21.0/dialog/oauth?%s" query in
      on_success url
    )
  
  (** Exchange OAuth code for a long-lived access token (~60 days).

      Internally performs two steps: exchanges the code for a short-lived token,
      then exchanges that for a long-lived token. Callers do not need to perform
      a separate long-lived token exchange. *)
  let exchange_code ~code ~redirect_uri on_success on_error =
    let client_id = Config.get_env "FACEBOOK_APP_ID" |> Option.value ~default:"" in
    let client_secret = Config.get_env "FACEBOOK_APP_SECRET" |> Option.value ~default:"" in
    
    if client_id = "" || client_secret = "" then
      on_error "Facebook OAuth credentials not configured"
    else
      let normalize_token_type creds =
        let token_type =
          if String.lowercase_ascii creds.token_type = "bearer" then "Bearer"
          else creds.token_type
        in
        { creds with token_type }
      in
      OAuth_http.exchange_code ~client_id ~client_secret ~redirect_uri ~code
        (fun short_lived_creds ->
          OAuth_http.exchange_for_long_lived_token
            ~client_id
            ~client_secret
            ~short_lived_token:short_lived_creds.access_token
            (fun long_lived_creds -> on_success (normalize_token_type long_lived_creds))
            (fun err ->
              on_error (Printf.sprintf
                "Failed to exchange for long-lived token: %s" err)))
        on_error

  (** Exchange OAuth code and return pages with Page access tokens.

      This helper performs the full server-side onboarding flow:
      1) Exchange code for short-lived user token
      2) Exchange for long-lived user token
      3) Retrieve managed pages via /me/accounts
  *)
  let exchange_code_and_get_pages ~code ~redirect_uri on_success on_error =
    let client_id = Config.get_env "FACEBOOK_APP_ID" |> Option.value ~default:"" in
    let client_secret = Config.get_env "FACEBOOK_APP_SECRET" |> Option.value ~default:"" in
    if client_id = "" || client_secret = "" then
      on_error "Facebook OAuth credentials not configured"
    else
      let normalize_token_type creds =
        let token_type =
          if String.lowercase_ascii creds.token_type = "bearer" then "Bearer"
          else creds.token_type
        in
        { creds with token_type }
      in
      OAuth_http.exchange_code ~client_id ~client_secret ~redirect_uri ~code
        (fun short_lived_creds ->
          let continue_with_user_creds user_creds =
            let user_creds = normalize_token_type user_creds in
            OAuth_http.get_user_pages ~user_access_token:user_creds.access_token
              (fun pages -> on_success (user_creds, pages))
              on_error
          in
          OAuth_http.exchange_for_long_lived_token
            ~client_id
            ~client_secret
            ~short_lived_token:short_lived_creds.access_token
            continue_with_user_creds
            (fun err ->
              on_error (Printf.sprintf
                "Failed to exchange for long-lived token before page discovery: %s" err)))
        on_error
  
  (** Validate content length *)
  let validate_content ~text =
    let len = String.length text in
    if len = 0 then
      Error "Text cannot be empty"
    else if len > 5000 then
      Error (Printf.sprintf "Facebook posts should be under 5000 characters for best engagement (current: %d)" len)
    else
      Ok ()

  (** {1 Insights / Analytics} *)

  type insight_timeseries_point = {
    end_time : string option;
    value : int;
  }

  type account_analytics = {
    page_impressions_unique : insight_timeseries_point list;
    page_posts_impressions_unique : insight_timeseries_point list;
    page_post_engagements : insight_timeseries_point list;
    page_daily_follows : insight_timeseries_point list;
    page_video_views : insight_timeseries_point list;
  }

  type post_analytics = {
    post_impressions_unique : int option;
    post_reactions_by_type_total : (string * int) list;
    post_clicks : int option;
    post_clicks_by_type : (string * int) list;
  }

  let account_analytics_metrics =
    "page_impressions_unique,page_posts_impressions_unique,page_post_engagements,page_daily_follows,page_video_views"

  let post_analytics_metrics =
    "post_impressions_unique,post_reactions_by_type_total,post_clicks,post_clicks_by_type"

  let account_analytics_canonical_metric_keys =
    Analytics_normalization.canonical_metric_keys_of_provider_metrics
      ~provider:Analytics_normalization.Facebook
      [ "page_impressions_unique";
        "page_posts_impressions_unique";
        "page_post_engagements";
        "page_daily_follows";
        "page_video_views" ]

  let post_analytics_canonical_metric_keys =
    Analytics_normalization.canonical_metric_keys_of_provider_metrics
      ~provider:Analytics_normalization.Facebook
      [ "post_impressions_unique";
        "post_reactions_by_type_total";
        "post_clicks";
        "post_clicks_by_type" ]

  let to_canonical_timeseries_points points =
    List.map
      (fun point -> Analytics_types.make_datapoint ?timestamp:point.end_time point.value)
      points

  let to_canonical_optional_point value_opt =
    match value_opt with
    | Some value -> [ Analytics_types.make_datapoint value ]
    | None -> []

  let to_canonical_assoc_total_points values =
    if values = [] then
      []
    else
      let total = List.fold_left (fun acc (_, value) -> acc + value) 0 values in
      [ Analytics_types.make_datapoint total ]

  let to_canonical_facebook_series ?time_range ~scope ~provider_metric points =
    match Analytics_normalization.facebook_metric_to_canonical provider_metric with
    | Some metric ->
        Some
          (Analytics_types.make_series
             ?time_range
             ~metric
             ~scope
             ~provider_metric
             points)
    | None -> None

  let to_canonical_account_analytics_series ?time_range account_analytics =
    [ ("page_impressions_unique",
       to_canonical_timeseries_points account_analytics.page_impressions_unique);
      ("page_posts_impressions_unique",
       to_canonical_timeseries_points account_analytics.page_posts_impressions_unique);
      ("page_post_engagements",
       to_canonical_timeseries_points account_analytics.page_post_engagements);
      ("page_daily_follows",
       to_canonical_timeseries_points account_analytics.page_daily_follows);
      ("page_video_views",
       to_canonical_timeseries_points account_analytics.page_video_views) ]
    |> List.filter_map (fun (provider_metric, points) ->
           to_canonical_facebook_series
             ?time_range
             ~scope:Analytics_types.Page
             ~provider_metric
             points)

  let to_canonical_post_analytics_series ?time_range post_analytics =
    [ ("post_impressions_unique",
       to_canonical_optional_point post_analytics.post_impressions_unique);
      ("post_reactions_by_type_total",
       to_canonical_assoc_total_points post_analytics.post_reactions_by_type_total);
      ("post_clicks",
       to_canonical_optional_point post_analytics.post_clicks);
      ("post_clicks_by_type",
       to_canonical_assoc_total_points post_analytics.post_clicks_by_type) ]
    |> List.filter_map (fun (provider_metric, points) ->
           to_canonical_facebook_series
             ?time_range
             ~scope:Analytics_types.Post
             ~provider_metric
             points)

  let int_of_json_safe json =
    try Some (Yojson.Basic.Util.to_int json)
    with _ ->
      try Some (int_of_float (Yojson.Basic.Util.to_float json))
      with _ -> None

  let int_assoc_of_json json =
    try
      json
      |> Yojson.Basic.Util.to_assoc
      |> List.filter_map (fun (name, value_json) ->
          match int_of_json_safe value_json with
          | Some value -> Some (name, value)
          | None -> None)
    with _ -> []

  let find_insight_by_name ~name insights =
    List.find_opt
      (fun metric_json ->
        try
          let metric_name =
            metric_json |> Yojson.Basic.Util.member "name" |> Yojson.Basic.Util.to_string
          in
          metric_name = name
        with _ -> false)
      insights

  let parse_timeseries_points metric_json =
    let open Yojson.Basic.Util in
    metric_json
    |> member "values"
    |> to_list
    |> List.map (fun value_json ->
           let value =
             match int_of_json_safe (value_json |> member "value") with
             | Some v -> v
             | None -> 0
           in
           {
             end_time = value_json |> member "end_time" |> to_string_option;
             value;
           })

  let parse_account_analytics json =
    let open Yojson.Basic.Util in
    let insights = json |> member "data" |> to_list in
    let metric_points metric_name =
      match find_insight_by_name ~name:metric_name insights with
      | Some metric_json -> parse_timeseries_points metric_json
      | None -> []
    in
    {
      page_impressions_unique = metric_points "page_impressions_unique";
      page_posts_impressions_unique = metric_points "page_posts_impressions_unique";
      page_post_engagements = metric_points "page_post_engagements";
      page_daily_follows = metric_points "page_daily_follows";
      page_video_views = metric_points "page_video_views";
    }

  let parse_post_analytics json =
    let open Yojson.Basic.Util in
    let insights = json |> member "data" |> to_list in
    let first_metric_value metric_name =
      match find_insight_by_name ~name:metric_name insights with
      | None -> None
      | Some metric_json ->
          (match metric_json |> member "values" |> to_list with
          | value_json :: _ -> Some (value_json |> member "value")
          | [] -> None)
    in
    {
      post_impressions_unique =
        (match first_metric_value "post_impressions_unique" with
        | Some json_value -> int_of_json_safe json_value
        | None -> None);
      post_reactions_by_type_total =
        (match first_metric_value "post_reactions_by_type_total" with
        | Some json_value -> int_assoc_of_json json_value
        | None -> []);
      post_clicks =
        (match first_metric_value "post_clicks" with
        | Some json_value -> int_of_json_safe json_value
        | None -> None);
      post_clicks_by_type =
        (match first_metric_value "post_clicks_by_type" with
        | Some json_value -> int_assoc_of_json json_value
        | None -> []);
    }

  (** Account analytics via page insights endpoint.

      Contract:
      GET https://graph.facebook.com/v21.0/{id}/insights
          ?metric=page_impressions_unique,page_posts_impressions_unique,page_post_engagements,page_daily_follows,page_video_views
          &period=day
          &since=...
          &until=...
          &access_token=...
  *)
  let get_account_analytics ~id ~since ~until ~access_token on_result =
    let url =
      Printf.sprintf
        "https://graph.facebook.com/v21.0/%s/insights?metric=%s&period=day&since=%s&until=%s&access_token=%s"
        id
        account_analytics_metrics
        since
        until
        access_token
    in
    Config.Http.get url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            on_result (Ok (parse_account_analytics json))
          with e ->
            on_result
              (Error
                 (Error_types.Internal_error
                    (Printf.sprintf
                       "Failed to parse account analytics response: %s"
                       (Printexc.to_string e))))
        else
          on_result
            (Error
               (parse_api_error_with_permissions
                  ~required_permissions:[ "pages_read_engagement" ]
                  ~status_code:response.status
                  ~response_body:response.body)))
      (fun err ->
        on_result
          (Error (Error_types.Internal_error (redact_sensitive_text_if_needed err))))

  let get_account_analytics_canonical ~id ~since ~until ~access_token on_result =
    let time_range =
      {
        Analytics_types.since = Some since;
        until_ = Some until;
        granularity = Some Analytics_types.Day;
      }
    in
    get_account_analytics ~id ~since ~until ~access_token
      (function
        | Ok analytics ->
            on_result
              (Ok
                 (to_canonical_account_analytics_series
                    ~time_range
                    analytics))
        | Error err -> on_result (Error err))

  (** Post analytics via post insights endpoint.

      Contract:
      GET https://graph.facebook.com/v21.0/{post_id}/insights
          ?metric=post_impressions_unique,post_reactions_by_type_total,post_clicks,post_clicks_by_type
          &access_token=...
  *)
  let get_post_analytics ~post_id ~access_token on_result =
    let url =
      Printf.sprintf
        "https://graph.facebook.com/v21.0/%s/insights?metric=%s&access_token=%s"
        post_id
        post_analytics_metrics
        access_token
    in
    Config.Http.get url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            on_result (Ok (parse_post_analytics json))
          with e ->
            on_result
              (Error
                 (Error_types.Internal_error
                    (Printf.sprintf
                       "Failed to parse post analytics response: %s"
                       (Printexc.to_string e))))
        else
          on_result
            (Error
               (parse_api_error_with_permissions
                  ~required_permissions:[ "pages_read_engagement" ]
                  ~status_code:response.status
                  ~response_body:response.body)))
      (fun err ->
        on_result
          (Error (Error_types.Internal_error (redact_sensitive_text_if_needed err))))

  let get_post_analytics_canonical ~post_id ~access_token on_result =
    get_post_analytics ~post_id ~access_token
      (function
        | Ok analytics ->
            on_result (Ok (to_canonical_post_analytics_series analytics))
        | Error err -> on_result (Error err))
  
  (** {1 Generic API Methods} *)
  
  (** Generic GET request to any Graph API endpoint *)
  let get ~path ~access_token ?fields ?required_permissions on_result =
    get_paginated ~path ~access_token ?fields ?cursor:None
      (function
        | Ok response -> on_result (Ok response)
        | Error (Error_types.Auth_error (Error_types.Insufficient_permissions _)) ->
            (match required_permissions with
             | Some perms ->
                 on_result (Error (Error_types.Auth_error (Error_types.Insufficient_permissions perms)))
             | None -> on_result (Error (Error_types.Auth_error (Error_types.Insufficient_permissions (permissions_for_path path)))))
        | Error e -> on_result (Error e))
  
  (** Get a page of results from a collection endpoint *)
  let get_page ~path ~access_token ?fields ?cursor (parse_data : Yojson.Basic.t -> 'a list) on_result =
    get_paginated ~path ~access_token ?fields ?cursor
      (function
        | Error e -> on_result (Error e)
        | Ok response ->
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let data = json |> member "data" |> parse_data in
              let paging, next_url, previous_url = parse_paging json in
              on_result (Ok {
                data;
                paging;
                next_url;
                previous_url;
              })
            with e ->
              on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e)))))
  
  (** Fetch next page using cursor *)
  let get_next_page ~path ~access_token ?fields ~cursor parse_data on_result =
    get_page ~path ~access_token ?fields ~cursor parse_data on_result
  
  (** Generic POST request to any Graph API endpoint *)
  let post ~path ~access_token ~params ?required_permissions on_result =
    let url = Printf.sprintf "%s/%s" graph_api_base path in
    
    let all_params = params @
      (match compute_app_secret_proof ~access_token with
       | Some proof -> [("appsecret_proof", [proof])]
       | None -> [])
    in
    
    let body = Uri.encoded_of_query all_params in
    let headers = [
      ("Content-Type", "application/x-www-form-urlencoded");
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
    ] in
    
    Config.Http.post ~headers ~body url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          on_result (Ok response)
        else
          let required_permissions =
            match required_permissions with
            | Some p -> p
            | None -> permissions_for_path path
          in
          on_result (Error (parse_api_error_with_permissions
            ~required_permissions
            ~status_code:response.status
            ~response_body:response.body)))
      (fun err -> on_result (Error (Error_types.Internal_error err)))
  
  (** Generic DELETE request to any Graph API endpoint *)
  let delete ~path ~access_token ?required_permissions on_result =
    let proof_params = match compute_app_secret_proof ~access_token with
      | Some proof -> [("appsecret_proof", [proof])]
      | None -> []
    in
    
    let url = Printf.sprintf "%s/%s" graph_api_base path in
    let final_url = 
      if List.length proof_params > 0 then
        url ^ "?" ^ Uri.encoded_of_query proof_params
      else url
    in
    
    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
    ] in
    
    Config.Http.delete ~headers final_url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          on_result (Ok response)
        else
          let required_permissions =
            match required_permissions with
            | Some p -> p
            | None -> permissions_for_path path
          in
          on_result (Error (parse_api_error_with_permissions
            ~required_permissions
            ~status_code:response.status
            ~response_body:response.body)))
      (fun err -> on_result (Error (Error_types.Internal_error err)))
  
  (** {1 Batch Requests} *)
  
  (** Batch request item *)
  type batch_request_item = {
    method_ : [`GET | `POST | `DELETE];
    relative_url : string;
    body : string option;
    name : string option;  (* For referencing in dependent requests *)
  }
  
  (** Batch response item *)
  type batch_response_item = {
    code : int;
    headers : (string * string) list;
    body : string;
  }
  
  (** Execute batch requests (up to 50 per batch) *)
  let batch_request ~requests ~access_token ?required_permissions on_result =
    if List.length requests = 0 then
      on_result (Error (Error_types.Internal_error "Batch request list cannot be empty"))
    else if List.length requests > 50 then
      on_result (Error (Error_types.Internal_error "Facebook allows maximum 50 requests per batch"))
    else
      let url = Printf.sprintf "%s/" graph_api_base in
      
      (* Build batch JSON *)
      let batch_items = List.map (fun req ->
        let method_str = match req.method_ with
          | `GET -> "GET"
          | `POST -> "POST"
          | `DELETE -> "DELETE"
        in
        let fields = [
          ("method", `String method_str);
          ("relative_url", `String req.relative_url);
        ] in
        let fields = match req.body with
          | Some body -> fields @ [("body", `String body)]
          | None -> fields
        in
        let fields = match req.name with
          | Some name -> fields @ [("name", `String name)]
          | None -> fields
        in
        `Assoc fields
      ) requests in
      
      let batch_json = Yojson.Basic.to_string (`List batch_items) in
      
      let params = [
        ("batch", [batch_json]);
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
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let results = json |> to_list |> List.map (fun item ->
                let code = item |> member "code" |> to_int in
                let headers_json = item |> member "headers" |> to_list in
                let headers = List.filter_map (fun h ->
                  try
                    let name = h |> member "name" |> to_string in
                    let value = h |> member "value" |> to_string in
                    Some (name, value)
                  with _ -> None
                ) headers_json in
                let body = item |> member "body" |> to_string in
                { code; headers; body }
              ) in
              on_result (Ok results)
            with e ->
              on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse batch response: %s" (Printexc.to_string e))))
          else
            let required_permissions =
              match required_permissions with
              | Some p -> p
              | None -> provider_required_permissions
            in
            on_result (Error (parse_api_error_with_permissions
              ~required_permissions
              ~status_code:response.status
              ~response_body:response.body)))
        (fun err -> on_result (Error (Error_types.Internal_error err)))

  (** {1 Comment Management} *)

  (** Comment on a post *)
  type comment = {
    comment_id : string;
    comment_message : string;
    comment_created_time : string option;
    comment_from : string option;
  }

  (** Post a comment on a post

      @param post_id The ID of the post to comment on
      @param access_token Page access token
      @param message The comment text
      @param on_result Continuation receiving Ok response or Error
  *)
  let post_comment ~post_id ~access_token ~message on_result =
    let path = Printf.sprintf "%s/comments" post_id in
    let params = [("message", [message])] in
    post ~path ~access_token ~params on_result

  (** Get comments on a post

      @param post_id The ID of the post to read comments from
      @param access_token Page access token
      @param on_result Continuation receiving Ok with list of comments or Error
  *)
  let get_comments ~post_id ~access_token on_result =
    let path = Printf.sprintf "%s/comments" post_id in
    get ~path ~access_token ~fields:["id"; "message"; "created_time"; "from"]
      (function
        | Ok response ->
            (try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let data = json |> member "data" |> to_list in
              let comments = List.map (fun item ->
                {
                  comment_id = item |> member "id" |> to_string;
                  comment_message = item |> member "message" |> to_string_option |> Option.value ~default:"";
                  comment_created_time = item |> member "created_time" |> to_string_option;
                  comment_from =
                    (try Some (item |> member "from" |> member "name" |> to_string)
                     with _ -> None);
                }
              ) data in
              on_result (Ok comments)
            with e ->
              on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse comments response: %s" (Printexc.to_string e)))))
        | Error e -> on_result (Error e))

  (** {1 Page Info} *)

  (** Page detail information *)
  type page_detail = {
    page_detail_id : string;
    page_detail_name : string option;
    page_detail_about : string option;
    page_detail_fan_count : int option;
    page_detail_followers_count : int option;
  }

  (** Fetch page info

      @param page_id The Facebook Page ID
      @param access_token Page access token
      @param on_result Continuation receiving Ok with page detail or Error
  *)
  let get_page_info ~page_id ~access_token on_result =
    get ~path:page_id ~access_token ~fields:["id"; "name"; "about"; "fan_count"; "followers_count"]
      (function
        | Ok response ->
            (try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let detail = {
                page_detail_id = json |> member "id" |> to_string;
                page_detail_name = json |> member "name" |> to_string_option;
                page_detail_about = json |> member "about" |> to_string_option;
                page_detail_fan_count = json |> member "fan_count" |> to_int_option;
                page_detail_followers_count = json |> member "followers_count" |> to_int_option;
              } in
              on_result (Ok detail)
            with e ->
              on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse page info response: %s" (Printexc.to_string e)))))
        | Error e -> on_result (Error e))
end
