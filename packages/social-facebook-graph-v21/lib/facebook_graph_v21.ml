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
      | Read_profile
      | Read_posts
      | Delete_post
      | Manage_pages
    
    (** Get scopes required for specific operations *)
    let for_operations ops =
      let base = ["public_profile"] in
      if List.exists (fun o -> o = Post_text || o = Post_media || o = Post_video || o = Delete_post || o = Manage_pages) ops
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
  
  (** {1 Error Handling} *)
  
  (** Parse Facebook error code into typed variant *)
  let parse_error_code code =
    match code with
    | 190 -> Invalid_token
    | 4 | 17 | 32 | 613 -> Rate_limit_exceeded
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
  
  (** {1 Rate Limiting} *)
  
  (** Parse X-App-Usage header *)
  let parse_rate_limit_header headers =
    try
      let usage_header = 
        List.find_opt (fun (k, _) -> 
          String.lowercase_ascii k = "x-app-usage"
        ) headers 
      in
      match usage_header with
      | Some (_, value) ->
          let json = Yojson.Basic.from_string value in
          let open Yojson.Basic.Util in
          let call_count = json |> member "call_count" |> to_int in
          let total_cputime = json |> member "total_cputime" |> to_int in
          let total_time = json |> member "total_time" |> to_int in
          (* Facebook uses percentage-based limits *)
          let percentage_used = float_of_int call_count in
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
  let get_paginated ~path ~access_token ?fields ?cursor on_success on_error =
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
          on_success response
        else
          match parse_facebook_error response.body with
          | Some err ->
              let msg = Printf.sprintf "Facebook API error (%s): %s%s"
                err.error_type err.message
                (match err.fbtrace_id with Some id -> Printf.sprintf " [trace: %s]" id | None -> "")
              in
              on_error msg
          | None ->
              on_error (Printf.sprintf "HTTP error (%d): %s" response.status response.body))
      on_error
  
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
            (fun () -> on_error "Facebook Page token expired - please reconnect")
            on_error
        else
          (* Token still valid *)
          Config.update_health_status ~account_id ~status:"healthy" ~error_message:None
            (fun () -> on_success creds.access_token)
            on_error)
      on_error
  
  (** Upload photo to Facebook Page with optional alt text *)
  let upload_photo ~page_id ~page_access_token ~image_url ~alt_text on_success on_error =
    (* Download image first *)
    Config.Http.get ~headers:[] image_url
      (fun image_response ->
        if image_response.status >= 200 && image_response.status < 300 then
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
                  on_success photo_id
                with e ->
                  on_error (Printf.sprintf "Failed to parse photo response: %s" (Printexc.to_string e))
              else
                match parse_facebook_error response.body with
                | Some err ->
                    let msg = Printf.sprintf "Photo upload failed (%s): %s" err.error_type err.message in
                    on_error msg
                | None ->
                    on_error (Printf.sprintf "Failed to upload photo (%d): %s" response.status response.body))
            on_error
        else
          on_error (Printf.sprintf "Failed to download image from %s (%d)" image_url image_response.status))
      on_error
  
  (** Upload video to Facebook Page for Reels 
      
      Facebook Reels use a two-step process:
      1. Initialize upload to get video_id
      2. Upload the video file
      3. Create the Reel post
      
      @see <https://developers.facebook.com/docs/video-api/guides/reels-publishing>
  *)
  let upload_video_reel ~page_id ~page_access_token ~video_url ~description on_success on_error =
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
                                  on_error "Facebook Reel upload failed: success=false"
                              with _e ->
                                (* If we can't parse but got 2xx, consider it a success *)
                                on_success video_id
                            else
                              match parse_facebook_error finish_response.body with
                              | Some err ->
                                  on_error (Printf.sprintf "Reel finish failed (%s): %s" err.error_type err.message)
                              | None ->
                                  on_error (Printf.sprintf "Reel finish failed (%d): %s" finish_response.status finish_response.body))
                          on_error
                      else
                        on_error (Printf.sprintf "Video upload failed (%d): %s" upload_response.status upload_response.body))
                    on_error
                with e ->
                  on_error (Printf.sprintf "Failed to parse init response: %s" (Printexc.to_string e))
              else
                match parse_facebook_error init_response.body with
                | Some err ->
                    on_error (Printf.sprintf "Reel init failed (%s): %s" err.error_type err.message)
                | None ->
                    on_error (Printf.sprintf "Reel init failed (%d): %s" init_response.status init_response.body))
            on_error
        else
          on_error (Printf.sprintf "Failed to download video from %s (%d)" video_url video_response.status))
      on_error
  
  (** Post Reel (short-form video) to Facebook Page *)
  let post_reel ~account_id ~text ~video_url on_success on_error =
    ensure_valid_token ~account_id
      (fun page_access_token ->
        Config.get_page_id ~account_id
          (fun page_id ->
            upload_video_reel ~page_id ~page_access_token ~video_url ~description:text
              on_success on_error)
          on_error)
      on_error
  
  (** Post to Facebook Page *)
  let post_single ~account_id ~text ~media_urls ?(alt_texts=[]) on_success on_error =
    ensure_valid_token ~account_id
      (fun page_access_token ->
        Config.get_page_id ~account_id
          (fun page_id ->
            (* Upload all photos first *)
            let rec upload_all_photos urls_with_alt acc on_complete =
              match urls_with_alt with
              | [] -> on_complete (List.rev acc)
              | (url, alt_text) :: rest ->
                  upload_photo ~page_id ~page_access_token ~image_url:url ~alt_text
                    (fun photo_id -> upload_all_photos rest (photo_id :: acc) on_complete)
                    on_error
            in
            
            (* Pair URLs with alt text - use None if alt text list is shorter *)
            let urls_with_alt = List.mapi (fun i url ->
              let alt_text = try List.nth alt_texts i with _ -> None in
              (url, alt_text)
            ) media_urls in
            
            upload_all_photos urls_with_alt [] (fun photo_ids ->
              (* Create Facebook Page post *)
              let url = Printf.sprintf "%s/%s/feed" graph_api_base page_id in
              
              let params = 
                [
                  ("message", [text]);
                ] @
                (if List.length photo_ids > 0 then
                  [("attached_media", [
                    Yojson.Basic.to_string (`List (List.map (fun photo_id ->
                      `Assoc [("media_fbid", `String photo_id)]
                    ) photo_ids))
                  ])]
                else []) @
                (* Add app secret proof *)
                (match compute_app_secret_proof ~access_token:page_access_token with
                 | Some proof -> [("appsecret_proof", [proof])]
                 | None -> [])
              in
              
              let body = Uri.encoded_of_query params in
              let headers = [
                ("Content-Type", "application/x-www-form-urlencoded");
                ("Authorization", Printf.sprintf "Bearer %s" page_access_token);
              ] in
              
              Config.Http.post ~headers ~body url
                (fun response ->
                  update_rate_limits response;
                  if response.status >= 200 && response.status < 300 then
                    try
                      let open Yojson.Basic.Util in
                      let json = Yojson.Basic.from_string response.body in
                      let post_id = json |> member "id" |> to_string in
                      on_success post_id
                    with _e ->
                      (* Post succeeded but couldn't parse ID *)
                      on_success "unknown"
                  else
                    match parse_facebook_error response.body with
                    | Some err ->
                        let msg = Printf.sprintf "Post failed (%s): %s%s"
                          err.error_type err.message
                          (if err.should_retry then 
                            match err.retry_after_seconds with
                            | Some delay -> Printf.sprintf " (retry after %d seconds)" delay
                            | None -> " (retry recommended)"
                          else "")
                        in
                        on_error msg
                    | None ->
                        on_error (Printf.sprintf "Facebook API error (%d): %s" response.status response.body))
                on_error))
          on_error)
      on_error
  
  (** Post thread (Facebook doesn't support threads, posts only first item) *)
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
    let client_id = Config.get_env "FACEBOOK_APP_ID" |> Option.value ~default:"" in
    
    if client_id = "" then
      on_error "Facebook App ID not configured"
    else (
      (* Facebook OAuth scopes for Pages management *)
      let scopes = [
        "pages_read_engagement";
        "pages_manage_posts";
        "pages_show_list";
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
  
  (** Exchange OAuth code for access token *)
  let exchange_code ~code ~redirect_uri on_success on_error =
    let client_id = Config.get_env "FACEBOOK_APP_ID" |> Option.value ~default:"" in
    let client_secret = Config.get_env "FACEBOOK_APP_SECRET" |> Option.value ~default:"" in
    
    if client_id = "" || client_secret = "" then
      on_error "Facebook OAuth credentials not configured"
    else (
      let params = [
        ("client_id", [client_id]);
        ("client_secret", [client_secret]);
        ("redirect_uri", [redirect_uri]);
        ("code", [code]);
      ] in
      
      let query = Uri.encoded_of_query params in
      let url = Printf.sprintf "%s/oauth/access_token?%s" graph_api_base query in
      
      Config.Http.get ~headers:[] url
        (fun response ->
          update_rate_limits response;
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let access_token = json |> member "access_token" |> to_string in
              let expires_in = json |> member "expires_in" |> to_int in
              let expires_at = 
                let now = Ptime_clock.now () in
                match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
                | Some exp -> Ptime.to_rfc3339 exp
                | None -> Ptime.to_rfc3339 now
              in
              let credentials = {
                access_token;
                refresh_token = None;  (* Facebook doesn't use refresh tokens *)
                expires_at = Some expires_at;
                token_type = "Bearer";
              } in
              on_success credentials
            with e ->
              on_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))
          else
            match parse_facebook_error response.body with
            | Some err ->
                on_error (Printf.sprintf "Token exchange failed (%s): %s" err.error_type err.message)
            | None ->
                on_error (Printf.sprintf "Token exchange failed (%d): %s" response.status response.body))
        on_error
    )
  
  (** Validate content length *)
  let validate_content ~text =
    let len = String.length text in
    if len = 0 then
      Error "Text cannot be empty"
    else if len > 5000 then
      Error (Printf.sprintf "Facebook posts should be under 5000 characters for best engagement (current: %d)" len)
    else
      Ok ()
  
  (** {1 Generic API Methods} *)
  
  (** Generic GET request to any Graph API endpoint *)
  let get ~path ~access_token ?fields on_success on_error =
    get_paginated ~path ~access_token ?fields ?cursor:None on_success on_error
  
  (** Get a page of results from a collection endpoint *)
  let get_page ~path ~access_token ?fields ?cursor (parse_data : Yojson.Basic.t -> 'a list) on_success on_error =
    get_paginated ~path ~access_token ?fields ?cursor
      (fun response ->
        try
          let json = Yojson.Basic.from_string response.body in
          let open Yojson.Basic.Util in
          let data = json |> member "data" |> parse_data in
          let paging, next_url, previous_url = parse_paging json in
          on_success {
            data;
            paging;
            next_url;
            previous_url;
          }
        with e ->
          on_error (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e)))
      on_error
  
  (** Fetch next page using cursor *)
  let get_next_page ~path ~access_token ?fields ~cursor parse_data on_success on_error =
    get_page ~path ~access_token ?fields ~cursor parse_data on_success on_error
  
  (** Generic POST request to any Graph API endpoint *)
  let post ~path ~access_token ~params on_success on_error =
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
          on_success response
        else
          match parse_facebook_error response.body with
          | Some err ->
              on_error (Printf.sprintf "POST failed (%s): %s" err.error_type err.message)
          | None ->
              on_error (Printf.sprintf "POST failed (%d): %s" response.status response.body))
      on_error
  
  (** Generic DELETE request to any Graph API endpoint *)
  let delete ~path ~access_token on_success on_error =
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
          on_success response
        else
          match parse_facebook_error response.body with
          | Some err ->
              on_error (Printf.sprintf "DELETE failed (%s): %s" err.error_type err.message)
          | None ->
              on_error (Printf.sprintf "DELETE failed (%d): %s" response.status response.body))
      on_error
  
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
  let batch_request ~requests ~access_token on_success on_error =
    if List.length requests = 0 then
      on_error "Batch request list cannot be empty"
    else if List.length requests > 50 then
      on_error "Facebook allows maximum 50 requests per batch"
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
              on_success results
            with e ->
              on_error (Printf.sprintf "Failed to parse batch response: %s" (Printexc.to_string e))
          else
            match parse_facebook_error response.body with
            | Some err ->
                on_error (Printf.sprintf "Batch request failed (%s): %s" err.error_type err.message)
            | None ->
                on_error (Printf.sprintf "Batch request failed (%d): %s" response.status response.body))
        on_error
end
