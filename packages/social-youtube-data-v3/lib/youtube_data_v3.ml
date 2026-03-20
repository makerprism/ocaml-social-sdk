(** YouTube Data API v3 Provider
    
    This implementation supports YouTube Shorts uploads.
    
    - Google OAuth 2.0 with PKCE
    - Access tokens expire after 1 hour
    - Refresh tokens don't expire (unless revoked)
    - Resumable upload for videos
*)

open Social_core

(** OAuth 2.0 module for YouTube Data API v3
    
    YouTube uses Google's OAuth 2.0 infrastructure with full PKCE support.
    
    Key characteristics:
    - PKCE is supported and recommended (S256 challenge method)
    - Access tokens expire after 1 hour
    - Refresh tokens never expire (unless revoked by user)
    - Uses URL-based scopes (e.g., https://www.googleapis.com/auth/youtube.upload)
    - Requires "access_type=offline" to receive refresh token
    - Requires "prompt=consent" to force consent and receive refresh token
    
    Required environment variables (or pass directly to functions):
    - YOUTUBE_CLIENT_ID: OAuth 2.0 Client ID from Google Cloud Console
    - YOUTUBE_CLIENT_SECRET: OAuth 2.0 Client Secret
    - YOUTUBE_REDIRECT_URI: Registered callback URL
*)
module OAuth = struct
  (** Scope definitions for YouTube Data API
      Note: YouTube uses full URL-based scopes, not short names
  *)
  module Scopes = struct
    (** Scopes for reading channel/video information *)
    let read = [
      "https://www.googleapis.com/auth/youtube.readonly";
    ]

    (** Scope for accessing YouTube Analytics data *)
    let analytics = [
      "https://www.googleapis.com/auth/yt-analytics.readonly";
    ]
    
    (** Scopes required for video upload *)
    let write = [
      "https://www.googleapis.com/auth/youtube.upload";
      "https://www.googleapis.com/auth/youtube";
    ]
    
    (** All commonly used scopes for YouTube management *)
    let all = [
      "https://www.googleapis.com/auth/youtube";
      "https://www.googleapis.com/auth/youtube.upload";
      "https://www.googleapis.com/auth/youtube.readonly";
      "https://www.googleapis.com/auth/youtube.force-ssl";
      "https://www.googleapis.com/auth/yt-analytics.readonly";
    ]
    
    (** Operations that can be performed with YouTube API *)
    type operation =
      | Upload_video
      | Read_channel
      | Read_videos
      | Manage_videos
      | Read_analytics

    (** Get scopes required for specific operations *)
    let for_operations ops =
      let needs_upload = List.exists (fun o -> o = Upload_video) ops in
      let needs_read = List.exists (fun o -> o = Read_channel || o = Read_videos) ops in
      let needs_manage = List.exists (fun o -> o = Manage_videos) ops in
      let needs_analytics = List.exists (fun o -> o = Read_analytics) ops in
      (if needs_manage then ["https://www.googleapis.com/auth/youtube"] else []) @
      (if needs_upload then ["https://www.googleapis.com/auth/youtube.upload"] else []) @
      (if needs_read && not needs_manage then ["https://www.googleapis.com/auth/youtube.readonly"] else []) @
      (if needs_analytics then ["https://www.googleapis.com/auth/yt-analytics.readonly"] else [])
  end
  
  (** Platform metadata for YouTube OAuth *)
  module Metadata = struct
    (** YouTube/Google supports PKCE with S256 method *)
    let supports_pkce = true
    
    (** YouTube supports token refresh *)
    let supports_refresh = true
    
    (** Access tokens expire after 1 hour *)
    let access_token_seconds = Some 3600
    
    (** Refresh tokens never expire (unless revoked) *)
    let refresh_token_seconds = None
    
    (** Recommended buffer before expiry (10 minutes) *)
    let refresh_buffer_seconds = 600
    
    (** Maximum retry attempts for token operations *)
    let max_refresh_attempts = 5
    
    (** Google OAuth 2.0 authorization endpoint *)
    let authorization_endpoint = "https://accounts.google.com/o/oauth2/v2/auth"
    
    (** Google OAuth 2.0 token endpoint *)
    let token_endpoint = "https://oauth2.googleapis.com/token"
    
    (** Token revocation endpoint *)
    let revocation_endpoint = "https://oauth2.googleapis.com/revoke"
    
    (** YouTube API base URL *)
    let api_base = "https://www.googleapis.com/youtube/v3"
    
    (** YouTube Upload API base URL *)
    let upload_base = "https://www.googleapis.com/upload/youtube/v3"
  end
  
  (** PKCE helper module for YouTube/Google OAuth *)
  module Pkce = struct
    (** Generate a code challenge from a code verifier using SHA256
        
        @param code_verifier The randomly generated code verifier (43-128 chars)
        @return Base64-URL-encoded SHA256 hash of the code verifier
    *)
    let generate_challenge code_verifier =
      let digest = Digestif.SHA256.digest_string code_verifier in
      let raw = Digestif.SHA256.to_raw_string digest in
      Base64.encode_string ~pad:false raw
      |> String.map (function '+' -> '-' | '/' -> '_' | c -> c)
    
    (** Code challenge method for Google OAuth *)
    let challenge_method = "S256"
  end
  
  (** Generate authorization URL for YouTube OAuth 2.0 flow with PKCE
      
      Note: Uses "access_type=offline" and "prompt=consent" to ensure
      a refresh token is returned.
      
      @param client_id Google OAuth 2.0 Client ID
      @param redirect_uri Registered callback URL
      @param state CSRF protection state parameter
      @param code_verifier PKCE code verifier (will be hashed for challenge)
      @param scopes OAuth scopes to request (defaults to Scopes.write)
      @return Full authorization URL to redirect user to
  *)
  let get_authorization_url ~client_id ~redirect_uri ~state ~code_verifier ?(scopes=Scopes.write) () =
    let code_challenge = Pkce.generate_challenge code_verifier in
    let scope_str = String.concat " " scopes in
    let params = [
      ("client_id", client_id);
      ("redirect_uri", redirect_uri);
      ("response_type", "code");
      ("scope", scope_str);
      ("state", state);
      ("access_type", "offline");  (* Required to get refresh token *)
      ("prompt", "consent");       (* Force consent to get refresh token *)
      ("code_challenge", code_challenge);
      ("code_challenge_method", Pkce.challenge_method);
    ] in
    let query = List.map (fun (k, v) ->
      Printf.sprintf "%s=%s" k (Uri.pct_encode v)
    ) params |> String.concat "&" in
    Printf.sprintf "%s?%s" Metadata.authorization_endpoint query
  
  (** Make functor for OAuth operations that need HTTP client *)
  module Make (Http : HTTP_CLIENT) = struct
    (** Exchange authorization code for access token with PKCE
        
        @param client_id Google OAuth 2.0 Client ID
        @param client_secret Google OAuth 2.0 Client Secret
        @param redirect_uri Registered callback URL
        @param code Authorization code from callback
        @param code_verifier PKCE code verifier (same as used for challenge)
        @param on_success Continuation receiving credentials
        @param on_error Continuation receiving error message
    *)
    let exchange_code ~client_id ~client_secret ~redirect_uri ~code ~code_verifier on_success on_error =
      let body = Printf.sprintf
        "grant_type=authorization_code&code=%s&redirect_uri=%s&client_id=%s&client_secret=%s&code_verifier=%s"
        (Uri.pct_encode code)
        (Uri.pct_encode redirect_uri)
        (Uri.pct_encode client_id)
        (Uri.pct_encode client_secret)
        (Uri.pct_encode code_verifier)
      in
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
              let refresh_token = 
                try Some (json |> member "refresh_token" |> to_string)
                with _ -> None in
              let expires_in = 
                try json |> member "expires_in" |> to_int
                with _ -> 3600 (* Default to 1 hour *)
              in
              let expires_at = 
                let now = Ptime_clock.now () in
                match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
                | Some exp -> Some (Ptime.to_rfc3339 exp)
                | None -> None in
              let token_type_str =
                try json |> member "token_type" |> to_string
                with _ -> "Bearer" in
              let creds : credentials = {
                access_token;
                refresh_token;
                expires_at;
                auth_type = auth_type_of_string token_type_str;
              } in
              on_success creds
            with e ->
              on_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "Token exchange failed (%d): %s" response.status response.body))
        on_error
    
    (** Refresh access token
        
        Google access tokens expire after 1 hour. Use this to get a new
        access token using the refresh token (which never expires).
        
        Note: Google doesn't always return a new refresh token. If not
        returned, continue using the original refresh token.
        
        @param client_id Google OAuth 2.0 Client ID
        @param client_secret Google OAuth 2.0 Client Secret
        @param refresh_token The refresh token from initial auth
        @param on_success Continuation receiving refreshed credentials
        @param on_error Continuation receiving error message
    *)
    let refresh_token ~client_id ~client_secret ~refresh_token on_success on_error =
      let body = Printf.sprintf
        "grant_type=refresh_token&refresh_token=%s&client_id=%s&client_secret=%s"
        (Uri.pct_encode refresh_token)
        (Uri.pct_encode client_id)
        (Uri.pct_encode client_secret)
      in
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
              (* Google doesn't always return new refresh token *)
              let new_refresh_token = 
                try Some (json |> member "refresh_token" |> to_string)
                with _ -> Some refresh_token in
              let expires_in = 
                try json |> member "expires_in" |> to_int
                with _ -> 3600
              in
              let expires_at = 
                let now = Ptime_clock.now () in
                match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
                | Some exp -> Some (Ptime.to_rfc3339 exp)
                | None -> None in
              let token_type_str =
                try json |> member "token_type" |> to_string
                with _ -> "Bearer" in
              let creds : credentials = {
                access_token;
                refresh_token = new_refresh_token;
                expires_at;
                auth_type = auth_type_of_string token_type_str;
              } in
              on_success creds
            with e ->
              on_error (Printf.sprintf "Failed to parse refresh response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "Token refresh failed (%d): %s" response.status response.body))
        on_error
    
    (** Revoke access or refresh token
        
        @param token The token to revoke (access or refresh)
        @param on_result Continuation receiving api_result
    *)
    let revoke_token ~token on_result =
      let url = Printf.sprintf "%s?token=%s" 
        Metadata.revocation_endpoint (Uri.pct_encode token) in
      
      Http.post ~headers:[] ~body:"" url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            on_result (Ok ())
          else
            on_result (Error (Error_types.Api_error {
              status_code = response.status;
              message = Printf.sprintf "Token revocation failed: %s" response.body;
              platform = Platform_types.YouTubeShorts;
              raw_response = Some response.body;
              request_id = None;
            })))
        (fun err -> on_result (Error (Error_types.Internal_error err)))
    
    (** Get user's channel info using access token
        
        @param access_token Valid access token
        @param on_result Continuation receiving api_result with channel info as JSON
    *)
    let get_channel_info ~access_token on_result =
      let url = Printf.sprintf "%s/channels?part=snippet,statistics&mine=true" 
        Metadata.api_base in
      let headers = [
        ("Authorization", "Bearer " ^ access_token);
      ] in
      
      Http.get ~headers url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              on_result (Ok json)
            with e ->
              on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse channel info: %s" (Printexc.to_string e))))
          else
            on_result (Error (Error_types.Api_error {
              status_code = response.status;
              message = Printf.sprintf "Get channel info failed: %s" response.body;
              platform = Platform_types.YouTubeShorts;
              raw_response = Some response.body;
              request_id = None;
            })))
        (fun err -> on_result (Error (Error_types.Internal_error err)))
  end
end

(** Configuration module type for YouTube provider *)
module type CONFIG = sig
  module Http : HTTP_CLIENT
  
  val get_env : string -> string option
  val get_credentials : account_id:string -> (credentials -> unit) -> (string -> unit) -> unit
  val update_credentials : account_id:string -> credentials:credentials -> (unit -> unit) -> (string -> unit) -> unit
  val encrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val decrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val update_health_status : account_id:string -> status:string -> error_message:string option -> (unit -> unit) -> (string -> unit) -> unit
end

(** Make functor to create YouTube provider with given configuration *)
module Make (Config : CONFIG) = struct
  let youtube_api_base = "https://www.googleapis.com/youtube/v3"
  let youtube_upload_base = "https://www.googleapis.com/upload/youtube/v3"
  let youtube_analytics_reports_base = "https://youtubeanalytics.googleapis.com/v2"
  let google_oauth_base = "https://oauth2.googleapis.com"
  
  (** {1 Platform Constants} *)
  
  let max_title_length = 100
  let max_description_length = 5000
  
  (** {1 Validation Functions} *)
  
  (** Validate a single post's content *)
  let validate_post ~text ?(media_count=0) () =
    let errors = ref [] in
    
    (* Check description length *)
    let text_len = String.length text in
    if text_len > max_description_length then
      errors := Error_types.Text_too_long { length = text_len; max = max_description_length } :: !errors;
    
    (* YouTube Shorts requires a video *)
    if media_count < 1 then
      errors := Error_types.Media_required :: !errors;
    
    if !errors = [] then Ok ()
    else Error (List.rev !errors)
  
  (** Validate thread content - YouTube only posts first item *)
  let validate_thread ~texts ~media_counts () =
    if List.length texts = 0 then
      Error [Error_types.Thread_empty]
    else
      (* Only validate first post since YouTube doesn't support threads *)
      let text = List.hd texts in
      let media_count = try List.hd media_counts with _ -> 0 in
      match validate_post ~text ~media_count () with
      | Error errs -> Error [Error_types.Thread_post_invalid { index = 0; errors = errs }]
      | Ok () -> Ok ()
  
  (** Validate video file for YouTube
      
      YouTube Shorts limits:
      - Videos: 256GB max (but Shorts are typically under 60 seconds)
      - For practical purposes we limit to 2GB since larger files would
        require chunked upload which isn't implemented
  *)
  let validate_media_file ~(media : Platform_types.post_media) =
    match media.Platform_types.media_type with
    | Platform_types.Video ->
        (* 2GB practical limit - avoids integer overflow and matches realistic upload scenarios *)
        let max_video_bytes = 2 * 1024 * 1024 * 1024 in (* 2GB *)
        if media.file_size_bytes > max_video_bytes then
          Error [Error_types.Media_too_large { 
            size_bytes = media.file_size_bytes; 
            max_bytes = max_video_bytes 
          }]
        else
          Ok ()
    | Platform_types.Image | Platform_types.Gif ->
        (* YouTube Shorts requires video, but we don't fail here - let validate_post handle it *)
        Ok ()
  
  (** Parse API error response and return structured Error_types.error *)
  let parse_api_error ~status_code ~response_body =
    try
      let json = Yojson.Basic.from_string response_body in
      let open Yojson.Basic.Util in
      let error_msg = 
        try 
          let error = json |> member "error" in
          try error |> member "message" |> to_string
          with _ -> response_body
        with _ -> response_body
      in
      
      (* Map common YouTube/Google errors *)
      if status_code = 401 then
        Error_types.Auth_error Error_types.Token_invalid
      else if status_code = 403 then
        Error_types.Auth_error (Error_types.Insufficient_permissions ["youtube.upload"])
      else if status_code = 429 then
        Error_types.Rate_limited { 
          retry_after_seconds = Some 60;
          limit = None;
          remaining = Some 0;
          reset_at = None;
        }
      else
        Error_types.Api_error {
          status_code;
          message = error_msg;
          platform = Platform_types.YouTubeShorts;
          raw_response = Some response_body;
          request_id = None;
        }
    with _ ->
      Error_types.Api_error {
        status_code;
        message = response_body;
        platform = Platform_types.YouTubeShorts;
        raw_response = Some response_body;
        request_id = None;
      }
  
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
  
  (** Refresh OAuth 2.0 access token *)
  let refresh_access_token ~client_id ~client_secret ~refresh_token on_success on_error =
    let url = google_oauth_base ^ "/token" in
    
    let body = Printf.sprintf
      "grant_type=refresh_token&refresh_token=%s&client_id=%s&client_secret=%s"
      (Uri.pct_encode refresh_token)
      (Uri.pct_encode client_id)
      (Uri.pct_encode client_secret)
    in
    
    let headers = [
      ("Content-Type", "application/x-www-form-urlencoded");
    ] in
    
    Config.Http.post ~headers ~body url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let open Yojson.Basic.Util in
            let json = Yojson.Basic.from_string response.body in
            let new_access = json |> member "access_token" |> to_string in
            (* Google doesn't always return new refresh token *)
            let new_refresh = 
              try json |> member "refresh_token" |> to_string
              with _ -> refresh_token
            in
            let expires_in = json |> member "expires_in" |> to_int in
            let expires_at = 
              let now = Ptime_clock.now () in
              match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
              | Some exp -> Ptime.to_rfc3339 exp
              | None -> Ptime.to_rfc3339 now
            in
            on_success (new_access, new_refresh, expires_at)
          with e ->
            on_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "Token refresh failed (%d): %s" response.status response.body))
      on_error
  
  (** Ensure valid OAuth 2.0 access token, refreshing if needed *)
  let ensure_valid_token ~account_id on_success on_error =
    let perform_refresh ~credentials on_refresh_success on_refresh_error =
      match credentials.Social_core.refresh_token with
      | None -> on_refresh_error (Error_types.Auth_error Error_types.Missing_credentials)
      | Some refresh_token ->
          let client_id = Config.get_env "YOUTUBE_CLIENT_ID" |> Option.value ~default:"" in
          let client_secret = Config.get_env "YOUTUBE_CLIENT_SECRET" |> Option.value ~default:"" in
          refresh_access_token ~client_id ~client_secret ~refresh_token
            (fun (new_access, new_refresh, expires_at) ->
              on_refresh_success {
                Social_core.access_token = new_access;
                refresh_token = Some new_refresh;
                expires_at = Some expires_at;
                auth_type = Bearer;
              })
            (fun err -> on_refresh_error (Error_types.Auth_error (Error_types.Refresh_failed err)))
    in
    Social_refresh.Orchestrator.ensure_valid_access_token
      ~policy:{ Social_refresh.refresh_window_seconds = 600 }
      ~account_id
      ~load_credentials:Config.get_credentials
      ~perform_refresh
      ~persist_credentials:Config.update_credentials
      ~update_health:Config.update_health_status
      (fun credentials -> on_success credentials.Social_core.access_token)
      on_error

  (** Channel-level analytics derived from YouTube Data API statistics. *)
  type account_analytics = {
    channel_id : string;
    title : string option;
    view_count : int;
    subscriber_count : int;
    hidden_subscriber_count : bool;
    video_count : int;
  }

  (** Video-level analytics derived from YouTube Data API statistics. *)
  type post_analytics = {
    video_id : string;
    title : string option;
    view_count : int;
    like_count : int;
    favorite_count : int;
    comment_count : int;
  }

  type report_column_header = {
    name : string;
    column_type : string;
    data_type : string;
  }

  type report_row = {
    day : string;
    metrics : (string * int) list;
  }

  type account_analytics_report = {
    start_date : string;
    end_date : string;
    metrics : string list;
    column_headers : report_column_header list;
    rows : report_row list;
  }

  type post_analytics_report = {
    video_id : string;
    start_date : string;
    end_date : string;
    metrics : string list;
    column_headers : report_column_header list;
    rows : report_row list;
  }

  let to_canonical_optional_point value_opt =
    match value_opt with
    | Some value -> [ Analytics_types.make_datapoint value ]
    | None -> []

  let to_canonical_youtube_series ?time_range ~scope ~provider_metric points =
    match Analytics_normalization.youtube_metric_to_canonical provider_metric with
    | Some metric ->
        Some
          (Analytics_types.make_series
             ?time_range
             ~metric
             ~scope
             ~provider_metric
             points)
    | None -> None

  let to_canonical_account_analytics_series
      (account_analytics : account_analytics) =
    [ ("view_count", to_canonical_optional_point (Some account_analytics.view_count));
      ("subscriber_count",
       to_canonical_optional_point (Some account_analytics.subscriber_count));
      ("video_count", to_canonical_optional_point (Some account_analytics.video_count)) ]
    |> List.filter_map (fun (provider_metric, points) ->
           to_canonical_youtube_series
             ~scope:Analytics_types.Channel
             ~provider_metric
             points)

  let to_canonical_post_analytics_series (post_analytics : post_analytics) =
    [ ("view_count", to_canonical_optional_point (Some post_analytics.view_count));
      ("like_count", to_canonical_optional_point (Some post_analytics.like_count));
      ("favorite_count", to_canonical_optional_point (Some post_analytics.favorite_count));
      ("comment_count", to_canonical_optional_point (Some post_analytics.comment_count)) ]
    |> List.filter_map (fun (provider_metric, points) ->
           to_canonical_youtube_series
             ~scope:Analytics_types.Video
             ~provider_metric
             points)

  type thumbnail_upload_request = {
    url : string;
    headers : (string * string) list;
    parts : multipart_part list;
  }

  let normalize_content_type value =
    value
    |> String.trim
    |> String.lowercase_ascii
    |> String.split_on_char ';'
    |> List.hd
    |> String.trim

  let read_json_int_field ~obj ~field ~default =
    let open Yojson.Basic.Util in
    let value_to_int = function
      | `Int i -> Some i
      | `String s -> int_of_string_opt s
      | `Float f -> Some (int_of_float f)
      | _ -> None
    in
    try
      let value = obj |> member field in
      if value = `Null then default
      else
        match value_to_int value with
        | Some i -> i
        | None -> default
    with _ -> default

  let read_json_bool_field ~obj ~field ~default =
    let open Yojson.Basic.Util in
    try
      let value = obj |> member field in
      if value = `Null then default else to_bool value
    with _ -> default

  let read_json_string_field_opt ~obj ~field =
    let open Yojson.Basic.Util in
    try
      let value = obj |> member field in
      if value = `Null then None
      else
        let s = to_string value in
        if String.trim s = "" then None else Some s
    with _ -> None

  let normalize_report_metrics metrics =
    metrics
    |> List.map String.trim
    |> List.filter (fun metric -> metric <> "")

  let validate_reports_query_inputs ~start_date ~end_date ~metrics =
    let normalized_start_date = String.trim start_date in
    let normalized_end_date = String.trim end_date in
    let normalized_metrics = normalize_report_metrics metrics in
    if normalized_start_date = "" then
      Error (Error_types.Internal_error "start_date is required")
    else if normalized_end_date = "" then
      Error (Error_types.Internal_error "end_date is required")
    else if normalized_metrics = [] then
      Error (Error_types.Internal_error "metrics is required")
    else
      Ok (normalized_start_date, normalized_end_date, normalized_metrics)

  let account_analytics_report_url ~start_date ~end_date ~metrics =
    let query =
      Uri.encoded_of_query
        [ ("ids", [ "channel==MINE" ]);
          ("startDate", [ start_date ]);
          ("endDate", [ end_date ]);
          ("metrics", [ String.concat "," metrics ]);
          ("dimensions", [ "day" ]) ]
    in
    Printf.sprintf "%s/reports?%s" youtube_analytics_reports_base query

  let post_analytics_report_url ~video_id ~start_date ~end_date ~metrics =
    let query =
      Uri.encoded_of_query
        [ ("ids", [ "channel==MINE" ]);
          ("startDate", [ start_date ]);
          ("endDate", [ end_date ]);
          ("metrics", [ String.concat "," metrics ]);
          ("dimensions", [ "day" ]);
          ("filters", [ "video==" ^ video_id ]) ]
    in
    Printf.sprintf "%s/reports?%s" youtube_analytics_reports_base query

  let parse_report_column_headers json =
    let open Yojson.Basic.Util in
    try
      let headers = json |> member "columnHeaders" |> to_list in
      headers
      |> List.map (fun header ->
             {
               name = header |> member "name" |> to_string;
               column_type = header |> member "columnType" |> to_string;
               data_type = header |> member "dataType" |> to_string;
             })
      |> fun parsed_headers ->
      if parsed_headers = [] then
        Error (`Malformed "Report payload is missing column headers")
      else
        Ok parsed_headers
    with e ->
      Error
        (`Malformed
          (Printf.sprintf
             "Failed to parse report column headers: %s"
             (Printexc.to_string e)))

  let report_day_column_index column_headers =
    let rec loop index = function
      | [] -> None
      | header :: rest ->
          if String.lowercase_ascii (String.trim header.name) = "day" then Some index
          else loop (index + 1) rest
    in
    loop 0 column_headers

  let report_metric_columns column_headers =
    let rec loop index acc = function
      | [] -> List.rev acc
      | header :: rest ->
          let acc' =
            if String.uppercase_ascii (String.trim header.column_type) = "METRIC" then
              (index, header) :: acc
            else
              acc
          in
          loop (index + 1) acc' rest
    in
    loop 0 [] column_headers

  let ensure_requested_report_metrics ~requested_metrics ~column_headers =
    let available_metrics =
      report_metric_columns column_headers
      |> List.map (fun (_, header) -> String.lowercase_ascii (String.trim header.name))
    in
    let missing_metrics =
      requested_metrics
      |> List.filter_map (fun metric ->
             let normalized_metric = String.lowercase_ascii (String.trim metric) in
             if normalized_metric = "" || List.mem normalized_metric available_metrics then
               None
             else
               Some metric)
    in
    if missing_metrics = [] then
      Ok ()
    else
      Error
        (`Malformed
          (Printf.sprintf
             "Report payload is missing metrics: %s"
             (String.concat "," missing_metrics)))

  let rec nth_opt values index =
    match values, index with
    | [], _ -> None
    | value :: _, 0 -> Some value
    | _ :: rest, i when i > 0 -> nth_opt rest (i - 1)
    | _ -> None

  let report_metric_value_to_int value =
    match value with
    | `Int i -> Some i
    | `String s -> int_of_string_opt (String.trim s)
    | `Float f -> Some (int_of_float f)
    | _ -> None

  let parse_report_rows ~column_headers json =
    let open Yojson.Basic.Util in
    let rows_result =
      let rows_value = json |> member "rows" in
      match rows_value with
      | `Null -> Ok []
      | `List row_items -> Ok row_items
      | _ -> Error "Report rows must be a list"
    in
    match rows_result with
    | Error msg -> Error (`Malformed msg)
    | Ok rows ->
        (match report_day_column_index column_headers with
         | None -> Error (`Malformed "Report column headers are missing day dimension")
         | Some day_column_index ->
             let metric_columns = report_metric_columns column_headers in
             if metric_columns = [] then
               Error (`Malformed "Report column headers are missing metric columns")
             else
               let parse_single_row row_index row_json =
                 try
                   let values = row_json |> to_list in
                   let day_result =
                     match nth_opt values day_column_index with
                     | Some value ->
                         (try
                            let day_value = to_string value in
                            if String.trim day_value = "" then
                              Error (Printf.sprintf "Row %d has empty day value" row_index)
                            else
                              Ok day_value
                          with exn ->
                            Error
                              (Printf.sprintf
                                 "Row %d has invalid day value: %s"
                                 row_index
                                 (Printexc.to_string exn)))
                     | None ->
                         Error
                           (Printf.sprintf
                              "Row %d is missing day value at index %d"
                              row_index
                              day_column_index)
                   in
                   match day_result with
                   | Error msg -> Error msg
                   | Ok day ->
                       let rec collect_metrics acc = function
                         | [] -> Ok (List.rev acc)
                         | (metric_index, metric_header) :: rest ->
                             let metric_result =
                               match nth_opt values metric_index with
                               | Some value ->
                                   (match report_metric_value_to_int value with
                                    | Some parsed -> Ok parsed
                                    | None ->
                                        Error
                                          (Printf.sprintf
                                             "Row %d metric %s is not an integer"
                                             row_index
                                             metric_header.name))
                               | None ->
                                   Error
                                     (Printf.sprintf
                                        "Row %d is missing metric %s"
                                        row_index
                                        metric_header.name)
                             in
                             (match metric_result with
                              | Error msg -> Error msg
                              | Ok metric_value ->
                                  collect_metrics
                                    ((metric_header.name, metric_value) :: acc)
                                    rest)
                       in
                       (match collect_metrics [] metric_columns with
                        | Error msg -> Error msg
                        | Ok metrics -> Ok { day; metrics })
                 with exn ->
                   Error
                     (Printf.sprintf
                        "Failed to parse report row %d: %s"
                        row_index
                        (Printexc.to_string exn))
               in
               let rec parse_all_rows row_index acc = function
                 | [] -> Ok (List.rev acc)
                 | row_json :: rest ->
                     (match parse_single_row row_index row_json with
                      | Error msg ->
                          Error (`Malformed (Printf.sprintf "Failed to parse report rows: %s" msg))
                      | Ok parsed_row ->
                          parse_all_rows (row_index + 1) (parsed_row :: acc) rest)
               in
               parse_all_rows 0 [] rows)

  let parse_account_analytics_report_response ~start_date ~end_date ~metrics response_body =
    try
      let json = Yojson.Basic.from_string response_body in
      match parse_report_column_headers json with
      | Error (`Malformed msg) -> Error (`Malformed msg)
      | Ok column_headers ->
          (match ensure_requested_report_metrics ~requested_metrics:metrics ~column_headers with
           | Error (`Malformed msg) -> Error (`Malformed msg)
           | Ok () ->
               (match parse_report_rows ~column_headers json with
                | Error (`Malformed msg) -> Error (`Malformed msg)
                | Ok rows ->
                    Ok
                      {
                        start_date;
                        end_date;
                        metrics;
                        column_headers;
                        rows;
                      }))
    with e ->
      Error
        (`Malformed
          (Printf.sprintf
             "Failed to parse account report response: %s"
             (Printexc.to_string e)))

  let parse_post_analytics_report_response ~video_id ~start_date ~end_date ~metrics response_body =
    try
      let json = Yojson.Basic.from_string response_body in
      match parse_report_column_headers json with
      | Error (`Malformed msg) -> Error (`Malformed msg)
      | Ok column_headers ->
          (match ensure_requested_report_metrics ~requested_metrics:metrics ~column_headers with
           | Error (`Malformed msg) -> Error (`Malformed msg)
           | Ok () ->
               (match parse_report_rows ~column_headers json with
                | Error (`Malformed msg) -> Error (`Malformed msg)
                | Ok rows ->
                    Ok
                      {
                        video_id;
                        start_date;
                        end_date;
                        metrics;
                        column_headers;
                        rows;
                      }))
    with e ->
      Error
        (`Malformed
          (Printf.sprintf "Failed to parse post report response: %s" (Printexc.to_string e)))

  let account_analytics_url () =
    youtube_api_base ^ "/channels?part=id,snippet,statistics&mine=true"

  let post_analytics_url ~video_id =
    let query =
      Uri.encoded_of_query
        [ ("part", [ "id,snippet,statistics" ]);
          ("id", [ String.trim video_id ]) ]
    in
    Printf.sprintf "%s/videos?%s" youtube_api_base query

  let parse_account_analytics_response response_body =
    let open Yojson.Basic.Util in
    try
      let json = Yojson.Basic.from_string response_body in
      let items = json |> member "items" |> to_list in
      match items with
      | [] -> Error (`Not_found "channel")
      | first :: _ ->
          let statistics = first |> member "statistics" in
          let snippet = first |> member "snippet" in
          Ok {
            channel_id = first |> member "id" |> to_string;
            title = read_json_string_field_opt ~obj:snippet ~field:"title";
            view_count = read_json_int_field ~obj:statistics ~field:"viewCount" ~default:0;
            subscriber_count = read_json_int_field ~obj:statistics ~field:"subscriberCount" ~default:0;
            hidden_subscriber_count =
              read_json_bool_field ~obj:statistics ~field:"hiddenSubscriberCount" ~default:false;
            video_count = read_json_int_field ~obj:statistics ~field:"videoCount" ~default:0;
          }
    with e ->
      Error (`Malformed (Printf.sprintf "Failed to parse account analytics response: %s" (Printexc.to_string e)))

  let parse_post_analytics_response ~video_id response_body =
    let open Yojson.Basic.Util in
    try
      let json = Yojson.Basic.from_string response_body in
      let items = json |> member "items" |> to_list in
      match items with
      | [] -> Error (`Not_found video_id)
      | first :: _ ->
          let statistics = first |> member "statistics" in
          let snippet = first |> member "snippet" in
          Ok {
            video_id = first |> member "id" |> to_string;
            title = read_json_string_field_opt ~obj:snippet ~field:"title";
            view_count = read_json_int_field ~obj:statistics ~field:"viewCount" ~default:0;
            like_count = read_json_int_field ~obj:statistics ~field:"likeCount" ~default:0;
            favorite_count = read_json_int_field ~obj:statistics ~field:"favoriteCount" ~default:0;
            comment_count = read_json_int_field ~obj:statistics ~field:"commentCount" ~default:0;
          }
    with e ->
      Error (`Malformed (Printf.sprintf "Failed to parse post analytics response: %s" (Printexc.to_string e)))

  let get_account_analytics ~account_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let url = account_analytics_url () in
        let headers = [ ("Authorization", "Bearer " ^ access_token) ] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              match parse_account_analytics_response response.body with
              | Ok analytics -> on_result (Ok analytics)
              | Error (`Not_found resource) -> on_result (Error (Error_types.Resource_not_found resource))
              | Error (`Malformed msg) -> on_result (Error (Error_types.Internal_error msg))
            else
              on_result
                (Error
                   (parse_api_error ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  let get_account_analytics_canonical ~account_id on_result =
    get_account_analytics ~account_id
      (function
        | Ok analytics ->
            on_result (Ok (to_canonical_account_analytics_series analytics))
        | Error err -> on_result (Error err))

  let get_post_analytics ~account_id ~video_id on_result =
    let normalized_video_id = String.trim video_id in
    if normalized_video_id = "" then
      on_result (Error (Error_types.Internal_error "video_id is required"))
    else
      ensure_valid_token ~account_id
        (fun access_token ->
          let url = post_analytics_url ~video_id:normalized_video_id in
          let headers = [ ("Authorization", "Bearer " ^ access_token) ] in
          Config.Http.get ~headers url
            (fun response ->
              if response.status >= 200 && response.status < 300 then
                match parse_post_analytics_response ~video_id:normalized_video_id response.body with
                | Ok analytics -> on_result (Ok analytics)
                | Error (`Not_found resource) -> on_result (Error (Error_types.Resource_not_found resource))
                | Error (`Malformed msg) -> on_result (Error (Error_types.Internal_error msg))
              else
                on_result
                  (Error
                     (parse_api_error ~status_code:response.status ~response_body:response.body)))
            (fun err -> on_result (Error (Error_types.Internal_error err))))
        (fun err -> on_result (Error err))

  let get_post_analytics_canonical ~account_id ~video_id on_result =
    get_post_analytics ~account_id ~video_id
      (function
        | Ok analytics -> on_result (Ok (to_canonical_post_analytics_series analytics))
        | Error err -> on_result (Error err))

  let get_account_analytics_report ~account_id ~start_date ~end_date ~metrics on_result =
    match validate_reports_query_inputs ~start_date ~end_date ~metrics with
    | Error err -> on_result (Error err)
    | Ok (normalized_start_date, normalized_end_date, normalized_metrics) ->
        ensure_valid_token ~account_id
          (fun access_token ->
            let url =
              account_analytics_report_url
                ~start_date:normalized_start_date
                ~end_date:normalized_end_date
                ~metrics:normalized_metrics
            in
            let headers = [ ("Authorization", "Bearer " ^ access_token) ] in
            Config.Http.get ~headers url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  match
                    parse_account_analytics_report_response
                      ~start_date:normalized_start_date
                      ~end_date:normalized_end_date
                      ~metrics:normalized_metrics
                      response.body
                  with
                  | Ok report -> on_result (Ok report)
                  | Error (`Malformed msg) ->
                      on_result (Error (Error_types.Internal_error msg))
                else
                  on_result
                    (Error
                       (parse_api_error
                          ~status_code:response.status
                          ~response_body:response.body)))
              (fun err -> on_result (Error (Error_types.Internal_error err))))
          (fun err -> on_result (Error err))

  let get_post_analytics_report
      ~account_id
      ~video_id
      ~start_date
      ~end_date
      ~metrics
      on_result =
    let normalized_video_id = String.trim video_id in
    if normalized_video_id = "" then
      on_result (Error (Error_types.Internal_error "video_id is required"))
    else
      match validate_reports_query_inputs ~start_date ~end_date ~metrics with
      | Error err -> on_result (Error err)
      | Ok (normalized_start_date, normalized_end_date, normalized_metrics) ->
          ensure_valid_token ~account_id
            (fun access_token ->
              let url =
                post_analytics_report_url
                  ~video_id:normalized_video_id
                  ~start_date:normalized_start_date
                  ~end_date:normalized_end_date
                  ~metrics:normalized_metrics
              in
              let headers = [ ("Authorization", "Bearer " ^ access_token) ] in
              Config.Http.get ~headers url
                (fun response ->
                  if response.status >= 200 && response.status < 300 then
                    match
                      parse_post_analytics_report_response
                        ~video_id:normalized_video_id
                        ~start_date:normalized_start_date
                        ~end_date:normalized_end_date
                        ~metrics:normalized_metrics
                        response.body
                    with
                    | Ok report -> on_result (Ok report)
                    | Error (`Malformed msg) ->
                        on_result (Error (Error_types.Internal_error msg))
                  else
                    on_result
                      (Error
                         (parse_api_error
                            ~status_code:response.status
                            ~response_body:response.body)))
                (fun err -> on_result (Error (Error_types.Internal_error err))))
            (fun err -> on_result (Error err))

  let supported_thumbnail_content_types =
    [ "image/jpeg"; "image/jpg"; "image/png"; "image/webp" ]

  let default_thumbnail_filename_for_content_type = function
    | "image/png" -> "thumbnail.png"
    | "image/webp" -> "thumbnail.webp"
    | _ -> "thumbnail.jpg"

  let build_thumbnail_upload_request
      ~access_token
      ~video_id
      ~thumbnail_content
      ?content_type
      ?filename
      () =
    let normalized_video_id = String.trim video_id in
    if normalized_video_id = "" then
      Error (Error_types.Internal_error "video_id is required")
    else if thumbnail_content = "" then
      Error (Error_types.Validation_error [ Error_types.Media_required ])
    else
      let normalized_content_type =
        match content_type with
        | Some value when String.trim value <> "" -> normalize_content_type value
        | _ -> "image/jpeg"
      in
      if not (List.mem normalized_content_type supported_thumbnail_content_types) then
        Error (Error_types.Validation_error [ Error_types.Media_unsupported_format normalized_content_type ])
      else
        let upload_query =
          Uri.encoded_of_query
            [ ("videoId", [ normalized_video_id ]);
              ("uploadType", [ "multipart" ]) ]
        in
        let upload_url = Printf.sprintf "%s/thumbnails/set?%s" youtube_upload_base upload_query in
        let resolved_filename =
          match filename with
          | Some name when String.trim name <> "" -> String.trim name
          | _ -> default_thumbnail_filename_for_content_type normalized_content_type
        in
        let request : thumbnail_upload_request = {
          url = upload_url;
          headers = [ ("Authorization", "Bearer " ^ access_token) ];
          parts = [
            {
              name = "media";
              filename = Some resolved_filename;
              content_type = Some normalized_content_type;
              content = thumbnail_content;
            }
          ];
        } in
        Ok request

  let upload_thumbnail
      ~account_id
      ~video_id
      ~thumbnail_content
      ?content_type
      ?filename
      on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        match build_thumbnail_upload_request
                ~access_token
                ~video_id
                ~thumbnail_content
                ?content_type
                ?filename
                ()
        with
        | Error err -> on_result (Error err)
        | Ok request ->
            Config.Http.post_multipart ~headers:request.headers ~parts:request.parts request.url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  on_result (Ok ())
                else
                  on_result
                    (Error
                       (parse_api_error ~status_code:response.status ~response_body:response.body)))
              (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  let upload_thumbnail_from_url
      ~account_id
      ~video_id
      ~thumbnail_url
      ?content_type
      ?filename
      on_result =
    let normalized_thumbnail_url = String.trim thumbnail_url in
    if normalized_thumbnail_url = "" then
      on_result (Error (Error_types.Validation_error [ Error_types.Invalid_url thumbnail_url ]))
    else
      ensure_valid_token ~account_id
        (fun access_token ->
          Config.Http.get ~headers:[] normalized_thumbnail_url
            (fun image_response ->
              if image_response.status >= 200 && image_response.status < 300 then
                let inferred_content_type =
                  match content_type with
                  | Some value when String.trim value <> "" -> Some value
                  | _ -> List.assoc_opt "content-type" image_response.headers
                in
                (match build_thumbnail_upload_request
                         ~access_token
                         ~video_id
                         ~thumbnail_content:image_response.body
                         ?content_type:inferred_content_type
                         ?filename
                         ()
                 with
                 | Error err -> on_result (Error err)
                 | Ok request ->
                     Config.Http.post_multipart ~headers:request.headers ~parts:request.parts request.url
                       (fun response ->
                         if response.status >= 200 && response.status < 300 then
                           on_result (Ok ())
                         else
                           on_result
                             (Error
                                (parse_api_error ~status_code:response.status ~response_body:response.body)))
                       (fun err -> on_result (Error (Error_types.Internal_error err))))
              else if image_response.status = 404 then
                on_result (Error (Error_types.Resource_not_found normalized_thumbnail_url))
              else
                on_result
                  (Error
                     (Error_types.Network_error
                        (Error_types.Connection_failed
                           (Printf.sprintf "Thumbnail download failed (%d)" image_response.status)))))
            (fun err -> on_result (Error (Error_types.Internal_error err))))
        (fun err -> on_result (Error err))
  
  (** Upload video to YouTube Shorts

      @param validate_media_before_upload When true, validates video file size after
             download but before upload. Practical limit: 2GB.
             Default: false
      @param publish_at Optional ISO 8601 timestamp for scheduled publishing.
             When set, privacy is automatically forced to "private".
      @param notify_subscribers When false, suppresses subscriber notifications.
             Default: true
  *)
  let post_single ~account_id ~text ~media_urls ?(alt_texts=[]) ?(title="") ?(description="") ?(privacy_status="public") ?(tags=[]) ?(made_for_kids=false) ?(category_id="22") ?(validate_media_before_upload=false) ?publish_at ?(notify_subscribers=true) on_result =
    let _ = alt_texts in (* YouTube doesn't support alt text for videos *)
    let media_count = List.length media_urls in
    match validate_post ~text ~media_count () with
    | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        (* Validation ensures media_urls is non-empty *)
        let video_url = List.hd media_urls in
        ensure_valid_token ~account_id
          (fun access_token ->
            (* Download video *)
            Config.Http.get ~headers:[] video_url
              (fun video_response ->
                if video_response.status >= 200 && video_response.status < 300 then
                  let video_data = video_response.body in
                  let file_size = String.length video_data in

                  (* Validate if requested *)
                  let validation_result =
                    if validate_media_before_upload then
                      let media : Platform_types.post_media = {
                        media_type = Platform_types.Video;
                        mime_type = List.assoc_opt "content-type" video_response.headers
                                    |> Option.value ~default:"video/mp4";
                        file_size_bytes = file_size;
                        width = None;
                        height = None;
                        duration_seconds = None;
                        alt_text = None;
                      } in
                      validate_media_file ~media
                    else
                      Ok ()
                  in

                  (match validation_result with
                  | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
                  | Ok () ->
                  let content_type =
                    match List.assoc_opt "content-type" video_response.headers with
                    | Some ct -> ct
                    | None -> "video/mp4"
                  in

                  (* Resolve metadata: use explicit params or derive from text *)
                  let resolved_title =
                    if title <> "" then title
                    else String.sub text 0 (min (String.length text) max_title_length)
                  in
                  let resolved_description =
                    if description <> "" then description
                    else text ^ " #Shorts"
                  in
                  let resolved_tags =
                    if tags <> [] then List.map (fun t -> `String t) tags
                    else [`String "shorts"; `String "short"]
                  in

                  (* When publish_at is set, force privacy to "private" per YouTube API requirement *)
                  let effective_privacy = match publish_at with
                    | Some _ -> "private"
                    | None -> privacy_status
                  in

                  (* Build status fields, optionally including publishAt *)
                  let status_fields =
                    [("privacyStatus", `String effective_privacy);
                     ("selfDeclaredMadeForKids", `Bool made_for_kids)]
                    @ (match publish_at with
                       | Some ts -> [("publishAt", `String ts)]
                       | None -> [])
                  in

                  (* Step 1: Initialize resumable upload with metadata *)
                  let video_metadata = `Assoc [
                    ("snippet", `Assoc [
                      ("title", `String resolved_title);
                      ("description", `String resolved_description);
                      ("tags", `List resolved_tags);
                      ("categoryId", `String category_id);
                    ]);
                    ("status", `Assoc status_fields);
                  ] in

                  let notify_param = if notify_subscribers then "true" else "false" in
                  let init_url = youtube_upload_base ^ "/videos?uploadType=resumable&part=snippet,status&notifySubscribers=" ^ notify_param in
                  let init_headers = [
                    ("Authorization", "Bearer " ^ access_token);
                    ("Content-Type", "application/json");
                    ("X-Upload-Content-Length", string_of_int (String.length video_data));
                    ("X-Upload-Content-Type", content_type);
                  ] in

                  let metadata_str = Yojson.Basic.to_string video_metadata in

                  let parse_upload_response upload_response =
                    if upload_response.status >= 200 && upload_response.status < 300 then
                      try
                        let open Yojson.Basic.Util in
                        let json = Yojson.Basic.from_string upload_response.body in
                        let video_id = json |> member "id" |> to_string in
                        on_result (Error_types.Success video_id)
                      with _e ->
                        on_result (Error_types.Failure (Error_types.Internal_error (Printf.sprintf "Failed to parse response: %s" upload_response.body)))
                    else
                      on_result (Error_types.Failure (parse_api_error ~status_code:upload_response.status ~response_body:upload_response.body))
                  in

                  (* H5: Resumable upload recovery - query upload URI for byte offset and resume *)
                  let attempt_resume upload_url =
                    let resume_headers = [
                      ("Content-Range", "bytes */" ^ string_of_int (String.length video_data));
                    ] in
                    Config.Http.put ~headers:resume_headers ~body:"" upload_url
                      (fun resume_response ->
                        if resume_response.status = 308 then
                          (* Server reports partial upload via Range header *)
                          let received =
                            match List.assoc_opt "range" resume_response.headers with
                            | Some range_hdr ->
                                (try
                                   let dash = String.index range_hdr '-' in
                                   int_of_string (String.sub range_hdr (dash + 1) (String.length range_hdr - dash - 1)) + 1
                                 with _ -> 0)
                            | None -> 0
                          in
                          let total = String.length video_data in
                          let remaining = total - received in
                          let remaining_data = String.sub video_data received remaining in
                          let resume_upload_headers = [
                            ("Content-Type", content_type);
                            ("Content-Length", string_of_int remaining);
                            ("Content-Range", Printf.sprintf "bytes %d-%d/%d" received (total - 1) total);
                          ] in
                          Config.Http.put ~headers:resume_upload_headers ~body:remaining_data upload_url
                            parse_upload_response
                            (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err)))
                        else if resume_response.status >= 200 && resume_response.status < 300 then
                          (* Upload was already complete *)
                          parse_upload_response resume_response
                        else
                          on_result (Error_types.Failure (parse_api_error ~status_code:resume_response.status ~response_body:resume_response.body)))
                      (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err)))
                  in

                  Config.Http.post ~headers:init_headers ~body:metadata_str init_url
                    (fun init_response ->
                      if init_response.status >= 200 && init_response.status < 300 then
                        (* Get upload URL from Location header *)
                        match List.assoc_opt "location" init_response.headers with
                        | Some upload_url ->
                            (* Step 2: Upload video data to resumable URL *)
                            let upload_headers = [
                              ("Content-Type", content_type);
                              ("Content-Length", string_of_int (String.length video_data));
                            ] in

                            Config.Http.put ~headers:upload_headers ~body:video_data upload_url
                              (fun upload_response ->
                                if upload_response.status >= 200 && upload_response.status < 300 then
                                  parse_upload_response upload_response
                                else if upload_response.status >= 500 || upload_response.status = 408 then
                                  (* Server error or timeout - attempt resumable recovery *)
                                  attempt_resume upload_url
                                else
                                  on_result (Error_types.Failure (parse_api_error ~status_code:upload_response.status ~response_body:upload_response.body)))
                              (fun _err ->
                                (* Network error during upload - attempt resumable recovery *)
                                attempt_resume upload_url)
                        | None ->
                            on_result (Error_types.Failure (Error_types.Internal_error (Printf.sprintf "No upload URL in response: %s" init_response.body)))
                      else
                        on_result (Error_types.Failure (parse_api_error ~status_code:init_response.status ~response_body:init_response.body)))
                    (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err))))
                else
                  on_result (Error_types.Failure (Error_types.Internal_error (Printf.sprintf "Failed to download video (%d)" video_response.status))))
              (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err))))
          (fun err -> on_result (Error_types.Failure err))
  
  (** Post thread (YouTube doesn't support threads, posts only first item)
      
      @param validate_media_before_upload When true, validates video file size after 
             download but before upload. Practical limit: 2GB.
             Default: false
  *)
  let post_thread ~account_id ~texts ~media_urls_per_post ?(alt_texts_per_post=[]) ?(validate_media_before_upload=false) on_result =
    let _ = alt_texts_per_post in
    let media_counts = List.map List.length media_urls_per_post in
    match validate_thread ~texts ~media_counts () with
    | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        (* YouTube only supports single videos, so we post just the first *)
        let first_text = List.hd texts in
        let first_media = List.hd media_urls_per_post in
        let total_requested = List.length texts in
        post_single ~account_id ~text:first_text ~media_urls:first_media
          ~validate_media_before_upload
          (fun outcome ->
            match outcome with
            | Error_types.Success video_id ->
                let thread_result = {
                  Error_types.posted_ids = [video_id];
                  failed_at_index = None;
                  total_requested;
                } in
                if total_requested > 1 then
                  on_result (Error_types.Partial_success {
                    result = thread_result;
                    warnings = [Error_types.Generic_warning { 
                      code = "youtube_no_threads"; 
                      message = Printf.sprintf "YouTube does not support threads. Only first of %d items posted." total_requested;
                      recoverable = false 
                    }]
                  })
                else
                  on_result (Error_types.Success thread_result)
            | Error_types.Partial_success { result = video_id; warnings } ->
                let thread_result = {
                  Error_types.posted_ids = [video_id];
                  failed_at_index = None;
                  total_requested;
                } in
                on_result (Error_types.Partial_success { result = thread_result; warnings })
            | Error_types.Failure err ->
                on_result (Error_types.Failure err))
  
  (** OAuth authorization URL with PKCE *)
  let get_oauth_url ~redirect_uri ~state ~code_verifier on_success on_error =
    let client_id = Config.get_env "YOUTUBE_CLIENT_ID" |> Option.value ~default:"" in
    
    if client_id = "" then
      on_error "YouTube client ID not configured"
    else (
      (* Generate code_challenge from code_verifier using SHA256 *)
      let code_challenge = 
        let digest = Digestif.SHA256.digest_string code_verifier in
        let raw = Digestif.SHA256.to_raw_string digest in
        Base64.encode_string ~pad:false raw
        |> String.map (function '+' -> '-' | '/' -> '_' | c -> c)
      in
      
      let scopes = "https://www.googleapis.com/auth/youtube.upload https://www.googleapis.com/auth/youtube" in
      
      let params = [
        ("client_id", client_id);
        ("redirect_uri", redirect_uri);
        ("response_type", "code");
        ("scope", scopes);
        ("state", state);
        ("access_type", "offline"); (* Get refresh token *)
        ("prompt", "consent"); (* Force consent to get refresh token *)
        ("code_challenge", code_challenge);
        ("code_challenge_method", "S256");
      ] in
      
      let query = List.map (fun (k, v) ->
        Printf.sprintf "%s=%s" k (Uri.pct_encode v)
      ) params |> String.concat "&" in
      
      let url = OAuth.Metadata.authorization_endpoint ^ "?" ^ query in
      on_success url
    )
  
  (** Exchange OAuth code for access token with PKCE *)
  let exchange_code ~code ~redirect_uri ~code_verifier on_success on_error =
    let client_id = Config.get_env "YOUTUBE_CLIENT_ID" |> Option.value ~default:"" in
    let client_secret = Config.get_env "YOUTUBE_CLIENT_SECRET" |> Option.value ~default:"" in
    
    if client_id = "" || client_secret = "" then
      on_error "YouTube OAuth credentials not configured"
    else (
      let url = google_oauth_base ^ "/token" in
      let body = Printf.sprintf
        "grant_type=authorization_code&code=%s&redirect_uri=%s&client_id=%s&client_secret=%s&code_verifier=%s"
        (Uri.pct_encode code)
        (Uri.pct_encode redirect_uri)
        (Uri.pct_encode client_id)
        (Uri.pct_encode client_secret)
        (Uri.pct_encode code_verifier)
      in
      
      let headers = [
        ("Content-Type", "application/x-www-form-urlencoded");
      ] in
      
      Config.Http.post ~headers ~body url
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
              let expires_in = json |> member "expires_in" |> to_int in
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
                auth_type = Bearer;
              } in
              on_success credentials
            with e ->
              on_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "OAuth exchange failed (%d): %s" response.status response.body))
        on_error
    )
  
  (** Update video metadata via PUT /youtube/v3/videos

      YouTube API requirement: when updating snippet fields, both
      [snippet.title] and [snippet.categoryId] are mandatory. Omitting
      them causes the API to return a 400 error. Therefore [~title] and
      [~category_id] are required whenever any snippet field (title,
      description, tags) is provided.

      @param account_id Account to authenticate as
      @param video_id ID of the video to update
      @param title New title (required when updating any snippet field)
      @param description Optional new description
      @param tags Optional new tags list
      @param category_id Category ID (required when updating any snippet field, default "22")
      @param privacy_status Optional new privacy status
      @param on_result Continuation receiving api_result
  *)
  let update_video ~account_id ~video_id ?title ?description ?tags ?(category_id="22") ?privacy_status on_result =
    let normalized_video_id = String.trim video_id in
    if normalized_video_id = "" then
      on_result (Error (Error_types.Internal_error "video_id is required"))
    else
      ensure_valid_token ~account_id
        (fun access_token ->
          let has_snippet_fields = title <> None || description <> None || tags <> None in
          if has_snippet_fields && title = None then
            on_result (Error (Error_types.Internal_error "title is required when updating snippet fields (YouTube API requirement)"))
          else
          let snippet_fields =
            if not has_snippet_fields then []
            else
              [("title", `String (match title with Some t -> t | None -> ""))]
              @ [("categoryId", `String category_id)]
              @ (match description with Some d -> [("description", `String d)] | None -> [])
              @ (match tags with Some ts -> [("tags", `List (List.map (fun t -> `String t) ts))] | None -> [])
          in
          let status_fields =
            match privacy_status with Some ps -> [("privacyStatus", `String ps)] | None -> []
          in
          let parts = ref [] in
          let body_fields = ref [("id", `String normalized_video_id)] in
          if snippet_fields <> [] then begin
            parts := "snippet" :: !parts;
            body_fields := ("snippet", `Assoc snippet_fields) :: !body_fields
          end;
          if status_fields <> [] then begin
            parts := "status" :: !parts;
            body_fields := ("status", `Assoc status_fields) :: !body_fields
          end;
          let part_str = String.concat "," (List.rev !parts) in
          if part_str = "" then
            on_result (Error (Error_types.Internal_error "No fields to update"))
          else
            let query = Uri.encoded_of_query [("part", [part_str])] in
            let url = Printf.sprintf "%s/videos?%s" youtube_api_base query in
            let body = Yojson.Basic.to_string (`Assoc (List.rev !body_fields)) in
            let headers = [
              ("Authorization", "Bearer " ^ access_token);
              ("Content-Type", "application/json");
            ] in
            Config.Http.put ~headers ~body url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  on_result (Ok ())
                else
                  on_result (Error (parse_api_error ~status_code:response.status ~response_body:response.body)))
              (fun err -> on_result (Error (Error_types.Internal_error err))))
        (fun err -> on_result (Error err))

  (** List playlists for the authenticated user's channel

      @param account_id Account to authenticate as
      @param on_result Continuation receiving api_result with playlists JSON
  *)
  let list_playlists ~account_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let url = Printf.sprintf "%s/playlists?part=snippet,contentDetails&mine=true" youtube_api_base in
        let headers = [("Authorization", "Bearer " ^ access_token)] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              (try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok json)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse playlists response: %s" (Printexc.to_string e)))))
            else
              on_result (Error (parse_api_error ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  (** Create a new playlist

      @param account_id Account to authenticate as
      @param title Playlist title
      @param description Optional playlist description
      @param privacy_status Privacy status (default "private")
      @param on_result Continuation receiving api_result with playlist id
  *)
  let create_playlist ~account_id ~title ?(description="") ?(privacy_status="private") on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let url = Printf.sprintf "%s/playlists?part=snippet,status" youtube_api_base in
        let body = Yojson.Basic.to_string (`Assoc [
          ("snippet", `Assoc [
            ("title", `String title);
            ("description", `String description);
          ]);
          ("status", `Assoc [
            ("privacyStatus", `String privacy_status);
          ]);
        ]) in
        let headers = [
          ("Authorization", "Bearer " ^ access_token);
          ("Content-Type", "application/json");
        ] in
        Config.Http.post ~headers ~body url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              (try
                let open Yojson.Basic.Util in
                let json = Yojson.Basic.from_string response.body in
                let playlist_id = json |> member "id" |> to_string in
                on_result (Ok playlist_id)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse create playlist response: %s" (Printexc.to_string e)))))
            else
              on_result (Error (parse_api_error ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  (** Add a video to a playlist

      @param account_id Account to authenticate as
      @param playlist_id The playlist to add to
      @param video_id The video to add
      @param on_result Continuation receiving api_result with playlistItem id
  *)
  let add_to_playlist ~account_id ~playlist_id ~video_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let url = Printf.sprintf "%s/playlistItems?part=snippet" youtube_api_base in
        let body = Yojson.Basic.to_string (`Assoc [
          ("snippet", `Assoc [
            ("playlistId", `String playlist_id);
            ("resourceId", `Assoc [
              ("kind", `String "youtube#video");
              ("videoId", `String video_id);
            ]);
          ]);
        ]) in
        let headers = [
          ("Authorization", "Bearer " ^ access_token);
          ("Content-Type", "application/json");
        ] in
        Config.Http.post ~headers ~body url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              (try
                let open Yojson.Basic.Util in
                let json = Yojson.Basic.from_string response.body in
                let item_id = json |> member "id" |> to_string in
                on_result (Ok item_id)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse playlistItem response: %s" (Printexc.to_string e)))))
            else
              on_result (Error (parse_api_error ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  (** Remove a video from a playlist by deleting the playlistItem

      @param account_id Account to authenticate as
      @param playlist_item_id The playlistItem ID to remove (not the video ID)
      @param on_result Continuation receiving api_result
  *)
  let remove_from_playlist ~account_id ~playlist_item_id on_result =
    let normalized_id = String.trim playlist_item_id in
    if normalized_id = "" then
      on_result (Error (Error_types.Internal_error "playlist_item_id is required"))
    else
      ensure_valid_token ~account_id
        (fun access_token ->
          let url = Printf.sprintf "%s/playlistItems?id=%s" youtube_api_base (Uri.pct_encode normalized_id) in
          let headers = [("Authorization", "Bearer " ^ access_token)] in
          Config.Http.delete ~headers url
            (fun response ->
              if response.status >= 200 && response.status < 300 then
                on_result (Ok ())
              else
                on_result (Error (parse_api_error ~status_code:response.status ~response_body:response.body)))
            (fun err -> on_result (Error (Error_types.Internal_error err))))
        (fun err -> on_result (Error err))

  (** Validate content length *)
  let validate_content ~text =
    let len = String.length text in
    if len = 0 then
      Error "Text cannot be empty"
    else if len > 5000 then
      Error (Printf.sprintf "YouTube description should be under 5000 characters (current: %d)" len)
    else
      Ok ()
end
