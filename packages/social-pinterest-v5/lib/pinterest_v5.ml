(** Pinterest API v5 Provider

    This implementation supports Pinterest pin creation, board management,
    rate limit parsing, and automatic retry for transient failures.

    - OAuth 2.0 with Basic Auth
    - Long-lived access tokens (no defined expiration)
    - Requires boards for all pins
    - Uses image_url source type for image pins (no download-reupload)
    - Board creation and section management
    - Rate limit header parsing (X-RateLimit-Limit, Remaining, Reset)
    - Retry wrapper for 429 and 5xx errors
*)

open Social_core

(** OAuth 2.0 module for Pinterest API v5
    
    Pinterest uses OAuth 2.0 with Basic Authentication for token exchange.
    
    Key characteristics:
    - No PKCE support
    - Token exchange requires Basic Auth (client_id:client_secret in header)
    - Access tokens last 30 days by default
    - Refresh tokens last 365 days (can refresh indefinitely)
    - Scopes use colon-separated format (e.g., boards:read, pins:write)
    
    Required environment variables (or pass directly to functions):
    - PINTEREST_CLIENT_ID: OAuth 2.0 App ID from Pinterest Developer Portal
    - PINTEREST_CLIENT_SECRET: OAuth 2.0 App Secret
    - PINTEREST_REDIRECT_URI: Registered callback URL
*)
module OAuth = struct
  (** Scope definitions for Pinterest API v5 *)
  module Scopes = struct
    (** Scopes for reading Pinterest data *)
    let read = [
      "user_accounts:read";
      "boards:read";
      "pins:read";
    ]
    
    (** Scopes required for creating pins *)
    let write = [
      "user_accounts:read";
      "boards:read";
      "pins:read";
      "pins:write";
    ]
    
    (** All commonly used scopes for Pinterest management *)
    let all = [
      "user_accounts:read";
      "boards:read";
      "boards:write";
      "pins:read";
      "pins:write";
      "ads:read";
      "ads:write";
    ]
    
    (** Operations that can be performed with Pinterest API *)
    type operation = 
      | Create_pin
      | Read_pins
      | Read_boards
      | Write_boards
      | Manage_ads
    
    (** Get scopes required for specific operations *)
    let for_operations ops =
      let base = ["user_accounts:read"] in
      let needs_pins_write = List.exists (fun o -> o = Create_pin) ops in
      let needs_pins_read = List.exists (fun o -> o = Read_pins) ops in
      let needs_boards_read = List.exists (fun o -> o = Read_boards || o = Create_pin) ops in
      let needs_boards_write = List.exists (fun o -> o = Write_boards) ops in
      let needs_ads = List.exists (fun o -> o = Manage_ads) ops in
      base @
      (if needs_boards_read then ["boards:read"] else []) @
      (if needs_boards_write then ["boards:write"] else []) @
      (if needs_pins_read || needs_pins_write then ["pins:read"] else []) @
      (if needs_pins_write then ["pins:write"] else []) @
      (if needs_ads then ["ads:read"; "ads:write"] else [])
  end
  
  (** Platform metadata for Pinterest OAuth *)
  module Metadata = struct
    (** Pinterest does NOT support PKCE *)
    let supports_pkce = false
    
    (** Pinterest supports token refresh *)
    let supports_refresh = true
    
    (** Access tokens last 30 days *)
    let access_token_seconds = Some 2592000
    
    (** Refresh tokens last 365 days *)
    let refresh_token_seconds = Some 31536000
    
    (** Recommended buffer before expiry (7 days) *)
    let refresh_buffer_seconds = 604800
    
    (** Maximum retry attempts for token operations *)
    let max_refresh_attempts = 5
    
    (** Pinterest OAuth authorization endpoint *)
    let authorization_endpoint = "https://www.pinterest.com/oauth"
    
    (** Pinterest OAuth token endpoint *)
    let token_endpoint = "https://api.pinterest.com/v5/oauth/token"
    
    (** Pinterest API base URL *)
    let api_base = "https://api.pinterest.com/v5"
  end
  
  (** Generate authorization URL for Pinterest OAuth 2.0 flow
      
      @param client_id Pinterest App ID
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
    ] in
    let query = List.map (fun (k, v) -> 
      Printf.sprintf "%s=%s" k (Uri.pct_encode v)
    ) params |> String.concat "&" in
    Printf.sprintf "%s?%s" Metadata.authorization_endpoint query
  
  (** Make functor for OAuth operations that need HTTP client *)
  module Make (Http : HTTP_CLIENT) = struct
    (** Helper to create Basic Auth header
        
        Pinterest requires credentials in Basic Auth format for token operations.
    *)
    let make_basic_auth ~client_id ~client_secret =
      let auth_string = String.trim client_id ^ ":" ^ String.trim client_secret in
      "Basic " ^ Base64.encode_exn auth_string
    
    (** Exchange authorization code for access token
        
        Note: Pinterest uses Basic Authentication - client credentials go
        in the Authorization header, not in the request body.
        
        @param client_id Pinterest App ID
        @param client_secret Pinterest App Secret
        @param redirect_uri Registered callback URL
        @param code Authorization code from callback
        @param on_success Continuation receiving credentials
        @param on_error Continuation receiving error message
    *)
    let exchange_code ~client_id ~client_secret ~redirect_uri ~code on_success on_error =
      let body = Printf.sprintf
        "grant_type=authorization_code&code=%s&redirect_uri=%s"
        (Uri.pct_encode code)
        (Uri.pct_encode redirect_uri)
      in
      let headers = [
        ("Content-Type", "application/x-www-form-urlencoded");
        ("Authorization", make_basic_auth ~client_id ~client_secret);
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
                with _ -> 2592000 (* Default to 30 days *)
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
                refresh_token;
                expires_at;
                token_type;
              } in
              on_success creds
            with e ->
              on_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "Token exchange failed (%d): %s" response.status response.body))
        on_error
    
    (** Refresh access token
        
        Pinterest access tokens last 30 days, refresh tokens 365 days.
        You can refresh indefinitely as long as the refresh token hasn't expired.
        
        @param client_id Pinterest App ID
        @param client_secret Pinterest App Secret
        @param refresh_token The refresh token from initial auth
        @param on_success Continuation receiving refreshed credentials
        @param on_error Continuation receiving error message
    *)
    let refresh_token ~client_id ~client_secret ~refresh_token on_success on_error =
      let body = Printf.sprintf
        "grant_type=refresh_token&refresh_token=%s"
        (Uri.pct_encode refresh_token)
      in
      let headers = [
        ("Content-Type", "application/x-www-form-urlencoded");
        ("Authorization", make_basic_auth ~client_id ~client_secret);
      ] in
      
      Http.post ~headers ~body Metadata.token_endpoint
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let access_token = json |> member "access_token" |> to_string in
              let new_refresh_token = 
                try Some (json |> member "refresh_token" |> to_string)
                with _ -> Some refresh_token in  (* Keep old if not returned *)
              let expires_in = 
                try json |> member "expires_in" |> to_int
                with _ -> 2592000
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
                refresh_token = new_refresh_token;
                expires_at;
                token_type;
              } in
              on_success creds
            with e ->
              on_error (Printf.sprintf "Failed to parse refresh response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "Token refresh failed (%d): %s" response.status response.body))
        on_error
    
    (** Get user account info using access token
        
        @param access_token Valid access token
        @param on_result Continuation receiving api_result with user info as JSON
    *)
    let get_user_info ~access_token on_result =
      let url = Metadata.api_base ^ "/user_account" in
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
              on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse user info: %s" (Printexc.to_string e))))
          else
            on_result (Error (Error_types.Internal_error (Printf.sprintf "Get user info failed (%d): %s" response.status response.body))))
        (fun err -> on_result (Error (Error_types.Internal_error err)))
    
    (** Get user's boards
        
        @param access_token Valid access token
        @param on_result Continuation receiving api_result with boards list as JSON
    *)
    let get_boards ~access_token on_result =
      let url = Metadata.api_base ^ "/boards" in
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
              on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse boards: %s" (Printexc.to_string e))))
          else
            on_result (Error (Error_types.Internal_error (Printf.sprintf "Get boards failed (%d): %s" response.status response.body))))
        (fun err -> on_result (Error (Error_types.Internal_error err)))
  end
end

(** Configuration module type for Pinterest provider *)
module type CONFIG = sig
  module Http : HTTP_CLIENT
  
  val get_env : string -> string option
  val get_credentials : account_id:string -> (credentials -> unit) -> (string -> unit) -> unit
  val update_credentials : account_id:string -> credentials:credentials -> (unit -> unit) -> (string -> unit) -> unit
  val encrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val decrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val update_health_status : account_id:string -> status:string -> error_message:string option -> (unit -> unit) -> (string -> unit) -> unit
end

(** Make functor to create Pinterest provider with given configuration *)
module Make (Config : CONFIG) = struct
  let pinterest_api_base = "https://api.pinterest.com/v5"
  let pinterest_auth_url = "https://www.pinterest.com/oauth"
  let pinterest_token_url = "https://api.pinterest.com/v5/oauth/token"
  
  (** {1 Platform Constants} *)
  
  let max_description_length = 500
  let max_title_length = 100
  let max_video_size_bytes = 2 * 1024 * 1024 * 1024
  let non_video_content_type_error_prefix = "non_video_content_type:"
  
  (** {1 Validation Functions} *)
  
  (** Validate a single post's content *)
  let validate_post ~text ?(media_count=0) () =
    let errors = ref [] in
    
    (* Check description length *)
    let text_len = String.length text in
    if text_len > max_description_length then
      errors := Error_types.Text_too_long { length = text_len; max = max_description_length } :: !errors;
    
    (* Pinterest requires at least one image *)
    if media_count < 1 then
      errors := Error_types.Media_required :: !errors;
    
    if !errors = [] then Ok ()
    else Error (List.rev !errors)
  
  (** Validate thread content - Pinterest only posts first item *)
  let validate_thread ~texts ~media_counts () =
    if List.length texts = 0 then
      Error [Error_types.Thread_empty]
    else
      (* Only validate first post since Pinterest doesn't support threads *)
      let text = List.hd texts in
      let media_count = try List.hd media_counts with _ -> 0 in
      match validate_post ~text ~media_count () with
      | Error errs -> Error [Error_types.Thread_post_invalid { index = 0; errors = errs }]
      | Ok () -> Ok ()
  
  (** Validate media file for Pinterest
      
      Pinterest limits:
      - Images: 20MB (recommended under 10MB)
      - Videos: 2GB
  *)
  let validate_media_file ~(media : Platform_types.post_media) =
    match media.Platform_types.media_type with
    | Platform_types.Image ->
        let max_image_bytes = 20 * 1024 * 1024 in (* 20MB *)
        if media.file_size_bytes > max_image_bytes then
          Error [Error_types.Media_too_large { 
            size_bytes = media.file_size_bytes; 
            max_bytes = max_image_bytes 
          }]
        else
          Ok ()
    | Platform_types.Gif ->
        let max_gif_bytes = 20 * 1024 * 1024 in (* 20MB *)
        if media.file_size_bytes > max_gif_bytes then
          Error [Error_types.Media_too_large { 
            size_bytes = media.file_size_bytes; 
            max_bytes = max_gif_bytes 
          }]
        else
          Ok ()
    | Platform_types.Video ->
        if media.file_size_bytes > max_video_size_bytes then
          Error [Error_types.Media_too_large {
            size_bytes = media.file_size_bytes;
            max_bytes = max_video_size_bytes
          }]
        else
          Ok ()
  
  (** Parse API error response and return structured Error_types.error *)
  let parse_api_error ~status_code ~response_body =
    try
      let json = Yojson.Basic.from_string response_body in
      let open Yojson.Basic.Util in
      let error_msg = 
        try json |> member "message" |> to_string
        with _ -> response_body
      in
      
      (* Map common Pinterest errors *)
      if status_code = 401 then
        Error_types.Auth_error Error_types.Token_invalid
      else if status_code = 403 then
        Error_types.Auth_error (Error_types.Insufficient_permissions ["pins:write"])
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
          platform = Platform_types.Pinterest;
          raw_response = Some response_body;
          request_id = None;
        }
    with _ ->
      Error_types.Api_error {
        status_code;
        message = response_body;
        platform = Platform_types.Pinterest;
        raw_response = Some response_body;
        request_id = None;
      }

  (** {1 Rate Limit Header Parsing} *)

  (** Parsed rate limit information from response headers *)
  type rate_limit_info = {
    limit: int option;
    remaining: int option;
    reset: int option;
  }

  (** Parse rate limit headers from an HTTP response.

      Pinterest returns:
      - X-RateLimit-Limit: requests allowed per interval
      - X-RateLimit-Remaining: requests remaining
      - X-RateLimit-Reset: seconds until the limit resets
  *)
  let parse_rate_limit_headers (headers : (string * string) list) : rate_limit_info =
    let find_int name =
      let lower_name = String.lowercase_ascii name in
      List.find_map (fun (k, v) ->
        if String.lowercase_ascii k = lower_name then
          (try Some (int_of_string (String.trim v)) with _ -> None)
        else
          None
      ) headers
    in
    {
      limit = find_int "x-ratelimit-limit";
      remaining = find_int "x-ratelimit-remaining";
      reset = find_int "x-ratelimit-reset";
    }

  (** {1 Retry Logic} *)

  (** Retry a request-making function on transient failures (429 and 5xx).

      @param max_retries Maximum number of retry attempts (default: 3)
      @param initial_delay_seconds Initial delay before first retry (default: 1.0)
      @param make_request Function that issues the HTTP request and calls on_result
      @param on_result Final continuation receiving the outcome
  *)
  let with_retry ?(max_retries=3) ?(initial_delay_seconds=1.0) ~make_request on_result =
    let rec loop attempt delay =
      make_request (fun response ->
        if response.status = 429 || response.status >= 500 then begin
          if attempt < max_retries then begin
            let actual_delay =
              if response.status = 429 then
                let rl = parse_rate_limit_headers response.headers in
                match rl.reset with
                | Some secs when secs > 0 -> float_of_int secs
                | _ -> delay
              else
                delay
            in
            let _ = Unix.select [] [] [] actual_delay in
            loop (attempt + 1) (delay *. 2.0)
          end else
            on_result response
        end else
          on_result response)
    in
    loop 0 initial_delay_seconds

  let normalize_content_type content_type =
    let lowered = String.lowercase_ascii content_type in
    let without_params =
      try
        let idx = String.index lowered ';' in
        String.sub lowered 0 idx
      with Not_found -> lowered
    in
    String.trim without_params

  let token_needs_refresh (creds : credentials) =
    match creds.expires_at with
    | None -> false
    | Some expires_at_str ->
        (match Ptime.of_rfc3339 expires_at_str with
         | Ok (exp_time, _, _) ->
             let now = Ptime_clock.now () in
             let buffer = Ptime.Span.of_int_s OAuth.Metadata.refresh_buffer_seconds in
             (match Ptime.sub_span exp_time buffer with
              | Some threshold -> Ptime.is_earlier now ~than:threshold |> not
              | None -> true)
         | Error _ -> false)
  
  (** Ensure valid access token, refreshing when near expiry *)
  let ensure_valid_token ~account_id on_success on_error =
    Config.get_credentials ~account_id
      (fun creds ->
        if token_needs_refresh creds then
          match creds.refresh_token with
          | None -> on_error (Error_types.Auth_error Error_types.Token_expired)
          | Some refresh_tok ->
              let client_id = Config.get_env "PINTEREST_CLIENT_ID" |> Option.value ~default:"" in
              let client_secret = Config.get_env "PINTEREST_CLIENT_SECRET" |> Option.value ~default:"" in
              if client_id = "" || client_secret = "" then
                on_error (Error_types.Auth_error Error_types.Missing_credentials)
              else
                let module OAuthHttp = OAuth.Make(Config.Http) in
                OAuthHttp.refresh_token ~client_id ~client_secret ~refresh_token:refresh_tok
                  (fun new_creds ->
                    Config.update_credentials ~account_id ~credentials:new_creds
                      (fun () ->
                        Config.update_health_status ~account_id ~status:"healthy" ~error_message:None
                          (fun () -> on_success new_creds.access_token)
                          (fun _ -> on_success new_creds.access_token))
                      (fun err ->
                        Config.update_health_status ~account_id ~status:"refresh_failed" ~error_message:(Some err)
                          (fun () -> on_error (Error_types.Auth_error (Error_types.Refresh_failed err)))
                          (fun _ -> on_error (Error_types.Auth_error (Error_types.Refresh_failed err)))))
                  (fun err ->
                    Config.update_health_status ~account_id ~status:"refresh_failed" ~error_message:(Some err)
                      (fun () -> on_error (Error_types.Auth_error (Error_types.Refresh_failed err)))
                      (fun _ -> on_error (Error_types.Auth_error (Error_types.Refresh_failed err))))
        else
          Config.update_health_status ~account_id ~status:"healthy" ~error_message:None
            (fun () -> on_success creds.access_token)
            (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err))))
      (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err)))

  type analytics_totals = {
    impression: int option;
    pin_click: int option;
    outbound_click: int option;
    save: int option;
  }

  type account_analytics = {
    start_date: string;
    end_date: string;
    totals: analytics_totals;
    raw_json: Yojson.Basic.t;
  }

  type pin_analytics = {
    pin_id: string;
    start_date: string;
    end_date: string;
    totals: analytics_totals;
    raw_json: Yojson.Basic.t;
  }

  let analytics_metric_names =
    [ "IMPRESSION"; "PIN_CLICK"; "OUTBOUND_CLICK"; "SAVE" ]

  let analytics_canonical_metric_keys =
    Analytics_normalization.canonical_metric_keys_of_provider_metrics
      ~provider:Analytics_normalization.Pinterest
      analytics_metric_names

  let int_of_json_opt json =
    match json with
    | `Int n -> Some n
    | `Float f -> Some (int_of_float f)
    | `String s -> (try Some (int_of_string s) with _ -> None)
    | _ -> None

  let first_item_opt json =
    match Yojson.Basic.Util.member "items" json with
    | `List (item :: _) -> Some item
    | _ -> None

  let extract_metric ~metric json =
    let open Yojson.Basic.Util in
    let lowered = String.lowercase_ascii metric in
    let candidates =
      json ::
      [ member "all" json;
        member "all_source_types" json;
        member "summary" json;
        member "summary_metrics" json ] @
      (match first_item_opt json with Some item -> [item] | None -> [])
    in
    let rec loop = function
      | [] -> None
      | candidate :: rest ->
          (match candidate with
           | `Assoc _ ->
               let metric_json =
                 match member metric candidate with
                 | `Null -> member lowered candidate
                 | value -> value
               in
               (match int_of_json_opt metric_json with
                | Some value -> Some value
                | None -> loop rest)
           | _ -> loop rest)
    in
    loop candidates

  let parse_analytics_totals json =
    {
      impression = extract_metric ~metric:"IMPRESSION" json;
      pin_click = extract_metric ~metric:"PIN_CLICK" json;
      outbound_click = extract_metric ~metric:"OUTBOUND_CLICK" json;
      save = extract_metric ~metric:"SAVE" json;
    }

  let to_canonical_optional_point value_opt =
    match value_opt with
    | Some value -> [ Analytics_types.make_datapoint value ]
    | None -> []

  let to_canonical_pinterest_series ?time_range ~scope ~provider_metric points =
    match Analytics_normalization.pinterest_metric_to_canonical provider_metric with
    | Some metric ->
        Some
          (Analytics_types.make_series
             ?time_range
             ~metric
             ~scope
             ~provider_metric
             points)
    | None -> None

  let to_canonical_analytics_totals_series ?time_range ~scope totals =
    [ ("IMPRESSION", to_canonical_optional_point totals.impression);
      ("PIN_CLICK", to_canonical_optional_point totals.pin_click);
      ("OUTBOUND_CLICK", to_canonical_optional_point totals.outbound_click);
      ("SAVE", to_canonical_optional_point totals.save) ]
    |> List.filter_map (fun (provider_metric, points) ->
           to_canonical_pinterest_series
             ?time_range
             ~scope
             ~provider_metric
             points)

  let to_canonical_account_analytics_series
      (account_analytics : account_analytics) =
    let time_range =
      {
        Analytics_types.since = Some account_analytics.start_date;
        until_ = Some account_analytics.end_date;
        granularity = Some Analytics_types.Day;
      }
    in
    to_canonical_analytics_totals_series
      ~time_range
      ~scope:Analytics_types.Account
      account_analytics.totals

  let to_canonical_pin_analytics_series (pin_analytics : pin_analytics) =
    let time_range =
      {
        Analytics_types.since = Some pin_analytics.start_date;
        until_ = Some pin_analytics.end_date;
        granularity = Some Analytics_types.Day;
      }
    in
    to_canonical_analytics_totals_series
      ~time_range
      ~scope:Analytics_types.Pin
      pin_analytics.totals

  let get_account_analytics ~account_id ~start_date ~end_date on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let query =
          Uri.encoded_of_query [
            ("start_date", [start_date]);
            ("end_date", [end_date]);
          ]
        in
        let url = Printf.sprintf "%s/user_account/analytics?%s" pinterest_api_base query in
        let headers = [
          ("Authorization", "Bearer " ^ access_token);
        ] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let parsed : account_analytics = {
                  start_date;
                  end_date;
                  totals = parse_analytics_totals json;
                  raw_json = json;
                } in
                on_result (Ok parsed)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse account analytics: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  let get_account_analytics_canonical ~account_id ~start_date ~end_date on_result =
    get_account_analytics ~account_id ~start_date ~end_date
      (function
        | Ok analytics ->
            on_result (Ok (to_canonical_account_analytics_series analytics))
        | Error err -> on_result (Error err))

  let get_pin_analytics ~account_id ~pin_id ~start_date ~end_date on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let query =
          Uri.encoded_of_query [
            ("start_date", [start_date]);
            ("end_date", [end_date]);
            ("metric_types", [String.concat "," analytics_metric_names]);
          ]
        in
        let url = Printf.sprintf "%s/pins/%s/analytics?%s" pinterest_api_base (Uri.pct_encode pin_id) query in
        let headers = [
          ("Authorization", "Bearer " ^ access_token);
        ] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let parsed : pin_analytics = {
                  pin_id;
                  start_date;
                  end_date;
                  totals = parse_analytics_totals json;
                  raw_json = json;
                } in
                on_result (Ok parsed)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse pin analytics: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  let get_pin_analytics_canonical ~account_id ~pin_id ~start_date ~end_date on_result =
    get_pin_analytics ~account_id ~pin_id ~start_date ~end_date
      (function
        | Ok analytics -> on_result (Ok (to_canonical_pin_analytics_series analytics))
        | Error err -> on_result (Error err))
  
  (** Get user's default board *)
  let get_default_board ~access_token on_success on_error =
    let url = pinterest_api_base ^ "/boards" in
    let headers = [
      ("Authorization", "Bearer " ^ access_token);
    ] in
    
    Config.Http.get ~headers url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let open Yojson.Basic.Util in
            let json = Yojson.Basic.from_string response.body in
            let boards = json |> member "items" |> to_list in
            match boards with
            | [] -> on_error "No Pinterest boards found - please create a board first"
            | first_board :: _ ->
                let board_id = first_board |> member "id" |> to_string in
                on_success board_id
          with e ->
            on_error (Printf.sprintf "Failed to parse boards: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "Failed to get boards (%d): %s" response.status response.body))
      on_error
  
  (** {1 Board Management} *)

  (** Create a new board.

      @param access_token Valid access token
      @param name Board name (required)
      @param description Optional board description
      @param privacy Optional privacy setting: "PUBLIC" (default) or "SECRET"
      @param on_result Continuation receiving api_result with created board JSON
  *)
  let create_board ~access_token ~name ?(description="") ?(privacy="PUBLIC") on_result =
    let url = pinterest_api_base ^ "/boards" in
    let fields = [
      ("name", `String name);
      ("privacy", `String privacy);
    ] in
    let fields =
      if description <> "" then ("description", `String description) :: fields
      else fields
    in
    let headers = [
      ("Authorization", "Bearer " ^ access_token);
      ("Content-Type", "application/json");
    ] in
    let body = Yojson.Basic.to_string (`Assoc fields) in
    Config.Http.post ~headers ~body url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            on_result (Ok json)
          with e ->
            on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse board response: %s" (Printexc.to_string e))))
        else
          on_result (Error (parse_api_error ~status_code:response.status ~response_body:response.body)))
      (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err))))

  (** {1 Board Section Management} *)

  (** Get sections for a board.

      @param access_token Valid access token
      @param board_id The board identifier
      @param on_result Continuation receiving api_result with sections list JSON
  *)
  let get_board_sections ~access_token ~board_id on_result =
    let url = Printf.sprintf "%s/boards/%s/sections" pinterest_api_base (Uri.pct_encode board_id) in
    let headers = [
      ("Authorization", "Bearer " ^ access_token);
    ] in
    Config.Http.get ~headers url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            on_result (Ok json)
          with e ->
            on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse board sections: %s" (Printexc.to_string e))))
        else
          on_result (Error (parse_api_error ~status_code:response.status ~response_body:response.body)))
      (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err))))

  (** Create a new section in a board.

      @param access_token Valid access token
      @param board_id The board identifier
      @param name Section name (required)
      @param on_result Continuation receiving api_result with created section JSON
  *)
  let create_board_section ~access_token ~board_id ~name on_result =
    let url = Printf.sprintf "%s/boards/%s/sections" pinterest_api_base (Uri.pct_encode board_id) in
    let headers = [
      ("Authorization", "Bearer " ^ access_token);
      ("Content-Type", "application/json");
    ] in
    let body = Yojson.Basic.to_string (`Assoc [("name", `String name)]) in
    Config.Http.post ~headers ~body url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            on_result (Ok json)
          with e ->
            on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse board section response: %s" (Printexc.to_string e))))
        else
          on_result (Error (parse_api_error ~status_code:response.status ~response_body:response.body)))
      (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err))))

  (** Upload image to Pinterest with optional alt text *)
  let upload_image ~access_token ~image_url ~alt_text ?(validate_before_upload=false) on_success on_error =
    (* Download image first *)
    Config.Http.get ~headers:[] image_url
      (fun image_response ->
        if image_response.status >= 200 && image_response.status < 300 then
          let content_type =
            List.assoc_opt "content-type" image_response.headers
            |> Option.value ~default:"application/octet-stream"
            |> normalize_content_type
          in
          if not (String.starts_with ~prefix:"image/" content_type) then
            on_error (Printf.sprintf "Unsupported media content-type: %s" content_type)
          else
          let file_size = String.length image_response.body in
          
          (* Validate if requested *)
          let validation_result =
            if validate_before_upload then
              let media : Platform_types.post_media = {
                media_type = Platform_types.Image;
                mime_type = content_type;
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
          | Error errs -> on_error (Printf.sprintf "Validation failed: %s" (String.concat ", " (List.map Error_types.validation_error_to_string errs)))
          | Ok () ->
          let url = pinterest_api_base ^ "/media" in
          
          (* Create multipart form data *)
          let parts = [{
            name = "file";
            filename = Some "image.jpg";
            content_type = Some content_type;
            content = image_response.body;
          }] in
          
          let headers = [
            ("Authorization", "Bearer " ^ access_token);
          ] in
          
          Config.Http.post_multipart ~headers ~parts url
            (fun response ->
              if response.status >= 200 && response.status < 300 then
                try
                  let open Yojson.Basic.Util in
                  let json = Yojson.Basic.from_string response.body in
                  let media_id = json |> member "media_id" |> to_string in
                  on_success (media_id, alt_text)
                with e ->
                  on_error (Printf.sprintf "Failed to parse media response: %s" (Printexc.to_string e))
              else
                on_error (Printf.sprintf "Media upload error (%d): %s" response.status response.body))
            on_error)
        else
          on_error (Printf.sprintf "Failed to download image (%d)" image_response.status))
      on_error

  type registered_media_upload = {
    media_id: string;
    upload_url: string;
    upload_parameters: (string * string) list;
  }

  let register_media_upload ~access_token ~media_type on_success on_error =
    let url = pinterest_api_base ^ "/media" in
    let headers = [
      ("Authorization", "Bearer " ^ access_token);
      ("Content-Type", "application/json");
    ] in
    let body = Yojson.Basic.to_string (`Assoc [ ("media_type", `String media_type) ]) in
    Config.Http.post ~headers ~body url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let open Yojson.Basic.Util in
            let json = Yojson.Basic.from_string response.body in
            let media_id = json |> member "media_id" |> to_string in
            let upload_url = json |> member "upload_url" |> to_string in
            let upload_parameters =
              try
                json
                |> member "upload_parameters"
                |> to_assoc
                |> List.map (fun (k, v) ->
                  let value = try to_string v with _ -> Yojson.Basic.to_string v in
                  (k, value)
                )
              with _ -> []
            in
            on_success { media_id; upload_url; upload_parameters }
          with e ->
            on_error (Printf.sprintf "Failed to parse media registration response: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "Media registration error (%d): %s" response.status response.body))
      on_error

  let upload_registered_media ~upload_url ~upload_parameters ~media_content ~content_type on_success on_error =
    let parameter_parts = List.map (fun (name, value) ->
      { name; filename = None; content_type = None; content = value }
    ) upload_parameters in
    let file_part = {
      name = "file";
      filename = Some "media";
      content_type = Some content_type;
      content = media_content;
    } in
    let parts = parameter_parts @ [file_part] in
    Config.Http.post_multipart ~headers:[] ~parts upload_url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          on_success ()
        else
          on_error (Printf.sprintf "Media upload failed (%d): %s" response.status response.body))
      on_error

  let get_media_upload_status ~access_token ~media_id on_success on_error =
    let url = pinterest_api_base ^ "/media/" ^ media_id in
    let headers = [ ("Authorization", "Bearer " ^ access_token) ] in
    Config.Http.get ~headers url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let open Yojson.Basic.Util in
            let json = Yojson.Basic.from_string response.body in
            let status = json |> member "status" |> to_string |> String.lowercase_ascii in
            on_success status
          with e ->
            on_error (Printf.sprintf "Failed to parse media status response: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "Media status check failed (%d): %s" response.status response.body))
      on_error

  let rec wait_for_media_ready ~access_token ~media_id ~attempts_left ?(delay_seconds=1.0) on_success on_error =
    if attempts_left <= 0 then
      on_error "Media processing timed out"
    else
      get_media_upload_status ~access_token ~media_id
        (fun status ->
          match status with
          | "succeeded" -> on_success ()
          | "failed" -> on_error "Media processing failed"
          | "processing" | "registered" ->
              let _ = Unix.select [] [] [] delay_seconds in
              let next_delay = min 5.0 (delay_seconds *. 1.5) in
              wait_for_media_ready ~access_token ~media_id ~attempts_left:(attempts_left - 1) ~delay_seconds:next_delay on_success on_error
          | other -> on_error (Printf.sprintf "Unexpected media status: %s" other))
        on_error

  let resolve_thumbnail_url thumbnail_url on_result =
    match thumbnail_url with
    | None -> on_result None
    | Some url when String.length url = 0 -> on_result None
    | Some url ->
        Config.Http.get ~headers:[] url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              let content_type =
                List.assoc_opt "content-type" response.headers
                |> Option.value ~default:"application/octet-stream"
                |> normalize_content_type
              in
              if String.starts_with ~prefix:"image/" content_type then
                on_result (Some url)
              else
                on_result None
            else
              on_result None)
          (fun _ -> on_result None)

  let upload_video ~access_token ~video_url ?thumbnail_url ?(validate_before_upload=false) on_success on_error =
    Config.Http.get ~headers:[] video_url
      (fun video_response ->
        if video_response.status >= 200 && video_response.status < 300 then
          let content_type =
            List.assoc_opt "content-type" video_response.headers
            |> Option.value ~default:"application/octet-stream"
            |> normalize_content_type
          in
          if not (String.starts_with ~prefix:"video/" content_type) then
            on_error (non_video_content_type_error_prefix ^ content_type)
          else
            let file_size = String.length video_response.body in
            let validation_result =
              if validate_before_upload then
                let media : Platform_types.post_media = {
                  media_type = Platform_types.Video;
                  mime_type = content_type;
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
             | Error errs -> on_error (Printf.sprintf "Validation failed: %s" (String.concat ", " (List.map Error_types.validation_error_to_string errs)))
             | Ok () ->
                 register_media_upload ~access_token ~media_type:"video"
                   (fun upload ->
                     upload_registered_media
                       ~upload_url:upload.upload_url
                        ~upload_parameters:upload.upload_parameters
                       ~media_content:video_response.body
                       ~content_type
                       (fun () ->
                         wait_for_media_ready ~access_token ~media_id:upload.media_id ~attempts_left:30
                           (fun () ->
                             resolve_thumbnail_url thumbnail_url
                               (fun resolved_thumbnail_url ->
                                 on_success (upload.media_id, resolved_thumbnail_url)))
                           on_error)
                       on_error)
                    on_error)
        else
          on_error (Printf.sprintf "Failed to download video (%d)" video_response.status))
      on_error
  
  (** Post to Pinterest
      
      @param validate_media_before_upload When true, validates image file size after 
             download but before upload. Pinterest limit: 20MB.
             Default: false
  *)
  let string_ends_with ~suffix s =
    let s_len = String.length s in
    let suffix_len = String.length suffix in
    s_len >= suffix_len && String.sub s (s_len - suffix_len) suffix_len = suffix

  let strip_url_suffixes url =
    let stop_at ch default =
      try String.index url ch with Not_found -> default
    in
    let stop_q = stop_at '?' (String.length url) in
    let stop_h = stop_at '#' (String.length url) in
    let stop = min stop_q stop_h in
    String.sub url 0 stop

  let is_likely_video_url url =
    let path = strip_url_suffixes url |> String.lowercase_ascii in
    let video_exts = [".mp4"; ".mov"; ".webm"; ".m4v"; ".avi"; ".mkv"] in
    List.exists (fun ext -> string_ends_with ~suffix:ext path) video_exts

  let is_likely_image_url url =
    let path = strip_url_suffixes url |> String.lowercase_ascii in
    let image_exts = [".jpg"; ".jpeg"; ".png"; ".gif"; ".webp"] in
    List.exists (fun ext -> string_ends_with ~suffix:ext path) image_exts

  let is_image_content_type_error msg =
    let lowered = String.lowercase_ascii msg in
    String.starts_with ~prefix:non_video_content_type_error_prefix lowered
    && String.starts_with
         ~prefix:"image/"
         (String.sub lowered (String.length non_video_content_type_error_prefix)
            (String.length lowered - String.length non_video_content_type_error_prefix))

  let format_video_upload_error msg =
    let lowered = String.lowercase_ascii msg in
    if String.starts_with ~prefix:non_video_content_type_error_prefix lowered then
      let prefix_len = String.length non_video_content_type_error_prefix in
      let content_type =
        String.sub lowered prefix_len (String.length lowered - prefix_len)
      in
      Printf.sprintf "Unsupported media content-type: %s" content_type
    else
      msg

  let post_single ~account_id ~text ~media_urls ?(alt_texts=[]) ?(board_id="") ?(section_id="") ?(title="") ?(link="") ?(validate_media_before_upload=false) on_result =
    let media_count = List.length media_urls in
    match validate_post ~text ~media_count () with
    | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        (* Validation ensures media_urls is non-empty *)
        let first_media_url = List.hd media_urls in
        let alt_text = try List.nth alt_texts 0 with _ -> None in
        ensure_valid_token ~account_id
          (fun access_token ->
            let continue_with_board resolved_board_id =
                let pin_title =
                  if title <> "" then title
                  else String.sub text 0 (min (String.length text) max_title_length)
                in
                let create_pin_with_media ~media_source ~alt_text_opt =
                  let url = pinterest_api_base ^ "/pins" in
                  let base_fields = [
                    ("board_id", `String resolved_board_id);
                    ("title", `String pin_title);
                    ("description", `String text);
                    ("media_source", media_source);
                  ] in
                  let base_fields =
                    if section_id <> "" then ("board_section_id", `String section_id) :: base_fields
                    else base_fields
                  in
                  let base_fields =
                    if link <> "" then ("link", `String link) :: base_fields
                    else base_fields
                  in
                  let pin_fields = match alt_text_opt with
                    | Some alt when String.length alt > 0 -> ("alt_text", `String alt) :: base_fields
                    | _ -> base_fields
                  in
                  let headers = [
                    ("Authorization", "Bearer " ^ access_token);
                    ("Content-Type", "application/json");
                  ] in
                  let body = Yojson.Basic.to_string (`Assoc pin_fields) in
                  Config.Http.post ~headers ~body url
                    (fun response ->
                      if response.status >= 200 && response.status < 300 then
                        try
                          let open Yojson.Basic.Util in
                          let json = Yojson.Basic.from_string response.body in
                          let pin_id = json |> member "id" |> to_string in
                          on_result (Error_types.Success pin_id)
                        with e ->
                          on_result (Error_types.Failure (Error_types.Internal_error (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e))))
                      else
                        on_result (Error_types.Failure (parse_api_error ~status_code:response.status ~response_body:response.body)))
                    (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err)))
                in
                let post_as_image ~image_url =
                  create_pin_with_media
                    ~media_source:(`Assoc [
                      ("source_type", `String "image_url");
                      ("url", `String image_url);
                    ])
                    ~alt_text_opt:alt_text
                in
                let post_as_video ~video_url ?thumbnail_url ?(on_video_error=None) () =
                  let handle_video_error err =
                    match on_video_error with
                    | Some f -> f err
                    | None ->
                        on_result (Error_types.Failure (Error_types.Internal_error (format_video_upload_error err)))
                  in
                  upload_video ~access_token ~video_url ?thumbnail_url
                    ~validate_before_upload:validate_media_before_upload
                    (fun (media_id, thumbnail_opt) ->
                      let video_media_source =
                        match thumbnail_opt with
                        | Some thumb when String.length thumb > 0 ->
                            `Assoc [
                              ("source_type", `String "video_id");
                              ("media_id", `String media_id);
                              ("cover_image_url", `String thumb);
                            ]
                        | _ ->
                            `Assoc [
                              ("source_type", `String "video_id");
                              ("media_id", `String media_id);
                            ]
                      in
                      create_pin_with_media
                        ~media_source:video_media_source
                        ~alt_text_opt:None)
                    handle_video_error
                in
                if is_likely_video_url first_media_url then
                  let thumbnail_url =
                    if List.length media_urls > 1 then
                      let candidate = List.nth media_urls 1 in
                      if is_likely_image_url candidate then Some candidate else None
                    else
                      None
                  in
                  post_as_video ~video_url:first_media_url ?thumbnail_url ()
                else if is_likely_image_url first_media_url then
                  post_as_image ~image_url:first_media_url
                else
                  let thumbnail_url =
                    if List.length media_urls > 1 then
                      let candidate = List.nth media_urls 1 in
                      if is_likely_image_url candidate then Some candidate else None
                    else
                      None
                  in
                  post_as_video ~video_url:first_media_url
                    ?thumbnail_url
                    ~on_video_error:(Some (fun err ->
                      if is_image_content_type_error err then
                        post_as_image ~image_url:first_media_url
                      else
                        on_result (Error_types.Failure (Error_types.Internal_error (format_video_upload_error err)))))
                    ()
            in
            if board_id <> "" then
              continue_with_board board_id
            else
              get_default_board ~access_token
                (fun resolved_board_id -> continue_with_board resolved_board_id)
                (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err))))
          (fun err -> on_result (Error_types.Failure err))
  
  (** Post thread (Pinterest doesn't support threads, posts only first item)
      
      @param validate_media_before_upload When true, validates image file size after 
             download but before upload. Pinterest limit: 20MB.
             Default: false
  *)
  let post_thread ~account_id ~texts ~media_urls_per_post ?(alt_texts_per_post=[]) ?(validate_media_before_upload=false) on_result =
    let media_counts = List.map List.length media_urls_per_post in
    match validate_thread ~texts ~media_counts () with
    | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        (* Pinterest only supports single pins, so we post just the first *)
        let first_text = List.hd texts in
        let first_media = List.hd media_urls_per_post in
        let first_alt_texts = try List.hd alt_texts_per_post with _ -> [] in
        let total_requested = List.length texts in
        post_single ~account_id ~text:first_text ~media_urls:first_media ~alt_texts:first_alt_texts
          ~validate_media_before_upload
          (fun outcome ->
            match outcome with
            | Error_types.Success pin_id ->
                let thread_result = {
                  Error_types.posted_ids = [pin_id];
                  failed_at_index = None;
                  total_requested;
                } in
                if total_requested > 1 then
                  (* Partial success - only first item posted *)
                  on_result (Error_types.Partial_success {
                    result = thread_result;
                    warnings = [Error_types.Generic_warning { 
                      code = "pinterest_no_threads"; 
                      message = Printf.sprintf "Pinterest does not support threads. Only first of %d items posted." total_requested;
                      recoverable = false 
                    }]
                  })
                else
                  on_result (Error_types.Success thread_result)
            | Error_types.Partial_success { result = pin_id; warnings } ->
                let thread_result = {
                  Error_types.posted_ids = [pin_id];
                  failed_at_index = None;
                  total_requested;
                } in
                on_result (Error_types.Partial_success { result = thread_result; warnings })
            | Error_types.Failure err ->
                on_result (Error_types.Failure err))
  
  (** OAuth authorization URL *)
  let get_oauth_url ~redirect_uri ~state on_success on_error =
    let client_id = Config.get_env "PINTEREST_CLIENT_ID" |> Option.value ~default:"" in
    
    if client_id = "" then
      on_error "Pinterest client ID not configured"
    else (
      let scopes = "boards:read,pins:read,pins:write,user_accounts:read" in
      let params = [
        ("client_id", client_id);
        ("redirect_uri", redirect_uri);
        ("response_type", "code");
        ("scope", scopes);
        ("state", state);
      ] in
      
      let query = List.map (fun (k, v) -> 
        Printf.sprintf "%s=%s" k (Uri.pct_encode v)
      ) params |> String.concat "&" in
      
      let url = pinterest_auth_url ^ "?" ^ query in
      on_success url
    )
  
  (** Exchange OAuth code for access token *)
  let exchange_code ~code ~redirect_uri on_success on_error =
    let client_id = Config.get_env "PINTEREST_CLIENT_ID" |> Option.value ~default:"" in
    let client_secret = Config.get_env "PINTEREST_CLIENT_SECRET" |> Option.value ~default:"" in
    
    if client_id = "" || client_secret = "" then
      on_error "Pinterest OAuth credentials not configured"
    else (
      let url = pinterest_token_url in
      
      (* Pinterest requires Basic Auth: credentials in header *)
      let auth_string = String.trim client_id ^ ":" ^ String.trim client_secret in
      let auth_b64 = Base64.encode_exn auth_string in
      
      (* Body contains grant_type, code, and redirect_uri *)
      let body = Printf.sprintf
        "grant_type=authorization_code&code=%s&redirect_uri=%s"
        (Uri.pct_encode code)
        (Uri.pct_encode redirect_uri)
      in
      
      let headers = [
        ("Content-Type", "application/x-www-form-urlencoded");
        ("Authorization", "Basic " ^ auth_b64);
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
              let expires_in =
                try json |> member "expires_in" |> to_int
                with _ -> 2592000
              in
              let granted_scope_raw =
                try Some (json |> member "scope" |> to_string)
                with _ -> None
              in
              let required_scopes = [
                "boards:read";
                "pins:read";
                "pins:write";
                "user_accounts:read";
              ] in
              let missing_scopes =
                match granted_scope_raw with
                | None -> required_scopes
                | Some scope_str ->
                    let granted_scopes =
                      scope_str
                      |> String.map (fun c -> if c = ',' then ' ' else c)
                      |> String.split_on_char ' '
                      |> List.map String.trim
                      |> List.filter (fun s -> String.length s > 0)
                    in
                    List.filter (fun required -> not (List.mem required granted_scopes)) required_scopes
              in
              if missing_scopes <> [] then
                on_error (Printf.sprintf
                  "Missing required Pinterest OAuth scopes: %s"
                  (String.concat ", " missing_scopes))
              else
              let expires_at =
                let now = Ptime_clock.now () in
                match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
                | Some exp -> Some (Ptime.to_rfc3339 exp)
                | None -> None
              in
              let credentials = {
                access_token;
                refresh_token;
                expires_at;
                token_type = "Bearer";
              } in
              on_success credentials
            with e ->
              on_error (Printf.sprintf "Failed to parse OAuth response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "OAuth exchange failed (%d): %s" response.status response.body))
        on_error
    )
  
  (** Validate content length *)
  let validate_content ~text =
    let len = String.length text in
    if len = 0 then
      Error "Text cannot be empty"
    else if len > 500 then
      Error (Printf.sprintf "Pinterest description should be under 500 characters (current: %d)" len)
    else
      Ok ()
end
