(** Twitter API v2 Provider
    
    This implementation supports OAuth 2.0 with refresh tokens.
    Tokens expire after 2 hours and need to be refreshed.
*)

open Social_core

(** OAuth 2.0 module for Twitter/X
    
    Twitter uses standard OAuth 2.0 with PKCE (Proof Key for Code Exchange).
    Access tokens expire after 2 hours and can be refreshed using refresh tokens.
    
    Required environment variables (or pass directly to functions):
    - TWITTER_CLIENT_ID: OAuth 2.0 Client ID from Twitter Developer Portal
    - TWITTER_CLIENT_SECRET: OAuth 2.0 Client Secret
    - TWITTER_REDIRECT_URI: Registered callback URL
*)
module OAuth = struct
  (** Redact sensitive token-like fields from strings used in errors/logging. *)
  let redact_sensitive_text text =
    let rules = [
      (Str.regexp "\"access_token\"[ \t\r\n]*:[ \t\r\n]*\"[^\"]*\"", "\"access_token\":\"[REDACTED]\"");
      (Str.regexp "\"refresh_token\"[ \t\r\n]*:[ \t\r\n]*\"[^\"]*\"", "\"refresh_token\":\"[REDACTED]\"");
      (Str.regexp {|Bearer[ 	]+[A-Za-z0-9._~+/=-]+|}, "Bearer [REDACTED]");
      (Str.regexp {|Basic[ 	]+[A-Za-z0-9._~+/=-]+|}, "Basic [REDACTED]");
    ] in
    List.fold_left (fun acc (re, repl) -> Str.global_replace re repl acc) text rules

  (** Scope definitions for Twitter API v2 *)
  module Scopes = struct
    (** Scopes required for read-only operations *)
    let read = ["tweet.read"; "users.read"]
    
    (** Scopes required for posting content with media (includes offline.access for refresh tokens)
        
        Per X API v2 OAuth 2.0 docs, media.write scope is required to upload media:
        https://developer.x.com/en/docs/authentication/oauth-2-0/authorization-code
    *)
    let write = ["tweet.read"; "tweet.write"; "users.read"; "offline.access"; "media.write"]
    
    (** All available Twitter OAuth 2.0 scopes *)
    let all = [
      "tweet.read"; "tweet.write"; "tweet.moderate.write";
      "users.read"; "follows.read"; "follows.write";
      "offline.access"; "space.read"; "mute.read"; "mute.write";
      "like.read"; "like.write"; "list.read"; "list.write";
      "block.read"; "block.write"; "bookmark.read"; "bookmark.write";
      "media.write"
    ]
    
    (** Operations that can be performed with Twitter API *)
    type operation = 
      | Post_text
      | Post_media
      | Post_video
      | Read_profile
      | Read_posts
      | Delete_post
      | Manage_pages  (** Not applicable to Twitter *)
    
    (** Get scopes required for specific operations *)
    let for_operations ops =
      let base = ["users.read"; "offline.access"] in
      let post_text_scopes = ["tweet.read"; "tweet.write"] in
      let post_media_scopes = ["tweet.read"; "tweet.write"; "media.write"] in
      let read_scopes = ["tweet.read"] in
      (* Media upload requires media.write scope *)
      if List.exists (fun o -> o = Post_media || o = Post_video) ops
      then base @ post_media_scopes
      else if List.exists (fun o -> o = Post_text || o = Delete_post) ops
      then base @ post_text_scopes
      else if List.exists (fun o -> o = Read_profile || o = Read_posts) ops
      then base @ read_scopes
      else base
  end
  
  (** Platform metadata for Twitter OAuth *)
  module Metadata = struct
    (** Twitter supports PKCE (S256 method) *)
    let supports_pkce = true
    
    (** Twitter supports token refresh *)
    let supports_refresh = true
    
    (** Twitter access tokens expire after 2 hours (7200 seconds) *)
    let token_lifetime_seconds = Some 7200
    
    (** Recommended buffer before expiry to refresh (30 minutes) *)
    let refresh_buffer_seconds = 1800
    
    (** Maximum retry attempts for token refresh *)
    let max_refresh_attempts = 10
    
    (** Authorization endpoint *)
    let authorization_endpoint = "https://twitter.com/i/oauth2/authorize"
    
    (** Token endpoint *)
    let token_endpoint = "https://api.twitter.com/2/oauth2/token"
    
    (** Token revocation endpoint *)
    let revocation_endpoint = "https://api.twitter.com/2/oauth2/revoke"
  end
  
  (** PKCE helpers for OAuth 2.0 with Proof Key for Code Exchange *)
  module Pkce = struct
    (** Generate a cryptographically random code verifier (43-128 chars)
        
        The code verifier is a high-entropy random string using characters
        [A-Z] / [a-z] / [0-9] / "-" / "." / "_" / "~" per RFC 7636.
        
        @return A 64-character random code verifier string
    *)
    let generate_code_verifier () =
      (* Use 48 random bytes -> 64 base64url chars *)
      let random_bytes = Bytes.create 48 in
      for i = 0 to 47 do
        Bytes.set random_bytes i (Char.chr (Random.int 256))
      done;
      (* Base64-URL encode without padding *)
      Base64.encode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet (Bytes.to_string random_bytes)
    
    (** Generate code_challenge from code_verifier using SHA256 (S256 method)
        
        code_challenge = BASE64URL(SHA256(ASCII(code_verifier)))
        
        @param verifier The code_verifier string
        @return Base64-URL encoded SHA256 hash without padding
    *)
    let generate_code_challenge verifier =
      let hash = Digestif.SHA256.digest_string verifier in
      let raw_hash = Digestif.SHA256.to_raw_string hash in
      Base64.encode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet raw_hash
  end
  
  (** Generate authorization URL for Twitter OAuth 2.0 flow
      
      @param client_id OAuth 2.0 Client ID
      @param redirect_uri Registered callback URL
      @param state CSRF protection state parameter (should be stored and verified on callback)
      @param scopes OAuth scopes to request (defaults to Scopes.write)
      @param code_challenge PKCE code challenge (generate with Pkce.generate_code_challenge)
      @return Full authorization URL to redirect user to
  *)
  let get_authorization_url ~client_id ~redirect_uri ~state ?(scopes=Scopes.write) ~code_challenge () =
    let scope_str = String.concat " " scopes in
    let params = [
      ("response_type", "code");
      ("client_id", client_id);
      ("redirect_uri", redirect_uri);
      ("scope", scope_str);
      ("state", state);
      ("code_challenge", code_challenge);
      ("code_challenge_method", "S256");
    ] in
    let query = Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) params) in
    Printf.sprintf "%s?%s" Metadata.authorization_endpoint query
  
  (** Make functor for OAuth operations that need HTTP client
      
      This separates the pure functions (URL generation, scope selection) from
      functions that need to make HTTP requests (token exchange, refresh).
  *)
  module Make (Http : HTTP_CLIENT) = struct
    (** Exchange authorization code for access token
        
        @param client_id OAuth 2.0 Client ID
        @param client_secret OAuth 2.0 Client Secret
        @param redirect_uri Registered callback URL (must match authorization request)
        @param code Authorization code from callback
        @param code_verifier PKCE code verifier (the original, not the challenge)
        @param on_success Continuation receiving credentials
        @param on_error Continuation receiving error message
    *)
    let exchange_code ~client_id ~client_secret ~redirect_uri ~code ~code_verifier on_success on_error =
      let body = Uri.encoded_of_query [
        ("grant_type", ["authorization_code"]);
        ("code", [code]);
        ("redirect_uri", [redirect_uri]);
        ("code_verifier", [code_verifier]);
      ] in
      
      let auth = Base64.encode_exn (client_id ^ ":" ^ client_secret) in
      let headers = [
        ("Authorization", "Basic " ^ auth);
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
            on_error (Printf.sprintf "Token exchange failed (%d): %s" response.status (redact_sensitive_text response.body)))
        on_error
    
    (** Refresh access token using refresh token
        
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
        ("client_id", [client_id]);
      ] in
      
      let auth = Base64.encode_exn (client_id ^ ":" ^ client_secret) in
      let headers = [
        ("Authorization", "Basic " ^ auth);
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
            on_error (Printf.sprintf "Token refresh failed (%d): %s" response.status (redact_sensitive_text response.body)))
        on_error
    
    (** Revoke a token
        
        @param client_id OAuth 2.0 Client ID
        @param client_secret OAuth 2.0 Client Secret
        @param token The access token or refresh token to revoke
        @param on_success Continuation called on successful revocation
        @param on_error Continuation receiving error message
    *)
    let revoke_token ~client_id ~client_secret ~token on_success on_error =
      let body = Uri.encoded_of_query [
        ("token", [token]);
      ] in
      
      let auth = Base64.encode_exn (client_id ^ ":" ^ client_secret) in
      let headers = [
        ("Authorization", "Basic " ^ auth);
        ("Content-Type", "application/x-www-form-urlencoded");
      ] in
      
      Http.post ~headers ~body Metadata.revocation_endpoint
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            on_success ()
          else
            on_error (Printf.sprintf "Token revocation failed (%d): %s" response.status (redact_sensitive_text response.body)))
        on_error
  end
end

(** Configuration module type for Twitter provider *)
module type CONFIG = sig
  module Http : HTTP_CLIENT
  
  val get_env : string -> string option
  val get_credentials : account_id:string -> (credentials -> unit) -> (string -> unit) -> unit
  val update_credentials : account_id:string -> credentials:credentials -> (unit -> unit) -> (string -> unit) -> unit
  val encrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val decrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val update_health_status : account_id:string -> status:string -> error_message:string option -> (unit -> unit) -> (string -> unit) -> unit
end

(** Make functor to create Twitter provider with given configuration *)
module Make (Config : CONFIG) = struct
  let twitter_api_base = "https://api.twitter.com/2"
  (* X API v2 media upload - requires OAuth tokens from S256 PKCE flow *)
  let twitter_upload_base = "https://api.x.com/2"
  
  (** Rate limiting state - Twitter Free tier: 15 posts per 24 hours *)
  let last_post_time = ref None
  let posts_in_window = ref 0
  let window_start = ref (Ptime_clock.now ())
  
  (** Rate limit info from response headers *)
  type rate_limit_info = {
    limit: int;
    remaining: int;
    reset: int;
  }
  
  (** Pagination metadata *)
  type pagination_meta = {
    next_token: string option;
    previous_token: string option;
    result_count: int;
  }

  (** Account-level analytics derived from user public metrics. *)
  type account_analytics = {
    user_id: string;
    username: string option;
    name: string option;
    followers_count: int;
    following_count: int;
    tweet_count: int;
    listed_count: int;
  }

  type post_metric_set =
    | Public_metrics_only
    | Expanded_metrics
    | Expanded_metrics_with_fallback

  type post_extended_metrics = {
    like_count: int option;
    reply_count: int option;
    retweet_count: int option;
    quote_count: int option;
    bookmark_count: int option;
    impression_count: int option;
    url_link_clicks: int option;
    user_profile_clicks: int option;
    engagements: int option;
    video_view_count: int option;
  }

  (** Tweet-level analytics derived from tweet public metrics plus optional
      expanded metric objects when requested. *)
  type post_analytics = {
    tweet_id: string;
    text: string option;
    retweet_count: int;
    reply_count: int;
    like_count: int;
    quote_count: int;
    bookmark_count: int;
    impression_count: int;
    non_public_metrics: post_extended_metrics option;
    organic_metrics: post_extended_metrics option;
    promoted_metrics: post_extended_metrics option;
  }

  let tweet_fields_for_post_metric_set = function
    | Public_metrics_only -> "text,public_metrics"
    | Expanded_metrics
    | Expanded_metrics_with_fallback ->
        "text,public_metrics,non_public_metrics,organic_metrics,promoted_metrics"

  let metric_set_allows_fallback = function
    | Expanded_metrics_with_fallback -> true
    | Public_metrics_only
    | Expanded_metrics -> false

  let to_canonical_optional_point value_opt =
    match value_opt with
    | Some value -> [ Analytics_types.make_datapoint value ]
    | None -> []

  let to_canonical_x_series ?time_range ~scope ~provider_metric points =
    if points = [] then
      None
    else
      match Analytics_normalization.x_metric_to_canonical provider_metric with
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
    [ ("follower_count", to_canonical_optional_point (Some account_analytics.followers_count));
      ("following_count", to_canonical_optional_point (Some account_analytics.following_count));
      ("tweet_count", to_canonical_optional_point (Some account_analytics.tweet_count));
      ("listed_count", to_canonical_optional_point (Some account_analytics.listed_count)) ]
    |> List.filter_map (fun (provider_metric, points) ->
            to_canonical_x_series
              ~scope:Analytics_types.Profile
              ~provider_metric
              points)

  let first_some_metric groups get_metric =
    groups
    |> List.find_map (fun group_opt ->
           match group_opt with
           | None -> None
           | Some group -> get_metric group)

  let to_canonical_post_analytics_series (post_analytics : post_analytics) =
    let expanded_groups =
      [ post_analytics.non_public_metrics;
        post_analytics.organic_metrics;
        post_analytics.promoted_metrics ]
    in
    [ ("retweet_count", to_canonical_optional_point (Some post_analytics.retweet_count));
      ("reply_count", to_canonical_optional_point (Some post_analytics.reply_count));
      ("like_count", to_canonical_optional_point (Some post_analytics.like_count));
      ("quote_count", to_canonical_optional_point (Some post_analytics.quote_count));
      ("bookmark_count", to_canonical_optional_point (Some post_analytics.bookmark_count));
      ("impression_count", to_canonical_optional_point (Some post_analytics.impression_count));
      ( "url_link_clicks",
        to_canonical_optional_point
          (first_some_metric expanded_groups (fun metric -> metric.url_link_clicks)) );
      ( "user_profile_clicks",
        to_canonical_optional_point
          (first_some_metric expanded_groups (fun metric -> metric.user_profile_clicks)) );
      ( "engagements",
        to_canonical_optional_point
          (first_some_metric expanded_groups (fun metric -> metric.engagements)) );
      ( "video_view_count",
        to_canonical_optional_point
          (first_some_metric expanded_groups (fun metric -> metric.video_view_count)) ) ]
    |> List.filter_map (fun (provider_metric, points) ->
           to_canonical_x_series
              ~scope:Analytics_types.Post
              ~provider_metric
              points)
  
  (** Parse rate limit headers from Twitter API response *)
  let parse_rate_limit_headers headers =
    try
      let limit = List.assoc_opt "x-rate-limit-limit" headers
        |> Option.map int_of_string
        |> Option.value ~default:0 in
      let remaining = List.assoc_opt "x-rate-limit-remaining" headers
        |> Option.map int_of_string
        |> Option.value ~default:0 in
      let reset = List.assoc_opt "x-rate-limit-reset" headers
        |> Option.map int_of_string
        |> Option.value ~default:0 in
      Some { limit; remaining; reset }
    with _ -> None
  
  (** Parse pagination metadata from response JSON *)
  let parse_pagination_meta json =
    try
      let open Yojson.Basic.Util in
      let meta = json |> member "meta" in
      let next_token = try Some (meta |> member "next_token" |> to_string) with _ -> None in
      let previous_token = try Some (meta |> member "previous_token" |> to_string) with _ -> None in
      let result_count = try meta |> member "result_count" |> to_int with _ -> 0 in
      { next_token; previous_token; result_count }
    with _ ->
      { next_token = None; previous_token = None; result_count = 0 }

  let json_value_to_int = function
    | `Int i -> Some i
    | `String s -> int_of_string_opt (String.trim s)
    | `Float f -> Some (int_of_float f)
    | _ -> None

  let read_json_int_field_opt ~obj ~field =
    let open Yojson.Basic.Util in
    try
      let value = obj |> member field in
      if value = `Null then None
      else json_value_to_int value
    with _ -> None

  let read_json_int_field ~obj ~field ~default =
    let open Yojson.Basic.Util in
    try
      let value = obj |> member field in
      if value = `Null then default
      else
        match json_value_to_int value with
        | Some i -> i
        | None -> default
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

  let parse_account_analytics_response response_body =
    let open Yojson.Basic.Util in
    try
      let json = Yojson.Basic.from_string response_body in
      let data = json |> member "data" in
      if data = `Null then
        Error (`Not_found "account")
      else
        let public_metrics = data |> member "public_metrics" in
        Ok {
          user_id = data |> member "id" |> to_string;
          username = read_json_string_field_opt ~obj:data ~field:"username";
          name = read_json_string_field_opt ~obj:data ~field:"name";
          followers_count = read_json_int_field ~obj:public_metrics ~field:"followers_count" ~default:0;
          following_count = read_json_int_field ~obj:public_metrics ~field:"following_count" ~default:0;
          tweet_count = read_json_int_field ~obj:public_metrics ~field:"tweet_count" ~default:0;
          listed_count = read_json_int_field ~obj:public_metrics ~field:"listed_count" ~default:0;
        }
    with e ->
      Error (`Malformed (Printf.sprintf "Failed to parse account analytics response: %s" (Printexc.to_string e)))

  let parse_post_extended_metrics metrics_obj =
    if metrics_obj = `Null then
      None
    else
      let metrics =
        {
          like_count = read_json_int_field_opt ~obj:metrics_obj ~field:"like_count";
          reply_count = read_json_int_field_opt ~obj:metrics_obj ~field:"reply_count";
          retweet_count = read_json_int_field_opt ~obj:metrics_obj ~field:"retweet_count";
          quote_count = read_json_int_field_opt ~obj:metrics_obj ~field:"quote_count";
          bookmark_count = read_json_int_field_opt ~obj:metrics_obj ~field:"bookmark_count";
          impression_count = read_json_int_field_opt ~obj:metrics_obj ~field:"impression_count";
          url_link_clicks = read_json_int_field_opt ~obj:metrics_obj ~field:"url_link_clicks";
          user_profile_clicks = read_json_int_field_opt ~obj:metrics_obj ~field:"user_profile_clicks";
          engagements = read_json_int_field_opt ~obj:metrics_obj ~field:"engagements";
          video_view_count = read_json_int_field_opt ~obj:metrics_obj ~field:"video_view_count";
        }
      in
      let has_values =
        [ metrics.like_count;
          metrics.reply_count;
          metrics.retweet_count;
          metrics.quote_count;
          metrics.bookmark_count;
          metrics.impression_count;
          metrics.url_link_clicks;
          metrics.user_profile_clicks;
          metrics.engagements;
          metrics.video_view_count ]
        |> List.exists Option.is_some
      in
      if has_values then Some metrics else None

  let parse_post_analytics_response ~tweet_id response_body =
    let open Yojson.Basic.Util in
    try
      let json = Yojson.Basic.from_string response_body in
      let data = json |> member "data" in
      if data = `Null then
        Error (`Not_found tweet_id)
      else
        let public_metrics = data |> member "public_metrics" in
        let non_public_metrics = data |> member "non_public_metrics" |> parse_post_extended_metrics in
        let organic_metrics = data |> member "organic_metrics" |> parse_post_extended_metrics in
        let promoted_metrics = data |> member "promoted_metrics" |> parse_post_extended_metrics in
        Ok {
          tweet_id =
            (read_json_string_field_opt ~obj:data ~field:"id"
             |> Option.value ~default:tweet_id);
          text = read_json_string_field_opt ~obj:data ~field:"text";
          retweet_count = read_json_int_field ~obj:public_metrics ~field:"retweet_count" ~default:0;
          reply_count = read_json_int_field ~obj:public_metrics ~field:"reply_count" ~default:0;
          like_count = read_json_int_field ~obj:public_metrics ~field:"like_count" ~default:0;
          quote_count = read_json_int_field ~obj:public_metrics ~field:"quote_count" ~default:0;
          bookmark_count = read_json_int_field ~obj:public_metrics ~field:"bookmark_count" ~default:0;
          impression_count = read_json_int_field ~obj:public_metrics ~field:"impression_count" ~default:0;
          non_public_metrics;
          organic_metrics;
          promoted_metrics;
        }
    with e ->
      Error (`Malformed (Printf.sprintf "Failed to parse post analytics response: %s" (Printexc.to_string e)))

  let add_optional_string_field key value_opt fields =
    match value_opt with
    | Some value when String.trim value <> "" -> (key, `String value) :: fields
    | _ -> fields
  
  let check_rate_limit () =
    let now = Ptime_clock.now () in
    let window_duration = Ptime.Span.of_int_s (24 * 3600) in
    
    (match Ptime.sub_span now window_duration with
     | Some window_ago when Ptime.is_later !window_start ~than:window_ago -> ()
     | _ ->
         window_start := now;
         posts_in_window := 0);
    
    if !posts_in_window >= 15 then
      Error "Twitter rate limit reached: Maximum 15 posts per 24 hours on Free tier"
    else
      Ok ()
  
  let record_post () =
    posts_in_window := !posts_in_window + 1;
    last_post_time := Some (Ptime_clock.now ())
  
  (** {1 Platform Constants} *)
  
  (** Maximum tweet length (Twitter counts characters specially - see Twitter_char_counter) *)
  let max_tweet_length = 280

  (** Maximum tweet length for Premium/X Premium subscribers (4000 characters) *)
  let max_tweet_length_premium = 4000
  
  (** Maximum number of images per tweet *)
  let max_images = 4
  
  (** Maximum video size in bytes (512MB for Twitter Blue, 512MB for non-Blue with some limits) *)
  let max_video_size_bytes = 512 * 1024 * 1024
  
  (** Maximum video duration in seconds (140s for most users, 10 min for Blue) *)
  let max_video_duration_seconds = 140
  
  (** {1 Validation} *)
  
  (** Validate a single tweet's content.

      @param premium When true, allows up to 4000 characters (X Premium limit).
                     Default: false (standard 280-character limit).
  *)
  let validate_post ~text ?(media_count=0) ?(is_reply=false) ?(premium=false) () =
    let errors = ref [] in
    let char_count = Twitter_char_counter.count ~is_reply text in
    let limit = if premium then max_tweet_length_premium else max_tweet_length in
    if char_count > limit then
      errors := Error_types.Text_too_long { length = char_count; max = limit } :: !errors;
    if media_count > max_images then
      errors := Error_types.Too_many_media { count = media_count; max = max_images } :: !errors;
    if !errors = [] then Ok ()
    else Error (List.rev !errors)
  
  (** Validate a thread before posting *)
  let validate_thread ~texts ?(media_counts=[]) () =
    if texts = [] then
      Error [Error_types.Thread_empty]
    else
      let errors = List.mapi (fun i text ->
        let media_count = try List.nth media_counts i with _ -> 0 in
        let is_reply = i > 0 in  (* Replies in thread don't count leading @mentions *)
        match validate_post ~text ~media_count ~is_reply () with
        | Ok () -> None
        | Error errs -> Some (Error_types.Thread_post_invalid { index = i; errors = errs })
      ) texts |> List.filter_map Fun.id in
      if errors = [] then Ok ()
      else Error errors
  
  (** Validate media constraints
      
      Exported for use by callers who want to pre-validate media before uploading.
  *)
  let validate_media ~(media : Platform_types.post_media) =
    match media.Platform_types.media_type with
    | Platform_types.Image ->
        (* Twitter's image size limits depend on format, but generally 5MB for images *)
        let max_image_bytes = 5 * 1024 * 1024 in
        if media.file_size_bytes > max_image_bytes then
          Error [Error_types.Media_too_large { 
            size_bytes = media.file_size_bytes; 
            max_bytes = max_image_bytes 
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
        (* GIFs on Twitter limited to 15MB *)
        let max_gif_bytes = 15 * 1024 * 1024 in
        if media.file_size_bytes > max_gif_bytes then
          Error [Error_types.Media_too_large { 
            size_bytes = media.file_size_bytes; 
            max_bytes = max_gif_bytes 
          }]
        else
          Ok ()
  
  (** Suppress warning for validate_media - exported for public use *)
  let _ = validate_media
  
  (** {1 Internal Helpers} *)
  
  (** Parse API error from response *)
  let parse_api_error ~status_code ~body ~headers =
    let contains_sub s sub =
      let s_l = String.lowercase_ascii s in
      let sub_l = String.lowercase_ascii sub in
      try
        ignore (Str.search_forward (Str.regexp_string sub_l) s_l 0);
        true
      with Not_found -> false
    in
    if status_code = 401 then
      Error_types.Auth_error Error_types.Token_invalid
    else if status_code = 403 then
      (* Check for specific Twitter 403 errors *)
      (try
        let json = Yojson.Basic.from_string body in
        let detail = json |> Yojson.Basic.Util.member "detail" |> Yojson.Basic.Util.to_string in
        let detail_lower = String.lowercase_ascii detail in
        if String.starts_with ~prefix:"forbidden" detail_lower then
          Error_types.Auth_error (Error_types.Insufficient_permissions ["tweet.write"])
        else
          Error_types.make_api_error
            ~platform:Platform_types.Twitter
            ~status_code
            ~message:detail
            ~raw_response:body ()
      with _ ->
        Error_types.Auth_error (Error_types.Insufficient_permissions ["tweet.write"]))
    else if status_code = 429 then
      let retry_after_seconds =
        let now_unix = int_of_float (Unix.time ()) in
        match List.assoc_opt "x-rate-limit-reset" headers with
        | Some reset_str ->
            (try
               let reset_unix = int_of_string reset_str in
               max 1 (reset_unix - now_unix)
             with _ -> 60)
        | None -> 60
      in
      Error_types.make_rate_limited ~retry_after_seconds ()
    else
      let message =
        try
          let json = Yojson.Basic.from_string body in
          let open Yojson.Basic.Util in
          let detail_opt = json |> member "detail" |> to_string_option in
          match detail_opt with
          | Some detail when detail <> "" -> detail
          | _ ->
              let errors =
                try json |> member "errors" |> to_list
                with _ -> []
              in
              (match errors with
               | err :: _ ->
                   let msg_opt = err |> member "message" |> to_string_option in
                   Option.value msg_opt ~default:"API error"
               | [] -> "API error")
        with _ -> "API error"
      in
      if status_code = 400 && contains_sub message "duplicate" then
        Error_types.Duplicate_content
      else
        Error_types.make_api_error
          ~platform:Platform_types.Twitter
          ~status_code
          ~message
          ~raw_response:body ()
  
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
    let url = "https://api.twitter.com/2/oauth2/token" in
    
    let body = Uri.encoded_of_query [
      ("grant_type", ["refresh_token"]);
      ("refresh_token", [refresh_token]);
      ("client_id", [client_id]);
    ] in
    
    let auth = Base64.encode_exn (client_id ^ ":" ^ client_secret) in
    let headers = [
      ("Authorization", "Basic " ^ auth);
      ("Content-Type", "application/x-www-form-urlencoded");
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
            let expires_in = json |> member "expires_in" |> to_int in
            let expires_at = 
              let now = Ptime_clock.now () in
              match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
              | Some exp -> Ptime.to_rfc3339 exp
              | None -> Ptime.to_rfc3339 now in
            on_success (new_access, new_refresh, expires_at)
          with e ->
            on_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "Token refresh failed (%d): %s" response.status (OAuth.redact_sensitive_text response.body)))
      on_error
  
  (** Ensure valid OAuth 2.0 access token, refreshing if needed *)
  let ensure_valid_token ~account_id on_success on_error =
    Config.get_credentials ~account_id
      (fun creds ->
        (* Check if token needs refresh (30 min buffer) *)
        if is_token_expired_buffer ~buffer_seconds:1800 creds.expires_at then
          (* Token expiring soon, refresh it *)
          match creds.refresh_token with
          | None ->
              Config.update_health_status ~account_id ~status:"token_expired" 
                ~error_message:(Some "No refresh token available")
                (fun () -> on_error (Error_types.Auth_error Error_types.Missing_credentials))
                (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err)))
          | Some refresh_token ->
               let client_id = Config.get_env "TWITTER_CLIENT_ID" |> Option.value ~default:"" in
               let client_secret = Config.get_env "TWITTER_CLIENT_SECRET" |> Option.value ~default:"" in
               if client_id = "" || client_secret = "" then
                 let err = "TWITTER_CLIENT_ID or TWITTER_CLIENT_SECRET is missing" in
                 Config.update_health_status ~account_id ~status:"refresh_failed"
                   ~error_message:(Some err)
                   (fun () -> on_error (Error_types.Auth_error (Error_types.Refresh_failed err)))
                   (fun err2 -> on_error (Error_types.Network_error (Error_types.Connection_failed err2)))
               else
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
                     Config.update_health_status ~account_id ~status:"refresh_failed" 
                       ~error_message:(Some err)
                       (fun () -> on_error (Error_types.Auth_error (Error_types.Refresh_failed err)))
                       (fun err2 -> on_error (Error_types.Network_error (Error_types.Connection_failed err2))))
        else
          (* Token still valid *)
          Config.update_health_status ~account_id ~status:"healthy" ~error_message:None
            (fun () -> on_success creds.access_token)
            (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err))))
      (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err)))
  
  (** Retry-on-401 wrapper.

      Performs [action] with the current access token.  If the HTTP response
      returns status 401 the wrapper refreshes the token once via
      {!ensure_valid_token} and retries the action with the new token.
      If the retry also returns 401, the error is returned without further
      retries.

      @param account_id   Account whose credentials should be refreshed
      @param action        [fun access_token on_done -> ...] where [on_done]
                           receives the HTTP response
      @param handle_response  Processes the HTTP response into the final result
      @param on_result     Final callback with the result
  *)
  let with_retry_on_401 ~account_id ~action ~handle_response on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        action access_token
          (fun response ->
            if response.Social_core.status = 401 then
              (* Force a token refresh by invalidating cache, then retry once *)
              ensure_valid_token ~account_id
                (fun new_access_token ->
                  action new_access_token
                    (fun retry_response ->
                      handle_response retry_response on_result)
                    (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
                (fun err -> on_result (Error err))
            else
              handle_response response on_result)
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Update media metadata with alt text using X API v2
      
      @param on_success Called with `true` if alt-text was set, `false` if it failed
  *)
  let update_media_metadata ~access_token ~media_id ~alt_text on_success =
    let url = Printf.sprintf "%s/media/metadata" twitter_upload_base in
    
    (* v2 format: id and metadata object *)
    let body_json = `Assoc [
      ("id", `String media_id);
      ("metadata", `Assoc [
        ("alt_text", `Assoc [
          ("text", `String alt_text)
        ])
      ]);
    ] in
    let body = Yojson.Basic.to_string body_json in
    
    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
      ("Content-Type", "application/json");
    ] in
    
    Config.Http.post ~headers ~body url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          on_success true  (* Alt-text set successfully *)
        else
          on_success false)  (* Alt-text failed, but don't block the upload *)
      (fun _err -> 
        on_success false)  (* Network error for alt-text, don't block *)

  type media_processing_status =
    | Media_processing_succeeded
    | Media_processing_pending of int option
    | Media_processing_failed of string

  let parse_media_processing_status json =
    let open Yojson.Basic.Util in
    let processing_info_in_data = json |> member "data" |> member "processing_info" in
    let processing_info =
      if processing_info_in_data = `Null then json |> member "processing_info"
      else processing_info_in_data
    in
    if processing_info = `Null then
      Media_processing_succeeded
    else
      let state =
        try processing_info |> member "state" |> to_string |> String.lowercase_ascii
        with _ -> "succeeded"
      in
      match state with
      | "succeeded" -> Media_processing_succeeded
      | "failed" ->
          let err_json = processing_info |> member "error" in
          let err_msg =
            if err_json = `Null then "Media processing failed"
            else
              let name = try err_json |> member "name" |> to_string with _ -> "" in
              let message = try err_json |> member "message" |> to_string with _ -> "" in
              let code =
                try string_of_int (err_json |> member "code" |> to_int)
                with _ -> ""
              in
              let parts = List.filter (fun s -> s <> "") [name; code; message] in
              if parts = [] then "Media processing failed" else String.concat " | " parts
          in
          Media_processing_failed err_msg
      | "pending" | "in_progress" ->
          let check_after_secs =
            try Some (processing_info |> member "check_after_secs" |> to_int)
            with _ -> None
          in
          Media_processing_pending check_after_secs
      | _ -> Media_processing_succeeded

  let get_media_processing_status ~access_token ~media_id on_success on_error =
    let query = Uri.encoded_of_query [
      ("command", ["STATUS"]);
      ("media_id", [media_id]);
    ] in
    let url = Printf.sprintf "%s/media/upload?%s" twitter_upload_base query in
    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
    ] in
    Config.Http.get ~headers url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            on_success (parse_media_processing_status json)
          with e ->
            on_error (Printf.sprintf "Failed to parse media STATUS response: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "Media STATUS check failed (%d): %s" response.status response.body))
      on_error

  let wait_for_media_processing ~access_token ~media_id ?(max_attempts=20) on_success on_error =
    let sleep_for_poll_delay seconds =
      if seconds > 0 then
        try
          ignore (Unix.select [] [] [] (float_of_int seconds))
        with _ -> ()
    in
    let rec poll attempt last_check_after_secs =
      if attempt >= max_attempts then
        let hint =
          match last_check_after_secs with
          | Some secs -> Printf.sprintf " Last check_after_secs=%d." secs
          | None -> ""
        in
        on_error (Printf.sprintf
          "Media processing timeout for media_id=%s after %d STATUS attempts.%s"
          media_id max_attempts hint)
      else
        get_media_processing_status ~access_token ~media_id
          (function
            | Media_processing_succeeded -> on_success ()
            | Media_processing_failed msg ->
                on_error (Printf.sprintf "Media processing failed for media_id=%s: %s" media_id msg)
            | Media_processing_pending check_after_secs ->
                sleep_for_poll_delay (Option.value ~default:1 check_after_secs);
                poll (attempt + 1) check_after_secs)
          on_error
    in
    poll 0 None

  let finalize_media_upload_result ~access_token ~media_id ~alt_text on_success =
    match alt_text with
    | Some alt when String.length alt > 0 ->
        update_media_metadata ~access_token ~media_id ~alt_text:alt
          (fun alt_text_succeeded -> on_success (media_id, not alt_text_succeeded))
    | _ -> on_success (media_id, false)

  let ensure_media_processing_ready ~access_token ~media_id ~initial_status ~alt_text on_success on_error =
    match initial_status with
    | Media_processing_succeeded ->
        finalize_media_upload_result ~access_token ~media_id ~alt_text on_success
    | Media_processing_failed msg ->
        on_error (Printf.sprintf "Media processing failed for media_id=%s: %s" media_id msg)
    | Media_processing_pending _ ->
        wait_for_media_processing ~access_token ~media_id
          (fun () -> finalize_media_upload_result ~access_token ~media_id ~alt_text on_success)
          on_error
  
  (** Upload media to X API v2 (requires S256 PKCE tokens)
      
      Uses the v2 endpoint at https://api.x.com/2/media/upload with
      application/json content type and base64-encoded media data,
      as specified in the official X API v2 documentation.
      
      Reference: https://docs.x.com/x-api/media/upload-media
      
      Note: 403 errors typically indicate:
      - Missing OAuth scopes (need tweet.write, users.read)
      - App permissions not set to "Read and Write" in Developer Portal
      - Quota exhaustion on Free tier
  *)
  let upload_media ~access_token ~media_data ~mime_type ?(alt_text=None) on_success on_error =
    let url = Printf.sprintf "%s/media/upload" twitter_upload_base in
    
    (* Determine media category based on MIME type *)
    let media_category = 
      if String.starts_with ~prefix:"video/" mime_type then "tweet_video" 
      else if mime_type = "image/gif" then "tweet_gif"
      else "tweet_image" in
    
    (* Base64 encode the media data per official X API v2 docs *)
    let media_base64 = Base64.encode_exn media_data in
    
    (* Build JSON body per official X API v2 documentation:
       {
         "media": "<base64-string>",
         "media_category": "tweet_image",
         "media_type": "image/png"
       }
    *)
    let body_json = `Assoc [
      ("media", `String media_base64);
      ("media_category", `String media_category);
      ("media_type", `String mime_type);
    ] in
    let body = Yojson.Basic.to_string body_json in
    
    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
      ("Content-Type", "application/json");
    ] in
    
    Config.Http.post ~headers ~body url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            (* X API v2 returns: { "data": { "id": "...", "media_key": "...", ... } }
               Per official docs: https://docs.x.com/x-api/media/upload-media *)
            let media_id = 
              try json |> Yojson.Basic.Util.member "data" |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_string
              with _ -> 
                (* Fallback for alternative response formats *)
                try json |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_string
                with _ -> json |> Yojson.Basic.Util.member "media_id_string" |> Yojson.Basic.Util.to_string
            in
            let processing_status = parse_media_processing_status json in
            ensure_media_processing_ready ~access_token ~media_id ~initial_status:processing_status ~alt_text
              on_success on_error
          with e ->
            on_error (Printf.sprintf "Failed to parse media response: %s\nBody: %s" (Printexc.to_string e) response.body)
        else if response.status = 403 then
          on_error (Printf.sprintf "Twitter media upload forbidden (403). Please disconnect and reconnect your Twitter account to refresh authentication. Details: %s" response.body)
        else
          on_error (Printf.sprintf "Media upload error (%d): %s" response.status response.body))
      on_error
  
  (** Chunked media upload for large files (videos) using X API v2

      This implements the 3-phase upload process using the dedicated v2 chunked
      upload endpoints (introduced April 2025, mandatory after June 2025):
      1. INIT - POST /2/media/upload/initialize with JSON body
      2. APPEND - POST /2/media/upload/{id}/append with multipart (media + segment_index)
      3. FINALIZE - POST /2/media/upload/{id}/finalize

      Reference: https://docs.x.com/x-api/media/quickstart/media-upload-chunked
  *)
  let upload_media_chunked ~access_token ~media_data ~mime_type ?(alt_text=None) () on_success on_error =
    let media_category =
      if String.starts_with ~prefix:"video/" mime_type then "tweet_video"
      else if mime_type = "image/gif" then "tweet_gif"
      else "tweet_image" in
    let total_bytes = String.length media_data in
    let auth_headers = [
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
    ] in

    (* Phase 1: INIT - POST /2/media/upload/initialize with JSON body *)
    let init_url = Printf.sprintf "%s/media/upload/initialize" twitter_upload_base in
    let init_body = Yojson.Basic.to_string (`Assoc [
      ("total_bytes", `Int total_bytes);
      ("media_type", `String mime_type);
      ("media_category", `String media_category);
    ]) in
    let init_headers = ("Content-Type", "application/json") :: auth_headers in

    Config.Http.post ~headers:init_headers ~body:init_body init_url
      (fun init_response ->
        if init_response.status >= 200 && init_response.status < 300 then
          try
            let init_json = Yojson.Basic.from_string init_response.body in
            (* v2 returns { "data": { "id": "...", "media_key": "...", "expires_after_secs": ... } } *)
            let media_id_string =
              try init_json |> Yojson.Basic.Util.member "data" |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_string
              with _ -> init_json |> Yojson.Basic.Util.member "media_id_string" |> Yojson.Basic.Util.to_string
            in

            (* Phase 2: APPEND chunks - POST /2/media/upload/{id}/append with multipart *)
            let chunk_size = 5 * 1024 * 1024 in (* 5MB chunks *)
            let rec upload_chunks offset segment_index =
              if offset >= total_bytes then begin
                (* Phase 3: FINALIZE - POST /2/media/upload/{id}/finalize *)
                let finalize_url = Printf.sprintf "%s/media/upload/%s/finalize" twitter_upload_base media_id_string in

                Config.Http.post ~headers:auth_headers finalize_url
                  (fun finalize_response ->
                    if finalize_response.status >= 200 && finalize_response.status < 300 then
                      let initial_status =
                        try
                          let finalize_json = Yojson.Basic.from_string finalize_response.body in
                          parse_media_processing_status finalize_json
                        with _ -> Media_processing_succeeded
                      in
                      ensure_media_processing_ready
                        ~access_token
                        ~media_id:media_id_string
                        ~initial_status
                        ~alt_text
                        on_success
                        on_error
                    else
                      on_error (Printf.sprintf "Failed to finalize upload (%d): %s"
                        finalize_response.status finalize_response.body))
                  on_error
              end else begin
                (* Upload next chunk *)
                let remaining = total_bytes - offset in
                let current_chunk_size = min chunk_size remaining in
                let chunk = String.sub media_data offset current_chunk_size in

                let append_url = Printf.sprintf "%s/media/upload/%s/append" twitter_upload_base media_id_string in
                let append_parts = [
                  { Social_core.name = "segment_index"; content = string_of_int segment_index; filename = None; content_type = None };
                  { name = "media"; content = chunk; filename = Some "chunk"; content_type = Some "application/octet-stream" };
                ] in

                Config.Http.post_multipart ~headers:auth_headers ~parts:append_parts append_url
                  (fun append_response ->
                    if append_response.status >= 200 && append_response.status < 300 then
                      upload_chunks (offset + current_chunk_size) (segment_index + 1)
                    else
                      on_error (Printf.sprintf "Failed to append chunk %d (%d): %s"
                        segment_index append_response.status append_response.body))
                  on_error
              end
            in
            upload_chunks 0 0
          with e ->
            on_error (Printf.sprintf "Failed to parse INIT response: %s" (Printexc.to_string e))
        else if init_response.status = 403 then
          on_error (Printf.sprintf "Twitter chunked upload forbidden (403). Common causes: 1) App needs 'Read and Write' permissions in Developer Portal, 2) OAuth token missing required scopes (tweet.write), 3) Free API tier doesn't support media upload (upgrade to Basic). Details: %s" init_response.body)
        else
          on_error (Printf.sprintf "Failed to initialize upload (%d): %s"
            init_response.status init_response.body))
      on_error

  let upload_media_with_mode ~access_token ~media_data ~mime_type ~alt_text on_success on_error =
    if String.starts_with ~prefix:"video/" mime_type then
      upload_media_chunked ~access_token ~media_data ~mime_type ~alt_text () on_success on_error
    else
      upload_media ~access_token ~media_data ~mime_type ~alt_text on_success on_error
  
  (** Post a single tweet with optional media
      
      @param account_id The account identifier
      @param text The tweet text (max 280 characters, URLs count as ~23)
      @param media_urls List of media URLs to attach (max 4 images, or 1 video/GIF)
      @param alt_texts Optional alt text for each media item (for accessibility)
      @param validate_media_before_upload When true, validates media size/format after 
             download but before upload to Twitter. Returns validation error if media 
             exceeds limits (512MB for video, 5MB for images, 15MB for GIFs).
             Default: false (for backward compatibility)
      @param on_result Callback receiving the outcome with tweet ID
  *)
  let post_single ~account_id ~text ~media_urls ?(alt_texts=[]) ?(validate_media_before_upload=false) ?reply_settings ?community_id ?(share_with_followers=false) on_result =
    (* Validate before making any API calls *)
    let media_count = List.length media_urls in
    match validate_post ~text ~media_count () with
    | Error errs ->
        on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        match check_rate_limit () with
        | Error _msg -> 
            on_result (Error_types.Failure (Error_types.make_rate_limited 
              ~retry_after_seconds:3600 ()))  (* Suggest retry after 1 hour *)
        | Ok () ->
            ensure_valid_token ~account_id
              (fun access_token ->
                let accumulated_warnings = ref [] in
                
                (* Helper to fetch media with retry logic *)
                let fetch_media_with_retry url max_retries on_success on_err =
                  let rec attempt retry_count =
                    Config.Http.get url
                      (fun media_resp ->
                        if media_resp.status >= 200 && media_resp.status < 300 then
                          on_success media_resp
                        else if retry_count < max_retries && 
                                (media_resp.status >= 500 || media_resp.status = 429) then
                          attempt (retry_count + 1)
                        else
                          on_err (Printf.sprintf "Failed to fetch media from %s (status: %d)" 
                            url media_resp.status))
                      (fun err ->
                        if retry_count < max_retries then
                          attempt (retry_count + 1)
                        else
                          on_err (Printf.sprintf "Failed to fetch media from %s: %s" url err))
                  in
                  attempt 0
                in
                
                (* Pair URLs with alt text - use None if alt text list is shorter *)
                let urls_with_alt = List.mapi (fun i url ->
                  let alt_text = try List.nth alt_texts i with _ -> None in
                  (url, alt_text)
                ) media_urls in
                
                (* Helper to determine media type from MIME type *)
                let media_type_of_mime mime_type =
                  if String.starts_with ~prefix:"video/" mime_type then Platform_types.Video
                  else if mime_type = "image/gif" then Platform_types.Gif
                  else Platform_types.Image
                in
                
                (* Helper to upload multiple media in sequence, accumulating warnings for alt text failures *)
                let rec upload_media_seq urls_with_alt acc on_complete on_err =
                  match urls_with_alt with
                  | [] -> on_complete (List.rev acc)
                  | (url, alt_text) :: rest ->
                      fetch_media_with_retry url 3
                        (fun media_resp ->
                          let mime_type = 
                            List.assoc_opt "content-type" media_resp.headers 
                            |> Option.value ~default:"image/jpeg"
                          in
                          let file_size = String.length media_resp.body in
                          
                          (* Validate media if requested *)
                          let validation_result =
                            if validate_media_before_upload then
                              let media : Platform_types.post_media = {
                                media_type = media_type_of_mime mime_type;
                                mime_type = mime_type;
                                file_size_bytes = file_size;
                                width = None;  (* Not available from download *)
                                height = None;
                                duration_seconds = None;  (* Would require parsing video *)
                                alt_text = alt_text;
                              } in
                              validate_media ~media
                            else
                              Ok ()
                          in
                          
                          match validation_result with
                          | Error errs ->
                              on_result (Error_types.Failure (Error_types.Validation_error errs))
                          | Ok () ->
                              (* Upload to Twitter - alt text failures become warnings, not errors *)
                              upload_media_with_mode ~access_token ~media_data:media_resp.body ~mime_type ~alt_text
                                (fun (media_id, alt_text_failed) -> 
                                  if alt_text_failed then
                                    accumulated_warnings := Error_types.Alt_text_failed media_id :: !accumulated_warnings;
                                  upload_media_seq rest (media_id :: acc) on_complete on_err)
                                on_err)
                        on_err
                in
                
                (* Upload media if provided (max 4) *)
                let media_to_upload = List.filteri (fun i _ -> i < 4) urls_with_alt in
                upload_media_seq media_to_upload []
                  (fun media_ids ->
                    (* Create tweet *)
                    let url = Printf.sprintf "%s/tweets" twitter_api_base in
                    
                    let base_fields =
                      [("text", `String text)]
                      |> add_optional_string_field "reply_settings" reply_settings
                      |> add_optional_string_field "community_id" community_id
                    in
                    (* When posting to a community, optionally share with followers *)
                    let base_fields = match community_id with
                      | Some _ when share_with_followers ->
                          ("share_with_followers", `Bool true) :: base_fields
                      | _ -> base_fields
                    in
                    let body_json =
                      if List.length media_ids > 0 then
                        `Assoc (("media", `Assoc [
                          ("media_ids", `List (List.map (fun id -> `String id) media_ids))
                        ]) :: base_fields)
                      else
                        `Assoc base_fields
                    in
                    let body = Yojson.Basic.to_string body_json in

                    let headers = [
                      ("Authorization", Printf.sprintf "Bearer %s" access_token);
                      ("Content-Type", "application/json");
                    ] in
                    
                    Config.Http.post ~headers ~body url
                      (fun response ->
                        if response.status >= 200 && response.status < 300 then
                          try
                            let json = Yojson.Basic.from_string response.body in
                            let tweet_id = json
                              |> Yojson.Basic.Util.member "data"
                              |> Yojson.Basic.Util.member "id"
                              |> Yojson.Basic.Util.to_string in
                            record_post ();
                            if !accumulated_warnings = [] then
                              on_result (Error_types.Success tweet_id)
                            else
                              on_result (Error_types.Partial_success { 
                                result = tweet_id; 
                                warnings = !accumulated_warnings 
                              })
                          with e ->
                            on_result (Error_types.Failure (Error_types.Internal_error 
                              (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e))))
                        else
                          on_result (Error_types.Failure (parse_api_error 
                            ~status_code:response.status ~body:response.body ~headers:response.headers)))
                      (fun err -> on_result (Error_types.Failure (Error_types.Network_error 
                        (Error_types.Connection_failed err)))))
                   (fun err -> on_result (Error_types.Failure (Error_types.Network_error 
                    (Error_types.Connection_failed err)))))
              (fun err -> on_result (Error_types.Failure err))
  
  (** Post single tweet with pre-uploaded media IDs
      
      This function is for when media has already been uploaded to Twitter
      and you have the media_ids. Use this when the backend handles media
      upload separately from posting.
      
      @param account_id The account identifier
      @param text The tweet text
      @param media_ids List of pre-uploaded media IDs
      @param on_result Callback receiving the outcome with tweet ID
  *)
  let post_single_with_media_ids ~account_id ~text ~media_ids on_result =
    let media_count = List.length media_ids in
    match validate_post ~text ~media_count () with
    | Error errs ->
        on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        match check_rate_limit () with
        | Error _ -> 
            on_result (Error_types.Failure (Error_types.make_rate_limited 
              ~retry_after_seconds:3600 ()))
        | Ok () ->
            ensure_valid_token ~account_id
              (fun access_token ->
                let url = Printf.sprintf "%s/tweets" twitter_api_base in
                
                let base_fields = [("text", `String text)] in
                let body_json = 
                  if List.length media_ids > 0 then
                    `Assoc (("media", `Assoc [
                      ("media_ids", `List (List.map (fun id -> `String id) media_ids))
                    ]) :: base_fields)
                  else
                    `Assoc base_fields
                in
                let body = Yojson.Basic.to_string body_json in
                
                let headers = [
                  ("Authorization", Printf.sprintf "Bearer %s" access_token);
                  ("Content-Type", "application/json");
                ] in
                
                Config.Http.post ~headers ~body url
                  (fun response ->
                    if response.status >= 200 && response.status < 300 then
                      try
                        let json = Yojson.Basic.from_string response.body in
                        let tweet_id = json
                          |> Yojson.Basic.Util.member "data"
                          |> Yojson.Basic.Util.member "id"
                          |> Yojson.Basic.Util.to_string in
                        record_post ();
                        on_result (Error_types.Success tweet_id)
                      with e ->
                        on_result (Error_types.Failure (Error_types.Internal_error 
                          (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e))))
                    else
                      on_result (Error_types.Failure (parse_api_error 
                        ~status_code:response.status ~body:response.body ~headers:response.headers)))
                   (fun err -> on_result (Error_types.Failure (Error_types.Network_error 
                    (Error_types.Connection_failed err)))))
              (fun err -> on_result (Error_types.Failure err))
  
  (** Post thread with pre-uploaded media IDs
      
      Each text gets paired with its corresponding media_ids list.
      The first tweet in the thread can have media, subsequent tweets
      are replies.
      
      @param account_id The account identifier
      @param texts List of tweet texts
      @param media_ids_per_post Media IDs for each post
      @param on_result Callback receiving the outcome with thread_result
  *)
  let post_thread_with_media_ids ~account_id ~texts ~media_ids_per_post on_result =
    let media_counts = List.map List.length media_ids_per_post in
    match validate_thread ~texts ~media_counts () with
    | Error errs ->
        on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        ensure_valid_token ~account_id
          (fun access_token ->
            let total_requested = List.length texts in
            
            (* Helper to post tweets in sequence with reply references *)
            let rec post_tweets_seq texts_remaining media_remaining reply_to_id acc_ids =
              match texts_remaining with
              | [] -> 
                  let result = { 
                    Error_types.posted_ids = List.rev acc_ids; 
                    failed_at_index = None;
                    total_requested;
                  } in
                  on_result (Error_types.Success result)
              | text :: rest_texts ->
                  let current_media = match media_remaining with
                    | media :: rest_media -> (media, rest_media)
                    | [] -> ([], [])
                  in
                  let (media_ids, next_media) = current_media in
                  
                  (* Create tweet *)
                  let url = Printf.sprintf "%s/tweets" twitter_api_base in
                  
                  let base_fields = [("text", `String text)] in
                  let base_with_reply = match reply_to_id with
                    | Some id -> ("reply", `Assoc [("in_reply_to_tweet_id", `String id)]) :: base_fields
                    | None -> base_fields
                  in
                  let body_json = 
                    if List.length media_ids > 0 then
                      `Assoc (("media", `Assoc [
                        ("media_ids", `List (List.map (fun id -> `String id) media_ids))
                      ]) :: base_with_reply)
                    else
                      `Assoc base_with_reply
                  in
                  let body = Yojson.Basic.to_string body_json in
                  
                  let headers = [
                    ("Authorization", Printf.sprintf "Bearer %s" access_token);
                    ("Content-Type", "application/json");
                  ] in
                  
                  Config.Http.post ~headers ~body url
                    (fun response ->
                      if response.status >= 200 && response.status < 300 then
                        try
                          let json = Yojson.Basic.from_string response.body in
                          let tweet_id = json
                            |> Yojson.Basic.Util.member "data"
                            |> Yojson.Basic.Util.member "id"
                            |> Yojson.Basic.Util.to_string in
                          record_post ();
                          post_tweets_seq rest_texts next_media (Some tweet_id) (tweet_id :: acc_ids)
                        with e ->
                          let result = { 
                            Error_types.posted_ids = List.rev acc_ids;
                            failed_at_index = Some (total_requested - List.length texts_remaining);
                            total_requested;
                          } in
                          if acc_ids = [] then
                            on_result (Error_types.Failure (Error_types.Internal_error 
                              (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e))))
                          else
                            on_result (Error_types.Partial_success { 
                              result; 
                              warnings = [Error_types.Enrichment_skipped 
                                (Printf.sprintf "Parse error: %s" (Printexc.to_string e))]
                            })
                      else
                        let result = { 
                          Error_types.posted_ids = List.rev acc_ids;
                          failed_at_index = Some (total_requested - List.length texts_remaining);
                          total_requested;
                        } in
                        let err = parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers in
                        if acc_ids = [] then
                          on_result (Error_types.Failure err)
                        else
                          on_result (Error_types.Partial_success { 
                            result; 
                            warnings = [Error_types.Enrichment_skipped (Error_types.error_to_string err)]
                          }))
                    (fun err ->
                      let result = { 
                        Error_types.posted_ids = List.rev acc_ids;
                        failed_at_index = Some (total_requested - List.length texts_remaining);
                        total_requested;
                      } in
                      if acc_ids = [] then
                        on_result (Error_types.Failure (Error_types.Network_error 
                          (Error_types.Connection_failed err)))
                      else
                        on_result (Error_types.Partial_success { 
                          result; 
                          warnings = [Error_types.Enrichment_skipped err]
                        }))
            in
            
            post_tweets_seq texts media_ids_per_post None [])
          (fun err -> on_result (Error_types.Failure err))
  
  (** Post thread with media URLs
      
      @param account_id The account identifier
      @param texts List of tweet texts
      @param media_urls_per_post Media URLs for each post
      @param alt_texts_per_post Alt texts for each post's media
      @param validate_media_before_upload When true, validates each media file after 
             download but before upload. Default: false
      @param on_result Callback receiving the outcome with thread_result
  *)
  let post_thread ~account_id ~texts ~media_urls_per_post ?(alt_texts_per_post=[]) ?(validate_media_before_upload=false) on_result =
    let media_counts = List.map List.length media_urls_per_post in
    match validate_thread ~texts ~media_counts () with
    | Error errs ->
        on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        ensure_valid_token ~account_id
          (fun access_token ->
            let total_requested = List.length texts in
            let accumulated_warnings = ref [] in
            
            (* Helper to fetch media with retry logic *)
            let fetch_media_with_retry url max_retries on_success on_err =
              let rec attempt retry_count =
                Config.Http.get url
                  (fun media_resp ->
                    if media_resp.status >= 200 && media_resp.status < 300 then
                      on_success media_resp
                    else if retry_count < max_retries && 
                            (media_resp.status >= 500 || media_resp.status = 429) then
                      attempt (retry_count + 1)
                    else
                      on_err (Printf.sprintf "Failed to fetch media from %s (status: %d)" url media_resp.status))
                  on_err
              in
              attempt 0
            in
            
            (* Helper to determine media type from MIME type *)
            let media_type_of_mime mime_type =
              if String.starts_with ~prefix:"video/" mime_type then Platform_types.Video
              else if mime_type = "image/gif" then Platform_types.Gif
              else Platform_types.Image
            in
            
            (* Helper to upload multiple media in sequence with alt text *)
            let rec upload_media_seq urls_with_alt acc on_complete on_err =
              match urls_with_alt with
              | [] -> on_complete (List.rev acc)
              | (url, alt_text) :: rest ->
                  fetch_media_with_retry url 3
                    (fun media_resp ->
                      let mime_type = 
                        List.assoc_opt "content-type" media_resp.headers 
                        |> Option.value ~default:"image/jpeg"
                      in
                      let file_size = String.length media_resp.body in
                      
                      (* Validate media if requested *)
                      let validation_result =
                        if validate_media_before_upload then
                          let media : Platform_types.post_media = {
                            media_type = media_type_of_mime mime_type;
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
                      
                      match validation_result with
                      | Error errs ->
                          on_result (Error_types.Failure (Error_types.Validation_error errs))
                      | Ok () ->
                          (* Upload with alt text - failures become warnings *)
                          upload_media_with_mode ~access_token ~media_data:media_resp.body ~mime_type ~alt_text
                            (fun (media_id, alt_text_failed) -> 
                              if alt_text_failed then
                                accumulated_warnings := Error_types.Alt_text_failed media_id :: !accumulated_warnings;
                              upload_media_seq rest (media_id :: acc) on_complete on_err)
                            on_err)
                    on_err
            in
            
            (* Pair media URLs with alt text for each post *)
            let padded_alt_texts = alt_texts_per_post @ List.init (max 0 (List.length media_urls_per_post - List.length alt_texts_per_post)) (fun _ -> []) in
            let media_with_alt_per_post = List.map2 (fun media_urls alt_texts ->
              List.mapi (fun i url ->
                let alt_text = try List.nth alt_texts i with _ -> None in
                (url, alt_text)
              ) media_urls
            ) media_urls_per_post padded_alt_texts in
            
            (* Post tweets in sequence with reply references *)
            let rec post_tweets_seq texts_remaining media_remaining reply_to_id acc_ids =
              match texts_remaining with
              | [] -> 
                  let result = { 
                    Error_types.posted_ids = List.rev acc_ids; 
                    failed_at_index = None;
                    total_requested;
                  } in
                  if !accumulated_warnings = [] then
                    on_result (Error_types.Success result)
                  else
                    on_result (Error_types.Partial_success { result; warnings = !accumulated_warnings })
              | text :: rest_texts ->
                  let current_media = match media_remaining with
                    | media :: rest_media -> (media, rest_media)
                    | [] -> ([], [])
                  in
                  let (media_with_alt, next_media) = current_media in
                  
                  (* Upload media for this tweet *)
                  let media_to_upload = List.filteri (fun i _ -> i < 4) media_with_alt in
                  upload_media_seq media_to_upload []
                    (fun media_ids ->
                      (* Create tweet *)
                      let url = Printf.sprintf "%s/tweets" twitter_api_base in
                      
                      let base_fields = [("text", `String text)] in
                      let base_with_reply = match reply_to_id with
                        | Some id -> ("reply", `Assoc [("in_reply_to_tweet_id", `String id)]) :: base_fields
                        | None -> base_fields
                      in
                      let body_json = 
                        if List.length media_ids > 0 then
                          `Assoc (("media", `Assoc [
                            ("media_ids", `List (List.map (fun id -> `String id) media_ids))
                          ]) :: base_with_reply)
                        else
                          `Assoc base_with_reply
                      in
                      let body = Yojson.Basic.to_string body_json in
                      
                      let headers = [
                        ("Authorization", Printf.sprintf "Bearer %s" access_token);
                        ("Content-Type", "application/json");
                      ] in
                      
                      Config.Http.post ~headers ~body url
                        (fun response ->
                          if response.status >= 200 && response.status < 300 then
                            try
                              let json = Yojson.Basic.from_string response.body in
                              let tweet_id = json
                                |> Yojson.Basic.Util.member "data"
                                |> Yojson.Basic.Util.member "id"
                                |> Yojson.Basic.Util.to_string in
                              record_post ();
                              post_tweets_seq rest_texts next_media (Some tweet_id) (tweet_id :: acc_ids)
                            with e ->
                              let result = { 
                                Error_types.posted_ids = List.rev acc_ids;
                                failed_at_index = Some (total_requested - List.length texts_remaining);
                                total_requested;
                              } in
                              if acc_ids = [] then
                                on_result (Error_types.Failure (Error_types.Internal_error 
                                  (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e))))
                              else
                                on_result (Error_types.Partial_success { 
                                  result; 
                                  warnings = !accumulated_warnings @ [Error_types.Enrichment_skipped 
                                    (Printf.sprintf "Parse error: %s" (Printexc.to_string e))]
                                })
                          else
                            let result = { 
                              Error_types.posted_ids = List.rev acc_ids;
                              failed_at_index = Some (total_requested - List.length texts_remaining);
                              total_requested;
                            } in
                            let err = parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers in
                            if acc_ids = [] then
                              on_result (Error_types.Failure err)
                            else
                              on_result (Error_types.Partial_success { 
                                result; 
                                warnings = !accumulated_warnings @ [Error_types.Enrichment_skipped 
                                  (Error_types.error_to_string err)]
                              }))
                        (fun err ->
                          let result = { 
                            Error_types.posted_ids = List.rev acc_ids;
                            failed_at_index = Some (total_requested - List.length texts_remaining);
                            total_requested;
                          } in
                          if acc_ids = [] then
                            on_result (Error_types.Failure (Error_types.Network_error 
                              (Error_types.Connection_failed err)))
                          else
                            on_result (Error_types.Partial_success { 
                              result; 
                              warnings = !accumulated_warnings @ [Error_types.Enrichment_skipped err]
                            })))
                    (fun err ->
                      let result = { 
                        Error_types.posted_ids = List.rev acc_ids;
                        failed_at_index = Some (total_requested - List.length texts_remaining);
                        total_requested;
                      } in
                      if acc_ids = [] then
                        on_result (Error_types.Failure (Error_types.Network_error 
                          (Error_types.Connection_failed err)))
                      else
                        on_result (Error_types.Partial_success { 
                          result; 
                          warnings = !accumulated_warnings @ [Error_types.Enrichment_skipped err]
                        }))
            in
            
            post_tweets_seq texts media_with_alt_per_post None [])
          (fun err -> on_result (Error_types.Failure err))
  
  (** Delete a tweet
      
      @param account_id The account identifier
      @param tweet_id The ID of the tweet to delete
      @param on_result Callback receiving the outcome
  *)
  let delete_tweet ~account_id ~tweet_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let url = Printf.sprintf "%s/tweets/%s" twitter_api_base tweet_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
        ] in
        
        Config.Http.delete ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let deleted = json
                  |> Yojson.Basic.Util.member "data"
                  |> Yojson.Basic.Util.member "deleted"
                  |> Yojson.Basic.Util.to_bool in
                if deleted then
                  on_result (Error_types.Success ())
                else
                  on_result (Error_types.Failure (Error_types.Internal_error "Tweet was not deleted"))
              with e ->
                on_result (Error_types.Failure (Error_types.Internal_error 
                  (Printf.sprintf "Failed to parse delete response: %s" (Printexc.to_string e))))
            else if response.status = 404 then
              on_result (Error_types.Failure (Error_types.Resource_not_found tweet_id))
            else
              on_result (Error_types.Failure (parse_api_error 
                ~status_code:response.status ~body:response.body ~headers:response.headers)))
          (fun err -> on_result (Error_types.Failure (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
              (fun err -> on_result (Error_types.Failure err))
  
  (** Get a tweet by ID with optional expansions and fields *)
  let get_tweet ~account_id ~tweet_id ?(expansions=[]) ?(tweet_fields=[]) () on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let params = [] in
        let params = if List.length expansions > 0 then
          ("expansions", String.concat "," expansions) :: params
        else params in
        let params = if List.length tweet_fields > 0 then
          ("tweet.fields", String.concat "," tweet_fields) :: params
        else params in
        
        let query = if List.length params > 0 then
          "?" ^ Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) params)
        else "" in
        
        let url = Printf.sprintf "%s/tweets/%s%s" twitter_api_base tweet_id query in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
        ] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok json)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse tweet response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Get multiple tweets by IDs (batch lookup).

      Uses GET /2/tweets with the `ids` query parameter for fetching multiple
      tweets in a single request.  This is more efficient than individual
      lookups for timeline and analytics workflows.

      @param account_id  The account identifier
      @param ids          List of tweet IDs to look up (max 100 per request)
      @param expansions   Optional expansions (e.g. ["author_id"])
      @param tweet_fields Optional tweet fields (e.g. ["created_at"; "public_metrics"])
      @return JSON response with a `data` array of tweet objects
  *)
  let get_tweets ~account_id ~ids ?(expansions=[]) ?(tweet_fields=[]) () on_result =
    if ids = [] then
      on_result (Error (Error_types.Internal_error "ids list must not be empty"))
    else
      ensure_valid_token ~account_id
        (fun access_token ->
          let params = [
            ("ids", String.concat "," ids);
          ] in
          let params = if List.length expansions > 0 then
            ("expansions", String.concat "," expansions) :: params
          else params in
          let params = if List.length tweet_fields > 0 then
            ("tweet.fields", String.concat "," tweet_fields) :: params
          else params in

          let query = Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) params) in
          let url = Printf.sprintf "%s/tweets?%s" twitter_api_base query in
          let headers = [
            ("Authorization", Printf.sprintf "Bearer %s" access_token);
          ] in

          Config.Http.get ~headers url
            (fun response ->
              if response.status >= 200 && response.status < 300 then
                try
                  let json = Yojson.Basic.from_string response.body in
                  on_result (Ok json)
                with e ->
                  on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse tweets response: %s" (Printexc.to_string e))))
              else
                on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
            (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
        (fun err -> on_result (Error err))

  (** Search recent tweets *)
  let search_tweets ~account_id ~query ?(max_results=10) ?(next_token=None) ?(expansions=[]) ?(tweet_fields=[]) () on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let params = [
          ("query", query);
          ("max_results", string_of_int max_results);
        ] in
        let params = match next_token with
          | Some token -> ("next_token", token) :: params
          | None -> params in
        let params = if List.length expansions > 0 then
          ("expansions", String.concat "," expansions) :: params
        else params in
        let params = if List.length tweet_fields > 0 then
          ("tweet.fields", String.concat "," tweet_fields) :: params
        else params in
        
        let query_str = Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) params) in
        let url = Printf.sprintf "%s/tweets/search/recent?%s" twitter_api_base query_str in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
        ] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok json)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse search response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Get user timeline (tweets by user ID).

      @param start_time  Oldest UTC datetime (RFC 3339) for returned tweets.
      @param end_time    Newest UTC datetime (RFC 3339) for returned tweets.
      @param exclude     List of tweet types to exclude, e.g. ["retweets"; "replies"].
  *)
  let get_user_timeline ~account_id ~user_id ?(max_results=10) ?(pagination_token=None) ?(start_time=None) ?(end_time=None) ?(exclude=[]) ?(expansions=[]) ?(tweet_fields=[]) () on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let params = [
          ("max_results", string_of_int max_results);
        ] in
        let params = match pagination_token with
          | Some token -> ("pagination_token", token) :: params
          | None -> params in
        let params = match start_time with
          | Some t -> ("start_time", t) :: params
          | None -> params in
        let params = match end_time with
          | Some t -> ("end_time", t) :: params
          | None -> params in
        let params = if List.length exclude > 0 then
          ("exclude", String.concat "," exclude) :: params
        else params in
        let params = if List.length expansions > 0 then
          ("expansions", String.concat "," expansions) :: params
        else params in
        let params = if List.length tweet_fields > 0 then
          ("tweet.fields", String.concat "," tweet_fields) :: params
        else params in

        let query = Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) params) in
        let url = Printf.sprintf "%s/users/%s/tweets?%s" twitter_api_base user_id query in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
        ] in

        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok json)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse timeline response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Get authenticated user's info *)
  let get_me ~account_id ?(user_fields=[]) () on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let params = if List.length user_fields > 0 then
          [("user.fields", String.concat "," user_fields)]
        else [] in
        
        let query = if List.length params > 0 then
          "?" ^ Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) params)
        else "" in
        
        let url = Printf.sprintf "%s/users/me%s" twitter_api_base query in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
        ] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok json)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse user response: %s" (Printexc.to_string e))))
            else
               on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
           (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Get account analytics from authenticated user public metrics. *)
  let get_account_analytics ~account_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let query =
          Uri.encoded_of_query
            [ ("user.fields", [ "id,username,name,public_metrics" ]) ]
        in
        let url = Printf.sprintf "%s/users/me?%s" twitter_api_base query in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
        ] in

        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              match parse_account_analytics_response response.body with
              | Ok analytics -> on_result (Ok analytics)
              | Error (`Not_found resource) ->
                  on_result (Error (Error_types.Resource_not_found resource))
              | Error (`Malformed msg) ->
                  on_result (Error (Error_types.Internal_error msg))
            else
              on_result
                (Error
                   (parse_api_error
                      ~status_code:response.status
                      ~body:response.body
                      ~headers:response.headers)))
          (fun err ->
            on_result
              (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  let should_fallback_post_metric_set ~status_code ~body =
    let contains_case_insensitive text fragment =
      let text_l = String.lowercase_ascii text in
      let fragment_l = String.lowercase_ascii fragment in
      try
        ignore (Str.search_forward (Str.regexp_string fragment_l) text_l 0);
        true
      with Not_found -> false
    in
    status_code = 403
    ||
    (status_code = 400
     && (contains_case_insensitive body "non_public_metrics"
         || contains_case_insensitive body "organic_metrics"
         || contains_case_insensitive body "promoted_metrics")
     && (contains_case_insensitive body "forbidden"
         || contains_case_insensitive body "unauthorized"
         || contains_case_insensitive body "not permitted"
         || contains_case_insensitive body "unsupported"
         || contains_case_insensitive body "invalid"))

  (** Get tweet analytics from tweet metrics.

      By default, this requests `public_metrics` only. Use [metric_set] to
      request expanded metric objects and optionally allow a graceful fallback
      to public-only metrics if expanded fields are unavailable for the caller.
  *)
  let get_post_analytics
      ~account_id
      ~tweet_id
      ?(metric_set = Public_metrics_only)
      on_result =
    let normalized_tweet_id = String.trim tweet_id in
    if normalized_tweet_id = "" then
      on_result (Error (Error_types.Internal_error "tweet_id is required"))
    else
      ensure_valid_token ~account_id
        (fun access_token ->
          let headers = [
            ("Authorization", Printf.sprintf "Bearer %s" access_token);
          ] in
          let rec fetch requested_metric_set ~already_fallback_attempted =
            let query =
              Uri.encoded_of_query
                [ ( "tweet.fields",
                    [ tweet_fields_for_post_metric_set requested_metric_set ] ) ]
            in
            let url =
              Printf.sprintf "%s/tweets/%s?%s"
                twitter_api_base
                normalized_tweet_id
                query
            in
            Config.Http.get ~headers url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  match parse_post_analytics_response ~tweet_id:normalized_tweet_id response.body with
                  | Ok analytics -> on_result (Ok analytics)
                  | Error (`Not_found resource) ->
                      on_result (Error (Error_types.Resource_not_found resource))
                  | Error (`Malformed msg) ->
                      on_result (Error (Error_types.Internal_error msg))
                else if
                  not already_fallback_attempted
                  && requested_metric_set <> Public_metrics_only
                  && metric_set_allows_fallback metric_set
                  && should_fallback_post_metric_set
                       ~status_code:response.status
                       ~body:response.body
                then
                  fetch Public_metrics_only ~already_fallback_attempted:true
                else
                  on_result
                    (Error
                       (parse_api_error
                          ~status_code:response.status
                          ~body:response.body
                          ~headers:response.headers)))
              (fun err ->
                on_result
                  (Error (Error_types.Network_error (Error_types.Connection_failed err))))
          in
          fetch metric_set ~already_fallback_attempted:false)
        (fun err -> on_result (Error err))

  let get_account_analytics_canonical ~account_id on_result =
    get_account_analytics ~account_id
      (function
        | Ok analytics ->
            on_result (Ok (to_canonical_account_analytics_series analytics))
        | Error err -> on_result (Error err))

  let get_post_analytics_canonical ~account_id ~tweet_id ?(metric_set = Public_metrics_only) on_result =
    get_post_analytics ~account_id ~tweet_id ~metric_set
      (function
        | Ok analytics -> on_result (Ok (to_canonical_post_analytics_series analytics))
        | Error err -> on_result (Error err))
  
  (** Get mentions timeline for authenticated user *)
  let get_mentions_timeline ~account_id ?(max_results=10) ?(pagination_token=None) ?(expansions=[]) ?(tweet_fields=[]) () on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (function
            | Error e -> on_result (Error e)
            | Ok me_json ->
              try
                let open Yojson.Basic.Util in
                let user_id = me_json |> member "data" |> member "id" |> to_string in
                
                let params = [
                  ("max_results", string_of_int max_results);
                ] in
                let params = match pagination_token with
                  | Some token -> ("pagination_token", token) :: params
                  | None -> params in
                let params = if List.length expansions > 0 then
                  ("expansions", String.concat "," expansions) :: params
                else params in
                let params = if List.length tweet_fields > 0 then
                  ("tweet.fields", String.concat "," tweet_fields) :: params
                else params in
                
                let query = Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) params) in
                let url = Printf.sprintf "%s/users/%s/mentions?%s" twitter_api_base user_id query in
                let headers = [
                  ("Authorization", Printf.sprintf "Bearer %s" access_token);
                ] in
                
                Config.Http.get ~headers url
                  (fun response ->
                    if response.status >= 200 && response.status < 300 then
                      try
                        let json = Yojson.Basic.from_string response.body in
                        on_result (Ok json)
                      with e ->
                        on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse mentions: %s" (Printexc.to_string e))))
                    else
                      on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
                  (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err))))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e))))))
      (fun err -> on_result (Error err))
  
  (** Get home timeline (reverse chronological timeline of tweets from followed users) 
      Note: This endpoint requires OAuth 2.0 with user context *)
  let get_home_timeline ~account_id ?(max_results=10) ?(pagination_token=None) ?(expansions=[]) ?(tweet_fields=[]) () on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (function
            | Error e -> on_result (Error e)
            | Ok me_json ->
              try
                let open Yojson.Basic.Util in
                let user_id = me_json |> member "data" |> member "id" |> to_string in
                
                let params = [
                  ("max_results", string_of_int max_results);
                ] in
                let params = match pagination_token with
                  | Some token -> ("pagination_token", token) :: params
                  | None -> params in
                let params = if List.length expansions > 0 then
                  ("expansions", String.concat "," expansions) :: params
                else params in
                let params = if List.length tweet_fields > 0 then
                  ("tweet.fields", String.concat "," tweet_fields) :: params
                else params in
                
                let query = Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) params) in
                let url = Printf.sprintf "%s/users/%s/timelines/reverse_chronological?%s" twitter_api_base user_id query in
                let headers = [
                  ("Authorization", Printf.sprintf "Bearer %s" access_token);
                ] in
                
                Config.Http.get ~headers url
                  (fun response ->
                    if response.status >= 200 && response.status < 300 then
                      try
                        let json = Yojson.Basic.from_string response.body in
                        on_result (Ok json)
                      with e ->
                        on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse home timeline: %s" (Printexc.to_string e))))
                    else
                      on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
                  (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err))))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e))))))
      (fun err -> on_result (Error err))
  
  (** Get user by ID *)
  let get_user_by_id ~account_id ~user_id ?(user_fields=[]) () on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let params = if List.length user_fields > 0 then
          [("user.fields", String.concat "," user_fields)]
        else [] in
        
        let query = if List.length params > 0 then
          "?" ^ Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) params)
        else "" in
        
        let url = Printf.sprintf "%s/users/%s%s" twitter_api_base user_id query in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
        ] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok json)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse user response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Get user by username *)
  let get_user_by_username ~account_id ~username ?(user_fields=[]) () on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let params = if List.length user_fields > 0 then
          [("user.fields", String.concat "," user_fields)]
        else [] in
        
        let query = if List.length params > 0 then
          "?" ^ Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) params)
        else "" in
        
        let url = Printf.sprintf "%s/users/by/username/%s%s" twitter_api_base username query in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
        ] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok json)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse user response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Follow a user *)
  let follow_user ~account_id ~target_user_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        (* First get authenticated user's ID *)
        get_me ~account_id ()
          (function
            | Error e -> on_result (Error e)
            | Ok me_json ->
              try
                let source_user_id = me_json
                  |> Yojson.Basic.Util.member "data"
                  |> Yojson.Basic.Util.member "id"
                  |> Yojson.Basic.Util.to_string in
                
                let url = Printf.sprintf "%s/users/%s/following" twitter_api_base source_user_id in
                let body_json = `Assoc [("target_user_id", `String target_user_id)] in
                let body = Yojson.Basic.to_string body_json in
                let headers = [
                  ("Authorization", Printf.sprintf "Bearer %s" access_token);
                  ("Content-Type", "application/json");
                ] in
                
                Config.Http.post ~headers ~body url
                  (fun response ->
                    if response.status >= 200 && response.status < 300 then
                      on_result (Ok ())
                    else
                      on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
                  (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err))))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e))))))
      (fun err -> on_result (Error err))
  
  (** Unfollow a user *)
  let unfollow_user ~account_id ~target_user_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (function
            | Error e -> on_result (Error e)
            | Ok me_json ->
              try
                let source_user_id = me_json
                  |> Yojson.Basic.Util.member "data"
                  |> Yojson.Basic.Util.member "id"
                  |> Yojson.Basic.Util.to_string in
                
                let url = Printf.sprintf "%s/users/%s/following/%s" twitter_api_base source_user_id target_user_id in
                let headers = [
                  ("Authorization", Printf.sprintf "Bearer %s" access_token);
                ] in
                
                Config.Http.delete ~headers url
                  (fun response ->
                    if response.status >= 200 && response.status < 300 then
                      on_result (Ok ())
                    else
                      on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
                  (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err))))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e))))))
      (fun err -> on_result (Error err))
  
  (** Block a user *)
  let block_user ~account_id ~target_user_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (function
            | Error e -> on_result (Error e)
            | Ok me_json ->
              try
                let source_user_id = me_json
                  |> Yojson.Basic.Util.member "data"
                  |> Yojson.Basic.Util.member "id"
                  |> Yojson.Basic.Util.to_string in
                
                let url = Printf.sprintf "%s/users/%s/blocking" twitter_api_base source_user_id in
                let body_json = `Assoc [("target_user_id", `String target_user_id)] in
                let body = Yojson.Basic.to_string body_json in
                let headers = [
                  ("Authorization", Printf.sprintf "Bearer %s" access_token);
                  ("Content-Type", "application/json");
                ] in
                
                Config.Http.post ~headers ~body url
                  (fun response ->
                    if response.status >= 200 && response.status < 300 then
                      on_result (Ok ())
                    else
                      on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
                  (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err))))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e))))))
      (fun err -> on_result (Error err))
  
  (** Unblock a user *)
  let unblock_user ~account_id ~target_user_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (function
            | Error e -> on_result (Error e)
            | Ok me_json ->
              try
                let source_user_id = me_json
                  |> Yojson.Basic.Util.member "data"
                  |> Yojson.Basic.Util.member "id"
                  |> Yojson.Basic.Util.to_string in
                
                let url = Printf.sprintf "%s/users/%s/blocking/%s" twitter_api_base source_user_id target_user_id in
                let headers = [
                  ("Authorization", Printf.sprintf "Bearer %s" access_token);
                ] in
                
                Config.Http.delete ~headers url
                  (fun response ->
                    if response.status >= 200 && response.status < 300 then
                      on_result (Ok ())
                    else
                      on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
                  (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err))))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e))))))
      (fun err -> on_result (Error err))
  
  (** Mute a user *)
  let mute_user ~account_id ~target_user_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (function
            | Error e -> on_result (Error e)
            | Ok me_json ->
              try
                let source_user_id = me_json
                  |> Yojson.Basic.Util.member "data"
                  |> Yojson.Basic.Util.member "id"
                  |> Yojson.Basic.Util.to_string in
                
                let url = Printf.sprintf "%s/users/%s/muting" twitter_api_base source_user_id in
                let body_json = `Assoc [("target_user_id", `String target_user_id)] in
                let body = Yojson.Basic.to_string body_json in
                let headers = [
                  ("Authorization", Printf.sprintf "Bearer %s" access_token);
                  ("Content-Type", "application/json");
                ] in
                
                Config.Http.post ~headers ~body url
                  (fun response ->
                    if response.status >= 200 && response.status < 300 then
                      on_result (Ok ())
                    else
                      on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
                  (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err))))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e))))))
      (fun err -> on_result (Error err))
  
  (** Unmute a user *)
  let unmute_user ~account_id ~target_user_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (function
            | Error e -> on_result (Error e)
            | Ok me_json ->
              try
                let source_user_id = me_json
                  |> Yojson.Basic.Util.member "data"
                  |> Yojson.Basic.Util.member "id"
                  |> Yojson.Basic.Util.to_string in
                
                let url = Printf.sprintf "%s/users/%s/muting/%s" twitter_api_base source_user_id target_user_id in
                let headers = [
                  ("Authorization", Printf.sprintf "Bearer %s" access_token);
                ] in
                
                Config.Http.delete ~headers url
                  (fun response ->
                    if response.status >= 200 && response.status < 300 then
                      on_result (Ok ())
                    else
                      on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
                  (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err))))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e))))))
      (fun err -> on_result (Error err))
  
  (** Get followers of a user *)
  let get_followers ~account_id ~user_id ?(max_results=100) ?(pagination_token=None) ?(user_fields=[]) () on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let params = [
          ("max_results", string_of_int (min max_results 1000));
        ] in
        let params = match pagination_token with
          | Some token -> ("pagination_token", token) :: params
          | None -> params in
        let params = if List.length user_fields > 0 then
          ("user.fields", String.concat "," user_fields) :: params
        else params in
        
        let query = Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) params) in
        let url = Printf.sprintf "%s/users/%s/followers?%s" twitter_api_base user_id query in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
        ] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok json)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse followers: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Get users that a user is following *)
  let get_following ~account_id ~user_id ?(max_results=100) ?(pagination_token=None) ?(user_fields=[]) () on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let params = [
          ("max_results", string_of_int (min max_results 1000));
        ] in
        let params = match pagination_token with
          | Some token -> ("pagination_token", token) :: params
          | None -> params in
        let params = if List.length user_fields > 0 then
          ("user.fields", String.concat "," user_fields) :: params
        else params in
        
        let query = Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) params) in
        let url = Printf.sprintf "%s/users/%s/following?%s" twitter_api_base user_id query in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
        ] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok json)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse following: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Search for users by keyword *)
  let search_users ~account_id ~query ?(max_results=100) ?(pagination_token=None) ?(user_fields=[]) () on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let params = [
          ("query", query);
          ("max_results", string_of_int (min max_results 100));
        ] in
        let params = match pagination_token with
          | Some token -> ("pagination_token", token) :: params
          | None -> params in
        let params = if List.length user_fields > 0 then
          ("user.fields", String.concat "," user_fields) :: params
        else params in
        
        let query_str = Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) params) in
        let url = Printf.sprintf "%s/users/search?%s" twitter_api_base query_str in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
        ] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok json)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse user search: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Like a tweet *)
  let like_tweet ~account_id ~tweet_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (function
            | Error e -> on_result (Error e)
            | Ok me_json ->
              try
                let user_id = me_json
                  |> Yojson.Basic.Util.member "data"
                  |> Yojson.Basic.Util.member "id"
                  |> Yojson.Basic.Util.to_string in
                
                let url = Printf.sprintf "%s/users/%s/likes" twitter_api_base user_id in
                let body_json = `Assoc [("tweet_id", `String tweet_id)] in
                let body = Yojson.Basic.to_string body_json in
                let headers = [
                  ("Authorization", Printf.sprintf "Bearer %s" access_token);
                  ("Content-Type", "application/json");
                ] in
                
                Config.Http.post ~headers ~body url
                  (fun response ->
                    if response.status >= 200 && response.status < 300 then
                      on_result (Ok ())
                    else
                      on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
                  (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err))))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e))))))
      (fun err -> on_result (Error err))
  
  (** Unlike a tweet *)
  let unlike_tweet ~account_id ~tweet_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (function
            | Error e -> on_result (Error e)
            | Ok me_json ->
              try
                let user_id = me_json
                  |> Yojson.Basic.Util.member "data"
                  |> Yojson.Basic.Util.member "id"
                  |> Yojson.Basic.Util.to_string in
                
                let url = Printf.sprintf "%s/users/%s/likes/%s" twitter_api_base user_id tweet_id in
                let headers = [
                  ("Authorization", Printf.sprintf "Bearer %s" access_token);
                ] in
                
                Config.Http.delete ~headers url
                  (fun response ->
                    if response.status >= 200 && response.status < 300 then
                      on_result (Ok ())
                    else
                      on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
                  (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err))))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e))))))
      (fun err -> on_result (Error err))
  
  (** Retweet a tweet *)
  let retweet ~account_id ~tweet_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (function
            | Error e -> on_result (Error e)
            | Ok me_json ->
              try
                let user_id = me_json
                  |> Yojson.Basic.Util.member "data"
                  |> Yojson.Basic.Util.member "id"
                  |> Yojson.Basic.Util.to_string in
                
                let url = Printf.sprintf "%s/users/%s/retweets" twitter_api_base user_id in
                let body_json = `Assoc [("tweet_id", `String tweet_id)] in
                let body = Yojson.Basic.to_string body_json in
                let headers = [
                  ("Authorization", Printf.sprintf "Bearer %s" access_token);
                  ("Content-Type", "application/json");
                ] in
                
                Config.Http.post ~headers ~body url
                  (fun response ->
                    if response.status >= 200 && response.status < 300 then
                      on_result (Ok ())
                    else
                      on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
                  (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err))))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e))))))
      (fun err -> on_result (Error err))
  
  (** Unretweet (delete retweet) *)
  let unretweet ~account_id ~tweet_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (function
            | Error e -> on_result (Error e)
            | Ok me_json ->
              try
                let user_id = me_json
                  |> Yojson.Basic.Util.member "data"
                  |> Yojson.Basic.Util.member "id"
                  |> Yojson.Basic.Util.to_string in
                
                let url = Printf.sprintf "%s/users/%s/retweets/%s" twitter_api_base user_id tweet_id in
                let headers = [
                  ("Authorization", Printf.sprintf "Bearer %s" access_token);
                ] in
                
                Config.Http.delete ~headers url
                  (fun response ->
                    if response.status >= 200 && response.status < 300 then
                      on_result (Ok ())
                    else
                      on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
                  (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err))))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e))))))
      (fun err -> on_result (Error err))
  
  (** Quote tweet (tweet with quoted tweet reference) *)
  let quote_tweet ~account_id ~text ~quoted_tweet_id ~media_urls ?reply_settings ?community_id on_success on_error =
    match check_rate_limit () with
    | Error msg -> on_error msg
    | Ok () ->
        ensure_valid_token ~account_id
          (fun access_token ->
            (* Helper to fetch and upload media *)
            let fetch_media_with_retry url max_retries on_success on_err =
              let rec attempt retry_count =
                Config.Http.get url
                  (fun media_resp ->
                    if media_resp.status >= 200 && media_resp.status < 300 then
                      on_success media_resp
                    else if retry_count < max_retries then
                      attempt (retry_count + 1)
                    else
                      on_err (Printf.sprintf "Failed to fetch media: %d" media_resp.status))
                  on_err
              in
              attempt 0
            in
            
            let rec upload_media_seq urls acc on_complete on_err =
              match urls with
              | [] -> on_complete (List.rev acc)
              | url :: rest ->
                  fetch_media_with_retry url 3
                    (fun media_resp ->
                      let mime_type = 
                        List.assoc_opt "content-type" media_resp.headers 
                        |> Option.value ~default:"image/jpeg"
                      in
                      upload_media_with_mode ~access_token ~media_data:media_resp.body ~mime_type ~alt_text:None
                        (fun (media_id, _alt_text_failed) -> 
                          upload_media_seq rest (media_id :: acc) on_complete on_err)
                        on_err)
                    on_err
            in
            
            let media_to_upload = List.filteri (fun i _ -> i < 4) media_urls in
            upload_media_seq media_to_upload []
              (fun media_ids ->
                let url = Printf.sprintf "%s/tweets" twitter_api_base in
                
                let base_fields =
                  [
                    ("text", `String text);
                    ("quote_tweet_id", `String quoted_tweet_id);
                  ]
                  |> add_optional_string_field "reply_settings" reply_settings
                  |> add_optional_string_field "community_id" community_id
                in
                let body_json = 
                  if List.length media_ids > 0 then
                    `Assoc (("media", `Assoc [
                      ("media_ids", `List (List.map (fun id -> `String id) media_ids))
                    ]) :: base_fields)
                  else
                    `Assoc base_fields
                in
                let body = Yojson.Basic.to_string body_json in
                
                let headers = [
                  ("Authorization", Printf.sprintf "Bearer %s" access_token);
                  ("Content-Type", "application/json");
                ] in
                
                Config.Http.post ~headers ~body url
                  (fun response ->
                    if response.status >= 200 && response.status < 300 then
                      try
                        let json = Yojson.Basic.from_string response.body in
                        let tweet_id = json
                          |> Yojson.Basic.Util.member "data"
                          |> Yojson.Basic.Util.member "id"
                          |> Yojson.Basic.Util.to_string in
                        record_post ();
                        on_success tweet_id
                      with e ->
                        on_error (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e))
                    else
                      on_error (Printf.sprintf "Failed to quote tweet (%d): %s" response.status response.body))
                  on_error)
              on_error)
          (fun err -> on_error (Error_types.error_to_string err))
  
  (** Reply to a tweet *)
  let reply_to_tweet ~account_id ~text ~reply_to_tweet_id ~media_urls ?reply_settings ?community_id on_success on_error =
    match check_rate_limit () with
    | Error msg -> on_error msg
    | Ok () ->
        ensure_valid_token ~account_id
          (fun access_token ->
            let fetch_media_with_retry url max_retries on_success on_err =
              let rec attempt retry_count =
                Config.Http.get url
                  (fun media_resp ->
                    if media_resp.status >= 200 && media_resp.status < 300 then
                      on_success media_resp
                    else if retry_count < max_retries then
                      attempt (retry_count + 1)
                    else
                      on_err (Printf.sprintf "Failed to fetch media: %d" media_resp.status))
                  on_err
              in
              attempt 0
            in
            
            let rec upload_media_seq urls acc on_complete on_err =
              match urls with
              | [] -> on_complete (List.rev acc)
              | url :: rest ->
                  fetch_media_with_retry url 3
                    (fun media_resp ->
                      let mime_type = 
                        List.assoc_opt "content-type" media_resp.headers 
                        |> Option.value ~default:"image/jpeg"
                      in
                      upload_media_with_mode ~access_token ~media_data:media_resp.body ~mime_type ~alt_text:None
                        (fun (media_id, _alt_text_failed) -> 
                          upload_media_seq rest (media_id :: acc) on_complete on_err)
                        on_err)
                    on_err
            in
            
            let media_to_upload = List.filteri (fun i _ -> i < 4) media_urls in
            upload_media_seq media_to_upload []
              (fun media_ids ->
                let url = Printf.sprintf "%s/tweets" twitter_api_base in
                
                let base_fields =
                  [
                    ("text", `String text);
                    ("reply", `Assoc [("in_reply_to_tweet_id", `String reply_to_tweet_id)]);
                  ]
                  |> add_optional_string_field "reply_settings" reply_settings
                  |> add_optional_string_field "community_id" community_id
                in
                let body_json = 
                  if List.length media_ids > 0 then
                    `Assoc (("media", `Assoc [
                      ("media_ids", `List (List.map (fun id -> `String id) media_ids))
                    ]) :: base_fields)
                  else
                    `Assoc base_fields
                in
                let body = Yojson.Basic.to_string body_json in
                
                let headers = [
                  ("Authorization", Printf.sprintf "Bearer %s" access_token);
                  ("Content-Type", "application/json");
                ] in
                
                Config.Http.post ~headers ~body url
                  (fun response ->
                    if response.status >= 200 && response.status < 300 then
                      try
                        let json = Yojson.Basic.from_string response.body in
                        let tweet_id = json
                          |> Yojson.Basic.Util.member "data"
                          |> Yojson.Basic.Util.member "id"
                          |> Yojson.Basic.Util.to_string in
                        record_post ();
                        on_success tweet_id
                      with e ->
                        on_error (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e))
                    else
                      on_error (Printf.sprintf "Failed to reply to tweet (%d): %s" response.status response.body))
                  on_error)
              on_error)
          (fun err -> on_error (Error_types.error_to_string err))
  
  (** Bookmark a tweet *)
  let bookmark_tweet ~account_id ~tweet_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (function
            | Error e -> on_result (Error e)
            | Ok me_json ->
              try
                let user_id = me_json
                  |> Yojson.Basic.Util.member "data"
                  |> Yojson.Basic.Util.member "id"
                  |> Yojson.Basic.Util.to_string in
                
                let url = Printf.sprintf "%s/users/%s/bookmarks" twitter_api_base user_id in
                let body_json = `Assoc [("tweet_id", `String tweet_id)] in
                let body = Yojson.Basic.to_string body_json in
                let headers = [
                  ("Authorization", Printf.sprintf "Bearer %s" access_token);
                  ("Content-Type", "application/json");
                ] in
                
                Config.Http.post ~headers ~body url
                  (fun response ->
                    if response.status >= 200 && response.status < 300 then
                      on_result (Ok ())
                    else
                      on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
                  (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err))))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e))))))
      (fun err -> on_result (Error err))
  
  (** Remove bookmark from a tweet *)
  let remove_bookmark ~account_id ~tweet_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (function
            | Error e -> on_result (Error e)
            | Ok me_json ->
              try
                let user_id = me_json
                  |> Yojson.Basic.Util.member "data"
                  |> Yojson.Basic.Util.member "id"
                  |> Yojson.Basic.Util.to_string in
                
                let url = Printf.sprintf "%s/users/%s/bookmarks/%s" twitter_api_base user_id tweet_id in
                let headers = [
                  ("Authorization", Printf.sprintf "Bearer %s" access_token);
                ] in
                
                Config.Http.delete ~headers url
                  (fun response ->
                    if response.status >= 200 && response.status < 300 then
                      on_result (Ok ())
                    else
                      on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
                  (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err))))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e))))))
      (fun err -> on_result (Error err))
  
  (** Create a list *)
  let create_list ~account_id ~name ?(description=None) ?(private_list=false) () on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let url = Printf.sprintf "%s/lists" twitter_api_base in
        
        let base_fields = [
          ("name", `String name);
          ("private", `Bool private_list);
        ] in
        let fields_with_desc = match description with
          | Some desc -> ("description", `String desc) :: base_fields
          | None -> base_fields
        in
        let body_json = `Assoc fields_with_desc in
        let body = Yojson.Basic.to_string body_json in
        
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
          ("Content-Type", "application/json");
        ] in
        
        Config.Http.post ~headers ~body url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok json)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse list response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Update a list *)
  let update_list ~account_id ~list_id ?(name=None) ?(description=None) ?(private_list=None) () on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let url = Printf.sprintf "%s/lists/%s" twitter_api_base list_id in
        
        let fields = [] in
        let fields = match name with
          | Some n -> ("name", `String n) :: fields
          | None -> fields
        in
        let fields = match description with
          | Some d -> ("description", `String d) :: fields
          | None -> fields
        in
        let fields = match private_list with
          | Some p -> ("private", `Bool p) :: fields
          | None -> fields
        in
        
        if List.length fields = 0 then
          on_result (Error (Error_types.Internal_error "No fields to update"))
        else
          let body_json = `Assoc fields in
          let body = Yojson.Basic.to_string body_json in
          
          let headers = [
            ("Authorization", Printf.sprintf "Bearer %s" access_token);
            ("Content-Type", "application/json");
          ] in
          
          Config.Http.put ~headers ~body url
            (fun response ->
              if response.status >= 200 && response.status < 300 then
                try
                  let json = Yojson.Basic.from_string response.body in
                  on_result (Ok json)
                with e ->
                  on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse list response: %s" (Printexc.to_string e))))
              else
                on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
            (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Delete a list *)
  let delete_list ~account_id ~list_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let url = Printf.sprintf "%s/lists/%s" twitter_api_base list_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
        ] in
        
        Config.Http.delete ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let deleted = json
                  |> Yojson.Basic.Util.member "data"
                  |> Yojson.Basic.Util.member "deleted"
                  |> Yojson.Basic.Util.to_bool in
                if deleted then
                  on_result (Ok ())
                else
                  on_result (Error (Error_types.Internal_error "List was not deleted"))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse delete response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Get a list by ID *)
  let get_list ~account_id ~list_id ?(list_fields=[]) () on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let params = if List.length list_fields > 0 then
          [("list.fields", String.concat "," list_fields)]
        else [] in
        
        let query = if List.length params > 0 then
          "?" ^ Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) params)
        else "" in
        
        let url = Printf.sprintf "%s/lists/%s%s" twitter_api_base list_id query in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
        ] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok json)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse list response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Add a member to a list *)
  let add_list_member ~account_id ~list_id ~user_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let url = Printf.sprintf "%s/lists/%s/members" twitter_api_base list_id in
        let body_json = `Assoc [("user_id", `String user_id)] in
        let body = Yojson.Basic.to_string body_json in
        
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
          ("Content-Type", "application/json");
        ] in
        
        Config.Http.post ~headers ~body url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_result (Ok ())
            else
              on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Remove a member from a list *)
  let remove_list_member ~account_id ~list_id ~user_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let url = Printf.sprintf "%s/lists/%s/members/%s" twitter_api_base list_id user_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
        ] in
        
        Config.Http.delete ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_result (Ok ())
            else
              on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Get list members *)
  let get_list_members ~account_id ~list_id ?(max_results=100) ?(pagination_token=None) ?(user_fields=[]) () on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let params = [
          ("max_results", string_of_int (min max_results 100));
        ] in
        let params = match pagination_token with
          | Some token -> ("pagination_token", token) :: params
          | None -> params in
        let params = if List.length user_fields > 0 then
          ("user.fields", String.concat "," user_fields) :: params
        else params in
        
        let query = Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) params) in
        let url = Printf.sprintf "%s/lists/%s/members?%s" twitter_api_base list_id query in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
        ] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok json)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse members: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Follow a list *)
  let follow_list ~account_id ~list_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (function
            | Error _ -> on_error "Failed to get authenticated user"
            | Ok me_json ->
              try
                let user_id = me_json
                  |> Yojson.Basic.Util.member "data"
                  |> Yojson.Basic.Util.member "id"
                  |> Yojson.Basic.Util.to_string in
                
                let url = Printf.sprintf "%s/users/%s/followed_lists" twitter_api_base user_id in
                let body_json = `Assoc [("list_id", `String list_id)] in
                let body = Yojson.Basic.to_string body_json in
                
                let headers = [
                  ("Authorization", Printf.sprintf "Bearer %s" access_token);
                  ("Content-Type", "application/json");
                ] in
                
                Config.Http.post ~headers ~body url
                  (fun response ->
                    if response.status >= 200 && response.status < 300 then
                      on_success ()
                    else
                      on_error (Printf.sprintf "Failed to follow list (%d): %s" response.status response.body))
                  on_error
              with e ->
                on_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e))))
      (fun err -> on_error (Error_types.error_to_string err))
  
  (** Unfollow a list *)
  let unfollow_list ~account_id ~list_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (function
            | Error _ -> on_error "Failed to get authenticated user"
            | Ok me_json ->
              try
                let user_id = me_json
                  |> Yojson.Basic.Util.member "data"
                  |> Yojson.Basic.Util.member "id"
                  |> Yojson.Basic.Util.to_string in
                
                let url = Printf.sprintf "%s/users/%s/followed_lists/%s" twitter_api_base user_id list_id in
                let headers = [
                  ("Authorization", Printf.sprintf "Bearer %s" access_token);
                ] in
                
                Config.Http.delete ~headers url
                  (fun response ->
                    if response.status >= 200 && response.status < 300 then
                      on_success ()
                    else
                      on_error (Printf.sprintf "Failed to unfollow list (%d): %s" response.status response.body))
                  on_error
              with e ->
                on_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e))))
      (fun err -> on_error (Error_types.error_to_string err))
  
  (** Get tweets from a list *)
  let get_list_tweets ~account_id ~list_id ?(max_results=100) ?(pagination_token=None) ?(expansions=[]) ?(tweet_fields=[]) () on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let params = [
          ("max_results", string_of_int (min max_results 100));
        ] in
        let params = match pagination_token with
          | Some token -> ("pagination_token", token) :: params
          | None -> params in
        let params = if List.length expansions > 0 then
          ("expansions", String.concat "," expansions) :: params
        else params in
        let params = if List.length tweet_fields > 0 then
          ("tweet.fields", String.concat "," tweet_fields) :: params
        else params in
        
        let query = Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) params) in
        let url = Printf.sprintf "%s/lists/%s/tweets?%s" twitter_api_base list_id query in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
        ] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok json)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse list tweets: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~status_code:response.status ~body:response.body ~headers:response.headers)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Pin a list for authenticated user *)
  let pin_list ~account_id ~list_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (function
            | Error _ -> on_error "Failed to get authenticated user"
            | Ok me_json ->
              try
                let user_id = me_json
                  |> Yojson.Basic.Util.member "data"
                  |> Yojson.Basic.Util.member "id"
                  |> Yojson.Basic.Util.to_string in
                
                let url = Printf.sprintf "%s/users/%s/pinned_lists" twitter_api_base user_id in
                let body_json = `Assoc [("list_id", `String list_id)] in
                let body = Yojson.Basic.to_string body_json in
                
                let headers = [
                  ("Authorization", Printf.sprintf "Bearer %s" access_token);
                  ("Content-Type", "application/json");
                ] in
                
                Config.Http.post ~headers ~body url
                  (fun response ->
                    if response.status >= 200 && response.status < 300 then
                      on_success ()
                    else
                      on_error (Printf.sprintf "Failed to pin list (%d): %s" response.status response.body))
                  on_error
              with e ->
                on_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e))))
      (fun err -> on_error (Error_types.error_to_string err))
  
  (** Unpin a list for authenticated user *)
  let unpin_list ~account_id ~list_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (function
            | Error _ -> on_error "Failed to get authenticated user"
            | Ok me_json ->
              try
                let user_id = me_json
                  |> Yojson.Basic.Util.member "data"
                  |> Yojson.Basic.Util.member "id"
                  |> Yojson.Basic.Util.to_string in
                
                let url = Printf.sprintf "%s/users/%s/pinned_lists/%s" twitter_api_base user_id list_id in
                let headers = [
                  ("Authorization", Printf.sprintf "Bearer %s" access_token);
                ] in
                
                Config.Http.delete ~headers url
                  (fun response ->
                    if response.status >= 200 && response.status < 300 then
                      on_success ()
                    else
                      on_error (Printf.sprintf "Failed to unpin list (%d): %s" response.status response.body))
                  on_error
              with e ->
                on_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e))))
      (fun err -> on_error (Error_types.error_to_string err))
  
  (** Validate content for Twitter.

      @param premium When true, allows up to 4000 characters (X Premium limit).
                     Default: false (standard 280-character limit).
  *)
  let validate_content ~text ?(premium=false) () =
    let max_length = if premium then max_tweet_length_premium else max_tweet_length in
    if String.length text > max_length then
      Error (Printf.sprintf "Tweet exceeds %d character limit" max_length)
    else
      Ok ()
  
  (** Validate media for Twitter *)
  let validate_media ~(media : Platform_types.post_media) =
    match media.Platform_types.media_type with
    | Platform_types.Image ->
        if media.file_size_bytes > 5 * 1024 * 1024 then
          Error "Image exceeds 5MB limit"
        else
          Ok ()
    | Platform_types.Video ->
        if media.file_size_bytes > 512 * 1024 * 1024 then
          Error "Video exceeds 512MB limit"
        else if Option.value ~default:0.0 media.duration_seconds > 140.0 then
          Error "Video exceeds 140 second limit"
        else
          Ok ()
    | Platform_types.Gif ->
        if media.file_size_bytes > 15 * 1024 * 1024 then
          Error "GIF exceeds 15MB limit"
        else
          Ok ()
  
  (** Generate PKCE code_challenge from code_verifier using SHA256
      
      @deprecated Use OAuth.Pkce.generate_code_challenge instead
      
      code_challenge = BASE64URL(SHA256(ASCII(code_verifier)))
      
      @param verifier The code_verifier string
      @return Base64-URL encoded SHA256 hash without padding
  *)
  let generate_code_challenge = OAuth.Pkce.generate_code_challenge

  (** Get OAuth authorization URL
      
      @deprecated Use OAuth.get_authorization_url for more flexibility
      
      This convenience function uses environment variables for configuration.
  *)
  let get_oauth_url ~state ~code_verifier =
    let client_id = Config.get_env "TWITTER_CLIENT_ID" |> Option.value ~default:"" in
    let redirect_uri = Config.get_env "TWITTER_LINK_REDIRECT_URI" |> Option.value ~default:"" in
    let code_challenge = OAuth.Pkce.generate_code_challenge code_verifier in
    OAuth.get_authorization_url 
      ~client_id 
      ~redirect_uri 
      ~state 
      ~code_challenge 
      ()

  (** Verify OAuth callback state against expected state for CSRF protection. *)
  let verify_oauth_state ~expected_state ~returned_state =
    String.equal expected_state returned_state
  
  (** Exchange authorization code for access token
      
      @deprecated Use OAuth.Make(Http).exchange_code for more flexibility
      
      This convenience function uses environment variables for configuration.
  *)
  let exchange_code ~code ~code_verifier on_success on_error =
    let client_id = Config.get_env "TWITTER_CLIENT_ID" |> Option.value ~default:"" in
    let client_secret = Config.get_env "TWITTER_CLIENT_SECRET" |> Option.value ~default:"" in
    let redirect_uri = Config.get_env "TWITTER_LINK_REDIRECT_URI" |> Option.value ~default:"" in
    
    let body = Uri.encoded_of_query [
      ("grant_type", ["authorization_code"]);
      ("code", [code]);
      ("redirect_uri", [redirect_uri]);
      ("code_verifier", [code_verifier]);
    ] in
    
    let auth = Base64.encode_exn (client_id ^ ":" ^ client_secret) in
    let headers = [
      ("Authorization", "Basic " ^ auth);
      ("Content-Type", "application/x-www-form-urlencoded");
    ] in
    
    Config.Http.post ~headers ~body OAuth.Metadata.token_endpoint
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            on_success json
          with e ->
            on_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "Token exchange failed (%d): %s" response.status (OAuth.redact_sensitive_text response.body)))
      on_error
end
