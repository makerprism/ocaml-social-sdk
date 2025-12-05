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
  (** Scope definitions for Twitter API v2 *)
  module Scopes = struct
    (** Scopes required for read-only operations *)
    let read = ["tweet.read"; "users.read"]
    
    (** Scopes required for posting content (includes offline.access for refresh tokens) *)
    let write = ["tweet.read"; "tweet.write"; "users.read"; "offline.access"]
    
    (** All available Twitter OAuth 2.0 scopes *)
    let all = [
      "tweet.read"; "tweet.write"; "tweet.moderate.write";
      "users.read"; "follows.read"; "follows.write";
      "offline.access"; "space.read"; "mute.read"; "mute.write";
      "like.read"; "like.write"; "list.read"; "list.write";
      "block.read"; "block.write"; "bookmark.read"; "bookmark.write"
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
      let post_scopes = ["tweet.read"; "tweet.write"] in
      let read_scopes = ["tweet.read"] in
      if List.exists (fun o -> o = Post_text || o = Post_media || o = Post_video || o = Delete_post) ops
      then base @ post_scopes
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
            on_error (Printf.sprintf "Token exchange failed (%d): %s" response.status response.body))
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
            on_error (Printf.sprintf "Token refresh failed (%d): %s" response.status response.body))
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
            on_error (Printf.sprintf "Token revocation failed (%d): %s" response.status response.body))
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
          on_error (Printf.sprintf "Token refresh failed (%d): %s" response.status response.body))
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
                (fun () -> on_error "No refresh token available - please reconnect")
                on_error
          | Some refresh_token ->
              let client_id = Config.get_env "TWITTER_CLIENT_ID" |> Option.value ~default:"" in
              let client_secret = Config.get_env "TWITTER_CLIENT_SECRET" |> Option.value ~default:"" in
              
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
                        on_error)
                    on_error)
                (fun err ->
                  Config.update_health_status ~account_id ~status:"refresh_failed" 
                    ~error_message:(Some err)
                    (fun () -> on_error err)
                    on_error)
        else
          (* Token still valid *)
          Config.update_health_status ~account_id ~status:"healthy" ~error_message:None
            (fun () -> on_success creds.access_token)
            on_error)
      on_error
  
  (** Update media metadata with alt text using X API v2 *)
  let update_media_metadata ~access_token ~media_id ~alt_text on_success _on_error =
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
    
    Printf.printf "[Twitter] Updating media metadata for media_id=%s with alt_text=%s\n%!" media_id alt_text;
    
    Config.Http.post ~headers ~body url
      (fun response ->
        if response.status >= 200 && response.status < 300 then begin
          Printf.printf "[Twitter] Alt text update succeeded for media_id=%s\n%!" media_id;
          on_success ()
        end else begin
          Printf.printf "[Twitter] Alt text update failed (%d): %s\n%!" response.status response.body;
          (* Don't fail the entire upload if alt text update fails, but log it *)
          on_success ()
        end)
      (fun err -> 
        Printf.printf "[Twitter] Alt text HTTP error: %s\n%!" err;
        (* Don't fail the entire upload if alt text update fails, but log it *)
        on_success ())
  
  (** Upload media to X API v2 (requires S256 PKCE tokens)
      
      Uses the new v2 endpoint at https://api.x.com/2/media/upload
      
      The X API v2 supports two content types:
      1. application/json with base64-encoded media (recommended, more reliable)
      2. multipart/form-data with raw binary
      
      Based on community feedback, JSON with base64 is more reliable.
      
      Required fields per X API v2 docs:
      - media: base64 encoded file data (string<byte>)
      - media_category: tweet_image, tweet_video, tweet_gif, etc.
      - media_type: MIME type (image/jpeg, image/png, etc.)
      
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
    
    (* Base64 encode the media data - this is more reliable per community feedback *)
    let media_b64 = Base64.encode_exn media_data in
    
    (* Build JSON body per X API v2 docs *)
    let body_json = `Assoc [
      ("media", `String media_b64);
      ("media_category", `String media_category);
      ("media_type", `String mime_type);
    ] in
    let body = Yojson.Basic.to_string body_json in
    
    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
      ("Content-Type", "application/json");
    ] in
    
    Printf.printf "[Twitter] Uploading media to X API v2 (url=%s, category=%s, mime=%s, raw_size=%d, b64_size=%d)\n%!" 
      url media_category mime_type (String.length media_data) (String.length media_b64);
    
    Config.Http.post ~headers ~body url
      (fun response ->
        Printf.printf "[Twitter] Media upload response: status=%d, body=%s\n%!" response.status response.body;
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            (* X API v2 returns: { "data": { "id": "...", "media_key": "..." } } *)
            let media_id = 
              try json |> Yojson.Basic.Util.member "data" |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_string
              with _ -> 
                try json |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_string
                with _ -> json |> Yojson.Basic.Util.member "media_id_string" |> Yojson.Basic.Util.to_string
            in
            Printf.printf "[Twitter] Media upload succeeded: media_id=%s\n%!" media_id;
            (* If alt text provided, update media metadata *)
            (match alt_text with
            | Some alt when String.length alt > 0 ->
                update_media_metadata ~access_token ~media_id ~alt_text:alt
                  (fun () -> on_success media_id)
                  on_error
            | _ -> on_success media_id)
          with e ->
            on_error (Printf.sprintf "Failed to parse media response: %s\nBody: %s" (Printexc.to_string e) response.body)
        else if response.status = 403 then
          (* 403 Forbidden - common causes:
             1. OAuth scopes missing (need tweet.write, users.read)
             2. App permissions not "Read and Write" in Developer Portal
             3. Free tier quota exhausted
             4. Token needs refresh - user should reconnect account *)
          on_error (Printf.sprintf "Twitter media upload forbidden (403). Common causes: 1) App needs 'Read and Write' permissions in Developer Portal, 2) OAuth token missing required scopes (tweet.write), 3) Free tier quota exhausted, 4) Try reconnecting your Twitter account. Details: %s" response.body)
        else
          on_error (Printf.sprintf "Media upload error (%d): %s" response.status response.body))
      on_error
  
  (** Chunked media upload for large files (videos) using X API v2
      
      This implements the 3-phase upload process per X API v2 docs:
      1. INIT - Initialize upload and get media_id (POST /2/media/upload with command=INIT)
      2. APPEND - Upload chunks of data (POST /2/media/upload with command=APPEND)
      3. FINALIZE - Complete the upload (POST /2/media/upload with command=FINALIZE)
      
      Reference: https://docs.x.com/x-api/media/quickstart/media-upload-chunked
  *)
  let upload_media_chunked ~access_token ~media_data ~mime_type ?(alt_text=None) () on_success on_error =
    let url = Printf.sprintf "%s/media/upload" twitter_upload_base in
    let media_category = 
      if String.starts_with ~prefix:"video/" mime_type then "tweet_video" 
      else if mime_type = "image/gif" then "tweet_gif"
      else "tweet_image" in
    let total_bytes = String.length media_data in
    
    Printf.printf "[Twitter] Starting chunked upload (url=%s, category=%s, mime=%s, total_bytes=%d)\n%!" url media_category mime_type total_bytes;
    
    (* Phase 1: INIT - use multipart/form-data per v2 docs *)
    let init_parts = [
      { Social_core.name = "command"; content = "INIT"; filename = None; content_type = None };
      { name = "media_type"; content = mime_type; filename = None; content_type = None };
      { name = "total_bytes"; content = string_of_int total_bytes; filename = None; content_type = None };
      { name = "media_category"; content = media_category; filename = None; content_type = None };
    ] in
    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
    ] in
    
    Config.Http.post_multipart ~headers ~parts:init_parts url
      (fun init_response ->
        Printf.printf "[Twitter] INIT response: status=%d, body=%s\n%!" init_response.status init_response.body;
        if init_response.status >= 200 && init_response.status < 300 then
          try
            let init_json = Yojson.Basic.from_string init_response.body in
            (* v2 returns { "data": { "id": "...", "media_key": "...", "expires_after_secs": ... } } *)
            let media_id_string = 
              try init_json |> Yojson.Basic.Util.member "data" |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_string
              with _ -> init_json |> Yojson.Basic.Util.member "media_id_string" |> Yojson.Basic.Util.to_string
            in
            Printf.printf "[Twitter] INIT succeeded: media_id=%s\n%!" media_id_string;
            
            (* Phase 2: APPEND chunks - use multipart with actual file data *)
            let chunk_size = 5 * 1024 * 1024 in (* 5MB chunks *)
            let rec upload_chunks offset segment_index =
              if offset >= total_bytes then begin
                (* Phase 3: FINALIZE *)
                Printf.printf "[Twitter] All chunks uploaded, sending FINALIZE\n%!";
                let finalize_parts = [
                  { Social_core.name = "command"; content = "FINALIZE"; filename = None; content_type = None };
                  { name = "media_id"; content = media_id_string; filename = None; content_type = None };
                ] in
                
                Config.Http.post_multipart ~headers ~parts:finalize_parts url
                  (fun finalize_response ->
                    Printf.printf "[Twitter] FINALIZE response: status=%d, body=%s\n%!" finalize_response.status finalize_response.body;
                    if finalize_response.status >= 200 && finalize_response.status < 300 then
                      (* Optionally add alt text *)
                      (match alt_text with
                       | Some text ->
                           update_media_metadata ~access_token ~media_id:media_id_string ~alt_text:text
                             (fun () -> on_success media_id_string)
                             on_error
                       | None -> on_success media_id_string)
                    else
                      on_error (Printf.sprintf "Failed to finalize upload (%d): %s" 
                        finalize_response.status finalize_response.body))
                  on_error
              end else begin
                (* Upload next chunk *)
                let remaining = total_bytes - offset in
                let current_chunk_size = min chunk_size remaining in
                let chunk = String.sub media_data offset current_chunk_size in
                
                Printf.printf "[Twitter] Uploading chunk %d (offset=%d, size=%d)\n%!" segment_index offset current_chunk_size;
                
                (* v2 APPEND uses multipart with raw media data *)
                let append_parts = [
                  { Social_core.name = "command"; content = "APPEND"; filename = None; content_type = None };
                  { name = "media_id"; content = media_id_string; filename = None; content_type = None };
                  { name = "segment_index"; content = string_of_int segment_index; filename = None; content_type = None };
                  { name = "media"; content = chunk; filename = Some "chunk"; content_type = Some "application/octet-stream" };
                ] in
                
                Config.Http.post_multipart ~headers ~parts:append_parts url
                  (fun append_response ->
                    Printf.printf "[Twitter] APPEND response: status=%d\n%!" append_response.status;
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
  
  (** Post single tweet *)
  let post_single ~account_id ~text ~media_urls ?(alt_texts=[]) on_success on_error =
    match check_rate_limit () with
    | Error msg -> on_error msg
    | Ok () ->
        ensure_valid_token ~account_id
          (fun access_token ->
            (* Helper to fetch media with retry logic *)
            let fetch_media_with_retry url max_retries on_success on_err =
              let rec attempt retry_count =
                Config.Http.get url
                  (fun media_resp ->
                    if media_resp.status >= 200 && media_resp.status < 300 then
                      on_success media_resp
                    else if retry_count < max_retries && 
                            (media_resp.status >= 500 || media_resp.status = 429) then
                      (* Retry on server errors and rate limits *)
                      attempt (retry_count + 1)
                    else
                      on_err (Printf.sprintf "Failed to fetch media from %s (status: %d, retries: %d)" 
                        url media_resp.status retry_count))
                  (fun err ->
                    if retry_count < max_retries then
                      attempt (retry_count + 1)
                    else
                      on_err (Printf.sprintf "Failed to fetch media from %s after %d retries: %s" 
                        url max_retries err))
              in
              attempt 0
            in
            
            (* Pair URLs with alt text - use None if alt text list is shorter *)
            let urls_with_alt = List.mapi (fun i url ->
              let alt_text = try List.nth alt_texts i with _ -> None in
              (url, alt_text)
            ) media_urls in
            
            (* Helper to upload multiple media in sequence *)
            let rec upload_media_seq urls_with_alt acc on_complete on_err =
              match urls_with_alt with
              | [] -> on_complete (List.rev acc)
              | (url, alt_text) :: rest ->
                  (* Fetch media from URL with retry *)
                  fetch_media_with_retry url 3
                    (fun media_resp ->
                      let mime_type = 
                        List.assoc_opt "content-type" media_resp.headers 
                        |> Option.value ~default:"image/jpeg"
                      in
                      (* Upload to Twitter with alt text *)
                      upload_media ~access_token ~media_data:media_resp.body ~mime_type ~alt_text
                        (fun media_id -> upload_media_seq rest (media_id :: acc) on_complete on_err)
                        on_err)
                    on_err
            in
            
            (* Upload media if provided (max 4) *)
            let media_to_upload = List.filteri (fun i _ -> i < 4) urls_with_alt in
            upload_media_seq media_to_upload []
              (fun media_ids ->
                (* Create tweet *)
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
                        on_success tweet_id
                      with e ->
                        on_error (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e))
                    else if response.status = 429 then
                      on_error "Twitter rate limit exceeded"
                    else
                      on_error (Printf.sprintf "Twitter API error (%d): %s" response.status response.body))
                  on_error)
              on_error)
          on_error
  
  (** Post single tweet with pre-uploaded media IDs 
      
      This function is for when media has already been uploaded to Twitter
      and you have the media_ids. Use this when the backend handles media
      upload separately from posting.
  *)
  let post_single_with_media_ids ~account_id ~text ~media_ids on_success on_error =
    match check_rate_limit () with
    | Error msg -> on_error msg
    | Ok () ->
        ensure_valid_token ~account_id
          (fun access_token ->
            (* Create tweet with pre-uploaded media IDs *)
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
                    on_success tweet_id
                  with e ->
                    on_error (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e))
                else if response.status = 429 then
                  on_error "Twitter rate limit exceeded"
                else
                  on_error (Printf.sprintf "Twitter API error (%d): %s" response.status response.body))
              on_error)
          on_error
  
  (** Post thread with pre-uploaded media IDs 
      
      Each text gets paired with its corresponding media_ids list.
      The first tweet in the thread can have media, subsequent tweets
      are replies.
  *)
  let post_thread_with_media_ids ~account_id ~texts ~media_ids_per_post on_success on_error =
    if List.length texts = 0 then
      on_error "No tweets in thread"
    else
      ensure_valid_token ~account_id
        (fun access_token ->
          (* Helper to post tweets in sequence with reply references *)
          let rec post_tweets_seq texts_remaining media_remaining reply_to_id acc_ids =
            match texts_remaining with
            | [] -> on_success (List.rev acc_ids)
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
                        (* Continue with next tweet *)
                        post_tweets_seq rest_texts next_media (Some tweet_id) (tweet_id :: acc_ids)
                      with e ->
                        on_error (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e))
                    else if response.status = 429 then
                      on_error "Twitter rate limit exceeded"
                    else
                      on_error (Printf.sprintf "Twitter API error (%d): %s" response.status response.body))
                  on_error
          in
          
          post_tweets_seq texts media_ids_per_post None [])
        on_error
  
  (** Post thread with media URLs *)
  let post_thread ~account_id ~texts ~media_urls_per_post ?(alt_texts_per_post=[]) on_success on_error =
    if List.length texts = 0 then
      on_error "No tweets in thread"
    else
      ensure_valid_token ~account_id
        (fun access_token ->
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
                    (* Upload with alt text *)
                    upload_media ~access_token ~media_data:media_resp.body ~mime_type ~alt_text
                      (fun media_id -> upload_media_seq rest (media_id :: acc) on_complete on_err)
                      on_err)
                  on_err
          in
          
          (* Pair media URLs with alt text for each post *)
          let media_with_alt_per_post = List.map2 (fun media_urls alt_texts ->
            List.mapi (fun i url ->
              let alt_text = try List.nth alt_texts i with _ -> None in
              (url, alt_text)
            ) media_urls
          ) media_urls_per_post (alt_texts_per_post @ List.init (List.length media_urls_per_post - List.length alt_texts_per_post) (fun _ -> [])) in
          
          (* Post tweets in sequence with reply references *)
          let rec post_tweets_seq texts_remaining media_remaining reply_to_id acc_ids =
            match texts_remaining with
            | [] -> on_success (List.rev acc_ids)
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
                            on_error (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e))
                        else if response.status = 429 then
                          on_error "Twitter rate limit exceeded"
                        else
                          on_error (Printf.sprintf "Twitter API error (%d): %s" response.status response.body))
                      on_error)
                  on_error
          in
          
          post_tweets_seq texts media_with_alt_per_post None [])
        on_error
  
  (** Delete a tweet *)
  let delete_tweet ~account_id ~tweet_id on_success on_error =
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
                  on_success ()
                else
                  on_error "Tweet was not deleted"
              with e ->
                on_error (Printf.sprintf "Failed to parse delete response: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Failed to delete tweet (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Get a tweet by ID with optional expansions and fields *)
  let get_tweet ~account_id ~tweet_id ?(expansions=[]) ?(tweet_fields=[]) () on_success on_error =
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
                on_success json
              with e ->
                on_error (Printf.sprintf "Failed to parse tweet response: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Failed to get tweet (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Search recent tweets *)
  let search_tweets ~account_id ~query ?(max_results=10) ?(next_token=None) ?(expansions=[]) ?(tweet_fields=[]) () on_success on_error =
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
                on_success json
              with e ->
                on_error (Printf.sprintf "Failed to parse search response: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Failed to search tweets (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Get user timeline (tweets by user ID) *)
  let get_user_timeline ~account_id ~user_id ?(max_results=10) ?(pagination_token=None) ?(expansions=[]) ?(tweet_fields=[]) () on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
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
        let url = Printf.sprintf "%s/users/%s/tweets?%s" twitter_api_base user_id query in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
        ] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_success json
              with e ->
                on_error (Printf.sprintf "Failed to parse timeline response: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Failed to get timeline (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Get authenticated user's info *)
  let get_me ~account_id ?(user_fields=[]) () on_success on_error =
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
                on_success json
              with e ->
                on_error (Printf.sprintf "Failed to parse user response: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Failed to get user info (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Get mentions timeline for authenticated user *)
  let get_mentions_timeline ~account_id ?(max_results=10) ?(pagination_token=None) ?(expansions=[]) ?(tweet_fields=[]) () on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (fun me_json ->
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
                      on_success json
                    with e ->
                      on_error (Printf.sprintf "Failed to parse mentions: %s" (Printexc.to_string e))
                  else
                    on_error (Printf.sprintf "Failed to get mentions (%d): %s" response.status response.body))
                on_error
            with e ->
              on_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e)))
          on_error)
      on_error
  
  (** Get home timeline (reverse chronological timeline of tweets from followed users) 
      Note: This endpoint requires OAuth 2.0 with user context *)
  let get_home_timeline ~account_id ?(max_results=10) ?(pagination_token=None) ?(expansions=[]) ?(tweet_fields=[]) () on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (fun me_json ->
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
                      on_success json
                    with e ->
                      on_error (Printf.sprintf "Failed to parse home timeline: %s" (Printexc.to_string e))
                  else
                    on_error (Printf.sprintf "Failed to get home timeline (%d): %s" response.status response.body))
                on_error
            with e ->
              on_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e)))
          on_error)
      on_error
  
  (** Get user by ID *)
  let get_user_by_id ~account_id ~user_id ?(user_fields=[]) () on_success on_error =
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
                on_success json
              with e ->
                on_error (Printf.sprintf "Failed to parse user response: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Failed to get user (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Get user by username *)
  let get_user_by_username ~account_id ~username ?(user_fields=[]) () on_success on_error =
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
                on_success json
              with e ->
                on_error (Printf.sprintf "Failed to parse user response: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Failed to get user (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Follow a user *)
  let follow_user ~account_id ~target_user_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        (* First get authenticated user's ID *)
        get_me ~account_id ()
          (fun me_json ->
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
                    on_success ()
                  else
                    on_error (Printf.sprintf "Failed to follow user (%d): %s" response.status response.body))
                on_error
            with e ->
              on_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e)))
          on_error)
      on_error
  
  (** Unfollow a user *)
  let unfollow_user ~account_id ~target_user_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (fun me_json ->
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
                    on_success ()
                  else
                    on_error (Printf.sprintf "Failed to unfollow user (%d): %s" response.status response.body))
                on_error
            with e ->
              on_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e)))
          on_error)
      on_error
  
  (** Block a user *)
  let block_user ~account_id ~target_user_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (fun me_json ->
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
                    on_success ()
                  else
                    on_error (Printf.sprintf "Failed to block user (%d): %s" response.status response.body))
                on_error
            with e ->
              on_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e)))
          on_error)
      on_error
  
  (** Unblock a user *)
  let unblock_user ~account_id ~target_user_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (fun me_json ->
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
                    on_success ()
                  else
                    on_error (Printf.sprintf "Failed to unblock user (%d): %s" response.status response.body))
                on_error
            with e ->
              on_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e)))
          on_error)
      on_error
  
  (** Mute a user *)
  let mute_user ~account_id ~target_user_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (fun me_json ->
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
                    on_success ()
                  else
                    on_error (Printf.sprintf "Failed to mute user (%d): %s" response.status response.body))
                on_error
            with e ->
              on_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e)))
          on_error)
      on_error
  
  (** Unmute a user *)
  let unmute_user ~account_id ~target_user_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (fun me_json ->
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
                    on_success ()
                  else
                    on_error (Printf.sprintf "Failed to unmute user (%d): %s" response.status response.body))
                on_error
            with e ->
              on_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e)))
          on_error)
      on_error
  
  (** Get followers of a user *)
  let get_followers ~account_id ~user_id ?(max_results=100) ?(pagination_token=None) ?(user_fields=[]) () on_success on_error =
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
                on_success json
              with e ->
                on_error (Printf.sprintf "Failed to parse followers: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Failed to get followers (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Get users that a user is following *)
  let get_following ~account_id ~user_id ?(max_results=100) ?(pagination_token=None) ?(user_fields=[]) () on_success on_error =
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
                on_success json
              with e ->
                on_error (Printf.sprintf "Failed to parse following: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Failed to get following (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Search for users by keyword *)
  let search_users ~account_id ~query ?(max_results=100) ?(pagination_token=None) ?(user_fields=[]) () on_success on_error =
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
                on_success json
              with e ->
                on_error (Printf.sprintf "Failed to parse user search: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Failed to search users (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Like a tweet *)
  let like_tweet ~account_id ~tweet_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (fun me_json ->
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
                    on_success ()
                  else
                    on_error (Printf.sprintf "Failed to like tweet (%d): %s" response.status response.body))
                on_error
            with e ->
              on_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e)))
          on_error)
      on_error
  
  (** Unlike a tweet *)
  let unlike_tweet ~account_id ~tweet_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (fun me_json ->
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
                    on_success ()
                  else
                    on_error (Printf.sprintf "Failed to unlike tweet (%d): %s" response.status response.body))
                on_error
            with e ->
              on_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e)))
          on_error)
      on_error
  
  (** Retweet a tweet *)
  let retweet ~account_id ~tweet_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (fun me_json ->
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
                    on_success ()
                  else
                    on_error (Printf.sprintf "Failed to retweet (%d): %s" response.status response.body))
                on_error
            with e ->
              on_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e)))
          on_error)
      on_error
  
  (** Unretweet (delete retweet) *)
  let unretweet ~account_id ~tweet_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (fun me_json ->
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
                    on_success ()
                  else
                    on_error (Printf.sprintf "Failed to unretweet (%d): %s" response.status response.body))
                on_error
            with e ->
              on_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e)))
          on_error)
      on_error
  
  (** Quote tweet (tweet with quoted tweet reference) *)
  let quote_tweet ~account_id ~text ~quoted_tweet_id ~media_urls on_success on_error =
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
                      upload_media ~access_token ~media_data:media_resp.body ~mime_type
                        (fun media_id -> upload_media_seq rest (media_id :: acc) on_complete on_err)
                        on_err)
                    on_err
            in
            
            let media_to_upload = List.filteri (fun i _ -> i < 4) media_urls in
            upload_media_seq media_to_upload []
              (fun media_ids ->
                let url = Printf.sprintf "%s/tweets" twitter_api_base in
                
                let base_fields = [
                  ("text", `String text);
                  ("quote_tweet_id", `String quoted_tweet_id);
                ] in
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
          on_error
  
  (** Reply to a tweet *)
  let reply_to_tweet ~account_id ~text ~reply_to_tweet_id ~media_urls on_success on_error =
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
                      upload_media ~access_token ~media_data:media_resp.body ~mime_type
                        (fun media_id -> upload_media_seq rest (media_id :: acc) on_complete on_err)
                        on_err)
                    on_err
            in
            
            let media_to_upload = List.filteri (fun i _ -> i < 4) media_urls in
            upload_media_seq media_to_upload []
              (fun media_ids ->
                let url = Printf.sprintf "%s/tweets" twitter_api_base in
                
                let base_fields = [
                  ("text", `String text);
                  ("reply", `Assoc [("in_reply_to_tweet_id", `String reply_to_tweet_id)]);
                ] in
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
          on_error
  
  (** Bookmark a tweet *)
  let bookmark_tweet ~account_id ~tweet_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (fun me_json ->
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
                    on_success ()
                  else
                    on_error (Printf.sprintf "Failed to bookmark tweet (%d): %s" response.status response.body))
                on_error
            with e ->
              on_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e)))
          on_error)
      on_error
  
  (** Remove bookmark from a tweet *)
  let remove_bookmark ~account_id ~tweet_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (fun me_json ->
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
                    on_success ()
                  else
                    on_error (Printf.sprintf "Failed to remove bookmark (%d): %s" response.status response.body))
                on_error
            with e ->
              on_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e)))
          on_error)
      on_error
  
  (** Create a list *)
  let create_list ~account_id ~name ?(description=None) ?(private_list=false) () on_success on_error =
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
                on_success json
              with e ->
                on_error (Printf.sprintf "Failed to parse list response: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Failed to create list (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Update a list *)
  let update_list ~account_id ~list_id ?(name=None) ?(description=None) ?(private_list=None) () on_success on_error =
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
          on_error "No fields to update"
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
                  on_success json
                with e ->
                  on_error (Printf.sprintf "Failed to parse list response: %s" (Printexc.to_string e))
              else
                on_error (Printf.sprintf "Failed to update list (%d): %s" response.status response.body))
            on_error)
      on_error
  
  (** Delete a list *)
  let delete_list ~account_id ~list_id on_success on_error =
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
                  on_success ()
                else
                  on_error "List was not deleted"
              with e ->
                on_error (Printf.sprintf "Failed to parse delete response: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Failed to delete list (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Get a list by ID *)
  let get_list ~account_id ~list_id ?(list_fields=[]) () on_success on_error =
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
                on_success json
              with e ->
                on_error (Printf.sprintf "Failed to parse list response: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Failed to get list (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Add a member to a list *)
  let add_list_member ~account_id ~list_id ~user_id on_success on_error =
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
              on_success ()
            else
              on_error (Printf.sprintf "Failed to add member (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Remove a member from a list *)
  let remove_list_member ~account_id ~list_id ~user_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        let url = Printf.sprintf "%s/lists/%s/members/%s" twitter_api_base list_id user_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_token);
        ] in
        
        Config.Http.delete ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_success ()
            else
              on_error (Printf.sprintf "Failed to remove member (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Get list members *)
  let get_list_members ~account_id ~list_id ?(max_results=100) ?(pagination_token=None) ?(user_fields=[]) () on_success on_error =
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
                on_success json
              with e ->
                on_error (Printf.sprintf "Failed to parse members: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Failed to get members (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Follow a list *)
  let follow_list ~account_id ~list_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (fun me_json ->
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
              on_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e)))
          on_error)
      on_error
  
  (** Unfollow a list *)
  let unfollow_list ~account_id ~list_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (fun me_json ->
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
              on_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e)))
          on_error)
      on_error
  
  (** Get tweets from a list *)
  let get_list_tweets ~account_id ~list_id ?(max_results=100) ?(pagination_token=None) ?(expansions=[]) ?(tweet_fields=[]) () on_success on_error =
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
                on_success json
              with e ->
                on_error (Printf.sprintf "Failed to parse list tweets: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Failed to get list tweets (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Pin a list for authenticated user *)
  let pin_list ~account_id ~list_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (fun me_json ->
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
              on_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e)))
          on_error)
      on_error
  
  (** Unpin a list for authenticated user *)
  let unpin_list ~account_id ~list_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_me ~account_id ()
          (fun me_json ->
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
              on_error (Printf.sprintf "Failed to parse user ID: %s" (Printexc.to_string e)))
          on_error)
      on_error
  
  (** Validate content for Twitter *)
  let validate_content ~text =
    let max_length = 280 in
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
          on_error (Printf.sprintf "Token exchange failed (%d): %s" response.status response.body))
      on_error
end