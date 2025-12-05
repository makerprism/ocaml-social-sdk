(** YouTube Data API v3 Provider
    
    This implementation supports YouTube Shorts uploads.
    
    - Google OAuth 2.0 with PKCE
    - Access tokens expire after 1 hour
    - Refresh tokens don't expire (unless revoked)
    - Resumable upload for videos
*)

open Social_provider_core

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
  let google_oauth_base = "https://oauth2.googleapis.com"
  
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
    Config.get_credentials ~account_id
      (fun creds ->
        (* Check if token needs refresh (10 minute buffer due to short lifetime) *)
        if is_token_expired_buffer ~buffer_seconds:600 creds.expires_at then
          (* Token expiring soon, refresh it *)
          match creds.refresh_token with
          | None ->
              Config.update_health_status ~account_id ~status:"token_expired" 
                ~error_message:(Some "No refresh token available")
                (fun () -> on_error "No refresh token available - please reconnect")
                on_error
          | Some refresh_token ->
              let client_id = Config.get_env "YOUTUBE_CLIENT_ID" |> Option.value ~default:"" in
              let client_secret = Config.get_env "YOUTUBE_CLIENT_SECRET" |> Option.value ~default:"" in
              
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
  
  (** Upload video to YouTube Shorts *)
  let post_single ~account_id ~text ~media_urls on_success on_error =
    if List.length media_urls = 0 then
      on_error "YouTube Shorts requires a vertical video"
    else
      ensure_valid_token ~account_id
        (fun access_token ->
          let video_url = List.hd media_urls in
          
          (* Download video *)
          Config.Http.get ~headers:[] video_url
            (fun video_response ->
              if video_response.status >= 200 && video_response.status < 300 then
                let video_data = video_response.body in
                let content_type = 
                  match List.assoc_opt "content-type" video_response.headers with
                  | Some ct -> ct
                  | None -> "video/mp4"
                in
                
                (* Step 1: Initialize resumable upload with metadata *)
                let video_metadata = `Assoc [
                  ("snippet", `Assoc [
                    ("title", `String (String.sub text 0 (min (String.length text) 100)));
                    ("description", `String (text ^ " #Shorts"));
                    ("tags", `List [`String "shorts"; `String "short"]);
                    ("categoryId", `String "22"); (* People & Blogs *)
                  ]);
                  ("status", `Assoc [
                    ("privacyStatus", `String "public");
                    ("selfDeclaredMadeForKids", `Bool false);
                  ]);
                ] in
                
                let init_url = youtube_upload_base ^ "/videos?uploadType=resumable&part=snippet,status" in
                let init_headers = [
                  ("Authorization", "Bearer " ^ access_token);
                  ("Content-Type", "application/json");
                  ("X-Upload-Content-Length", string_of_int (String.length video_data));
                  ("X-Upload-Content-Type", content_type);
                ] in
                
                let metadata_str = Yojson.Basic.to_string video_metadata in
                
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
                                try
                                  let open Yojson.Basic.Util in
                                  let json = Yojson.Basic.from_string upload_response.body in
                                  let video_id = json |> member "id" |> to_string in
                                  on_success video_id
                                with _e ->
                                  on_error (Printf.sprintf "Failed to parse response: %s" upload_response.body)
                              else
                                on_error (Printf.sprintf "Video upload failed (%d): %s" 
                                  upload_response.status upload_response.body))
                            on_error
                      | None ->
                          on_error (Printf.sprintf "No upload URL in response: %s" init_response.body)
                    else
                      on_error (Printf.sprintf "Upload initialization failed (%d): %s" 
                        init_response.status init_response.body))
                  on_error
              else
                on_error (Printf.sprintf "Failed to download video (%d)" video_response.status))
            on_error)
        on_error
  
  (** Post thread (YouTube doesn't support threads, posts only first item) *)
  let post_thread ~account_id ~texts ~media_urls_per_post on_success on_error =
    if List.length texts = 0 then
      on_error "No content to post"
    else
      let first_text = List.hd texts in
      let first_media = if List.length media_urls_per_post > 0 then List.hd media_urls_per_post else [] in
      post_single ~account_id ~text:first_text ~media_urls:first_media
        (fun video_id -> on_success [video_id])
        on_error
  
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
      
      let url = google_oauth_base ^ "/auth?" ^ query in
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
                token_type = "Bearer";
              } in
              on_success credentials
            with e ->
              on_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "OAuth exchange failed (%d): %s" response.status response.body))
        on_error
    )
  
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
