(** TikTok Content Posting API v1 Client
    
    Full implementation of TikTok's Content Posting API supporting:
    - Direct video upload (FILE_UPLOAD source)
    - Video posting with captions
    - Publish status tracking
    - OAuth 2.0 token management
    
    @see <https://developers.tiktok.com/doc/content-posting-api-get-started>
*)

open Social_provider_core

(** {1 Types} *)

(** Privacy level for posts *)
type privacy_level =
  | PublicToEveryone
  | MutualFollowFriends
  | SelfOnly

let string_of_privacy_level = function
  | PublicToEveryone -> "PUBLIC_TO_EVERYONE"
  | MutualFollowFriends -> "MUTUAL_FOLLOW_FRIENDS"
  | SelfOnly -> "SELF_ONLY"

let privacy_level_of_string = function
  | "PUBLIC_TO_EVERYONE" -> PublicToEveryone
  | "MUTUAL_FOLLOW_FRIENDS" -> MutualFollowFriends
  | "SELF_ONLY" -> SelfOnly
  | _ -> SelfOnly

(** Post information for video uploads *)
type post_info = {
  title : string;  (** Caption with hashtags and mentions *)
  privacy_level : privacy_level;
  disable_duet : bool;
  disable_comment : bool;
  disable_stitch : bool;
  video_cover_timestamp_ms : int option;
}

(** Creator information returned by TikTok *)
type creator_info = {
  creator_avatar_url : string;
  creator_username : string;
  creator_nickname : string;
  privacy_level_options : privacy_level list;
  comment_disabled : bool;
  duet_disabled : bool;
  stitch_disabled : bool;
  max_video_post_duration_sec : int;
}

(** Publish status for tracking upload progress *)
type publish_status =
  | Processing
  | Published of string  (** TikTok video ID *)
  | Failed of { error_code : string; error_message : string }

(** {1 API Endpoints} *)

let api_base_url = "https://open.tiktokapis.com/v2"
let auth_base_url = "https://www.tiktok.com/v2/auth/authorize"

(** {1 Constraints and Validation} *)

let max_video_duration_sec = 600  (* 10 minutes, varies by user *)
let min_video_duration_sec = 3
let max_video_size_bytes = 50 * 1024 * 1024  (* 50MB for FeedMansion, TikTok allows up to 4GB *)
let max_caption_length = 2200
let supported_formats = ["mp4"; "webm"; "mov"]
let min_resolution = 360
let max_resolution = 4096
let min_fps = 23
let max_fps = 60

(** Validate video constraints *)
let validate_video ~duration_sec ~file_size_bytes ~width ~height =
  if duration_sec < min_video_duration_sec then
    Error (Printf.sprintf "Video too short: must be at least %d seconds" min_video_duration_sec)
  else if duration_sec > max_video_duration_sec then
    Error (Printf.sprintf "Video too long: maximum %d seconds" max_video_duration_sec)
  else if file_size_bytes > max_video_size_bytes then
    Error (Printf.sprintf "Video too large: maximum %d MB" (max_video_size_bytes / 1024 / 1024))
  else if width < min_resolution || height < min_resolution then
    Error (Printf.sprintf "Video resolution too low: minimum %dpx" min_resolution)
  else if width > max_resolution || height > max_resolution then
    Error (Printf.sprintf "Video resolution too high: maximum %dpx" max_resolution)
  else
    Ok ()

(** Validate caption *)
let validate_caption text =
  if String.length text > max_caption_length then
    Error (Printf.sprintf "Caption too long: maximum %d characters" max_caption_length)
  else
    Ok ()

(** {1 Helper Functions} *)

(** Create post_info with defaults *)
let make_post_info ~title ?(privacy_level=SelfOnly) ?(disable_duet=false) 
    ?(disable_comment=false) ?(disable_stitch=false) ?video_cover_timestamp_ms () =
  { title; privacy_level; disable_duet; disable_comment; disable_stitch; video_cover_timestamp_ms }

(** {1 JSON Serialization} *)

let post_info_to_json info =
  let base = [
    ("title", `String info.title);
    ("privacy_level", `String (string_of_privacy_level info.privacy_level));
    ("disable_duet", `Bool info.disable_duet);
    ("disable_comment", `Bool info.disable_comment);
    ("disable_stitch", `Bool info.disable_stitch);
  ] in
  let with_cover = match info.video_cover_timestamp_ms with
    | Some ms -> ("video_cover_timestamp_ms", `Int ms) :: base
    | None -> base
  in
  `Assoc with_cover

let parse_creator_info json =
  let open Yojson.Basic.Util in
  try
    let data = json |> member "data" in
    let parse_privacy_levels arr =
      arr |> to_list |> List.map (fun p -> privacy_level_of_string (to_string p))
    in
    Ok {
      creator_avatar_url = data |> member "creator_avatar_url" |> to_string;
      creator_username = data |> member "creator_username" |> to_string;
      creator_nickname = data |> member "creator_nickname" |> to_string;
      privacy_level_options = data |> member "privacy_level_options" |> parse_privacy_levels;
      comment_disabled = data |> member "comment_disabled" |> to_bool;
      duet_disabled = data |> member "duet_disabled" |> to_bool;
      stitch_disabled = data |> member "stitch_disabled" |> to_bool;
      max_video_post_duration_sec = data |> member "max_video_post_duration_sec" |> to_int;
    }
  with e ->
    Error (Printf.sprintf "Failed to parse creator info: %s" (Printexc.to_string e))

let parse_publish_status json =
  let open Yojson.Basic.Util in
  try
    let data = json |> member "data" in
    let status = data |> member "status" |> to_string in
    match status with
    | "PROCESSING_DOWNLOAD" | "PROCESSING_UPLOAD" -> Processing
    | "PUBLISH_COMPLETE" -> 
        let video_id = data |> member "publicaly_available_post_id" |> to_list |> List.hd |> member "id" |> to_string in
        Published video_id
    | "FAILED" ->
        let fail_reason = data |> member "fail_reason" |> to_string in
        Failed { error_code = "UPLOAD_FAILED"; error_message = fail_reason }
    | _ -> Processing
  with e ->
    Failed { error_code = "PARSE_ERROR"; error_message = Printexc.to_string e }

(** {1 OAuth URL Generation} *)

let get_authorization_url ~client_id ~redirect_uri ~scope ~state =
  let query = Uri.encoded_of_query [
    ("client_key", [client_id]);
    ("redirect_uri", [redirect_uri]);
    ("response_type", ["code"]);
    ("scope", [scope]);
    ("state", [state]);
  ] in
  auth_base_url ^ "?" ^ query

(** {1 Configuration Module Type} *)

module type CONFIG = sig
  module Http : HTTP_CLIENT
  
  val get_env : string -> string option
  val get_credentials : account_id:string -> (credentials -> unit) -> (string -> unit) -> unit
  val update_credentials : account_id:string -> credentials:credentials -> (unit -> unit) -> (string -> unit) -> unit
  val encrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val decrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val update_health_status : account_id:string -> status:string -> error_message:string option -> (unit -> unit) -> (string -> unit) -> unit
end

(** {1 API Client Functor} *)

module Make (Config : CONFIG) = struct
  let token_url = api_base_url ^ "/oauth/token/"
  let creator_info_url = api_base_url ^ "/post/publish/creator_info/query/"
  let video_init_url = api_base_url ^ "/post/publish/video/init/"
  let status_fetch_url = api_base_url ^ "/post/publish/status/fetch/"
  
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
  let refresh_access_token ~refresh_token on_success on_error =
    let client_key = Config.get_env "TIKTOK_CLIENT_KEY" |> Option.value ~default:"" in
    let client_secret = Config.get_env "TIKTOK_CLIENT_SECRET" |> Option.value ~default:"" in
    
    if client_key = "" || client_secret = "" then
      on_error "TikTok OAuth credentials not configured"
    else
      let headers = [
        ("Content-Type", "application/x-www-form-urlencoded");
      ] in
      let body = Uri.encoded_of_query [
        ("client_key", [client_key]);
        ("client_secret", [client_secret]);
        ("grant_type", ["refresh_token"]);
        ("refresh_token", [refresh_token]);
      ] in
      
      Config.Http.post ~headers ~body token_url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let new_access = json |> member "access_token" |> to_string in
              let new_refresh = json |> member "refresh_token" |> to_string in
              let expires_in = json |> member "expires_in" |> to_int in
              let expires_at = 
                let now = Ptime_clock.now () in
                match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
                | Some exp -> Ptime.to_rfc3339 exp
                | None -> Ptime.to_rfc3339 now
              in
              on_success (new_access, new_refresh, expires_at)
            with e ->
              on_error (Printf.sprintf "Failed to parse refresh response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "Token refresh failed (%d): %s" response.status response.body))
        on_error
  
  (** Ensure valid access token, refreshing if needed *)
  let ensure_valid_token ~account_id on_success on_error =
    Config.get_credentials ~account_id
      (fun creds ->
        (* TikTok tokens expire after 24 hours, refresh 1 hour before *)
        if is_token_expired_buffer ~buffer_seconds:3600 creds.expires_at then
          match creds.refresh_token with
          | None ->
              Config.update_health_status ~account_id ~status:"token_expired"
                ~error_message:(Some "No refresh token available")
                (fun () -> on_error "No refresh token - please reconnect TikTok account")
                on_error
          | Some rt ->
              refresh_access_token ~refresh_token:rt
                (fun (new_access, new_refresh, expires_at) ->
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
          Config.update_health_status ~account_id ~status:"healthy" ~error_message:None
            (fun () -> on_success creds.access_token)
            on_error)
      on_error
  
  (** Query creator info to get available privacy options *)
  let get_creator_info ~account_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        let headers = [
          ("Authorization", "Bearer " ^ access_token);
          ("Content-Type", "application/json; charset=UTF-8");
        ] in
        
        Config.Http.post ~headers ~body:"{}" creator_info_url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              match parse_creator_info (Yojson.Basic.from_string response.body) with
              | Ok info -> on_success info
              | Error e -> on_error e
            else
              on_error (Printf.sprintf "Creator info query failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Initialize video upload and get upload URL *)
  let init_video_upload ~account_id ~post_info ~video_size on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        let headers = [
          ("Authorization", "Bearer " ^ access_token);
          ("Content-Type", "application/json; charset=UTF-8");
        ] in
        let body = `Assoc [
          ("post_info", post_info_to_json post_info);
          ("source_info", `Assoc [
            ("source", `String "FILE_UPLOAD");
            ("video_size", `Int video_size);
            ("chunk_size", `Int video_size);  (* Single chunk upload *)
            ("total_chunk_count", `Int 1);
          ]);
        ] |> Yojson.Basic.to_string in
        
        Config.Http.post ~headers ~body video_init_url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let open Yojson.Basic.Util in
                let data = json |> member "data" in
                let publish_id = data |> member "publish_id" |> to_string in
                let upload_url = data |> member "upload_url" |> to_string in
                on_success (publish_id, upload_url)
              with e ->
                on_error (Printf.sprintf "Failed to parse init response: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Video init failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Upload video content to the upload URL *)
  let upload_video_chunk ~upload_url ~video_content on_success on_error =
    let video_size = String.length video_content in
    let headers = [
      ("Content-Type", "video/mp4");
      ("Content-Length", string_of_int video_size);
      ("Content-Range", Printf.sprintf "bytes 0-%d/%d" (video_size - 1) video_size);
    ] in
    
    Config.Http.put ~headers ~body:video_content upload_url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          on_success ()
        else
          on_error (Printf.sprintf "Video upload failed (%d): %s" response.status response.body))
      on_error
  
  (** Check publish status *)
  let check_publish_status ~account_id ~publish_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        let headers = [
          ("Authorization", "Bearer " ^ access_token);
          ("Content-Type", "application/json; charset=UTF-8");
        ] in
        let body = `Assoc [
          ("publish_id", `String publish_id);
        ] |> Yojson.Basic.to_string in
        
        Config.Http.post ~headers ~body status_fetch_url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              let status = parse_publish_status (Yojson.Basic.from_string response.body) in
              on_success status
            else
              on_error (Printf.sprintf "Status check failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Post a video to TikTok (high-level function)
      
      This handles the full upload flow:
      1. Initialize upload
      2. Upload video content
      3. Return publish_id for status tracking
      
      Note: TikTok video publishing is asynchronous. After this returns,
      you should poll check_publish_status until the video is published.
  *)
  let post_video ~account_id ~caption ~video_content 
      ?(privacy_level=SelfOnly) 
      ?(disable_duet=false) 
      ?(disable_comment=false) 
      ?(disable_stitch=false)
      ?video_cover_timestamp_ms
      on_success on_error =
    (* Validate caption *)
    match validate_caption caption with
    | Error e -> on_error e
    | Ok () ->
        let post_info = make_post_info 
          ~title:caption 
          ~privacy_level 
          ~disable_duet 
          ~disable_comment 
          ~disable_stitch
          ?video_cover_timestamp_ms
          ()
        in
        let video_size = String.length video_content in
        
        init_video_upload ~account_id ~post_info ~video_size
          (fun (publish_id, upload_url) ->
            upload_video_chunk ~upload_url ~video_content
              (fun () -> on_success publish_id)
              on_error)
          on_error
  
  (** Post a video from URL (downloads and uploads)
      
      This is a convenience function that:
      1. Downloads video from URL
      2. Uploads to TikTok
      3. Returns publish_id
  *)
  let post_video_from_url ~account_id ~caption ~video_url
      ?(privacy_level=SelfOnly)
      ?(disable_duet=false)
      ?(disable_comment=false) 
      ?(disable_stitch=false)
      ?video_cover_timestamp_ms
      on_success on_error =
    (* Download video *)
    Config.Http.get video_url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          post_video ~account_id ~caption ~video_content:response.body
            ~privacy_level ~disable_duet ~disable_comment ~disable_stitch
            ?video_cover_timestamp_ms
            on_success on_error
        else
          on_error (Printf.sprintf "Failed to download video (%d)" response.status))
      on_error
  
  (** Post single video (matches other provider signatures) *)
  let post_single ~account_id ~text ~media_urls ?(alt_texts=[]) on_success on_error =
    let _ = alt_texts in (* TikTok doesn't support alt text *)
    match media_urls with
    | [] -> on_error "TikTok requires a video - no media provided"
    | video_url :: _ ->
        post_video_from_url ~account_id ~caption:text ~video_url
          on_success on_error
  
  (** Post thread (TikTok doesn't support threads, posts videos separately) *)
  let post_thread ~account_id ~texts ~media_urls_per_post ?(alt_texts_per_post=[]) on_success on_error =
    let _ = alt_texts_per_post in
    let rec post_all acc texts media =
      match texts, media with
      | [], _ | _, [] -> on_success (List.rev acc)
      | text :: rest_texts, urls :: rest_media ->
          (match urls with
           | [] -> on_error "Each TikTok post requires a video"
           | video_url :: _ ->
               post_video_from_url ~account_id ~caption:text ~video_url
                 (fun post_id -> post_all (post_id :: acc) rest_texts rest_media)
                 on_error)
    in
    post_all [] texts media_urls_per_post
  
  (** Exchange authorization code for access token *)
  let exchange_code ~code ~redirect_uri on_success on_error =
    let client_key = Config.get_env "TIKTOK_CLIENT_KEY" |> Option.value ~default:"" in
    let client_secret = Config.get_env "TIKTOK_CLIENT_SECRET" |> Option.value ~default:"" in
    
    if client_key = "" || client_secret = "" then
      on_error "TikTok OAuth credentials not configured"
    else
      let headers = [
        ("Content-Type", "application/x-www-form-urlencoded");
      ] in
      let body = Uri.encoded_of_query [
        ("client_key", [client_key]);
        ("client_secret", [client_secret]);
        ("code", [code]);
        ("grant_type", ["authorization_code"]);
        ("redirect_uri", [redirect_uri]);
      ] in
      
      Config.Http.post ~headers ~body token_url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let access_token = json |> member "access_token" |> to_string in
              let refresh_token = json |> member "refresh_token" |> to_string in
              let expires_in = json |> member "expires_in" |> to_int in
              let expires_at = 
                let now = Ptime_clock.now () in
                match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
                | Some exp -> Ptime.to_rfc3339 exp
                | None -> Ptime.to_rfc3339 now
              in
              let credentials = {
                access_token;
                refresh_token = Some refresh_token;
                expires_at = Some expires_at;
                token_type = "Bearer";
              } in
              on_success credentials
            with e ->
              on_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "Token exchange failed (%d): %s" response.status response.body))
        on_error
  
  (** Get OAuth URL *)
  let get_oauth_url ~redirect_uri ~state ~code_verifier:_ on_success _on_error =
    let client_key = Config.get_env "TIKTOK_CLIENT_KEY" |> Option.value ~default:"" in
    let url = get_authorization_url
      ~client_id:client_key
      ~redirect_uri
      ~scope:"user.info.basic,video.publish"
      ~state
    in
    on_success url
  
  (** Validate content *)
  let validate_content ~text =
    validate_caption text
end
