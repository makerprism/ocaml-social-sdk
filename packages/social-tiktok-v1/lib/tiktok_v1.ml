(** TikTok Content Posting API v1 Client
    
    Full implementation of TikTok's Content Posting API supporting:
    - Direct video upload (FILE_UPLOAD source)
    - Video posting with captions
    - Publish status tracking
    - OAuth 2.0 token management
    
    @see <https://developers.tiktok.com/doc/content-posting-api-get-started>
*)

open Social_core

(** OAuth 2.0 module for TikTok Content Posting API
    
    TikTok uses a standard OAuth 2.0 flow with some unique characteristics:
    
    Key differences from other platforms:
    - Uses "client_key" instead of "client_id" in API parameters
    - Access tokens expire after 24 hours (very short compared to others)
    - Refresh tokens are valid for 365 days
    - No PKCE support
    - Uses TikTok-specific scopes like "video.publish"
    
    Token lifetimes:
    - Access tokens: 24 hours (86400 seconds)
    - Refresh tokens: 365 days
    
    Required environment variables (or pass directly to functions):
    - TIKTOK_CLIENT_KEY: Client key from TikTok Developer Portal
    - TIKTOK_CLIENT_SECRET: Client secret
    - TIKTOK_REDIRECT_URI: Registered callback URL
*)
module OAuth = struct
  (** Scope definitions for TikTok Content Posting API *)
  module Scopes = struct
    (** Scopes for basic user information *)
    let read = [
      "user.info.basic";
    ]
    
    (** Scopes required for video posting *)
    let write = [
      "user.info.basic";
      "video.publish";
    ]
    
    (** All commonly used scopes *)
    let all = [
      "user.info.basic";
      "user.info.profile";
      "user.info.stats";
      "video.list";
      "video.publish";
      "video.upload";
    ]
    
    (** Operations that can be performed with TikTok API *)
    type operation = 
      | Post_video
      | Read_profile
      | Read_videos
      | Read_stats
    
    (** Get scopes required for specific operations *)
    let for_operations ops =
      let base = ["user.info.basic"] in
      let needs_publish = List.exists (fun o -> o = Post_video) ops in
      let needs_profile = List.exists (fun o -> o = Read_profile) ops in
      let needs_videos = List.exists (fun o -> o = Read_videos) ops in
      let needs_stats = List.exists (fun o -> o = Read_stats) ops in
      base @
      (if needs_publish then ["video.publish"] else []) @
      (if needs_profile then ["user.info.profile"] else []) @
      (if needs_videos then ["video.list"] else []) @
      (if needs_stats then ["user.info.stats"] else [])
  end
  
  (** Platform metadata for TikTok OAuth *)
  module Metadata = struct
    (** TikTok does NOT support PKCE *)
    let supports_pkce = false
    
    (** TikTok supports token refresh *)
    let supports_refresh = true
    
    (** Access tokens expire after 24 hours *)
    let access_token_seconds = Some 86400
    
    (** Refresh tokens expire after 365 days *)
    let refresh_token_seconds = Some 31536000
    
    (** Recommended buffer before expiry (1 hour for access tokens) *)
    let refresh_buffer_seconds = 3600
    
    (** Maximum retry attempts for token operations *)
    let max_refresh_attempts = 5
    
    (** Authorization endpoint *)
    let authorization_endpoint = "https://www.tiktok.com/v2/auth/authorize"
    
    (** Token endpoint *)
    let token_endpoint = "https://open.tiktokapis.com/v2/oauth/token/"
    
    (** API base URL *)
    let api_base = "https://open.tiktokapis.com/v2"
  end
  
  (** Generate authorization URL for TikTok OAuth 2.0 flow
      
      Note: TikTok uses "client_key" instead of "client_id".
      
      @param client_key TikTok Client Key (NOT client_id!)
      @param redirect_uri Registered callback URL
      @param state CSRF protection state parameter
      @param scopes OAuth scopes to request (defaults to Scopes.write)
      @return Full authorization URL to redirect user to
  *)
  let get_authorization_url ~client_key ~redirect_uri ~state ?(scopes=Scopes.write) () =
    let scope_str = String.concat "," scopes in
    let params = [
      ("client_key", client_key);
      ("redirect_uri", redirect_uri);
      ("state", state);
      ("scope", scope_str);
      ("response_type", "code");
    ] in
    let query = Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) params) in
    Printf.sprintf "%s?%s" Metadata.authorization_endpoint query
  
  (** Make functor for OAuth operations that need HTTP client *)
  module Make (Http : HTTP_CLIENT) = struct
    (** Exchange authorization code for access token
        
        @param client_key TikTok Client Key
        @param client_secret TikTok Client Secret
        @param redirect_uri Registered callback URL
        @param code Authorization code from callback
        @param on_success Continuation receiving credentials
        @param on_error Continuation receiving error message
    *)
    let exchange_code ?code_verifier ~client_key ~client_secret ~redirect_uri ~code on_success on_error =
      let headers = [
        ("Content-Type", "application/x-www-form-urlencoded");
      ] in
      let base_query = [
        ("client_key", [client_key]);
        ("client_secret", [client_secret]);
        ("code", [code]);
        ("grant_type", ["authorization_code"]);
        ("redirect_uri", [redirect_uri]);
      ] in
      let full_query =
        match code_verifier with
        | Some cv when cv <> "" -> base_query @ [ ("code_verifier", [cv]) ]
        | _ -> base_query
      in
      let body = Uri.encoded_of_query full_query in
      
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
                with _ -> 86400 (* Default to 24 hours *)
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
        
        TikTok access tokens expire after 24 hours. Use this to get a new
        access token using the refresh token (valid for 365 days).
        
        @param client_key TikTok Client Key
        @param client_secret TikTok Client Secret
        @param refresh_token The refresh token from initial auth
        @param on_success Continuation receiving refreshed credentials
        @param on_error Continuation receiving error message
    *)
    let refresh_token ~client_key ~client_secret ~refresh_token on_success on_error =
      let headers = [
        ("Content-Type", "application/x-www-form-urlencoded");
      ] in
      let body = Uri.encoded_of_query [
        ("client_key", [client_key]);
        ("client_secret", [client_secret]);
        ("grant_type", ["refresh_token"]);
        ("refresh_token", [refresh_token]);
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
                with _ -> 86400
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
    
    (** Revoke access token
        
        @param access_token The token to revoke
        @param on_success Continuation called on success
        @param on_error Continuation receiving error message
    *)
    let revoke_token ~access_token on_success on_error =
      let url = Printf.sprintf "%s/oauth/revoke/?access_token=%s" 
        Metadata.api_base access_token in
      
      Http.post ~headers:[] ~body:"" url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            on_success ()
          else
            on_error (Printf.sprintf "Token revocation failed (%d): %s" response.status response.body))
        on_error
    
    (** Get user info using access token
        
        @param access_token Valid access token
        @param on_success Continuation receiving user info as JSON
        @param on_error Continuation receiving error message
    *)
    let get_user_info ~access_token on_success on_error =
      let url = Printf.sprintf "%s/user/info/?fields=open_id,union_id,avatar_url,display_name" 
        Metadata.api_base in
      let headers = [
        ("Authorization", "Bearer " ^ access_token);
      ] in
      
      Http.get ~headers url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              on_success json
            with e ->
              on_error (Printf.sprintf "Failed to parse user info: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "Get user info failed (%d): %s" response.status response.body))
        on_error
  end
end

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

(** Video source type for upload *)
type video_source =
  | File_upload   (** Upload video binary directly *)
  | Pull_from_url (** TikTok pulls video from a URL *)

let string_of_video_source = function
  | File_upload -> "FILE_UPLOAD"
  | Pull_from_url -> "PULL_FROM_URL"

(** Post mode for publishing *)
type post_mode =
  | Direct_post   (** Publish directly to the creator's profile *)
  | Media_upload  (** Upload to the creator's inbox/drafts *)

let string_of_post_mode = function
  | Direct_post -> "DIRECT_POST"
  | Media_upload -> "MEDIA_UPLOAD"

(** Media type for posts *)
type media_type =
  | Video  (** Video content *)
  | Photo  (** Photo/image content *)

let string_of_media_type = function
  | Video -> "VIDEO"
  | Photo -> "PHOTO"

(** Post information for video uploads *)
type post_info = {
  title : string;  (** Caption with hashtags and mentions *)
  privacy_level : privacy_level;
  disable_duet : bool;
  disable_comment : bool;
  disable_stitch : bool;
  video_cover_timestamp_ms : int option;
  is_aigc : bool;  (** Whether the video is AI-generated content *)
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

(** Account-level counters from TikTok analytics endpoints *)
type account_stats = {
  follower_count : int;
  following_count : int;
  likes_count : int;
  video_count : int;
}

(** Engagement metrics for a TikTok video *)
type video_analytics = {
  id : string;
  like_count : int;
  comment_count : int;
  share_count : int;
  view_count : int;
}

(** Aggregated account analytics payload *)
type account_analytics = {
  account : account_stats;
  recent_video_analytics : video_analytics list;
}

let account_metric_names =
  [ "follower_count"; "following_count"; "likes_count"; "video_count" ]

let video_metric_names =
  [ "view_count"; "like_count"; "comment_count"; "share_count" ]

let account_canonical_metric_keys =
  Analytics_normalization.canonical_metric_keys_of_provider_metrics
    ~provider:Analytics_normalization.TikTok
    account_metric_names

let video_canonical_metric_keys =
  Analytics_normalization.canonical_metric_keys_of_provider_metrics
    ~provider:Analytics_normalization.TikTok
    video_metric_names

(** Typed callback aliases for analytics APIs *)
type account_analytics_success_callback = account_analytics -> unit
type post_analytics_success_callback = video_analytics -> unit
type analytics_error_callback = Error_types.error -> unit

let to_canonical_optional_point value_opt =
  match value_opt with
  | Some value -> [ Analytics_types.make_datapoint value ]
  | None -> []

let to_canonical_tiktok_series ?time_range ~scope ~provider_metric points =
  match Analytics_normalization.tiktok_metric_to_canonical provider_metric with
  | Some metric ->
      Some
        (Analytics_types.make_series
           ?time_range
           ~metric
           ~scope
           ~provider_metric
           points)
  | None -> None

let to_canonical_account_stats_series (account_stats : account_stats) =
  [ ("follower_count", to_canonical_optional_point (Some account_stats.follower_count));
    ("following_count", to_canonical_optional_point (Some account_stats.following_count));
    ("likes_count", to_canonical_optional_point (Some account_stats.likes_count));
    ("video_count", to_canonical_optional_point (Some account_stats.video_count)) ]
  |> List.filter_map (fun (provider_metric, points) ->
         to_canonical_tiktok_series
           ~scope:Analytics_types.Account
           ~provider_metric
           points)

let to_canonical_video_analytics_series (video_analytics : video_analytics) =
  [ ("view_count", to_canonical_optional_point (Some video_analytics.view_count));
    ("like_count", to_canonical_optional_point (Some video_analytics.like_count));
    ("comment_count", to_canonical_optional_point (Some video_analytics.comment_count));
    ("share_count", to_canonical_optional_point (Some video_analytics.share_count)) ]
  |> List.filter_map (fun (provider_metric, points) ->
         to_canonical_tiktok_series
           ~scope:Analytics_types.Video
           ~provider_metric
           points)

let to_canonical_recent_video_totals_series recent_video_analytics =
  let has_values = recent_video_analytics <> [] in
  let total getter =
    List.fold_left (fun acc video -> acc + getter video) 0 recent_video_analytics
  in
  let points value =
    if has_values then [ Analytics_types.make_datapoint value ] else []
  in
  [ ("view_count", points (total (fun video -> video.view_count)));
    ("like_count", points (total (fun video -> video.like_count)));
    ("comment_count", points (total (fun video -> video.comment_count)));
    ("share_count", points (total (fun video -> video.share_count))) ]
  |> List.filter_map (fun (provider_metric, value_points) ->
         to_canonical_tiktok_series
           ~scope:Analytics_types.Video
           ~provider_metric
           value_points)

let to_canonical_account_analytics_series (analytics : account_analytics) =
  to_canonical_account_stats_series analytics.account
  @ to_canonical_recent_video_totals_series analytics.recent_video_analytics

(** {1 API Endpoints} *)

let api_base_url = "https://open.tiktokapis.com/v2"
let auth_base_url = "https://www.tiktok.com/v2/auth/authorize"

(** {1 Constraints and Validation} *)

let max_video_duration_sec = 600  (* 10 minutes, varies by user *)
let min_video_duration_sec = 3
let max_video_size_bytes = 4_294_967_296  (* 4GB - TikTok's actual limit *)
let default_upload_chunk_size_bytes = 10 * 1024 * 1024  (* 10MB *)
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
    ?(disable_comment=false) ?(disable_stitch=false) ?video_cover_timestamp_ms ?(is_aigc=false) () =
  { title; privacy_level; disable_duet; disable_comment; disable_stitch; video_cover_timestamp_ms; is_aigc }

(** {1 JSON Serialization} *)

let post_info_to_json info =
  let base = [
    ("title", `String info.title);
    ("privacy_level", `String (string_of_privacy_level info.privacy_level));
    ("disable_duet", `Bool info.disable_duet);
    ("disable_comment", `Bool info.disable_comment);
    ("disable_stitch", `Bool info.disable_stitch);
    ("is_aigc", `Bool info.is_aigc);
  ] in
  let with_cover = match info.video_cover_timestamp_ms with
    | Some ms -> ("video_cover_timestamp_ms", `Int ms) :: base
    | None -> base
  in
  `Assoc with_cover

(** Photo post information *)
type photo_post_info = {
  title : string;  (** Caption with hashtags and mentions *)
  privacy_level : privacy_level;
  disable_comment : bool;
  is_aigc : bool;  (** Whether the content is AI-generated *)
  photo_images : string list;  (** List of photo image URLs *)
  photo_cover_index : int;  (** Index of the cover photo in photo_images *)
  post_mode : post_mode;  (** DIRECT_POST or MEDIA_UPLOAD *)
}

(** Create photo_post_info with defaults *)
let make_photo_post_info ~title ~photo_images ?(privacy_level=SelfOnly)
    ?(disable_comment=false) ?(is_aigc=false) ?(photo_cover_index=0)
    ?(post_mode=Direct_post) () =
  { title; privacy_level; disable_comment; is_aigc; photo_images;
    photo_cover_index; post_mode }

let photo_post_info_to_json info =
  `Assoc [
    ("post_info", `Assoc [
      ("title", `String info.title);
      ("privacy_level", `String (string_of_privacy_level info.privacy_level));
      ("disable_comment", `Bool info.disable_comment);
      ("is_aigc", `Bool info.is_aigc);
    ]);
    ("source_info", `Assoc [
      ("source", `String "PULL_FROM_URL");
      ("photo_images", `List (List.map (fun url -> `String url) info.photo_images));
      ("photo_cover_index", `Int info.photo_cover_index);
    ]);
    ("post_mode", `String (string_of_post_mode info.post_mode));
    ("media_type", `String "PHOTO");
  ]

let parse_creator_info json =
  let open Yojson.Basic.Util in
  try
    let data = json |> member "data" in
    let privacy_level_of_string_opt = function
      | "PUBLIC_TO_EVERYONE" -> Some PublicToEveryone
      | "MUTUAL_FOLLOW_FRIENDS" -> Some MutualFollowFriends
      | "SELF_ONLY" -> Some SelfOnly
      | _ -> None
    in
    let read_string_field field default =
      try
        let value = data |> member field in
        if value = `Null then default else to_string value
      with _ -> default
    in
    let read_bool_field field default =
      try
        let value = data |> member field in
        if value = `Null then default else to_bool value
      with _ -> default
    in
    let read_int_field field default =
      try
        let value = data |> member field in
        if value = `Null then default else to_int value
      with _ -> default
    in
    let parse_privacy_levels field =
      try
        let arr = data |> member field in
        let parsed =
          arr
          |> to_list
          |> List.filter_map (fun p -> privacy_level_of_string_opt (to_string p))
        in
        if parsed = [] then [SelfOnly] else parsed
      with _ -> [SelfOnly]
    in
    Ok {
      creator_avatar_url = read_string_field "creator_avatar_url" "";
      creator_username = read_string_field "creator_username" "";
      creator_nickname = read_string_field "creator_nickname" "";
      privacy_level_options = parse_privacy_levels "privacy_level_options";
      comment_disabled = read_bool_field "comment_disabled" false;
      duet_disabled = read_bool_field "duet_disabled" false;
      stitch_disabled = read_bool_field "stitch_disabled" false;
      max_video_post_duration_sec = read_int_field "max_video_post_duration_sec" max_video_duration_sec;
    }
  with e ->
    Error (Printf.sprintf "Failed to parse creator info: %s" (Printexc.to_string e))

let parse_publish_status json =
  let open Yojson.Basic.Util in
  let get_first_published_id data =
    let extract_id value =
      match value with
      | `String s -> Some s
      | `Assoc _ -> (try Some (value |> member "id" |> to_string) with _ -> None)
      | `List (`String s :: _) -> Some s
      | `List (first :: _) -> (try Some (first |> member "id" |> to_string) with _ -> None)
      | _ -> None
    in
    let rec try_fields = function
      | [] -> None
      | field :: rest ->
          let value = data |> member field in
          if value = `Null then try_fields rest
          else
            match extract_id value with
            | Some id -> Some id
            | None -> try_fields rest
    in
    try_fields [
      "publicaly_available_post_id";
      "publicly_available_post_id";
      "public_available_post_id";
      "video_id";
      "post_id";
    ]
  in
  let get_failure_message data =
    let from_field field =
      try
        let value = data |> member field in
        if value = `Null then None else Some (to_string value)
      with _ -> None
    in
    match from_field "fail_reason" with
    | Some msg -> msg
    | None ->
        (match from_field "error_message" with
         | Some msg -> msg
         | None -> "TikTok publish failed")
  in
  try
    let data = json |> member "data" in
    let status = data |> member "status" |> to_string in
    match status with
    | "PROCESSING_DOWNLOAD" | "PROCESSING_UPLOAD" -> Processing
    | "PUBLISH_COMPLETE" ->
        (match get_first_published_id data with
         | Some video_id -> Published video_id
         | None -> Failed { error_code = "PARSE_ERROR"; error_message = "PUBLISH_COMPLETE without post ID" })
    | "FAILED" ->
        let fail_reason = get_failure_message data in
        Failed { error_code = "UPLOAD_FAILED"; error_message = fail_reason }
    | _ -> Processing
  with e ->
    Failed { error_code = "PARSE_ERROR"; error_message = Printexc.to_string e }

let read_json_int_field ~obj ~field ~default =
  let open Yojson.Basic.Util in
  let value_to_int value =
    match value with
    | `Int i -> Some i
    | `String s -> int_of_string_opt s
    | _ -> None
  in
  try
    let value = obj |> member field in
    if value = `Null then default
    else match value_to_int value with Some i -> i | None -> default
  with _ -> default

let parse_account_stats_response json =
  let open Yojson.Basic.Util in
  try
    let data = json |> member "data" in
    let user =
      let user_obj = data |> member "user" in
      if user_obj = `Null then data else user_obj
    in
    Ok {
      follower_count = read_json_int_field ~obj:user ~field:"follower_count" ~default:0;
      following_count = read_json_int_field ~obj:user ~field:"following_count" ~default:0;
      likes_count = read_json_int_field ~obj:user ~field:"likes_count" ~default:0;
      video_count = read_json_int_field ~obj:user ~field:"video_count" ~default:0;
    }
  with e ->
    Error (Printf.sprintf "Failed to parse account stats: %s" (Printexc.to_string e))

let parse_video_ids_response json =
  let open Yojson.Basic.Util in
  try
    let data = json |> member "data" in
    let videos = data |> member "videos" |> to_list in
    let ids =
      videos
      |> List.filter_map (fun video ->
           try
             let id = video |> member "id" |> to_string in
             if id = "" then None else Some id
           with _ -> None)
    in
    Ok ids
  with e ->
    Error (Printf.sprintf "Failed to parse video IDs: %s" (Printexc.to_string e))

let parse_video_analytics_response json =
  let open Yojson.Basic.Util in
  try
    let data = json |> member "data" in
    let videos = data |> member "videos" |> to_list in
    let analytics =
      videos
      |> List.filter_map (fun video ->
           try
             let id = video |> member "id" |> to_string in
             if id = "" then None
             else
               Some {
                 id;
                 like_count = read_json_int_field ~obj:video ~field:"like_count" ~default:0;
                 comment_count = read_json_int_field ~obj:video ~field:"comment_count" ~default:0;
                 share_count = read_json_int_field ~obj:video ~field:"share_count" ~default:0;
                 view_count = read_json_int_field ~obj:video ~field:"view_count" ~default:0;
               }
           with _ -> None)
    in
    Ok analytics
  with e ->
    Error (Printf.sprintf "Failed to parse video analytics: %s" (Printexc.to_string e))

let parse_single_video_analytics_response json =
  match parse_video_analytics_response json with
  | Error _ as err -> err
  | Ok (first :: _) -> Ok first
  | Ok [] -> Error "No video analytics returned"

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
  let content_publish_url = api_base_url ^ "/post/publish/content/init/"
  let inbox_video_init_url = api_base_url ^ "/post/publish/inbox/video/init/"
  let status_fetch_url = api_base_url ^ "/post/publish/status/fetch/"
  let user_info_analytics_url = api_base_url ^ "/user/info/?fields=follower_count,following_count,likes_count,video_count"
  let video_list_url = api_base_url ^ "/video/list/?fields=id"
  let video_query_url = api_base_url ^ "/video/query/?fields=id,like_count,comment_count,share_count,view_count"
  
  (** {1 Validation Functions} *)
  
  (** Validate a single post's content *)
  let validate_post ~text ?(media_count=0) () =
    let errors = ref [] in
    
    (* Check caption length *)
    let text_len = String.length text in
    if text_len > max_caption_length then
      errors := Error_types.Text_too_long { length = text_len; max = max_caption_length } :: !errors;
    
    (* TikTok requires exactly one video per post *)
    if media_count < 1 then
      errors := Error_types.Media_required :: !errors;
    if media_count > 1 then
      errors := Error_types.Too_many_media { count = media_count; max = 1 } :: !errors;
    
    if !errors = [] then Ok ()
    else Error (List.rev !errors)
  
  (** Validate thread content *)
  let validate_thread ~texts ~media_counts () =
    let text_count = List.length texts in
    let media_count = List.length media_counts in
    if text_count = 0 then
      Error [Error_types.Thread_empty]
    else if text_count <> media_count then
      let mismatch_error =
        if media_count < text_count then Error_types.Media_required else Error_types.Text_empty
      in
      Error [Error_types.Thread_post_invalid {
        index = min text_count media_count;
        errors = [mismatch_error];
      }]
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

  let is_valid_http_url url =
    try
      let uri = Uri.of_string url in
      match Uri.scheme uri, Uri.host uri with
      | Some scheme, Some host when host <> "" ->
          let normalized = String.lowercase_ascii scheme in
          normalized = "http" || normalized = "https"
      | _ -> false
    with _ -> false
  
  (** Parse API error response and return structured Error_types.error *)
  let parse_api_error ~status_code ~response_body ?(response_headers=[]) ?(required_scopes=["video.publish"]) () =
    let lowercase s = String.lowercase_ascii s in
    let find_header name =
      let target = lowercase name in
      let rec loop = function
        | [] -> None
        | (k, v) :: rest ->
            if lowercase k = target then Some v else loop rest
      in
      loop response_headers
    in
    let request_id =
      match find_header "x-tt-logid" with
      | Some v -> Some v
      | None ->
          (match find_header "x-request-id" with
           | Some v -> Some v
           | None -> find_header "x-tt-trace-id")
    in
    let retry_after_seconds =
      match find_header "retry-after" with
      | Some s -> (match int_of_string_opt s with Some n -> Some n | None -> Some 60)
      | None -> Some 60
    in
    let parsed_code, parsed_msg =
      try
        let json = Yojson.Basic.from_string response_body in
        let open Yojson.Basic.Util in
        let value_to_string = function
          | `String s -> Some s
          | `Int i -> Some (string_of_int i)
          | `Float f -> Some (string_of_float f)
          | _ -> None
        in
        let read_string_field obj field =
          try
            let value = obj |> member field in
            if value = `Null then None else value_to_string value
          with _ -> None
        in
        let error_obj = json |> member "error" in
        let code =
          match read_string_field error_obj "code" with
          | Some v -> Some v
          | None ->
              (match read_string_field json "code" with
               | Some v -> Some v
               | None -> read_string_field json "error_code")
        in
        let msg =
          match read_string_field error_obj "message" with
          | Some v -> Some v
          | None ->
              (match read_string_field json "message" with
               | Some v -> Some v
               | None -> read_string_field json "error_message")
        in
        (code, msg)
      with _ ->
        (None, None)
    in
    let error_msg =
      match parsed_code, parsed_msg with
      | Some c, Some m -> Printf.sprintf "%s: %s" c m
      | Some c, None -> c
      | None, Some m -> m
      | None, None -> response_body
    in
    let has_substring haystack needle =
      let h_len = String.length haystack in
      let n_len = String.length needle in
      let rec loop i =
        if i + n_len > h_len then false
        else if String.sub haystack i n_len = needle then true
        else loop (i + 1)
      in
      if n_len = 0 then true else loop 0
    in
    let lower_error_signals = String.lowercase_ascii error_msg in
    if status_code = 401 then
      if has_substring lower_error_signals "expired" || has_substring lower_error_signals "expire" then
        Error_types.Auth_error Error_types.Token_expired
      else
        Error_types.Auth_error Error_types.Token_invalid
    else if status_code = 403 then
      Error_types.Auth_error (Error_types.Insufficient_permissions required_scopes)
    else if status_code = 404 then
      Error_types.Resource_not_found (if error_msg = "" then "TikTok resource" else error_msg)
    else if status_code = 429 then
      Error_types.Rate_limited {
        retry_after_seconds;
        limit = None;
        remaining = Some 0;
        reset_at = None;
      }
    else
      Error_types.Api_error {
        status_code;
        message = error_msg;
        platform = Platform_types.TikTok;
        raw_response = Some response_body;
        request_id;
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
              let new_refresh =
                try json |> member "refresh_token" |> to_string
                with _ -> refresh_token
              in
              let expires_in =
                try json |> member "expires_in" |> to_int
                with _ -> 86400
              in
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
                (fun () -> on_error (Error_types.Auth_error Error_types.Missing_credentials))
                (fun _ -> on_error (Error_types.Auth_error Error_types.Missing_credentials))
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
                        (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err))))
                    (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err))))
                (fun err ->
                  Config.update_health_status ~account_id ~status:"refresh_failed"
                    ~error_message:(Some err)
                    (fun () -> on_error (Error_types.Auth_error (Error_types.Refresh_failed err)))
                    (fun _ -> on_error (Error_types.Auth_error (Error_types.Refresh_failed err))))
        else
          Config.update_health_status ~account_id ~status:"healthy" ~error_message:None
            (fun () -> on_success creds.access_token)
            (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err))))
      (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err)))
  
  (** Query creator info to get available privacy options *)
  let get_creator_info ~account_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let headers = [
          ("Authorization", "Bearer " ^ access_token);
          ("Content-Type", "application/json; charset=UTF-8");
        ] in
        
        Config.Http.post ~headers ~body:"{}" creator_info_url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              (try
                 match parse_creator_info (Yojson.Basic.from_string response.body) with
                 | Ok info -> on_result (Ok info)
                 | Error e -> on_result (Error (Error_types.Internal_error e))
               with e ->
                 on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse creator info response: %s" (Printexc.to_string e)))))
            else
              on_result (Error (parse_api_error ~status_code:response.status ~response_body:response.body ~response_headers:response.headers ~required_scopes:["user.info.basic"] ())))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))
  
  (** Initialize video upload and get upload URL *)
  let init_video_upload ~account_id ~post_info ~video_size ?chunk_size ?total_chunk_count on_result =
    let resolved_chunk_size =
      match chunk_size with
      | Some cs when cs > 0 -> cs
      | _ -> video_size
    in
    let resolved_total_chunk_count =
      match total_chunk_count with
      | Some c when c > 0 -> c
      | _ -> 1
    in
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
            ("chunk_size", `Int resolved_chunk_size);
            ("total_chunk_count", `Int resolved_total_chunk_count);
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
                on_result (Ok (publish_id, upload_url))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse init response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~status_code:response.status ~response_body:response.body ~response_headers:response.headers ~required_scopes:["video.publish"] ())))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  (** Initialize video upload via PULL_FROM_URL - TikTok downloads the video from the given URL *)
  let init_video_pull_from_url ~account_id ~post_info ~video_url on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let headers = [
          ("Authorization", "Bearer " ^ access_token);
          ("Content-Type", "application/json; charset=UTF-8");
        ] in
        let body = `Assoc [
          ("post_info", post_info_to_json post_info);
          ("source_info", `Assoc [
            ("source", `String "PULL_FROM_URL");
            ("video_url", `String video_url);
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
                on_result (Ok publish_id)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse pull-from-url init response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~status_code:response.status ~response_body:response.body ~response_headers:response.headers ~required_scopes:["video.publish"] ())))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  (** Initialize a photo post via content/init endpoint *)
  let init_photo_post ~account_id ~photo_post_info on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let headers = [
          ("Authorization", "Bearer " ^ access_token);
          ("Content-Type", "application/json; charset=UTF-8");
        ] in
        let body = photo_post_info_to_json photo_post_info |> Yojson.Basic.to_string in

        Config.Http.post ~headers ~body content_publish_url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let open Yojson.Basic.Util in
                let data = json |> member "data" in
                let publish_id = data |> member "publish_id" |> to_string in
                on_result (Ok publish_id)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse photo post init response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~status_code:response.status ~response_body:response.body ~response_headers:response.headers ~required_scopes:["video.publish"] ())))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  (** Initialize a video upload to the creator's inbox/drafts instead of publishing directly.

      Uses the /v2/post/publish/inbox/video/init/ endpoint with post_mode MEDIA_UPLOAD.
  *)
  let init_inbox_video_upload ~account_id ~post_info ~video_size ?chunk_size ?total_chunk_count on_result =
    let resolved_chunk_size =
      match chunk_size with
      | Some cs when cs > 0 -> cs
      | _ -> video_size
    in
    let resolved_total_chunk_count =
      match total_chunk_count with
      | Some c when c > 0 -> c
      | _ -> 1
    in
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
            ("chunk_size", `Int resolved_chunk_size);
            ("total_chunk_count", `Int resolved_total_chunk_count);
          ]);
        ] |> Yojson.Basic.to_string in

        Config.Http.post ~headers ~body inbox_video_init_url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let open Yojson.Basic.Util in
                let data = json |> member "data" in
                let publish_id = data |> member "publish_id" |> to_string in
                let upload_url = data |> member "upload_url" |> to_string in
                on_result (Ok (publish_id, upload_url))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse inbox init response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~status_code:response.status ~response_body:response.body ~response_headers:response.headers ~required_scopes:["video.publish"] ())))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  (** Upload video content to the upload URL *)
  let upload_video_chunk
      ~upload_url
      ~video_content
      ?(content_type="video/mp4")
      ?(start_byte=0)
      ?end_byte
      ?total_size
      on_result =
    let chunk_size = String.length video_content in
    if chunk_size = 0 then
      on_result (Error (Error_types.Validation_error [Error_types.Media_required]))
    else if start_byte < 0 then
      on_result (Error (Error_types.Internal_error "Invalid upload range: start_byte < 0"))
    else
    let resolved_total_size =
      match total_size with
      | Some s when s > 0 -> s
      | _ -> chunk_size
    in
    let resolved_end_byte_result =
      match end_byte with
      | Some e when e < start_byte -> Error "Invalid upload range: end_byte < start_byte"
      | Some e -> Ok e
      | None -> Ok (start_byte + chunk_size - 1)
    in
    match resolved_end_byte_result with
    | Error msg -> on_result (Error (Error_types.Internal_error msg))
    | Ok resolved_end_byte ->
        let expected_chunk_size = (resolved_end_byte - start_byte) + 1 in
        if expected_chunk_size <> chunk_size then
          on_result (Error (Error_types.Internal_error "Invalid upload range: content length does not match byte range"))
        else if start_byte >= resolved_total_size || resolved_end_byte >= resolved_total_size then
          on_result (Error (Error_types.Internal_error "Invalid upload range: byte range exceeds total size"))
        else
          let headers = [
            ("Content-Type", content_type);
            ("Content-Length", string_of_int chunk_size);
            ("Content-Range", Printf.sprintf "bytes %d-%d/%d" start_byte resolved_end_byte resolved_total_size);
          ] in

          Config.Http.put ~headers ~body:video_content upload_url
            (fun response ->
              if response.status >= 200 && response.status < 300 then
                on_result (Ok ())
              else
                on_result (Error (parse_api_error ~status_code:response.status ~response_body:response.body ~response_headers:response.headers ~required_scopes:["video.upload"] ())))
            (fun err -> on_result (Error (Error_types.Internal_error err)))

  let upload_video_in_chunks ~upload_url ~video_content ?(content_type="video/mp4") ~chunk_size on_result =
    let total_size = String.length video_content in
    let resolved_chunk_size =
      if chunk_size <= 0 then total_size else chunk_size
    in
    if total_size = 0 then
      on_result (Error (Error_types.Validation_error [Error_types.Media_required]))
    else
      let total_chunks = (total_size + resolved_chunk_size - 1) / resolved_chunk_size in
      let rec loop chunk_index start_byte =
        if chunk_index >= total_chunks then
          on_result (Ok ())
        else
          let remaining = total_size - start_byte in
          let current_chunk_size = min resolved_chunk_size remaining in
          let current_end_byte = start_byte + current_chunk_size - 1 in
          let chunk_data = String.sub video_content start_byte current_chunk_size in
          upload_video_chunk
            ~upload_url
            ~video_content:chunk_data
            ~content_type
            ~start_byte
            ~end_byte:current_end_byte
            ~total_size
            (function
              | Ok () -> loop (chunk_index + 1) (current_end_byte + 1)
              | Error e -> on_result (Error e))
      in
      loop 0 0
  
  (** Check publish status *)
  let check_publish_status ~account_id ~publish_id on_result =
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
              (try
                 let status = parse_publish_status (Yojson.Basic.from_string response.body) in
                 on_result (Ok status)
               with e ->
                 on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse publish status response: %s" (Printexc.to_string e)))))
            else
              on_result (Error (parse_api_error ~status_code:response.status ~response_body:response.body ~response_headers:response.headers ~required_scopes:["video.publish"] ())))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  (** Fetch account analytics and recent video engagement metrics.

      Flow:
      1. GET /user/info for account counters
      2. POST /video/list to fetch recent video IDs (max_count=20)
      3. POST /video/query for engagement metrics on those IDs
  *)
  let get_account_analytics ~account_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let auth_headers = [
          ("Authorization", "Bearer " ^ access_token);
        ] in
        let json_headers = auth_headers @ [
          ("Content-Type", "application/json; charset=UTF-8");
        ] in
        Config.Http.get ~headers:auth_headers user_info_analytics_url
          (fun account_response ->
            if account_response.status >= 200 && account_response.status < 300 then
              let parsed_account_stats =
                try
                  let account_json = Yojson.Basic.from_string account_response.body in
                  parse_account_stats_response account_json
                with e ->
                  Error (Printf.sprintf "Failed to parse account analytics response: %s" (Printexc.to_string e))
              in
              (match parsed_account_stats with
               | Error msg -> on_result (Error (Error_types.Internal_error msg))
               | Ok account_stats ->
                   let video_list_body =
                     `Assoc [
                       ("max_count", `Int 20);
                     ]
                     |> Yojson.Basic.to_string
                   in
                   Config.Http.post ~headers:json_headers ~body:video_list_body video_list_url
                     (fun list_response ->
                       if list_response.status >= 200 && list_response.status < 300 then
                         let parsed_video_ids =
                           try
                             let list_json = Yojson.Basic.from_string list_response.body in
                             parse_video_ids_response list_json
                           with e ->
                             Error (Printf.sprintf "Failed to parse video list response: %s" (Printexc.to_string e))
                         in
                         (match parsed_video_ids with
                          | Error msg -> on_result (Error (Error_types.Internal_error msg))
                          | Ok [] ->
                              on_result (Ok { account = account_stats; recent_video_analytics = [] })
                          | Ok video_ids ->
                              let query_body =
                                `Assoc [
                                  ("filters", `Assoc [
                                    ("video_ids", `List (List.map (fun id -> `String id) video_ids));
                                  ]);
                                ]
                                |> Yojson.Basic.to_string
                              in
                              Config.Http.post ~headers:json_headers ~body:query_body video_query_url
                                (fun query_response ->
                                  if query_response.status >= 200 && query_response.status < 300 then
                                    let parsed_video_analytics =
                                      try
                                        let query_json = Yojson.Basic.from_string query_response.body in
                                        parse_video_analytics_response query_json
                                      with e ->
                                        Error (Printf.sprintf "Failed to parse video query response: %s" (Printexc.to_string e))
                                    in
                                    (match parsed_video_analytics with
                                     | Ok recent_video_analytics ->
                                         on_result (Ok { account = account_stats; recent_video_analytics })
                                     | Error msg -> on_result (Error (Error_types.Internal_error msg)))
                                  else
                                    on_result (Error (parse_api_error ~status_code:query_response.status ~response_body:query_response.body ~response_headers:query_response.headers ~required_scopes:["video.list"] ())))
                                (fun err -> on_result (Error (Error_types.Internal_error err))))
                       else
                         on_result (Error (parse_api_error ~status_code:list_response.status ~response_body:list_response.body ~response_headers:list_response.headers ~required_scopes:["video.list"] ())))
                     (fun err -> on_result (Error (Error_types.Internal_error err))))
            else
              on_result (Error (parse_api_error ~status_code:account_response.status ~response_body:account_response.body ~response_headers:account_response.headers ~required_scopes:["user.info.stats"] ())))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  (** Fetch analytics for a single TikTok video. *)
  let get_post_analytics ~account_id ~video_id on_result =
    if String.trim video_id = "" then
      on_result (Error (Error_types.Internal_error "video_id is required"))
    else
      ensure_valid_token ~account_id
        (fun access_token ->
          let headers = [
            ("Authorization", "Bearer " ^ access_token);
            ("Content-Type", "application/json; charset=UTF-8");
          ] in
          let body =
            `Assoc [
              ("filters", `Assoc [
                ("video_ids", `List [`String video_id]);
              ]);
            ]
            |> Yojson.Basic.to_string
          in
          Config.Http.post ~headers ~body video_query_url
            (fun response ->
              if response.status >= 200 && response.status < 300 then
                (try
                   let json = Yojson.Basic.from_string response.body in
                   (match parse_single_video_analytics_response json with
                    | Ok video -> on_result (Ok video)
                    | Error "No video analytics returned" -> on_result (Error (Error_types.Resource_not_found video_id))
                    | Error msg -> on_result (Error (Error_types.Internal_error msg)))
                 with e ->
                   on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse post analytics response: %s" (Printexc.to_string e)))))
              else
                on_result (Error (parse_api_error ~status_code:response.status ~response_body:response.body ~response_headers:response.headers ~required_scopes:["video.list"] ())))
            (fun err -> on_result (Error (Error_types.Internal_error err))))
        (fun err -> on_result (Error err))

  let get_account_analytics_canonical ~account_id on_result =
    get_account_analytics ~account_id
      (function
        | Ok analytics ->
            on_result (Ok (to_canonical_account_analytics_series analytics))
        | Error err -> on_result (Error err))

  let get_post_analytics_canonical ~account_id ~video_id on_result =
    get_post_analytics ~account_id ~video_id
      (function
        | Ok analytics ->
            on_result (Ok (to_canonical_video_analytics_series analytics))
        | Error err -> on_result (Error err))

  (** Callback-style wrapper for get_account_analytics. *)
  let get_account_analytics_callbacks ~account_id
      (on_success : account_analytics_success_callback)
      (on_error : analytics_error_callback) =
    get_account_analytics ~account_id
      (function
        | Ok analytics -> on_success analytics
        | Error err -> on_error err)

  (** Callback-style wrapper for get_post_analytics. *)
  let get_post_analytics_callbacks ~account_id ~video_id
      (on_success : post_analytics_success_callback)
      (on_error : analytics_error_callback) =
    get_post_analytics ~account_id ~video_id
      (function
        | Ok analytics -> on_success analytics
        | Error err -> on_error err)

  let validate_post_info_against_creator ~(post_info : post_info) ~creator_info =
    let privacy_allowed =
      List.exists (fun p -> p = post_info.privacy_level) creator_info.privacy_level_options
    in
    if not privacy_allowed then
      Error (Error_types.Content_policy_violation
               (Printf.sprintf "Privacy level %s not allowed for this creator"
                  (string_of_privacy_level post_info.privacy_level)))
    else if creator_info.comment_disabled && not post_info.disable_comment then
      Error (Error_types.Content_policy_violation
               "Comments are disabled for this creator; post must disable comments")
    else if creator_info.duet_disabled && not post_info.disable_duet then
      Error (Error_types.Content_policy_violation
               "Duet is disabled for this creator; post must disable duet")
    else if creator_info.stitch_disabled && not post_info.disable_stitch then
      Error (Error_types.Content_policy_violation
               "Stitch is disabled for this creator; post must disable stitch")
    else
      Ok ()

  (** Poll publish status until terminal state or timeout.

      Note: this helper retries immediately without sleeping. Callers that need
      timed backoff should orchestrate delays externally between attempts.
  *)
  let wait_for_publish ~account_id ~publish_id ?(max_attempts=30) ?(on_processing=(fun ~attempt:_ ~max_attempts:_ -> ())) on_result =
    if max_attempts <= 0 then
      on_result (Error (Error_types.Internal_error "max_attempts must be > 0"))
    else
      let rec loop attempt =
        check_publish_status ~account_id ~publish_id
          (function
            | Error e -> on_result (Error e)
            | Ok Processing ->
                on_processing ~attempt ~max_attempts;
                if attempt >= max_attempts then
                  on_result (Error (Error_types.Internal_error "Publish status polling timed out"))
                else
                  loop (attempt + 1)
            | Ok (Published _ as status) -> on_result (Ok status)
            | Ok (Failed _ as status) -> on_result (Ok status))
      in
      loop 1
  
  (** Post a video to TikTok (high-level function)
      
      This handles the full upload flow:
      1. Initialize upload
      2. Upload video content
      3. Return publish_id for status tracking
      
      Note: TikTok video publishing is asynchronous. After this returns,
      you should poll check_publish_status until the video is published.
  *)
  let post_video ~account_id ~caption ~video_content
      ?(content_type="video/mp4")
      ?(upload_chunk_size_bytes=default_upload_chunk_size_bytes)
      ?(privacy_level=SelfOnly)
      ?(disable_duet=false)
      ?(disable_comment=false)
      ?(disable_stitch=false)
      ?video_cover_timestamp_ms
      ?(is_aigc=false)
      on_result =
    let caption_len = String.length caption in
    if caption_len > max_caption_length then
      on_result (Error (Error_types.Validation_error [Error_types.Text_too_long { length = caption_len; max = max_caption_length }]))
    else
        let post_info = make_post_info
          ~title:caption
          ~privacy_level
          ~disable_duet
          ~disable_comment
          ~disable_stitch
          ?video_cover_timestamp_ms
          ~is_aigc
          ()
        in
        let video_size = String.length video_content in

        if video_size = 0 then
          on_result (Error (Error_types.Validation_error [Error_types.Media_required]))
        else
          get_creator_info ~account_id
            (function
              | Error e -> on_result (Error e)
              | Ok creator_info ->
                  (match validate_post_info_against_creator ~post_info ~creator_info with
                   | Error e -> on_result (Error e)
                   | Ok () ->
                       let effective_chunk_size =
                         let requested = if upload_chunk_size_bytes <= 0 then video_size else upload_chunk_size_bytes in
                         min requested video_size
                       in
                       let total_chunk_count = (video_size + effective_chunk_size - 1) / effective_chunk_size in
                       init_video_upload
                         ~account_id
                         ~post_info
                         ~video_size
                         ~chunk_size:effective_chunk_size
                         ~total_chunk_count
                         (function
                           | Ok (publish_id, upload_url) ->
                               upload_video_in_chunks ~upload_url ~video_content ~content_type ~chunk_size:effective_chunk_size
                                 (function
                                   | Ok () -> on_result (Ok publish_id)
                                   | Error e -> on_result (Error e))
                           | Error e -> on_result (Error e))))
  
  (** Post a video from URL (downloads and uploads)
      
      This is a convenience function that:
      1. Downloads video from URL
      2. Optionally validates video size
      3. Uploads to TikTok
      4. Returns publish_id
      
      @param validate_before_upload When true, validates video size after download.
             TikTok limits: 50MB default (up to 4GB via API), 3-600s duration.
             Default: false
  *)
  let post_video_from_url ~account_id ~caption ~video_url
      ?(privacy_level=SelfOnly)
      ?(disable_duet=false)
      ?(disable_comment=false)
      ?(disable_stitch=false)
      ?video_cover_timestamp_ms
      ?(is_aigc=false)
      ?(upload_chunk_size_bytes=default_upload_chunk_size_bytes)
      ?(validate_before_upload=false)
      on_result =
    if not (is_valid_http_url video_url) then
      on_result (Error (Error_types.Validation_error [Error_types.Invalid_url video_url]))
    else
    (* Download video *)
    Config.Http.get video_url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          let video_size = String.length response.body in
          let content_type =
            let rec find = function
              | [] -> "video/mp4"
              | (k, v) :: rest ->
                  if String.lowercase_ascii k = "content-type" then v else find rest
            in
            find response.headers
          in
          let normalized_content_type =
            content_type
            |> String.split_on_char ';'
            |> List.hd
            |> String.trim
            |> String.lowercase_ascii
          in
          let effective_content_type =
            if normalized_content_type = "" then "video/mp4" else normalized_content_type
          in
          let content_type_error =
              match effective_content_type with
              | "video/mp4" | "video/webm" | "video/quicktime" | "video/x-quicktime" | "video/mov" -> None
              | other -> Some (Error_types.Media_unsupported_format other)
          in

          (* Validate if requested - only size check since we don't parse video *)
          let validation_error =
            if validate_before_upload && video_size > max_video_size_bytes then
              Some (Error_types.Media_too_large { 
                size_bytes = video_size; 
                max_bytes = max_video_size_bytes 
              })
            else if content_type_error <> None then
              content_type_error
            else
              None
          in
          
          (match validation_error with
          | Some err ->
              on_result (Error (Error_types.Validation_error [err]))
          | None ->
              post_video ~account_id ~caption ~video_content:response.body
                ~content_type:effective_content_type
                ~upload_chunk_size_bytes
                ~privacy_level ~disable_duet ~disable_comment ~disable_stitch
                ?video_cover_timestamp_ms ~is_aigc
                on_result)
        else
          if response.status = 404 then
            on_result (Error (Error_types.Resource_not_found video_url))
          else
            on_result (Error (Error_types.Network_error (Error_types.Connection_failed (Printf.sprintf "Failed to download video (%d)" response.status)))))
      (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err))))
  
  (** Post single video (matches other provider signatures)
      
      @param validate_media_before_upload When true, validates video size after download.
             Default: false
  *)
  let post_single ~account_id ~text ~media_urls ?(alt_texts=[])
      ?(privacy_level=SelfOnly) ?(disable_duet=false) ?(disable_comment=false)
      ?(disable_stitch=false) ?video_cover_timestamp_ms ?(is_aigc=false)
      ?(validate_media_before_upload=false) on_result =
    let _ = alt_texts in (* TikTok doesn't support alt text *)
    let media_count = List.length media_urls in
    match validate_post ~text ~media_count () with
    | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        (* Validation ensures media_urls is non-empty *)
        let video_url = List.hd media_urls in
        if not (is_valid_http_url video_url) then
          on_result (Error_types.Failure (Error_types.Validation_error [Error_types.Invalid_url video_url]))
        else
          post_video_from_url ~account_id ~caption:text ~video_url
            ~privacy_level ~disable_duet ~disable_comment ~disable_stitch
            ?video_cover_timestamp_ms ~is_aigc
            ~validate_before_upload:validate_media_before_upload
            (function
              | Ok publish_id -> on_result (Error_types.Success publish_id)
              | Error err -> on_result (Error_types.Failure err))
  
  (** Post thread (TikTok doesn't support threads, posts videos separately) *)
  let post_thread ~account_id ~texts ~media_urls_per_post ?(alt_texts_per_post=[]) ?(is_aigc=false) on_result =
    let _ = alt_texts_per_post in
    let media_counts = List.map List.length media_urls_per_post in
    match validate_thread ~texts ~media_counts () with
    | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        let url_validation_errors = ref [] in
        List.iteri (fun i urls ->
          match urls with
          | first_url :: _ when not (is_valid_http_url first_url) ->
              url_validation_errors := Error_types.Thread_post_invalid {
                index = i;
                errors = [Error_types.Invalid_url first_url]
              } :: !url_validation_errors
          | _ -> ()
        ) media_urls_per_post;
        if !url_validation_errors <> [] then
          on_result (Error_types.Failure (Error_types.Validation_error (List.rev !url_validation_errors)))
        else
        let total_requested = List.length texts in
        (* Validation ensures each post has at least one video URL *)
        let rec post_all acc post_index texts media =
          match texts, media with
          | [], _ | _, [] -> 
              let thread_result = {
                Error_types.posted_ids = List.rev acc;
                failed_at_index = None;
                total_requested;
              } in
              on_result (Error_types.Success thread_result)
          | text :: rest_texts, urls :: rest_media ->
              (* Validation ensures urls is non-empty *)
              let video_url = List.hd urls in
              post_video_from_url ~account_id ~caption:text ~video_url ~is_aigc
                (function
                  | Ok post_id -> post_all (post_id :: acc) (post_index + 1) rest_texts rest_media
                  | Error err ->
                      let thread_result = {
                        Error_types.posted_ids = List.rev acc;
                        failed_at_index = Some post_index;
                        total_requested;
                      } in
                      if List.length acc > 0 then
                        on_result (Error_types.Partial_success { 
                          result = thread_result;
                          warnings = [Error_types.Generic_warning { code = "thread_incomplete"; message = Error_types.error_to_string err; recoverable = false }]
                        })
                      else
                        on_result (Error_types.Failure err))
        in
        post_all [] 0 texts media_urls_per_post
  
  (** Exchange authorization code for access token *)
  let exchange_code ?code_verifier ~code ~redirect_uri on_result =
    let client_key = Config.get_env "TIKTOK_CLIENT_KEY" |> Option.value ~default:"" in
    let client_secret = Config.get_env "TIKTOK_CLIENT_SECRET" |> Option.value ~default:"" in
    
    if client_key = "" || client_secret = "" then
      on_result (Error (Error_types.Internal_error "TikTok OAuth credentials not configured"))
    else
      let headers = [
        ("Content-Type", "application/x-www-form-urlencoded");
      ] in
      let base_query = [
        ("client_key", [client_key]);
        ("client_secret", [client_secret]);
        ("code", [code]);
        ("grant_type", ["authorization_code"]);
        ("redirect_uri", [redirect_uri]);
      ] in
      let full_query =
        match code_verifier with
        | Some cv when cv <> "" -> base_query @ [ ("code_verifier", [cv]) ]
        | _ -> base_query
      in
      let body = Uri.encoded_of_query full_query in
      
      Config.Http.post ~headers ~body token_url
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
                with _ -> 86400
              in
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
              on_result (Ok credentials)
            with e ->
              on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))))
          else
            on_result (Error (parse_api_error ~status_code:response.status ~response_body:response.body ~response_headers:response.headers ~required_scopes:["user.info.basic"; "video.publish"] ())))
        (fun err -> on_result (Error (Error_types.Internal_error err)))
  
  (** Get OAuth URL *)
  let get_oauth_url ~redirect_uri ~state ~code_verifier on_result =
    let client_key = Config.get_env "TIKTOK_CLIENT_KEY" |> Option.value ~default:"" in
    if client_key = "" then
      on_result (Error (Error_types.Internal_error "TikTok OAuth client key not configured"))
    else
      let url = get_authorization_url
        ~client_id:client_key
        ~redirect_uri
        ~scope:"user.info.basic,video.publish"
        ~state
      in
      if code_verifier = "" then
        on_result (Ok url)
      else
        let uri = Uri.of_string url in
        let query = Uri.query uri @ [
          ("code_challenge", [code_verifier]);
          ("code_challenge_method", ["plain"]);
        ] in
        on_result (Ok (Uri.with_query uri query |> Uri.to_string))
  
  (** Validate content *)
  let validate_content ~text =
    validate_caption text
end
