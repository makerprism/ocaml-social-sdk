(** Platform Capabilities - Comprehensive constraint data for all supported platforms.

    This module is the single source of truth for platform-specific limits:
    text lengths, media constraints, format variants, rate limits, etc.

    Applications should use [get_capability] rather than maintaining their own
    constraint tables. *)

open Platform_types

(** Whether a platform requires media to post *)
type media_requirement =
  | NoMedia          (** Text-only posts are allowed *)
  | MediaRequired    (** At least one image or video is required *)
  | VideoRequired    (** A video is specifically required *)
  | ImageRequired    (** An image is specifically required *)

(** Posting format variants (e.g., Instagram Reels vs feed posts) *)
type posting_format =
  | PostFormat
  | ReelFormat
  | StoryFormat

(** Rich platform capability record *)
type capability = {
  platform : platform;
  display_name : string;
  max_text_length : int;
  requires_title : bool;
  max_title_length : int option;
  supports_threads : bool;
  media_requirement : media_requirement;

  (* Image constraints *)
  image_max_width : int option;
  image_max_height : int option;
  image_formats : string list;
  image_max_size_mb : int option;
  (* Video constraints *)
  video_max_duration_seconds : int option;
  video_formats : string list;
  video_max_size_mb : int option;
  video_aspect_ratios : string list;
  video_max_frame_rate : float option;
  (* Carousel/multi-media limits *)
  max_carousel_items : int option;
  (* Rate limits *)
  posts_per_day : int option;
  posts_per_hour : int option;
}

(* ------------------------------------------------------------------ *)
(*  Per-platform capability records                                    *)
(* ------------------------------------------------------------------ *)

let twitter = {
  platform = Twitter;
  display_name = "Twitter / X";
  max_text_length = 280;
  requires_title = false;
  max_title_length = None;
  supports_threads = true;
  media_requirement = NoMedia;
  image_max_width = Some 4096;
  image_max_height = Some 4096;
  image_formats = ["jpg"; "jpeg"; "png"; "gif"; "webp"];
  image_max_size_mb = Some 5;
  video_max_duration_seconds = Some 140;
  video_formats = ["mp4"; "mov"];
  video_max_size_mb = Some 512;
  video_aspect_ratios = ["16:9"; "1:1"; "9:16"];
  video_max_frame_rate = Some 60.0;
  max_carousel_items = Some 4;
  posts_per_day = Some 15;
  posts_per_hour = None;
}

let linkedin = {
  platform = LinkedIn;
  display_name = "LinkedIn";
  max_text_length = 3000;
  requires_title = false;
  max_title_length = None;
  supports_threads = false;
  media_requirement = NoMedia;
  image_max_width = Some 7680;
  image_max_height = Some 4320;
  image_formats = ["jpg"; "jpeg"; "png"; "gif"];
  image_max_size_mb = Some 10;
  video_max_duration_seconds = Some 600;
  video_formats = ["mp4"];
  video_max_size_mb = Some 200;
  video_aspect_ratios = ["16:9"; "1:1"; "9:16"];
  video_max_frame_rate = Some 60.0;
  max_carousel_items = Some 9;
  posts_per_day = Some 100;
  posts_per_hour = None;
}

let bluesky = {
  platform = Bluesky;
  display_name = "Bluesky";
  max_text_length = 300;
  requires_title = false;
  max_title_length = None;
  supports_threads = true;
  media_requirement = NoMedia;
  image_max_width = Some 2000;
  image_max_height = Some 2000;
  image_formats = ["jpg"; "jpeg"; "png"];
  image_max_size_mb = Some 1;
  video_max_duration_seconds = Some 60;
  video_formats = ["mp4"; "mpeg"; "webm"; "mov"];
  video_max_size_mb = Some 50;
  video_aspect_ratios = [];
  video_max_frame_rate = Some 60.0;
  max_carousel_items = Some 4;
  posts_per_day = None;
  posts_per_hour = None;
}

let pinterest = {
  platform = Pinterest;
  display_name = "Pinterest";
  max_text_length = 500;
  requires_title = false;
  max_title_length = None;
  supports_threads = false;
  media_requirement = MediaRequired;
  image_max_width = Some 4000;
  image_max_height = Some 10000;
  image_formats = ["jpg"; "jpeg"; "png"];
  image_max_size_mb = Some 32;
  video_max_duration_seconds = Some 900;
  video_formats = ["mp4"; "mov"; "m4v"];
  video_max_size_mb = Some 2048;
  video_aspect_ratios = ["2:3"; "1:1"; "9:16"];
  video_max_frame_rate = Some 60.0;
  max_carousel_items = Some 1;
  posts_per_day = Some 100;
  posts_per_hour = None;
}

let youtube_shorts = {
  platform = YouTubeShorts;
  display_name = "YouTube Shorts";
  max_text_length = 5000;
  requires_title = true;
  max_title_length = Some 100;
  supports_threads = false;
  media_requirement = VideoRequired;
  image_max_width = None;
  image_max_height = None;
  image_formats = [];
  image_max_size_mb = None;
  video_max_duration_seconds = Some 60;
  video_formats = ["mp4"; "mov"];
  video_max_size_mb = Some 256;
  video_aspect_ratios = ["9:16"];
  video_max_frame_rate = Some 60.0;
  max_carousel_items = None;
  posts_per_day = None;
  posts_per_hour = None;
}

let mastodon = {
  platform = Mastodon;
  display_name = "Mastodon";
  max_text_length = 500;
  requires_title = false;
  max_title_length = None;
  supports_threads = true;
  media_requirement = NoMedia;
  image_max_width = Some 3840;
  image_max_height = Some 2160;
  image_formats = ["jpg"; "jpeg"; "png"; "gif"; "webp"];
  image_max_size_mb = Some 10;
  video_max_duration_seconds = Some 60;
  video_formats = ["mp4"; "webm"; "mov"];
  video_max_size_mb = Some 40;
  video_aspect_ratios = ["16:9"; "1:1"; "9:16"];
  video_max_frame_rate = Some 60.0;
  max_carousel_items = Some 4;
  posts_per_day = None;
  posts_per_hour = None;
}

let facebook_page = {
  platform = FacebookPage;
  display_name = "Facebook Page";
  max_text_length = 63206;
  requires_title = false;
  max_title_length = None;
  supports_threads = false;
  media_requirement = NoMedia;
  image_max_width = Some 2048;
  image_max_height = Some 2048;
  image_formats = ["jpg"; "jpeg"; "png"; "gif"];
  image_max_size_mb = Some 10;
  video_max_duration_seconds = Some 7200;
  video_formats = ["mp4"; "mov"];
  video_max_size_mb = Some 1024;
  video_aspect_ratios = ["16:9"; "1:1"; "9:16"];
  video_max_frame_rate = Some 60.0;
  max_carousel_items = Some 10;
  posts_per_day = None;
  posts_per_hour = None;
}

let instagram = {
  platform = Instagram;
  display_name = "Instagram";
  max_text_length = 2200;
  requires_title = false;
  max_title_length = None;
  supports_threads = false;
  media_requirement = MediaRequired;
  image_max_width = Some 1440;
  image_max_height = Some 1800;
  image_formats = ["jpg"; "jpeg"; "png"];
  image_max_size_mb = Some 8;
  video_max_duration_seconds = Some 60;
  video_formats = ["mp4"; "mov"];
  video_max_size_mb = Some 100;
  video_aspect_ratios = ["1:1"; "4:5"; "1.91:1"];
  video_max_frame_rate = Some 60.0;
  max_carousel_items = Some 10;
  posts_per_day = Some 25;
  posts_per_hour = Some 25;
}

let tiktok = {
  platform = TikTok;
  display_name = "TikTok";
  max_text_length = 2200;
  requires_title = false;
  max_title_length = None;
  supports_threads = false;
  media_requirement = VideoRequired;
  image_max_width = None;
  image_max_height = None;
  image_formats = ["webp"; "jpeg"];
  image_max_size_mb = Some 20;
  video_max_duration_seconds = Some 600;
  video_formats = ["mp4"; "webm"; "mov"];
  video_max_size_mb = Some 50;
  video_aspect_ratios = ["9:16"; "1:1"; "16:9"];
  video_max_frame_rate = Some 60.0;
  max_carousel_items = None;
  posts_per_day = None;
  posts_per_hour = None;
}

let reddit = {
  platform = Reddit;
  display_name = "Reddit";
  max_text_length = 40000;
  requires_title = true;
  max_title_length = Some 300;
  supports_threads = false;
  media_requirement = NoMedia;
  image_max_width = None;
  image_max_height = None;
  image_formats = ["jpg"; "jpeg"; "png"; "gif"];
  image_max_size_mb = Some 20;
  video_max_duration_seconds = Some 900;
  video_formats = ["mp4"];
  video_max_size_mb = Some 1000;
  video_aspect_ratios = ["16:9"; "1:1"; "9:16"; "4:3"];
  video_max_frame_rate = Some 60.0;
  max_carousel_items = Some 20;
  posts_per_day = None;
  posts_per_hour = None;
}

let threads = {
  platform = Threads;
  display_name = "Threads";
  max_text_length = 500;
  requires_title = false;
  max_title_length = None;
  supports_threads = true;
  media_requirement = NoMedia;
  image_max_width = Some 1080;
  image_max_height = Some 1920;
  image_formats = ["jpg"; "jpeg"; "png"];
  image_max_size_mb = Some 8;
  video_max_duration_seconds = Some 300;
  video_formats = ["mp4"; "mov"];
  video_max_size_mb = Some 100;
  video_aspect_ratios = ["1:1"; "9:16"; "16:9"];
  video_max_frame_rate = Some 60.0;
  max_carousel_items = Some 20;
  posts_per_day = Some 25;
  posts_per_hour = Some 25;
}

let google_business_profile = {
  platform = GoogleBusinessProfile;
  display_name = "Google Business Profile";
  max_text_length = 1500;
  requires_title = false;
  max_title_length = None;
  supports_threads = false;
  media_requirement = NoMedia;
  image_max_width = None;
  image_max_height = None;
  image_formats = ["jpg"; "jpeg"; "png"; "gif"];
  image_max_size_mb = Some 5;
  video_max_duration_seconds = None;
  video_formats = [];
  video_max_size_mb = None;
  video_aspect_ratios = [];
  video_max_frame_rate = None;
  max_carousel_items = Some 1;
  posts_per_day = None;
  posts_per_hour = None;
}

let telegram = {
  platform = Telegram;
  display_name = "Telegram";
  max_text_length = 4096;
  requires_title = false;
  max_title_length = None;
  supports_threads = false;
  media_requirement = NoMedia;
  image_max_width = None;
  image_max_height = None;
  image_formats = ["jpg"; "jpeg"; "png"; "gif"; "webp"];
  image_max_size_mb = Some 10;
  video_max_duration_seconds = None;
  video_formats = ["mp4"; "mov"];
  video_max_size_mb = Some 50;
  video_aspect_ratios = [];
  video_max_frame_rate = None;
  max_carousel_items = Some 10;
  posts_per_day = None;
  posts_per_hour = None;
}

(* ------------------------------------------------------------------ *)
(*  Lookup                                                             *)
(* ------------------------------------------------------------------ *)

(** Get the capability record for a platform *)
let get_capability = function
  | Twitter -> twitter
  | LinkedIn -> linkedin
  | Bluesky -> bluesky
  | Mastodon -> mastodon
  | FacebookPage -> facebook_page
  | Instagram -> instagram
  | YouTubeShorts -> youtube_shorts
  | Pinterest -> pinterest
  | Threads -> threads
  | TikTok -> tiktok
  | Reddit -> reddit
  | Telegram -> telegram
  | GoogleBusinessProfile -> google_business_profile

(** Get capability with format-specific overrides applied.
    For example, Instagram Reels have different constraints than feed posts. *)
let get_capability_with_format platform format =
  match platform, format with
  | Instagram, Some ReelFormat ->
      { instagram with
        display_name = "Instagram Reels";
        media_requirement = VideoRequired;
        video_max_duration_seconds = Some 90;
        max_carousel_items = None;
        video_aspect_ratios = ["9:16"] }
  | Instagram, Some StoryFormat ->
      { instagram with
        display_name = "Instagram Stories";
        max_text_length = 0;
        media_requirement = MediaRequired;
        video_max_duration_seconds = Some 60;
        max_carousel_items = None;
        video_aspect_ratios = ["9:16"] }
  | FacebookPage, Some ReelFormat ->
      { facebook_page with
        display_name = "Facebook Reels";
        media_requirement = VideoRequired;
        video_max_duration_seconds = Some 90;
        max_carousel_items = None;
        video_aspect_ratios = ["9:16"] }
  | _ -> get_capability platform

(** All platform capabilities *)
let all_capabilities = [
  twitter; linkedin; bluesky; pinterest; youtube_shorts;
  mastodon; facebook_page; instagram; tiktok; reddit;
  threads; google_business_profile; telegram;
]

(* ------------------------------------------------------------------ *)
(*  Validation helpers                                                 *)
(* ------------------------------------------------------------------ *)

(** Validate text length against platform limit *)
let validate_text_length ~platform ~text =
  let cap = get_capability platform in
  if String.length text > cap.max_text_length then
    Error (Printf.sprintf "Text exceeds %d character limit for %s"
      cap.max_text_length cap.display_name)
  else
    Ok ()

(** Map a video MIME type to the file extension used in [video_formats] lists.
    MIME subtypes don't always match file extensions (e.g. video/quicktime -> mov). *)
let video_mime_to_ext mime_type =
  match String.split_on_char '/' mime_type with
  | ["video"; subtype] ->
      (match subtype with
       | "quicktime" -> "mov"
       | "x-msvideo" -> "avi"
       | "x-matroska" -> "mkv"
       | other -> other)
  | _ -> ""

(** Validate an image against platform constraints *)
let validate_image ~platform ~mime_type ~file_size_bytes ~width ~height =
  let cap = get_capability platform in
  if cap.media_requirement = VideoRequired then
    Error (Printf.sprintf "%s requires video, not images" cap.display_name)
  else
    let ext = match String.split_on_char '/' mime_type with
      | ["image"; ext] -> ext
      | _ -> ""
    in
    if not (List.mem ext cap.image_formats) then
      Error (Printf.sprintf "Image format %s not supported by %s. Supported: %s"
        ext cap.display_name (String.concat ", " cap.image_formats))
    else
      let max_bytes = match cap.image_max_size_mb with
        | Some mb -> mb * 1024 * 1024
        | None -> max_int
      in
      if file_size_bytes > max_bytes then
        Error (Printf.sprintf "Image size exceeds %dMB limit for %s"
          (Option.get cap.image_max_size_mb) cap.display_name)
      else
        let width_ok = match cap.image_max_width with
          | Some max_w -> width <= max_w
          | None -> true
        in
        let height_ok = match cap.image_max_height with
          | Some max_h -> height <= max_h
          | None -> true
        in
        if not width_ok || not height_ok then
          Error (Printf.sprintf "Image dimensions %dx%d exceed limits for %s"
            width height cap.display_name)
        else
          Ok ()

(** Validate a video against platform constraints *)
let validate_video ~platform ~mime_type ~file_size_bytes ~duration_seconds =
  let cap = get_capability platform in
  if cap.video_formats = [] then
    Error (Printf.sprintf "%s does not support video" cap.display_name)
  else
    let ext = video_mime_to_ext mime_type in
    if not (List.mem ext cap.video_formats) then
      Error (Printf.sprintf "Video format %s not supported by %s. Supported: %s"
        ext cap.display_name (String.concat ", " cap.video_formats))
    else
      let max_bytes = match cap.video_max_size_mb with
        | Some mb -> mb * 1024 * 1024
        | None -> max_int
      in
      if file_size_bytes > max_bytes then
        Error (Printf.sprintf "Video size exceeds %dMB limit for %s"
          (Option.get cap.video_max_size_mb) cap.display_name)
      else
        match cap.video_max_duration_seconds with
        | Some max_dur when duration_seconds > float_of_int max_dur ->
            Error (Printf.sprintf "Video duration %.1fs exceeds %ds limit for %s"
              duration_seconds max_dur cap.display_name)
        | _ -> Ok ()

(** Parse aspect ratio string like "9:16" to float *)
let parse_aspect_ratio_string ratio_str =
  match String.split_on_char ':' ratio_str with
  | [w; h] ->
      (try
        let width = float_of_string w in
        let height = float_of_string h in
        if height > 0.0 then Some (width /. height) else None
      with Failure _ -> None)
  | _ -> None

(** Check if an aspect ratio matches a target ratio (with 5% tolerance) *)
let aspect_ratio_matches ~actual ~target =
  let tolerance = 0.05 in
  abs_float (actual -. target) /. target <= tolerance

(** Check if video aspect ratio is compatible with platform *)
let is_aspect_ratio_compatible ~platform ~aspect_ratio =
  let cap = get_capability platform in
  if cap.video_aspect_ratios = [] then true
  else
    List.exists (fun ratio_str ->
      match parse_aspect_ratio_string ratio_str with
      | Some target -> aspect_ratio_matches ~actual:aspect_ratio ~target
      | None -> false
    ) cap.video_aspect_ratios

(** Video format for platforms that distinguish Reels from regular videos *)
type video_format = Reel | Regular

(** Classify a video as Reel or Regular based on dimensions and duration.
    Returns [Reel] if the video is vertical (aspect ratio <= 0.6) and short
    (under 90 seconds). This matches the criteria for Instagram Reels and
    Facebook Reels. *)
let classify_video_format ~width ~height ~duration_seconds =
  if width > 0 && height > 0 then
    let aspect_ratio = float_of_int width /. float_of_int height in
    if aspect_ratio <= 0.6 && duration_seconds <= 90.0 then Reel
    else Regular
  else Regular

(** Check if a platform+format combination is video-first (vertical video preferred) *)
let is_vertical_video_platform ?format platform =
  match platform, format with
  | TikTok, _ -> true
  | YouTubeShorts, _ -> true
  | Instagram, Some ReelFormat -> true
  | Instagram, Some StoryFormat -> true
  | FacebookPage, Some ReelFormat -> true
  | _ -> false
