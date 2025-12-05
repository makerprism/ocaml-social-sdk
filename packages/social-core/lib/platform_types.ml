(** Platform Types - Common types for social media platforms *)

(** Social media platforms *)
type platform =
  | Twitter
  | LinkedIn
  | Bluesky
  | Mastodon
  | FacebookPage
  | Instagram
  | YouTubeShorts
  | Pinterest
  | Threads

(** Convert platform to string *)
let platform_to_string = function
  | Twitter -> "twitter"
  | LinkedIn -> "linkedin"
  | Bluesky -> "bluesky"
  | Mastodon -> "mastodon"
  | FacebookPage -> "facebook_page"
  | Instagram -> "instagram"
  | YouTubeShorts -> "youtube_shorts"
  | Pinterest -> "pinterest"
  | Threads -> "threads"

(** Parse platform from string *)
let platform_of_string = function
  | "twitter" -> Some Twitter
  | "linkedin" -> Some LinkedIn
  | "bluesky" -> Some Bluesky
  | "mastodon" -> Some Mastodon
  | "facebook_page" -> Some FacebookPage
  | "instagram" -> Some Instagram
  | "youtube_shorts" -> Some YouTubeShorts
  | "pinterest" -> Some Pinterest
  | "threads" -> Some Threads
  | _ -> None

(** Media type *)
type media_type =
  | Image
  | Video
  | Gif

(** Media constraints for a platform *)
type media_constraints = {
  max_images: int;
  max_videos: int;
  max_image_size_mb: float;
  max_video_size_mb: float;
  max_video_duration_seconds: int;
  supported_image_formats: string list;
  supported_video_formats: string list;
  max_width: int option;
  max_height: int option;
  min_width: int option;
  min_height: int option;
}

(** Platform capabilities *)
type platform_capability = {
  platform: platform;
  max_text_length: int;
  supports_threads: bool;
  supports_media: bool;
  media_constraints: media_constraints option;
  supports_hashtags: bool;
  supports_mentions: bool;
  supports_links: bool;
  supports_polls: bool;
}

(** Account information *)
type account = {
  id: string;
  platform: platform;
  platform_user_id: string;
  platform_username: string;
  credentials_encrypted: string;
}

(** Post media *)
type post_media = {
  media_type: media_type;
  mime_type: string;
  file_size_bytes: int;
  width: int option;
  height: int option;
  duration_seconds: float option;
  alt_text: string option;
}
