# social-instagram-graph-v21

OCaml library for Instagram Graph API v21 integration (Business accounts) with runtime-agnostic design.

> **Warning:** This library was primarily built using LLMs and has not been tested. Expect breaking changes.

## Features

- **OAuth 2.0 Authentication**: Full OAuth flow via Facebook
- **Long-Lived Tokens**: Automatic exchange for 60-day tokens
- **Automatic Token Refresh**: Refreshes tokens before expiry (60-day extension)
- **Business Account Posting**: Post to Instagram Business accounts
- **Two-Step Publishing**: Create container, then publish
- **Smart Container Polling**: Exponential backoff retry logic (up to 30 seconds)
- **Enhanced Error Messages**: User-friendly error messages with actionable guidance
- **Single Posts**: Images or videos
- **Carousel Posts**: 2-10 images/videos per post
- **Reels**: Short-form video content (3-90 seconds)
- **Video Posts**: Feed videos (3-60 seconds)
- **Media Type Detection**: Automatic detection from file extensions
- **Caption Support**: Text captions up to 2,200 characters
- **Hashtag Validation**: Maximum 30 hashtags per post
- **Runtime Agnostic**: Works with Lwt, Eio, or synchronous runtimes via CPS

## Critical Requirements

⚠️ **Instagram Business or Creator account ONLY**
- Personal accounts are not supported
- Account must be linked to a Facebook Page
- Requires Facebook App with Instagram permissions

## Installation

### From source
```bash
cd packages/social-instagram-graph-v21
dune build
dune install
```

## Usage

### Basic Example - Single Image

```ocaml
open Social_provider_core
open Social_instagram_graph_v21

module Config = struct
  module Http = Social_provider_lwt.Cohttp_client
  
  let get_env = Sys.getenv_opt
  let get_credentials ~account_id on_success on_error = (* ... *)
  let get_ig_user_id ~account_id on_success on_error = (* ... *)
  let sleep duration on_continue = (* Wait for duration seconds *)
  (* ... other required functions *)
end

module Instagram = Instagram_graph_v21.Make(Config)

(* Post single image *)
let post_image account_id =
  let text = "Hello Instagram! #ocaml" in
  let media_urls = ["https://cdn.example.com/public/image.jpg"] in
  
  Instagram.post_single ~account_id ~text ~media_urls
    (fun media_id ->
      Printf.printf "Posted successfully: %s\n" media_id;
      ())
    (fun err -> Printf.eprintf "Error: %s\n" err)
```

### Carousel Example (2-10 Items)

```ocaml
(* Post carousel with multiple images *)
let post_carousel account_id =
  let text = "Check out these photos! #carousel #ocaml" in
  let media_urls = [
    "https://cdn.example.com/image1.jpg";
    "https://cdn.example.com/image2.jpg";
    "https://cdn.example.com/image3.jpg";
  ] in
  
  Instagram.post_single ~account_id ~text ~media_urls
    (fun media_id ->
      Printf.printf "Carousel posted: %s\n" media_id;
      ())
    (fun err -> Printf.eprintf "Error: %s\n" err)
```

### Video Example

```ocaml
(* Post feed video *)
let post_video account_id =
  let text = "My first video post! #video #ocaml" in
  let media_urls = ["https://cdn.example.com/video.mp4"] in
  
  Instagram.post_single ~account_id ~text ~media_urls
    (fun media_id ->
      Printf.printf "Video posted: %s\n" media_id;
      ())
    (fun err -> Printf.eprintf "Error: %s\n" err)
```

### Reel Example

```ocaml
(* Post Reel (short-form video) *)
let post_reel account_id =
  let text = "My first Reel! #reels #ocaml" in
  let video_url = "https://cdn.example.com/reel.mp4" in
  
  Instagram.post_reel ~account_id ~text ~video_url
    (fun media_id ->
      Printf.printf "Reel posted: %s\n" media_id;
      ())
    (fun err -> Printf.eprintf "Error: %s\n" err)
```

## Two-Step Publishing Process

Instagram requires a two-step process:

1. **Create Container**: Upload image URL and caption
2. **Publish Container**: After Instagram processes the media

The library handles this automatically:
- Creates container
- Polls status with exponential backoff (2s, 3s, 5s, 8s, 13s)
- Checks container status up to 5 times (30+ seconds total)
- Publishes when ready
- Returns helpful error messages if processing fails

## OAuth Scopes

Required Facebook/Instagram permissions:

- `instagram_basic` - Basic Instagram profile info
- `instagram_content_publish` - Publish content
- `pages_read_engagement` - Read page insights
- `pages_show_list` - List connected pages

## Configuration

Set these environment variables:

```bash
FACEBOOK_APP_ID=your_app_id
FACEBOOK_APP_SECRET=your_app_secret
INSTAGRAM_REDIRECT_URI=https://yourapp.com/callback
```

## Platform Constraints

### Text
- Maximum: 2,200 characters
- Maximum hashtags: 30 per post

### Images
- **Must be publicly accessible URLs**
- Minimum: 320px on shortest edge
- Recommended: 1080px (will be resized)
- Maximum file size: 8 MB
- Formats: JPG, PNG

### Videos
- **Must be publicly accessible URLs**
- Formats: MP4, MOV
- Feed Videos: 3-60 seconds duration
- Maximum file size: 100 MB
- Resolution: 1080px max (longest edge)
- Frame rate: 23-60 FPS
- Bitrate: 3.5 Mbps max

### Reels
- **Must be publicly accessible URLs**
- Formats: MP4, MOV
- Duration: 3-90 seconds
- Maximum file size: 100 MB
- Vertical format recommended (9:16)

### Carousel Posts
- **2-10 media items** (mix of images and videos)
- All items must have same aspect ratio
- Caption applies to entire carousel
- Each item must be publicly accessible URL

### Threading
- Instagram doesn't support threads
- Only single posts allowed

## Rate Limits

Instagram enforces strict rate limits:

- **200 API calls/hour** per user
- **25 container creations/hour** per user
- **25 posts/day** per user

## Token Management

- **Short-lived tokens** (1 hour) automatically exchanged for **long-lived tokens** (60 days)
- **Automatic refresh** when token expires within 7 days
- Tokens extended by 60 days on each refresh
- Failed refresh triggers re-authentication
- Health status tracking for token state

## API Reference

### Functions

#### `get_oauth_url`
Generate OAuth authorization URL (via Facebook).

#### `exchange_code`
Exchange authorization code for short-lived token, then automatically exchange for long-lived token (60 days).

#### `refresh_token`
Refresh long-lived token to extend validity by 60 days. Automatically called by `ensure_valid_token` when token expires within 7 days.

#### `ensure_valid_token`
Get valid access token, automatically refreshing if needed (7-day buffer before expiry).

#### `post_single`
Post single image, video, or carousel (2-10 items) with two-step process and smart container polling.

#### `post_reel`
Post Reel (short-form video, 3-90 seconds).

#### `create_image_container`
Create image container (step 1a). Can be carousel item or standalone.

#### `create_video_container`
Create video container (step 1b). Supports VIDEO and REELS media types.

#### `create_carousel_container`
Create carousel container from child containers (step 1c).

#### `create_carousel_children`
Recursively create child containers for carousel items.

#### `publish_container`
Publish container (step 2).

#### `check_container_status`
Check if container is ready to publish.

#### `poll_container_status`
Poll container status with exponential backoff and auto-publish when ready.

#### `detect_media_type`
Automatically detect media type from URL extension (IMAGE or VIDEO).

#### `validate_content`
Validate caption length and hashtag count.

#### `validate_carousel`
Validate carousel has 2-10 items.

#### `validate_video`
Validate video URL and media type (VIDEO or REELS).

#### `validate_carousel_items`
Validate all carousel items are accessible URLs.

## Testing

Run the test suite:

```bash
cd packages/social-instagram-graph-v21
dune test
```

Tests include:
- OAuth URL generation
- Token exchange
- Container creation
- Container publishing
- Status checking
- Content validation
- Full posting flow

## Important Notes

### Public URLs Required

Instagram requires images to be **publicly accessible URLs**. The API cannot accept:
- Direct file uploads
- Base64-encoded data
- Private URLs

You must:
1. Upload images to a public CDN or storage
2. Pass the public URL to the API
3. Instagram fetches and processes the image

### Container Processing

After creating a container:
- Wait 2-5 seconds for Instagram to process
- Check status before publishing
- Container status can be: `FINISHED`, `IN_PROGRESS`, or `ERROR`

### Error Handling

The library now provides user-friendly error messages with actionable guidance:

**Authentication Errors:**
- `"Instagram access token expired or invalid. Please reconnect your Instagram account."` (Code 190)
- `"Missing Instagram permissions. Please reconnect your account..."` (Code 10)

**Rate Limit Errors:**
- `"Instagram rate limit exceeded. You can post up to 25 times per day..."` (Code 4)
- `"Instagram page rate limit exceeded. Please wait a few minutes..."` (Code 32)

**Content Errors:**
- `"This Instagram account is not a Business or Creator account. Please convert..."` (Code 100 + "business")
- `"Instagram couldn't access the image URL. Make sure the image is publicly accessible..."` (Code 9004)
- `"Invalid image format. Please use JPEG or PNG images."` (Code 9005)
- `"Caption is too long. Instagram captions must be 2,200 characters or less."` (Code 100 + "caption")

**Processing Errors:**
- `"Container not ready for publishing. The image is still being processed..."` (Code 100 + "creation_id")
- `"Container still processing after 5 attempts. Try again in a few minutes."` (Polling timeout)

## License

MIT

## Related Packages

- `social-core` - Core interfaces and types
- `social-lwt` - Lwt runtime adapters
- `social-facebook-graph-v21` - Facebook Pages API (shares Graph API)
- `social-linkedin-v2` - LinkedIn API v2
- `social-twitter-v2` - Twitter API v2
