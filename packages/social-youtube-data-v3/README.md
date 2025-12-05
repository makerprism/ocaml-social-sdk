# social-youtube-data-v3

OCaml library for YouTube Data API v3 integration (YouTube Shorts) with runtime-agnostic design.

## Features

- **Google OAuth 2.0 with PKCE**: Secure authentication
- **YouTube Shorts Upload**: Resumable video upload
- **Short-Lived Tokens**: 1-hour access tokens with automatic refresh
- **Refresh Tokens**: Long-lived refresh tokens (don't expire)
- **Runtime Agnostic**: Works with Lwt, Eio, or synchronous runtimes via CPS

## Installation

### From source
```bash
cd packages/social-youtube-data-v3
dune build
dune install
```

## Usage

### Basic Example

```ocaml
open Social_provider_core
open Social_youtube_data_v3

module Config = struct
  module Http = Social_provider_lwt.Cohttp_client
  (* ... other required functions *)
end

module YouTube = Youtube_data_v3.Make(Config)

(* Upload a YouTube Short *)
let upload_short account_id =
  let text = "Check out this Short! #Shorts" in
  let media_urls = ["https://cdn.example.com/video.mp4"] in
  
  YouTube.post_single ~account_id ~text ~media_urls
    (fun video_id ->
      Printf.printf "Uploaded successfully: %s\n" video_id;
      ())
    (fun err -> Printf.eprintf "Error: %s\n" err)
```

## OAuth Configuration

Set these environment variables:

```bash
YOUTUBE_CLIENT_ID=your_client_id
YOUTUBE_CLIENT_SECRET=your_client_secret
YOUTUBE_REDIRECT_URI=https://yourapp.com/callback
```

## OAuth Scopes

- `https://www.googleapis.com/auth/youtube.upload` - Upload videos
- `https://www.googleapis.com/auth/youtube` - Manage YouTube account

## Platform Constraints

### Text
- Maximum: 5,000 characters (description)
- Title: First 100 characters used

### Videos
- **Vertical video required** for Shorts
- Resumable upload process
- Formats: MP4, MOV, etc.
- Automatically tagged with #Shorts

### Token Lifetime
- **Access tokens**: 1 hour
- **Refresh tokens**: Don't expire (unless revoked)
- Auto-refresh 10 minutes before expiry

## Resumable Upload Process

YouTube uses a two-step resumable upload:

1. **Initialize**: Send metadata (title, description, etc.)
2. **Upload**: Send video binary data to resumable URL

The library handles this automatically.

## API Reference

### Functions

#### `get_oauth_url`
Generate OAuth authorization URL with PKCE.

#### `exchange_code`
Exchange authorization code for access token with PKCE.

#### `post_single`
Upload video to YouTube Shorts.

#### `refresh_access_token`
Refresh expired access token.

#### `ensure_valid_token`
Ensure token is valid, auto-refreshing if needed.

#### `validate_content`
Validate description length.

## Testing

Run the test suite:

```bash
cd packages/social-youtube-data-v3
dune test
```

Tests include:
- OAuth URL generation with PKCE
- Token exchange
- Token refresh
- Content validation
- Auto-refresh on expiry

## License

MIT

## Related Packages

- `social-provider-core` - Core interfaces and types
- `social-provider-lwt` - Lwt runtime adapters
- `social-twitter-v2` - Twitter API v2
- `social-linkedin-v2` - LinkedIn API v2
