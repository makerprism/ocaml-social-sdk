# social-tiktok-v1

TikTok Content Posting API client for OCaml.

> **Warning:** This library was primarily built using LLMs and has not been tested. Expect breaking changes.

## Features

- **OAuth 2.0 Authentication**: Complete OAuth flow with PKCE support
- **Video Posting**: Upload videos via FILE_UPLOAD or PULL_FROM_URL
- **Photo Carousels**: Post multiple images as a carousel
- **Creator Info**: Query user's posting capabilities and limits
- **Status Tracking**: Monitor publish progress

## Installation

```bash
opam install social-tiktok-v1
```

## Usage

### OAuth Flow

```ocaml
open Social_tiktok_v1

(* Generate authorization URL *)
let auth_url = get_authorization_url
  ~client_id:"your_client_key"
  ~redirect_uri:"https://your-app.com/callback"
  ~scopes:["video.publish"]
  ~state:"random_state"

(* Exchange code for token *)
let () = exchange_code_for_token
  ~client_id:"your_client_key"
  ~client_secret:"your_client_secret"
  ~code:authorization_code
  ~redirect_uri:"https://your-app.com/callback"
  ~http_post
  ~on_success:(fun (access_token, refresh_token, expires_in, open_id) ->
    (* Store tokens securely *)
  )
  ~on_error:(fun err -> print_endline err)
```

### Query Creator Info (Required Before Posting)

```ocaml
let () = query_creator_info
  ~access_token
  ~http_post
  ~on_success:(fun info ->
    Printf.printf "Max duration: %d seconds\n" info.max_video_post_duration_sec;
    Printf.printf "Privacy options: %d\n" (List.length info.privacy_level_options)
  )
  ~on_error:(fun err -> print_endline err)
```

### Post a Video (FILE_UPLOAD)

```ocaml
let post_info = {
  title = "Check out this #video on @tiktok #fyp";
  privacy_level = PublicToEveryone;
  disable_duet = false;
  disable_comment = false;
  disable_stitch = false;
  video_cover_timestamp_ms = Some 1000;
}

let () = init_video_upload_file
  ~access_token
  ~post_info
  ~video_size:(10 * 1024 * 1024)  (* 10MB *)
  ~http_post
  ~on_success:(fun response ->
    Printf.printf "Publish ID: %s\n" response.publish_id;
    match response.upload_url with
    | Some url -> Printf.printf "Upload to: %s\n" url
    | None -> ()
  )
  ~on_error:(fun err -> print_endline err)
```

### Post a Video (PULL_FROM_URL)

```ocaml
let () = init_video_upload_url
  ~access_token
  ~post_info
  ~video_url:"https://your-domain.com/video.mp4"
  ~http_post
  ~on_success:(fun response ->
    Printf.printf "Publish ID: %s\n" response.publish_id
  )
  ~on_error:(fun err -> print_endline err)
```

### Check Publish Status

```ocaml
let () = get_publish_status
  ~access_token
  ~publish_id
  ~http_post
  ~on_success:(function
    | Processing -> print_endline "Still processing..."
    | Published video_id -> Printf.printf "Published! ID: %s\n" video_id
    | Failed { error_code; error_message } ->
        Printf.printf "Failed: %s - %s\n" error_code error_message
  )
  ~on_error:(fun err -> print_endline err)
```

## Video Constraints

| Specification | Default Limit | TikTok Limit |
|--------------|-------------------|--------------|
| Max file size | 50 MB | 4 GB |
| Duration | 3s - 600s | 3s - 600s |
| Resolution | 360-4096px | 360-4096px |
| Frame rate | 23-60 FPS | 23-60 FPS |
| Formats | MP4, WebM, MOV | MP4, WebM, MOV |

### Aspect Ratios

| Ratio | Type | Recommendation |
|-------|------|----------------|
| 9:16 | Vertical | Best for TikTok |
| 1:1 | Square | Works, less optimal |
| 16:9 | Horizontal | Works, letterboxed |

## Chunked Upload

For videos > 5MB, use chunked upload:

```ocaml
(* Calculate chunks *)
let chunk_size, total_chunks = calculate_chunks ~video_size

(* Chunk requirements:
   - Minimum: 5MB
   - Maximum: 64MB (final chunk can be 128MB)
   - Maximum 1000 chunks
   - Must upload sequentially
*)
```

## Error Handling

All functions use continuation-passing style:

```ocaml
init_video_upload_file
  ~access_token
  ~post_info
  ~video_size
  ~http_post
  ~on_success:(fun response -> (* handle success *))
  ~on_error:(fun err -> 
    (* Common errors:
       - "access_token_invalid" - Token expired
       - "scope_not_authorized" - Missing video.publish scope
       - "rate_limit_exceeded" - Too many requests
       - "url_ownership_unverified" - Domain not verified
    *)
  )
```

## Important Notes

1. **App Audit Required**: All content from unaudited apps is private
2. **Query Creator Info**: Must call before every post
3. **Domain Verification**: Required for PULL_FROM_URL method
4. **Sequential Chunks**: Chunked uploads must be sequential, not parallel

## License

MIT
