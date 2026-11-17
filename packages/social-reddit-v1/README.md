# social-reddit-v1

Reddit API v1 client for OCaml. Supports posting to subreddits where the authenticated user is a moderator.

## Features

- **OAuth 2.0 with Basic Auth** - Similar to Pinterest's OAuth flow
- **Access tokens** - Expire in 1 hour with automatic refresh
- **Refresh tokens** - Long-lived (use `duration=permanent`)
- **Post types**: Self-posts (text), link posts, image posts, video posts, galleries
- **Crossposting** - Share posts between subreddits
- **Flair support** - Get available flairs and set flair on posts
- **Subreddit management** - List moderated subreddits, check posting requirements

## Platform Limits

| Type | Limit |
|------|-------|
| Title | 300 characters |
| Body (self-post) | 40,000 characters |
| Images | 20MB max, jpg/png/gif |
| Video | 1GB max, 15 minutes |
| Gallery | up to 20 images |

## Installation

```
opam install social-reddit-v1
```

## Usage

### OAuth Setup

```ocaml
open Social_reddit_v1

(* Generate authorization URL *)
let auth_url = OAuth.get_authorization_url
  ~client_id:"your_client_id"
  ~redirect_uri:"https://your-app.com/callback"
  ~state:"random_state_string"
  ~duration:"permanent"  (* Required for refresh tokens *)
  ()

(* Exchange code for tokens *)
module OAuthClient = OAuth.Make(Your_http_client)

OAuthClient.exchange_code
  ~client_id:"your_client_id"
  ~client_secret:"your_client_secret"
  ~redirect_uri:"https://your-app.com/callback"
  ~code:"authorization_code"
  (fun credentials ->
    (* credentials.access_token, credentials.refresh_token, etc. *)
    ())
  (fun error -> print_endline error)
```

### Posting

```ocaml
module Reddit = Make(Your_config)

(* Self-post (text) *)
Reddit.submit_self_post
  ~account_id:"account123"
  ~subreddit:"your_subreddit"
  ~title:"Post Title"
  ~body:"Post content here"
  ~flair_id:"flair_template_id"  (* Optional *)
  ()
  (fun outcome ->
    match outcome with
    | Error_types.Success post_id -> 
        Printf.printf "Posted: %s\n" post_id
    | Error_types.Failure err ->
        Printf.printf "Error: %s\n" (Error_types.error_to_string err)
    | _ -> ())

(* Link post *)
Reddit.submit_link_post
  ~account_id:"account123"
  ~subreddit:"your_subreddit"
  ~title:"Check out this link"
  ~url:"https://example.com"
  ~resubmit:true  (* Allow reposting same URL *)
  ()
  on_result

(* Image post *)
Reddit.submit_image_post
  ~account_id:"account123"
  ~subreddit:"your_subreddit"
  ~title:"My image"
  ~image_url:"https://example.com/image.jpg"
  ()
  on_result

(* Native video post (upload lease + S3 upload + submit kind=video) *)
Reddit.submit_video_post
  ~account_id:"account123"
  ~subreddit:"your_subreddit"
  ~title:"My video"
  ~video_url:"https://example.com/video.mp4"
  ~thumbnail_url:"https://example.com/poster.jpg"  (* Optional *)
  ()
  on_result

(* Crosspost *)
Reddit.submit_crosspost
  ~account_id:"account123"
  ~subreddit:"target_subreddit"
  ~title:"Crosspost title"
  ~original_post_id:"t3_abc123"
  ()
  on_result

(* Unified post_single (auto-detects type; choose one mode per call) *)
Reddit.post_single
  ~account_id:"account123"
  ~subreddit:"your_subreddit"
  ~title:"Video example"
  ~media_urls:["https://example.com/video.mp4"; "https://example.com/poster.jpg"]
  ()
  on_result
```

### Video Caveats

- Native video posting is used for video media (no image fallback for real video content).
- `post_single` treats the first media URL as the primary video source; if a second URL is present and is image-like, it is used as `video_poster_url`.
- Video limits are enforced before upload: max 1GB, max 15 minutes, `video/mp4`.
- Unknown-extension media is content-type checked; non-image/non-video media fails with structured validation errors.
- Use exactly one posting mode per `post_single` call: self (`body`), link (`url`), or media (`media_urls`).

### Subreddit Management

```ocaml
(* Get moderated subreddits *)
Reddit.get_moderated_subreddits
  ~account_id:"account123"
  (fun outcome ->
    match outcome with
    | Error_types.Success subreddits ->
        List.iter (fun s -> 
          Printf.printf "%s (%d subscribers)\n" s.name s.subscribers
        ) subreddits
    | _ -> ())

(* Get available flairs for a subreddit *)
Reddit.get_subreddit_flairs
  ~account_id:"account123"
  ~subreddit:"your_subreddit"
  (fun outcome ->
    match outcome with
    | Error_types.Success flairs ->
        List.iter (fun f ->
          Printf.printf "%s: %s\n" f.flair_id f.flair_text
        ) flairs
    | _ -> ())

(* Check if flair is required *)
Reddit.check_flair_required
  ~account_id:"account123"
  ~subreddit:"your_subreddit"
  (fun outcome ->
    match outcome with
    | Error_types.Success required ->
        Printf.printf "Flair required: %b\n" required
    | _ -> ())
```

### Validation

```ocaml
(* Validate before posting *)
match Reddit.validate_content ~title:"My Title" ~body:"My content" () with
| Ok () -> (* Valid, proceed with posting *)
| Error msg -> Printf.printf "Validation error: %s\n" msg
```

## Required Scopes

- `submit` - Submit posts
- `read` - Read content
- `mysubreddits` - Access moderated/subscribed subreddits
- `flair` - Set flair on posts
- `modposts` - Moderate posts (optional, for moderation features)

## Environment Variables

- `REDDIT_CLIENT_ID` - Your Reddit app's client ID
- `REDDIT_CLIENT_SECRET` - Your Reddit app's client secret
- `REDDIT_USER_AGENT` - Strongly recommended. Reddit requires `<platform>:<app ID>:<version> (by /u/<reddit username>)` and throttles generic agents. Without it the library default `ocaml:social-reddit-v1:v0.1.0` is sent. `OAuth.Make` functions take `?user_agent` instead.

## References

- [Reddit OAuth2 Documentation](https://github.com/reddit-archive/reddit/wiki/OAuth2)
- [Reddit API Documentation](https://www.reddit.com/dev/api/)
- [PRAW (Python Reddit API Wrapper)](https://praw.readthedocs.io/) - Reference implementation
- [go-reddit](https://github.com/vartanbeno/go-reddit) - Go client used for test patterns
