# social-bluesky-v1

Bluesky AT Protocol v1 client for OCaml.

> **Status:** This library was primarily built using LLMs. Authentication and posting functionality have been used successfully. Read operations and other features should be considered untested. Expect breaking changes as we work towards stability.

## Features

### Core Posting
- ✅ Post creation with URI and CID extraction
- ✅ Delete posts
- ✅ Full thread support with proper reply chains
- ✅ Quote posts with optional media
- ✅ Media upload (up to 4 images; one video via `app.bsky.video.uploadVideo` + job polling when `OPTIONS.sleep` is set, else `uploadBlob`)
- ✅ Video upload validation (50MB size limit, duration check when duration metadata is available)

### Rich Text
- ✅ URL detection and linking
- ✅ Mention detection (@username.bsky.social) with DID resolution
- ✅ Hashtag detection (#hashtag)
- ✅ Link card embeds (external links)

### Social Interactions
- ✅ Like/unlike posts
- ✅ Repost/unrepost
- ✅ Follow/unfollow users

### Read Operations
- ⚠️ Typed reads (`Views`, `Reads.Make`, `*_view` / `*_page` wrappers): post thread with `depth` / `parentHeight`, author feed with `filter`, `getPosts` in batches of 25, notifications with `reasons`, `resolveHandle`
- ✅ Get post thread
- ✅ Get user profile (by handle or DID)
- ✅ Get timeline
- ✅ Get author feed (user's posts)
- ✅ Get likes for a post
- ✅ Get reposts for a post
- ✅ Get followers list
- ✅ Get follows list

### Notifications
- ✅ List notifications with pagination
- ✅ Count unread notifications
- ✅ Mark notifications as seen

### Search
- ✅ Search for users/actors
- ✅ Search for posts

### Moderation
- ✅ Mute/unmute actors
- ✅ Block/unblock actors

### Authentication
- ✅ App password authentication (no OAuth)
- ✅ Session management
- ✅ Health status tracking

## Authentication (Current Provider Model)

This provider currently authenticates with app passwords via AT Protocol session endpoints.

### How It Works

| Property | Value |
|----------|-------|
| Auth method | App passwords |
| OAuth | Not used by this provider implementation |
| App password lifetime | User-managed (can be revoked/rotated) |
| Session tokens | Access JWT cached ~4 minutes; the stored refresh JWT is rotated via `refreshSession` |

### Setup

1. Go to https://bsky.app/settings/app-passwords
2. Create a new app password
3. Use the identifier (handle or DID) + app password to authenticate

```ocaml
(* Bluesky authenticates via createSession, not OAuth *)
(* Your credentials are: identifier + app_password *)

(* Exchange the app password for a session once, then store the session's
   refresh JWT. Never store the app password. *)
module Bsky_auth = Social_bluesky_v1.Auth.Make (My_http_client)

Bsky_auth.create_session ~identifier ~app_password (function
  | Social_core.Error_types.Success session ->
      store (Social_bluesky_v1.Auth.session_to_refresh_credentials session)
  | _ -> ...)
(* ensure_valid_token refreshes (and rotates) the stored refresh JWT. *)
```

### No Scopes

Since Bluesky uses app passwords instead of OAuth, there are no scopes to request. App passwords grant full access to the account's capabilities.

### Validation
- ✅ Content length validation (300 chars)
- ✅ Media size validation (1MB images, 50MB video)
- ✅ Media type validation
- ✅ Video duration validation (60s max, when duration metadata is available)

## Usage

### Basic Example

```ocaml
(* Create a configuration module *)
module My_config = struct
  module Http = My_http_client  (* Your HTTP client *)
  
  let get_env = Sys.getenv_opt
  let get_credentials ~account_id on_success on_error = ...
  let encrypt data on_success on_error = ...
  let decrypt data on_success on_error = ...
  let update_credentials ~account_id ~credentials on_success on_error = ...
  let update_health_status ~account_id ~status ~error_message on_success on_error = ...
end

(* Create provider instance *)
module Bluesky = Social_bluesky_v1.Bluesky_v1.Make(My_config)

(* Post to Bluesky *)
Bluesky.post_single
  ~account_id:"my_account"
  ~text:"Hello from OCaml! 🐫"
  ~media_urls:[]
  (function
    | Social_core.Error_types.Success post_uri -> 
        Printf.printf "Posted: %s\n" post_uri
    | Social_core.Error_types.Partial_success { result = post_uri; warnings } ->
        Printf.printf "Posted: %s with %d warnings\n" post_uri (List.length warnings)
    | Social_core.Error_types.Failure err ->
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### Self-hosted PDS

`Make` talks to `https://bsky.social`. For an account on another PDS (or a
local mock server), pass options. `service_url` is read on every call, so it
can come from configuration loaded after the functor is applied.

```ocaml
module My_options = struct
  include Social_bluesky_v1.Default_options  (* future options keep defaults *)
  let service_url () = "https://pds.example.org"
end

module Bluesky = Social_bluesky_v1.Make_with_options (My_config) (My_options)
```

### Video

Bluesky recommends uploading video to its video service and polling the
processing job, so the clip is ready when the post appears. Polling needs a
wait, and the SDK never blocks, so give it one in your runtime:

```ocaml
module My_options = struct
  include Social_bluesky_v1.Default_options
  let sleep =
    Some (fun ~seconds on_continue _on_error ->
      Lwt.async (fun () -> Lwt.map on_continue (Lwt_unix.sleep seconds)))
end
```

With `sleep = None` (the default) video is uploaded with `uploadBlob`; it
still publishes, but Bluesky processes it only after the post appears.

### Errors

XRPC errors are mapped by their atproto error name first: `ExpiredToken` and
`InvalidToken` become `Auth_error`, `NotFound` and `RecordNotFound` become
`Resource_not_found`. Other failures map by HTTP status (401 `Auth_error`,
429 `Rate_limited`, otherwise `Api_error`). `atproto_error_name` exposes the
raw name for anything finer.

### With Validation

```ocaml
(* Validate content before posting *)
match Bluesky.validate_content ~text:"My post text" with
| Ok () -> 
    (* Content is valid, proceed with posting *)
    Bluesky.post_single ...
| Error msg ->
    Printf.printf "Invalid content: %s\n" msg
```

### With Media and Alt Text

```ocaml
Bluesky.post_single
  ~account_id:"my_account"
  ~text:"Check out this image!"
  ~media_urls:["https://example.com/image.png"]
  ~alt_texts:["A beautiful sunset"]
  (function
    | Social_core.Error_types.Success post_uri_cid -> 
        Printf.printf "Posted with media: %s\n" post_uri_cid
    | Social_core.Error_types.Partial_success { result = post_uri_cid; warnings } ->
        Printf.printf "Posted: %s with %d warnings\n" post_uri_cid (List.length warnings)
    | Social_core.Error_types.Failure err ->
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### Posting a Thread

```ocaml
(* Each post will be a reply to the previous one *)
Bluesky.post_thread
  ~account_id:"my_account"
  ~texts:["First post in thread"; "Second post (reply to first)"; "Third post (reply to second)"]
  ~media_urls_per_post:[
    [];  (* No media on first post *)
    [];  (* No media on second post *)
    []   (* No media on third post *)
  ]
  (function
    | Social_core.Error_types.Success result -> 
        Printf.printf "Posted thread with %d posts\n" (List.length result.posted_ids)
    | Social_core.Error_types.Partial_success { result; warnings } ->
        Printf.printf "Posted %d/%d posts with warnings\n" 
          (List.length result.posted_ids) result.total_requested
    | Social_core.Error_types.Failure err ->
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### Replying to a Post

```ocaml
(* root = first post of the thread, parent = the post you answer.
   For a top-level post both are that post. *)
let target = { Social_bluesky_v1.uri = "at://did:plc:xyz/app.bsky.feed.post/abc"; cid = "bafy..." } in
Bluesky.reply_to_post
  ~account_id:"my_account"
  ~reply_to:{ Social_bluesky_v1.root = target; parent = target }
  ~text:"Thanks!"
  ~media_urls:[]
  (fun outcome -> ...)

(* Post a thread as a reply, or resume a failed thread: the first post
   attaches under reply_to, the rest chain under it. *)
Bluesky.post_thread ~account_id:"my_account" ~reply_to:{ root; parent = last_posted }
  ~texts:remaining_texts ~media_urls_per_post:remaining_media (fun outcome -> ...)
```

`post_single`, `reply_to_post` and `post_thread` return ids as `"uri|cid"`;
`Social_bluesky_v1.strong_ref_of_post_id` parses one into a `strong_ref`.

### Deleting a Post

```ocaml
Bluesky.delete_post
  ~account_id:"my_account"
  ~post_uri:"at://did:plc:xyz/app.bsky.feed.post/abc123"
  (function
    | Social_core.Error_types.Success () -> 
        Printf.printf "Post deleted\n"
    | Social_core.Error_types.Partial_success _ ->
        Printf.printf "Post deleted\n"  (* Partial success unlikely for delete *)
    | Social_core.Error_types.Failure err ->
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### Rich Text with Mentions and Hashtags

```ocaml
(* Text with mentions, hashtags, and URLs will be automatically detected *)
Bluesky.post_single
  ~account_id:"my_account"
  ~text:"Hey @alice.bsky.social check out #ocaml! https://ocaml.org"
  ~media_urls:[]
  (function
    | Social_core.Error_types.Success post_uri -> 
        Printf.printf "Posted: %s\n" post_uri
    | Social_core.Error_types.Partial_success { result = post_uri; warnings } ->
        Printf.printf "Posted: %s (with %d warnings)\n" post_uri (List.length warnings)
    | Social_core.Error_types.Failure err ->
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### Like and Repost

```ocaml
(* Like a post *)
Bluesky.like_post
  ~account_id:"my_account"
  ~post_uri:"at://did:plc:xyz/app.bsky.feed.post/abc123"
  ~post_cid:"bafyreiabc123"
  (function
    | Ok like_uri -> Printf.printf "Liked: %s\n" like_uri
    | Error err -> Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Unlike a post *)
Bluesky.unlike_post
  ~account_id:"my_account"
  ~like_uri:"at://did:plc:xyz/app.bsky.feed.like/def456"
  (function
    | Ok () -> Printf.printf "Unliked\n"
    | Error err -> Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Repost *)
Bluesky.repost
  ~account_id:"my_account"
  ~post_uri:"at://did:plc:xyz/app.bsky.feed.post/abc123"
  ~post_cid:"bafyreiabc123"
  (function
    | Ok repost_uri -> Printf.printf "Reposted: %s\n" repost_uri
    | Error err -> Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Unrepost *)
Bluesky.unrepost
  ~account_id:"my_account"
  ~repost_uri:"at://did:plc:xyz/app.bsky.feed.repost/ghi789"
  (function
    | Ok () -> Printf.printf "Unreposted\n"
    | Error err -> Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### Social Graph

```ocaml
(* Follow a user *)
Bluesky.follow
  ~account_id:"my_account"
  ~did:"did:plc:xyz123abc"
  (function
    | Ok follow_uri -> Printf.printf "Following: %s\n" follow_uri
    | Error err -> Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Unfollow a user *)
Bluesky.unfollow
  ~account_id:"my_account"
  ~follow_uri:"at://did:plc:xyz/app.bsky.graph.follow/abc123"
  (function
    | Ok () -> Printf.printf "Unfollowed\n"
    | Error err -> Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### Read Operations

```ocaml
(* Get a user profile *)
Bluesky.get_profile
  ~account_id:"my_account"
  ~actor:"alice.bsky.social"
  (function
    | Ok json -> Printf.printf "Profile: %s\n" (Yojson.Basic.to_string json)
    | Error err -> Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Get a thread *)
Bluesky.get_post_thread
  ~account_id:"my_account"
  ~post_uri:"at://did:plc:xyz/app.bsky.feed.post/abc123"
  (function
    | Ok json -> Printf.printf "Thread: %s\n" (Yojson.Basic.to_string json)
    | Error err -> Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Get timeline *)
Bluesky.get_timeline
  ~account_id:"my_account"
  ~limit:50
  (function
    | Ok json -> Printf.printf "Timeline: %s\n" (Yojson.Basic.to_string json)
    | Error err -> Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Get author feed *)
Bluesky.get_author_feed
  ~account_id:"my_account"
  ~actor:"alice.bsky.social"
  ~limit:20
  (function
    | Ok json -> Printf.printf "Author feed: %s\n" (Yojson.Basic.to_string json)
    | Error err -> Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Get likes for a post *)
Bluesky.get_likes
  ~account_id:"my_account"
  ~post_uri:"at://did:plc:xyz/app.bsky.feed.post/abc123"
  (function
    | Ok json -> Printf.printf "Likes: %s\n" (Yojson.Basic.to_string json)
    | Error err -> Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Get followers *)
Bluesky.get_followers
  ~account_id:"my_account"
  ~actor:"alice.bsky.social"
  (function
    | Ok json -> Printf.printf "Followers: %s\n" (Yojson.Basic.to_string json)
    | Error err -> Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### Quote Posts

```ocaml
(* Quote a post with text *)
Bluesky.quote_post
  ~account_id:"my_account"
  ~post_uri:"at://did:plc:xyz/app.bsky.feed.post/abc123"
  ~post_cid:"bafyreiabc123"
  ~text:"Great point! 👍"
  ~media_urls:[]
  (function
    | Social_core.Error_types.Success post_uri -> 
        Printf.printf "Quoted: %s\n" post_uri
    | Social_core.Error_types.Partial_success { result = post_uri; warnings } ->
        Printf.printf "Quoted: %s (with %d warnings)\n" post_uri (List.length warnings)
    | Social_core.Error_types.Failure err ->
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Quote with media *)
Bluesky.quote_post
  ~account_id:"my_account"
  ~post_uri:"at://did:plc:xyz/app.bsky.feed.post/abc123"
  ~post_cid:"bafyreiabc123"
  ~text:"Check this out!"
  ~media_urls:["https://example.com/image.png"]
  (function
    | Social_core.Error_types.Success post_uri -> 
        Printf.printf "Quoted with media: %s\n" post_uri
    | Social_core.Error_types.Partial_success { result = post_uri; warnings } ->
        Printf.printf "Quoted: %s (with %d warnings)\n" post_uri (List.length warnings)
    | Social_core.Error_types.Failure err ->
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### Notifications

```ocaml
(* List notifications *)
Bluesky.list_notifications
  ~account_id:"my_account"
  ~limit:20
  (function
    | Ok json -> Printf.printf "Notifications: %s\n" (Yojson.Basic.to_string json)
    | Error err -> Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Count unread *)
Bluesky.count_unread_notifications
  ~account_id:"my_account"
  (function
    | Ok count -> Printf.printf "Unread: %d\n" count
    | Error err -> Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Mark as seen *)
Bluesky.update_seen_notifications
  ~account_id:"my_account"
  (function
    | Ok () -> Printf.printf "Marked as seen\n"
    | Error err -> Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### Search

```ocaml
(* Search for users *)
Bluesky.search_actors
  ~account_id:"my_account"
  ~query:"ocaml"
  ~limit:10
  (function
    | Ok json -> Printf.printf "Users: %s\n" (Yojson.Basic.to_string json)
    | Error err -> Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Search for posts *)
Bluesky.search_posts
  ~account_id:"my_account"
  ~query:"functional programming"
  ~limit:20
  (function
    | Ok json -> Printf.printf "Posts: %s\n" (Yojson.Basic.to_string json)
    | Error err -> Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### Moderation

```ocaml
(* Mute a user *)
Bluesky.mute_actor
  ~account_id:"my_account"
  ~actor:"spammer.bsky.social"
  (function
    | Ok () -> Printf.printf "Muted\n"
    | Error err -> Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Block a user *)
Bluesky.block_actor
  ~account_id:"my_account"
  ~actor:"troll.bsky.social"
  (function
    | Ok block_uri -> Printf.printf "Blocked: %s\n" block_uri
    | Error err -> Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Unblock *)
Bluesky.unblock_actor
  ~account_id:"my_account"
  ~block_uri:"at://did:plc:xyz/app.bsky.graph.block/abc123"
  (function
    | Ok () -> Printf.printf "Unblocked\n"
    | Error err -> Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### Typed Reads

`Social_bluesky_v1.Views` holds typed post, thread, feed and notification
views. `Reads.Make (Http)` sends the calls to any service URL, with or
without a session; `Make_with_options` has account-bound versions
(`get_post_thread_view`, `get_author_feed_page`, `get_posts`,
`list_notifications_page`) that use the configured PDS.

```ocaml
module Reads = Social_bluesky_v1.Reads.Make (Social_lwt.Cohttp_client.Make)
module V = Social_bluesky_v1.Views

(* Whole conversation under a post: the default depth (6) truncates it *)
let rec count_replies = function
  | V.Post { V.replies; _ } -> List.fold_left (fun n r -> n + 1 + count_replies r) 0 replies
  | V.Not_found_post _ | V.Blocked_post _ | V.Unknown_post _ -> 0

let () =
  Reads.get_post_thread ~service_url:Social_bluesky_v1.Reads.public_api_url
    ~post_uri:"at://did:plc:z72i7hdynmk6r22z27h6tvur/app.bsky.feed.post/3l6oveex3ii2l"
    ~depth:100 ~parent_height:0
    (function
      | Ok thread -> Printf.printf "%d replies\n" (count_replies thread)
      | Error err -> prerr_endline (Error_types.error_to_string err))

(* Replies and mentions since the last page *)
let () =
  Reads.list_notifications ~service_url:"https://bsky.social" ~access_jwt
    ~reasons:[ V.Reply; V.Mention ] ?cursor:last_cursor
    (function
      | Ok page -> (* page.notifications, page.cursor *) ignore page
      | Error _ -> ())
```

## Examples

See the `examples/` directory:
- `simple_cps.ml` - Basic CPS usage with synchronous HTTP client

## Architecture

This package uses continuation-passing style (CPS) to remain agnostic of:
- Async runtime (works with Lwt, Eio, or synchronous code)
- HTTP client library (works with Cohttp, Curly, Httpaf, etc.)

The provider is a functor that takes a CONFIG module implementing:
- HTTP_CLIENT interface for making requests
- Credential storage/retrieval callbacks
- Encryption/decryption callbacks
- Health status reporting callbacks

## Testing

```bash
dune test
```

## License

MIT
