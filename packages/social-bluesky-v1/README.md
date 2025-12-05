# social-bluesky-v1

Bluesky AT Protocol v1 client for OCaml.

> **Status:** This library was primarily built using LLMs. Authentication and posting functionality have been used successfully. Read operations and other features should be considered untested. Expect breaking changes as we work towards stability.

## Features

### Core Posting
- ✅ Post creation with URI and CID extraction
- ✅ Delete posts
- ✅ Full thread support with proper reply chains
- ✅ Quote posts with optional media
- ✅ Media upload (blobs) - up to 4 images
- ✅ Video upload validation (50MB, 60s max)

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

## Authentication (Not OAuth)

**Bluesky does not use OAuth.** Instead, it uses app passwords via the AT Protocol.

### How It Works

| Property | Value |
|----------|-------|
| Auth method | App passwords |
| OAuth | Not used |
| Token lifetime | App passwords never expire |
| Session tokens | Short-lived, auto-refreshed |

### Setup

1. Go to https://bsky.app/settings/app-passwords
2. Create a new app password
3. Use the identifier (handle or DID) + app password to authenticate

```ocaml
(* Bluesky authenticates via createSession, not OAuth *)
(* Your credentials are: identifier + app_password *)

(* The SDK handles session creation internally when you make API calls *)
(* Just store the identifier and app_password in your credentials *)
```

### No Scopes

Since Bluesky uses app passwords instead of OAuth, there are no scopes to request. App passwords grant full access to the account's capabilities.

### Validation
- ✅ Content length validation (300 chars)
- ✅ Media size validation (1MB images, 50MB video)
- ✅ Media type validation
- ✅ Video duration validation (60s max)

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
  (fun post_uri -> Printf.printf "Posted: %s\n" post_uri)
  (fun error -> Printf.printf "Error: %s\n" error)
```

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
  ~media_urls:[("https://example.com/image.png", Some "A beautiful sunset")]
  (fun post_uri_cid -> Printf.printf "Posted with media: %s\n" post_uri_cid)
  (fun error -> Printf.printf "Error: %s\n" error)
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
  (fun post_uri_cids -> 
    (* Each element is "uri|cid" format *)
    Printf.printf "Posted thread with %d posts\n" (List.length post_uri_cids))
  (fun error -> Printf.printf "Error: %s\n" error)
```

### Deleting a Post

```ocaml
Bluesky.delete_post
  ~account_id:"my_account"
  ~post_uri:"at://did:plc:xyz/app.bsky.feed.post/abc123"
  (fun () -> Printf.printf "Post deleted\n")
  (fun error -> Printf.printf "Error: %s\n" error)
```

### Rich Text with Mentions and Hashtags

```ocaml
(* Text with mentions, hashtags, and URLs will be automatically detected *)
Bluesky.post_single
  ~account_id:"my_account"
  ~text:"Hey @alice.bsky.social check out #ocaml! https://ocaml.org"
  ~media_urls:[]
  (fun post_uri -> Printf.printf "Posted: %s\n" post_uri)
  (fun error -> Printf.printf "Error: %s\n" error)
```

### Like and Repost

```ocaml
(* Like a post *)
Bluesky.like_post
  ~account_id:"my_account"
  ~post_uri:"at://did:plc:xyz/app.bsky.feed.post/abc123"
  ~post_cid:"bafyreiabc123"
  (fun like_uri -> Printf.printf "Liked: %s\n" like_uri)
  (fun error -> Printf.printf "Error: %s\n" error)

(* Unlike a post *)
Bluesky.unlike_post
  ~account_id:"my_account"
  ~like_uri:"at://did:plc:xyz/app.bsky.feed.like/def456"
  (fun () -> Printf.printf "Unliked\n")
  (fun error -> Printf.printf "Error: %s\n" error)

(* Repost *)
Bluesky.repost
  ~account_id:"my_account"
  ~post_uri:"at://did:plc:xyz/app.bsky.feed.post/abc123"
  ~post_cid:"bafyreiabc123"
  (fun repost_uri -> Printf.printf "Reposted: %s\n" repost_uri)
  (fun error -> Printf.printf "Error: %s\n" error)

(* Unrepost *)
Bluesky.unrepost
  ~account_id:"my_account"
  ~repost_uri:"at://did:plc:xyz/app.bsky.feed.repost/ghi789"
  (fun () -> Printf.printf "Unreposted\n")
  (fun error -> Printf.printf "Error: %s\n" error)
```

### Social Graph

```ocaml
(* Follow a user *)
Bluesky.follow
  ~account_id:"my_account"
  ~did:"did:plc:xyz123abc"
  (fun follow_uri -> Printf.printf "Following: %s\n" follow_uri)
  (fun error -> Printf.printf "Error: %s\n" error)

(* Unfollow a user *)
Bluesky.unfollow
  ~account_id:"my_account"
  ~follow_uri:"at://did:plc:xyz/app.bsky.graph.follow/abc123"
  (fun () -> Printf.printf "Unfollowed\n")
  (fun error -> Printf.printf "Error: %s\n" error)
```

### Read Operations

```ocaml
(* Get a user profile *)
Bluesky.get_profile
  ~account_id:"my_account"
  ~actor:"alice.bsky.social"
  (fun json ->
    Printf.printf "Profile: %s\n" (Yojson.Basic.to_string json))
  (fun error -> Printf.printf "Error: %s\n" error)

(* Get a thread *)
Bluesky.get_post_thread
  ~account_id:"my_account"
  ~post_uri:"at://did:plc:xyz/app.bsky.feed.post/abc123"
  (fun json -> 
    Printf.printf "Thread: %s\n" (Yojson.Basic.to_string json))
  (fun error -> Printf.printf "Error: %s\n" error)

(* Get timeline *)
Bluesky.get_timeline
  ~account_id:"my_account"
  ~limit:50
  (fun json ->
    Printf.printf "Timeline: %s\n" (Yojson.Basic.to_string json))
  (fun error -> Printf.printf "Error: %s\n" error)

(* Get author feed *)
Bluesky.get_author_feed
  ~account_id:"my_account"
  ~actor:"alice.bsky.social"
  ~limit:20
  (fun json ->
    Printf.printf "Author feed: %s\n" (Yojson.Basic.to_string json))
  (fun error -> Printf.printf "Error: %s\n" error)

(* Get likes for a post *)
Bluesky.get_likes
  ~account_id:"my_account"
  ~post_uri:"at://did:plc:xyz/app.bsky.feed.post/abc123"
  (fun json ->
    Printf.printf "Likes: %s\n" (Yojson.Basic.to_string json))
  (fun error -> Printf.printf "Error: %s\n" error)

(* Get followers *)
Bluesky.get_followers
  ~account_id:"my_account"
  ~actor:"alice.bsky.social"
  (fun json ->
    Printf.printf "Followers: %s\n" (Yojson.Basic.to_string json))
  (fun error -> Printf.printf "Error: %s\n" error)
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
  (fun post_uri -> Printf.printf "Quoted: %s\n" post_uri)
  (fun error -> Printf.printf "Error: %s\n" error)

(* Quote with media *)
Bluesky.quote_post
  ~account_id:"my_account"
  ~post_uri:"at://did:plc:xyz/app.bsky.feed.post/abc123"
  ~post_cid:"bafyreiabc123"
  ~text:"Check this out!"
  ~media_urls:["https://example.com/image.png"]
  (fun post_uri -> Printf.printf "Quoted with media: %s\n" post_uri)
  (fun error -> Printf.printf "Error: %s\n" error)
```

### Notifications

```ocaml
(* List notifications *)
Bluesky.list_notifications
  ~account_id:"my_account"
  ~limit:20
  (fun json ->
    Printf.printf "Notifications: %s\n" (Yojson.Basic.to_string json))
  (fun error -> Printf.printf "Error: %s\n" error)

(* Count unread *)
Bluesky.count_unread_notifications
  ~account_id:"my_account"
  (fun count -> Printf.printf "Unread: %d\n" count)
  (fun error -> Printf.printf "Error: %s\n" error)

(* Mark as seen *)
Bluesky.update_seen_notifications
  ~account_id:"my_account"
  (fun () -> Printf.printf "Marked as seen\n")
  (fun error -> Printf.printf "Error: %s\n" error)
```

### Search

```ocaml
(* Search for users *)
Bluesky.search_actors
  ~account_id:"my_account"
  ~query:"ocaml"
  ~limit:10
  (fun json ->
    Printf.printf "Users: %s\n" (Yojson.Basic.to_string json))
  (fun error -> Printf.printf "Error: %s\n" error)

(* Search for posts *)
Bluesky.search_posts
  ~account_id:"my_account"
  ~query:"functional programming"
  ~limit:20
  (fun json ->
    Printf.printf "Posts: %s\n" (Yojson.Basic.to_string json))
  (fun error -> Printf.printf "Error: %s\n" error)
```

### Moderation

```ocaml
(* Mute a user *)
Bluesky.mute_actor
  ~account_id:"my_account"
  ~actor:"spammer.bsky.social"
  (fun () -> Printf.printf "Muted\n")
  (fun error -> Printf.printf "Error: %s\n" error)

(* Block a user *)
Bluesky.block_actor
  ~account_id:"my_account"
  ~actor:"troll.bsky.social"
  (fun block_uri -> Printf.printf "Blocked: %s\n" block_uri)
  (fun error -> Printf.printf "Error: %s\n" error)

(* Unblock *)
Bluesky.unblock_actor
  ~account_id:"my_account"
  ~block_uri:"at://did:plc:xyz/app.bsky.graph.block/abc123"
  (fun () -> Printf.printf "Unblocked\n")
  (fun error -> Printf.printf "Error: %s\n" error)
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
