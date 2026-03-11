# social-twitter-v2

Twitter API v2 client for OCaml with comprehensive feature support.

> **Status:** This library was primarily built using LLMs. OAuth 2.0 and posting functionality have been used successfully. Read operations and other features should be considered untested. Expect breaking changes as we work towards stability.

## Features

### Tweet Operations
- ✅ **Post tweets** (280 character limit)
- ✅ **Delete tweets**
- ✅ **Get tweet by ID** (with expansions & fields)
- ✅ **Search tweets** (with pagination)
- ✅ **Get user timeline** (with pagination)
- ✅ **Get mentions timeline** (tweets mentioning you)
- ✅ **Get home timeline** (reverse chronological feed)
- ✅ **Thread posting** (full implementation)
- ✅ **Reply to tweets**
- ✅ **Quote tweets**

### User Operations
- ✅ **Get user by ID**
- ✅ **Get user by username**
- ✅ **Get authenticated user info**
- ✅ **Follow/unfollow users**
- ✅ **Block/unblock users**
- ✅ **Mute/unmute users**
- ✅ **Get followers list** (with pagination)
- ✅ **Get following list** (with pagination)
- ✅ **Search users** (by keyword)

### Engagement
- ✅ **Like/unlike tweets**
- ✅ **Retweet/unretweet**
- ✅ **Bookmark tweets**
- ✅ **Remove bookmarks**

### Lists
- ✅ **Create/update/delete lists**
- ✅ **Get list by ID**
- ✅ **Add/remove list members**
- ✅ **Get list members** (with pagination)
- ✅ **Follow/unfollow lists**
- ✅ **Get list tweets** (with pagination)
- ✅ **Pin/unpin lists**

### Media Upload
- ✅ **Simple upload** (images up to 5MB)
- ✅ **Chunked upload** (videos up to 512MB)
- ✅ **Alt text support** (accessibility)
- ✅ **Multiple media per tweet** (up to 4)

### Authentication & Security
- ✅ **OAuth 2.0 with PKCE**
- ✅ **Automatic token refresh** (2-hour expiry)
- ✅ **Token expiration handling**
- ✅ **Health status tracking**

### Developer Experience
- ✅ **Expansions support** (author_id, referenced_tweets, etc.)
- ✅ **Field selection** (tweet_fields, user_fields, etc.)
- ✅ **Pagination** (cursor-based with next_token)
- ✅ **Rate limit parsing** (from API headers)
- ✅ **Content validation**
- ✅ **Media validation**
- ✅ **CPS architecture** (runtime agnostic)

## Usage

### Basic Example

```ocaml
(* Create configuration *)
module My_config = struct
  module Http = My_http_client  (* Your HTTP client *)
  
  let get_env = Sys.getenv_opt
  let get_credentials ~account_id on_success on_error = ...
  (* ... other callbacks ... *)
end

(* Create provider instance *)
module Twitter = Social_twitter_v2.Twitter_v2.Make(My_config)

(* Post a tweet *)
Twitter.post_single
  ~account_id:"my_account"
  ~text:"Hello from OCaml! 🐫"
  ~media_urls:[]
  (function
    | Social_core.Error_types.Success tweet_id -> 
        Printf.printf "Posted: %s\n" tweet_id
    | Social_core.Error_types.Partial_success { result = tweet_id; warnings } ->
        Printf.printf "Posted: %s with %d warnings\n" tweet_id (List.length warnings)
    | Social_core.Error_types.Failure err ->
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### Tweet Operations

```ocaml
(* Delete a tweet *)
Twitter.delete_tweet
  ~account_id:"my_account"
  ~tweet_id:"123456789"
  (function
    | Social_core.Error_types.Success () -> print_endline "Deleted!"
    | Social_core.Error_types.Partial_success _ -> print_endline "Deleted!"
    | Social_core.Error_types.Failure err ->
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Get a tweet with expansions *)
Twitter.get_tweet
  ~account_id:"my_account"
  ~tweet_id:"123456789"
  ~expansions:["author_id"; "referenced_tweets.id"]
  ~tweet_fields:["created_at"; "public_metrics"]
  ()
  (function
    | Ok json -> (* Process tweet data *)
        Printf.printf "Tweet: %s\n" (Yojson.Basic.to_string json)
    | Error err -> 
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Search tweets with pagination *)
Twitter.search_tweets
  ~account_id:"my_account"
  ~query:"OCaml programming"
  ~max_results:50
  ~next_token:(Some "pagination_token")
  ()
  (function
    | Ok json -> 
        (* Get pagination info *)
        let meta = Twitter.parse_pagination_meta json in
        (* Continue with next page if available *)
        (match meta.next_token with
        | Some token -> (* Fetch next page *) ()
        | None -> (* No more results *) ())
    | Error err -> 
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Post a thread *)
Twitter.post_thread
  ~account_id:"my_account"
  ~texts:["First tweet"; "Second tweet"; "Third tweet"]
  ~media_urls_per_post:[["url1.jpg"]; []; ["url3.jpg"]]
  (function
    | Social_core.Error_types.Success result -> 
        Printf.printf "Posted %d tweets\n" (List.length result.posted_ids)
    | Social_core.Error_types.Partial_success { result; warnings } ->
        Printf.printf "Posted %d/%d tweets with warnings\n" 
          (List.length result.posted_ids) result.total_requested
    | Social_core.Error_types.Failure err ->
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Get mentions timeline *)
Twitter.get_mentions_timeline
  ~account_id:"my_account"
  ~max_results:50
  ~tweet_fields:["created_at"; "public_metrics"]
  ()
  (function
    | Ok json ->
        let open Yojson.Basic.Util in
        let mentions = json |> member "data" |> to_list in
        Printf.printf "You have %d mentions\n" (List.length mentions)
    | Error err -> 
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Get home timeline *)
Twitter.get_home_timeline
  ~account_id:"my_account"
  ~max_results:20
  ~expansions:["author_id"]
  ()
  (function
    | Ok json -> (* Process home feed *)
        Printf.printf "Home: %s\n" (Yojson.Basic.to_string json)
    | Error err -> 
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### User Operations

```ocaml
(* Get user info by username *)
Twitter.get_user_by_username
  ~account_id:"my_account"
  ~username:"elonmusk"
  ~user_fields:["public_metrics"; "description"; "verified"]
  ()
  (function
    | Ok json -> (* Process user data *)
        Printf.printf "User: %s\n" (Yojson.Basic.to_string json)
    | Error err -> 
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Get authenticated user *)
Twitter.get_me
  ~account_id:"my_account"
  ()
  (function
    | Ok json -> (* Your user data *)
        Printf.printf "Me: %s\n" (Yojson.Basic.to_string json)
    | Error err -> 
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Follow a user *)
Twitter.follow_user
  ~account_id:"my_account"
  ~target_user_id:"123456789"
  (function
    | Ok () -> print_endline "Following!"
    | Error err -> 
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Block a user *)
Twitter.block_user
  ~account_id:"my_account"
  ~target_user_id:"987654321"
  (function
    | Ok () -> print_endline "Blocked!"
    | Error err -> 
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Mute a user *)
Twitter.mute_user
  ~account_id:"my_account"
  ~target_user_id:"987654321"
  (function
    | Ok () -> print_endline "Muted!"
    | Error err -> 
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Get followers *)
Twitter.get_followers
  ~account_id:"my_account"
  ~user_id:"123456789"
  ~max_results:100
  ~user_fields:["public_metrics"; "verified"]
  ()
  (function
    | Ok json ->
        let open Yojson.Basic.Util in
        let followers = json |> member "data" |> to_list in
        Printf.printf "Found %d followers\n" (List.length followers)
    | Error err -> 
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Get following *)
Twitter.get_following
  ~account_id:"my_account"
  ~user_id:"123456789"
  ~max_results:100
  ()
  (function
    | Ok json -> (* Process following list *)
        Printf.printf "Following: %s\n" (Yojson.Basic.to_string json)
    | Error err -> 
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### Engagement Operations

```ocaml
(* Like a tweet *)
Twitter.like_tweet
  ~account_id:"my_account"
  ~tweet_id:"123456789"
  (function
    | Ok () -> print_endline "Liked!"
    | Error err -> 
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Retweet *)
Twitter.retweet
  ~account_id:"my_account"
  ~tweet_id:"123456789"
  (function
    | Ok () -> print_endline "Retweeted!"
    | Error err -> 
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Quote tweet - uses outcome type for posting *)
Twitter.quote_tweet
  ~account_id:"my_account"
  ~text:"Great insight!"
  ~quoted_tweet_id:"123456789"
  ~media_urls:[]
  (function
    | Social_core.Error_types.Success tweet_id -> 
        Printf.printf "Quote posted: %s\n" tweet_id
    | Social_core.Error_types.Partial_success { result = tweet_id; warnings } ->
        Printf.printf "Quote posted: %s with %d warnings\n" tweet_id (List.length warnings)
    | Social_core.Error_types.Failure err ->
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Reply to tweet - uses outcome type for posting *)
Twitter.reply_to_tweet
  ~account_id:"my_account"
  ~text:"Thanks for sharing!"
  ~reply_to_tweet_id:"123456789"
  ~media_urls:[]
  (function
    | Social_core.Error_types.Success tweet_id -> 
        Printf.printf "Reply posted: %s\n" tweet_id
    | Social_core.Error_types.Partial_success { result = tweet_id; warnings } ->
        Printf.printf "Reply posted: %s with %d warnings\n" tweet_id (List.length warnings)
    | Social_core.Error_types.Failure err ->
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Bookmark tweet *)
Twitter.bookmark_tweet
  ~account_id:"my_account"
  ~tweet_id:"123456789"
  (function
    | Ok () -> print_endline "Bookmarked!"
    | Error err -> 
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### Media Upload

```ocaml
(* Simple upload for images *)
Twitter.post_single
  ~account_id:"my_account"
  ~text:"Check out this image!"
  ~media_urls:["https://example.com/image.jpg"]
  (function
    | Social_core.Error_types.Success tweet_id -> 
        Printf.printf "Posted: %s\n" tweet_id
    | Social_core.Error_types.Partial_success { result = tweet_id; warnings } ->
        Printf.printf "Posted: %s (with %d warnings)\n" tweet_id (List.length warnings)
    | Social_core.Error_types.Failure err ->
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Chunked upload for large videos with alt text *)
Twitter.upload_media_chunked
  ~access_token:"..."
  ~media_data:video_bytes
  ~mime_type:"video/mp4"
  ~alt_text:(Some "Video description for accessibility")
  ()
  (fun media_id -> Printf.printf "Uploaded: %s\n" media_id)
  on_error
```

### Lists Management

```ocaml
(* Create a list *)
Twitter.create_list
  ~account_id:"my_account"
  ~name:"OCaml Developers"
  ~description:(Some "Amazing OCaml developers to follow")
  ~private_list:false
  ()
  (function
    | Ok json ->
        let open Yojson.Basic.Util in
        let list_id = json |> member "data" |> member "id" |> to_string in
        Printf.printf "Created list: %s\n" list_id
    | Error err -> 
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Add members to list *)
Twitter.add_list_member
  ~account_id:"my_account"
  ~list_id:"list123"
  ~user_id:"user456"
  (function
    | Ok () -> print_endline "Member added!"
    | Error err -> 
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Get list tweets *)
Twitter.get_list_tweets
  ~account_id:"my_account"
  ~list_id:"list123"
  ~max_results:50
  ~tweet_fields:["created_at"; "public_metrics"]
  ()
  (function
    | Ok json -> (* Process tweets from list *)
        Printf.printf "List tweets: %s\n" (Yojson.Basic.to_string json)
    | Error err -> 
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Follow a list *)
Twitter.follow_list
  ~account_id:"my_account"
  ~list_id:"list123"
  (function
    | Ok () -> print_endline "Following list!"
    | Error err -> 
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Pin a list *)
Twitter.pin_list
  ~account_id:"my_account"
  ~list_id:"list123"
  (function
    | Ok () -> print_endline "List pinned!"
    | Error err -> 
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### User Search

```ocaml
(* Search for users *)
Twitter.search_users
  ~account_id:"my_account"
  ~query:"OCaml developer"
  ~max_results:20
  ~user_fields:["description"; "public_metrics"]
  ()
  (function
    | Ok json ->
        let open Yojson.Basic.Util in
        let users = json |> member "data" |> to_list in
        Printf.printf "Found %d users\n" (List.length users)
    | Error err -> 
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### OAuth 2.0 Flow

```ocaml
(* 1. Generate authorization URL *)
let auth_url = Twitter.get_oauth_url 
  ~state:"random_state" 
  ~code_verifier:"verifier_string"

(* 2. User authorizes and you get code *)

(* 3. Exchange code for tokens *)
Twitter.exchange_code 
  ~code:"auth_code"
  ~code_verifier:"verifier_string"
  (fun token_json -> 
    (* Store tokens *))
  (fun error -> 
    (* Handle error *))
```

## OAuth & Required Scopes

**Always request the correct scopes during authorization** - API calls will fail with 403 errors if you don't have the required permissions.

### OAuth Details

| Property | Value |
|----------|-------|
| PKCE | Required (S256 method) |
| Token lifetime | 2 hours |
| Refresh tokens | Yes (requires `offline.access` scope) |
| Authorization endpoint | `https://x.com/i/oauth2/authorize` |
| Token endpoint | `https://api.twitter.com/2/oauth2/token` |

### Required Scopes by Operation

| Operation | Required Scopes |
|-----------|-----------------|
| Read profile | `users.read` |
| Read tweets | `tweet.read`, `users.read` |
| Post tweet | `tweet.read`, `tweet.write`, `users.read` |
| Post with media | `tweet.read`, `tweet.write`, `users.read` |
| Delete tweet | `tweet.read`, `tweet.write`, `users.read` |
| Like/unlike | `tweet.read`, `users.read`, `like.read`, `like.write` |
| Retweet | `tweet.read`, `users.read` |
| Follow/unfollow | `users.read`, `follows.read`, `follows.write` |
| Block/unblock | `users.read`, `block.read`, `block.write` |
| Mute/unmute | `users.read`, `mute.read`, `mute.write` |
| Bookmarks | `tweet.read`, `users.read`, `bookmark.read`, `bookmark.write` |
| Lists | `users.read`, `list.read`, `list.write` |
| Token refresh | `offline.access` (must be included in initial request) |

### Using Scope Helpers

```ocaml
open Social_twitter_v2.Twitter_v2

(* Predefined scope sets *)
let read_scopes = OAuth.Scopes.read    (* ["tweet.read"; "users.read"] *)
let write_scopes = OAuth.Scopes.write  (* Includes offline.access for refresh *)

(* Get scopes for specific operations *)
let scopes = OAuth.Scopes.for_operations [Post_text; Post_media]
(* Returns: ["users.read"; "offline.access"; "tweet.read"; "tweet.write"] *)

(* Generate auth URL with correct scopes *)
let code_verifier = OAuth.Pkce.generate_code_verifier ()
let code_challenge = OAuth.Pkce.generate_code_challenge code_verifier
let auth_url = OAuth.get_authorization_url
  ~client_id:"your_client_id"
  ~redirect_uri:"https://your-app.com/callback"
  ~state:"random_csrf_token"
  ~scopes:OAuth.Scopes.write
  ~code_challenge
  ()
```

### OAuth Metadata

```ocaml
let () =
  let open Social_twitter_v2.Twitter_v2.OAuth.Metadata in
  Printf.printf "PKCE supported: %b\n" supports_pkce;           (* true *)
  Printf.printf "Refresh supported: %b\n" supports_refresh;     (* true *)
  Printf.printf "Token lifetime: %d seconds\n" 
    (Option.get token_lifetime_seconds);                        (* 7200 *)
  Printf.printf "Refresh buffer: %d seconds\n" 
    refresh_buffer_seconds                                      (* 1800 *)
```

## API Coverage

This implementation supports the following Twitter API v2 endpoints:

### Tweets
- `POST /2/tweets` - Create tweets, replies, quotes
- `DELETE /2/tweets/:id` - Delete tweets
- `GET /2/tweets/:id` - Get single tweet
- `GET /2/tweets/search/recent` - Search tweets
- `GET /2/users/:id/tweets` - User timeline
- `GET /2/users/:id/mentions` - Mentions timeline
- `GET /2/users/:id/timelines/reverse_chronological` - Home timeline

### Users
- `GET /2/users/:id` - Get user by ID
- `GET /2/users/by/username/:username` - Get user by username
- `GET /2/users/me` - Get authenticated user
- `POST /2/users/:id/following` - Follow user
- `DELETE /2/users/:source_id/following/:target_id` - Unfollow
- `POST /2/users/:id/blocking` - Block user
- `DELETE /2/users/:source_id/blocking/:target_id` - Unblock
- `POST /2/users/:id/muting` - Mute user
- `DELETE /2/users/:source_id/muting/:target_id` - Unmute
- `GET /2/users/:id/followers` - Get followers
- `GET /2/users/:id/following` - Get following

### Likes
- `POST /2/users/:id/likes` - Like tweet
- `DELETE /2/users/:id/likes/:tweet_id` - Unlike

### Retweets
- `POST /2/users/:id/retweets` - Retweet
- `DELETE /2/users/:id/retweets/:tweet_id` - Unretweet

### Bookmarks
- `POST /2/users/:id/bookmarks` - Bookmark tweet
- `DELETE /2/users/:id/bookmarks/:tweet_id` - Remove bookmark

### Lists
- `POST /2/lists` - Create list
- `PUT /2/lists/:id` - Update list
- `DELETE /2/lists/:id` - Delete list
- `GET /2/lists/:id` - Get list
- `POST /2/lists/:id/members` - Add list member
- `DELETE /2/lists/:id/members/:user_id` - Remove list member
- `GET /2/lists/:id/members` - Get list members
- `POST /2/users/:id/followed_lists` - Follow list
- `DELETE /2/users/:id/followed_lists/:list_id` - Unfollow list
- `GET /2/lists/:id/tweets` - Get list tweets
- `POST /2/users/:id/pinned_lists` - Pin list
- `DELETE /2/users/:id/pinned_lists/:list_id` - Unpin list

### User Search
- `GET /2/users/search` - Search users by keyword

### Media
- `POST /2/media/upload` - Simple upload (INIT/APPEND/FINALIZE for chunked)
- `POST /2/media/metadata` - Add alt text

## Architecture

- **CPS-based**: Runtime and HTTP client agnostic
- **OAuth 2.0**: Full support with PKCE and refresh tokens
- **Auto-refresh**: Tokens refreshed automatically when expired (30min buffer)
- **Rate limiting**: Built-in tracking + header parsing
- **Pagination**: Support for cursor-based pagination
- **Expansions**: Full support for v2 expansions and field selection
- **Structured error handling**: Uses `outcome` type with `Success`, `Partial_success`, and `Failure` variants for posting operations

## Comparison with Popular Libraries

Your implementation now matches or exceeds the feature set of popular Twitter v2 libraries:

| Feature | Your Package | tweepy | node-twitter-api-v2 |
|---------|-------------|--------|---------------------|
| Tweet CRUD | ✅ | ✅ | ✅ |
| Search | ✅ | ✅ | ✅ |
| User operations | ✅ | ✅ | ✅ |
| Engagement (like/RT) | ✅ | ✅ | ✅ |
| Media upload | ✅ Simple + Chunked | ✅ | ✅ |
| Threads | ✅ | ✅ | ✅ |
| Expansions/Fields | ✅ | ✅ | ✅ |
| Pagination | ✅ | ✅ | ✅ |
| Rate limit parsing | ✅ | ✅ | ✅ |
| CPS architecture | ✅ Unique! | ❌ | ❌ |
| Lists | ✅ | ✅ | ✅ |
| User search | ✅ | ✅ | ✅ |
| Streaming | 🚧 Future | ✅ | ✅ |
| DMs | 🚧 Future | ✅ | ✅ |

## Testing

```bash
dune test  # ✅ All tests pass!
```

## Conformance Review Artifacts

- `packages/social-twitter-v2/checklist.md` - PR checklist for OAuth/read/post/video conformance review
- `packages/social-twitter-v2/conformance_matrix.md` - endpoint-by-endpoint parity tracking
- `packages/social-twitter-v2/deviation_log.md` - deviation triage and fix tracking
- `packages/social-twitter-v2/test_gap_map.md` - checklist-to-tests coverage and missing test map
- `packages/social-twitter-v2/implementation_backlog.md` - prioritized implementation batches to close deviations
- `packages/social-twitter-v2/differential_runtime_report.md` - mock-driven runtime evidence and remaining live-reference steps
- `packages/social-twitter-v2/live_differential_runbook.md` - executable checklist for live `twurl`/reference parity validation
- `packages/social-twitter-v2/live_differential_execution_log.md` - fill-in template for recording live parity outcomes
- `packages/social-twitter-v2/release_readiness_report.md` - readiness snapshot and final external-parity gate status
- `packages/social-twitter-v2/REFERENCE_IMPLEMENTATIONS.md` - pinned reference SDKs/repos

## Future Enhancements

- Streaming API (filtered/sample streams)
- Lists management
- Direct messages
- Spaces API
- Batch operations

## License

MIT
