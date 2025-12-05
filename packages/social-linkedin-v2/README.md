# social-linkedin-v2

OCaml library for LinkedIn API v2 integration with runtime-agnostic design.

> **Status:** This library was primarily built using LLMs. OAuth 2.0 and posting functionality have been used successfully. Read operations and other features should be considered untested. Expect breaking changes as we work towards stability.

## Features

- **OAuth 2.0 Authentication**: Full OAuth flow support (authorization_code grant)
- **Personal Posting**: Post text, images, and videos to personal profiles
- **OpenID Connect**: Uses OpenID Connect for user identification
- **Media Upload**: Support for images (up to 10 MB) and videos (up to 200 MB)
- **Alt Text Support**: Accessibility support for media descriptions
- **Token Refresh**: Optional programmatic refresh (Partner Program only)
- **Profile Fetching**: Get current user's profile information
- **Post Management**: Fetch single posts, list user posts, batch get multiple posts
- **Pagination Support**: Built-in scroller pattern for navigating through pages
- **Collection Responses**: Structured responses with paging metadata
- **Batch Operations**: Efficiently fetch multiple entities in one API call
- **Search/FINDER**: Search posts by keywords and criteria
- **Engagement**: Like, unlike, comment on posts
- **Social Metrics**: Fetch engagement statistics (likes, comments, shares)
- **Runtime Agnostic**: Works with Lwt, Eio, or synchronous runtimes via CPS

## LinkedIn Token Refresh Notes

⚠️ **Important**: Programmatic token refresh is ONLY available for LinkedIn Partner Program apps.

### Token Expiration Behavior

**Critical**: This library now correctly reads the actual `expires_in` value from LinkedIn's OAuth response. Previous versions hardcoded a 60-day expiration, which could cause unexpected failures if LinkedIn returned shorter-lived tokens.

LinkedIn's token lifetime can vary:
- **Documented**: 60 days for standard apps
- **Reality**: May be shorter (hours to days) depending on app configuration, scopes, or security policies
- **This library**: Always uses the actual `expires_in` value from LinkedIn's response

**Example**: If LinkedIn returns `{"expires_in": 7200}` (2 hours), the token will be correctly set to expire in 2 hours, not 60 days.

### Standard Apps
- Apps using "Sign In with LinkedIn" or "Share on LinkedIn" products
- Token lifetime varies (check LinkedIn's response)
- Typically do NOT receive a `refresh_token` in OAuth response
- Users must re-authorize through OAuth when tokens expire
- The consent screen is bypassed if user is already logged in
- This is the default configuration

### Partner Program Apps
- Apps enrolled in LinkedIn Marketing Developer Platform or similar programs
- May receive `refresh_token` in OAuth response
- Can use programmatic refresh with `refresh_token` grant
- Set `LINKEDIN_ENABLE_PROGRAMMATIC_REFRESH=true` in environment
- Requires special LinkedIn approval

### Debugging Token Issues

The library now includes comprehensive logging to help diagnose token issues:

```
[LinkedIn] OAuth exchange successful: access_token received, refresh_token ABSENT
[LinkedIn] Token expires in 7200 seconds (0 days) according to LinkedIn response
[LinkedIn] Token expires at: 2025-11-15T14:30:00Z
```

Monitor these logs to understand:
1. Whether LinkedIn provides a `refresh_token`
2. The actual token lifetime (in seconds and days)
3. When the token will expire

If you see short-lived tokens (< 24 hours) consistently:
- Check your LinkedIn app configuration
- Verify the scopes you're requesting
- Review LinkedIn Developer Portal for API changes
- Consider applying for LinkedIn Partner Program for longer-lived tokens

Check your app's products in the LinkedIn Developer Portal to determine which type you have.

## Installation

### With opam (when published)
```bash
opam install social-linkedin-v2
```

### From source
```bash
cd packages/social-linkedin-v2
dune build
dune install
```

## Usage

### Basic Setup with Lwt

```ocaml
open Lwt.Syntax
open Social_provider_core
open Social_linkedin_v2

(* Configure the provider *)
module Config = struct
  module Http = Social_provider_lwt.Cohttp_client
  
  let get_env = Sys.getenv_opt
  
  let get_credentials ~account_id on_success on_error =
    (* Fetch from database *)
    match%lwt Db.get_credentials account_id with
    | Some creds -> Lwt.return (on_success creds)
    | None -> Lwt.return (on_error "Not found")
  
  let update_credentials ~account_id ~credentials on_success on_error =
    (* Update database *)
    match%lwt Db.update_credentials account_id credentials with
    | Ok () -> Lwt.return (on_success ())
    | Error e -> Lwt.return (on_error e)
  
  (* Implement other required functions... *)
end

module LinkedIn = LinkedIn_v2.Make(Config)

(* OAuth flow *)
let start_oauth () =
  LinkedIn.get_oauth_url 
    ~redirect_uri:"https://myapp.com/callback"
    ~state:"random_state_123"
    (fun url ->
      Printf.printf "Visit: %s\n" url;
      ())
    (fun err -> Printf.eprintf "Error: %s\n" err)

(* Exchange code for tokens *)
let complete_oauth code =
  LinkedIn.exchange_code 
    ~code 
    ~redirect_uri:"https://myapp.com/callback"
    (fun credentials ->
      Printf.printf "Got access token: %s\n" credentials.access_token;
      (* Store credentials in database *)
      ())
    (fun err -> Printf.eprintf "Error: %s\n" err)

(* Post to LinkedIn *)
let post_to_linkedin account_id =
  let text = "Hello LinkedIn from OCaml! 🚀" in
  let media = [] in  (* No media for this example *)
  
  LinkedIn.post ~account_id ~text ~media_items:media
    (fun post_id ->
      Printf.printf "Posted successfully: %s\n" post_id;
      ())
    (fun err -> Printf.eprintf "Error: %s\n" err)

(* Post with image *)
let post_with_image account_id =
  let text = "Check out this image!" in
  let media = [{
    storage_key = "path/to/image.jpg";
    media_type = "image";
    alt_text = Some "A beautiful sunset";
  }] in
  
  LinkedIn.post ~account_id ~text ~media_items:media
    (fun post_id ->
      Printf.printf "Posted with image: %s\n" post_id;
      ())
    (fun err -> Printf.eprintf "Error: %s\n" err)
```

### Get User Profile

```ocaml
(* Fetch current user's profile *)
LinkedIn.get_profile ~account_id
  (fun profile ->
    Printf.printf "User ID: %s\n" profile.sub;
    Printf.printf "Name: %s\n" (Option.value profile.name ~default:"N/A");
    Printf.printf "Email: %s\n" (Option.value profile.email ~default:"N/A");
    ())
  (fun err -> Printf.eprintf "Error: %s\n" err)
```

### Fetch User Posts with Pagination

```ocaml
(* Get first page of posts *)
LinkedIn.get_posts ~account_id ~start:0 ~count:10
  (fun collection ->
    List.iter (fun post ->
      Printf.printf "Post ID: %s\n" post.id;
      Option.iter (Printf.printf "Text: %s\n") post.text;
    ) collection.elements;
    
    (* Check pagination info *)
    match collection.paging with
    | Some p -> 
        Printf.printf "Showing %d-%d of %s\n" 
          p.start 
          (p.start + p.count)
          (match p.total with Some t -> string_of_int t | None -> "unknown");
    | None -> ();
    ())
  (fun err -> Printf.eprintf "Error: %s\n" err)
```

### Using Scrollers for Easy Pagination

```ocaml
(* Create a scroller to navigate through pages *)
let scroller = LinkedIn.create_posts_scroller ~account_id ~page_size:5 () in

(* Scroll to next page *)
scroller.scroll_next
  (fun page ->
    List.iter (fun post ->
      Printf.printf "Post: %s\n" post.id;
    ) page.elements;
    
    Printf.printf "Current position: %d\n" (scroller.current_position ());
    Printf.printf "Has more: %b\n" (scroller.has_more ());
    
    (* Can continue scrolling *)
    if scroller.has_more () then
      scroller.scroll_next handle_page handle_error
    else
      Printf.printf "No more posts\n")
  (fun err -> Printf.eprintf "Error: %s\n" err)

(* Scroll back *)
scroller.scroll_back
  (fun page -> (* Handle previous page *) ())
  (fun err -> Printf.eprintf "Error: %s\n" err)
```

### Get Specific Post

```ocaml
(* Fetch a single post by URN *)
LinkedIn.get_post ~account_id ~post_urn:"urn:li:share:123456"
  (fun post ->
    Printf.printf "Post ID: %s\n" post.id;
    Printf.printf "Author: %s\n" post.author;
    Option.iter (Printf.printf "Text: %s\n") post.text;
    ())
  (fun err -> Printf.eprintf "Error: %s\n" err)
```

### Batch Get Multiple Posts

```ocaml
(* Efficiently fetch multiple posts in one API call *)
let post_urns = [
  "urn:li:share:123";
  "urn:li:share:456";
  "urn:li:share:789";
] in

LinkedIn.batch_get_posts ~account_id ~post_urns
  (fun posts ->
    Printf.printf "Retrieved %d posts\n" (List.length posts);
    List.iter (fun post ->
      Printf.printf "- %s: %s\n" 
        post.id 
        (Option.value post.text ~default:"(no text)");
    ) posts;
    ())
  (fun err -> Printf.eprintf "Error: %s\n" err)
```

### Search Posts (FINDER Pattern)

```ocaml
(* Search posts by keywords *)
LinkedIn.search_posts ~account_id ~keywords:"OCaml" ~start:0 ~count:10
  (fun collection ->
    Printf.printf "Found %d posts\n" (List.length collection.elements);
    List.iter (fun post ->
      Printf.printf "- %s\n" (Option.value post.text ~default:"(no text)");
    ) collection.elements;
    ())
  (fun err -> Printf.eprintf "Error: %s\n" err)

(* Use scroller for search *)
let search_scroller = LinkedIn.create_search_scroller 
  ~account_id ~keywords:"functional programming" ~page_size:5 () in

search_scroller.scroll_next
  (fun page -> (* Handle search results *) ())
  (fun err -> (* Handle error *) ())
```

### Engagement APIs

```ocaml
(* Like a post *)
LinkedIn.like_post ~account_id ~post_urn:"urn:li:share:123"
  (fun () -> Printf.printf "Liked!\n")
  (fun err -> Printf.eprintf "Error: %s\n" err)

(* Unlike a post *)
LinkedIn.unlike_post ~account_id ~post_urn:"urn:li:share:123"
  (fun () -> Printf.printf "Unliked!\n")
  (fun err -> Printf.eprintf "Error: %s\n" err)

(* Comment on a post *)
LinkedIn.comment_on_post 
  ~account_id 
  ~post_urn:"urn:li:share:123"
  ~text:"Great insights!"
  (fun comment_id ->
    Printf.printf "Comment posted: %s\n" comment_id;
    ())
  (fun err -> Printf.eprintf "Error: %s\n" err)

(* Get comments on a post *)
LinkedIn.get_post_comments ~account_id ~post_urn:"urn:li:share:123"
  (fun collection ->
    List.iter (fun comment ->
      Printf.printf "%s: %s\n" comment.actor comment.text;
    ) collection.elements;
    ())
  (fun err -> Printf.eprintf "Error: %s\n" err)

(* Get engagement statistics *)
LinkedIn.get_post_engagement ~account_id ~post_urn:"urn:li:share:123"
  (fun stats ->
    Option.iter (Printf.printf "Likes: %d\n") stats.like_count;
    Option.iter (Printf.printf "Comments: %d\n") stats.comment_count;
    Option.iter (Printf.printf "Shares: %d\n") stats.share_count;
    ())
  (fun err -> Printf.eprintf "Error: %s\n" err)
```

### Validate Content

```ocaml
(* Check if content is valid *)
match LinkedIn.validate_content ~text:"My post text" with
| Ok () -> print_endline "Valid!"
| Error msg -> Printf.eprintf "Invalid: %s\n" msg
```

## OAuth Scopes

The LinkedIn provider uses these scopes depending on features:

### Basic Features (Posting)
- `openid` - OpenID Connect authentication
- `profile` - User profile information
- `email` - User email address
- `w_member_social` - Post as individual member

### Reading Features (Profile, Posts)
- `r_liteprofile` - Read profile data (legacy, prefer `profile`)
- `r_member_social` - Read member's posts and social activity

### Engagement Features (Likes, Comments)
- `w_member_social` - Required for liking and commenting
- `r_member_social` - Required for reading engagement stats

### Advanced Features
For posting to company pages, you need:
- `w_organization_social` - Post as organization
- Community Management API access

**Recommended Scope Set for Full Functionality:**
```
openid profile email w_member_social r_member_social
```

## Configuration

Set these environment variables:

```bash
# Required for all apps
LINKEDIN_CLIENT_ID=your_client_id
LINKEDIN_CLIENT_SECRET=your_client_secret
LINKEDIN_REDIRECT_URI=https://yourapp.com/callback

# Optional - only if you have LinkedIn Partner Program access
LINKEDIN_ENABLE_PROGRAMMATIC_REFRESH=true
```

## API Reference

### Core Types

```ocaml
type credentials = {
  access_token: string;
  refresh_token: string option;
  expires_at: string option;  (* RFC3339 timestamp *)
  token_type: string;
}

type media_item = {
  storage_key: string;
  media_type: string;  (* "image" or "video" *)
  alt_text: string option;
}

(* Pagination *)
type paging = {
  start: int;        (* Zero-based index of first result *)
  count: int;        (* Number of results in this response *)
  total: int option; (* Total number of results (if known) *)
}

type 'a collection_response = {
  elements: 'a list;         (* List of entities in this page *)
  paging: paging option;      (* Paging metadata *)
  metadata: Yojson.Basic.t option; (* Optional response metadata *)
}

(* Profile *)
type profile_info = {
  sub: string;                    (* User ID *)
  name: string option;            (* Full name *)
  given_name: string option;      (* First name *)
  family_name: string option;     (* Last name *)
  picture: string option;         (* Profile picture URL *)
  email: string option;           (* Email address *)
  email_verified: bool option;    (* Email verification status *)
  locale: string option;          (* User's locale *)
}

(* Posts *)
type post_info = {
  id: string;                     (* Post URN/ID *)
  author: string;                 (* Author URN *)
  created_at: string option;      (* Creation timestamp *)
  text: string option;            (* Post text content *)
  visibility: string option;      (* Visibility setting *)
  lifecycle_state: string option; (* State: PUBLISHED, DRAFT, etc *)
}

(* Scroller for pagination *)
type 'a scroller = {
  scroll_next: (('a collection_response -> unit) -> (string -> unit) -> unit);
  scroll_back: (('a collection_response -> unit) -> (string -> unit) -> unit);
  current_position: unit -> int;
  has_more: unit -> bool;
}

(* Engagement *)
type engagement_info = {
  like_count: int option;
  comment_count: int option;
  share_count: int option;
  impression_count: int option;
}

type comment_info = {
  id: string;
  actor: string;
  text: string;
  created_at: string option;
}
```

### Authentication Functions

#### `get_oauth_url`
Generate OAuth authorization URL.

```ocaml
val get_oauth_url : 
  redirect_uri:string -> 
  state:string -> 
  (string -> 'a) ->  (* on_success *)
  (string -> 'a) ->  (* on_error *)
  'a
```

#### `exchange_code`
Exchange authorization code for access token.

```ocaml
val exchange_code : 
  code:string -> 
  redirect_uri:string -> 
  (credentials -> 'a) ->  (* on_success *)
  (string -> 'a) ->        (* on_error *)
  'a
```

#### `post`
Post to LinkedIn with optional media.

```ocaml
val post : 
  account_id:string -> 
  text:string -> 
  media_items:media_item list -> 
  (string -> 'a) ->  (* on_success: returns post_id *)
  (string -> 'a) ->  (* on_error *)
  'a
```

#### `validate_content`
Validate post content.

```ocaml
val validate_content : text:string -> (unit, string) result
```

#### `ensure_valid_token`
Ensure access token is valid, refreshing if needed.

```ocaml
val ensure_valid_token : 
  account_id:string -> 
  (string -> 'a) ->  (* on_success: returns access_token *)
  (string -> 'a) ->  (* on_error *)
  'a
```

### Profile Functions

#### `get_profile`
Get current user's profile information using OpenID Connect.

```ocaml
val get_profile :
  account_id:string ->
  (profile_info -> 'a) -> (* on_success *)
  (string -> 'a) ->       (* on_error *)
  'a
```

Requires `openid` and `profile` scopes. Returns basic profile information including user ID, name, email, and profile picture.

### Post Management Functions

#### `get_post`
Fetch a single post by its URN.

```ocaml
val get_post :
  account_id:string ->
  post_urn:string ->
  (post_info -> 'a) -> (* on_success *)
  (string -> 'a) ->    (* on_error *)
  'a
```

#### `get_posts`
Fetch user's posts with pagination support.

```ocaml
val get_posts :
  account_id:string ->
  ?start:int ->              (* Starting index (default: 0) *)
  ?count:int ->              (* Number to fetch (default: 10, max: 50) *)
  (post_info collection_response -> 'a) -> (* on_success *)
  (string -> 'a) ->                        (* on_error *)
  'a
```

Returns a collection response with posts and paging metadata. The response includes:
- `elements`: List of posts
- `paging`: Metadata with start, count, and total
- `metadata`: Optional additional data

#### `batch_get_posts`
Efficiently fetch multiple posts in a single API call.

```ocaml
val batch_get_posts :
  account_id:string ->
  post_urns:string list ->
  (post_info list -> 'a) -> (* on_success *)
  (string -> 'a) ->         (* on_error *)
  'a
```

Batch operations are more efficient than multiple individual requests. Useful when you have specific post URNs to fetch.

### Pagination Helper

#### `create_posts_scroller`
Create a scroller for convenient page navigation.

```ocaml
val create_posts_scroller :
  account_id:string ->
  ?page_size:int ->           (* Posts per page (default: 10) *)
  unit ->
  post_info scroller
```

The returned scroller provides:
- `scroll_next`: Fetch next page
- `scroll_back`: Fetch previous page  
- `current_position`: Get current index
- `has_more`: Check if more pages available

**Example workflow:**
```ocaml
let scroller = create_posts_scroller ~account_id ~page_size:10 () in

(* Scroll forward *)
scroller.scroll_next 
  (fun page -> (* handle page */) ()) 
  (fun err -> (* handle error */) ());

(* Check state *)
if scroller.has_more () then
  scroller.scroll_next handle_page handle_error;

(* Scroll backward *)
scroller.scroll_back handle_page handle_error;
```

### Search Functions

#### `search_posts`
Search for posts using the FINDER pattern with flexible criteria.

```ocaml
val search_posts :
  account_id:string ->
  ?keywords:string ->        (* Search keywords *)
  ?author:string ->          (* Filter by author URN *)
  ?start:int ->              (* Starting index (default: 0) *)
  ?count:int ->              (* Results per page (default: 10, max: 50) *)
  (post_info collection_response -> 'a) -> (* on_success *)
  (string -> 'a) ->                        (* on_error *)
  'a
```

More flexible than `get_posts` as it supports keyword search and filtering.

#### `create_search_scroller`
Create a scroller for search results.

```ocaml
val create_search_scroller :
  account_id:string ->
  ?keywords:string ->
  ?author:string ->
  ?page_size:int ->
  unit ->
  post_info scroller
```

### Engagement Functions

#### `like_post`
Add a like/reaction to a post.

```ocaml
val like_post :
  account_id:string ->
  post_urn:string ->
  (unit -> 'a) -> (* on_success *)
  (string -> 'a) -> (* on_error *)
  'a
```

#### `unlike_post`
Remove a like/reaction from a post.

```ocaml
val unlike_post :
  account_id:string ->
  post_urn:string ->
  (unit -> 'a) -> (* on_success *)
  (string -> 'a) -> (* on_error *)
  'a
```

#### `comment_on_post`
Add a comment to a post.

```ocaml
val comment_on_post :
  account_id:string ->
  post_urn:string ->
  text:string ->
  (string -> 'a) -> (* on_success: returns comment_id *)
  (string -> 'a) -> (* on_error *)
  'a
```

#### `get_post_comments`
Fetch comments on a post with pagination.

```ocaml
val get_post_comments :
  account_id:string ->
  post_urn:string ->
  ?start:int ->
  ?count:int ->
  (comment_info collection_response -> 'a) -> (* on_success *)
  (string -> 'a) ->                           (* on_error *)
  'a
```

#### `get_post_engagement`
Get engagement statistics for a post.

```ocaml
val get_post_engagement :
  account_id:string ->
  post_urn:string ->
  (engagement_info -> 'a) -> (* on_success *)
  (string -> 'a) ->          (* on_error *)
  'a
```

Returns metrics including:
- Like count
- Comment count
- Share count
- Impression count (if available)

**Note**: May require additional API permissions beyond basic posting scopes.
```

## Platform Constraints

### Text
- Maximum length: 3,000 characters
- Minimum length: 1 character
- URLs are automatically converted to link previews

### Images
- Maximum file size: 10 MB
- Supported formats: JPEG, PNG, GIF
- Maximum resolution: 7680 × 4320
- Maximum count: 9 images per post

### Videos
- Maximum file size: 200 MB
- Supported formats: MP4, MOV, MPEG
- Duration: 3 seconds to 10 minutes
- Resolution: 256×144 to 4096×2304
- Maximum count: 1 video per post

### Threading
LinkedIn does not support thread/chain posting. Only single posts are allowed.

## Error Handling

The library uses continuation-passing style (CPS) for error handling:

```ocaml
LinkedIn.post ~account_id ~text ~media_items
  (fun post_id -> 
    (* Success case *)
    handle_success post_id)
  (fun error -> 
    (* Error case *)
    handle_error error)
```

Common error messages:
- `"No refresh token available - please reconnect"` - User needs to re-authorize
- `"Token refresh failed"` - OAuth refresh failed (check credentials)
- `"LinkedIn API error (XXX)"` - API returned an error (see status code)
- `"Programmatic refresh not enabled"` - Standard app trying to use refresh (expected)

## Testing

Run the test suite:

```bash
cd packages/social-linkedin-v2
dune test
```

Tests include:
- OAuth URL generation
- Token exchange
- Person URN retrieval
- Media upload registration
- Content validation
- Token refresh (both standard and partner apps)
- Health status updates

## Development

Build the library:

```bash
dune build
```

Run tests:

```bash
dune test
```

Generate documentation:

```bash
dune build @doc
```

## Architecture

This library uses:

1. **CPS (Continuation-Passing Style)**: No direct async dependencies
2. **Functor Pattern**: Configurable via module parameters
3. **HTTP Client Abstraction**: Works with any HTTP implementation
4. **Storage Abstraction**: Pluggable media storage backend

See `social-provider-core` for interface definitions and `social-provider-lwt` for Lwt adapters.

## License

MIT

## Related Packages

- `social-provider-core` - Core interfaces and types
- `social-provider-lwt` - Lwt runtime adapters
- `social-twitter-v2` - Twitter API v2
- `social-bluesky-v1` - Bluesky AT Protocol
- `social-mastodon-v1` - Mastodon API

## Support

For issues and questions:
- File an issue on GitHub
- Check LinkedIn's API documentation: https://docs.microsoft.com/en-us/linkedin/
- Review LinkedIn Developer Portal: https://www.linkedin.com/developers/
