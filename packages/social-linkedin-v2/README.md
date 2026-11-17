# social-linkedin-v2

OCaml library for LinkedIn API v2 integration with runtime-agnostic design.

> **Status:** This library was primarily built using LLMs. OAuth, posting, and key read paths now have mocked contract tests. Live LinkedIn integration testing is still recommended before production rollout. Expect breaking changes as we work towards stability.

## Features

- **OAuth 2.0 Authentication**: Full OAuth flow support (authorization_code grant)
- **Personal Posting**: Post text, images, and videos to personal profiles
- **OpenID Connect**: Uses OpenID Connect for user identification
- **Media Upload**: Support for images (up to 8 MB) and videos (up to 200 MB)
- **Alt Text Support**: Accessibility support for media descriptions
- **Token Refresh**: Optional programmatic refresh (Partner Program only)
- **Profile Fetching**: Get current user's profile information
- **Post Management**: Fetch single posts, list user posts, batch get multiple posts
- **Pagination Support**: Built-in scroller pattern for navigating through pages
- **Collection Responses**: Structured responses with paging metadata
- **Batch Operations**: Efficiently fetch multiple entities in one API call
- **Search/FINDER**: Author-based finder support for post retrieval
- **Engagement**: Like, unlike, comment on posts
- **Engagement Data**: Fetch comments and basic interaction data for posts
- **Runtime Agnostic**: Works with Lwt, Eio, or synchronous runtimes via CPS

## Coverage Status

Test labels used below:
- `mocked` = covered by deterministic unit/contract tests in `test/test_linkedin.ml`
- `live` = validated against real LinkedIn APIs (limited, depends on app/account permissions)

Current status:
- OAuth (`get_oauth_url`, `exchange_code`, refresh gating/flows, state validation): `mocked`, limited `live`
- Posting (`post_single`, URL preview, video path, media upload registration): `mocked`, limited `live`
- Reads (`get_profile`, `get_post`, `get_posts`, `batch_get_posts`, author finder search): `mocked`
- Engagement writes/reads (`like_post`, `unlike_post`, `comment_on_post`, `get_post_comments`, `get_post_engagement`): `mocked`
- Pagination helpers (`create_posts_scroller`, `create_search_scroller`): `mocked`

## LinkedIn Token Refresh Notes

- Programmatic refresh is available only to approved LinkedIn Partner Program apps.
- Standard apps should expect re-authorization when access tokens expire.
- Token expiration is taken from LinkedIn's returned `expires_in` value (no hardcoded lifetime).
- Enable partner refresh with `LINKEDIN_ENABLE_PROGRAMMATIC_REFRESH=true`.

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
open Social_core
open Social_linkedin_v2

(* Configure the provider *)
module Config = struct
  module Http = Social_lwt.Cohttp_client.Make
  
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
  
  LinkedIn.post_single ~account_id ~text ~media_urls:[]
    (function
      | Social_core.Error_types.Success post_id ->
          Printf.printf "Posted successfully: %s\n" post_id
      | Social_core.Error_types.Partial_success { result = post_id; warnings } ->
          Printf.printf "Posted: %s with %d warnings\n" post_id (List.length warnings)
      | Social_core.Error_types.Failure err ->
          Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Post with image *)
let post_with_image account_id =
  let text = "Check out this image!" in
  
  LinkedIn.post_single ~account_id ~text ~media_urls:["https://cdn.example.com/image.jpg"]
    (function
      | Social_core.Error_types.Success post_id ->
          Printf.printf "Posted with image: %s\n" post_id
      | Social_core.Error_types.Partial_success { result = post_id; warnings } ->
          Printf.printf "Posted: %s (with %d warnings)\n" post_id (List.length warnings)
      | Social_core.Error_types.Failure err ->
          Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### Account Linking Examples

#### Person + organization scope consent URL

```ocaml
let start_linkedin_linking () =
  LinkedIn.get_oauth_url
    ~include_organization_scopes:true
    ~redirect_uri:"https://myapp.com/callback"
    ~state:"csrf_token_123"
    (fun url ->
      (* Redirect user to LinkedIn consent screen *)
      Printf.printf "Redirect user to: %s\n" url)
    (fun err -> Printf.eprintf "OAuth URL error: %s\n" err)
```

#### OAuth callback: link account and fetch page/org access

```ocaml
let complete_linkedin_linking ~account_id ~code =
  LinkedIn.exchange_code_and_get_organizations
    ~code
    ~redirect_uri:"https://myapp.com/callback"
    ~acl_state:"APPROVED"
    (fun (credentials, organizations) ->
      (* 1) Persist OAuth credentials for this account_id *)
      Db.save_linkedin_credentials account_id credentials;

      (* 2) Store available author URNs for org/page posting UI *)
      Db.replace_linkedin_org_access account_id organizations;
      List.iter
        (fun org ->
          Printf.printf "Org access: %s role=%s state=%s\n"
            org.organization_urn
            (Option.value org.role ~default:"unknown")
            (Option.value org.state ~default:"unknown"))
        organizations;

      (* 3) Optionally preselect one author_urn for future posts *)
      (match LinkedIn.select_preferred_organization_access organizations with
      | Some preferred ->
          Db.set_default_linkedin_author account_id preferred.organization_urn
      | None -> ());

      ())
    (fun err -> Printf.eprintf "Linking failed: %s\n" err)
```

#### OAuth callback: link account and fetch one preferred author candidate

```ocaml
let complete_linkedin_linking_with_default ~account_id ~code =
  LinkedIn.exchange_code_and_get_preferred_organization
    ~code
    ~redirect_uri:"https://myapp.com/callback"
    ~acl_state:"APPROVED"
    (fun (credentials, preferred_opt) ->
      Db.save_linkedin_credentials account_id credentials;
      (match preferred_opt with
      | Some preferred ->
          Db.set_default_linkedin_author account_id preferred.organization_urn
      | None -> ()))
    (fun err -> Printf.eprintf "Linking failed: %s\n" err)
```

#### Refresh org/page access for a linked account

```ocaml
let refresh_linkedin_org_access account_id =
  LinkedIn.get_organization_access ~account_id ~acl_state:"APPROVED"
    (function
      | Ok organizations ->
          Printf.printf "Found %d organizations\n" (List.length organizations)
      | Error err ->
          Printf.eprintf "Organization access fetch failed: %s\n"
            (Social_core.Error_types.error_to_string err))
```

#### Fetch one best org/page author candidate

```ocaml
let refresh_default_linkedin_author account_id =
  LinkedIn.get_preferred_organization_access ~account_id ~acl_state:"APPROVED"
    (function
      | Ok (Some preferred) ->
          Db.set_default_linkedin_author account_id preferred.organization_urn
      | Ok None ->
          Printf.printf "No eligible organization access found\n"
      | Error err ->
          Printf.eprintf "Preferred org fetch failed: %s\n"
            (Social_core.Error_types.error_to_string err))
```

#### Post as selected organization/page

```ocaml
let post_as_organization ~account_id ~organization_urn ~text =
  LinkedIn.post_single
    ~account_id
    ~author_urn:organization_urn
    ~text
    ~media_urls:[]
    (function
      | Social_core.Error_types.Success post_id ->
          Printf.printf "Posted as org: %s\n" post_id
      | Social_core.Error_types.Partial_success { result = post_id; warnings } ->
          Printf.printf "Posted as org: %s (warnings=%d)\n"
            post_id (List.length warnings)
      | Social_core.Error_types.Failure err ->
          Printf.eprintf "Org post failed: %s\n"
            (Social_core.Error_types.error_to_string err))
```

#### Account linking checklist

- Persist OAuth credentials returned by `exchange_code` or `exchange_code_and_get_organizations`.
- Persist discovered `organization_access_info` entries for account settings UI.
- Persist a default `author_urn` for posting (via `select_preferred_organization_access` or user choice).
- Re-sync organization access periodically or on reconnect with `get_organization_access`.
- If no eligible org/page remains, fall back to person posting by omitting `author_urn`.
- On 403 posting errors, guide re-consent with `include_organization_scopes:true` and verify product access.

#### Account linking recipe (recommended flow)

```ocaml
let link_linkedin_account ~account_id ~code =
  LinkedIn.exchange_code_and_get_preferred_organization
    ~code
    ~redirect_uri:"https://myapp.com/callback"
    ~acl_state:"APPROVED"
    (fun (credentials, preferred_opt) ->
      (* Persist credentials first *)
      Db.save_linkedin_credentials account_id credentials;

      (* Resolve default author URN *)
      match preferred_opt with
      | Some preferred ->
          Db.set_default_linkedin_author account_id preferred.organization_urn
      | None ->
          (* Keep default unset and allow person posting fallback *)
          Db.clear_default_linkedin_author account_id)
    (fun err ->
      (* Retry/backoff for 429, re-consent for 403 org scope failures *)
      Printf.eprintf "LinkedIn link flow failed: %s\n" err)
```

### Get User Profile

```ocaml
(* Fetch current user's profile *)
LinkedIn.get_profile ~account_id
  (function
    | Ok profile ->
        Printf.printf "User ID: %s\n" profile.sub;
        Printf.printf "Name: %s\n" (Option.value profile.name ~default:"N/A");
        Printf.printf "Email: %s\n" (Option.value profile.email ~default:"N/A")
    | Error err -> 
        Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### Fetch User Posts with Pagination

```ocaml
(* Get first page of posts *)
LinkedIn.get_posts ~account_id ~start:0 ~count:10
  (function
    | Ok collection ->
        List.iter (fun post ->
          Printf.printf "Post ID: %s\n" post.id;
          Option.iter (Printf.printf "Text: %s\n") post.text;
        ) collection.elements;
        
        (* Check pagination info *)
        (match collection.paging with
        | Some p -> 
            Printf.printf "Showing %d-%d of %s\n" 
              p.start 
              (p.start + p.count)
              (match p.total with Some t -> string_of_int t | None -> "unknown")
        | None -> ())
    | Error err -> 
        Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### Using Scrollers for Easy Pagination

```ocaml
(* Create a scroller to navigate through pages *)
let scroller = LinkedIn.create_posts_scroller ~account_id ~page_size:5 () in

(* Scroll to next page *)
scroller.scroll_next
  (function
    | Ok page ->
        List.iter (fun post ->
          Printf.printf "Post: %s\n" post.id;
        ) page.elements;
        
        Printf.printf "Current position: %d\n" (scroller.current_position ());
        Printf.printf "Has more: %b\n" (scroller.has_more ());
        
        (* Can continue scrolling *)
        if scroller.has_more () then
          scroller.scroll_next (function
            | Ok page -> (* handle next page *) ()
            | Error _ -> ())
        else
          Printf.printf "No more posts\n"
    | Error err -> 
        Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Scroll back *)
scroller.scroll_back
  (function
    | Ok page -> (* Handle previous page *) ()
    | Error err -> 
        Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### Get Specific Post

```ocaml
(* Fetch a single post by URN *)
LinkedIn.get_post ~account_id ~post_urn:"urn:li:share:123456"
  (function
    | Ok post ->
        Printf.printf "Post ID: %s\n" post.id;
        Printf.printf "Author: %s\n" post.author;
        Option.iter (Printf.printf "Text: %s\n") post.text
    | Error err -> 
        Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))
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
  (function
    | Ok posts ->
        Printf.printf "Retrieved %d posts\n" (List.length posts);
        List.iter (fun post ->
          Printf.printf "- %s: %s\n" 
            post.id 
            (Option.value post.text ~default:"(no text)");
        ) posts
    | Error err -> 
        Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### Search Posts (Author Finder)

```ocaml
(* Search posts by author (or omit author to use current authenticated member) *)
LinkedIn.search_posts ~account_id ~author:"urn:li:person:abc123" ~start:0 ~count:10
  (function
    | Ok collection ->
        Printf.printf "Found %d posts\n" (List.length collection.elements);
        List.iter (fun post ->
          Printf.printf "- %s\n" (Option.value post.text ~default:"(no text)");
        ) collection.elements
    | Error err -> 
        Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Use scroller for author-based search *)
let search_scroller = LinkedIn.create_search_scroller 
  ~account_id ~author:"urn:li:person:abc123" ~page_size:5 () in

search_scroller.scroll_next
  (function
    | Ok page -> (* Handle search results *) ()
    | Error err -> 
        Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Keyword search on ugcPosts is intentionally rejected *)
LinkedIn.search_posts ~account_id ~keywords:"ocaml"
  (function
    | Ok _ -> ()
    | Error err ->
        Printf.eprintf "Expected limitation: %s\n" (Social_core.Error_types.error_to_string err))
```

### Engagement APIs

```ocaml
(* Like a post *)
LinkedIn.like_post ~account_id ~post_urn:"urn:li:share:123"
  (function
    | Ok () -> Printf.printf "Liked!\n"
    | Error err -> 
        Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Unlike a post *)
LinkedIn.unlike_post ~account_id ~post_urn:"urn:li:share:123"
  (function
    | Ok () -> Printf.printf "Unliked!\n"
    | Error err -> 
        Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Comment on a post *)
LinkedIn.comment_on_post 
  ~account_id 
  ~post_urn:"urn:li:share:123"
  ~text:"Great insights!"
  (function
    | Ok comment_id -> Printf.printf "Comment posted: %s\n" comment_id
    | Error err -> 
        Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Get comments on a post *)
LinkedIn.get_post_comments ~account_id ~post_urn:"urn:li:share:123"
  (function
    | Ok collection ->
        List.iter (fun comment ->
          Printf.printf "%s: %s\n" comment.actor comment.text;
        ) collection.elements
    | Error err -> 
        Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Get engagement statistics *)
LinkedIn.get_post_engagement ~account_id ~post_urn:"urn:li:share:123"
  (function
    | Ok stats ->
        Option.iter (Printf.printf "Likes: %d\n") stats.like_count;
        Option.iter (Printf.printf "Comments: %d\n") stats.comment_count;
        Option.iter (Printf.printf "Shares: %d\n") stats.share_count
    | Error err -> 
        Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))
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
- `profile` - OpenID profile data via userinfo endpoint
- `r_member_social` - Read member's posts and social activity

### Engagement Features (Likes, Comments)
- `w_member_social` - Required for liking and commenting
- `r_member_social` - Required for reading engagement stats

### Advanced Features
For posting to company pages, you need:
- `w_organization_social` - Post as organization
- `r_organization_admin` - Discover org/page access via organization ACLs
- Community Management API access

Organization/page onboarding helpers are available:
- `get_oauth_url ~include_organization_scopes:true ...`
- `exchange_code_and_get_organizations` to exchange OAuth code and list organization access
- `get_organization_access` to refresh discoverable organizations for a linked account

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

# Optional - LinkedIn version header used for /rest organization ACL APIs (YYYYMM)
LINKEDIN_VERSION=202601
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
  scroll_next: (('a collection_response, Error_types.error) result -> unit) -> unit;
  scroll_back: (('a collection_response, Error_types.error) result -> unit) -> unit;
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

type organization_access_info = {
  organization_urn: string;
  organization_id: string option;
  role: string option;
  state: string option;
}
```

### Authentication Functions

#### `get_oauth_url`
Generate OAuth authorization URL.

```ocaml
val get_oauth_url : 
  ?include_organization_scopes:bool ->
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

#### `exchange_code_and_get_organizations`
Exchange authorization code and immediately fetch organization/page access entries.

```ocaml
val exchange_code_and_get_organizations :
  code:string ->
  redirect_uri:string ->
  ?role:string ->
  ?acl_state:string ->
  ((credentials * organization_access_info list) -> 'a) ->
  (string -> 'a) ->
  'a
```

#### `exchange_code_and_get_preferred_organization`
Exchange authorization code and return one preferred organization/page access candidate.

```ocaml
val exchange_code_and_get_preferred_organization :
  code:string ->
  redirect_uri:string ->
  ?role:string ->
  ?acl_state:string ->
  ((credentials * organization_access_info option) -> 'a) ->
  (string -> 'a) ->
  'a
```

#### `get_organization_access`
Fetch organization/page access entries for an already linked account.

```ocaml
val get_organization_access :
  account_id:string ->
  ?role:string ->
  ?acl_state:string ->
  ((organization_access_info list, Error_types.error) result -> unit) ->
  unit
```

#### `select_preferred_organization_access`
Select one preferred organization entry from a list using provider ranking (state, then role).

```ocaml
val select_preferred_organization_access :
  organization_access_info list ->
  organization_access_info option
```

#### `get_preferred_organization_access`
Fetch organization/page access entries and return one preferred candidate.

```ocaml
val get_preferred_organization_access :
  account_id:string ->
  ?role:string ->
  ?acl_state:string ->
  ((organization_access_info option, Error_types.error) result -> unit) ->
  unit
```

#### `post_single`
Post to LinkedIn with optional media. Uses structured error handling with the `outcome` type.

```ocaml
val post_single : 
  account_id:string -> 
  text:string -> 
  media_urls:string list ->
  ?author_urn:string ->
  ?alt_texts:string list ->
  (string Social_core.Error_types.outcome -> unit) ->  (* on_result *)
  unit
```

#### `post_thread`
Post multiple items (LinkedIn only supports single posts, so this posts just the first item).

```ocaml
val post_thread : 
  account_id:string -> 
  texts:string list -> 
  media_urls_per_post:string list list ->
  ?author_urn:string ->
  ?alt_texts_per_post:string list list ->
  (Social_core.Error_types.thread_result Social_core.Error_types.outcome -> unit) ->  (* on_result *)
  unit
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
  ((profile_info, Error_types.error) result -> unit) ->  (* on_result *)
  unit
```

Requires `openid` and `profile` scopes. Returns basic profile information including user ID, name, email, and profile picture.

### Post Management Functions

#### `get_post`
Fetch a single post by its URN.

```ocaml
val get_post :
  account_id:string ->
  post_urn:string ->          (* Required; must be trimmed and not contain ',', '(' or ')' *)
  ((post_info, Error_types.error) result -> unit) ->  (* on_result *)
  unit
```

#### `get_posts`
Fetch user's posts with pagination support.

```ocaml
val get_posts :
  account_id:string ->
  ?start:int ->              (* Starting index (default: 0, clamped to >= 0) *)
  ?count:int ->              (* Number to fetch (default: 10, normalized to 1..50) *)
  ((post_info collection_response, Error_types.error) result -> unit) ->  (* on_result *)
  unit
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
  ((post_info list, Error_types.error) result -> unit) ->  (* on_result *)
  unit
```

Batch operations are more efficient than multiple individual requests. Useful when you have specific post URNs to fetch.
Input notes:
- `post_urns` must be non-blank, trimmed, and must not contain `,`, `(`, or `)` (Rest.li list encoding guardrails)

### Pagination Helper

#### `create_posts_scroller`
Create a scroller for convenient page navigation.

```ocaml
val create_posts_scroller :
  account_id:string ->
  ?page_size:int ->           (* Posts per page (default: 10, minimum: 1) *)
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
  (function
    | Ok page -> (* handle page *) ()
    | Error err -> (* handle error *) ());

(* Check state *)
if scroller.has_more () then
  scroller.scroll_next handle_result;

(* Scroll backward *)
scroller.scroll_back handle_result;
```

### Search Functions

#### `search_posts`
Search for posts using author-based finder criteria.

```ocaml
val search_posts :
  account_id:string ->
  ?keywords:string ->        (* Currently unsupported for ugcPosts; returns error if set *)
  ?author:string ->          (* Filter by author URN; must be trimmed and Rest.li-safe *)
  ?start:int ->              (* Starting index (default: 0, clamped to >= 0) *)
  ?count:int ->              (* Results per page (default: 10, normalized to 1..50) *)
  ((post_info collection_response, Error_types.error) result -> unit) ->  (* on_result *)
  unit
```

Current behavior:
- Supports author filtering (`authors=List(...)` finder shape)
- Rejects keyword finder search because this is not supported by the LinkedIn `ugcPosts` finder contract

#### `create_search_scroller`
Create a scroller for search results.

```ocaml
val create_search_scroller :
  account_id:string ->
  ?keywords:string ->        (* Currently unsupported for ugcPosts; returns error if set *)
  ?author:string ->
  ?page_size:int ->          (* Results per page (default: 10, minimum: 1) *)
  unit ->
  post_info scroller
```

### Engagement Functions

#### `like_post`
Add a like/reaction to a post.

```ocaml
val like_post :
  account_id:string ->
  post_urn:string ->          (* Required; must be trimmed and not contain ',', '(' or ')' *)
  ((unit, Error_types.error) result -> unit) ->  (* on_result *)
  unit
```

#### `unlike_post`
Remove a like/reaction from a post.

```ocaml
val unlike_post :
  account_id:string ->
  post_urn:string ->          (* Required; must be trimmed and not contain ',', '(' or ')' *)
  ((unit, Error_types.error) result -> unit) ->  (* on_result *)
  unit
```

#### `comment_on_post`
Add a comment to a post.

```ocaml
val comment_on_post :
  account_id:string ->
  post_urn:string ->          (* Required; must be trimmed and not contain ',', '(' or ')' *)
  text:string ->              (* Required; blank/whitespace-only text is rejected *)
  ((string, Error_types.error) result -> unit) ->  (* on_result: Ok returns comment_id *)
  unit
```

#### `get_post_comments`
Fetch comments on a post with pagination.

```ocaml
val get_post_comments :
  account_id:string ->
  post_urn:string ->          (* Required; must be trimmed and not contain ',', '(' or ')' *)
  ?start:int ->              (* Starting index (default: 0, clamped to >= 0) *)
  ?count:int ->              (* Results per page (default: 10, normalized to 1..100) *)
  ((comment_info collection_response, Error_types.error) result -> unit) ->  (* on_result *)
  unit
```

#### `get_post_engagement`
Get engagement statistics for a post.

```ocaml
val get_post_engagement :
  account_id:string ->
  post_urn:string ->          (* Required; must be trimmed and not contain ',', '(' or ')' *)
  ((engagement_info, Error_types.error) result -> unit) ->  (* on_result *)
  unit
```

Returns metrics including:
- Like count
- Comment count
- Share count
- Impression count (if available)

**Note**: May require additional API permissions beyond basic posting scopes.

## Platform Constraints

### Text
- Maximum length: 3,000 characters
- Minimum length: 1 character
- Without media, the first URL becomes a link-preview article card. Pass `~link_preview:false` to `post_single` / `post_thread` to post the text alone (the URL stays clickable; the text is Little Text Format-escaped either way)

### Images
- Maximum file size: 8 MB
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

Posting operations use structured error handling with the `outcome` type:

```ocaml
LinkedIn.post_single ~account_id ~text ~media_urls:[]
  (function
    | Social_core.Error_types.Success post_id -> 
        Printf.printf "Posted: %s\n" post_id
    | Social_core.Error_types.Partial_success { result = post_id; warnings } ->
        Printf.printf "Posted: %s with %d warnings\n" post_id (List.length warnings)
    | Social_core.Error_types.Failure err ->
        Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

Non-posting operations use `api_result` (standard OCaml result type):

```ocaml
LinkedIn.get_profile ~account_id
  (function
    | Ok profile -> Printf.printf "User: %s\n" profile.sub
    | Error err -> Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

Common error messages:
- `"No refresh token available - please reconnect"` - User needs to re-authorize
- `"Token refresh failed"` - OAuth refresh failed (check credentials)
- `"LinkedIn API error (XXX)"` - API returned an error (see status code)
- `"Programmatic refresh not enabled"` - Standard app trying to use refresh (expected)

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

See `social-core` for interface definitions and `social-lwt` for Lwt adapters.

## License

MIT

## Related Packages

- `social-core` - Core interfaces and types
- `social-lwt` - Lwt runtime adapters
- `social-twitter-v2` - Twitter API v2
- `social-bluesky-v1` - Bluesky AT Protocol
- `social-mastodon-v1` - Mastodon API

## Support

For issues and questions:
- File an issue on GitHub
- Check LinkedIn's API documentation: https://docs.microsoft.com/en-us/linkedin/
- Review LinkedIn Developer Portal: https://www.linkedin.com/developers/
