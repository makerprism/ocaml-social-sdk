# ocaml-social-sdk

[![CI](https://github.com/makerprism/ocaml-social-sdk/actions/workflows/ci.yml/badge.svg)](https://github.com/makerprism/ocaml-social-sdk/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![OCaml](https://img.shields.io/badge/OCaml-%3E%3D4.14-orange)](https://ocaml.org/)

OCaml SDK for social media APIs. Post content, manage media, handle threads across Twitter, LinkedIn, Bluesky, Mastodon, Facebook, Instagram, YouTube, Pinterest, TikTok. Runtime-agnostic design works with Lwt, Eio, or sync code.

> **Warning: Experimental Software**
>
> This SDK is **not production-ready**. It was primarily built using LLMs and is under active development. We are working towards making these libraries stable and usable.
>
> **What has been used successfully:**
> - OAuth 2.0 flows for Twitter, LinkedIn, Bluesky, and Mastodon
> - Posting (write) functionality for Twitter, LinkedIn, Bluesky, and Mastodon
>
> All other functionality (Facebook, Instagram, YouTube, Pinterest, TikTok, and read operations) should be considered untested. Use at your own risk and expect breaking changes.

## Packages

| Package | Description |
|---------|-------------|
| `social-core` | Core interfaces and types (runtime-agnostic) |
| `social-lwt` | Lwt runtime adapter with Cohttp |
| `social-twitter-v1` | Twitter API v1.1 (OAuth 1.0a) |
| `social-twitter-v2` | Twitter API v2 |
| `social-bluesky-v1` | Bluesky/AT Protocol |
| `social-linkedin-v2` | LinkedIn API v2 |
| `social-mastodon-v1` | Mastodon API |
| `social-facebook-graph-v21` | Facebook Graph API v21 |
| `social-instagram-graph-v21` | Instagram Graph API v21 |
| `social-youtube-data-v3` | YouTube Data API v3 |
| `social-pinterest-v5` | Pinterest API v5 |
| `social-tiktok-v1` | TikTok Content Posting API |

## Installation

### Using Dune Package Management (recommended)

Add to your `dune-project`:

```scheme
(pin
 (url "git+https://github.com/makerprism/ocaml-social-sdk")
 (package (name social-core)))

(pin
 (url "git+https://github.com/makerprism/ocaml-social-sdk")
 (package (name social-lwt)))

(pin
 (url "git+https://github.com/makerprism/ocaml-social-sdk")
 (package (name social-twitter-v2)))
```

Then run:
```bash
dune pkg lock
dune build
```

## Usage

### Posting to Twitter

```ocaml
open Social_twitter_v2

let client = Twitter_v2.create
  ~bearer_token:"your_bearer_token"
  ~api_key:"your_api_key"
  ~api_secret:"your_api_secret"
  ~access_token:"your_access_token"
  ~access_token_secret:"your_access_token_secret"
  ()

let post = Twitter_v2.create_post client
  ~text:"Hello from OCaml!"
  ()
```

### Posting to LinkedIn

```ocaml
open Social_linkedin_v2

let client = Linkedin_v2.create
  ~access_token:"your_access_token"
  ~person_id:"your_person_urn"
  ()

let post = Linkedin_v2.create_post client
  ~text:"Excited to share this update!"
  ()
```

### Posting to Bluesky

```ocaml
open Social_bluesky_v1

let client = Bluesky_v1.create
  ~handle:"your.handle.bsky.social"
  ~app_password:"your_app_password"
  ()

let post = Bluesky_v1.create_post client
  ~text:"Hello Bluesky from OCaml!"
  ()
```

### With Lwt Runtime

```ocaml
open Social_provider_lwt

let%lwt result = Lwt_adapter.post client ~text:"Hello!" ()
```

## Architecture

The SDK follows a runtime-agnostic design:

1. **Core** (`social-core`): Pure OCaml types, interfaces, and utilities
2. **Runtime Adapters** (`social-lwt`): HTTP client implementations
3. **Platform SDKs** (`social-*`): Platform-specific API implementations

### Features

- **Content Validation**: Platform-specific validation (character limits, media types)
- **URL Extraction**: Parse and handle URLs in content
- **Media Upload**: Support for images, videos, and GIFs
- **Thread Posting**: Post threads/reply chains on supported platforms (Twitter, Bluesky, Mastodon)

## Supported Platforms

| Platform | OAuth | Post | Media | Threads | Stories | Shorts/Reels | Read | Analytics |
|----------|-------|------|-------|---------|---------|--------------|------|-----------|
| Twitter v2 | ✅ | ✅ | ✅ | ✅ | - | - | ⚠️ | ⚠️ |
| Bluesky | ✅ | ✅ | ✅ | ✅ | - | - | ⚠️ | ⚠️ |
| LinkedIn | ✅ | ✅ | ✅ | - | - | - | ⚠️ | ⚠️ |
| Mastodon | ✅ | ✅ | ✅ | ✅ | - | - | ⚠️ | ⚠️ |
| Twitter v1 | ⚠️ | ⚠️ | ⚠️ | ⚠️ | - | - | ⚠️ | ⚠️ |
| Facebook | ⚠️ | ⚠️ | ⚠️ | - | ⚠️ | ⚠️ | ⚠️ | ⚠️ |
| Instagram | ⚠️ | ⚠️ | ⚠️ | - | ⚠️ | ⚠️ | ⚠️ | ⚠️ |
| YouTube | ⚠️ | - | - | - | - | ⚠️ | ⚠️ | ⚠️ |
| Pinterest | ⚠️ | ⚠️ | ⚠️ | - | - | - | ⚠️ | ⚠️ |
| TikTok | ⚠️ | - | - | - | - | ⚠️ | ⚠️ | ⚠️ |

✅ = Used successfully, ⚠️ = Implemented but untested, ❌ = Not implemented (API available), - = Not applicable

## OAuth & Required Scopes

Each platform SDK includes an `OAuth` module with scope definitions and metadata. **Always request the correct scopes during authorization** - API calls will fail with 403 errors if you don't have the required permissions.

### Twitter/X (`social-twitter-v2`)

**OAuth Details:**
- PKCE: Required (S256 method)
- Token lifetime: 2 hours
- Refresh tokens: Yes (include `offline.access` scope)

**Scopes:**

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
| Block/mute | `users.read`, `block.read`, `block.write`, `mute.read`, `mute.write` |
| Bookmarks | `tweet.read`, `users.read`, `bookmark.read`, `bookmark.write` |
| Token refresh | `offline.access` (must be included in initial request) |

**Recommended for posting apps:** `tweet.read tweet.write users.read offline.access`

```ocaml
(* Use the SDK's scope helpers *)
let scopes = Twitter_v2.OAuth.Scopes.write  (* Includes offline.access *)
let url = Twitter_v2.OAuth.get_authorization_url
  ~client_id ~redirect_uri ~state ~code_challenge ~scopes ()
```

### LinkedIn (`social-linkedin-v2`)

**OAuth Details:**
- PKCE: Not supported
- Token lifetime: 60 days
- Refresh tokens: Partner Program only (most apps cannot refresh)

**Scopes:**

| Operation | Required Scopes |
|-----------|-----------------|
| Read profile | `openid`, `profile`, `email` |
| Post to personal profile | `openid`, `profile`, `email`, `w_member_social` |
| Post with media | `openid`, `profile`, `email`, `w_member_social` |

**For Company Pages** (requires separate app with Community Management API):

| Operation | Required Scopes |
|-----------|-----------------|
| Manage pages | `r_organization_admin`, `w_organization_social` |
| Post to page | `r_organization_admin`, `w_organization_social` |

**Recommended for posting apps:** `openid profile email w_member_social`

```ocaml
let scopes = Linkedin_v2.OAuth.Scopes.write
let url = Linkedin_v2.OAuth.get_authorization_url
  ~client_id ~redirect_uri ~state ~scopes ()
```

### Bluesky (`social-bluesky-v1`)

**Auth Details:**
- OAuth: Not used (uses app passwords)
- Token lifetime: App passwords don't expire
- Authentication: AT Protocol `createSession` endpoint

Bluesky doesn't use OAuth scopes. Instead, users create app passwords at https://bsky.app/settings/app-passwords. Full access is granted to the app password.

```ocaml
let session = Bluesky_v1.Auth.create_session
  ~identifier:"user.bsky.social"
  ~app_password:"xxxx-xxxx-xxxx-xxxx"
  on_success on_error
```

### Mastodon (`social-mastodon-v1`)

**OAuth Details:**
- PKCE: Supported (optional)
- Token lifetime: Never expires
- Refresh tokens: Not needed (tokens don't expire)
- App registration: Required per-instance

**Scopes:**

| Operation | Required Scopes |
|-----------|-----------------|
| Read profile/posts | `read` |
| Post status | `read`, `write` |
| Post with media | `read`, `write` |
| Follow accounts | `read`, `write`, `follow` |
| Push notifications | `push` |

**Recommended for posting apps:** `read write follow`

```ocaml
(* Mastodon requires per-instance app registration first *)
let scopes = Mastodon_v1.OAuth.Scopes.write
let url = Mastodon_v1.OAuth.get_authorization_url
  ~instance_url:"https://mastodon.social"
  ~client_id ~redirect_uri ~scopes ()
```

### Facebook Pages (`social-facebook-graph-v21`)

**OAuth Details:**
- PKCE: Not supported
- Token lifetime: Short-lived (1 hour), exchange for long-lived (60 days)
- Refresh tokens: Use `fb_exchange_token` grant type

**Scopes:**

| Operation | Required Scopes |
|-----------|-----------------|
| Read page info | `pages_read_engagement` |
| Post to page | `pages_read_engagement`, `pages_manage_posts` |
| Post with media | `pages_read_engagement`, `pages_manage_posts` |
| Read page content | `pages_read_user_content` |
| Manage page settings | `pages_manage_metadata` |

**Recommended for posting apps:** `pages_read_engagement pages_manage_posts`

```ocaml
let scopes = Facebook_graph_v21.OAuth.Scopes.write
let url = Facebook_graph_v21.OAuth.get_authorization_url
  ~client_id ~redirect_uri ~state ~scopes ()

(* After getting short-lived token, exchange for long-lived *)
Facebook_graph_v21.OAuth.Make(Http).exchange_for_long_lived_token
  ~client_id ~client_secret ~short_lived_token on_success on_error
```

### Instagram (`social-instagram-graph-v21`)

**OAuth Details:**
- PKCE: Not supported  
- Token lifetime: 60 days (use `ig_exchange_token` for long-lived)
- Requires: Instagram Business/Creator account linked to Facebook Page

**Scopes:**

| Operation | Required Scopes |
|-----------|-----------------|
| Read profile | `instagram_basic` |
| Post content | `instagram_basic`, `instagram_content_publish` |
| Read insights | `instagram_manage_insights` |
| Manage comments | `instagram_manage_comments` |

**Recommended for posting apps:** `instagram_basic instagram_content_publish`

### TikTok (`social-tiktok-v1`)

**OAuth Details:**
- PKCE: Not supported
- Token lifetime: 24 hours access, 365 days refresh
- Note: Uses `client_key` instead of `client_id`

**Scopes:**

| Operation | Required Scopes |
|-----------|-----------------|
| Read profile | `user.info.basic` |
| Post video | `user.info.basic`, `video.publish` |
| List videos | `video.list` |

**Recommended for posting apps:** `user.info.basic video.publish`

```ocaml
(* Note: TikTok uses client_key, not client_id *)
let url = Tiktok_v1.OAuth.get_authorization_url
  ~client_key ~redirect_uri ~scope:"user.info.basic,video.publish" ~state
```

### YouTube (`social-youtube-data-v3`)

**OAuth Details:**
- PKCE: Supported (S256 method)
- Token lifetime: 1 hour
- Refresh tokens: Yes (never expire)

**Scopes:**

| Operation | Required Scopes |
|-----------|-----------------|
| Read channel | `https://www.googleapis.com/auth/youtube.readonly` |
| Upload video | `https://www.googleapis.com/auth/youtube.upload` |
| Full access | `https://www.googleapis.com/auth/youtube` |

**Recommended for posting apps:** `https://www.googleapis.com/auth/youtube.upload`

### Pinterest (`social-pinterest-v5`)

**OAuth Details:**
- PKCE: Not supported
- Token lifetime: 30 days access, 365 days refresh
- Auth method: Basic Auth for token exchange

**Scopes:**

| Operation | Required Scopes |
|-----------|-----------------|
| Read boards | `boards:read` |
| Create boards | `boards:read`, `boards:write` |
| Read pins | `pins:read` |
| Create pins | `pins:read`, `pins:write` |
| Read account | `user_accounts:read` |

**Recommended for posting apps:** `boards:read boards:write pins:read pins:write user_accounts:read`

### Using OAuth.Scopes Helpers

Each SDK provides helper functions to get the right scopes:

```ocaml
(* Get scopes for specific operations *)
let scopes = Twitter_v2.OAuth.Scopes.for_operations [Post_text; Post_media]
(* Returns: ["users.read"; "offline.access"; "tweet.read"; "tweet.write"] *)

(* Or use predefined sets *)
let read_scopes = Twitter_v2.OAuth.Scopes.read   (* Read-only operations *)
let write_scopes = Twitter_v2.OAuth.Scopes.write (* Includes posting *)
let all_scopes = Twitter_v2.OAuth.Scopes.all     (* Everything available *)
```

### OAuth Metadata

Each SDK also provides metadata about the platform's OAuth implementation:

```ocaml
let () =
  let open Twitter_v2.OAuth.Metadata in
  Printf.printf "PKCE supported: %b\n" supports_pkce;
  Printf.printf "Refresh supported: %b\n" supports_refresh;
  Printf.printf "Token lifetime: %s\n" 
    (match token_lifetime_seconds with 
     | Some s -> Printf.sprintf "%d seconds" s 
     | None -> "never expires")
```

## License

MIT
