# ocaml-social-sdk

[![CI](https://github.com/makerprism/ocaml-social-sdk/actions/workflows/ci.yml/badge.svg)](https://github.com/makerprism/ocaml-social-sdk/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![OCaml](https://img.shields.io/badge/OCaml-%3E%3D4.14-orange)](https://ocaml.org/)

OCaml SDK for social media APIs. Post content, manage media, handle threads across Twitter, LinkedIn, Bluesky, Mastodon, Facebook, Instagram, YouTube, Pinterest, Reddit, and TikTok. Runtime-agnostic design works with Lwt, Eio, or sync code.

> **Warning: Experimental Software**
>
> This SDK is **not production-ready**. It was primarily built using LLMs and is under active development. We are working towards making these libraries stable and usable.
>
> **What has been used successfully:**
> - OAuth 2.0 flows for Twitter, LinkedIn, Bluesky, and Mastodon
> - Posting (write) functionality for Twitter, LinkedIn, Bluesky, and Mastodon
>
> Other functionality (Facebook, Instagram, YouTube, Pinterest, TikTok, Threads, Reddit, and broader read/analytics flows) now has automated unit/contract coverage in this repository, but still has limited production validation. Use at your own risk and expect breaking changes.

## Packages

| Package | Description |
|---------|-------------|
| `social-core` | Core interfaces and types (runtime-agnostic) |
| `social-lwt` | Lwt runtime adapter with Cohttp |
| `social-refresh` | Shared token refresh orchestration types |
| `social-twitter-v2` | Twitter API v2 |
| `social-bluesky-v1` | Bluesky/AT Protocol |
| `social-linkedin-v2` | LinkedIn API v2 |
| `social-mastodon-v1` | Mastodon API |
| `social-facebook-graph-v21` | Facebook Graph API v21 |
| `social-instagram-graph-v21` | Instagram Graph API v21 |
| `social-youtube-data-v3` | YouTube Data API v3 |
| `social-pinterest-v5` | Pinterest API v5 |
| `social-reddit-v1` | Reddit API v1 |
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

### Posting to Reddit

```ocaml
open Social_reddit_v1

module Reddit = Reddit_v1.Make(Your_config)

let () = Reddit.submit_self_post
  ~account_id:"account123"
  ~subreddit:"your_subreddit"
  ~title:"Hello from OCaml!"
  ~body:"Posted using ocaml-social-sdk"
  ()
  (fun outcome ->
    match outcome with
    | Error_types.Success post_id -> 
        Printf.printf "Posted: %s\n" post_id
    | _ -> ())
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
3. **Shared Orchestration** (`social-refresh`): Optional token-refresh decision/orchestration helpers
4. **Platform SDKs** (`social-*`): Platform-specific API implementations

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
| Facebook | ✅ | ✅ | ⚠️ | - | ⚠️ | ⚠️ | ⚠️ | ⚠️ |
| Instagram | ✅ | ✅ | ⚠️ | - | ⚠️ | ⚠️ | ⚠️ | ⚠️ |
| Threads | ✅ | ✅ | ⚠️ | ⚠️ | - | - | ⚠️ | ⚠️ |
| YouTube | ⚠️ | ⚠️ | ⚠️ | - | - | ⚠️ | ⚠️ | ⚠️ |
| Pinterest | ⚠️ | ⚠️ | ⚠️ | - | - | - | ⚠️ | ⚠️ |
| Reddit | ⚠️ | ⚠️ | ⚠️ | - | - | - | ⚠️ | - |
| TikTok | ⚠️ | ⚠️ | ⚠️ | - | - | ⚠️ | ⚠️ | ⚠️ |

✅ = Used successfully in production workflows, ⚠️ = Implemented with automated tests but limited production validation, ❌ = Not implemented (API available), - = Not applicable

For endpoint-level request/response contract coverage, see `docs/parity-http-matrix.md`.

## Development

Build the project:

```bash
dune build
```

If the build fails due to missing dependencies, regenerate the lockfile first:

```bash
dune pkg lock
dune build
```

## License

MIT

---

These libraries emerged from building [FeedMansion](https://feedmansion.com), a social media management tool. We're sharing them so others can build cool stuff too. Maintained by [Makerprism](https://makerprism.com).
