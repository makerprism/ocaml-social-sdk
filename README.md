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
- **Media Upload**: Support for images, videos, and documents
- **Thread Posting**: Post threads/reply chains on supported platforms (Twitter, Bluesky, Mastodon)

## Supported Platforms

| Platform | OAuth | Post | Media | Threads | Status |
|----------|-------|------|-------|---------|--------|
| Twitter v2 | ✅ | ✅ | ✅ | ✅ | Used |
| Bluesky | ✅ | ✅ | ✅ | ✅ | Used |
| LinkedIn | ✅ | ✅ | ✅ | - | Used |
| Mastodon | ✅ | ✅ | ✅ | ✅ | Used |
| Twitter v1 | ⚠️ | ⚠️ | ⚠️ | ⚠️ | Untested |
| Facebook | ⚠️ | ⚠️ | ⚠️ | - | Untested |
| Instagram | ⚠️ | ⚠️ | ⚠️ | - | Untested |
| YouTube | ⚠️ | ⚠️ | ⚠️ | - | Untested |
| Pinterest | ⚠️ | ⚠️ | ⚠️ | - | Untested |
| TikTok | ⚠️ | ⚠️ | ⚠️ | - | Untested |

✅ = Used successfully, ⚠️ = Implemented but untested

## License

MIT
