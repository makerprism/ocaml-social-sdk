# Unreleased

- `CONFIG` inherits the social-core change: `encrypt` / `decrypt` are no longer required members (no SDK operation called them). Configurations that still define them keep compiling.

# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.1] - Unreleased

### Added

- **`Make_nonblocking (Config : NONBLOCKING_CONFIG)`**: the provider with
  container polling waiting through `Social_core.SLEEP`
  (`sleep : seconds:float -> (unit -> unit) -> (string -> unit) -> unit`),
  which a cooperative runtime implements without blocking. With Lwt:
  `let sleep = Social_lwt.Lwt_adapter.sleep`. A timer the runtime cannot
  schedule fails the post. `BASE_CONFIG` holds everything else `CONFIG`
  required.

### Deprecated

- **This package is deprecated.** Meta makes Graph API v21.0 unavailable on 2027-01-21. There is no v25 package for Instagram via Facebook Login yet; for Instagram Business Login use `social-instagram-standalone-v25`. The package stays and still builds; it gets fixes only.
- **`CONFIG.sleep : float -> (unit -> 'a) -> 'a` and `Make`.** The
  polymorphic return type can only be implemented by blocking the calling
  thread, which freezes an Lwt or Eio program for the whole backoff (up to
  13 s per poll). `Make` keeps working unchanged and now delegates to
  `Make_nonblocking`; migrate by replacing `sleep` and the functor name.

### Changed

- Permission-denied responses (Meta error code 10/200) now carry
  Instagram's verbatim `error.message` text via the new `platform_message`
  field on `Insufficient_permissions`. See `social-core/CHANGES.md` for
  the breaking variant change.

### Added

#### Authentication
- OAuth 2.0 via Facebook integration
- Long-lived token exchange (60 days)
- Automatic token refresh with 7-day buffer
- Token expiry tracking

#### Posting
- Single image posts
- Single video posts (3-60 seconds)
- Carousel posts (2-10 images/videos)
- Reels support (3-90 seconds)
- Caption validation (2,200 chars, 30 hashtags)

#### Media Support
- Automatic media type detection from URL
- Image formats: .jpg, .jpeg, .png
- Video formats: .mp4, .mov (up to 100MB)
- Mixed media carousels

#### Container Publishing
- Two-step publishing (create container, publish)
- Smart polling with exponential backoff
- Container status checking

#### Error Handling
- Instagram API error code parsing
- User-friendly error messages
- Actionable guidance for common errors
- Structured error handling:
  - Posting operations use `outcome` type with Success/Partial_success/Failure
  - Non-posting operations use `api_result` type with Ok/Error

#### Analytics
- Added account audience insights API:
  - `get_account_audience_insights` for `follower_count` and `reach`
- Added account engagement insights API:
  - `get_account_engagement_insights` with `metric_type=total_value`
- Added media/post insights API:
  - `get_media_insights` for views/reach/saves/likes/comments/shares
- Added account-scoped convenience wrappers and canonical analytics conversion helpers
- Added request-contract and parser tests for all insights flows
- Added redaction guard for network error text containing token-bearing query fragments

#### Architecture
- CPS (Continuation Passing Style) implementation
- Runtime agnostic
- HTTP client agnostic
- Integrated with social-core
