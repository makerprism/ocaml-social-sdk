# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

### Fixed

- **Only a genuinely dead refresh JWT gets bucketed as a revoked session.**
  `ensure_valid_token` treated *any* HTTP 400 from
  `com.atproto.server.refreshSession` as proof the session was gone. atproto
  also answers 400 for `InvalidRequest`, which means the SDK sent a malformed
  call, and a reverse proxy or WAF in front of a struggling PDS can emit a 400
  of its own with no XRPC envelope at all. Both wrote `token_revoked` and gave
  the user a "reconnect this account" badge for something they could not fix.
  The branch now reads the atproto error *name* out of the response body and
  fires only for `ExpiredToken` and `InvalidToken`. Everything else falls
  through to the same `Health_status.of_error` path as any other refresh
  failure, so it leaves the stored status alone.
- **A network error during session refresh is no longer recorded as a
  credential failure.** `ensure_valid_token` funnelled every non-auth refresh
  failure through an `on_other_refresh_failure` helper that wrote the health
  status `refresh_failed` and rewrapped the error as
  `Auth_error (Refresh_failed _)`. A PDS that was briefly unreachable or
  answering 5xx therefore looked exactly like a revoked session: the account
  got a credential-failure status, and consumers routing on
  `Error_types.classify_error` saw `Auth_failure`. Refresh failures are now
  classified with `Health_status.of_error`. Confirmed auth failures still write
  `token_revoked`; everything else leaves the stored status alone and surfaces
  the underlying error unchanged, so a connectivity blip stays transient.

### Added

- `atproto_error_name`, which extracts the machine-readable `error` field from
  an XRPC error body. `parse_api_error` keeps only the human-readable
  `message`, so callers that need to tell one 4xx from another read the name
  with this.

### Changed

- **Security: stop persisting the Bluesky app password long-term.**
  `Make(Config).ensure_valid_token` now exchanges legacy app-password rows
  (`auth_type = App_password`) for a JWT session on first use via
  `Auth.create_session`, then rotates storage to `auth_type = Bearer` with
  `refresh_token = refresh_jwt`. Subsequent refreshes go through
  `Auth.refresh_session`, which rotates the `refresh_jwt` itself. The app
  password is forgotten after the one-time exchange. Previously the app
  password was retained indefinitely and would remain usable by anyone with
  DB access until the user manually revoked it in Bluesky settings.
  `access_token` continues to hold the user's identifier (handle/DID), so
  callers that use `creds.access_token` as the repo field are unaffected.

## [0.0.1] - Unreleased

### Added

- App password authentication with session management
- Post creation with URI and CID extraction
- Delete posts
- Thread support with proper reply chains
- Quote posts with optional media
- Media upload (blobs) supporting up to 4 images
- Video upload validation (50MB, 60s max)
- Rich text facets:
  - URL detection and linking
  - Mention detection with DID resolution
  - Hashtag detection
- Link card embeds for external URLs
- Social interactions: like/unlike, repost/unrepost, follow/unfollow
- Read operations: get post thread, user profile, timeline, author feed
- Get likes, reposts, followers, and follows lists
- Notifications: list, count unread, mark as seen
- Search for users and posts
- Moderation: mute/unmute, block/unblock actors
- Content validation (300 chars, media size/type limits)
- Health status tracking
- Structured error handling:
  - Posting operations use `outcome` type with Success/Partial_success/Failure
  - Non-posting operations use `api_result` type with Ok/Error
  - Partial success for posts where enrichment (link cards) fails
