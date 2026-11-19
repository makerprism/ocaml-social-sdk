# Unreleased
- `OAuth.get_authorization_url` and the account-backed `Make.get_oauth_url` pass the new `?access_type` / `?prompt` through to `social-google-oauth`; defaults are unchanged.

- `CONFIG` inherits the social-core change: `encrypt` / `decrypt` are no longer required members (no SDK operation called them). Configurations that still define them keep compiling.


- Add `ensure_valid_token_with_outcome ~with_refresh_lock` reporting reused or renewed-and-persisted tokens through an explicit completion boundary. Existing APIs remain compatible; credential-only functors never write health.

# Changelog

- Add `Make_credentials_only` for coordinated credential operations without account-health callbacks. Legacy functors retain their behavior.

- Add `COORDINATED_CONFIG` and `Make_coordinated` for account refresh locks held through persistence, with credential reload under the lock. Existing functors remain compatible.

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.1] - Unreleased

### Changed

- 403 responses now carry the YouTube Data API's verbatim
  `error.message` text via the new `platform_message` field on
  `Insufficient_permissions`. See `social-core/CHANGES.md` for the
  breaking variant change.

### Added

- **`Make(Config).upload_video ~account_id ~media_urls ~metadata ?validate_media_before_upload`**: `videos.insert` with explicit metadata. The `video_metadata` record carries `title`, `description`, `tags`, `category_id`, `privacy_status` (a new `privacy_status` ADT: `Public` / `Unlisted` / `Private`), `made_for_kids`, `publish_at`, `notify_subscribers` and `recording_location`; `video_metadata_json` serializes it and `validate_video_metadata` checks the endpoint-encoded limits (title required and ≤ 100 characters, description ≤ 5000 bytes, media required — the units YouTube itself documents). Nothing is derived, defaulted or mutated: no ` #Shorts` suffix, no byte-truncated title, no `shorts`/`short` tags, no default visibility or category. An empty `tags` list or `category_id` omits those fields. `publish_at` still forces `private` because the API rejects a scheduled public video. `post_single` and `post_thread` (which delegates to it) are deprecated: they keep deriving the Shorts payload from `text` unchanged, and the derivation is now pinned by a test.

- **`OAuth.Make(Http).get_my_channels ~access_token`**: typed `channels.list?part=snippet,statistics&mine=true` (1 quota unit) as `OAuth.channel list` (`id`, `title`, `description`, `custom_url`, `thumbnail_url`, `view_count`, `subscriber_count`, `hidden_subscriber_count`, `video_count`). Counts are read from JSON strings or numbers; `subscriber_count` is `None` when the channel hides it. Errors through `Social_google_oauth.api_error` (403 `quotaExceeded` is `Rate_limited`). `get_channel_info` (raw JSON) is unchanged.
- `post_single` takes an optional `?recording_location` (a
  `recording_location` record with `latitude` and `longitude`). When given,
  the upload sends `recordingDetails.location` and adds `recordingDetails`
  to `part`. The deprecated `locationDescription` is never sent.

- Google OAuth 2.0 authentication with PKCE support
- YouTube Shorts upload via resumable upload API
- Two-step upload process (initialize metadata, upload video)
- Access token management (1-hour tokens with automatic refresh)
- Refresh token support (long-lived, no expiration)
- Content validation for video descriptions (5,000 char limit)
- Automatic #Shorts tagging
- CPS-based architecture for runtime independence
- Structured error handling:
  - Posting operations use `outcome` type with Success/Partial_success/Failure
  - Non-posting operations use `api_result` type with Ok/Error
- Added account analytics API `get_account_analytics` using channel statistics.
- Added post analytics API `get_post_analytics` using video statistics.
- Added optional YouTube Analytics Reports API time-series helpers:
  - account report queries via `get_account_analytics_report` (`ids=channel==MINE`, `dimensions=day`)
  - post report queries via `get_post_analytics_report` (adds `filters=video=={id}`)
  - typed report response records and parser helpers for column headers/rows
- Added thumbnail upload support:
  - request-contract builder `build_thumbnail_upload_request`
  - upload helpers for direct bytes and URL-fetched images
- Added canonical analytics conversion helpers for social-core metric normalization.
- Added analytics reports request-contract/parser/error-path tests.
- Added analytics and thumbnail request-contract tests.
