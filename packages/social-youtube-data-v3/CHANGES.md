# Changelog

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
