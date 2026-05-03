# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.1] - Unreleased

### Changed

- 403 responses with scope/permission body markers now carry TikTok's
  verbatim error text via the new `platform_message` field on
  `Insufficient_permissions`. See `social-core/CHANGES.md` for the
  breaking variant change.

### Added

- OAuth 2.0 authentication with PKCE support
- Video posting via FILE_UPLOAD method
- Video posting via PULL_FROM_URL method
- Photo carousel posting (multiple images)
- Creator info query (posting capabilities and limits)
- Publish status tracking
- Chunked upload support for large videos (>5MB)
- Privacy level options (public, friends, private)
- Video constraints validation:
  - Duration: 3-600 seconds
  - Resolution: 360-4096px
  - Frame rate: 23-60 FPS
  - Formats: MP4, WebM, MOV
- CPS-based architecture for runtime independence
- Structured error handling:
  - Posting operations use `outcome` type with Success/Partial_success/Failure
  - Non-posting operations use `api_result` type with Ok/Error
- Added account analytics flow:
  - `GET /v2/user/info`
  - `POST /v2/video/list`
  - `POST /v2/video/query`
- Added post analytics API `get_post_analytics` via `POST /v2/video/query`
- Added typed analytics records and canonical analytics conversion helpers
- Added request-contract and parsing tests for analytics endpoints
