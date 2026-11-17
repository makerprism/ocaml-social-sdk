# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.1] - Unreleased

### Changed

- **Breaking (Config implementers):** `CONFIG` now requires
  `sleep : seconds:float -> (unit -> unit) -> (string -> unit) -> unit`
  (`Social_core.SLEEP`). `with_retry` backoff and the media-upload status poll
  used to block the whole process with `Unix.select`; they now wait through
  `Config.sleep`. If the timer cannot be scheduled, `with_retry` returns the
  last transient response instead of retrying immediately. The package no
  longer depends on `unix`. Lwt users: `let sleep = Social_lwt.Lwt_adapter.sleep`.

- 403 responses now carry Pinterest's verbatim `message` text via the
  new `platform_message` field on `Insufficient_permissions`. See
  `social-core/CHANGES.md` for the breaking variant change.

### Added

- **`OAuth.Make(Http).get_user_account ~access_token ?ad_account_id`** and **`Make.get_user_account ~account_id`** (refreshes the stored token first): typed `GET /v5/user_account` as `OAuth.user_account` (`id`, `username`, `account_type = Pinner | Business | Unknown_account_type _`, `business_name`, `about`, `profile_image`, `website_url`, `board_count`, `pin_count`, `follower_count`, `following_count`, `monthly_views`), per the `Account` schema of Pinterest's OpenAPI description. Failures are typed (401 `Token_invalid`, 403 `Insufficient_permissions ["user_accounts:read"]`, 404, 429, else `Api_error` with the body). `OAuth.get_user_info` (raw JSON, every failure `Internal_error`) is unchanged.

- OAuth 2.0 authentication flow
- Automatic token refresh with 30-day token validity
- Pin creation with title, description, and media
- Board management:
  - Create boards (public/private)
  - Get board by name or ID
  - List all boards with pagination
- Search API for pins, boards, and users
- User profile management
- Bulk pin creation for efficiency
- Rate limiting with exponential backoff
- Structured error types for better error handling
- Debug logging support
- Request retry logic with automatic retries
- Optional response caching
- Content validation (500 char description, 100 char title)
- Structured error handling:
  - Posting operations use `outcome` type with Success/Partial_success/Failure
  - Non-posting operations use `api_result` type with Ok/Error
- Added account analytics API:
  - `get_account_analytics` for `/v5/user_account/analytics`
- Added pin analytics API:
  - `get_pin_analytics` for `/v5/pins/{pin_id}/analytics`
- Added typed analytics response records and canonical analytics conversion helpers
- Added request-contract and parsing tests for account and pin analytics
