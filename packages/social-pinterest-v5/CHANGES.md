# Unreleased

- Add `ensure_valid_token_with_outcome ~with_refresh_lock` reporting reused or renewed-and-persisted tokens through an explicit completion boundary. Existing APIs remain compatible; credential-only functors never write health.

# Changelog

- Add `Make_credentials_only` for coordinated credential operations without account-health callbacks. Legacy functors retain their behavior.

- Add `COORDINATED_CONFIG` and `Make_coordinated` for account refresh locks held through persistence, with credential reload under the lock. Existing functors remain compatible.

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

- **Typed analytics reports (`Analytics`, module `Pinterest_v5_analytics`)**: `OAuth.Make(Http).get_user_account_analytics ~access_token ~start_date ~end_date ?metric_types ?ad_account_id` (`GET /v5/user_account/analytics`) and `OAuth.Make(Http).get_pin_analytics ~access_token ~pin_id ~start_date ~end_date ?metric_types ?ad_account_id` (`GET /v5/pins/{pin_id}/analytics`, `Analytics.pin_metric_types` by default since the API requires the parameter), plus `Make.get_account_analytics_report` / `Make.get_pin_analytics_report` for a stored account. They return `Analytics.report`: groups by split value, each with `daily_metrics` (`date`, `data_status`, metric map), `summary_metrics` and, for pins, `lifetime_metrics`. Helpers `Analytics.unsplit` (the `all` group, or the only one), `summary`, `summary_count`. Errors go through the same mapping as `get_user_account`. Source: the v5 OpenAPI description (https://github.com/pinterest/api-description/blob/main/v5/openapi.yaml, `AnalyticsResponse`, `PinAnalyticsResponse`). **Verify live**: the spec types the response as a map keyed by group without naming the unsplit key; `Analytics.unsplit` accepts `all` or a single group of any name.

### Fixed

- `get_account_analytics` / `get_pin_analytics` read the totals from `<group>.summary_metrics` (`all`, or the only group), where the documented response keeps them. Before, they searched the top level and the group object itself, so every total of a documented response came back `None`. The earlier fixtures used shapes the API does not document; the old lookups still apply as fallbacks.

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
