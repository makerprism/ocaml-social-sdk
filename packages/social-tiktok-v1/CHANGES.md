# Changelog

- Add `Make_credentials_only` for coordinated credential operations without account-health callbacks. Legacy functors retain their behavior.

- Add `COORDINATED_CONFIG` and `Make_coordinated` for account refresh locks held through persistence, with credential reload under the lock. Existing functors remain compatible.

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.1] - Unreleased

### Error diagnostics

- Token endpoint HTTP failures preserve their status and raw body as `Api_error`, including non-JSON bodies and JSON without an OAuth error key. A failure status cannot be accepted as a successful token response.

### Migration notes

- #117: Video-init size/chunk diagnostics now live in `Api_error.message`; `raw_response` contains the untouched provider JSON. Consumers should parse `raw_response` and display `message`.

### Fixed

- OAuth metadata now advertises PKCE support. Desktop login uses hex S256 challenges; the existing RFC 7636 verifier generator is compatible with TikTok’s documented unreserved-character alphabet.

### Fixed

- **The test suite runs under `dune runtest` (and so in CI).** `test/` was
  declared as an `executable`, so it never ran, and three of its assertions
  had gone stale against the code:
  - `to_canonical_account_analytics_series` returns 8 series (4 account
    stats, 4 recent-video totals); the test still expected 9 from the
    `collect_count` series that was removed because the v2 Video Object has
    no such field.
  - Chunked upload: the test asked for 4-byte chunks of a 10-byte video and
    expected three chunks. TikTok requires 5-64 MB chunks, a floor-divided
    chunk count with the remainder in the last chunk, and a single whole
    chunk under 5 MB, which is what the code does. The test now checks a
    12 MB upload (two chunks) and a sub-5 MB upload (one chunk).
  - `post_single` / `post_thread` were tested for `PULL_FROM_URL`, but they
    download and use `FILE_UPLOAD` because `PULL_FROM_URL` needs URL
    ownership verification that presigned storage URLs cannot pass.
  No library code changed.

### Changed

- 403 responses with scope/permission body markers now carry TikTok's
  verbatim error text via the new `platform_message` field on
  `Insufficient_permissions`. See `social-core/CHANGES.md` for the
  breaking variant change.

### Added

- **`Make.get_oauth_url ?scopes ?pkce_method`** (S12): the requested scopes are a parameter (default unchanged: `default_oauth_scopes` = `user.info.basic`, `video.publish`, `video.upload`), so `user.info.profile`, `user.info.stats` and `video.list` can be requested. `?pkce_method:Pkce_s256_hex` sends TikTok's documented S256 challenge, which is the **hex** SHA-256 of the verifier (Login Kit for Desktop), not RFC 7636 base64url; `pkce_challenge_s256_hex` is exported. The default stays `Pkce_plain` (unchanged URL). Login Kit for Web does not document PKCE.
- **`OAuth.Make(Http).exchange_code_full` / `refresh_token_full`** and **`Make.exchange_code_full`**: the token answer as `OAuth.token_response = {credentials; open_id; scopes; expires_in; refresh_expires_in; refresh_expires_at}`; `credentials.scope` is the granted scope string. An OAuth error delivered with HTTP 200 becomes `Api_error` (status 400, `request_id` = `log_id`, body kept). `exchange_code` / `refresh_token` are unchanged.
- **`OAuth.Make(Http).get_user ~access_token ?fields`**: typed `/v2/user/info/` (`OAuth.user_info`, including `username`, `is_verified` and the stats counts) with `user_fields_basic` / `user_fields_profile` / `user_fields_stats`. `error.code` other than `"ok"` is an error even under HTTP 200: `access_token_invalid` is `Token_invalid`, `scope_not_authorized` / `scope_permission_missed` are `Insufficient_permissions` naming the scopes the requested fields need, `rate_limit_exceeded` is `Rate_limited`. `get_user_info` (raw JSON) is unchanged.
- New direct dependency: `digestif` (already a dependency of `social-core`).

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
