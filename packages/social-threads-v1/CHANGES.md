# Changelog

## 0.1.1 - Unreleased

- Added account analytics API `get_account_insights` for `/v1.0/{id}/threads_insights`.
- Added post analytics API `get_post_insights` for `/v1.0/{post_id}/insights`.
- Added typed analytics parsing for account/post insight payloads.
- Added canonical analytics conversion helpers for social-core metric normalization.
- Added request-contract and malformed-payload tests for analytics endpoints.
- Added token redaction guard for network errors surfaced from query-token analytics/read URLs.
- Changed OAuth authorization endpoint from `threads.net` to `www.threads.com` to match Meta's April 2025 domain migration (threads.net now 301-redirects to threads.com, breaking the GDPR consent flow).

## 0.1.0 - 2026-02-12

- Added new `social-threads-v1` package scaffold.
- Added OAuth authorization URL generation and token helpers:
  - auth code exchange
  - long-lived token exchange
  - token refresh
- Added read APIs for `get_me` and `get_posts`.
- Added publish flow for text and single-media posts in `post_single`.
- Added reply-chain support in `post_thread` with `reply_to_id` chaining.
- Added explicit validation for supported media URL formats and single-media-per-post limits.
- Added media-only post support (empty text allowed when a valid media URL is provided).
- Improved rate-limit parsing with `Retry-After` normalization and request-id header fallback mapping.
- Normalized outbound idempotency keys and media URLs (trimmed input before request payload generation).
- Added token expiry skew precheck with configurable `THREADS_TOKEN_EXPIRY_SKEW_SECONDS`.
- Added optional automatic token refresh path for near-expired credentials (`THREADS_AUTO_REFRESH_TOKEN`).
- Hardened expiry parsing: malformed `expires_at` values are treated as expired (with optional auto-refresh recovery).
- Added stricter OAuth input validation for empty `state`, `redirect_uri`, and authorization code values.
- Added stricter token input validation for long-lived exchange/refresh and introduced `validate_oauth_state` helper.
- Hardened CSRF state checks to require exact state equality (no implicit whitespace normalization).
- Improved credential loading error mapping: missing credentials stay auth errors, backend failures map to internal errors.
- Expanded credential error classification to treat "not found" credential-load failures as missing credentials.
- Hardened OAuth response parsing to reject empty access tokens and tightened credential error classification context for "not found" failures.
- Enforced strict OAuth/token input hygiene by rejecting leading/trailing whitespace in sensitive inputs.
- Tightened credential error classification to avoid mapping non-credential "missing" errors to auth failures.
- Added upper-bound normalization for `Retry-After` parsing (clamped to 86400 seconds).
- Expanded credential error phrase matching for "no credential"/"credentials missing" variants.
- Refined credential error phrase matching to reduce false positives for non-auth backend failures.
- Hardened `Retry-After` float parsing for overflow/Infinity values with safe clamping.
- Enforced strict whitespace rejection for configured OAuth credentials (`THREADS_CLIENT_ID`, `THREADS_CLIENT_SECRET`).
- Aligned OAuth authorization endpoint host to official sample (`https://www.threads.net/oauth/authorize`).
- Added JSON error payload redaction for sensitive fields before surfacing OAuth/read errors.
- Added non-JSON error-body redaction fallback when sensitive token/credential markers are present.
- Added sensitive-text redaction for provider `error.message` values in structured API errors.
- Pinned OAuth/token helper endpoints to `v1.0` for consistent API versioning.
- Preserved provider `code`/`type`/`error_subcode` metadata in structured API error messages for unknown API errors.
- Added optional strict redirect URI matching against `THREADS_REDIRECT_URI` for auth URL and auth code exchange.
- Enforced whitespace hygiene for configured `THREADS_REDIRECT_URI` when strict redirect matching is enabled.
- Tightened OAuth state validation to reject leading/trailing whitespace in both expected and received values.
- Normalized OAuth `token_type` parsing to default to `Bearer` when missing or blank.
- Canonicalized OAuth `token_type` value `bearer` to `Bearer` for consistency.
- Normalized OAuth `expires_in` parsing by clamping negative values to `0` and very large values to `31536000`.
- Hardened `expires_in` float parsing for extreme values (including overflow to infinity).
- Updated fractional `expires_in` parsing to round up positive float values.
- Added explicit `refresh_account_credentials` helper with persistence/error handling paths.
- Reused credential error classification in explicit refresh flow to distinguish missing credentials vs backend load failures.
- Tightened credential-missing phrase detection to require credential-specific context (reducing account/backend false positives).
- Added initial package documentation and conformance planning docs.
