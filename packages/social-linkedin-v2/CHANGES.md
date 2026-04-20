# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.1] - Unreleased

### Fixed

- Article payloads (`content.article`) now include the required `title` field.
  LinkedIn's `/rest/posts` endpoint rejects articles without a title (HTTP 422,
  `/content/article/title :: field is required but not found`). `post_single` and
  `post_thread` now accept an optional `?title` argument, and fall back to the
  first line of the post text (truncated to 200 chars) when no explicit title
  is provided so publishing never fails on this field alone.

### Changed

- OAuth hardening:
  - Token exchange now uses form-encoded POST body (`application/x-www-form-urlencoded`) instead of query-string token parameters.
  - Added strict whitespace/blank validation for OAuth inputs (`state`, `code`, `redirect_uri`, credentials, refresh token).
  - Added strict `validate_oauth_state` helper with constant-time equality check.
  - Normalized token parsing (`token_type`, `expires_in`) and blank refresh-token handling.
- Rest.li request conformance:
  - Finder and batch calls now include explicit `X-RestLi-Method` (`FINDER`/`BATCH_GET`).
  - Finder/batch list parameters now use `List(...)` encoding with input guardrails against malformed list-breaking values.
  - `search_posts` now explicitly uses author-based finder behavior and rejects unsupported keyword finder mode.
- Runtime normalization and safety:
  - Pagination params normalized (`start >= 0`, `count >= 1`, capped by endpoint limits).
  - Scroller `page_size` normalized to a minimum of `1`.
  - Scroller `scroll_back` corrected to move back one page and preserve position on failed back requests.
  - Post URN validation tightened across post-targeting endpoints (required, trimmed, delimiter-safe).
  - `post_single` now validates explicit `author_urn` before token/network work (fail-fast on malformed URNs).
  - `LINKEDIN_VERSION` for organization ACL requests is now strictly validated as `YYYYMM` with safe fallback.
  - Organization ACL filter arg is named `acl_state` (not `state`) to avoid confusion with OAuth CSRF state.
  - Added preferred-organization helpers for account-linking UX (`select_preferred_organization_access`, `get_preferred_organization_access`).
  - Added `exchange_code_and_get_preferred_organization` for one-step account linking plus default author selection.
- Error handling:
  - API raw-response redaction added for sensitive fields (`access_token`, `refresh_token`, `client_secret`) and non-JSON payloads.
  - `get_person_urn` hardened with strict `sub` validation and status-specific error hints for 401/403/429.

### Fixed

- Expanded contract-style test coverage for:
  - OAuth request shapes and validation edge cases.
  - Finder and batch Rest.li headers and URL encoding.
  - Search and scroller behavior (author/default-author, keyword rejection).
  - Engagement reads (`get_post_engagement`) and error mapping paths.
  - Pagination normalization and malformed-input rejection paths.
  - Scope-aware 403 mapping assertions across read/write endpoints.
  - `get_person_urn` parse/error branches and redaction behavior.
  - Organization ACL version-header fallback behavior for dotted/invalid formats.
  - Explicit invalid `author_urn` short-circuit behavior (no network calls).
  - Explicit whitespace `author_urn` rejection before any network/token calls.

### Documentation

- Added end-to-end account linking examples for organization/page onboarding:
  - OAuth URL generation with organization scopes
  - OAuth callback exchange + organization access discovery
  - Refreshing organization access for a linked account
  - Posting as selected organization/page author URN
  - Recommended account-linking recipe for persisting credentials and default author selection.

### Tests

- Added OAuth-linking contract coverage for `exchange_code_and_get_preferred_organization` error mapping:
  - ACL `403` -> insufficient permissions context with org scope hints
  - ACL `429` -> rate-limit context

### Added

- Account analytics API:
  - `get_account_analytics` aggregating follower and share/impression statistics from LinkedIn finder endpoints.
- Contract tests for LinkedIn account analytics request shape and parsing.
- OAuth 2.0 authentication with OpenID Connect
- Token refresh support (partner and standard flows)
- Profile API:
  - Get current user profile (name, email, picture, locale)
- Post operations:
  - Create posts with text and media
  - Thread posting with proper reply chains
  - Get single post by URN
  - Get posts with pagination
  - Batch get multiple posts
- Pagination support:
  - Structured `paging` and `collection_response` types
  - Scroller pattern for easy navigation
- Search API (FINDER pattern):
  - Search posts by author finder criteria
  - Explicitly reject unsupported keyword finder mode on `ugcPosts`
- Engagement APIs:
  - Like/unlike posts
  - Comment on posts
  - Get post comments with pagination
  - Get engagement statistics (likes, comments, shares, impressions)
- Media upload with LinkedIn's asset registration flow
- Rich type system for API responses
- Content validation
- CPS-based architecture for runtime independence
- Structured error handling:
  - Posting operations use `outcome` type with Success/Partial_success/Failure
  - Non-posting operations use `api_result` type with Ok/Error
