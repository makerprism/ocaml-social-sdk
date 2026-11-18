# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.1] - Unreleased

### Fixed

- Organization lookup permission errors now name `rw_organization_admin`, matching the provider reference.

- Video part uploads use only `Content-Type`, as documented for signed upload URLs. Member bearer tokens are no longer forwarded to upload hosts.

### Fixed

- Video upload follows the Videos API. `initializeUpload` now declares
  `fileSizeBytes`. Each part in `value.uploadInstructions` is PUT with its own
  `firstByte..lastByte` slice, so multi-part uploads over 4 MiB work. The
  parts' `ETag`s and the `uploadToken` then go to `finalizeUpload`.
  Before this fix the SDK read a `value.uploadUrl` that the Videos API never
  returns and never finalized, so every video post failed.
- Video detection ignores the query string and fragment. A presigned URL such
  as `.../clip.mp4?X-Amz-Signature=...` used to be uploaded as an image.
  The downloaded file's `Content-Type` (`video/*`, `image/*`) now decides
  before the URL path does.
- The post id is read from the `x-restli-id` header case-insensitively.

### Added

- **Organization names by id**: `get_organizations ~access_token ~ids` (in `Make` / `Make_with_options`) returns `organization list` (`{id; localized_name; vanity_name}`) from `GET /rest/organizationsLookup?ids=List(...)`, at most `organizations_lookup_batch_size` (50) ids per request, sent one after the other. `organizationsLookup` returns the non-admin fields for any organization; `/rest/organizations?ids=` would answer 403 for each organization the member does not administer. Duplicate ids are asked once; an id LinkedIn cannot resolve is left out; ids must be all digits (else `Validation_error`, nothing sent). Pure parser `parse_organizations_lookup`. Pairs with `fetch_organization_accesses_by_token`, which returns the ids but no names. Replaces the legacy `v2/organizationalEntityAcls` projection (`organization~(localizedName,vanityName)`) that consumers used to get names. Additive. Source: https://learn.microsoft.com/en-us/linkedin/marketing/community-management/organizations/organization-lookup-api
- **`OAuth.Make(Http).get_userinfo ~access_token`**: token-based `GET /v2/userinfo` (OpenID Connect) returning `profile_info`, for OAuth callbacks before an account is stored. Errors: 401 `Token_expired`, 403 `Insufficient_permissions ["openid"; "profile"]`, 429 `Rate_limited`, else `Api_error` with the body. Pure `parse_userinfo` and `userinfo_error` are exported.
- `media_kind` (`Image | Video | Document`) with `media_kind_of_content_type`,
  `media_kind_of_url` and `resolve_media_kind`. `post_single ?media_kinds` and
  `post_thread ?media_kinds_per_post` take each item's kind by position when
  the caller knows it. A missing or `None` entry is detected from the
  download's `Content-Type`, then from the URL path, and otherwise defaults
  to image.
- `Make_with_options (Config) (Options)` with `OPTIONS.sleep`
  (`Social_core.SLEEP` shape; `Social_lwt.Lwt_adapter.sleep` under Lwt).
  With a timer, a video post polls `GET /rest/videos/{urn}` every
  `video_status_poll_interval_seconds` (5 s) until `AVAILABLE`, for at most
  `video_status_max_polls` (60) checks. `PROCESSING_FAILED` fails the post
  with LinkedIn's `processingFailureReason`. When the checks run out, the
  post is created anyway. `Make (Config)` is
  `Make_with_options (Config) (Default_options)` and has no timer, so it
  creates the post right after `finalizeUpload`.
- Videos API primitives: `initialize_video_upload`, `upload_video_parts`,
  `finalize_video_upload`, `get_video_status`, `upload_video`, and the pure
  `parse_video_upload_session`, `video_part_slice`,
  `video_part_id_of_headers`, `parse_video_status`.
- `post_single` / `post_thread` take `?link_preview` (default `true`, the
  existing behaviour). With `~link_preview:false` a text-only post sends no
  `article` content block, so no link card is generated from the first URL.
  The commentary still goes through `escape_little_text_format`, so callers
  that only wanted to avoid the card no longer need to build their own
  `/rest/posts` request (and lose the escaping).

### Changed

- `register_upload ~media_type:"video"` returns an error and sends no
  request. A video needs its size declared and a multi-part upload, so use
  `upload_video` (`upload_media ~media_type:"video"` and `post_single` do).
  Image and document registration are unchanged.
- `post_single` gains `?media_kinds` and `post_thread` gains
  `?media_kinds_per_post`. Calls compile unchanged, but code that restates
  the function type (for example a module type for a first-class module) must
  add the new optional argument.
- 403 responses now carry LinkedIn's verbatim `message` text via the new
  `platform_message` field on `Insufficient_permissions`. See
  `social-core/CHANGES.md` for the breaking variant change.

### Fixed

- `Make.get_profile` parses with `parse_userinfo`: a `locale` sent as `{"language","country"}` is read as `"en-US"` instead of failing the whole profile with `Internal_error`, and a member of an unexpected type reads as `None`. `profile_info` moved above `OAuth` in the module (same name and fields).

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
