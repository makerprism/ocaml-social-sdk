# Unreleased

- `Twitter_char_counter` moved to social-core as `Social_text_counter` so the shared capability table can count text per platform; the old module name is a deprecated alias with the identical API.

- One shared read-retry policy for every remote media download (post, thread, quote, reply) through `social-core` `Social_http_retry.read_with_retry`, replacing four private copies that disagreed: transport failures are now retried on every path, permanent 4xx answers fail on the first response instead of being retried, and 429/5xx waits run exponential backoff (0.5 s doubling, 8 s per delay, 20 s total) honoring a 429's `Retry-After` up to 10 s. Failure messages keep the URL: `Failed to fetch media from <url> (status: <n>)` / `... : <transport error>`.

- Add `ensure_valid_token_with_outcome ~with_refresh_lock` reporting reused or renewed-and-persisted tokens through an explicit completion boundary. Existing APIs remain compatible; credential-only functors never write health.

# Changelog

- Add `Make_credentials_only` for coordinated credential operations without account-health callbacks. Legacy functors retain their behavior.

- Add `COORDINATED_CONFIG` and `Make_coordinated` for account credential renewal held under a caller-provided lock through durable persistence.

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.1] - Unreleased

- `User.error_of_response` accepts optional `now` for deterministic retry-delay calculation; existing calls continue using system time.

### Security

- PKCE verifiers came from `Stdlib.Random`, which is predictable. They now
  come from the OS CSPRNG via `Oauth_pkce` (still 64 characters).
  `generate_code_challenge` delegates to `Oauth_pkce.challenge_s256`.
- OAuth 1.0a nonces are drawn from `Secure_random` (still 64 hex characters).
  Linking the package no longer calls `Random.self_init` as a module-init side
  effect; programs that relied on that to seed their own `Random` must seed
  it themselves.

### Changed

- **Breaking (Config implementers):** `CONFIG` now requires
  `sleep : seconds:float -> (unit -> unit) -> (string -> unit) -> unit`
  (`Social_core.SLEEP`). The media-processing poll used to block the whole
  process with `Unix.select` while waiting `check_after_secs`; under Lwt or
  Eio that froze every other request. It now waits through `Config.sleep`.
  Lwt users: `let sleep = Social_lwt.Lwt_adapter.sleep`.

- Forbidden 403 responses now carry Twitter's verbatim `detail` text via
  the new `platform_message` field on `Insufficient_permissions`. See
  `social-core/CHANGES.md` for the breaking variant change.

### Added

- **Token-based `GET /2/users/me`, typed**: `OAuth.Make(Http).get_me ~access_token ?user_fields` (OAuth 2.0 user token) and `Oauth1a.Make(Http).get_me ~consumer_key ~consumer_secret ~token ~token_secret ?user_fields` (signed OAuth 1.0a), both returning `User.t` (`id`, `username`, `name`, `description`, `profile_image_url`, `location`, `url`, `created_at`, `protected`, `verified`, `verified_type`, `public_metrics`) from `api.x.com`. New module `Twitter_user` (re-exported as `Social_twitter_v2.User`) holds the type, parser (`tweet_count`, or `post_count`) and error mapping (401 `Token_invalid`, 403 `Insufficient_permissions ["tweet.read"; "users.read"]`, 429 `Rate_limited` with the `x-rate-limit-reset` delay). The account-bound `Make.get_me` (raw JSON) is unchanged.
#### Character Counting (`Char_counter` module)
- **`count`** - Count characters according to Twitter's rules (URLs=23, CJK/emoji=2)
- **`is_valid`** - Check if text fits within 280 character limit
- **`remaining`** - Get remaining characters available
- Reply mention handling (excluded from count when `is_reply=true`)
- Proper Unicode support for CJK characters and emojis

#### Tweet Operations
- **`post_single`** - Post single tweets with media support
- **`post_thread`** - Post threads with proper reply chaining
- **`delete_tweet`** - Delete tweets by ID
- **`get_tweet`** - Get tweet by ID with expansions and field selection
- **`search_tweets`** - Search recent tweets with pagination support
- **`get_user_timeline`** - Get user's tweets with pagination
- **`reply_to_tweet`** - Reply to tweets with media support
- **`quote_tweet`** - Quote tweets with media support
- **Expansions support** - All read endpoints support v2 expansions
- **Field selection** - Support for tweet_fields, user_fields, etc.

#### User Operations
- **`get_user_by_id`** - Get user information by ID
- **`get_user_by_username`** - Get user information by username
- **`get_me`** - Get authenticated user information
- **`follow_user`** - Follow users
- **`unfollow_user`** - Unfollow users
- **`block_user`** - Block users
- **`unblock_user`** - Unblock users

#### Analytics
- **`get_account_analytics`** - Account metrics via `/2/users/me` + `public_metrics`
- **`get_post_analytics`** - Post metrics via `/2/tweets/{id}` + `public_metrics`
- Added optional `metric_set` parameter for `get_post_analytics` and `get_post_analytics_canonical`:
  - `Public_metrics_only` (default, backward-compatible behavior)
  - `Expanded_metrics` (requests `non_public_metrics`, `organic_metrics`, `promoted_metrics`)
  - `Expanded_metrics_with_fallback` (automatically retries with public-only metrics when expanded metrics are forbidden/unavailable)
- Added typed parsing for expanded post metric objects (when present), including fields such as `url_link_clicks`, `user_profile_clicks`, and `video_view_count`
- Added canonical analytics conversion helpers for social-core metric normalization

#### Payload Parity
- Added optional tweet payload parity fields on write helpers:
  - `reply_settings`
  - `community_id`
- Wired parity fields through `post_single`, `reply_to_tweet`, and `quote_tweet`
- Added request-contract + parser tests for analytics and payload parity fields

#### Engagement Operations
- **`like_tweet`** - Like tweets
- **`unlike_tweet`** - Unlike tweets
- **`retweet`** - Retweet tweets
- **`unretweet`** - Remove retweets
- **`bookmark_tweet`** - Bookmark tweets
- **`remove_bookmark`** - Remove bookmarks

#### Media Upload
- **`upload_media`** - Simple media upload for images
- **`upload_media_chunked`** - Chunked upload for large videos (up to 512MB)
- **Alt text support** - Add accessibility descriptions to media
- **INIT/APPEND/FINALIZE workflow** - Proper 3-phase upload for large files

#### Authentication
- OAuth 2.0 with PKCE authentication
- Token refresh with 30-minute expiration buffer
- Credential management

#### Developer Experience
- **`parse_pagination_meta`** - Helper to extract pagination metadata
- **`parse_rate_limit_headers`** - Parse rate limit info from API responses
- **`rate_limit_info` type** - Structured rate limit information
- **`pagination_meta` type** - Structured pagination metadata
- **Pagination support** - next_token/pagination_token parameters on all list endpoints

#### Structured Error Handling
- Posting operations use `outcome` type with Success/Partial_success/Failure
- Non-posting operations use `api_result` type with Ok/Error
- Partial success for posts where media upload partially fails

#### Architecture
- CPS (Continuation Passing Style) implementation
- Runtime agnostic
- HTTP client agnostic
- Integrated with social-core
