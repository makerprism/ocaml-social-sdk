# Unreleased

- Add `Posts.list_fields`: the fields parameter that fills every field of the typed `Posts.post` from the `/{threads-user-id}/threads` edge. `get_user_posts_page` and `get_user_posts_page_detailed` take `~fields` optionally and default to it. Explicit `~fields` callers are unchanged.
- Add `OAuth.Scopes.insights` (`threads_manage_insights`) and `OAuth.Scopes.reply_management` (`threads_read_replies`, `threads_manage_replies`): per-operation scope requirements as scope strings, for comparing against a granted-scope string.
- Text and alt-text validation count characters the way Threads documents ("Text posts are limited to 500 characters. Emojis are counted as the number of UTF-8 bytes." / "The maximum length of alt_text is 1,000 characters.") through social-core `Social_text_counter`: accented text is no longer over-counted (200 `é` are 200 characters, not 400 bytes) and astral emoji count as their 4 UTF-8 bytes instead of raw bytes for every character.

- Add the typed `thumbnail_url` field (video poster frame) to `Posts.post`, so user-post listings no longer need raw-JSON re-parsing for callers that show a video poster. Field additions are additive; existing readers and `raw` are unchanged.

- Add `create_text_container_detailed`, `publish_container_detailed`, `check_container_status_detailed`, `get_conversation_page_detailed` and `get_user_posts_page_detailed`, preserving Graph error details for every HTTP error kind, clamped numeric Retry-After and trace headers. A 2xx answer the operation cannot read (no id, unreadable status, a page without readable `data`) is a `Malformed_response` keeping its status; a paging URL on another host or edge is a `Validation_failure` that sends no request. Retained diagnostics redact whole text carrying a secret (`redact_sensitive_diagnostics`). Read retries keep their existing configuration and the envelope describes the last attempt. The legacy APIs retain their contracts; detailed and legacy share request construction and response parsing.
- `find_header` and `parse_retry_after` delegate to `social-core` `Social_http_retry` (same semantics: case-insensitive lookup; whole or fractional `Retry-After` rounded up, clamped to one day, numeric negatives clamp to 0) instead of package-private copies. No request or error bytes change.
- Parse typed posts and insights with the new `social-core` `Social_json_reader` primitives instead of per-file copies. Reader leniency is unchanged (whole-number floats still accepted for insight counts).

- Add `ensure_valid_token_with_outcome ~with_refresh_lock` reporting reused or renewed-and-persisted tokens through an explicit completion boundary. Existing APIs remain compatible; credential-only functors never write health.

- Add `Make_credentials_only` for coordinated credential operations without account-health callbacks. Legacy functors retain their behavior.

- Add Make_coordinated with a completion-aware account lock for worker, read and publish refresh. Reload and recheck expiry under the lock; preserve Make compatibility.

# Changelog

## Unreleased

- Enabled auto-refresh now preserves transport, rate-limit, server and Graph authentication failures instead of replacing every failure with `Token_expired`. Malformed successful refresh responses are parse errors, not credential-expiry evidence. Added `OAuth.Make.refresh_token_typed`; the legacy string-error refresh API and default-disabled auto-refresh setting are unchanged.

- Added token-based user post pages with caller-selected fields, checked Meta page URLs/cursors, typed common fields and complete raw JSON per item. Existing readers are unchanged.

## 0.1.1 - Unreleased

- Analytics migration: legacy `to_canonical_account_insights_series` maps account views to `Views`; typed `Insights.to_canonical_series ~scope:Account` maps them to `Profile_visits`. Consumers migrating to typed readers must update their canonical-series selection.

- Migration (#66): public `reply_action` changed from `~endpoint ?extra_params` to typed `~edge ~params`. Use `~edge:Manage_pending_reply ~params:[("approve", "true")]` or the stable `approve_pending_reply`, `ignore_pending_reply`, `hide_reply`, and `unhide_reply` wrappers.

- Reply, conversation, and pending-reply pages continue on `cursors.after` without `paging.next`. Nonempty cursor-only pages expose a reconstructed `next_url`; empty pages end traversal. Explicit next URLs remain supported.

- **Location search (`Locations`, module `Threads_v1_locations`)**: `search_locations ~access_token ?query ?coordinates` (`GET /location_search`) and `search_locations_for_account ~account_id`, returning `Locations.location list` (`{id; name; address; city; country; postal_code; coordinates}`, all but `id` optional). Takes a text query, a point (`Locations.coordinates = {latitude; longitude}`), or both; neither is `Validation_error [Text_empty]` and sends nothing. Requests every documented field (`Locations.fields`), because the endpoint returns only `id` by default. The `id` is read from a string or a number (the guide shows both) and kept as a string; it is the `location_id` that `post_single` takes. Reads go through the package's retry policy. Additive. Sources: https://developers.facebook.com/docs/threads/reference/location-search, https://developers.facebook.com/docs/threads/create-posts/location-tagging
- **Fix: `Insights.parse_insights` no longer fails a whole response over one empty value.** A time-series bucket or link total without an integer `value` (Threads leaves the day still being computed empty), or a `total_value` without one, is left out; an entry left with no value shape is skipped. The other metrics still arrive. Before, any of these made the read an `Internal_error`, so one empty bucket cost the caller every metric of the call, including `followers_count`. `Error` remains for a body that is not JSON or has no `data` array. An entry with a non-empty `values` array is now read as a time series even when it also carries `total_value` (an empty `values` still falls back to the total).
- **Typed Threads Insights (`Insights`, module `Threads_v1_insights`)**: `get_user_insights ~access_token ~user_id ?metrics ?since ?until` (`GET /{user_id}/threads_insights`) and `get_media_insights ~access_token ~media_id ?metrics` (`GET /{media_id}/insights`), plus `_for_account` variants (`get_user_insights_for_account ?user_id` skips the `GET /me` lookup). Metrics are `Insights.metric` (`Views | Likes | Replies | Reposts | Quotes | Shares | Clicks | Followers_count`); defaults `Insights.user_metrics` (no `shares`, which Meta documents for media only) and `Insights.media_metrics`. Each result keeps the shape Meta sent: `Time_series` (`values`), `Total` (`total_value`) or `Link_totals` (`link_total_values`, the `clicks` shape). Pure helpers `parse_insights`, `find`, `latest_value ?skip_zero`, `sum`, `to_canonical_series` (user `views` maps to `Profile_visits`: Meta documents it as profile views; `clicks` to `Outbound_clicks`; `followers_count` to `Followers`). No `period` is sent on user insights; the endpoint documents none. Additive. Source: https://developers.facebook.com/docs/threads/insights
- **Fix: `get_account_insights` no longer asks for `shares`** (a media-level metric; the user endpoint documents `views, likes, replies, reposts, quotes, clicks, followers_count, follower_demographics`) and accepts `total_value` entries, which Meta returns for likes, replies, reposts and quotes. Before, the parser required `values[]` on every entry, so a documented response failed the whole call with `Internal_error`; a `total_value` entry is now one value without `end_time`. The existing test fixture used a shape Meta does not send for these metrics. Deprecated in favour of `get_user_insights_for_account` (documentation only). **Verify live**: one `threads_insights` call with all five metrics, to confirm the response shapes and that `period=day` (still sent by this legacy function, undocumented) is ignored.
- `get_post_insights` also reads a `total_value` entry. Its request is unchanged.
- **`OAuth.Make(Http).get_me ~access_token ?fields`**: token-based typed profile (`OAuth.profile = {id; username; name; profile_picture_url; biography; is_verified}`) from `GET /v1.0/me` with every documented field by default (`OAuth.profile_fields`). For OAuth callbacks, before an account is stored. Pure parser `OAuth.parse_profile`. `fetch_me_with_access_token` / `thread_user` are unchanged.
- **`OAuth.authorize_url ~client_id ~redirect_uri ~state ~scopes ?extra_params ()`** takes scope names as strings, the same shape as the Facebook and Instagram builders. `OAuth.get_authorization_url` (typed scopes) and `Make.get_oauth_url` delegate to it; their URLs are unchanged, and so is the `threads.net` host.
- **`OAuth.Make(Http).debug_token_info ~app_access_token ~input_token`**: typed `GET /v1.0/debug_token` (scopes, validity, expiry, user id as Meta recorded them), returning `Social_meta_debug_token.t api_result`. Build the app token with `Social_meta_debug_token.threads_app_access_token` (`TH|app_id|app_secret`).
- **Token-based reply reads with cursor and URL paging**: `get_replies_page`, `get_conversation_page` and `get_pending_replies_page ~access_token ~media_id ?position ?reverse ?limit` (`?approval_status` on pending) return a `thread_post Meta_graph_paging.page` (`data`, `paging` cursors, `next_url`, `previous_url`). `?position` is `First`, `After cursor`, `Before cursor` (new: `before` was not reachable) or `Page_url url`: a `paging.next`/`previous` URL is followed only on `graph.threads.net`/`graph.threads.com` and only for the same edge, with its `access_token` replaced by the caller's. They need no stored credentials. `get_replies`, `get_conversation` and `get_pending_replies` (`~account_id`) now delegate to them; their results are unchanged (items plus `paging.cursors.after`, which Meta also returns on the last page; `Meta_graph_paging.next_after` returns it only while a next page exists). Additive.
- **Token-based reply moderation**: `manage_reply ~access_token ~reply_id ~hide` (`POST /{reply_id}/manage_reply`) and `manage_pending_reply ~access_token ~reply_id ~approve` (`POST /{reply_id}/manage_pending_reply`). `hide_reply`, `unhide_reply`, `approve_reply` and `ignore_reply` delegate to them. Additive.
- `paging_url_hosts`: the hosts a Threads paging URL may name.
- **`get_permalink ~post_id ~access_token`** and **`get_permalink_for_account ~account_id ~post_id`**: fetch an object's `permalink` by id (`GET /{id}?fields=permalink`). The result is `string option api_result`: `Ok None` when Meta returns no permalink for an object that exists. The pure parser is exported as `parse_permalink_response`.
- Graph errors are classified by the table shared with Facebook and Instagram (`Meta_graph_error`, see social-core); `parse_api_error` delegates to it and keeps the ` [code=..;type=..;subcode=..]` message suffix. Behaviour changes, types unchanged: 9, 17, 341, 368, 80000-80014 are `Rate_limited`; 3 and 201-298 are `Insufficient_permissions`; 100 with subcode 33 is `Resource_not_found`; 506 is `Duplicate_content`. **190 without a subcode is now `Token_invalid`** (was `Token_expired` for every 190); 463 and 467 stay `Token_expired`, 458 and 460 become `Token_revoked`. Consumers that write account health through `Health_status.of_error` will record `token_revoked` instead of `token_expired` for a 190 that carries no expiry subcode.
- Numeric `code` fields are no longer redacted from `raw_response`. The secret scrubber replaced every `"code"` key, including Graph error codes, so `Provider_error_detail` could not read them; it now redacts `"code"` only when the value is a string (an OAuth authorization code).
- Added top-level API constants: `api_version` (`"v1.0"`), `graph_api_host`, `oauth_authorize_url`, `token_endpoint`; `threads_api_base` and the token exchange/refresh endpoints now derive from them, and `OAuth.authorization_endpoint` / `OAuth.token_endpoint` alias them. Use these instead of writing `"v1.0"` in application code.
- **Breaking (Config implementers):** `CONFIG.sleep` changes from `float -> (unit -> 'a) -> 'a` to `Social_core.SLEEP` (`seconds:float -> (unit -> unit) -> (string -> unit) -> unit`). The old type could only be implemented by blocking (a polymorphic `'a` result forces the continuation to run before `sleep` returns), and read retries bypassed it entirely with `Unix.select`, freezing Lwt/Eio programs during backoff. Read-retry backoff and container polling now both go through `Config.sleep`. If the timer cannot be scheduled, a read returns the last transient response (or network error) instead of retrying immediately. The package no longer depends on `unix`. Lwt users: `let sleep = Social_lwt.Lwt_adapter.sleep`.
- Fixed `get_replies`, `get_conversation` and `get_pending_replies` requesting the top-level post field list. They asked for `owner` (never returned on replies) and none of the reply fields, so an inbox could not tell who wrote a reply or where it sits in the tree. They now request the documented reply fields (`username`, `permalink`, `root_post`, `replied_to`, `is_reply`, `is_reply_owned_by_me`, `hide_status`, plus `reply_approval_status` on `pending_replies`), exposed as `reply_fields` / `pending_reply_fields`.
- Added reply fields to `thread_post`: `username`, `is_reply`, `root_post_id`, `replied_to_id`, `is_reply_owned_by_me`, `hide_status` (typed `hide_status`, with `Unknown_hide_status` for values Meta adds later) and `reply_approval_status`. **Breaking only for code that builds `thread_post` records literally**; field reads are unaffected.
- Added optional `?reverse` to the three reply readers and `?approval_status` to `get_pending_replies`. Existing calls compile unchanged.
- **Breaking (exhaustive matches only):** `container_status` gains `Published` and `Expired`, matching the documented `PUBLISHED` and `EXPIRED` container states. Previously both were folded into `Error_status`, so `poll_container_status` reported an already-published container as a processing failure. `poll_container_status` now returns `Error Duplicate_content` for a published container and an explicit expiry error for an expired one. Code that matches `container_status` exhaustively must add the two cases.
- Fixed reply moderation endpoints. `hide_reply`/`unhide_reply` now call `POST /{reply_id}/manage_reply` with `hide=true|false`, and `approve_reply`/`ignore_reply` call `POST /{reply_id}/manage_pending_reply` with `approve=true|false`, as documented in Meta's reply-management guide. Previously they posted to `/{reply_id}`, `/{reply_id}/approve` and `/{reply_id}/ignore`, which do not exist. Signatures are unchanged.
- Added account analytics API `get_account_insights` for `/v1.0/{id}/threads_insights`.
- Added post analytics API `get_post_insights` for `/v1.0/{post_id}/insights`.
- Added typed analytics parsing for account/post insight payloads.
- Added canonical analytics conversion helpers for social-core metric normalization.
- Added request-contract and malformed-payload tests for analytics endpoints.
- Added token redaction guard for network errors surfaced from query-token analytics/read URLs.
- Documented the hosts on `oauth_authorize_url` and `graph_api_host`; the hosts themselves are unchanged. The authorization endpoint is `https://threads.net/oauth/authorize`. An earlier entry here said it had moved to `www.threads.com`, but that change was reverted before release. As of 2026-11-17 `threads.net`, `www.threads.net` and `threads.com` 301-redirect to `https://www.threads.com/oauth/authorize`; Meta's docs show `threads.com`. `graph.threads.net` and `graph.threads.com` both serve the API (Meta documents both; its examples use `.com`).

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
