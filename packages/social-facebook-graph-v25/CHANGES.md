# Unreleased

- Add `like_object_detailed`, `get_comments_page_detailed` and `get_page_posts_page_detailed`, preserving Graph error details for every HTTP error kind, numeric Retry-After and trace headers. A 2xx answer the operation cannot read (`{"success":false}`, a page without readable `data`) is a `Malformed_response` keeping its status; a paging URL on another host or edge is a `Validation_failure` that sends no request. Sensitive diagnostics use the existing redactor. The legacy APIs retain their contracts; detailed and legacy share request construction and response parsing.
- Parse typed comments and Page posts with the new `social-core` `Social_json_reader` primitives instead of per-file copies. Reader leniency is unchanged (nonempty strings, strict integers).

- Add `reply_to_comment_detailed`, preserving Graph error details for every HTTP error kind, numeric Retry-After and trace headers. Sensitive diagnostics use the existing redactor. The legacy reply API retains its contract; both APIs share request construction and response parsing.

- Add `ensure_valid_token_with_outcome ~with_refresh_lock` reporting reused or renewed-and-persisted tokens through an explicit completion boundary. Existing APIs remain compatible; credential-only functors never write health.

# Changelog

- Add `Make_credentials_only` for coordinated credential operations without account-health callbacks. Legacy functors retain their behavior.

- **Typed ordinary Page video creation (`Videos`, module `Facebook_graph_v25_videos`)**: `create_video ~page_id ~page_access_token ~file_url ~description` is one `POST /{page-id}/videos` with Facebook fetching the hosted file (token as the Bearer header, `appsecret_proof` appended to the form when `FACEBOOK_APP_SECRET` is set) and returns the created `Videos.video` (`{id}`, the only documented non-resumable return field). A 2xx without a string `id` is an `Internal_error` (Facebook reported no created video); non-2xx keeps the Graph error and its code through `parse_api_error_with_permissions` (`pages_manage_posts`). `post_video ~account_id ~text ~video_url` runs it under the Page context with caption validation and one Page-token recovery retry on an auth failure; the retry follows only a POST Facebook rejected as an auth error, so a completed write is never repeated. The edge has no `place` parameter. Additive; the generic `post` is unchanged. Source: https://developers.facebook.com/docs/graph-api/reference/page/videos/
- Add `COORDINATED_CONFIG` and `Make_coordinated` for account credential renewal held under a caller-provided lock through durable persistence.
- Coordinated Page recovery reports credential reload and persistence failures instead of continuing with unsaved credentials.

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.1] - Unreleased

### Added

- **Typed metric descriptors and projections (`Insights`)**: `typed_metric` constructors (`Page_follows`, `Page_daily_follows_unique`, `Page_media_view`, `Page_unique_viewers`, `Post_media_view`, `Post_unique_viewers`, `Post_organic_unique_viewers`, `Post_reactions_by_type_total`, `Post_clicks`, `Post_clicks_by_type`, `Reel_plays`) with `descriptor` (request name, deprecated aliases that still report the same figure, unit, `Running_total`/`Per_period`/`Lifetime_value` aggregation, accepting edges), `typed_metric_of_name`, `request_names`, and `series` (the response series reporting a typed metric, resolved by descriptor order so a rename cannot flip which figure wins). Projections over a response: `reel_plays` (the first count of the Reel plays family, with the provider metric that answered) plus `reel_plays_metrics` (its request list: `fb_reels_total_plays`, then `fb_reels_total_views` (not in the video insights reference but observed in responses), then `blue_reels_play_count` (documented to exclude replays while `fb_reels_total_plays` includes them)), and `follower_totals_by_day` (`page_follows` reports the follower total as of each day, so every point is an absolute; the day is the date part of `end_time`). Additive; the name-based helpers are unchanged. Sources: https://developers.facebook.com/docs/graph-api/reference/video/video_insights, https://developers.facebook.com/docs/platforminsights/page/deprecated-metrics

### Analytics correction

- Facebook `post_total_media_view_unique` now normalizes to `Reach`, preserving it alongside total `post_media_view` (`Views`) instead of losing one series. The deprecated legacy metric keeps its previous mapping.

### Fixed

- Place locations accept numeric postal codes as well as strings, matching Meta’s field reference while retaining string formatting when supplied.

- Encode permalink lookup identifiers as one path segment, preventing slashes and query delimiters from selecting another Graph edge.

- Video insight permission errors name `read_insights` and `pages_manage_engagement`. Page search no longer presents an unrelated OAuth scope as required; token/app-feature requirements remain documented.

- Remove `post_impressions_unique` from `Page_posts.analytics_fields` and the legacy page-post request, following Meta’s metric deprecation. This avoids invalidating the entire inline insights expansion; reaction and click metrics remain. Correct the general-purpose page-post documentation.

### Fixed

- **`Insights.parse` / `metrics_of_json` no longer fail a whole response over one empty value.** A point whose `value` is missing, `null`, or neither a number nor an object of numbers is left out of that metric's `points` (`latest_count` / `by_type` then answer `None` for it); the other metrics still arrive. Before, such a point turned the read into `Internal_error`, so one metric Meta had no figure for cost the caller every other one. `Error` remains for a root without a `data` array or an entry without a `name`.

### Added

- **Place search (`Places`, module `Facebook_graph_v25_places`)**: `search_places ~access_token ~query` (`GET /pages/search?q=..&fields=id,name,location`, Bearer token, `appsecret_proof` when `FACEBOOK_APP_SECRET` is set) returns `Places.place list` (`{id; name; location}`, `location = {street; city; state; country; zip; latitude; longitude}`). Pages without a location are kept; `Places.coordinates` is `None` for them (Meta rejects such a `place` on a post). A blank query is `Validation_error [Text_empty]`. `type=place` is not sent: the reference documents `q` and `fields` only, and `/search?type=place` is deprecated since v8.0. Source: https://developers.facebook.com/docs/pages-api/search-pages
- **Typed Page feed (`Page_posts`, module `Facebook_graph_v25_page_posts`)**: `get_page_posts_page ~page_id ~access_token ?edge ?fields ?position ?limit` and `get_page_posts_page_for_account ~account_id` return a `Page_posts.page_post Social_meta_graph_paging.page`. `edge` is `Posts` (default), `Feed` (also public posts by others that tag the Page) or `Published_posts`; `fields` is `Page_posts.basic_fields` (default) or `Page_posts.analytics_fields` (inline insights, comment and reaction summaries, shares); `limit` is clamped to the documented 100; `paging.next` URLs are followed only on `graph.facebook.com` and the same edge. `page_post` carries the attachments (`type`, `media_type`, `url`, `media.image.src`, `target.id`, subattachments), the inline `insights` as `Insights.metric list` (`[]` when absent or dropped), `comment_count`, `reaction_count` and `share_count`. Pure helpers `Page_posts.media_type`, `is_reel` (a video whose permalink has `/reel/`) and `video_id_of` (the first video attachment's `target.id`, for `get_video_insights`). Additive. Source: https://developers.facebook.com/docs/graph-api/reference/page/feed/
- `get_page_posts` (raw JSON, `/feed`) is unchanged; it now takes its field list from `Page_posts.analytics_fields` (the same string) and documents `get_page_posts_page_for_account ~edge:Feed ~fields:Page_posts.analytics_fields` as its typed equivalent.
- **Typed Page, post and video insights on v25 metrics (`Insights`, module `Facebook_graph_v25_insights`)**: `get_page_insights ~page_id ~access_token ?metrics ?period ?since ?until`, `get_post_insights ~post_id ~access_token ?metrics ?period`, `get_video_insights ~video_id ~access_token ?metrics ?period` (`GET /{video_id}/video_insights`, Reels plays and watch time), each with a `_for_account` variant. Defaults are metrics Meta does not list as deprecated: `Insights.page_metrics` (`page_media_view`, `page_total_media_view_unique`, `page_follows`, `page_daily_follows_unique`, `page_post_engagements`, `page_video_views`), `Insights.post_metrics` (`post_media_view`, `post_total_media_view_unique`, `post_reactions_by_type_total`, `post_clicks`, `post_clicks_by_type`), `Insights.reels_metrics`, `Insights.video_metrics`. Results are `Insights.metric list` (`name`, typed `period`, `points` of `Count n` or `By_type counts`); pure helpers `parse`, `metrics_of_json` (also reads the `insights` object of a `get_page_posts` item), `find`, `latest_count`, `by_type`, `count`, `to_canonical_series`, `deprecated_metrics`, `is_deprecated_metric`. Requests are percent-encoded, send the token as a Bearer header and in the query, and add `appsecret_proof` when `FACEBOOK_APP_SECRET` is set. Sources: https://developers.facebook.com/docs/platforminsights/page/deprecated-metrics, https://developers.facebook.com/docs/graph-api/reference/insights, https://developers.facebook.com/docs/graph-api/reference/video/video_insights, https://developers.facebook.com/docs/graph-api/changelog/version25.0. **Verify live**: one `get_page_insights` call with the default metrics (in particular whether `page_follows` is the follower total per day, as consumers have observed, while Meta's description reads like a running count), one `get_post_insights` call (in May 2026 a `get_page_posts` field expansion listing `post_total_media_view_unique` came back without insights), one `get_video_insights` call on a Reel.

- **`OAuth.authorize_url ~client_id ~redirect_uri ~state ~scopes ?auth_type ?extra_params ()`**: the one documented Login dialog builder (plain comma-separated `scope`, `response_type=code`, optional `auth_type = Rerequest | Reauthenticate | Reauthorize`). `Make.get_oauth_url` now builds its URL with it (same parameters as before, including `auth_type=rerequest`).
- **Deprecated**: `OAuth.get_authorization_url` (sends the undocumented `params[scope]=[...]` and always `auth_type=rerequest`); use `OAuth.authorize_url`. Unchanged.
- **Login discovery in `OAuth.Make(Http)`**, token-based (no `Config`), each with optional `?app_secret` for `appsecret_proof`:
  - `get_me_id ~access_token`: `GET /me?fields=id`.
  - `get_managed_pages ~access_token ?fields ?max_pages`: `GET /me/accounts`, following `paging.cursors.after` while `paging.next` is present, returning typed `OAuth.page {id; name; access_token; category; about; picture_url; tasks}`. Successor of `get_user_pages`, which reads only the first page and stays.
  - `get_businesses`: `GET /me/businesses` (`OAuth.business`).
  - `get_business_pages ~business_id ~edge:(Owned_pages | Client_pages)`.
  - `get_all_business_pages`: both edges of every Business, deduplicated by Page id; returns `{pages; failures}` so one unreadable Business edge does not hide the others.
  - Pure: `OAuth.default_page_fields`, `OAuth.page_of_json`, `OAuth.parse_graph_list`.
- **`OAuth.Make(Http).debug_token_info ~app_access_token ~input_token`**: typed `GET /debug_token`, returning `Social_meta_debug_token.t api_result` (scopes, granular per-Page scopes, validity, expiry, user id). Graph failures go through `Meta_graph_error`, transport failures are `Network_error`. The JSON-returning `debug_token` stays for compatibility.
- **`OAuth.Make(Http).get_permissions ~access_token ?app_secret`**: typed `GET /me/permissions`, returning `{permission; status = Granted | Declined | Expired | Unknown_status _}` list; `OAuth.granted_permissions` / `declined_permissions` split it. `?app_secret` adds `appsecret_proof`.
- `OAuth.app_secret_proof ~app_secret ~access_token` (lowercase-hex HMAC-SHA256), `OAuth.token_query`, `OAuth.parse_permissions`, usable without a `Config`.
- **Typed Page post comments with paging**: `get_comments_page ~object_id ~access_token ?filter ?order ?position ?limit` (and `get_comments_page_for_account ~account_id`) reads `GET /{object_id}/comments` with `fields=id,message,created_time,from{id,name},permalink_url,parent{id}` and returns `Comments.comment Meta_graph_paging.page`. `Comments.filter` is `Toplevel | Stream` (`stream` lists every level flat, `parent_id` places replies), `Comments.order` is `Chronological | Reverse_chronological`; both are omitted unless given. `from` is an `author option` (`author_id`, `author_name`) because Meta leaves it out without Business Asset User Profile Access; `message` is optional for attachment-only comments. `?position` takes `After`/`Before` cursors or a `paging.next` URL, followed only on `graph.facebook.com` for a `/comments` edge, with the echoed `access_token` dropped and the caller's token sent as a Bearer header with `appsecret_proof`. The existing `get_comments` is unchanged.
- **Comment moderation with a Page token**: `reply_to_comment ~comment_id ~access_token ~message` (`POST /{comment_id}/comments`, returns the new id), `set_comment_hidden ~comment_id ~access_token ~hidden` (`POST /{comment_id}` `is_hidden`), `delete_comment` (`DELETE /{comment_id}`), `like_object` / `unlike_object ~object_id` (`POST` / `DELETE /{object_id}/likes`). They send `appsecret_proof` when an app secret is configured, report `{"success": false}` as an `Api_error`, and name `pages_manage_engagement` in permission errors.
- `paging_cursors` and `'a page_result` are now re-exports of `Meta_graph_paging.cursors` / `'a Meta_graph_paging.page` (same fields), so Facebook, Instagram and Threads pages share one type. `paging_url_hosts` lists the hosts a paging URL may name.
- Internal: `get_url` (GET an absolute Graph URL with Bearer + `appsecret_proof`), which `get_paginated` now uses.
- **`get_permalink ~post_id ~access_token`** and **`get_permalink_for_account ~account_id ~post_id`**: fetch an object's `permalink_url` by id (`GET /{id}?fields=permalink_url`, with `appsecret_proof` when an app secret is configured). The result is `string option api_result`: `Ok None` when Meta returns no permalink for an object that exists. The pure parser is exported as `parse_permalink_response`.
- Top-level API constants, usable without applying `Make`: `api_version`
  (`"v25.0"`), `graph_api_base`, `rupload_base`, `oauth_authorize_url`,
  `token_endpoint`. Applications should use these instead of writing
  `"v25.0"`, so a Graph API bump is a dependency bump. Every URL in the
  package now derives from them (`OAuth.Metadata`, `Make.graph_api_base` /
  `graph_api_version` / `rupload_base` are aliases; `get_oauth_url` and the
  two analytics readers no longer contain their own `v25.0` literal).

### Fixed

- `get_account_analytics` / `get_post_analytics` are built on the shared Graph GET path: they now send `appsecret_proof` when `FACEBOOK_APP_SECRET` is set (apps with "Require App Secret" could not read insights before) and the token as a Bearer header as well as in the query. Their metric sets are unchanged. Both are documented as deprecated: Meta lists `page_impressions_unique`, `page_posts_impressions_unique` and `post_impressions_unique` as deprecated for every API version; use `get_page_insights` / `get_post_insights`. The `get_post_analytics` docstring named `post_total_media_view_unique`; it documents the `post_impressions_unique` it sends.

- **`get_account_analytics` and `get_post_analytics` percent-encode their
  query.** Both interpolated the access token (and `since`/`until`) into the
  URL raw, so a token containing `&`, `+`, `/` or `=` could split the query
  or decode differently. They now build it with `Uri.encoded_of_query`, like
  the rest of the module. On the wire the metric list's commas are now sent
  as `%2C`, which Graph decodes to the same value. Signatures are unchanged.
- **Graph errors are classified by the table shared with Instagram and
  Threads** (`Meta_graph_error`, see social-core). `parse_api_error`,
  `parse_api_error_with_permissions`, `parse_error_code` and
  `parse_facebook_error` delegate to it. Behaviour changes, types unchanged:
  - 100 with subcode 33 (object does not exist) is `Resource_not_found`, not
    `Api_error`.
  - 102 is `Auth_error Token_invalid`; 190 with subcode 458 (app not
    installed) or 460 (password changed) is `Token_revoked` instead of
    `Token_invalid`. 463 and 467 stay `Token_expired`. Page-token recovery
    now also runs on `Token_revoked`, so those two subcodes still go
    through it.
  - 3 and 201-298 are `Insufficient_permissions` (were `Api_error`).
  - 9, 341 and 368 are `Rate_limited` (368 was "temporarily unavailable" and
    surfaced as `Api_error`); `parse_error_code` maps them to
    `Rate_limit_exceeded`, and 1/102/3 to `Temporarily_unavailable` /
    `Invalid_token` / `Permission_denied`.
  - An HTTP 429 is `Rate_limited` whatever the body.
- **Transport failures are `Network_error`, not `Internal_error`.** Every
  request that got no HTTP response (connection refused, DNS, TLS, timeout)
  was reported as `Internal_error`, which the error taxonomy reserves for SDK
  bugs, so consumers routing on the variant treated outages as defects. They
  are now `Network_error (Connection_failed msg)` (message still redacted).
  Worse, Page-token recovery turned a failed `/me/accounts` request into
  `Token_invalid`; after exhausting the candidates it recorded
  `token_revoked` on a healthy account during an outage. It now stops on the
  network error and writes no health status. Behaviour change for code that
  matched `Internal_error` on these paths; types are unchanged.
- **`OAuth.Make.get_user_pages` and `OAuth.Make.debug_token` percent-encode
  their tokens.** Both interpolated tokens into the query string raw. An app
  token is `app_id|app_secret`, and opaque tokens can contain `+`, `/`, `=` or
  `&`, so a reserved character could change or split the query. Both now build
  the query with `Uri.encoded_of_query`, like `exchange_code` already did.
  Signatures are unchanged.
- **Reels publish through the documented Reels Publishing API.**
  `upload_video_reel_typed` (and so `post_reel`) used to post `file_url` to
  `/{page_id}/videos`, which is not the Reels flow and has no `place`
  parameter. It now runs `upload_phase=start` on `/{page_id}/video_reels`, a
  hosted-file upload to `rupload.facebook.com/video-upload/v25.0/{video_id}`
  (`Authorization: OAuth`, `file_url` header), then `upload_phase=finish` with
  `video_state=PUBLISHED`. A 2xx upload or finish response without
  `"success": true` is now a failure instead of a silent success. Token
  recovery still retries the whole flow once.

- **Page-token recovery no longer reports unrecognised health statuses.**
  `recover_page_access_token` wrote the literals `"token_recovered"` and
  `"token_recovery_failed"`, neither of which any consumer recognises. A
  *successful* recovery could therefore land the account in the consumer's
  default state. Recovery now writes `healthy` when it succeeds and a status
  derived from the failure when every candidate user token was rejected.
- **The exhausted-candidates branch now derives its status from the error
  instead of hardcoding `token_revoked`.** It asks `Health_status.of_error`,
  falling back to `token_revoked`. The visible difference today: an expired
  user token (Facebook error 190 with subcode 463 or 467) now records
  `token_expired`, which automatic refresh can still recover, rather than the
  terminal `token_revoked`. The hardcoded value was only ever correct because
  `should_try_next_recovery_token` recurses solely on `Auth_error`, and nothing
  pinned that invariant.
- **A non-auth failure during recovery no longer changes account health.** The
  branch that stops recovery on a rate limit or server error used to write a
  failure status on the way out, even though its own message said the failure
  was not an auth problem. It now leaves the stored status alone.

### Changed

- `parse_api_error_with_permissions` now preserves Facebook's verbatim
  `error.message` when the upstream returns a permission-denied code (200,
  299, or 10), via the new `platform_message` field on
  `Insufficient_permissions`. Previously the SDK substituted a hardcoded
  required-scopes list for FB's actual message, which made it impossible
  for consumers to tell which specific scope or asset binding the platform
  rejected. The synthetic `Insufficient_permissions` constructions in
  `recover_page_access_token` (page token missing / page not found in
  `/me/accounts`) carry descriptive `platform_message` values for the same
  reason.

### Added

- `post_reel`, `upload_video_reel_typed` and `upload_video_reel` take an
  optional `?place` (a location Page ID), sent on the Reels finish phase.

#### Authentication
- OAuth 2.0 authentication flow
- OAuth short-lived -> long-lived exchange support
- 60-day token expiry tracking
- App Secret Proof (HMAC-SHA256 signing)
- Authorization header token transmission
- Page token recovery from `/me/accounts` for posting retries

#### Page Operations
- Facebook Page posting (text + images)
- Photo upload with multipart support
- Field selection for API responses

#### Pagination Support
- `get_page` function for cursor-based pagination
- `get_next_page` helper for fetching subsequent pages
- Cursor-based and URL-based pagination support

#### Rate Limiting
- Automatic parsing of `X-App-Usage` response headers
- `rate_limit_info` type with usage metrics
- Callback hook for rate limit updates

#### Error Handling
- Typed error codes (Invalid_token, Rate_limit_exceeded, Permission_denied, etc.)
- Structured error responses with retry recommendations
- Facebook trace IDs for debugging
- Expanded Facebook/BUC rate limit code mapping (e.g. `80001`, `80002`)

#### Posting Reliability
- Indexed `attached_media[n]` payload for multi-photo feed posts
- Best-effort cleanup of uploaded unpublished photos when feed publish fails

#### Batch Requests
- Combine up to 50 API calls in single request
- Support for dependent requests

#### Generic API Methods
- `get` - GET any endpoint with field selection
- `post` - POST any endpoint
- `delete` - DELETE any resource

#### Analytics
- Added account analytics endpoint support:
  - `get_account_analytics` for `/v20.0/{id}/insights` with day-bucket metrics
- Added post analytics endpoint support:
  - `get_post_analytics` for `/v20.0/{post_id}/insights`
- Added canonical analytics conversion helpers for social-core metric normalization
- Added request-contract and parser tests for account/post analytics flows
- Added redaction guard for analytics network errors so query-token text is not surfaced in internal errors

#### Structured Error Handling
- Posting operations use `outcome` type with Success/Partial_success/Failure
- Non-posting operations use `api_result` type with Ok/Error

#### Architecture
- CPS (Continuation Passing Style) implementation
- Runtime agnostic
- HTTP client agnostic
- Integrated with social-core
