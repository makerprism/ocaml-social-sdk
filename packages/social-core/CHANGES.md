# Unreleased
- **Breaking:** `CONFIG` no longer requires `encrypt` / `decrypt`: no SDK operation ever called them (credential storage encrypts what it persists itself), so a CLI or in-memory consumer no longer implements irrelevant encryption callbacks. Configurations that still define the members keep compiling; they are simply unused. `Config_to_lwt` in `social-lwt` dropped the corresponding wrappers.
- **Breaking:** removed the unused `Platform_types.account` storage-shaped record (`credentials_encrypted` and friends). Stored-account shape is consumer infrastructure, not a provider fact; nothing in this SDK read it.

- The capability table now says **how** each platform measures its `max_text_length`: new `text_counting` field (`Utf8_codepoints` / `Utf8_bytes` / `Twitter_weighted` / `Codepoints_emoji_as_bytes`) with a shared `count_text`. `validate_text_length` counts the platform's way instead of raw bytes: YouTube keeps bytes (its description limit is documented as "a maximum length of 5000 bytes"), Threads counts characters with emoji as their UTF-8 byte count, Twitter uses its weighted count, and the character-based platforms no longer reject accented text below their limit (200 `é` are 200 characters, not 400 bytes). Additive fields; behavior of `validate_text_length` changes as described.
- Add `alt_text_max_length` and `alt_text_length_violation` to the capability table: Threads documents 1,000 characters and Instagram 1,000 for image posts; platforms without a documented cap report `None` (Bluesky's lexicon currently defines no alt length).
- Instagram's feed video/image aspect ratio is the documented **range** "within a 4:5 to 1.91:1 range" (new `video_aspect_ratio_range` field), not three discrete ratios at 5% tolerance: `is_aspect_ratio_compatible`, `aspect_ratio_satisfied` and `eligible_video_formats` accept any ratio in 0.8..1.91 for the feed format. Reels and Stories keep their discrete 9:16 constraint.
- Video upload caps now separate platform fact from client implementation: `video_max_size_mb` is the documented platform limit (YouTube is 256 GB per videos.insert, fixing the old 256 MB entry) and new `video_client_max_size_mb` is this library's own upload ceiling (YouTube 2 GB, no chunked resumable upload). `effective_video_max_size_mb` picks the binding one and `validate_video` enforces it.
- Add `Social_text_counter` (moved from `Twitter_char_counter` in social-twitter-v2, unchanged API) plus `utf8_codepoint_length` and `codepoints_emoji_as_bytes_length`; the Twitter module is now a deprecated alias.
- `Content_validator.validate_video` compares the duration as a float: 60.9 s now exceeds a 60 s cap (truncating to 60 first let fractional overruns pass).

- Add `Social_http_retry`: case-insensitive header lookup (`find_header`, `header_int`), `Retry-After` delay parsing with fractional seconds rounded up and a caller-supplied safety cap (`delay_header_seconds`), and a CPS read-retry combinator (`read_with_retry` with `read_retry_policy`) covering attempt limits, retryable statuses, transport failures, per-trigger delays and a summed-delay budget. Read (idempotent GET) use only.

- Add `Social_meta_graph_error.failure` and `response_error`, retaining Graph classification, HTTP status, provider code/subcode, trace, redacted diagnostics and optional retry advice. Transport, malformed-response and validation failures remain distinct. `Social_provider_error_detail.of_response` takes `?redact`, applied to the complete body before the excerpt is cut so a redactor always sees whole secrets.

- Add runtime-agnostic `Social_credential_outcome` (`Reused` / `Renewed`) with projection helpers and documented persistence semantics.
- Add `Social_json_reader`: the shared JSON field readers (`member`/`field`/`path` lookup, strict and lenient string/bool/int projections, whole-float `int_value`, paging `data_list`, `map_result` traversal). The coercion variants stay separately named so a migrating parser keeps the exact leniency it had.
- Add `Social_http_request`: repeated-key and Rails-array query expansion (`repeated_query_param` / `repeated_query_pairs`), `percent_encoded_query` (the `Uri.pct_encode` encoding the OAuth and XRPC query strings use, distinct from `form_urlencode_kvs`), and `bearer_pair` / `bearer_header` / `bearer_header_opt` / `json_content_type` header constructors.

# Changelog

- Add an opt-in path to disable legacy account-health callbacks without changing credential results.

## Unreleased

- **Breaking:** removed the deprecated `Secure_random`, `Oauth_pkce`, `Meta_graph_error`, `Meta_graph_paging` and `Provider_error_detail` aliases. Use their `Social_`-prefixed equivalents; their APIs are unchanged.
- **Added:** `Social_capabilities.eligible_video_formats ~platform ~width ~height ~duration_seconds` reports the posting formats a video can be published in on a platform, computed from the format-specific constraint records (`get_capability_with_format`): aspect ratio within the format's `video_aspect_ratios` at the 5% tolerance, duration within the format's cap. `Social_capabilities.classify_video_format` is deprecated in favour of it: choosing Reel over Regular is a publishing preference, not a platform fact, so the caller picks the default. Both functions stay available and unchanged.

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.1] - Unreleased

- Use application-neutral examples and documentation for this general-purpose SDK.

### Analytics correction

- Facebook `post_total_media_view_unique` now normalizes to `Reach`, preserving it alongside total `post_media_view` (`Views`) instead of losing one series. The deprecated legacy metric keeps its previous mapping.

### Security

- Add `Social_public_host.is_public_host`, a runtime-independent literal-address and DNS-name filter using `ipaddr`. It rejects private/reserved and alternate numeric addresses; callers must still enforce DNS and redirect policy.

### Changed

- **Prefixed modules**: `Secure_random`, `Oauth_pkce`, `Meta_graph_error`, `Provider_error_detail` and `Meta_graph_paging` are now `Social_secure_random`, `Social_oauth_pkce`, `Social_meta_graph_error`, `Social_provider_error_detail` and `Social_meta_graph_paging`. Same reason as the token-tooling rename below: `social-core` is `(wrapped false)`, so a generic unprefixed name can clash with any consumer module. The old names stay as deprecated aliases (`include` of the new module, types equal) so existing code keeps compiling; they will be removed in a later release. The deprecation is documented, not an `[@@@deprecated]` alert, because dune's dev profile turns alerts into errors and would break the very consumers the aliases exist for.

### Added

- `Analytics_normalization.facebook_metrics` knows the v25 Page/post/video metrics: `page_media_view` and `post_media_view` (Views), `page_total_media_view_unique` (Reach), `page_follows` (Followers, the follower total), `page_daily_follows_unique` (Follows, the daily gain), `fb_reels_total_plays` and `total_video_views` (Views). Existing mappings are unchanged.
- **Fix (behaviour):** `Analytics_normalization.instagram_metrics` maps `follower_count` (the per-day follower insight) to `Follows`, no longer `Followers`, and adds `followers_count` (the profile total) as `Followers`. Consumers that stored the `Followers` series of Instagram audience insights as the follower total were overwriting it with one day's change. Affects every Instagram package's `to_canonical_account_audience_insights_series`.
- **Renamed before any release**: the modules added for token tooling are now `Social_oauth_scope`, `Social_token_expiry` and `Social_meta_debug_token` (were `Oauth_scope`, `Token_expiry`, `Meta_debug_token`, briefly on main since 884f218 / 69fbb7c). `social-core` is `(wrapped false)`, so its module names share the consumer's global namespace; an unprefixed `Meta_debug_token` clashed with an application module of the same name ("inconsistent assumptions over interface"). New modules in this package carry the `Social_` prefix.
- `Social_meta_debug_token`: the typed answer of Meta's `debug_token` endpoint,
  shared by the Facebook and Threads packages. `t` carries `is_valid`,
  `app_id`, `application`, `token_type`, `user_id`, `profile_id`, `scopes`,
  `granular_scopes` (`{scope; target_ids : string list option}`, ids read
  from strings or integers), `expires_at` (`Never_expires` for `0` |
  `Expires_at unix`), `data_access_expires_at`, `issued_at` and `error`.
  `of_json` / `of_body`, `expires_at_rfc3339`, `target_ids ~scope`,
  `granted_for_target ~target_id ~scope` (per-object grants: a
  `pages_manage_posts` grant limited to Pages A and B is not a grant for
  Page C), and the app tokens `app_access_token` (`id|secret`) and
  `threads_app_access_token` (`TH|id|secret`).
- `Social_oauth_scope`: one reading of granted-scope strings for every provider.
  `split` accepts comma-, space- or mixed-delimited strings (Meta/TikTok/
  Pinterest commas, RFC 6749 spaces) and keeps case; `join Comma|Space`
  builds the request parameter; `mem`, `granted ~scope`, `contains_all
  ~required`, `missing ~requested ~granted` and `dedup` compare names
  case-insensitively.
- `Social_token_expiry`: `expires_in_of_json` (int, float or numeric string,
  clamped to `0 .. max_expires_in` = one year), `expires_at_of_expires_in
  ?now`, `seconds_until ?now` and `is_expired ?buffer_seconds ?now`
  (`None` never expires, an unparseable instant counts as expired). The
  per-package `is_token_expired_buffer` copies (Facebook v21/v25, Instagram
  standalone v21/v25, Instagram graph v21, LinkedIn, TikTok), Threads'
  `parse_expires_in` / `expires_at_of_expires_in` and Google OAuth's
  `expires_at_of_expires_in` now delegate to it; their results are
  unchanged, except that Threads now also reads a numeric-string
  `expires_in`.
- New dependency: `ptime` (already a dependency of every provider package).
- `Meta_graph_paging` (with `.mli`): the page shape of Meta Graph list edges
  shared by the Facebook, Instagram and Threads packages. `'a page = { data;
  paging : cursors option; next_url; previous_url }`, `position = First |
  After of string | Before of string | Page_url of string`, `parse` /
  `of_body ~item`, `has_next`, `next_after` (the after cursor only while a
  `paging.next` exists; Meta also returns one on the last page),
  `cursor_query`, and `resolve_page_url ~allowed_hosts ~edge`, which accepts
  a `paging.next` URL only over https on an allowed host and for the expected
  edge, and strips `access_token` / `appsecret_proof` so the reader attaches
  its own credentials. social-core now depends on `uri` (already a
  dependency of every Meta package).
- `Provider_error_detail` (issue #63): the provider's own error detail of an
  `Api_error`, read back from its `raw_response`. `of_error : Error_types.error
  -> t option`, `of_api_error`, and `of_response ~status_code ?request_id
  body`. A `t` carries `http_status`, `code` (as text: `"190"`,
  `"url_ownership_unverified"`, `"ExpiredToken"`, `"quotaExceeded"`, ...),
  `subcode`, `error_type`, the provider `message`, `trace_id` (recorded
  request id, else `fbtrace_id` / TikTok `log_id`) and a `body_excerpt` of at
  most 1024 bytes. It understands the Meta Graph, TikTok, Google, AT Protocol,
  OAuth 2.0, Mastodon, X/Twitter, LinkedIn, Pinterest and Reddit envelopes.
  Additive: `Error_types.api_error` is unchanged, so no record construction
  or pattern match breaks, and it works for every package's `Api_error`
  without package changes. Other constructors (`Auth_error`, `Rate_limited`,
  ...) carry no body and return `None`.
- `Meta_graph_error`: the one Graph error classifier for the Facebook,
  Instagram and Threads packages, which used to carry three disagreeing
  copies. `of_body`, `classify` and `to_error` follow Meta's error-handling and
  rate-limiting guides: 100/33 `Resource_not_found`, other 100
  `Invalid_request`, 1/2 `Transient`, 4/9/17/32/341/368/613/80000-80014
  `Rate_limited`, 102/190 (and an `OAuthException` without a code)
  `Token_invalid` with the 458/459/460/463/464/467/492 subcodes as a
  `token_reason`, 3/10/200-299 `Permission_denied`, 506 `Duplicate_content`.
  An `OAuthException` that has a code outside the table is *not* a token
  error: Instagram labels media errors (9004, 352, 36003) that way.
  `transport_error` is the shared `Network_error` for requests that got no
  response. `of_error` reads the Graph error back out of an `Api_error`, so
  `Option.map classify (of_error err)` recovers a `Transient` that arrived
  under a 4xx status (the status-only `Error_types.classify_error` reads that
  as permanent; the status is never rewritten). Pinned per code by `test_meta_graph_error`.

  Namespace note: like `Health_status`, both modules claim a bare module name
  (`social-core` is `(wrapped false)`).
- `Social_core.SLEEP`: the runtime timer interface
  (`sleep : seconds:float -> (unit -> unit) -> (string -> unit) -> unit`).
  Providers that back off or poll take it from their `Config` instead of
  blocking the thread. `social-lwt` implements it as `Lwt_adapter.sleep`.
- `Secure_random.bytes`: bytes from the OS CSPRNG (`/dev/urandom`), for
  secrets the SDK generates. Raises `Failure` rather than falling back to a
  predictable generator when no entropy source is readable.
- `Oauth_pkce` (RFC 7636): `generate_verifier ?bytes ()`, `challenge_s256`,
  `challenge_method`. Replaces three per-package copies; verified against the
  RFC's appendix B known answer. `social-core` now depends on `digestif` and
  `base64` (already in every consumer's closure via the provider packages).
  No `mirage-crypto-rng` dependency on purpose: it would need runtime-specific
  seeding and its 0.x/1.x APIs are incompatible, forcing consumers to migrate.
- `Health_status`: the closed set of account-health values providers may report
  through `CONFIG.update_health_status`, with `to_string`, `of_string`, and
  `of_error`. Providers now build the status string with `to_string` instead of
  writing literals, so an unrecognised status can no longer reach a consumer.
  `of_error` is the single source of truth for which errors are evidence about
  stored credentials: it returns `None` for network, rate-limit and generic API
  errors, meaning the caller must leave the stored status untouched.

  Namespace note: `social-core` is `(wrapped false)`, so `health_status.ml`
  claims the bare module name `Health_status` for every consumer that links
  this library. A consumer with its own `Health_status` module will get a name
  clash and has to rename one of them. This is the same trade-off the library
  already makes for `Error_types`, `Platform_types` and `Social_core` itself.

### Changed

- **Breaking**: `Insufficient_permissions` now carries an inline record
  `{ required: string list; platform_message: string option }` instead of a
  bare `string list`. The new `platform_message` field lets platform SDKs
  preserve the verbatim error text from the upstream API alongside the
  required-scopes list, so consumers can distinguish a token-side scope gap
  from a server-side App-Review/asset-binding rejection. Callers that
  pattern-matched `Insufficient_permissions perms` need to be updated to
  `Insufficient_permissions { required = perms; _ }`.

### Added

- Core interfaces for runtime-agnostic social media API clients
- `HTTP_CLIENT` module type for abstract HTTP operations
- `STORAGE` module type for abstract storage operations
- `CONFIG` module type for configuration and credential management
- `Platform_types` module with common types for social platforms
- `Content_validator` module for text and media validation
- `Thread_splitter` module for splitting content into thread posts
- `Url_extractor` module for extracting URLs from text
- CPS (Continuation Passing Style) architecture for runtime independence
- `Error_types` module with structured error handling:
  - `outcome` type for posting operations (Success/Partial_success/Failure)
  - `api_result` type for non-posting operations (Ok/Error)
  - Comprehensive error types: auth, network, validation, API, rate limiting
  - Warning types for partial success scenarios (link card failures, etc.)
  - `thread_result` type for tracking thread posting progress
  - Helper functions: `ok`, `fail`, `api_result_map`, `error_to_string`
- `Analytics_types` module with canonical analytics primitives:
  - canonical metric keys
  - account/post scope modeling
  - time-range and granularity records
  - normalized datapoint and series records
- `Analytics_normalization` module with provider metric mappers for:
  - Facebook, Instagram, Threads, Pinterest, TikTok, X, YouTube
  - stable ordering and dedupe helpers for normalized metric lists
