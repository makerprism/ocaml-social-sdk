# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.1] - Unreleased

### Added

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
