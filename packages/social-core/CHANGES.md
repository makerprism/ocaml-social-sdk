# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.1] - Unreleased

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
