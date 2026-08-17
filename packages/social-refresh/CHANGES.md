# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.1] - Unreleased

### Changed

- **Breaking**: `ensure_valid_access_token`'s `map_refresh_error_to_health`
  now returns `(string * string) option` instead of `string * string`. `None`
  means "write nothing": the refresh failed for a reason that is not evidence
  about the stored credentials, so the last successful health check stands.
  Callers that pass their own mapping must wrap their result in `Some`.

  This fixes the bug the previous release documented instead of fixing. The
  default mapping sent every error that was not `Missing_credentials` to
  `refresh_failed`, including network errors, timeouts, 5xx responses and rate
  limits. All eleven provider packages that use this orchestrator
  (`social-google-business-v4`, `social-instagram-graph-v21`,
  `social-instagram-standalone-v21`, `social-instagram-standalone-v25`,
  `social-linkedin-v2`, `social-pinterest-v5`, `social-reddit-v1`,
  `social-threads-v1`, `social-tiktok-v1`, `social-twitter-v2`,
  `social-youtube-data-v3`) take that default as it comes, so a platform
  outage marked every account on that platform as needing reconnection while
  the caller's scheduler was still retrying it with backoff. Consumers treat
  `refresh_failed` as terminal, so a transient blip disconnected accounts with
  no path back to healthy.

  The default now mirrors `Health_status.of_error`, the same rule
  `social-bluesky-v1` and `social-mastodon-v1` already follow. Genuine
  credential failures are still recorded, and `Missing_credentials` still maps
  to `token_expired` with its own message.

### Added

- Initial `social-refresh` package scaffold
- Provider-neutral refresh type definitions
- `refresh_time` RFC3339 and refresh-window helpers
- `refresh_decision` engine (`Skip` / `Refresh_required`)
- `refresh_orchestrator` for load -> decide -> refresh -> persist -> health updates
- Production hardening hooks in `refresh_orchestrator`:
  `with_account_lock`, `reload_credentials`, refresh retry controls,
  custom refresh-error to health mapping, and refresh telemetry callbacks
- Package unit tests for malformed timestamps, boundary windows, refresh failure mapping,
  missing refresh token handling, refresh token preservation, and health transitions
- Additional unit tests for retry behavior, reload-before-refresh, and lock hook execution
- Provider integrations in `social-twitter-v2`, `social-youtube-data-v3`,
  `social-tiktok-v1`, `social-reddit-v1`, `social-pinterest-v5`,
  `social-linkedin-v2`, `social-instagram-graph-v21`,
  `social-instagram-standalone-v21`, and `social-threads-v1`
