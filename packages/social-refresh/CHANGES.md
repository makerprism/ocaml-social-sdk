# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.1] - Unreleased

### Changed

- `ensure_valid_access_token` builds its health-status strings with
  `Health_status.to_string` rather than string literals. The values written are
  unchanged.

  Note: the default `map_refresh_error_to_health` still maps every non
  `Missing_credentials` error to `refresh_failed`, including network errors.
  That is the same class of bug fixed in `social-bluesky-v1` and
  `social-mastodon-v1`, but it affects the eleven providers that use this
  orchestrator, and correcting it needs the argument to become optional
  (`... option`) so a caller can express "write nothing". Tracked separately
  rather than folded into a bug-fix release.

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
