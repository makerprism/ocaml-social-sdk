# social-refresh

Optional shared token refresh orchestration package for `ocaml-social-sdk`.

This package introduces provider-neutral refresh types and API surface so refresh
decision and orchestration logic can be centralized without changing provider
public APIs.

## Modules

- `Social_refresh.Types`: shared refresh policy, decisions, and outcomes.
- `Social_refresh.Time`: RFC3339 parsing and refresh-window calculations.
- `Social_refresh.Decision`: token refresh decision engine (`Skip` vs `Refresh_required`).
- `Social_refresh.Orchestrator`: load/decide/refresh/persist/health pipeline.

## Orchestrator hooks for production hardening

`Social_refresh.Orchestrator.ensure_valid_access_token` now supports optional
hooks to make delegation safer in production environments:

- `with_account_lock`: serialize refresh work per account (singleflight lock).
- `reload_credentials`: reload latest credentials before refresh to reduce
  refresh-token rotation races.
- `max_refresh_attempts` + `should_retry_refresh_error` + `sleep_before_retry`:
  controlled retry for transient refresh failures.
- `map_refresh_error_to_health`: customize health-state mapping on refresh
  failures.
- `on_refresh_attempt` / `on_refresh_success` / `on_refresh_failure`:
  structured telemetry hooks.

## Current integrations

- `social-twitter-v2` (2h access token, 30min refresh window)
- `social-youtube-data-v3` (1h access token, 10min refresh window)
- `social-tiktok-v1` (24h access token, 1h refresh window)
- `social-reddit-v1` (1h access token, 5min refresh window)
- `social-pinterest-v5` (30d access token, 7d refresh window)
- `social-linkedin-v2` (60d access token, 7d refresh window; Partner Program only)
- `social-instagram-graph-v21` (60d access token, 7d refresh window)
- `social-instagram-standalone-v21` (60d access token, 7d refresh window)
- `social-threads-v1` (60d access token, configurable refresh window)

## License

MIT
