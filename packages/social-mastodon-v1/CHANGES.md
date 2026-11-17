# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.1] - Unreleased

### Fixed

- **Breaking: `revoke_token` now takes `~client_id ~client_secret` and
  reports failures.** Mastodon's `POST /oauth/revoke` requires `client_id`,
  `client_secret` and `token`
  (https://docs.joinmastodon.org/methods/oauth/#revoke). `Make.revoke_token`
  sent only `token` and returned `Ok ()` on every answer and every network
  error, so disconnects reported success while the token stayed live. It now
  sends all three and returns `Api_error` (with the instance's
  `error_description`) on a non-2xx answer and `Network_error` when the
  instance does not answer. Callers must pass the credentials of the app that
  issued the token (the ones `register_app` returned). Both `revoke_token`
  functions now send form data, as documented, instead of JSON; the request
  is built once by the new `OAuth.revoke_request`.
- **`ensure_valid_token` no longer reports a credential failure when the
  instance is unreachable.** It used to write the health status
  `"invalid_token"` on any `verify_credentials` failure. That literal is not a
  value any consumer recognises, so it got coerced into whatever the consumer's
  default branch happened to be, and it was written even when the instance never
  answered. An instance being down for a minute therefore marked the account as
  needing reconnection. Health is now only written when the instance actually
  answered about the token: `token_revoked` on a 401 or a missing scope,
  `healthy` on success, and nothing at all on a timeout, DNS failure, or 5xx.
- **A rejected health write no longer fails the operation it was reporting
  on.** If the consumer's `update_health_status` called its error
  continuation, `ensure_valid_token` turned that into a
  `Network_error (Connection_failed _)`. On the success path that aborted a
  publish for a token that had *just* verified; on the failure path it replaced
  the real reason with a network error that never happened. Recording health is
  a side effect, never a gate: the outcome of `verify_credentials` decides what
  happens either way. This matters because consumers do reject writes on
  purpose, for instance when they refuse to store a status string they do not
  recognise. `Bluesky.ensure_valid_token` and Facebook's
  `report_recovery_status` already behaved this way.

### Added

- `verify_credentials_result`, which reports failures as a structured
  `Error_types.error` instead of a string. The existing string-returning
  `verify_credentials` is unchanged and now delegates to it. Callers that branch
  on *why* verification failed must use the new function; a string cannot
  distinguish "the instance rejected this token" from "the instance did not
  answer".

### Changed

- 403 responses now carry the Mastodon instance's verbatim `error` text
  via the new `platform_message` field on `Insufficient_permissions`.
  Same applies to the OAuth scope-grant verification path. See
  `social-core/CHANGES.md` for the breaking variant change.

### Added

- OAuth 2.0 authentication flow
  - App registration
  - Authorization URL generation
  - Token exchange from authorization code
  - Credential verification
- Status operations:
  - Post statuses with text, media, visibility, content warnings
  - Thread posting with media support
  - Edit statuses (Mastodon 3.5.0+)
  - Delete statuses
- Visibility levels: public, unlisted, private, direct
- Poll support with multiple choice, expiration, hidden totals
- Interactions: favorite/unfavorite, boost/unboost, bookmark/unbookmark
- Media upload with descriptions (alt text) and focus points
- Content validation (configurable character limits)
- Media size and format validation
- Scheduled publishing support
- Idempotency keys for safe retries
- Structured error handling:
  - Posting operations use `outcome` type with Success/Partial_success/Failure
  - Non-posting operations use `api_result` type with Ok/Error
