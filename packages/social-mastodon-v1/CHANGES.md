# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.1] - Unreleased

### Security

- PKCE verifiers came from an unseeded `Stdlib.Random`, so every process
  produced the same verifier sequence. They now come from the OS CSPRNG via
  `Oauth_pkce` (still 128 characters, now base64url).
- `Idempotency-Key` UUIDs used the same unseeded generator, so after a restart
  the first post could reuse a key the instance still remembered and be
  answered with an earlier status. They now come from `Secure_random`.

### Fixed

- **Array query parameters are sent once per value.** `get_notifications
  ~exclude_types`, `get_account_relationships` (`id[]`) and
  `remove_accounts_from_list` (`account_ids[]`) passed the list to
  `Uri.with_query`, which joins values with commas
  (`exclude_types[]=follow,favourite`). Rails reads that as the one value
  `"follow,favourite"`, so with more than one value nothing was excluded and
  relationships came back for no account. Each value is now its own
  `name[]=value` pair. The tests that missed this decoded the query with
  `Uri.query`, which splits on commas the same way; they now assert one pair
  per value.
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

- **Typed status, context, notification and WebFinger reads.**
  - `Entities` (module `Mastodon_entities`, with an `.mli`): the Status
    entity as `status` (account `acct` / `display_name` / `url`,
    `in_reply_to_id`, `in_reply_to_account_id`, `visibility`, `created_at`,
    `edited_at`, HTML `content`, `spoiler_text`, `mentions`, `reblog`, the
    three counts), `status_context` (with the experimental
    `Mastodon-Async-Refresh` header as `async_refresh`), `notification` with
    a `notification_type` variant covering every documented type (unknown
    ones kept as `Other_notification_type`), `notification_page` /
    `status_page` with `Link`-header cursors, and the WebFinger JRD as
    `webfinger`. Parsers fail on a missing required attribute instead of
    defaulting it.
  - Pure helpers: `html_to_plain_text`, `status_web_url`, `status_id_of_url`,
    `normalize_instance_url` (returns `None` instead of falling back to a
    default instance), `federated_acct`, `split_acct`, `webfinger_resource`,
    `webfinger_actor_url`, `webfinger_profile_page`.
  - `Reads.Make (Http)`: token-based calls that need no account store.
    `get_status`, `get_status_context` (`GET /api/v1/statuses/:id/context`),
    `get_notifications` (`types[]`, `exclude_types[]`, `account_id`,
    `max_id` / `since_id` / `min_id`), `get_account_statuses`,
    `lookup_account` (`GET /api/v1/accounts/lookup`) and `webfinger`
    (`/.well-known/webfinger`). The token is optional where the endpoint is
    public. 404 and 410 are `Resource_not_found`; a 403 names the read scope
    the call needs.
  - `Make` wrappers for a stored account: `get_status_context`,
    `get_status_entity`, `get_notifications_page`,
    `get_account_statuses_page`.
  - `parse_api_error_response ~required_scopes`, the top-level form of
    `Make.parse_api_error`, which is unchanged.
  - Not breaking: `visibility`, `account_summary` and `pagination_cursors`
    moved to `Mastodon_entities` and are re-exported with the same
    constructors and fields. `get_status`, `get_notifications`,
    `get_account_statuses`, `lookup_account_by_acct` and their 404 mapping
    are unchanged.
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
