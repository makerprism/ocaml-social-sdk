# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.1] - Unreleased

### Added

- **`OAuth.Make(Http).exchange_code_full`**: the code exchange keeping the whole answer, `OAuth.token_exchange = {credentials; user_id; permissions : string list option; expires_in}`. Reads both the documented `{"data":[{...}]}` shape and the flat one, with `permissions` as a comma-joined string or an array; `credentials.scope` carries the granted permissions. Replaces the `?on_response` side channel for recovering `permissions`. `exchange_code` is unchanged.
- **`OAuth.Make(Http).get_me ~access_token ?fields ?app_secret`**: token-based typed profile (`OAuth.profile`: `id`, `user_id`, `username`, `name`, `account_type`, `profile_picture_url`, `followers_count`, `follows_count`, `media_count`, `biography`, `website`), flat or data-wrapped answer. Defaults to the fields the Instagram Login guide documents (`OAuth.profile_fields`); `biography`/`website` must be requested explicitly. Pure parsers `OAuth.parse_token_exchange` / `OAuth.parse_profile`.
- **`OAuth.authorize_url ~client_id ~redirect_uri ~state ~scopes ?enable_fb_login ?force_reauth ?extra_params ()`**: the one documented Business Login builder (plain comma-separated `scope`). `enable_fb_login` and `force_reauth` are sent only when given. Host unchanged (`api.instagram.com`).
- **Deprecated**: `OAuth.get_authorization_url` and `Make.get_oauth_url` (both send the undocumented `params[scope]=[...]`); use `OAuth.authorize_url`. Both unchanged.
- **`Make_nonblocking (Config : NONBLOCKING_CONFIG)`**: the provider with
  container polling waiting through `Social_core.SLEEP`
  (`sleep : seconds:float -> (unit -> unit) -> (string -> unit) -> unit`),
  which a cooperative runtime implements without blocking. With Lwt:
  `let sleep = Social_lwt.Lwt_adapter.sleep`. A timer the runtime cannot
  schedule fails the post. `BASE_CONFIG` holds everything else `CONFIG`
  required.
- **`get_permalink ~media_id ~access_token`** and **`get_permalink_for_account ~account_id ~media_id`**: fetch an object's `permalink` by id (`GET /{id}?fields=permalink`, with `appsecret_proof` when an app secret is configured). The result is `string option api_result`: `Ok None` when Meta returns no permalink for an object that exists. The pure parser is exported as `parse_permalink_response`.

### Deprecated

- **`CONFIG.sleep : float -> (unit -> 'a) -> 'a` and `Make`.** The
  polymorphic return type can only be implemented by blocking the calling
  thread, which freezes an Lwt or Eio program for the whole backoff (up to
  13 s per poll). `Make` keeps working unchanged and now delegates to
  `Make_nonblocking`; migrate by replacing `sleep` and the functor name.

### Fixed

- **Graph errors are classified by the table shared with Facebook and
  Threads** (`Meta_graph_error`, see social-core). `parse_api_error` and
  `OAuth.parse_api_error` delegate to it. Behaviour changes, types unchanged:
  - **190 without a subcode is now `Auth_error Token_invalid`** (every 190
    used to be `Token_expired`). 463 and 467 stay `Token_expired`; 458 and 460
    are `Token_revoked`. Consumers that write account health through
    `Health_status.of_error` will record `token_revoked` instead of
    `token_expired` for a 190 that carries no expiry subcode.
  - 9 (publishing limit), 17, 341, 368 and 80000-80014 are `Rate_limited`
    (were `Api_error`). `Rate_limited.limit` is now `None`; it used to claim
    `Some 25`, a publishing quota that is not the limit Graph reported.
  - 3 and 201-298 are `Insufficient_permissions`.
  - 100 with subcode 33 is `Resource_not_found`; 506 is `Duplicate_content`.
  - An HTTP 429 is `Rate_limited` whatever the body.
  - `Api_error.request_id` now carries the body's `fbtrace_id`.
  - Media errors Instagram labels `OAuthException` (9004, 352, 36003, ...)
    stay `Api_error`; they are not token errors.
