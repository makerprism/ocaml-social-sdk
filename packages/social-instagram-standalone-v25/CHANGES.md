# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.1] - Unreleased

### Added

- **Typed insights (`Insights`, module `Instagram_standalone_v25_insights`)**:
  - `get_media_metrics ~post_id ~access_token ~media_product ?metrics` (+ `_for_account`): lifetime media insights per `media_product_type` (`Insights.Feed | Reels | Story`), defaults `Insights.default_media_metrics`. Asking for a metric the product does not support (`Insights.supported_media_metrics`) is a `Validation_error` before any request, because Meta fails the whole call.
  - `get_account_totals ~id ~access_token ~since ~until ?metrics` (+ `_for_account`): `metric_type=total_value&period=day`, defaults `views, reach, accounts_engaged, total_interactions, likes, comments, shares, saves, replies`. Account `views` is documented with `total_value` only.
  - `get_daily_follower_changes` (+ `_for_account`): the `follower_count` insight as `Insights.follower_change = {end_time; change}` per day.
  - `get_account_counts` (+ `_for_account`): absolute `Insights.account_counts = {followers_count; follows_count; media_count}` from the profile.
  The two follower numbers are typed apart so a daily change cannot be stored as the total. Sources: https://developers.facebook.com/docs/instagram-platform/reference/instagram-media/insights, https://developers.facebook.com/docs/instagram-platform/api-reference/instagram-user/insights, https://developers.facebook.com/documentation/instagram-platform/instagram-graph-api/reference/ig-user. **Verify live**: `follower_count` is no longer in Meta's IG User insights table (only a limitation mentions it); one `get_daily_follower_changes` call should confirm it still answers and whether a day's value is new followers or a net change. Also one `get_media_metrics` call per product (`views` and `total_interactions` are marked "in development").
- `get_media_insights ?media_product` (also on `get_media_insights_canonical` and `get_media_insights_for_account`): with `Story`, only `views, reach, shares` are requested (Meta rejects `saved`, `likes`, `comments` for stories). Without it the request is unchanged.
- **`OAuth.Make(Http).exchange_code_full`**: the code exchange keeping the whole answer, `OAuth.token_exchange = {credentials; user_id; permissions : string list option; expires_in}`. Reads both the documented `{"data":[{...}]}` shape and the flat one, with `permissions` as a comma-joined string or an array; `credentials.scope` carries the granted permissions. Replaces the `?on_response` side channel for recovering `permissions`. `exchange_code` is unchanged.
- **`OAuth.Make(Http).get_me ~access_token ?fields ?app_secret`**: token-based typed profile (`OAuth.profile`: `id`, `user_id`, `username`, `name`, `account_type`, `profile_picture_url`, `followers_count`, `follows_count`, `media_count`, `biography`, `website`), flat or data-wrapped answer. Defaults to the fields the Instagram Login guide documents (`OAuth.profile_fields`); `biography`/`website` must be requested explicitly. Pure parsers `OAuth.parse_token_exchange` / `OAuth.parse_profile`.
- **`OAuth.authorize_url ~client_id ~redirect_uri ~state ~scopes ?enable_fb_login ?force_reauth ?extra_params ()`**: the one documented Business Login builder (plain comma-separated `scope`). `enable_fb_login` and `force_reauth` are sent only when given. Host unchanged (`api.instagram.com`).
- **Deprecated**: `OAuth.get_authorization_url` and `Make.get_oauth_url` (both send the undocumented `params[scope]=[...]`); use `OAuth.authorize_url`. Both unchanged.
- **Typed media comments and replies with paging** (`Comments`, with `.mli`): `comment = { id; text; username; timestamp; hidden; parent_id; like_count; replies : comment Meta_graph_paging.page option }`. `get_comments_page ~media_id ~access_token ?replies_limit ?position ?limit` reads `GET /{media_id}/comments` with the `replies.limit(N){...}` expansion (default 50), so each comment carries the first page of its replies; `get_comment_replies_page ~comment_id` reads `GET /{comment_id}/replies` for the rest. `?limit` is clamped to 1..50 (Instagram's per-page maximum). `?position` takes `After`/`Before` cursors or a `paging.next` URL (top-level or `replies.paging.next`), followed only on `graph.instagram.com` and for the matching edge, with the echoed `access_token` dropped; the token is sent as a Bearer header with `appsecret_proof`.
- **Comment moderation**: `reply_to_comment ~comment_id ~access_token ~message` (`POST /{comment_id}/replies`, returns the new id; Instagram only threads replies under top-level comments), `set_comment_hidden ~comment_id ~access_token ~hidden` (`POST /{comment_id}` `hide`), `delete_comment` (`DELETE /{comment_id}`). `{"success": false}` is an `Api_error`.
- `_for_account` variants of the five calls above, on the stored (auto-refreshed) token.
- No comment-like call: Instagram's API documents no endpoint for liking a comment.
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

- The `follower_count` canonical series is `Follows`, not `Followers` (social-core `Analytics_normalization`): it is a per-day change, and normalizing it as the follower total let one day's change overwrite the total in consumers. `account_audience_insights.follower_count` is documented as such.
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
