# Unreleased

- Keep pending sessions unusable until the coordinated completion callback confirms commit. A failed acquisition cannot evict a newer waiter's cache entry.

- Add `ensure_valid_token_with_outcome ~with_refresh_lock` reporting reused or renewed-and-persisted tokens through an explicit completion boundary. Existing APIs remain compatible; credential-only functors never write health.

# Changelog

- Add `Make_credentials_only` for coordinated credential operations without account-health callbacks. Legacy functors retain their behavior.

- Add `COORDINATED_CONFIG` and `Make_coordinated` for account credential renewal held under a caller-provided lock through durable persistence. Failed commits invalidate the session cache so subsequent callers cannot reuse an uncommitted session.

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

### Added

- **Typed profile with counts**: `Reads.Make.get_profile ~service_url ?access_jwt ~actor` (`app.bsky.actor.getProfile`, works on `Reads.public_api_url` without a session) and `Make_with_options.get_profile_view ~account_id ~actor` return `Views.profile_detailed` (`did`, `handle`, `display_name`, `description`, `avatar`, `banner`, `followers_count`, `follows_count`, `posts_count`, `created_at`, `indexed_at`; counts are `option` because the lexicon does not require them). The raw-JSON `get_profile` is unchanged. Source: `app.bsky.actor.defs#profileViewDetailed` (https://github.com/bluesky-social/atproto/blob/main/packages/api/lexicons/app/bsky/actor/defs.json), https://docs.bsky.app/docs/api/app-bsky-actor-get-profile. Additive.
- **Typed reads: threads, author feeds, post batches, notifications,
  handles.**
  - `Views` (module `Bluesky_views`, with an `.mli`): `post_view` (author,
    record `text` / `createdAt` / `reply`, counts), the recursive `thread`
    (`threadViewPost` with `parent` and `replies`, `notFoundPost`,
    `blockedPost`, unknown `$type` kept as `Unknown_post`), `feed_view_post`
    (hydrated reply context, `reasonRepost` / `reasonPin`),
    `author_feed_page`, `notification` with a `notification_reason`
    variant, `notifications_page`, `author_feed_filter`. Helpers:
    `post_record_text`, `post_record_reply`, `rkey_of_uri`,
    `post_web_url`.
  - `Reads.Make (Http)`: `~service_url` with an optional `~access_jwt`, so
    public reads can go to `Reads.public_api_url`
    (`https://public.api.bsky.app`) with no session.
    - `get_post_thread ?depth ?parent_height`
    - `get_author_feed ?filter ?include_pins ?cursor`
    - `get_posts`: batches of 25 `uris`, sent as repeated parameters
    - `list_notifications ?reasons ?cursor`
    - `resolve_handle`: `HandleNotFound` and the 400 "Unable to resolve
      handle" are `Resource_not_found`.
  - `Make_with_options` wrappers for a stored account, sent to the
    configured `service_url`: `get_post_thread_view`, `get_author_feed_page`,
    `get_posts`, `list_notifications_page`.
  - `parse_xrpc_error` and `atproto_error_name` at top level;
    `Make_with_options.parse_api_error` / `atproto_error_name` are the same
    functions under their old names.
  - Not breaking: `strong_ref` and `reply_ref` moved to `Bluesky_views` and
    are re-exported with the same fields. The JSON-returning
    `get_post_thread`, `get_author_feed`, `list_notifications` and
    `resolve_handle` are unchanged.

- **`OPTIONS.sleep`**: a non-blocking wait in the consumer's runtime
  (`seconds:float -> (unit -> unit) -> (string -> unit) -> unit`).
  `Default_options.sleep = None`.

- **`reply_to_post ~reply_to`** replies to any post. `reply_to` is a
  `reply_ref` (`root` and `parent`, each a `strong_ref` of `uri` and `cid`);
  text, media, facets and link cards work as in `post_single`. Replying used
  to need the internal `post_with_reply_impl` and a 4-tuple.
- **`post_thread ?reply_to`** attaches the first post of a thread under a
  reply reference. That posts a whole thread as a reply, and resumes a
  thread that failed part-way: pass the original root, the last post that
  went out, and only the remaining texts.
- `strong_ref_of_post_id` / `post_id_of_strong_ref` convert between a
  `strong_ref` and the `"uri|cid"` id the posting functions return.

### Fixed

- **Video upload follows Bluesky's documented video-service flow, and works
  without one.** Three defects meant the `video.bsky.app` path could only
  succeed for a video the service had already processed:
  - `getJobStatus` was polled 5 times back to back with no wait, so any real
    processing (seconds to a minute) ended in "Video processing timeout".
    With `Options.sleep` the SDK now waits
    `video_job_poll_interval_seconds` (2 s) between polls, for at most
    `video_job_max_polls` (150, i.e. 5 minutes).
  - The service token's audience was `did:web:<service_url host>`
    (`did:web:bsky.social`), but a Bluesky-hosted account lives on a PDS
    shard and the video service stores the processed blob there. The
    audience now comes from the `#atproto_pds` endpoint in the session's
    DID document (`getSession`, `resolve_pds_endpoint`), and the token is
    requested from that PDS, as Bluesky's own client does. A non-public or
    non-https endpoint is ignored in favour of `service_url`.
  - When service auth failed, the user's access JWT was sent to
    `video.bsky.app` instead. It now falls back to `uploadBlob`, and job
    polling sends no credentials at all.
  Also: a blob in any response wins (an already-processed video is answered
  with `already_exists` plus the earlier blob or job), job states are read
  per the lexicon (`JOB_STATE_FAILED` reports `failureCode`/`error`), and a
  401 from the video service (token refused) falls back to `uploadBlob` like
  an unavailable method does.
  **Behaviour change for `Make` without a sleep:** video now goes straight to
  `uploadBlob` (Bluesky's documented "simple method"; the clip shows as
  missing for a few seconds after posting) instead of the polling path that
  could not wait. Pass a `sleep` to use the video service.
- **Posts and quote posts name the session DID as `repo`.** They used the
  stored identifier, which can be a login email (valid for
  `createSession`, rejected as a record `repo`) or a handle the user has
  since changed.

- **No helper stores the app password any more.**
  `Auth.Make.session_to_credentials` put the app password in
  `refresh_token`, so anyone with read access to the credential store could
  log in as the user until they revoked it, while `ensure_valid_token` had
  already moved rows to the refresh JWT. New
  `Auth.session_to_refresh_credentials session` (also exposed in
  `Auth.Make`) returns `access_token = did`, `refresh_token = refresh_jwt`,
  `auth_type = Bearer`. `ensure_valid_token` writes the same shape after
  every refresh and when it rotates a legacy app-password row, so the stored
  identifier becomes the DID. A login email or a since-changed handle in
  `access_token` is therefore replaced by a value that is valid as a record
  `repo`. `session_to_credentials` stays for source compatibility and is
  documented as deprecated.

- **The PDS URL is configurable.** `Make` hard-coded `https://bsky.social`
  for every XRPC call, so accounts on a self-hosted PDS and local mock
  servers could not be targeted. The new `Make_with_options (Config)
  (Options)` reads `Options.service_url ()` on every call (a trailing slash
  is ignored); session refresh and creation inside `ensure_valid_token` use
  it too. `Make (Config)` is `Make_with_options (Config) (Default_options)`
  and behaves exactly as before. Define options with
  `include Social_bluesky_v1.Default_options` so options added later do not
  break you. `Make.pds_url` is kept for source compatibility but deprecated: it
  never reflects the options; read `service_url ()`.
- **`parse_api_error` reads the atproto error name.** atproto answers most
  failures with HTTP 400 and names the reason in the body. `ExpiredToken`
  and `InvalidToken` were reported as `Api_error`, and `NotFound` /
  `RecordNotFound` too. They are now `Auth_error Token_expired`,
  `Auth_error Token_invalid` and `Resource_not_found <message>`, so callers
  routing on `Error_types.classify_error` can tell a dead session and a
  deleted post from a malformed request. This applies to every read and
  write that goes through `parse_api_error`.

- **Only a genuinely dead refresh JWT gets bucketed as a revoked session.**
  `ensure_valid_token` treated *any* HTTP 400 from
  `com.atproto.server.refreshSession` as proof the session was gone. atproto
  also answers 400 for `InvalidRequest`, which means the SDK sent a malformed
  call, and a reverse proxy or WAF in front of a struggling PDS can emit a 400
  of its own with no XRPC envelope at all. Both wrote `token_revoked` and gave
  the user a "reconnect this account" badge for something they could not fix.
  The branch now reads the atproto error *name* out of the response body and
  fires only for `ExpiredToken` and `InvalidToken`. Everything else falls
  through to the same `Health_status.of_error` path as any other refresh
  failure, so it leaves the stored status alone.
- **A network error during session refresh is no longer recorded as a
  credential failure.** `ensure_valid_token` funnelled every non-auth refresh
  failure through an `on_other_refresh_failure` helper that wrote the health
  status `refresh_failed` and rewrapped the error as
  `Auth_error (Refresh_failed _)`. A PDS that was briefly unreachable or
  answering 5xx therefore looked exactly like a revoked session: the account
  got a credential-failure status, and consumers routing on
  `Error_types.classify_error` saw `Auth_failure`. Refresh failures are now
  classified with `Health_status.of_error`. Confirmed auth failures still write
  `token_revoked`; everything else leaves the stored status alone and surfaces
  the underlying error unchanged, so a connectivity blip stays transient.

### Added

- `atproto_error_name`, which extracts the machine-readable `error` field from
  an XRPC error body. `parse_api_error` keeps only the human-readable
  `message`, so callers that need to tell one 4xx from another read the name
  with this.

### Changed

- **Security: stop persisting the Bluesky app password long-term.**
  `Make(Config).ensure_valid_token` now exchanges legacy app-password rows
  (`auth_type = App_password`) for a JWT session on first use via
  `Auth.create_session`, then rotates storage to `auth_type = Bearer` with
  `refresh_token = refresh_jwt`. Subsequent refreshes go through
  `Auth.refresh_session`, which rotates the `refresh_jwt` itself. The app
  password is forgotten after the one-time exchange. Previously the app
  password was retained indefinitely and would remain usable by anyone with
  DB access until the user manually revoked it in Bluesky settings.
  `access_token` continues to hold the user's identifier (handle/DID), so
  callers that use `creds.access_token` as the repo field are unaffected.

## [0.0.1] - Unreleased

### Robustness

- Poll video job IDs even when an `already_exists` response says completed but omits the blob.
- Evict matching cached sessions when an authenticated response reports `ExpiredToken` or `InvalidToken`; the next operation refreshes instead of reusing the rejected JWT.
- Preserve `RateLimit-Reset` as a retry delay in HTTP-aware XRPC error mapping, including typed reads.

### Migration notes

- #74: `blob_looks_like_text` moved out of the `Make` functor. Call `Social_bluesky_v1.blob_looks_like_text` instead of `Make(Config).blob_looks_like_text`.

### Security

- Use the shared public-host filter for remote media and discovered PDS endpoints, blocking decimal/hex IPv4, shared/private IPv4 ranges, IPv6 ULA/link-local and transition addresses before sending credentials.

### Fixed

- Use the authenticated session DID for all like, repost, follow, block, and delete record requests, including when persisting refreshed credentials fails.

### Added

- App password authentication with session management
- Post creation with URI and CID extraction
- Delete posts
- Thread support with proper reply chains
- Quote posts with optional media
- Media upload (blobs) supporting up to 4 images
- Video upload validation (50MB, 60s max)
- Rich text facets:
  - URL detection and linking
  - Mention detection with DID resolution
  - Hashtag detection
- Link card embeds for external URLs
- Social interactions: like/unlike, repost/unrepost, follow/unfollow
- Read operations: get post thread, user profile, timeline, author feed
- Get likes, reposts, followers, and follows lists
- Notifications: list, count unread, mark as seen
- Search for users and posts
- Moderation: mute/unmute, block/unblock actors
- Content validation (300 chars, media size/type limits)
- Health status tracking
- Structured error handling:
  - Posting operations use `outcome` type with Success/Partial_success/Failure
  - Non-posting operations use `api_result` type with Ok/Error
  - Partial success for posts where enrichment (link cards) fails
