(** Instagram Business Login client.

    [Make_coordinated] uses [COORDINATED_CONFIG.with_refresh_lock] for all
    account-based refreshes. Hold the lock through credential persistence and
    release it before delivering completion. Credential loading and expiry
    decisions happen inside the lock. Do not acquire it around SDK calls.
    [Make_nonblocking] and [Make] retain their uncoordinated contracts.
    Token-only OAuth operations do not acquire account locks.

    Provider contract:
    {{:https://developers.facebook.com/docs/instagram-platform/instagram-api-with-instagram-login/business-login/}Instagram Business Login token refresh}.
*)
include module type of Instagram_standalone_v25
