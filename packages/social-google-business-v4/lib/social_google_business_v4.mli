(** Coordinated account refresh.

    [Make_coordinated] accepts [COORDINATED_CONFIG.with_refresh_lock]. Hold
    the account lock through asynchronous persistence and release it before
    delivering completion. Credentials are reloaded and expiry checked inside
    the lock. Do not wrap account operations in another acquisition.
    Existing functors remain compatible. Token-only OAuth helpers do not
    persist credentials or acquire an account lock.

    Provider contract: {{:https://developers.google.com/identity/protocols/oauth2/web-server#offline}Token refresh}.
*)
include module type of Google_business_v4
