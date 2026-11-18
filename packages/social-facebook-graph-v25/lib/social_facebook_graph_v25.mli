(** Account credential coordination.

    [Make_coordinated] accepts [COORDINATED_CONFIG.with_refresh_lock]. Hold
    the account lock until persistence completes and release it before
    delivering completion. Reload credentials inside the lock. Do not acquire
    the same lock around account operations. Token-only OAuth/session helpers
    do not acquire a lock or persist account credentials.

    Provider contract: {{:https://developers.facebook.com/docs/pages/access-tokens/}Credential renewal}.
*)
include module type of Facebook_graph_v25
