(** Threads API client.

    [OAuth.Make.refresh_token_typed] preserves refresh failures as
    [Error_types.error]; [OAuth.Make.refresh_token] retains its legacy
    string-error contract. Account-based operations use the typed helper when
    [THREADS_AUTO_REFRESH_TOKEN] is enabled (disabled by default).

    Refresh contract:
    {{:https://developers.facebook.com/docs/threads/get-started/long-lived-tokens/}Threads long-lived tokens}.
    Graph error envelope:
    {{:https://developers.facebook.com/docs/graph-api/guides/error-handling/}Meta error handling}.

    Keep the existing public surface while documenting the typed refresh API. *)
include module type of Threads_v1

(** {1 Coordinated refresh}

    [Make_coordinated] accepts [COORDINATED_CONFIG.with_refresh_lock]. The hook
    must serialize the account across every process, run the supplied callback
    inside the lock, and release the lock before delivering either completion
    callback. It must keep the lock until asynchronous persistence finishes and
    report commit failures through the error callback. Never acquire this lock
    again outside an account-based operation.

    [ensure_account_credentials] reloads credentials inside the lock and checks
    expiry again. Workers can supply their refresh window and [allow_refresh]
    independently of the read-side auto-refresh flag. Explicit
    [refresh_account_credentials] forces a refresh, even without stored expiry.
    Account reads and publishing use the same orchestrator and hook.

    [Make] remains available with its existing uncoordinated configuration and
    explicit-refresh behavior. Token-only OAuth helpers do not persist account
    credentials and do not acquire an account lock.

    Provider contract:
    {{:https://developers.facebook.com/docs/threads/get-started/long-lived-tokens/}Threads long-lived tokens}.
*)

(** {1 Retry and refresh options}

    How many attempts a read may cost and whether an account read may refresh
    an expiring token is consumer policy, not a platform fact, so the
    {b Make} functors come in pairs: [Make_with_options] /
    [Make_coordinated_with_options] / [Make_credentials_only_with_options]
    take an explicit {!OPTIONS}; the plain [Make] / [Make_coordinated] /
    [Make_credentials_only] keep the historical behavior by reading the six
    [THREADS_*] environment keys through {!Env_options} (same keys, same
    defaults, parsed on every call). {!Default_options} holds those defaults
    without any environment lookup. *)

