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
