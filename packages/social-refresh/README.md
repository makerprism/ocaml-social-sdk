# social-refresh

`social-refresh` is an optional package that centralizes token refresh orchestration for providers in this repository.

It provides:

- shared RFC3339 expiry checks with configurable refresh windows;
- a provider-neutral decision engine (`Skip` vs `Refresh_required`);
- a reusable orchestration pipeline (`load -> decide -> refresh -> persist -> health update`);
- provider-neutral refresh errors with mapping helpers to `Social_core.Error_types`.

## Public modules

- `Social_refresh.Types`
- `Social_refresh.Time`
- `Social_refresh.Decision`
- `Social_refresh.Orchestrator`

## Basic usage

```ocaml
open Social_refresh

let callbacks : Orchestrator.callbacks = {
  load_credentials = ...;
  perform_refresh = ...;
  persist_credentials = ...;
  update_health = ...;
}

let () =
  ensure_valid_token
    ~policy:{ default_policy with refresh_buffer_seconds = 1800 }
    ~account_id:"acct_123"
    ~callbacks
    (function
      | Token_reused access_token ->
          (* Token is still valid. *)
          ignore access_token
      | Token_refreshed { access_token; _ } ->
          (* Token was refreshed and persisted. *)
          ignore access_token)
    (fun refresh_error ->
      let sdk_error = Types.to_social_error refresh_error in
      ignore sdk_error)
```
