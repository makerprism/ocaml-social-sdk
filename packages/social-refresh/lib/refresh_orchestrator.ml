let merge_refreshed_credentials ~(current : Social_core.credentials) ~(refreshed : Social_core.credentials) =
  let refresh_token =
    match refreshed.Social_core.refresh_token with
    | Some token when String.trim token <> "" -> Some token
    | _ -> current.Social_core.refresh_token
  in
  let expires_at =
    match refreshed.Social_core.expires_at with
    | Some timestamp when String.trim timestamp <> "" -> Some timestamp
    | _ -> current.Social_core.expires_at
  in
  {
    refreshed with
    Social_core.refresh_token;
    expires_at;
    auth_type = current.Social_core.auth_type;
  }

let is_blank value = String.trim value = ""

(** Load, decide, refresh, persist, and record account health for one account.

    [map_refresh_error_to_health] decides what a failed refresh says about the
    stored credentials. It returns [None] to mean "write nothing": the refresh
    failed for a reason that is not evidence about the credentials, so the last
    successful health check should stand. The default mirrors
    {!Health_status.of_error}, so network failures, rate limits and
    uninterpreted API errors leave the stored status alone.

    That default matters because no provider in this SDK overrides it. Before
    it returned an option, every refresh failure that was not
    [Missing_credentials] wrote [refresh_failed], so a platform outage marked
    every account on that platform as needing reconnection while the scheduler
    was still retrying it with backoff. Consumers treat [refresh_failed] as
    terminal, so a transient blip disconnected accounts with no path back. *)
let ensure_valid_access_token
    ?(policy = Refresh_types.default_policy)
    ?(map_load_error = fun err -> Error_types.Network_error (Error_types.Connection_failed err))
    ?(map_persist_error = fun err -> Error_types.Network_error (Error_types.Connection_failed err))
    ?(map_health_error = fun err -> Error_types.Network_error (Error_types.Connection_failed err))
    ?reload_credentials
    ?(with_account_lock = fun ~account_id:_ run -> run ())
    ?(max_refresh_attempts = 1)
    ?(should_retry_refresh_error = fun _ -> false)
    ?(sleep_before_retry = fun ~attempt:_ continue -> continue ())
    ?(map_refresh_error_to_health = fun err ->
      match err with
      | Error_types.Auth_error Error_types.Missing_credentials ->
          Some (Health_status.(to_string Token_expired), "No refresh token available")
      | Error_types.Auth_error _
      | Error_types.Validation_error _
      | Error_types.Rate_limited _
      | Error_types.Api_error _
      | Error_types.Network_error _
      | Error_types.Duplicate_content
      | Error_types.Content_policy_violation _
      | Error_types.Resource_not_found _
      | Error_types.Internal_error _ ->
          Option.map
            (fun status ->
               (Health_status.to_string status, Error_types.error_to_string err))
            (Health_status.of_error err))
    ?(on_refresh_attempt = fun ~attempt:_ -> ())
    ?(on_refresh_success = fun ~attempt:_ _credentials -> ())
    ?(on_refresh_failure = fun ~attempt:_ _error -> ())
    ~account_id
    ~load_credentials
    ~perform_refresh
    ~persist_credentials
    ~update_health
    on_success
    on_error =
  let fail_health status error_message mapped_error =
    update_health ~account_id ~status ~error_message:(Some error_message)
      (fun () -> on_error mapped_error)
      (fun _ -> on_error mapped_error)
  in
  let refresh_with_retry ~current_credentials =
    let attempts = if max_refresh_attempts < 1 then 1 else max_refresh_attempts in
    let rec loop attempt credentials =
      on_refresh_attempt ~attempt;
      perform_refresh ~credentials
        (fun refreshed_credentials ->
           if is_blank refreshed_credentials.Social_core.access_token then
             fail_health Health_status.(to_string Refresh_failed) "Refresh response missing access token"
               (Error_types.Auth_error (Error_types.Refresh_failed "Refresh response missing access token"))
           else
             let merged = merge_refreshed_credentials ~current:credentials ~refreshed:refreshed_credentials in
             persist_credentials ~account_id ~credentials:merged
               (fun () ->
                  update_health ~account_id ~status:Health_status.(to_string Healthy) ~error_message:None
                    (fun () ->
                      on_refresh_success ~attempt merged;
                      on_success merged)
                    (fun err -> on_error (map_health_error err)))
               (fun err ->
                  on_error (map_persist_error err)))
        (fun err ->
           on_refresh_failure ~attempt err;
           if attempt < attempts && should_retry_refresh_error err then
             sleep_before_retry ~attempt (fun () -> loop (attempt + 1) credentials)
           else
             match map_refresh_error_to_health err with
             | None -> on_error err
             | Some (status, message) -> fail_health status message err)
    in
    loop 1 current_credentials
  in
  let run_with_loaded_credentials credentials =
    match Refresh_decision.decide ~policy credentials with
    | Refresh_types.Skip ->
        update_health ~account_id ~status:Health_status.(to_string Healthy) ~error_message:None
          (fun () -> on_success credentials)
          (fun err -> on_error (map_health_error err))
    | Refresh_types.Refresh_required ->
        (match reload_credentials with
         | None -> refresh_with_retry ~current_credentials:credentials
         | Some reload ->
             reload ~account_id
               (fun latest_credentials ->
                  match Refresh_decision.decide ~policy latest_credentials with
                  | Refresh_types.Skip ->
                      update_health ~account_id ~status:Health_status.(to_string Healthy) ~error_message:None
                        (fun () -> on_success latest_credentials)
                        (fun err -> on_error (map_health_error err))
                  | Refresh_types.Refresh_required ->
                      refresh_with_retry ~current_credentials:latest_credentials)
               (fun err -> on_error (map_load_error err)))
  in
  with_account_lock ~account_id (fun () ->
    load_credentials ~account_id
      run_with_loaded_credentials
      (fun err ->
         on_error (map_load_error err)))
