let rfc3339_in_seconds seconds =
  let now = Ptime_clock.now () in
  let target =
    match Ptime.add_span now (Ptime.Span.of_int_s seconds) with
    | Some t -> t
    | None -> now
  in
  Ptime.to_rfc3339 target

let make_credentials ?refresh_token ?expires_at ?(auth_type = Social_core.Bearer) ?scope access_token =
  {
    Social_core.access_token;
    refresh_token;
    expires_at;
    auth_type;
    scope;
  }

let test_refresh_time_boundaries () =
  let fresh = Social_refresh.Time.needs_refresh ~refresh_window_seconds:1800 (rfc3339_in_seconds 2000) in
  let near = Social_refresh.Time.needs_refresh ~refresh_window_seconds:1800 (rfc3339_in_seconds 1700) in
  let equal = Social_refresh.Time.needs_refresh ~refresh_window_seconds:1800 (rfc3339_in_seconds 1800) in
  assert (fresh = Ok false);
  assert (near = Ok true);
  assert (equal = Ok true);
  print_endline "✓ refresh_time boundaries"

let test_refresh_time_malformed_timestamp () =
  match Social_refresh.Time.needs_refresh ~refresh_window_seconds:1800 "not-a-timestamp" with
  | Ok _ -> failwith "Malformed timestamp should fail"
  | Error _ -> print_endline "✓ refresh_time malformed timestamp"

let test_refresh_decision_engine () =
  let policy = Social_refresh.default_policy in
  let no_expiry = make_credentials "a" in
  let fresh = make_credentials ~expires_at:(rfc3339_in_seconds 4000) "b" in
  let stale = make_credentials ~expires_at:(rfc3339_in_seconds 10) "c" in
  assert (Social_refresh.Decision.decide ~policy no_expiry = Social_refresh.Skip);
  assert (Social_refresh.Decision.decide ~policy fresh = Social_refresh.Skip);
  assert (Social_refresh.Decision.decide ~policy stale = Social_refresh.Refresh_required);
  print_endline "✓ refresh_decision skip/refresh_required"

let test_orchestrator_missing_refresh_token () =
  let statuses = ref [] in
  let result = ref None in
  let load_credentials ~account_id:_ on_success _on_error =
    on_success (make_credentials ~expires_at:(rfc3339_in_seconds 5) "expired")
  in
  let perform_refresh ~credentials:_ _on_success on_error =
    on_error (Error_types.Auth_error Error_types.Missing_credentials)
  in
  let persist_credentials ~account_id:_ ~credentials:_ on_success _on_error = on_success () in
  let update_health ~account_id:_ ~status ~error_message:_ on_success _on_error =
    statuses := status :: !statuses;
    on_success ()
  in
  Social_refresh.Orchestrator.ensure_valid_access_token
    ~account_id:"acct"
    ~load_credentials
    ~perform_refresh
    ~persist_credentials
    ~update_health
    (fun _ -> result := Some (Ok ()))
    (fun err -> result := Some (Error err));
  (match !result with
   | Some (Error (Error_types.Auth_error Error_types.Missing_credentials)) -> ()
   | _ -> failwith "Expected Missing_credentials");
  assert (List.mem "token_expired" !statuses);
  print_endline "✓ orchestrator missing refresh token"

let test_orchestrator_refresh_failure_mapping () =
  let statuses = ref [] in
  let result = ref None in
  let load_credentials ~account_id:_ on_success _on_error =
    on_success (make_credentials ~expires_at:(rfc3339_in_seconds 5) ~refresh_token:"bad" "expired")
  in
  let perform_refresh ~credentials:_ _on_success on_error =
    on_error (Error_types.Auth_error (Error_types.Refresh_failed "invalid refresh token"))
  in
  let persist_credentials ~account_id:_ ~credentials:_ on_success _on_error = on_success () in
  let update_health ~account_id:_ ~status ~error_message:_ on_success _on_error =
    statuses := status :: !statuses;
    on_success ()
  in
  Social_refresh.Orchestrator.ensure_valid_access_token
    ~account_id:"acct"
    ~load_credentials
    ~perform_refresh
    ~persist_credentials
    ~update_health
    (fun _ -> result := Some (Ok ()))
    (fun err -> result := Some (Error err));
  (match !result with
   | Some (Error (Error_types.Auth_error (Error_types.Refresh_failed _))) -> ()
   | _ -> failwith "Expected Refresh_failed");
  assert (List.mem "refresh_failed" !statuses);
  print_endline "✓ orchestrator refresh failure mapping"

let test_orchestrator_preserves_refresh_token_when_missing_in_response () =
  let persisted = ref None in
  let result = ref None in
  let load_credentials ~account_id:_ on_success _on_error =
    on_success (make_credentials ~expires_at:(rfc3339_in_seconds 5) ~refresh_token:"old_refresh" "expired")
  in
  let perform_refresh ~credentials:_ on_success _on_error =
    on_success (make_credentials ~expires_at:(rfc3339_in_seconds 3600) "new_access")
  in
  let persist_credentials ~account_id:_ ~credentials on_success _on_error =
    persisted := Some credentials;
    on_success ()
  in
  let update_health ~account_id:_ ~status:_ ~error_message:_ on_success _on_error = on_success () in
  Social_refresh.Orchestrator.ensure_valid_access_token
    ~account_id:"acct"
    ~load_credentials
    ~perform_refresh
    ~persist_credentials
    ~update_health
    (fun credentials -> result := Some (Ok credentials))
    (fun err -> result := Some (Error err));
  (match !result with
   | Some (Ok credentials) ->
       assert (credentials.Social_core.access_token = "new_access");
       assert (credentials.Social_core.refresh_token = Some "old_refresh")
   | _ -> failwith "Expected refreshed credentials");
  (match !persisted with
   | Some credentials -> assert (credentials.Social_core.refresh_token = Some "old_refresh")
   | None -> failwith "Expected persisted credentials");
  print_endline "✓ orchestrator refresh token preservation"

let test_orchestrator_preserves_expires_at_and_auth_type () =
  let persisted = ref None in
  let result = ref None in
  let current_expiry = rfc3339_in_seconds 1200 in
  let load_credentials ~account_id:_ on_success _on_error =
    on_success
      (make_credentials
         ~expires_at:current_expiry
         ~auth_type:Social_core.OAuth1a
         ~refresh_token:"old_refresh"
         "old_access")
  in
  let perform_refresh ~credentials:_ on_success _on_error =
    on_success
      (make_credentials
         ~refresh_token:"   "
         ~expires_at:"   "
         ~auth_type:Social_core.Bearer
         "new_access")
  in
  let persist_credentials ~account_id:_ ~credentials on_success _on_error =
    persisted := Some credentials;
    on_success ()
  in
  let update_health ~account_id:_ ~status:_ ~error_message:_ on_success _on_error = on_success () in
  Social_refresh.Orchestrator.ensure_valid_access_token
    ~account_id:"acct"
    ~load_credentials
    ~perform_refresh
    ~persist_credentials
    ~update_health
    (fun credentials -> result := Some (Ok credentials))
    (fun err -> result := Some (Error err));
  (match !result with
   | Some (Ok credentials) ->
       assert (credentials.Social_core.access_token = "new_access");
       assert (credentials.Social_core.refresh_token = Some "old_refresh");
       assert (credentials.Social_core.expires_at = Some current_expiry);
       assert (credentials.Social_core.auth_type = Social_core.OAuth1a)
   | _ -> failwith "Expected refreshed credentials");
  (match !persisted with
   | Some credentials ->
       assert (credentials.Social_core.refresh_token = Some "old_refresh");
       assert (credentials.Social_core.expires_at = Some current_expiry);
       assert (credentials.Social_core.auth_type = Social_core.OAuth1a)
   | None -> failwith "Expected persisted credentials");
  print_endline "✓ orchestrator expiry/auth-type preservation"

let test_orchestrator_failure_keeps_root_error_when_health_update_fails () =
  let result = ref None in
  let load_credentials ~account_id:_ on_success _on_error =
    on_success (make_credentials ~expires_at:(rfc3339_in_seconds 5) ~refresh_token:"bad" "expired")
  in
  let perform_refresh ~credentials:_ _on_success on_error =
    on_error (Error_types.Auth_error (Error_types.Refresh_failed "invalid refresh token"))
  in
  let persist_credentials ~account_id:_ ~credentials:_ on_success _on_error = on_success () in
  let update_health ~account_id:_ ~status:_ ~error_message:_ _on_success on_error =
    on_error "health update failed"
  in
  Social_refresh.Orchestrator.ensure_valid_access_token
    ~account_id:"acct"
    ~load_credentials
    ~perform_refresh
    ~persist_credentials
    ~update_health
    (fun _ -> result := Some (Ok ()))
    (fun err -> result := Some (Error err));
  (match !result with
   | Some (Error (Error_types.Auth_error (Error_types.Refresh_failed "invalid refresh token"))) -> ()
   | _ -> failwith "Expected root refresh error when health update fails");
  print_endline "✓ orchestrator failure keeps root cause"

let test_orchestrator_health_transitions () =
  let statuses = ref [] in
  let result = ref None in
  let load_credentials ~account_id:_ on_success _on_error =
    on_success (make_credentials ~expires_at:(rfc3339_in_seconds 5000) "valid_access")
  in
  let perform_refresh ~credentials:_ _on_success on_error =
    on_error (Error_types.Auth_error (Error_types.Refresh_failed "should not be called"))
  in
  let persist_credentials ~account_id:_ ~credentials:_ on_success _on_error = on_success () in
  let update_health ~account_id:_ ~status ~error_message:_ on_success _on_error =
    statuses := status :: !statuses;
    on_success ()
  in
  Social_refresh.Orchestrator.ensure_valid_access_token
    ~account_id:"acct"
    ~load_credentials
    ~perform_refresh
    ~persist_credentials
    ~update_health
    (fun _ -> result := Some (Ok ()))
    (fun err -> result := Some (Error err));
  (match !result with
   | Some (Ok ()) -> ()
   | _ -> failwith "Expected healthy pass-through");
  assert (List.mem "healthy" !statuses);
  print_endline "✓ orchestrator healthy transition"

let test_orchestrator_retries_transient_errors () =
  let attempts = ref 0 in
  let result = ref None in
  let load_credentials ~account_id:_ on_success _on_error =
    on_success (make_credentials ~expires_at:(rfc3339_in_seconds 5) ~refresh_token:"ok" "expired")
  in
  let perform_refresh ~credentials:_ on_success on_error =
    attempts := !attempts + 1;
    if !attempts < 3 then
      on_error (Error_types.Network_error (Error_types.Connection_failed "temporary"))
    else
      on_success (make_credentials ~expires_at:(rfc3339_in_seconds 3600) "new_access")
  in
  let persist_credentials ~account_id:_ ~credentials:_ on_success _on_error = on_success () in
  let update_health ~account_id:_ ~status:_ ~error_message:_ on_success _on_error = on_success () in
  Social_refresh.Orchestrator.ensure_valid_access_token
    ~account_id:"acct"
    ~load_credentials
    ~perform_refresh
    ~persist_credentials
    ~update_health
    ~max_refresh_attempts:3
    ~should_retry_refresh_error:(function Error_types.Network_error _ -> true | _ -> false)
    (fun _ -> result := Some (Ok ()))
    (fun err -> result := Some (Error err));
  (match !result with
   | Some (Ok ()) -> ()
   | _ -> failwith "Expected retry flow to succeed");
  assert (!attempts = 3);
  print_endline "✓ orchestrator transient retry policy"

let test_orchestrator_reloads_latest_credentials_before_refresh () =
  let refreshed_from_token = ref "" in
  let result = ref None in
  let load_credentials ~account_id:_ on_success _on_error =
    on_success (make_credentials ~expires_at:(rfc3339_in_seconds 5) ~refresh_token:"old_rt" "expired")
  in
  let reload_credentials ~account_id:_ on_success _on_error =
    on_success (make_credentials ~expires_at:(rfc3339_in_seconds 5) ~refresh_token:"new_rt" "expired")
  in
  let perform_refresh ~credentials on_success _on_error =
    refreshed_from_token := (match credentials.Social_core.refresh_token with Some t -> t | None -> "");
    on_success (make_credentials ~expires_at:(rfc3339_in_seconds 3600) "new_access")
  in
  let persist_credentials ~account_id:_ ~credentials:_ on_success _on_error = on_success () in
  let update_health ~account_id:_ ~status:_ ~error_message:_ on_success _on_error = on_success () in
  Social_refresh.Orchestrator.ensure_valid_access_token
    ~account_id:"acct"
    ~load_credentials
    ~reload_credentials
    ~perform_refresh
    ~persist_credentials
    ~update_health
    (fun _ -> result := Some (Ok ()))
    (fun err -> result := Some (Error err));
  (match !result with
   | Some (Ok ()) -> ()
   | _ -> failwith "Expected refresh with reloaded credentials");
  assert (!refreshed_from_token = "new_rt");
  print_endline "✓ orchestrator reloads latest credentials"

let test_orchestrator_uses_account_lock_hook () =
  let lock_calls = ref 0 in
  let result = ref None in
  let with_account_lock ~account_id:_ run =
    lock_calls := !lock_calls + 1;
    run ()
  in
  let load_credentials ~account_id:_ on_success _on_error =
    on_success (make_credentials ~expires_at:(rfc3339_in_seconds 3600) "valid_access")
  in
  let perform_refresh ~credentials:_ _on_success on_error =
    on_error (Error_types.Internal_error "should not refresh")
  in
  let persist_credentials ~account_id:_ ~credentials:_ on_success _on_error = on_success () in
  let update_health ~account_id:_ ~status:_ ~error_message:_ on_success _on_error = on_success () in
  Social_refresh.Orchestrator.ensure_valid_access_token
    ~account_id:"acct"
    ~with_account_lock
    ~load_credentials
    ~perform_refresh
    ~persist_credentials
    ~update_health
    (fun _ -> result := Some (Ok ()))
    (fun err -> result := Some (Error err));
  (match !result with
   | Some (Ok ()) -> ()
   | _ -> failwith "Expected success via lock wrapper");
  assert (!lock_calls = 1);
  print_endline "✓ orchestrator account lock hook"

(* Drive one failed refresh through the orchestrator with the stock defaults,
   and report every health status the run wrote plus the error it surfaced.

   The stock defaults are what matters: all eleven provider packages that use
   this orchestrator take [map_refresh_error_to_health] as it comes. *)
let run_refresh_failure refresh_error =
  let statuses = ref [] in
  let result = ref None in
  let load_credentials ~account_id:_ on_success _on_error =
    on_success
      (make_credentials ~expires_at:(rfc3339_in_seconds 5) ~refresh_token:"stored" "expired")
  in
  let perform_refresh ~credentials:_ _on_success on_error = on_error refresh_error in
  let persist_credentials ~account_id:_ ~credentials:_ on_success _on_error = on_success () in
  let update_health ~account_id:_ ~status ~error_message:_ on_success _on_error =
    statuses := !statuses @ [ status ];
    on_success ()
  in
  Social_refresh.Orchestrator.ensure_valid_access_token
    ~account_id:"acct"
    ~load_credentials
    ~perform_refresh
    ~persist_credentials
    ~update_health
    (fun _ -> result := Some (Ok ()))
    (fun err -> result := Some (Error err));
  (!statuses, !result)

(* A refresh that failed because the platform was unreachable, overloaded or
   rate limiting us says nothing about the stored credentials. The default
   mapping used to write [refresh_failed] for all of these, so a platform
   outage painted every account on that platform as needing reconnection while
   the scheduler was still retrying it with backoff. Consumers treat
   [refresh_failed] as terminal, so a blip disconnected accounts for good. *)
let test_orchestrator_non_credential_failures_write_no_health () =
  let cases =
    [
      ("connection refused",
       Error_types.Network_error (Error_types.Connection_failed "connection refused"));
      ("timeout", Error_types.Network_error Error_types.Timeout);
      ("upstream 503",
       Error_types.make_api_error ~platform:Platform_types.Twitter ~status_code:503
         ~message:"Service Unavailable" ());
      ("rate limited", Error_types.make_rate_limited ());
      ("unparsable refresh response",
       Error_types.Internal_error "Failed to parse refresh response");
    ]
  in
  List.iter
    (fun (label, err) ->
      let statuses, result = run_refresh_failure err in
      if statuses <> [] then
        failwith
          (Printf.sprintf "%s wrote health status(es): %s" label
             (String.concat ", " statuses));
      match result with
      | Some (Error observed) when observed = err -> ()
      | _ ->
          failwith (Printf.sprintf "%s did not surface the original error" label))
    cases;
  print_endline "✓ orchestrator leaves health alone on non-credential refresh failures"

(* The other half of the contract: a refresh the platform actually rejected is
   still recorded, and recorded as the status that error implies. *)
let test_orchestrator_credential_failures_still_write_health () =
  List.iter
    (fun (err, expected) ->
      let statuses, _ = run_refresh_failure err in
      if statuses <> [ expected ] then
        failwith
          (Printf.sprintf "expected [%s], got [%s]" expected
             (String.concat ", " statuses)))
    [
      (Error_types.Auth_error (Error_types.Refresh_failed "invalid refresh token"),
       "refresh_failed");
      (Error_types.Auth_error Error_types.Token_expired, "token_expired");
      (Error_types.Auth_error Error_types.Token_revoked, "token_revoked");
      (Error_types.Auth_error Error_types.Token_invalid, "token_revoked");
    ];
  print_endline "✓ orchestrator still records genuine credential failures"

(* A caller that supplies its own mapping keeps full control, including the
   ability to say "write nothing" for an error the default would record. *)
let test_orchestrator_custom_health_mapping_can_write_nothing () =
  let statuses = ref [] in
  let load_credentials ~account_id:_ on_success _on_error =
    on_success
      (make_credentials ~expires_at:(rfc3339_in_seconds 5) ~refresh_token:"stored" "expired")
  in
  let perform_refresh ~credentials:_ _on_success on_error =
    on_error (Error_types.Auth_error (Error_types.Refresh_failed "invalid refresh token"))
  in
  let persist_credentials ~account_id:_ ~credentials:_ on_success _on_error = on_success () in
  let update_health ~account_id:_ ~status ~error_message:_ on_success _on_error =
    statuses := status :: !statuses;
    on_success ()
  in
  Social_refresh.Orchestrator.ensure_valid_access_token
    ~account_id:"acct"
    ~map_refresh_error_to_health:(fun _ -> None)
    ~load_credentials
    ~perform_refresh
    ~persist_credentials
    ~update_health
    (fun _ -> ())
    (fun _ -> ());
  assert (!statuses = []);
  print_endline "✓ orchestrator honours a custom mapping that writes nothing"

let () =
  test_refresh_time_boundaries ();
  test_refresh_time_malformed_timestamp ();
  test_refresh_decision_engine ();
  test_orchestrator_missing_refresh_token ();
  test_orchestrator_refresh_failure_mapping ();
  test_orchestrator_preserves_refresh_token_when_missing_in_response ();
  test_orchestrator_preserves_expires_at_and_auth_type ();
  test_orchestrator_failure_keeps_root_error_when_health_update_fails ();
  test_orchestrator_health_transitions ();
  test_orchestrator_retries_transient_errors ();
  test_orchestrator_reloads_latest_credentials_before_refresh ();
  test_orchestrator_uses_account_lock_hook ();
  test_orchestrator_non_credential_failures_write_no_health ();
  test_orchestrator_credential_failures_still_write_health ();
  test_orchestrator_custom_health_mapping_can_write_nothing ();
  print_endline "social-refresh tests passed"
