open Social_core
open Social_refresh

type test_store = {
  mutable creds : credentials option;
  mutable statuses : (string * string option) list;
  mutable refresh_calls : int;
  mutable refresh_response : Types.refreshed_token option;
  mutable refresh_error : string option;
}

let make_store () = {
  creds = None;
  statuses = [];
  refresh_calls = 0;
  refresh_response = None;
  refresh_error = None;
}

let callbacks_of_store store : Orchestrator.callbacks =
  let load_credentials ~account_id:_ on_success on_error =
    match store.creds with
    | Some creds -> on_success creds
    | None -> on_error "no credentials"
  in
  let perform_refresh ~credentials:_ ~refresh_token:_ on_success on_error =
    store.refresh_calls <- store.refresh_calls + 1;
    match store.refresh_error, store.refresh_response with
    | Some err, _ -> on_error err
    | None, Some refreshed -> on_success refreshed
    | None, None -> on_error "no refresh response configured"
  in
  let persist_credentials ~account_id:_ ~credentials on_success _on_error =
    store.creds <- Some credentials;
    on_success ()
  in
  let update_health ~account_id:_ ~status ~error_message on_success _on_error =
    store.statuses <- (status, error_message) :: store.statuses;
    on_success ()
  in
  {
    load_credentials;
    perform_refresh;
    persist_credentials;
    update_health;
  }

let rfc3339_in_seconds delta =
  let now = Ptime_clock.now () in
  match Ptime.add_span now (Ptime.Span.of_int_s delta) with
  | Some t -> Ptime.to_rfc3339 t
  | None -> Ptime.to_rfc3339 now

let test_time_boundaries () =
  assert (Time.is_expired_with_buffer ~buffer_seconds:1800 (Some (rfc3339_in_seconds 2000)) = false);
  assert (Time.is_expired_with_buffer ~buffer_seconds:1800 (Some (rfc3339_in_seconds 1800)) = true);
  assert (Time.is_expired_with_buffer ~buffer_seconds:1800 (Some (rfc3339_in_seconds 1000)) = true);
  print_endline "✓ Refresh time boundary checks"

let test_malformed_timestamp_refreshes () =
  assert (Time.is_expired_with_buffer ~buffer_seconds:10 (Some "not-a-timestamp") = true);
  print_endline "✓ Malformed timestamp is treated as expiring"

let test_missing_refresh_token () =
  let store = make_store () in
  store.creds <- Some {
    access_token = "expired";
    refresh_token = None;
    expires_at = Some (rfc3339_in_seconds 5);
    token_type = "Bearer";
  };
  let got_error = ref None in
  ensure_valid_token ~account_id:"acct" ~callbacks:(callbacks_of_store store)
    (fun _ -> failwith "Expected missing refresh token to fail")
    (fun err -> got_error := Some err);
  assert (store.refresh_calls = 0);
  assert (List.mem ("token_expired", Some "No refresh token available") store.statuses);
  assert (!got_error = Some Types.Missing_refresh_token);
  print_endline "✓ Missing refresh token path"

let test_refresh_failure_mapping () =
  let store = make_store () in
  store.creds <- Some {
    access_token = "expired";
    refresh_token = Some "refresh";
    expires_at = Some (rfc3339_in_seconds 5);
    token_type = "Bearer";
  };
  store.refresh_error <- Some "bad refresh";
  let got_social_error = ref None in
  ensure_valid_token ~account_id:"acct" ~callbacks:(callbacks_of_store store)
    (fun _ -> failwith "Expected refresh failure")
    (fun err -> got_social_error := Some (Types.to_social_error err));
  assert (store.refresh_calls = 1);
  assert (List.mem ("refresh_failed", Some "bad refresh") store.statuses);
  (match !got_social_error with
   | Some (Error_types.Auth_error (Error_types.Refresh_failed msg)) -> assert (msg = "bad refresh")
   | _ -> failwith "Expected Auth_error (Refresh_failed ...)" );
  print_endline "✓ Refresh failure mapping"

let test_refresh_token_preservation () =
  let store = make_store () in
  store.creds <- Some {
    access_token = "expired";
    refresh_token = Some "old_refresh";
    expires_at = Some (rfc3339_in_seconds 5);
    token_type = "Bearer";
  };
  store.refresh_response <- Some {
    access_token = "new_access";
    refresh_token = None;
    expires_at = Time.expires_at_from_now ~expires_in_seconds:3600;
    token_type = Some "Bearer";
  };

  ensure_valid_token ~account_id:"acct" ~callbacks:(callbacks_of_store store)
    (fun outcome ->
      match outcome with
      | Token_refreshed { refresh_token; access_token; _ } ->
          assert (access_token = "new_access");
          assert (refresh_token = Some "old_refresh")
      | Token_reused _ -> failwith "Expected token refresh")
    (fun _ -> failwith "Expected successful refresh");

  assert (List.mem ("healthy", None) store.statuses);
  (match store.creds with
   | Some creds ->
       assert (creds.access_token = "new_access");
       assert (creds.refresh_token = Some "old_refresh")
   | None -> failwith "Expected credentials to be persisted");
  print_endline "✓ Refresh token preservation"

let test_healthy_skip_path () =
  let store = make_store () in
  store.creds <- Some {
    access_token = "fresh_access";
    refresh_token = Some "refresh";
    expires_at = Some (rfc3339_in_seconds 5000);
    token_type = "Bearer";
  };
  ensure_valid_token
    ~policy:{ Types.default_policy with refresh_buffer_seconds = 300 }
    ~account_id:"acct"
    ~callbacks:(callbacks_of_store store)
    (fun outcome ->
      match outcome with
      | Token_reused token -> assert (token = "fresh_access")
      | Token_refreshed _ -> failwith "Expected skip path")
    (fun _ -> failwith "Expected skip path to succeed");
  assert (store.refresh_calls = 0);
  assert (List.mem ("healthy", None) store.statuses);
  print_endline "✓ Healthy skip path"

let () =
  test_time_boundaries ();
  test_malformed_timestamp_refreshes ();
  test_missing_refresh_token ();
  test_refresh_failure_mapping ();
  test_refresh_token_preservation ();
  test_healthy_skip_path ();
  print_endline "\nAll social-refresh tests passed!"
