(** Account-health contract tests for the Mastodon provider.

    [ensure_valid_token] used to write the literal ["invalid_token"] whenever
    [verify_credentials] failed, for any reason. Two things were wrong with
    that. The literal is not a status any consumer recognises, so it got
    coerced into whatever the consumer's default branch happened to be. And
    the write happened even when the instance never answered, so a Mastodon
    server being down for a minute marked the account as needing
    reconnection.

    These tests pin that only a real answer from the instance about the token
    moves account health, and that whatever gets written is a recognised
    status. *)

type verify_mode =
  | Verify_ok
  | Verify_transport_error   (** the instance could not be reached *)
  | Verify_server_error      (** the instance answered 502 *)
  | Verify_unauthorized      (** the instance answered 401: the token is dead *)

let verify_mode = ref Verify_ok

(** Every ~status handed to [update_health_status], oldest first. *)
let health_writes = ref []

let reset mode =
  verify_mode := mode;
  health_writes := []

module Mock_http : Social_core.HTTP_CLIENT = struct
  let get ?headers:_ url on_success on_error =
    if String.length url > 0
       && Filename.check_suffix url "/api/v1/accounts/verify_credentials"
    then
      match !verify_mode with
      | Verify_ok ->
          on_success
            {
              Social_core.status = 200;
              headers = [ ("content-type", "application/json") ];
              body = {|{"id":"1","username":"tester","acct":"tester"}|};
            }
      | Verify_transport_error -> on_error "connection reset by peer"
      | Verify_server_error ->
          on_success
            {
              Social_core.status = 502;
              headers = [];
              body = {|{"error":"Bad Gateway"}|};
            }
      | Verify_unauthorized ->
          on_success
            {
              Social_core.status = 401;
              headers = [];
              body = {|{"error":"The access token is invalid"}|};
            }
    else on_success { Social_core.status = 404; headers = []; body = "not found" }

  let post ?headers:_ ?body:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }

  let post_multipart ?headers:_ ~parts:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }

  let put ?headers:_ ?body:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }

  let delete ?headers:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }
end

module Mock_config = struct
  module Http = Mock_http

  let get_env _key = None

  let get_credentials ~account_id:_ on_success _on_error =
    on_success
      {
        Social_core.access_token =
          {|{"access_token":"tok","instance_url":"https://mastodon.example"}|};
        refresh_token = None;
        expires_at = None;
        auth_type = Social_core.Bearer;
        scope = None;
      }

  let update_credentials ~account_id:_ ~credentials:_ on_success _on_error = on_success ()
  let encrypt data on_success _on_error = on_success data
  let decrypt data on_success _on_error = on_success data
  let sleep ~seconds:_ on_success _on_error = on_success ()

  let update_health_status ~account_id:_ ~status ~error_message:_ on_success _on_error =
    health_writes := !health_writes @ [ status ];
    on_success ()
end

module Mastodon = Social_mastodon_v1.Make (Mock_config)

let failures = ref 0

let check name condition =
  if condition then Printf.printf "  ✓ %s\n" name
  else begin
    incr failures;
    Printf.printf "  ✗ %s\n" name
  end

let describe_writes () = String.concat ", " !health_writes

let run_ensure_valid_token () =
  let observed_error = ref None in
  let succeeded = ref false in
  Mastodon.ensure_valid_token ~account_id:"acct"
    (fun _creds -> succeeded := true)
    (fun err -> observed_error := Some err);
  (!succeeded, !observed_error)

let test_unreachable_instance_writes_no_health_status () =
  Printf.printf "Test: an unreachable instance does not touch account health\n";
  reset Verify_transport_error;
  let _, err = run_ensure_valid_token () in
  check
    (Printf.sprintf "no health status written (got: %s)" (describe_writes ()))
    (!health_writes = []);
  check "error surfaces as a network error"
    (match err with Some (Error_types.Network_error _) -> true | _ -> false);
  check "error does not classify as an auth failure"
    (match err with
     | Some e -> Error_types.classify_error e = Error_types.Transient_failure
     | None -> false)

let test_instance_server_error_writes_no_health_status () =
  Printf.printf "Test: a 502 from the instance does not touch account health\n";
  reset Verify_server_error;
  let _, err = run_ensure_valid_token () in
  check
    (Printf.sprintf "no health status written (got: %s)" (describe_writes ()))
    (!health_writes = []);
  check "error keeps its 5xx API shape"
    (match err with
     | Some (Error_types.Api_error { status_code = 502; _ }) -> true
     | _ -> false)

let test_rejected_token_marks_token_revoked () =
  Printf.printf "Test: a 401 from the instance marks the token revoked\n";
  reset Verify_unauthorized;
  let _, err = run_ensure_valid_token () in
  check
    (Printf.sprintf "wrote exactly token_revoked (got: %s)" (describe_writes ()))
    (!health_writes = [ Health_status.to_string Health_status.Token_revoked ]);
  check "error classifies as an auth failure"
    (match err with
     | Some e -> Error_types.classify_error e = Error_types.Auth_failure
     | None -> false)

let test_success_marks_healthy () =
  Printf.printf "Test: a verified token marks the account healthy\n";
  reset Verify_ok;
  let succeeded, _ = run_ensure_valid_token () in
  check "ensure_valid_token succeeded" succeeded;
  check
    (Printf.sprintf "wrote exactly healthy (got: %s)" (describe_writes ()))
    (!health_writes = [ Health_status.to_string Health_status.Healthy ])

(* The regression that started this: no code path may emit a literal that
   consumers cannot parse. *)
let test_every_written_status_is_recognised () =
  Printf.printf "Test: no code path emits an unrecognised status literal\n";
  List.iter
    (fun mode ->
      reset mode;
      let _ = run_ensure_valid_token () in
      List.iter
        (fun status ->
          check
            (Printf.sprintf "%S is a recognised status" status)
            (Health_status.of_string status <> None))
        !health_writes)
    [ Verify_ok; Verify_transport_error; Verify_server_error; Verify_unauthorized ]

let () =
  Printf.printf "Running Mastodon account-health contract tests\n\n";
  test_unreachable_instance_writes_no_health_status ();
  test_instance_server_error_writes_no_health_status ();
  test_rejected_token_marks_token_revoked ();
  test_success_marks_healthy ();
  test_every_written_status_is_recognised ();
  Printf.printf "\n";
  if !failures > 0 then begin
    Printf.printf "%d check(s) failed\n" !failures;
    exit 1
  end;
  Printf.printf "All Mastodon account-health contract tests passed\n"
