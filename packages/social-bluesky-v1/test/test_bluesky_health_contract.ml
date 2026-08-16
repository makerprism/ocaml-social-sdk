(** Account-health contract tests for the Bluesky provider.

    [ensure_valid_token] used to funnel every non-auth refresh failure through
    an [on_other_refresh_failure] helper that wrote [refresh_failed] and
    rewrapped the error as [Auth_error (Refresh_failed _)]. A PDS that was
    briefly unreachable therefore looked exactly like a revoked session: the
    account got a credential-failure status, and consumers routing on
    [Error_types.classify_error] saw [Auth_failure].

    These tests pin both halves of the fix. *)

type refresh_mode =
  | Refresh_transport_error  (** the PDS could not be reached at all *)
  | Refresh_server_error     (** the PDS answered 503 *)
  | Refresh_unauthorized     (** the PDS answered 401: the session is dead *)

let refresh_mode = ref Refresh_transport_error

(** Every ~status handed to [update_health_status], oldest first. *)
let health_writes = ref []

let reset mode =
  refresh_mode := mode;
  health_writes := []

module Mock_http : Social_core.HTTP_CLIENT = struct
  let get ?headers:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }

  let post ?headers:_ ?body:_ url on_success on_error =
    if String.length url >= 1 && Filename.check_suffix url "com.atproto.server.refreshSession"
    then
      match !refresh_mode with
      | Refresh_transport_error -> on_error "connection refused"
      | Refresh_server_error ->
          on_success
            {
              Social_core.status = 503;
              headers = [];
              body = {|{"error":"UpstreamFailure"}|};
            }
      | Refresh_unauthorized ->
          on_success
            {
              Social_core.status = 401;
              headers = [];
              body = {|{"error":"ExpiredToken"}|};
            }
    else on_success { Social_core.status = 200; headers = []; body = "{}" }

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

  (* A refresh_jwt (starts with "eyJ") so ensure_valid_token takes the
     refresh_session path rather than the legacy app-password path. *)
  let get_credentials ~account_id:_ on_success _on_error =
    on_success
      {
        Social_core.access_token = "tester.bsky.social";
        refresh_token = Some "eyJhbGciOiJIUzI1NiJ9.refresh";
        expires_at = None;
        auth_type = Social_core.Bearer;
        scope = None;
      }

  let update_credentials ~account_id:_ ~credentials:_ on_success _on_error = on_success ()
  let encrypt data on_success _on_error = on_success data
  let decrypt data on_success _on_error = on_success data

  let update_health_status ~account_id:_ ~status ~error_message:_ on_success _on_error =
    health_writes := !health_writes @ [ status ];
    on_success ()

  let resize_image ~data:_ ~mime_type:_ ~max_bytes:_ on_result = on_result None
end

module Bluesky = Social_bluesky_v1.Make (Mock_config)

let failures = ref 0

let check name condition =
  if condition then Printf.printf "  ✓ %s\n" name
  else begin
    incr failures;
    Printf.printf "  ✗ %s\n" name
  end

let describe_writes () = String.concat ", " !health_writes

(* Each test uses a fresh account_id: the provider caches sessions in a
   module-level table keyed by account_id. *)
let run_ensure_valid_token ~account_id =
  let observed_error = ref None in
  Bluesky.ensure_valid_token ~account_id
    (fun _jwt -> failwith "expected ensure_valid_token to fail")
    (fun err -> observed_error := Some err);
  match !observed_error with
  | Some err -> err
  | None -> failwith "ensure_valid_token called neither continuation"

let test_unreachable_pds_writes_no_health_status () =
  Printf.printf "Test: an unreachable PDS does not touch account health\n";
  reset Refresh_transport_error;
  let err = run_ensure_valid_token ~account_id:"acct-transport" in
  check
    (Printf.sprintf "no health status written (got: %s)" (describe_writes ()))
    (!health_writes = []);
  check "error surfaces as a network error"
    (match err with Error_types.Network_error _ -> true | _ -> false);
  check "error does not classify as an auth failure"
    (Error_types.classify_error err = Error_types.Transient_failure)

let test_pds_server_error_writes_no_health_status () =
  Printf.printf "Test: a 503 from the PDS does not touch account health\n";
  reset Refresh_server_error;
  let err = run_ensure_valid_token ~account_id:"acct-503" in
  check
    (Printf.sprintf "no health status written (got: %s)" (describe_writes ()))
    (!health_writes = []);
  check "error keeps its 5xx API shape"
    (match err with
     | Error_types.Api_error { status_code = 503; _ } -> true
     | _ -> false);
  check "error does not classify as an auth failure"
    (Error_types.classify_error err = Error_types.Transient_failure)

let test_unauthorized_refresh_marks_token_revoked () =
  Printf.printf "Test: a 401 from the PDS marks the session revoked\n";
  reset Refresh_unauthorized;
  let err = run_ensure_valid_token ~account_id:"acct-401" in
  check
    (Printf.sprintf "wrote exactly token_revoked (got: %s)" (describe_writes ()))
    (!health_writes = [ Health_status.to_string Health_status.Token_revoked ]);
  check "every written status is a recognised one"
    (List.for_all
       (fun s -> Health_status.of_string s <> None)
       !health_writes);
  check "error classifies as an auth failure"
    (Error_types.classify_error err = Error_types.Auth_failure)

let () =
  Printf.printf "Running Bluesky account-health contract tests\n\n";
  test_unreachable_pds_writes_no_health_status ();
  test_pds_server_error_writes_no_health_status ();
  test_unauthorized_refresh_marks_token_revoked ();
  Printf.printf "\n";
  if !failures > 0 then begin
    Printf.printf "%d check(s) failed\n" !failures;
    exit 1
  end;
  Printf.printf "All Bluesky account-health contract tests passed\n"
