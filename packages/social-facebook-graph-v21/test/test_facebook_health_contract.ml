(** Account-health contract tests for Facebook Page-token recovery.

    [recover_page_access_token] walks the candidate user tokens, and when it
    runs out it records what the failure means for the stored credentials. That
    branch used to hardcode [token_revoked] regardless of the error it was
    holding. It happened to agree with {!Health_status.of_error} for a plainly
    invalid token, but not for an expired one: Facebook error 190 with subcode
    463 is an expired session, which automatic refresh can still recover, and
    [token_revoked] is terminal.

    These tests pin the status against the error rather than against a
    constant. *)

type recovery_mode =
  | Me_accounts_expired_token  (** 190/463: the user token aged out *)
  | Me_accounts_invalid_token  (** 190, no subcode: the user token is dead *)
  | Me_accounts_unreachable  (** no HTTP response at all *)

let recovery_mode = ref Me_accounts_expired_token

(** Every ~status handed to [update_health_status], oldest first. *)
let health_writes = ref []

let reset mode =
  recovery_mode := mode;
  health_writes := []

let expired_token_body =
  {|{"error":{"message":"Error validating access token: Session has expired","type":"OAuthException","code":190,"error_subcode":463,"fbtrace_id":"Atrace"}}|}

let invalid_token_body =
  {|{"error":{"message":"Error validating access token: The user has not authorized application","type":"OAuthException","code":190,"fbtrace_id":"Atrace"}}|}

module Mock_http : Social_core.HTTP_CLIENT = struct
  let get ?headers:_ _url on_success on_error =
    match !recovery_mode with
    | Me_accounts_expired_token ->
        on_success { Social_core.status = 400; headers = []; body = expired_token_body }
    | Me_accounts_invalid_token ->
        on_success { Social_core.status = 400; headers = []; body = invalid_token_body }
    | Me_accounts_unreachable -> on_error "Connection refused"

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

  (* A stored user token in refresh_token, so the recovery loop has two
     candidates to exhaust rather than one. *)
  let get_credentials ~account_id:_ on_success _on_error =
    on_success
      {
        Social_core.access_token = "page-token-stale";
        refresh_token = Some "user-token-stale";
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

  let get_page_id ~account_id:_ on_success _on_error = on_success "page-123"
  let on_rate_limit_update _info = ()
end

module Facebook = Social_facebook_graph_v21.Make (Mock_config)

let failures = ref 0

let check name condition =
  if condition then Printf.printf "  ✓ %s\n" name
  else begin
    incr failures;
    Printf.printf "  ✗ %s\n" name
  end

let describe_writes () = String.concat ", " !health_writes

let run_recovery ~account_id =
  let observed_error = ref None in
  Facebook.recover_page_access_token ~account_id
    ~current_access_token:"page-token-stale"
    (fun _page_id _token -> failwith "expected recovery to fail")
    (fun err -> observed_error := Some err);
  match !observed_error with
  | Some err -> err
  | None -> failwith "recover_page_access_token called neither continuation"

let test_expired_user_token_records_token_expired () =
  Printf.printf "Test: exhausted recovery on an expired token records token_expired\n";
  reset Me_accounts_expired_token;
  let err = run_recovery ~account_id:"acct-expired" in
  check "the loop ended on an expired-token error"
    (err = Error_types.Auth_error Error_types.Token_expired);
  check
    (Printf.sprintf "wrote exactly token_expired (got: %s)" (describe_writes ()))
    (!health_writes = [ Health_status.to_string Health_status.Token_expired ]);
  check "every written status is a recognised one"
    (List.for_all (fun s -> Health_status.of_string s <> None) !health_writes)

let test_invalid_user_token_still_records_token_revoked () =
  Printf.printf "Test: exhausted recovery on an invalid token still records token_revoked\n";
  reset Me_accounts_invalid_token;
  let err = run_recovery ~account_id:"acct-invalid" in
  check "the loop ended on an invalid-token error"
    (err = Error_types.Auth_error Error_types.Token_invalid);
  check
    (Printf.sprintf "wrote exactly token_revoked (got: %s)" (describe_writes ()))
    (!health_writes = [ Health_status.to_string Health_status.Token_revoked ])

(* Regression: the /me/accounts transport failure used to be reported as
   Token_invalid, so an outage during recovery exhausted the candidates and
   recorded token_revoked on a healthy account. *)
let test_unreachable_graph_leaves_health_alone () =
  Printf.printf "Test: recovery against an unreachable Graph API writes no health status\n";
  reset Me_accounts_unreachable;
  let err = run_recovery ~account_id:"acct-outage" in
  check "the error is a network error"
    (match err with
     | Error_types.Network_error (Error_types.Connection_failed _) -> true
     | _ -> false);
  check
    (Printf.sprintf "wrote no health status (got: %s)" (describe_writes ()))
    (!health_writes = [])

let () =
  Printf.printf "Running Facebook account-health contract tests\n\n";
  test_expired_user_token_records_token_expired ();
  test_invalid_user_token_still_records_token_revoked ();
  test_unreachable_graph_leaves_health_alone ();
  Printf.printf "\n";
  if !failures > 0 then begin
    Printf.printf "%d check(s) failed\n" !failures;
    exit 1
  end;
  Printf.printf "All Facebook account-health contract tests passed\n"
