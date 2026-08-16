(** Regression tests for {!Health_status}.

    These pin the two properties that make the account-health signal
    trustworthy for consumers:

    1. Every status a provider can emit round-trips through
       [to_string]/[of_string], so a consumer never has to guess at an
       unrecognised literal.
    2. Only errors that are actually evidence about the stored credentials
       produce a status to write. Network, rate-limit and generic API errors
       produce [None], meaning "leave the stored status alone". *)

let failures = ref 0

let check name condition =
  if condition then Printf.printf "  ✓ %s\n" name
  else begin
    incr failures;
    Printf.printf "  ✗ %s\n" name
  end

let all_statuses =
  [
    Health_status.Healthy;
    Health_status.Token_expired;
    Health_status.Token_revoked;
    Health_status.Refresh_failed;
    Health_status.User_unlinked;
  ]

let test_round_trip () =
  Printf.printf "Test: every status round-trips through to_string/of_string\n";
  List.iter
    (fun status ->
      let literal = Health_status.to_string status in
      check
        (Printf.sprintf "%s round-trips" literal)
        (Health_status.of_string literal = Some status))
    all_statuses

(* The literals below were all emitted by providers in this SDK at some point
   and none of them are recognised by consumers. Pinning them as unparseable
   documents why providers must go through [to_string]. *)
let test_historic_bad_literals_do_not_parse () =
  Printf.printf "Test: historic invalid literals do not parse\n";
  List.iter
    (fun literal ->
      check
        (Printf.sprintf "%S is not a valid status" literal)
        (Health_status.of_string literal = None))
    [ "invalid_token"; "token_recovered"; "token_recovery_failed"; ""; "HEALTHY" ]

let test_non_credential_errors_write_nothing () =
  Printf.printf "Test: errors that are not credential evidence produce None\n";
  let cases =
    [
      ("connection failed",
       Error_types.Network_error (Error_types.Connection_failed "econnrefused"));
      ("timeout", Error_types.Network_error Error_types.Timeout);
      ("dns failure", Error_types.Network_error Error_types.Dns_resolution_failed);
      ("ssl failure", Error_types.Network_error (Error_types.Ssl_error "bad cert"));
      ("rate limited",
       Error_types.Rate_limited
         { retry_after_seconds = Some 30; limit = None; remaining = None; reset_at = None });
      ("server 503",
       Error_types.make_api_error ~platform:Platform_types.Mastodon ~status_code:503
         ~message:"Service Unavailable" ());
      ("bad gateway",
       Error_types.make_api_error ~platform:Platform_types.Bluesky ~status_code:502
         ~message:"Bad Gateway" ());
      ("unparseable response", Error_types.Internal_error "json parse failure");
      ("duplicate content", Error_types.Duplicate_content);
      ("policy violation", Error_types.Content_policy_violation "spam");
      ("not found", Error_types.Resource_not_found "status-123");
      ("validation", Error_types.Validation_error [ Error_types.Text_empty ]);
    ]
  in
  List.iter
    (fun (name, err) -> check name (Health_status.of_error err = None))
    cases

let test_auth_errors_map_to_a_status () =
  Printf.printf "Test: authentication errors map to a credential status\n";
  let cases =
    [
      ("expired token", Error_types.Token_expired, Health_status.Token_expired);
      ("invalid token", Error_types.Token_invalid, Health_status.Token_revoked);
      ("revoked token", Error_types.Token_revoked, Health_status.Token_revoked);
      ("refresh failed", Error_types.Refresh_failed "bad grant",
       Health_status.Refresh_failed);
      ("missing credentials", Error_types.Missing_credentials,
       Health_status.Refresh_failed);
      ("missing scope",
       Error_types.Insufficient_permissions
         { required = [ "read:accounts" ]; platform_message = None },
       Health_status.Token_revoked);
    ]
  in
  List.iter
    (fun (name, auth, expected) ->
      check name (Health_status.of_error (Error_types.Auth_error auth) = Some expected))
    cases

let () =
  Printf.printf "Running Health_status tests\n\n";
  test_round_trip ();
  test_historic_bad_literals_do_not_parse ();
  test_non_credential_errors_write_nothing ();
  test_auth_errors_map_to_a_status ();
  Printf.printf "\n";
  if !failures > 0 then begin
    Printf.printf "%d check(s) failed\n" !failures;
    exit 1
  end;
  Printf.printf "All Health_status tests passed\n"
