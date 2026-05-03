(** Focused error and rate-limit contract tests for Mastodon provider. *)

module Error_http = struct
  type mode =
    | Return_429
    | Return_500_with_token

  let current_mode = ref Return_429

  let set_mode mode = current_mode := mode

  let get ?headers:_ url on_success _on_error =
    if String.ends_with ~suffix:"/api/v1/accounts/verify_credentials" url then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body = {|{"id":"123","username":"tester"}|};
      }
    else
      on_success { Social_core.status = 404; headers = []; body = "not found" }

  let post ?headers:_ ?body:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }

  let post_multipart ?headers:_ ~parts:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }

  let put ?headers:_ ?body:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }

  let delete ?headers:_ _url on_success _on_error =
    match !current_mode with
    | Return_429 ->
        on_success {
          Social_core.status = 429;
          headers = [ ("Retry-After", "17") ];
          body = {|{"error":"rate limit exceeded"}|};
        }
    | Return_500_with_token ->
        on_success {
          Social_core.status = 500;
          headers = [];
          body = {|{"error":"boom","access_token":"super-secret-token"}|};
        }
end

module Error_config = struct
  module Http = Error_http

  let get_env _key = None

  let get_credentials ~account_id:_ on_success _on_error =
    on_success {
      Social_core.access_token =
        {|{"access_token":"error_token","instance_url":"https://mastodon.social"}|};
      refresh_token = None;
      expires_at = None;
      auth_type = Social_core.Bearer;
      scope = None;
    }

  let update_credentials ~account_id:_ ~credentials:_ on_success _on_error = on_success ()
  let encrypt _data on_success _on_error = on_success "encrypted"
  let decrypt _data on_success _on_error = on_success "decrypted"
  let sleep ~seconds:_ on_success _on_error = on_success ()
  let update_health_status ~account_id:_ ~status:_ ~error_message:_ on_success _on_error = on_success ()
end

module Mastodon = Social_mastodon_v1.Make (Error_config)

let string_contains s sub =
  try
    let _ = Str.search_forward (Str.regexp_string sub) s 0 in
    true
  with Not_found -> false

let test_retry_after_header_mapping () =
  Printf.printf "Test: 429 Retry-After header mapping... ";
  Error_http.set_mode Return_429;
  let got_result = ref false in
  Mastodon.delete_status
    ~account_id:"acct"
    ~status_id:"123"
    (fun () -> failwith "Expected rate-limit error")
    (fun err ->
      got_result := true;
      match err with
      | Error_types.Rate_limited { retry_after_seconds = Some 17; _ } -> ()
      | _ ->
          failwith (Printf.sprintf "Unexpected error mapping: %s" (Error_types.error_to_string err)));
  assert !got_result;
  Printf.printf "✓\n"

let test_error_redaction_in_api_error () =
  Printf.printf "Test: API error redaction for token-like fields... ";
  Error_http.set_mode Return_500_with_token;
  let got_result = ref false in
  let saw_api_error = ref false in
  let saw_no_secret_in_message = ref false in
  let saw_expected_message = ref false in
  let saw_no_secret_in_raw = ref false in
  let saw_redacted_in_raw = ref false in
  Mastodon.delete_status
    ~account_id:"acct"
    ~status_id:"123"
    (fun () -> ())
    (fun err ->
      got_result := true;
      match err with
      | Error_types.Api_error { message; raw_response; _ } ->
          saw_api_error := true;
          saw_no_secret_in_message := not (string_contains message "super-secret-token");
          saw_expected_message := String.equal message "boom" || string_contains message "[REDACTED]";
          (match raw_response with
          | Some body ->
              saw_no_secret_in_raw := not (string_contains body "super-secret-token");
              saw_redacted_in_raw := string_contains body "[REDACTED]"
          | None -> ())
      | _ -> ());
  assert !got_result;
  assert !saw_api_error;
  assert !saw_no_secret_in_message;
  assert !saw_expected_message;
  assert !saw_no_secret_in_raw;
  assert !saw_redacted_in_raw;
  Printf.printf "✓\n"

let () =
  Printf.printf "\n=== Mastodon Error Contract Tests ===\n\n";
  test_retry_after_header_mapping ();
  test_error_redaction_in_api_error ();
  Printf.printf "\n✓ Error contract tests passed\n"
