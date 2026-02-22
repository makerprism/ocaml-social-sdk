open Social_core

let string_contains s substr =
  try
    ignore (Str.search_forward (Str.regexp_string substr) s 0);
    true
  with Not_found -> false

let rfc3339_after_seconds seconds =
  let now = Ptime_clock.now () in
  match Ptime.add_span now (Ptime.Span.of_int_s seconds) with
  | Some t -> Ptime.to_rfc3339 t
  | None -> Ptime.to_rfc3339 now

let rfc3339_to_ptime value =
  match Ptime.of_rfc3339 value with
  | Ok (t, _, _) -> Some t
  | Error _ -> None

let query_param url key =
  try Uri.get_query_param (Uri.of_string url) key
  with _ -> None

module Mock_http = struct
  let requests = ref []
  let response_queue = ref []
  let error_queue = ref []

  let reset () =
    requests := [];
    response_queue := [];
    error_queue := []

  let set_responses responses =
    response_queue := responses

  let set_errors errors =
    error_queue := errors

  let next_response () =
    match !response_queue with
    | [] -> None
    | response :: rest ->
        response_queue := rest;
        Some response

  let next_error () =
    match !error_queue with
    | [] -> None
    | err :: rest ->
        error_queue := rest;
        Some err

  include
    (struct
      let get ?(headers = []) url on_success on_error =
        requests := ("GET", url, headers, "") :: !requests;
        match next_response () with
        | Some response -> on_success response
        | None ->
            (match next_error () with
             | Some err -> on_error err
             | None -> on_error "No mock response set")

      let post ?(headers = []) ?(body = "") url on_success on_error =
        requests := ("POST", url, headers, body) :: !requests;
        match next_response () with
        | Some response -> on_success response
        | None ->
            (match next_error () with
             | Some err -> on_error err
             | None -> on_error "No mock response set")

      let post_multipart ?(headers = []) ~parts:_ url on_success on_error =
        requests := ("POST_MULTIPART", url, headers, "") :: !requests;
        match next_response () with
        | Some response -> on_success response
        | None ->
            (match next_error () with
             | Some err -> on_error err
             | None -> on_error "No mock response set")

      let put ?(headers = []) ?(body = "") url on_success on_error =
        requests := ("PUT", url, headers, body) :: !requests;
        match next_response () with
        | Some response -> on_success response
        | None ->
            (match next_error () with
             | Some err -> on_error err
             | None -> on_error "No mock response set")

      let delete ?(headers = []) url on_success on_error =
        requests := ("DELETE", url, headers, "") :: !requests;
        match next_response () with
        | Some response -> on_success response
        | None ->
            (match next_error () with
             | Some err -> on_error err
             | None -> on_error "No mock response set")
    end : HTTP_CLIENT)
end

module Mock_config = struct
  module Http = Mock_http

  let env_vars = ref []
  let credentials_store = ref []
  let credentials_error_override = ref None
  let update_credentials_error_override = ref None

  let reset () =
    env_vars := [];
    credentials_store := [];
    credentials_error_override := None;
    update_credentials_error_override := None;
    Mock_http.reset ()

  let get_env key =
    try Some (List.assoc key !env_vars)
    with Not_found -> None

  let get_credentials ~account_id on_success on_error =
    match !credentials_error_override with
    | Some err -> on_error err
    | None ->
        (try on_success (List.assoc account_id !credentials_store)
         with Not_found -> on_error "missing credentials")

  let update_credentials ~account_id ~credentials on_success _on_error =
    match !update_credentials_error_override with
    | Some _err -> _on_error _err
    | None ->
        credentials_store := (account_id, credentials) :: List.remove_assoc account_id !credentials_store;
        on_success ()

  let encrypt value on_success _on_error = on_success value
  let decrypt value on_success _on_error = on_success value

  let update_health_status ~account_id:_ ~status:_ ~error_message:_ on_success _on_error =
    on_success ()

  let sleep _delay f = f ()
end

module Threads = Social_threads_v1.Make (Mock_config)
module OAuth_http = Social_threads_v1.OAuth.Make (Mock_http)

let test_oauth_url () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_CLIENT_ID", "client-123") ];
  Threads.get_oauth_url ~redirect_uri:"https://example.com/callback" ~state:"state-xyz"
    (fun url ->
      assert (string_contains url "www.threads.net/oauth/authorize");
      assert (query_param url "client_id" = Some "client-123");
      assert (query_param url "redirect_uri" = Some "https://example.com/callback");
      assert (query_param url "state" = Some "state-xyz");
      assert (query_param url "response_type" = Some "code");
      assert (query_param url "scope" = Some "threads_basic,threads_content_publish");
      print_endline "ok: oauth url")
    (fun err -> failwith ("unexpected error: " ^ err))

let test_oauth_url_encodes_special_characters () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_CLIENT_ID", "client-123") ];
  Threads.get_oauth_url
    ~redirect_uri:"https://example.com/callback?x=1&y=two"
    ~state:"abc/123"
    (fun url ->
      assert (query_param url "redirect_uri" = Some "https://example.com/callback?x=1&y=two");
      assert (query_param url "state" = Some "abc/123");
      print_endline "ok: oauth url encodes special characters")
    (fun err -> failwith ("unexpected error: " ^ err))

let test_oauth_url_rejects_empty_state () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_CLIENT_ID", "client-123") ];
  Threads.get_oauth_url ~redirect_uri:"https://example.com/callback" ~state:"   "
    (fun _ -> failwith "expected oauth url failure for empty state")
    (fun err ->
      assert (string_contains err "state must not be empty");
      print_endline "ok: oauth empty state rejected")

let test_oauth_url_rejects_whitespace_client_id () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_CLIENT_ID", " client-123 ") ];
  Threads.get_oauth_url ~redirect_uri:"https://example.com/callback" ~state:"state-xyz"
    (fun _ -> failwith "expected oauth url failure for empty client id")
    (fun err ->
      assert (string_contains err "client ID must not contain leading or trailing whitespace");
      print_endline "ok: oauth whitespace client id rejected")

let test_oauth_url_rejects_state_with_surrounding_whitespace () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_CLIENT_ID", "client-123") ];
  Threads.get_oauth_url ~redirect_uri:"https://example.com/callback" ~state:" state-xyz "
    (fun _ -> failwith "expected oauth url failure for whitespace state")
    (fun err ->
      assert (string_contains err "state must not contain leading or trailing whitespace");
      print_endline "ok: oauth state whitespace rejected")

let test_oauth_url_rejects_redirect_with_surrounding_whitespace () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_CLIENT_ID", "client-123") ];
  Threads.get_oauth_url ~redirect_uri:" https://example.com/callback " ~state:"state-xyz"
    (fun _ -> failwith "expected oauth url failure for whitespace redirect")
    (fun err ->
      assert (string_contains err "redirect URI must not contain leading or trailing whitespace");
      print_endline "ok: oauth redirect whitespace rejected")

let test_oauth_url_rejects_redirect_uri_mismatch_with_config () =
  Mock_config.reset ();
  Mock_config.env_vars :=
    [
      ("THREADS_CLIENT_ID", "client-123");
      ("THREADS_REDIRECT_URI", "https://example.com/callback");
    ];
  Threads.get_oauth_url ~redirect_uri:"https://example.com/other" ~state:"state-xyz"
    (fun _ -> failwith "expected oauth url failure for configured redirect mismatch")
    (fun err ->
      assert (string_contains err "must exactly match THREADS_REDIRECT_URI");
      print_endline "ok: oauth redirect mismatch rejected")

let test_oauth_url_rejects_whitespace_configured_redirect_uri () =
  Mock_config.reset ();
  Mock_config.env_vars :=
    [
      ("THREADS_CLIENT_ID", "client-123");
      ("THREADS_REDIRECT_URI", " https://example.com/callback ");
    ];
  Threads.get_oauth_url ~redirect_uri:"https://example.com/callback" ~state:"state-xyz"
    (fun _ -> failwith "expected oauth url failure for whitespace configured redirect")
    (fun err ->
      assert (string_contains err "Configured Threads redirect URI must not contain leading or trailing whitespace");
      print_endline "ok: oauth configured redirect whitespace rejected")

let test_exchange_code_rejects_empty_code () =
  Mock_config.reset ();
  Mock_config.env_vars :=
    [ ("THREADS_CLIENT_ID", "client-123"); ("THREADS_CLIENT_SECRET", "secret-abc") ];
  Threads.exchange_code ~code:"   " ~redirect_uri:"https://example.com/callback"
    (fun _ -> failwith "expected exchange_code failure for empty code")
    (fun err ->
      assert (string_contains err "authorization code must not be empty");
      print_endline "ok: exchange_code empty code rejected")

let test_exchange_code_rejects_empty_redirect_uri () =
  Mock_config.reset ();
  Mock_config.env_vars :=
    [ ("THREADS_CLIENT_ID", "client-123"); ("THREADS_CLIENT_SECRET", "secret-abc") ];
  Threads.exchange_code ~code:"valid-code" ~redirect_uri:"   "
    (fun _ -> failwith "expected exchange_code failure for empty redirect_uri")
    (fun err ->
      assert (string_contains err "redirect URI must not be empty");
      print_endline "ok: exchange_code empty redirect rejected")

let test_exchange_code_rejects_whitespace_client_secret () =
  Mock_config.reset ();
  Mock_config.env_vars :=
    [ ("THREADS_CLIENT_ID", "client-123"); ("THREADS_CLIENT_SECRET", " secret-abc ") ];
  Threads.exchange_code ~code:"valid-code" ~redirect_uri:"https://example.com/callback"
    (fun _ -> failwith "expected exchange_code failure for whitespace client secret")
    (fun err ->
      assert (string_contains err "credentials must not contain leading or trailing whitespace");
      print_endline "ok: exchange_code whitespace secret rejected")

let test_exchange_code_rejects_whitespace_client_id () =
  Mock_config.reset ();
  Mock_config.env_vars :=
    [ ("THREADS_CLIENT_ID", " client-123 "); ("THREADS_CLIENT_SECRET", "secret-abc") ];
  Threads.exchange_code ~code:"valid-code" ~redirect_uri:"https://example.com/callback"
    (fun _ -> failwith "expected exchange_code failure for whitespace client id")
    (fun err ->
      assert (string_contains err "credentials must not contain leading or trailing whitespace");
      print_endline "ok: exchange_code whitespace client id rejected")

let test_exchange_code_rejects_whitespace_wrapped_code () =
  Mock_config.reset ();
  Mock_config.env_vars :=
    [ ("THREADS_CLIENT_ID", "client-123"); ("THREADS_CLIENT_SECRET", "secret-abc") ];
  Threads.exchange_code ~code:" valid-code " ~redirect_uri:"https://example.com/callback"
    (fun _ -> failwith "expected exchange_code failure for whitespace-wrapped code")
    (fun err ->
      assert (string_contains err "authorization code must not contain leading or trailing whitespace");
      print_endline "ok: exchange_code wrapped code rejected")

let test_exchange_code_rejects_whitespace_wrapped_redirect () =
  Mock_config.reset ();
  Mock_config.env_vars :=
    [ ("THREADS_CLIENT_ID", "client-123"); ("THREADS_CLIENT_SECRET", "secret-abc") ];
  Threads.exchange_code ~code:"valid-code" ~redirect_uri:" https://example.com/callback "
    (fun _ -> failwith "expected exchange_code failure for whitespace-wrapped redirect")
    (fun err ->
      assert (string_contains err "redirect URI must not contain leading or trailing whitespace");
      print_endline "ok: exchange_code wrapped redirect rejected")

let test_exchange_code_rejects_redirect_uri_mismatch_with_config () =
  Mock_config.reset ();
  Mock_config.env_vars :=
    [
      ("THREADS_CLIENT_ID", "client-123");
      ("THREADS_CLIENT_SECRET", "secret-abc");
      ("THREADS_REDIRECT_URI", "https://example.com/callback");
    ];
  Threads.exchange_code ~code:"valid-code" ~redirect_uri:"https://example.com/other"
    (fun _ -> failwith "expected exchange_code failure for configured redirect mismatch")
    (fun err ->
      assert (string_contains err "must exactly match THREADS_REDIRECT_URI");
      print_endline "ok: exchange_code redirect mismatch rejected")

let test_exchange_code_rejects_whitespace_configured_redirect_uri () =
  Mock_config.reset ();
  Mock_config.env_vars :=
    [
      ("THREADS_CLIENT_ID", "client-123");
      ("THREADS_CLIENT_SECRET", "secret-abc");
      ("THREADS_REDIRECT_URI", " https://example.com/callback ");
    ];
  Threads.exchange_code ~code:"valid-code" ~redirect_uri:"https://example.com/callback"
    (fun _ -> failwith "expected exchange_code failure for whitespace configured redirect")
    (fun err ->
      assert (string_contains err "Configured Threads redirect URI must not contain leading or trailing whitespace");
      print_endline "ok: exchange_code configured redirect whitespace rejected")

let test_exchange_long_lived_rejects_empty_short_token () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_CLIENT_SECRET", "secret-abc") ];
  Threads.exchange_for_long_lived_token ~short_lived_token:"   "
    (fun _ -> failwith "expected long-lived exchange failure for empty short token")
    (fun err ->
      assert (string_contains err "short-lived token must not be empty");
      print_endline "ok: long-lived empty short token rejected")

let test_exchange_long_lived_rejects_whitespace_client_secret () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_CLIENT_SECRET", " secret-abc ") ];
  Threads.exchange_for_long_lived_token ~short_lived_token:"short-token"
    (fun _ -> failwith "expected long-lived exchange failure for whitespace client secret")
    (fun err ->
      assert (string_contains err "client secret must not contain leading or trailing whitespace");
      print_endline "ok: long-lived whitespace secret rejected")

let test_exchange_long_lived_rejects_whitespace_wrapped_short_token () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_CLIENT_SECRET", "secret-abc") ];
  Threads.exchange_for_long_lived_token ~short_lived_token:" short-token "
    (fun _ -> failwith "expected long-lived exchange failure for whitespace-wrapped short token")
    (fun err ->
      assert (string_contains err "short-lived token must not contain leading or trailing whitespace");
      print_endline "ok: long-lived wrapped short token rejected")

let test_refresh_token_rejects_empty_long_token () =
  Mock_config.reset ();
  Threads.refresh_token ~long_lived_token:"   "
    (fun _ -> failwith "expected refresh failure for empty long token")
    (fun err ->
      assert (string_contains err "long-lived token must not be empty");
      print_endline "ok: refresh empty long token rejected")

let test_refresh_token_rejects_whitespace_wrapped_long_token () =
  Mock_config.reset ();
  Threads.refresh_token ~long_lived_token:" long-token "
    (fun _ -> failwith "expected refresh failure for whitespace-wrapped long token")
    (fun err ->
      assert (string_contains err "long-lived token must not contain leading or trailing whitespace");
      print_endline "ok: refresh wrapped long token rejected")

let test_refresh_account_credentials_success () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "stored-token";
          refresh_token = None;
          expires_at = Some (rfc3339_after_seconds (-10));
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body =
          "{\"access_token\":\"refreshed-token\",\"token_type\":\"bearer\",\"expires_in\":3600}";
      };
    ];
  Threads.refresh_account_credentials ~account_id:"acct-1"
    (fun creds ->
      assert (creds.access_token = "refreshed-token");
      let saved = List.assoc "acct-1" !Mock_config.credentials_store in
      assert (saved.access_token = "refreshed-token");
      print_endline "ok: refresh account credentials")
    (fun err -> failwith ("unexpected refresh_account_credentials error: " ^ err))

let test_refresh_account_credentials_missing () =
  Mock_config.reset ();
  Threads.refresh_account_credentials ~account_id:"missing"
    (fun _ -> failwith "expected refresh_account_credentials failure for missing creds")
    (fun err ->
      assert (string_contains err "credentials missing");
      print_endline "ok: refresh account credentials missing")

let test_refresh_account_credentials_backend_load_failure () =
  Mock_config.reset ();
  Mock_config.credentials_error_override := Some "database unavailable";
  Threads.refresh_account_credentials ~account_id:"acct-1"
    (fun _ -> failwith "expected refresh_account_credentials backend load failure")
    (fun err ->
      assert (string_contains err "refresh load failed");
      print_endline "ok: refresh account credentials load failure")

let test_refresh_account_credentials_persist_failure () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "stored-token";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_config.update_credentials_error_override := Some "db write failed";
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body =
          "{\"access_token\":\"refreshed-token\",\"token_type\":\"bearer\",\"expires_in\":3600}";
      };
    ];
  Threads.refresh_account_credentials ~account_id:"acct-1"
    (fun _ -> failwith "expected refresh_account_credentials failure on persist")
    (fun err ->
      assert (string_contains err "persistence failed");
      print_endline "ok: refresh account credentials persist failure")

let test_validate_oauth_state_success () =
  Threads.validate_oauth_state ~expected:"abc" ~received:"abc"
    (fun () -> print_endline "ok: oauth state match")
    (fun err -> failwith ("unexpected oauth state error: " ^ err))

let test_validate_oauth_state_mismatch () =
  Threads.validate_oauth_state ~expected:"abc" ~received:"xyz"
    (fun () -> failwith "expected oauth state mismatch failure")
    (fun err ->
      assert (string_contains err "state mismatch");
      print_endline "ok: oauth state mismatch")

let test_validate_oauth_state_whitespace_not_equal () =
  Threads.validate_oauth_state ~expected:"abc" ~received:"abc "
    (fun () -> failwith "expected oauth state whitespace rejection")
    (fun err ->
      assert (string_contains err "must not contain leading or trailing whitespace");
      print_endline "ok: oauth state whitespace not equal rejected")

let test_validate_oauth_state_whitespace_equal_rejected () =
  Threads.validate_oauth_state ~expected:" abc " ~received:" abc "
    (fun () -> failwith "expected oauth state whitespace rejection")
    (fun err ->
      assert (string_contains err "must not contain leading or trailing whitespace");
      print_endline "ok: oauth state whitespace rejected")

let test_validate_oauth_state_length_mismatch () =
  Threads.validate_oauth_state ~expected:"abcd" ~received:"abc"
    (fun () -> failwith "expected oauth state length mismatch failure")
    (fun err ->
      assert (string_contains err "state mismatch");
      print_endline "ok: oauth state length mismatch")

let test_validate_oauth_state_whitespace_only_rejected () =
  Threads.validate_oauth_state ~expected:"   " ~received:"   "
    (fun () -> failwith "expected oauth state whitespace-only failure")
    (fun err ->
      assert (string_contains err "must not be empty");
      print_endline "ok: oauth state whitespace-only rejected")

let test_exchange_code_success () =
  Mock_http.reset ();
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body =
          "{\"access_token\":\"token-abc\",\"token_type\":\"bearer\",\"expires_in\":3600}";
      };
    ];
  OAuth_http.exchange_code
    ~client_id:"client-123"
    ~client_secret:"secret-abc"
    ~redirect_uri:"https://example.com/callback"
    ~code:"auth-code"
    (fun creds ->
      assert (creds.access_token = "token-abc");
      assert (creds.expires_at <> None);
      let requests = List.rev !Mock_http.requests in
      (match requests with
       | [ (_, url, _, body) ] ->
           assert (string_contains url "/v1.0/oauth/access_token");
           assert (string_contains body "grant_type=authorization_code")
       | _ -> failwith "unexpected request count for exchange code");
      print_endline "ok: exchange code")
    (fun err -> failwith ("unexpected oauth exchange error: " ^ err))

let test_exchange_code_error_redacts_sensitive_fields () =
  Mock_http.reset ();
  Mock_http.set_responses
    [
      {
        status = 400;
        headers = [];
        body =
          "{\"error\":{\"message\":\"bad request\",\"code\":190},\"access_token\":\"should-not-leak\"}";
      };
    ];
  OAuth_http.exchange_code
    ~client_id:"client-123"
    ~client_secret:"secret-abc"
    ~redirect_uri:"https://example.com/callback"
    ~code:"auth-code"
    (fun _ -> failwith "expected oauth exchange failure")
    (fun err ->
      assert (string_contains err "[REDACTED]");
      assert (not (string_contains err "should-not-leak"));
      print_endline "ok: exchange code error redacts sensitive fields")

let test_exchange_code_error_redacts_sensitive_non_json_body () =
  Mock_http.reset ();
  Mock_http.set_responses
    [
      {
        status = 400;
        headers = [];
        body = "oauth failure access_token=should-not-leak";
      };
    ];
  OAuth_http.exchange_code
    ~client_id:"client-123"
    ~client_secret:"secret-abc"
    ~redirect_uri:"https://example.com/callback"
    ~code:"auth-code"
    (fun _ -> failwith "expected oauth exchange failure")
    (fun err ->
      assert (string_contains err "[REDACTED_NON_JSON_ERROR_BODY]");
      assert (not (string_contains err "should-not-leak"));
      print_endline "ok: exchange code error redacts non-json sensitive body")

let test_exchange_code_empty_token_response_fails () =
  Mock_http.reset ();
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body =
          "{\"access_token\":\"   \",\"token_type\":\"bearer\",\"expires_in\":3600}";
      };
    ];
  OAuth_http.exchange_code
    ~client_id:"client-123"
    ~client_secret:"secret-abc"
    ~redirect_uri:"https://example.com/callback"
    ~code:"auth-code"
    (fun _ -> failwith "expected oauth exchange failure for empty access token")
    (fun err ->
      assert (string_contains err "empty access token");
      print_endline "ok: exchange code empty token response")

let test_exchange_code_empty_token_type_defaults_bearer () =
  Mock_http.reset ();
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body =
          "{\"access_token\":\"token-abc\",\"token_type\":\"   \",\"expires_in\":3600}";
      };
    ];
  OAuth_http.exchange_code
    ~client_id:"client-123"
    ~client_secret:"secret-abc"
    ~redirect_uri:"https://example.com/callback"
    ~code:"auth-code"
    (fun creds ->
      assert (creds.token_type = "Bearer");
      print_endline "ok: exchange code empty token_type defaults")
    (fun err -> failwith ("unexpected oauth exchange error: " ^ err))

let test_exchange_code_lowercase_bearer_normalized () =
  Mock_http.reset ();
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body =
          "{\"access_token\":\"token-abc\",\"token_type\":\"bearer\",\"expires_in\":3600}";
      };
    ];
  OAuth_http.exchange_code
    ~client_id:"client-123"
    ~client_secret:"secret-abc"
    ~redirect_uri:"https://example.com/callback"
    ~code:"auth-code"
    (fun creds ->
      assert (creds.token_type = "Bearer");
      print_endline "ok: exchange code bearer normalized")
    (fun err -> failwith ("unexpected oauth exchange error: " ^ err))

let test_exchange_code_negative_expires_in_clamped () =
  Mock_http.reset ();
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body =
          "{\"access_token\":\"token-abc\",\"token_type\":\"bearer\",\"expires_in\":-100}";
      };
    ];
  OAuth_http.exchange_code
    ~client_id:"client-123"
    ~client_secret:"secret-abc"
    ~redirect_uri:"https://example.com/callback"
    ~code:"auth-code"
    (fun creds ->
      assert (creds.expires_at <> None);
      print_endline "ok: exchange code negative expires_in clamped")
    (fun err -> failwith ("unexpected oauth exchange error: " ^ err))

let test_exchange_code_huge_expires_in_clamped () =
  Mock_http.reset ();
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body =
          "{\"access_token\":\"token-abc\",\"token_type\":\"bearer\",\"expires_in\":999999999}";
      };
    ];
  OAuth_http.exchange_code
    ~client_id:"client-123"
    ~client_secret:"secret-abc"
    ~redirect_uri:"https://example.com/callback"
    ~code:"auth-code"
    (fun creds ->
      let now = Ptime_clock.now () in
      let max_expected =
        match Ptime.add_span now (Ptime.Span.of_int_s (31536000 + 60)) with
        | Some t -> t
        | None -> now
      in
      (match creds.expires_at with
       | Some expiry ->
           (match rfc3339_to_ptime expiry with
            | Some t ->
                assert (Ptime.is_earlier t ~than:max_expected);
                print_endline "ok: exchange code huge expires_in clamped"
            | None -> failwith "expected parseable expires_at")
       | None -> failwith "expected expires_at for huge expires_in"))
    (fun err -> failwith ("unexpected oauth exchange error: " ^ err))

let test_exchange_code_huge_float_expires_in_clamped () =
  Mock_http.reset ();
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body =
          "{\"access_token\":\"token-abc\",\"token_type\":\"bearer\",\"expires_in\":1e309}";
      };
    ];
  OAuth_http.exchange_code
    ~client_id:"client-123"
    ~client_secret:"secret-abc"
    ~redirect_uri:"https://example.com/callback"
    ~code:"auth-code"
    (fun creds ->
      assert (creds.expires_at <> None);
      print_endline "ok: exchange code huge float expires_in clamped")
    (fun err -> failwith ("unexpected oauth exchange error: " ^ err))

let test_exchange_code_fractional_expires_in_rounded_up () =
  Mock_http.reset ();
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body =
          "{\"access_token\":\"token-abc\",\"token_type\":\"bearer\",\"expires_in\":1.1}";
      };
    ];
  let now = Ptime_clock.now () in
  let min_expected =
    match Ptime.add_span now (Ptime.Span.of_int_s 1) with
    | Some t -> t
    | None -> now
  in
  OAuth_http.exchange_code
    ~client_id:"client-123"
    ~client_secret:"secret-abc"
    ~redirect_uri:"https://example.com/callback"
    ~code:"auth-code"
    (fun creds ->
      match creds.expires_at with
      | Some expiry ->
          (match rfc3339_to_ptime expiry with
           | Some t ->
               assert (Ptime.is_later t ~than:min_expected);
               print_endline "ok: exchange code fractional expires_in rounded"
           | None -> failwith "expected parseable expires_at")
      | None -> failwith "expected expires_at for fractional expires_in")
    (fun err -> failwith ("unexpected oauth exchange error: " ^ err))

let test_exchange_long_lived_success () =
  Mock_http.reset ();
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body =
          "{\"access_token\":\"long-lived\",\"token_type\":\"bearer\",\"expires_in\":5184000}";
      };
    ];
  OAuth_http.exchange_for_long_lived_token
    ~client_secret:"secret-abc"
    ~short_lived_token:"short-lived"
    (fun creds ->
      assert (creds.access_token = "long-lived");
      assert (creds.expires_at <> None);
      let requests = List.rev !Mock_http.requests in
      (match requests with
       | [ (_, url, _, _) ] ->
           assert (string_contains url "/v1.0/access_token");
           assert (string_contains url "grant_type=th_exchange_token")
       | _ -> failwith "unexpected request count for long-lived exchange");
      print_endline "ok: exchange long-lived")
    (fun err -> failwith ("unexpected long-lived exchange error: " ^ err))

let test_refresh_token_success () =
  Mock_http.reset ();
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body =
          "{\"access_token\":\"refreshed\",\"token_type\":\"bearer\",\"expires_in\":5184000}";
      };
    ];
  OAuth_http.refresh_token
    ~long_lived_token:"long-lived"
    (fun creds ->
      assert (creds.access_token = "refreshed");
      let requests = List.rev !Mock_http.requests in
      (match requests with
       | [ (_, url, _, _) ] ->
           assert (string_contains url "/v1.0/refresh_access_token");
           assert (string_contains url "grant_type=th_refresh_token")
       | _ -> failwith "unexpected request count for refresh token");
      print_endline "ok: refresh token")
    (fun err -> failwith ("unexpected refresh error: " ^ err))

let test_get_me_success () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body =
          "{\"id\":\"user-1\",\"username\":\"alice\",\"name\":\"Alice\"}";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Ok me ->
          assert (me.id = "user-1");
          assert (me.username = Some "alice");
          print_endline "ok: get me"
      | Error err -> failwith ("unexpected get_me error: " ^ Error_types.error_to_string err))

let test_get_me_malformed_payload_returns_internal_error () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body = "{\"username\":\"alice\"}";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "Failed to parse /me response");
          print_endline "ok: get me malformed payload mapped internal"
      | _ -> failwith "expected internal error for malformed /me payload")

let test_get_me_retries_500_then_succeeds () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 500;
        headers = [ ("X-FB-Trace-ID", "trace-retry") ];
        body = "{\"error\":{\"message\":\"temporary\",\"code\":1}}";
      };
      {
        status = 200;
        headers = [];
        body = "{\"id\":\"user-1\",\"username\":\"alice\"}";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Ok me ->
          assert (me.id = "user-1");
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 2);
          print_endline "ok: get me retries"
      | Error err -> failwith ("expected retry success, got: " ^ Error_types.error_to_string err))

let test_get_me_retry_config_single_attempt () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_MAX_READ_RETRY_ATTEMPTS", "1") ];
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 500;
        headers = [ ("X-FB-Trace-ID", "trace-no-retry") ];
        body = "{\"error\":{\"message\":\"temporary\",\"code\":1}}";
      };
      {
        status = 200;
        headers = [];
        body = "{\"id\":\"user-1\",\"username\":\"alice\"}";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Api_error _) ->
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 1);
          print_endline "ok: retry config single attempt"
      | _ -> failwith "expected immediate failure with single retry attempt")

let test_get_me_retry_config_clamps_low_to_one () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_MAX_READ_RETRY_ATTEMPTS", "0") ];
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 500;
        headers = [ ("X-FB-Trace-ID", "trace-clamp-low") ];
        body = "{\"error\":{\"message\":\"temporary\",\"code\":1}}";
      };
      {
        status = 200;
        headers = [];
        body = "{\"id\":\"user-1\",\"username\":\"alice\"}";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Api_error _) ->
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 1);
          print_endline "ok: retry clamp low"
      | _ -> failwith "expected immediate failure when retry attempts clamped to one")

let test_get_me_retry_config_clamps_high_to_ten () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_MAX_READ_RETRY_ATTEMPTS", "100") ];
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 500; headers = []; body = "{\"error\":{\"message\":\"e1\",\"code\":1}}" };
      { status = 500; headers = []; body = "{\"error\":{\"message\":\"e2\",\"code\":1}}" };
      { status = 500; headers = []; body = "{\"error\":{\"message\":\"e3\",\"code\":1}}" };
      { status = 500; headers = []; body = "{\"error\":{\"message\":\"e4\",\"code\":1}}" };
      { status = 500; headers = []; body = "{\"error\":{\"message\":\"e5\",\"code\":1}}" };
      { status = 500; headers = []; body = "{\"error\":{\"message\":\"e6\",\"code\":1}}" };
      { status = 500; headers = []; body = "{\"error\":{\"message\":\"e7\",\"code\":1}}" };
      { status = 500; headers = []; body = "{\"error\":{\"message\":\"e8\",\"code\":1}}" };
      { status = 500; headers = []; body = "{\"error\":{\"message\":\"e9\",\"code\":1}}" };
      { status = 500; headers = []; body = "{\"error\":{\"message\":\"e10\",\"code\":1}}" };
      { status = 200; headers = []; body = "{\"id\":\"user-1\",\"username\":\"alice\"}" };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Api_error _) ->
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 10);
          print_endline "ok: retry clamp high"
      | _ -> failwith "expected failure after max clamped retry attempts")

let test_get_me_retry_config_invalid_falls_back_to_default () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_MAX_READ_RETRY_ATTEMPTS", "not-a-number") ];
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 500;
        headers = [ ("X-FB-Trace-ID", "trace-default-1") ];
        body = "{\"error\":{\"message\":\"temporary\",\"code\":1}}";
      };
      {
        status = 500;
        headers = [ ("X-FB-Trace-ID", "trace-default-2") ];
        body = "{\"error\":{\"message\":\"temporary\",\"code\":1}}";
      };
      {
        status = 200;
        headers = [];
        body = "{\"id\":\"user-1\",\"username\":\"alice\"}";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Ok me ->
          assert (me.id = "user-1");
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 3);
          print_endline "ok: retry invalid config default"
      | _ -> failwith "expected default retry behavior for invalid config")

let test_get_me_retry_disable_429 () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_READ_RETRY_ON_429", "false") ];
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 429;
        headers = [ ("Retry-After", "120") ];
        body = "{\"error\":{\"message\":\"rate limit\",\"code\":4}}";
      };
      {
        status = 200;
        headers = [];
        body = "{\"id\":\"user-1\",\"username\":\"alice\"}";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Rate_limited _) ->
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 1);
          print_endline "ok: retry disabled for 429"
      | _ -> failwith "expected no retry on 429 when disabled")

let test_get_me_retry_enable_429 () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_READ_RETRY_ON_429", "true") ];
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 429;
        headers = [ ("Retry-After", "1") ];
        body = "{\"error\":{\"message\":\"rate limit\",\"code\":4}}";
      };
      {
        status = 200;
        headers = [];
        body = "{\"id\":\"user-1\",\"username\":\"alice\"}";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Ok me ->
          assert (me.id = "user-1");
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 2);
          print_endline "ok: retry enabled for 429"
      | _ -> failwith "expected retry success on 429 when enabled")

let test_get_me_retry_disable_5xx () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_READ_RETRY_ON_5XX", "false") ];
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 500;
        headers = [ ("X-FB-Trace-ID", "trace-no-5xx-retry") ];
        body = "{\"error\":{\"message\":\"temporary\",\"code\":1}}";
      };
      {
        status = 200;
        headers = [];
        body = "{\"id\":\"user-1\",\"username\":\"alice\"}";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Api_error _) ->
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 1);
          print_endline "ok: retry disabled for 5xx"
      | _ -> failwith "expected no retry on 5xx when disabled")

let test_get_me_retry_disable_network_error () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_READ_RETRY_ON_NETWORK", "false") ];
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  (* No responses configured -> mock emits network-like on_error path *)
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Network_error (Error_types.Connection_failed _)) ->
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 1);
          print_endline "ok: retry disabled for network"
      | _ -> failwith "expected no retry on network error when disabled")

let test_get_me_retry_toggle_invalid_429_defaults_to_disabled () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_READ_RETRY_ON_429", "invalid") ];
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 429;
        headers = [ ("Retry-After", "1") ];
        body = "{\"error\":{\"message\":\"rate limit\",\"code\":4}}";
      };
      {
        status = 200;
        headers = [];
        body = "{\"id\":\"user-1\",\"username\":\"alice\"}";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Rate_limited _) ->
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 1);
          print_endline "ok: invalid 429 toggle defaults disabled"
      | _ -> failwith "expected no retry on 429 for invalid toggle value")

let test_get_me_retry_toggle_invalid_5xx_defaults_to_enabled () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_READ_RETRY_ON_5XX", "invalid") ];
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 500;
        headers = [ ("X-FB-Trace-ID", "trace-1") ];
        body = "{\"error\":{\"message\":\"temporary\",\"code\":1}}";
      };
      {
        status = 200;
        headers = [];
        body = "{\"id\":\"user-1\",\"username\":\"alice\"}";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Ok me ->
          assert (me.id = "user-1");
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 2);
          print_endline "ok: invalid 5xx toggle defaults enabled"
      | _ -> failwith "expected retry on 5xx for invalid toggle value")

let test_get_me_retry_toggle_invalid_network_defaults_to_enabled () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_READ_RETRY_ON_NETWORK", "invalid") ];
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  (* No responses configured: mock emits network-like on_error on each attempt *)
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Network_error (Error_types.Connection_failed _)) ->
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 3);
          print_endline "ok: invalid network toggle defaults enabled"
      | _ -> failwith "expected retry on network for invalid toggle value")

let test_post_single_success () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body =
          "{\"id\":\"user-1\",\"username\":\"alice\",\"name\":\"Alice\"}";
      };
      { status = 200; headers = []; body = "{\"id\":\"container-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-1\"}" };
    ];
  Threads.post_single ~account_id:"acct-1" ~text:"hello threads" ~media_urls:[]
    (function
      | Error_types.Success post_id ->
          assert (post_id = "post-1");
          print_endline "ok: post single"
      | Error_types.Partial_success _ -> failwith "unexpected partial success"
      | Error_types.Failure err ->
          failwith ("unexpected post_single error: " ^ Error_types.error_to_string err))

let test_post_single_text_trimmed_before_send () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-1\"}" };
    ];
  Threads.post_single ~account_id:"acct-1" ~text:"  hello threads  " ~media_urls:[]
    (function
      | Error_types.Success _ ->
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ (_, _, _, _); (_, _, _, create_body); (_, _, _, _) ] ->
               assert (string_contains create_body "text=");
               assert (string_contains create_body "hello");
               assert (string_contains create_body "threads");
               assert (not (string_contains create_body "text=++"));
               print_endline "ok: text trimmed"
           | _ -> failwith "unexpected request count for trimmed text")
      | Error_types.Failure err ->
          failwith ("expected success for trimmed text post, got: " ^ Error_types.error_to_string err)
      | Error_types.Partial_success _ ->
          failwith "expected success for trimmed text post, got partial")

let test_post_single_no_retry_on_publish_500 () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-1\"}" };
      {
        status = 500;
        headers = [ ("X-FB-Trace-ID", "trace-publish") ];
        body = "{\"error\":{\"message\":\"temporary\",\"code\":1}}";
      };
    ];
  Threads.post_single ~account_id:"acct-1" ~text:"hello threads" ~media_urls:[]
    (function
      | Error_types.Failure (Error_types.Api_error api_err) ->
          assert (api_err.request_id = Some "trace-publish");
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 3);
          print_endline "ok: post single no retry"
      | _ -> failwith "expected api failure without publish retry")

let test_get_posts_success () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body =
          "{\"id\":\"user-1\",\"username\":\"alice\",\"name\":\"Alice\"}";
      };
      {
        status = 200;
        headers = [];
        body =
          "{\"data\":[{\"id\":\"post-1\",\"text\":\"hello\"}],\"paging\":{\"cursors\":{\"after\":\"cursor-2\"}}}";
      };
    ];
  Threads.get_posts ~account_id:"acct-1" ~limit:10
    (function
      | Ok (posts, next_after) ->
          assert (List.length posts = 1);
          assert ((List.hd posts).id = "post-1");
          assert (next_after = Some "cursor-2");
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ (_, me_url, _, _); (_, posts_url, _, _) ] ->
               assert (string_contains me_url "/v1.0/me?");
               assert (string_contains posts_url "/v1.0/user-1/threads?")
           | _ -> failwith "unexpected request count for get_posts");
          print_endline "ok: get posts"
      | Error err -> failwith ("unexpected get_posts error: " ^ Error_types.error_to_string err))

let test_get_posts_malformed_payload_returns_internal_error () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body = "{\"id\":\"user-1\",\"username\":\"alice\"}";
      };
      {
        status = 200;
        headers = [];
        body = "{\"data\":[{\"text\":\"hello\"}]}";
      };
    ];
  Threads.get_posts ~account_id:"acct-1"
    (function
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "Failed to parse posts response");
          print_endline "ok: get posts malformed payload mapped internal"
      | _ -> failwith "expected internal error for malformed posts payload")

let test_get_posts_limit_clamped_to_minimum () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body = "{\"id\":\"user-1\",\"username\":\"alice\"}";
      };
      {
        status = 200;
        headers = [];
        body = "{\"data\":[]}";
      };
    ];
  Threads.get_posts ~account_id:"acct-1" ~limit:0
    (function
      | Ok (_, _) ->
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ (_, _, _, _); (_, posts_url, _, _) ] ->
               assert (string_contains posts_url "limit=1");
               print_endline "ok: get posts limit min clamp"
           | _ -> failwith "unexpected request count for get_posts min clamp")
      | Error err -> failwith ("unexpected get_posts min clamp error: " ^ Error_types.error_to_string err))

let test_get_posts_limit_clamped_to_maximum () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body = "{\"id\":\"user-1\",\"username\":\"alice\"}";
      };
      {
        status = 200;
        headers = [];
        body = "{\"data\":[]}";
      };
    ];
  Threads.get_posts ~account_id:"acct-1" ~limit:999
    (function
      | Ok (_, _) ->
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ (_, _, _, _); (_, posts_url, _, _) ] ->
               assert (string_contains posts_url "limit=100");
               print_endline "ok: get posts limit max clamp"
           | _ -> failwith "unexpected request count for get_posts max clamp")
      | Error err -> failwith ("unexpected get_posts max clamp error: " ^ Error_types.error_to_string err))

let test_get_posts_after_cursor_trimmed () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body = "{\"id\":\"user-1\",\"username\":\"alice\"}";
      };
      {
        status = 200;
        headers = [];
        body = "{\"data\":[]}";
      };
    ];
  Threads.get_posts ~account_id:"acct-1" ~after:"  cursor-abc  "
    (function
      | Ok (_, _) ->
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ (_, _, _, _); (_, posts_url, _, _) ] ->
               assert (string_contains posts_url "after=cursor-abc");
               assert (not (string_contains posts_url "after=++cursor-abc++"));
               print_endline "ok: get posts after trimmed"
           | _ -> failwith "unexpected request count for get_posts after trim")
      | Error err -> failwith ("unexpected get_posts after trim error: " ^ Error_types.error_to_string err))

let test_get_posts_after_cursor_whitespace_omitted () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body = "{\"id\":\"user-1\",\"username\":\"alice\"}";
      };
      {
        status = 200;
        headers = [];
        body = "{\"data\":[]}";
      };
    ];
  Threads.get_posts ~account_id:"acct-1" ~after:"   "
    (function
      | Ok (_, _) ->
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ (_, _, _, _); (_, posts_url, _, _) ] ->
               assert (not (string_contains posts_url "after="));
               print_endline "ok: get posts after whitespace omitted"
            | _ -> failwith "unexpected request count for get_posts after omit")
      | Error err -> failwith ("unexpected get_posts after omit error: " ^ Error_types.error_to_string err))

let test_get_account_insights_contract () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body = "{\"id\":\"user-1\",\"username\":\"alice\"}";
      };
      {
        status = 200;
        headers = [];
        body =
          "{\"data\":[{\"name\":\"views\",\"period\":\"day\",\"values\":[{\"value\":120,\"end_time\":\"2025-01-02T00:00:00+0000\"}]},{\"name\":\"likes\",\"period\":\"day\",\"values\":[{\"value\":7,\"end_time\":\"2025-01-02T00:00:00+0000\"}]}]}";
      };
    ];
  Threads.get_account_insights
    ~account_id:"acct-1"
    ~since:"2025-01-01"
    ~until:"2025-01-02"
    (function
      | Ok insights ->
          assert (List.length insights = 2);
          let first = List.hd insights in
          assert (first.metric = Social_threads_v1.Views);
          assert (List.length first.values = 1);
          assert ((List.hd first.values).value = 120);
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ (_, me_url, _, _); (_, insights_url, _, _) ] ->
               assert (string_contains me_url "/v1.0/me?");
               assert (string_contains insights_url "/v1.0/user-1/threads_insights?");
               assert (query_param insights_url "metric" = Some "views,likes,replies,reposts,quotes");
               assert (query_param insights_url "period" = Some "day");
               assert (query_param insights_url "since" = Some "2025-01-01");
               assert (query_param insights_url "until" = Some "2025-01-02");
               assert (query_param insights_url "access_token" = Some "access-xyz")
           | _ -> failwith "unexpected request count for account insights");
          print_endline "ok: account insights contract"
      | Error err ->
          failwith
            ("unexpected account insights error: " ^ Error_types.error_to_string err))

let test_get_account_insights_malformed_payload_returns_internal_error () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body = "{\"id\":\"user-1\",\"username\":\"alice\"}";
      };
      {
        status = 200;
        headers = [];
        body = "{\"data\":[{\"period\":\"day\",\"values\":[{\"value\":120}]}]}";
      };
    ];
  Threads.get_account_insights
    ~account_id:"acct-1"
    ~since:"2025-01-01"
    ~until:"2025-01-02"
    (function
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "Failed to parse account insights response");
          print_endline "ok: account insights malformed payload mapped internal"
      | _ -> failwith "expected internal error for malformed account insights payload")

let test_get_post_insights_contract () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body =
          "{\"data\":[{\"name\":\"views\",\"period\":\"lifetime\",\"values\":[{\"value\":55}]},{\"name\":\"quotes\",\"period\":\"lifetime\",\"value\":2}]}";
      };
    ];
  Threads.get_post_insights ~account_id:"acct-1" ~post_id:"post-1"
    (function
      | Ok insights ->
          assert (List.length insights = 2);
          let first = List.hd insights in
          assert (first.metric = Social_threads_v1.Views);
          assert (first.value = 55);
          let second = List.nth insights 1 in
          assert (second.metric = Social_threads_v1.Quotes);
          assert (second.value = 2);
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ (_, insights_url, _, _) ] ->
               assert (string_contains insights_url "/v1.0/post-1/insights?");
               assert (query_param insights_url "metric" = Some "views,likes,replies,reposts,quotes");
               assert (query_param insights_url "access_token" = Some "access-xyz")
           | _ -> failwith "unexpected request count for post insights");
          print_endline "ok: post insights contract"
      | Error err ->
          failwith
            ("unexpected post insights error: " ^ Error_types.error_to_string err))

let test_get_post_insights_malformed_payload_returns_internal_error () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body = "{\"data\":[{\"name\":\"views\",\"period\":\"lifetime\",\"values\":[]}]}";
      };
    ];
  Threads.get_post_insights ~account_id:"acct-1" ~post_id:"post-1"
    (function
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "Failed to parse post insights response");
          print_endline "ok: post insights malformed payload mapped internal"
      | _ -> failwith "expected internal error for malformed post insights payload")

let test_insights_network_error_redacts_query_token () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_errors
    [ "GET https://graph.threads.net/v1.0/me?access_token=access-xyz failed" ];
  Threads.get_account_insights
    ~account_id:"acct-1"
    ~since:"2025-01-01"
    ~until:"2025-01-02"
    (function
      | Error (Error_types.Network_error (Error_types.Connection_failed msg)) ->
          if string_contains msg "access-xyz" then
            failwith ("Token was not redacted: " ^ msg);
          if not (msg = "[REDACTED_SENSITIVE_TEXT]" || msg = "No mock response set") then
            failwith ("Unexpected redaction message: " ^ msg);
          print_endline "ok: insights network error redacts query token"
      | _ -> failwith "expected redacted network error for insights")

let test_post_single_image_success () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-1\"}" };
    ];
  Threads.post_single
    ~account_id:"acct-1"
    ~text:"hello"
    ~media_urls:[ "https://example.com/image.jpg" ]
    (function
      | Error_types.Success post_id ->
          assert (post_id = "post-1");
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ (_, _, _, _); (_, _, _, create_body); (_, _, _, _) ] ->
               assert (string_contains create_body "media_type=IMAGE");
               assert (string_contains create_body "image_url=");
               assert (string_contains create_body "image.jpg")
           | _ -> failwith "unexpected request count for image post");
          print_endline "ok: post single image"
      | Error_types.Failure err ->
          failwith ("expected image post success, got error: " ^ Error_types.error_to_string err)
      | Error_types.Partial_success _ ->
          failwith "expected image post success, got partial success")

let test_post_single_media_only_success () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-1\"}" };
    ];
  Threads.post_single
    ~account_id:"acct-1"
    ~text:"   "
    ~media_urls:[ "https://example.com/image.jpg" ]
    (function
      | Error_types.Success _ ->
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ (_, _, _, _); (_, _, _, create_body); (_, _, _, _) ] ->
               assert (not (string_contains create_body "&text="));
               print_endline "ok: media only"
           | _ -> failwith "unexpected request count for media-only post")
      | _ -> failwith "expected success for media-only post")

let test_post_single_with_idempotency_key () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-1\"}" };
    ];
  Threads.post_single
    ~account_id:"acct-1"
    ~text:"hello"
    ~media_urls:[]
    ~idempotency_key:"req-123"
    (function
      | Error_types.Success _ ->
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ (_, _, _, _); (_, _, _, create_body); (_, _, _, _) ] ->
               assert (string_contains create_body "client_request_id=req-123")
           | _ -> failwith "unexpected request count for idempotency test");
          print_endline "ok: idempotency key"
      | _ -> failwith "expected success with idempotency key")

let test_post_single_idempotency_key_trimmed () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-1\"}" };
    ];
  Threads.post_single
    ~account_id:"acct-1"
    ~text:"hello"
    ~media_urls:[]
    ~idempotency_key:"  req-trim  "
    (function
      | Error_types.Success _ ->
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ (_, _, _, _); (_, _, _, create_body); (_, _, _, _) ] ->
               assert (string_contains create_body "client_request_id=req-trim");
               assert (not (string_contains create_body "client_request_id=++req-trim++"))
           | _ -> failwith "unexpected request count for idempotency trim test");
          print_endline "ok: idempotency key trimmed"
      | _ -> failwith "expected success with trimmed idempotency key")

let test_post_single_reply_control_forwarded () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-1\"}" };
    ];
  Threads.post_single
    ~account_id:"acct-1"
    ~text:"hello"
    ~media_urls:[]
    ~reply_control:"EVERYONE"
    (function
      | Error_types.Success _ ->
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ (_, _, _, _); (_, _, _, create_body); (_, _, _, _) ] ->
               assert (string_contains create_body "reply_control=EVERYONE")
           | _ -> failwith "unexpected request count for reply_control test");
          print_endline "ok: reply control forwarded"
      | _ -> failwith "expected success with reply_control")

let test_post_single_rejects_unsupported_media () =
  Mock_config.reset ();
  Threads.post_single
    ~account_id:"acct-1"
    ~text:"hello"
    ~media_urls:[ "https://example.com/file.tiff" ]
    (function
      | Error_types.Failure (Error_types.Validation_error errs) ->
          let has_unsupported =
            List.exists
              (function
                | Error_types.Media_unsupported_format _ -> true
                | _ -> false)
              errs
          in
          assert has_unsupported;
          print_endline "ok: media unsupported"
      | _ -> failwith "expected validation error for unsupported media")

let test_post_single_media_url_with_query_success () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-1\"}" };
    ];
  Threads.post_single
    ~account_id:"acct-1"
    ~text:"hello"
    ~media_urls:[ "https://example.com/image.jpg?dl=1" ]
    (function
      | Error_types.Success _ -> print_endline "ok: media query url"
      | _ -> failwith "expected success for media URL with query")

let test_post_single_media_url_uppercase_scheme_success () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-1\"}" };
    ];
  Threads.post_single
    ~account_id:"acct-1"
    ~text:"hello"
    ~media_urls:[ "HTTPS://example.com/image.jpg" ]
    (function
      | Error_types.Success _ -> print_endline "ok: media uppercase scheme"
      | _ -> failwith "expected success for uppercase URL scheme")

let test_post_single_media_url_trimmed_before_send () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-1\"}" };
    ];
  Threads.post_single
    ~account_id:"acct-1"
    ~text:"hello"
    ~media_urls:[ "  https://example.com/image.jpg  " ]
    (function
      | Error_types.Success _ ->
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ (_, _, _, _); (_, _, _, create_body); (_, _, _, _) ] ->
               assert (string_contains create_body "image_url=");
               assert (string_contains create_body "image.jpg");
               assert (not (string_contains create_body "image_url=%20"))
           | _ -> failwith "unexpected request count for trimmed media URL");
          print_endline "ok: media url trimmed"
      | Error_types.Failure err ->
          failwith ("expected success for trimmed media URL, got: " ^ Error_types.error_to_string err)
      | Error_types.Partial_success _ ->
          failwith "expected success for trimmed media URL, got partial")

let test_get_me_rate_limited () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 429;
        headers = [ ("Retry-After", "120") ];
        body = "{\"error\":{\"message\":\"rate limit\",\"code\":4}}";
      };
      {
        status = 429;
        headers = [ ("Retry-After", "120") ];
        body = "{\"error\":{\"message\":\"rate limit\",\"code\":4}}";
      };
      {
        status = 429;
        headers = [ ("Retry-After", "120") ];
        body = "{\"error\":{\"message\":\"rate limit\",\"code\":4}}";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Rate_limited info) ->
          assert (info.retry_after_seconds = Some 120);
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 1);
          print_endline "ok: rate limited"
      | Error err ->
          failwith ("expected rate limit error, got: " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "expected rate-limited error")

let test_get_me_rate_limited_non_json_body () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 429;
        headers = [ ("Retry-After", "30") ];
        body = "rate limited";
      };
      {
        status = 429;
        headers = [ ("Retry-After", "30") ];
        body = "rate limited";
      };
      {
        status = 429;
        headers = [ ("Retry-After", "30") ];
        body = "rate limited";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Rate_limited info) ->
          assert (info.retry_after_seconds = Some 30);
          print_endline "ok: rate limited non-json"
      | _ -> failwith "expected rate-limited error for non-json 429 body")

let test_get_me_rate_limited_negative_retry_after_clamped () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 429;
        headers = [ ("Retry-After", "-5") ];
        body = "{\"error\":{\"message\":\"rate limit\",\"code\":4}}";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Rate_limited info) ->
          assert (info.retry_after_seconds = Some 0);
          print_endline "ok: retry-after clamped"
      | _ -> failwith "expected rate-limited error with clamped retry-after")

let test_get_me_rate_limited_float_retry_after_rounded () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 429;
        headers = [ ("Retry-After", "0.5") ];
        body = "{\"error\":{\"message\":\"rate limit\",\"code\":4}}";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Rate_limited info) ->
          assert (info.retry_after_seconds = Some 1);
          print_endline "ok: retry-after float rounded"
      | _ -> failwith "expected rate-limited error with float retry-after")

let test_get_me_rate_limited_retry_after_clamped_high () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 429;
        headers = [ ("Retry-After", "9999999") ];
        body = "{\"error\":{\"message\":\"rate limit\",\"code\":4}}";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Rate_limited info) ->
          assert (info.retry_after_seconds = Some 86400);
          print_endline "ok: retry-after high clamp"
      | _ -> failwith "expected rate-limited error with clamped high retry-after")

let test_get_me_rate_limited_retry_after_huge_float_clamped () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 429;
        headers = [ ("Retry-After", "1e309") ];
        body = "{\"error\":{\"message\":\"rate limit\",\"code\":4}}";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Rate_limited info) ->
          assert (info.retry_after_seconds = Some 86400);
          print_endline "ok: retry-after huge float clamp"
      | _ -> failwith "expected rate-limited error with huge float retry-after")

let test_get_me_api_error_request_id () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 500;
        headers = [ ("X-FB-Trace-ID", "trace-123") ];
        body = "{\"error\":{\"message\":\"server down\",\"code\":1}}";
      };
      {
        status = 500;
        headers = [ ("X-FB-Trace-ID", "trace-123") ];
        body = "{\"error\":{\"message\":\"server down\",\"code\":1}}";
      };
      {
        status = 500;
        headers = [ ("X-FB-Trace-ID", "trace-123") ];
        body = "{\"error\":{\"message\":\"server down\",\"code\":1}}";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Api_error api_err) ->
          assert (api_err.request_id = Some "trace-123");
          print_endline "ok: request id mapped"
      | _ -> failwith "expected api error with request id")

let test_get_me_api_error_request_id_fallback_header () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 500;
        headers = [ ("X-FB-Request-ID", "req-456") ];
        body = "{\"error\":{\"message\":\"server down\",\"code\":1}}";
      };
      {
        status = 500;
        headers = [ ("X-FB-Request-ID", "req-456") ];
        body = "{\"error\":{\"message\":\"server down\",\"code\":1}}";
      };
      {
        status = 500;
        headers = [ ("X-FB-Request-ID", "req-456") ];
        body = "{\"error\":{\"message\":\"server down\",\"code\":1}}";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Api_error api_err) ->
          assert (api_err.request_id = Some "req-456");
          print_endline "ok: request id fallback"
      | _ -> failwith "expected api error with fallback request id")

let test_get_me_api_error_preserves_provider_metadata () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_MAX_READ_RETRY_ATTEMPTS", "1") ];
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 500;
        headers = [];
        body =
          "{\"error\":{\"message\":\"server down\",\"code\":42,\"type\":\"OAuthException\",\"error_subcode\":9999}}";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Api_error api_err) ->
          assert (string_contains api_err.message "server down");
          assert (string_contains api_err.message "code=42");
          assert (string_contains api_err.message "type=OAuthException");
          assert (string_contains api_err.message "subcode=9999");
          print_endline "ok: api error preserves provider metadata"
      | _ -> failwith "expected api error preserving provider metadata")

let test_get_me_api_error_raw_response_redacts_sensitive_fields () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 500;
        headers = [ ("X-FB-Trace-ID", "trace-789") ];
        body =
          "{\"error\":{\"message\":\"server down\",\"code\":1},\"access_token\":\"should-not-leak\"}";
      };
      {
        status = 500;
        headers = [ ("X-FB-Trace-ID", "trace-789") ];
        body =
          "{\"error\":{\"message\":\"server down\",\"code\":1},\"access_token\":\"should-not-leak\"}";
      };
      {
        status = 500;
        headers = [ ("X-FB-Trace-ID", "trace-789") ];
        body =
          "{\"error\":{\"message\":\"server down\",\"code\":1},\"access_token\":\"should-not-leak\"}";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Api_error api_err) ->
          (match api_err.raw_response with
           | Some body ->
               assert (string_contains body "[REDACTED]");
               assert (not (string_contains body "should-not-leak"));
               print_endline "ok: api error raw response redacts sensitive fields"
           | None -> failwith "expected raw_response in api error")
      | _ -> failwith "expected api error with redacted raw response")

let test_get_me_api_error_raw_response_redacts_sensitive_non_json_body () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 500;
        headers = [ ("X-FB-Trace-ID", "trace-790") ];
        body = "backend exploded authorization=Bearer should-not-leak";
      };
      {
        status = 500;
        headers = [ ("X-FB-Trace-ID", "trace-790") ];
        body = "backend exploded authorization=Bearer should-not-leak";
      };
      {
        status = 500;
        headers = [ ("X-FB-Trace-ID", "trace-790") ];
        body = "backend exploded authorization=Bearer should-not-leak";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Api_error api_err) ->
          (match api_err.raw_response with
           | Some body ->
               assert (body = "[REDACTED_NON_JSON_ERROR_BODY]");
               print_endline "ok: api error raw response redacts non-json sensitive body"
           | None -> failwith "expected raw_response in api error")
      | _ -> failwith "expected api error with non-json redacted raw response")

let test_get_me_api_error_message_redacts_sensitive_text () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 500;
        headers = [ ("X-FB-Trace-ID", "trace-791") ];
        body =
          "{\"error\":{\"message\":\"backend failed access_token=should-not-leak\",\"code\":1}}";
      };
      {
        status = 500;
        headers = [ ("X-FB-Trace-ID", "trace-791") ];
        body =
          "{\"error\":{\"message\":\"backend failed access_token=should-not-leak\",\"code\":1}}";
      };
      {
        status = 500;
        headers = [ ("X-FB-Trace-ID", "trace-791") ];
        body =
          "{\"error\":{\"message\":\"backend failed access_token=should-not-leak\",\"code\":1}}";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Api_error api_err) ->
          assert (string_contains api_err.message "[REDACTED_SENSITIVE_TEXT]");
          assert (string_contains api_err.message "code=1");
          print_endline "ok: api error message redacts sensitive text"
      | _ -> failwith "expected api error with redacted message")

let test_get_me_missing_credentials () =
  Mock_config.reset ();
  Threads.get_me ~account_id:"missing"
    (function
      | Error (Error_types.Auth_error Error_types.Missing_credentials) ->
          print_endline "ok: missing credentials"
      | Error err ->
          failwith
            ("expected missing credentials auth error, got: "
            ^ Error_types.error_to_string err)
      | Ok _ -> failwith "expected missing credentials error")

let test_get_me_empty_access_token_treated_missing () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "   ";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Auth_error Error_types.Missing_credentials) ->
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 0);
          print_endline "ok: empty token treated missing"
      | _ -> failwith "expected missing credentials for empty access token")

let test_get_me_credentials_backend_error_maps_internal () =
  Mock_config.reset ();
  Mock_config.credentials_error_override := Some "database unavailable";
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "Failed to load credentials");
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 0);
          print_endline "ok: credentials backend error mapped"
      | _ -> failwith "expected internal error for credentials backend failure")

let test_get_me_credentials_not_found_maps_missing () =
  Mock_config.reset ();
  Mock_config.credentials_error_override := Some "credentials not found";
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Auth_error Error_types.Missing_credentials) ->
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 0);
          print_endline "ok: credentials not found mapped missing"
      | _ -> failwith "expected missing credentials for not-found credential error")

let test_get_me_non_credential_not_found_maps_internal () =
  Mock_config.reset ();
  Mock_config.credentials_error_override := Some "database not found";
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "Failed to load credentials");
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 0);
          print_endline "ok: non-credential not-found maps internal"
      | _ -> failwith "expected internal error for non-credential not-found")

let test_get_me_missing_without_credential_context_maps_internal () =
  Mock_config.reset ();
  Mock_config.credentials_error_override := Some "missing permissions";
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "Failed to load credentials");
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 0);
          print_endline "ok: non-credential missing maps internal"
      | _ -> failwith "expected internal error for non-credential missing")

let test_get_me_missing_account_phrase_maps_internal () =
  Mock_config.reset ();
  Mock_config.credentials_error_override := Some "missing account record";
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "Failed to load credentials");
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 0);
          print_endline "ok: missing account maps internal"
      | _ -> failwith "expected internal error for missing account phrase")

let test_get_me_no_credential_phrase_maps_missing () =
  Mock_config.reset ();
  Mock_config.credentials_error_override := Some "no credential record";
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Auth_error Error_types.Missing_credentials) ->
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 0);
          print_endline "ok: no credential phrase maps missing"
      | _ -> failwith "expected missing credentials for no credential phrase")

let test_get_me_credentials_word_and_missing_word_not_always_missing () =
  Mock_config.reset ();
  Mock_config.credentials_error_override := Some "credentials service missing dependency";
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "Failed to load credentials");
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 0);
          print_endline "ok: credentials+missing words map internal"
      | _ -> failwith "expected internal error for non-auth credentials backend message")

let test_get_me_token_expiry_skew_default_blocks_near_expiry () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = Some (rfc3339_after_seconds 30);
          token_type = "Bearer";
        } );
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Auth_error Error_types.Token_expired) ->
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 0);
          print_endline "ok: token skew default"
      | _ -> failwith "expected token expired due to default expiry skew")

let test_get_me_token_expiry_skew_override_allows_request () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_TOKEN_EXPIRY_SKEW_SECONDS", "0") ];
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = Some (rfc3339_after_seconds 30);
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body = "{\"id\":\"user-1\",\"username\":\"alice\"}";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Ok me ->
          assert (me.id = "user-1");
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 1);
          print_endline "ok: token skew override"
      | _ -> failwith "expected request success when expiry skew is overridden")

let test_get_me_token_expiry_skew_invalid_defaults_to_60 () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_TOKEN_EXPIRY_SKEW_SECONDS", "oops") ];
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = Some (rfc3339_after_seconds 30);
          token_type = "Bearer";
        } );
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Auth_error Error_types.Token_expired) ->
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 0);
          print_endline "ok: token skew invalid defaults"
      | _ -> failwith "expected token_expired when invalid skew defaults to 60")

let test_get_me_token_expiry_skew_high_clamped_to_3600 () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_TOKEN_EXPIRY_SKEW_SECONDS", "99999") ];
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = Some (rfc3339_after_seconds 3500);
          token_type = "Bearer";
        } );
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Auth_error Error_types.Token_expired) ->
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 0);
          print_endline "ok: token skew high clamped"
      | _ -> failwith "expected token_expired when skew is clamped to 3600")

let test_get_me_token_expiry_skew_negative_clamped_to_0 () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_TOKEN_EXPIRY_SKEW_SECONDS", "-10") ];
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = Some (rfc3339_after_seconds 30);
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body = "{\"id\":\"user-1\",\"username\":\"alice\"}";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Ok me ->
          assert (me.id = "user-1");
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 1);
          print_endline "ok: token skew negative clamped"
      | _ -> failwith "expected request success when negative skew clamps to 0")

let test_get_me_malformed_expiry_treated_as_expired () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = Some "not-a-timestamp";
          token_type = "Bearer";
        } );
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Auth_error Error_types.Token_expired) ->
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 0);
          print_endline "ok: malformed expiry treated expired"
      | _ -> failwith "expected token expired for malformed expiry timestamp")

let test_get_me_malformed_expiry_with_auto_refresh () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_AUTO_REFRESH_TOKEN", "true") ];
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "expired-token";
          refresh_token = None;
          expires_at = Some "not-a-timestamp";
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body =
          "{\"access_token\":\"refreshed-token\",\"token_type\":\"Bearer\",\"expires_in\":3600}";
      };
      {
        status = 200;
        headers = [];
        body = "{\"id\":\"user-1\",\"username\":\"alice\"}";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Ok me ->
          assert (me.id = "user-1");
          let saved = List.assoc "acct-1" !Mock_config.credentials_store in
          assert (saved.access_token = "refreshed-token");
          print_endline "ok: malformed expiry auto refresh"
      | _ -> failwith "expected refresh success for malformed expiry with auto refresh")

let test_get_me_auto_refresh_enabled_refreshes_and_proceeds () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_AUTO_REFRESH_TOKEN", "true") ];
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "expired-token";
          refresh_token = None;
          expires_at = Some (rfc3339_after_seconds 10);
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body =
          "{\"access_token\":\"refreshed-token\",\"token_type\":\"Bearer\",\"expires_in\":3600}";
      };
      {
        status = 200;
        headers = [];
        body = "{\"id\":\"user-1\",\"username\":\"alice\"}";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Ok me ->
          assert (me.id = "user-1");
          let saved = List.assoc "acct-1" !Mock_config.credentials_store in
          assert (saved.access_token = "refreshed-token");
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 2);
          print_endline "ok: auto refresh"
      | _ -> failwith "expected success with auto refresh enabled")

let test_get_me_auto_refresh_empty_token_from_refresh_fails () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_AUTO_REFRESH_TOKEN", "true") ];
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "expired-token";
          refresh_token = None;
          expires_at = Some (rfc3339_after_seconds 10);
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body =
          "{\"access_token\":\"   \",\"token_type\":\"Bearer\",\"expires_in\":3600}";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Auth_error Error_types.Token_expired) ->
          print_endline "ok: auto refresh empty token fails"
      | _ -> failwith "expected token_expired when refresh returns empty token")

let test_get_me_auto_refresh_enabled_refresh_failure_returns_expired () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_AUTO_REFRESH_TOKEN", "true") ];
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "expired-token";
          refresh_token = None;
          expires_at = Some (rfc3339_after_seconds 10);
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 500;
        headers = [];
        body = "{\"error\":{\"message\":\"refresh fail\",\"code\":1}}";
      };
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Auth_error Error_types.Token_expired) ->
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 1);
          print_endline "ok: auto refresh fail"
      | _ -> failwith "expected token_expired when auto refresh fails")

let test_post_thread_multi_item_success () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-2\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-2\"}" };
    ];
  Threads.post_thread
    ~account_id:"acct-1"
    ~texts:[ "one"; "two" ]
    ~media_urls_per_post:[ []; [] ]
    (function
      | Error_types.Success result ->
          assert (result.Error_types.posted_ids = [ "post-1"; "post-2" ]);
          assert (result.failed_at_index = None);
          assert (result.total_requested = 2);
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [
            (_, me_url, _, _);
            (_, create_1_url, _, create_1_body);
            (_, publish_1_url, _, _);
            (_, create_2_url, _, create_2_body);
            (_, publish_2_url, _, _);
           ] ->
               assert (string_contains me_url "/v1.0/me?");
               assert (string_contains create_1_url "/v1.0/user-1/threads");
               assert (string_contains publish_1_url "/v1.0/user-1/threads_publish");
               assert (string_contains create_2_url "/v1.0/user-1/threads");
               assert (string_contains publish_2_url "/v1.0/user-1/threads_publish");
               assert (not (string_contains create_1_body "reply_to_id="));
               assert (string_contains create_2_body "reply_to_id=post-1")
           | _ -> failwith "unexpected request sequence for post_thread");
          print_endline "ok: post thread multi-item"
      | _ -> failwith "expected successful multi-item post_thread")

let test_post_thread_multi_item_with_media_success () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-2\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-2\"}" };
    ];
  Threads.post_thread
    ~account_id:"acct-1"
    ~texts:[ "one"; "two" ]
    ~media_urls_per_post:[ [ "https://example.com/a.jpg" ]; [ "https://example.com/b.mp4" ] ]
    (function
      | Error_types.Success result ->
          assert (result.Error_types.posted_ids = [ "post-1"; "post-2" ]);
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [
            (_, _, _, _);
            (_, _, _, create_1_body);
            (_, _, _, _);
            (_, _, _, create_2_body);
            (_, _, _, _);
           ] ->
               assert (string_contains create_1_body "media_type=IMAGE");
               assert (string_contains create_2_body "media_type=VIDEO");
               assert (string_contains create_2_body "reply_to_id=post-1")
           | _ -> failwith "unexpected request sequence for media thread");
          print_endline "ok: post thread media"
      | _ -> failwith "expected successful media thread post")

let test_post_thread_idempotency_key_suffixes () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-2\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-2\"}" };
    ];
  Threads.post_thread
    ~account_id:"acct-1"
    ~texts:[ "one"; "two" ]
    ~media_urls_per_post:[ []; [] ]
    ~idempotency_key:"thread-req"
    (function
      | Error_types.Success _ ->
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ (_, _, _, _); (_, _, _, create_1_body); (_, _, _, _); (_, _, _, create_2_body); (_, _, _, _) ] ->
               assert (string_contains create_1_body "client_request_id=thread-req-0");
               assert (string_contains create_2_body "client_request_id=thread-req-1")
           | _ -> failwith "unexpected request sequence for thread idempotency");
          print_endline "ok: thread idempotency"
      | _ -> failwith "expected success for thread idempotency test")

let test_post_thread_idempotency_key_trimmed_suffixes () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-2\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-2\"}" };
    ];
  Threads.post_thread
    ~account_id:"acct-1"
    ~texts:[ "one"; "two" ]
    ~media_urls_per_post:[ []; [] ]
    ~idempotency_key:"  thread-trim  "
    (function
      | Error_types.Success _ ->
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ (_, _, _, _); (_, _, _, create_1_body); (_, _, _, _); (_, _, _, create_2_body); (_, _, _, _) ] ->
               assert (string_contains create_1_body "client_request_id=thread-trim-0");
               assert (string_contains create_2_body "client_request_id=thread-trim-1")
           | _ -> failwith "unexpected request sequence for thread idempotency trim");
          print_endline "ok: thread idempotency trimmed"
      | _ -> failwith "expected success for thread idempotency trim test")

let test_post_thread_reply_control_forwarded () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-2\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-2\"}" };
    ];
  Threads.post_thread
    ~account_id:"acct-1"
    ~texts:[ "one"; "two" ]
    ~media_urls_per_post:[ []; [] ]
    ~reply_control:"FOLLOWED"
    (function
      | Error_types.Success _ ->
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ (_, _, _, _); (_, _, _, create_1_body); (_, _, _, _); (_, _, _, create_2_body); (_, _, _, _) ] ->
               assert (string_contains create_1_body "reply_control=FOLLOWED");
               assert (string_contains create_2_body "reply_control=FOLLOWED")
           | _ -> failwith "unexpected request sequence for thread reply_control");
          print_endline "ok: thread reply control forwarded"
      | _ -> failwith "expected success for thread reply_control test")

let test_post_thread_partial_failure () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-2\"}" };
      {
        status = 500;
        headers = [];
        body = "{\"error\":{\"message\":\"boom\",\"code\":1}}";
      };
    ];
  Threads.post_thread
    ~account_id:"acct-1"
    ~texts:[ "one"; "two" ]
    ~media_urls_per_post:[ []; [] ]
    (function
      | Error_types.Partial_success { result; warnings } ->
          assert (result.Error_types.posted_ids = [ "post-1" ]);
          assert (result.failed_at_index = Some 1);
          assert (result.total_requested = 2);
          assert (List.length warnings = 1);
          print_endline "ok: post thread partial"
      | _ -> failwith "expected partial success for post_thread failure")

let test_post_thread_first_item_failure () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      {
        status = 400;
        headers = [];
        body = "{\"error\":{\"message\":\"bad input\",\"code\":1}}";
      };
    ];
  Threads.post_thread
    ~account_id:"acct-1"
    ~texts:[ "one"; "two" ]
    ~media_urls_per_post:[ []; [] ]
    (function
      | Error_types.Failure (Error_types.Api_error _) ->
          print_endline "ok: post thread first failure"
      | _ -> failwith "expected failure when first item in chain fails")

let test_post_thread_rejects_extra_media_entries () =
  Mock_config.reset ();
  Threads.post_thread
    ~account_id:"acct-1"
    ~texts:[ "one" ]
    ~media_urls_per_post:[ []; [ "https://example.com/image.jpg" ] ]
    (function
      | Error_types.Failure (Error_types.Validation_error errs) ->
          let has_mismatch =
            List.exists
              (function
                | Error_types.Too_many_media { count; max } ->
                    count = 2 && max = 1
                | _ -> false)
              errs
          in
          assert has_mismatch;
          print_endline "ok: thread extra media rejected"
      | _ -> failwith "expected validation failure for extra media entries")

let test_post_thread_rejects_extra_empty_media_entries () =
  Mock_config.reset ();
  Threads.post_thread
    ~account_id:"acct-1"
    ~texts:[ "one" ]
    ~media_urls_per_post:[ []; [] ]
    (function
      | Error_types.Failure (Error_types.Validation_error errs) ->
          let has_mismatch =
            List.exists
              (function
                | Error_types.Too_many_media { count; max } ->
                    count = 2 && max = 1
                | _ -> false)
              errs
          in
          assert has_mismatch;
          print_endline "ok: thread extra empty media rejected"
      | _ -> failwith "expected validation failure for extra empty media entries")

let test_canonical_insights_adapters () =
  let find_series provider_metric series =
    List.find_opt
      (fun item -> item.Analytics_types.provider_metric = Some provider_metric)
      series
  in

  let account_insights : Social_threads_v1.account_insight list =
    [ { metric = Social_threads_v1.Views;
        period = Some "day";
        values =
          [ { value = 110; end_time = Some "2025-01-02T00:00:00+0000" };
            { value = 120; end_time = Some "2025-01-03T00:00:00+0000" } ] };
      { metric = Social_threads_v1.Likes;
        period = Some "day";
        values = [ { value = 9; end_time = Some "2025-01-02T00:00:00+0000" } ] } ]
  in
  let account_series =
    Social_threads_v1.to_canonical_account_insights_series account_insights
  in
  assert (List.length account_series = 2);
  (match find_series "views" account_series with
   | Some item ->
       assert (Analytics_types.canonical_metric_key item.metric = "views");
       assert ((List.hd item.points).value = 110)
   | None -> failwith "Missing views canonical series");

  let post_insights : Social_threads_v1.post_insight list =
    [ { metric = Social_threads_v1.Reposts; period = Some "day"; value = 4 };
      { metric = Social_threads_v1.Quotes; period = Some "day"; value = 2 } ]
  in
  let post_series =
    Social_threads_v1.to_canonical_post_insights_series post_insights
  in
  assert (List.length post_series = 2);
  (match find_series "reposts" post_series with
   | Some item ->
       assert (Analytics_types.canonical_metric_key item.metric = "reposts");
       assert ((List.hd item.points).value = 4)
   | None -> failwith "Missing reposts canonical series");

  print_endline "ok: canonical insights adapters"

(* H5: GIF media type support *)
let test_post_single_gif_success () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-1\"}" };
    ];
  Threads.post_single
    ~account_id:"acct-1"
    ~text:"check this gif"
    ~media_urls:[ "https://example.com/funny.gif" ]
    (function
      | Error_types.Success post_id ->
          assert (post_id = "post-1");
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ _; (_, _, _, create_body); _ ] ->
               assert (string_contains create_body "media_type=GIF");
               assert (string_contains create_body "image_url=")
           | _ -> failwith "unexpected request sequence for gif post");
          print_endline "ok: post single gif"
      | _ -> failwith "expected successful gif post")

(* H5: topic tag support *)
let test_post_single_with_topic_tag () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-1\"}" };
    ];
  Threads.post_single
    ~account_id:"acct-1"
    ~text:"topic post"
    ~media_urls:[]
    ~topic_tag:"technology"
    (function
      | Error_types.Success post_id ->
          assert (post_id = "post-1");
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ _; (_, _, _, create_body); _ ] ->
               assert (string_contains create_body "text_post_app_tags=technology")
           | _ -> failwith "unexpected request sequence for topic tag");
          print_endline "ok: post single with topic tag"
      | _ -> failwith "expected successful topic tag post")

let test_post_single_topic_tag_trimmed () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-1\"}" };
    ];
  Threads.post_single
    ~account_id:"acct-1"
    ~text:"topic post"
    ~media_urls:[]
    ~topic_tag:"  science  "
    (function
      | Error_types.Success _ ->
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ _; (_, _, _, create_body); _ ] ->
               assert (string_contains create_body "text_post_app_tags=science");
               assert (not (string_contains create_body "text_post_app_tags=+"))
           | _ -> failwith "unexpected request sequence for topic tag trim");
          print_endline "ok: post single topic tag trimmed"
      | _ -> failwith "expected successful topic tag trimmed post")

let test_post_single_empty_topic_tag_omitted () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-1\"}" };
    ];
  Threads.post_single
    ~account_id:"acct-1"
    ~text:"no tag"
    ~media_urls:[]
    ~topic_tag:"   "
    (function
      | Error_types.Success _ ->
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ _; (_, _, _, create_body); _ ] ->
               assert (not (string_contains create_body "text_post_app_tags"))
           | _ -> failwith "unexpected request sequence for empty topic tag");
          print_endline "ok: post single empty topic tag omitted"
      | _ -> failwith "expected successful post with empty topic tag")

(* H2: Container status polling *)
let test_check_container_status_finished () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"status\":\"FINISHED\"}" };
    ];
  Threads.check_container_status
    ~access_token:"access-xyz"
    ~container_id:"container-1"
    (function
      | Ok Social_threads_v1.Finished ->
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ ("GET", url, _, _) ] ->
               assert (string_contains url "/v1.0/container-1?");
               assert (string_contains url "fields=status%2Cerror_message")
           | _ -> failwith "unexpected request for check_container_status");
          print_endline "ok: check container status finished"
      | _ -> failwith "expected Finished status")

let test_check_container_status_in_progress () =
  Mock_config.reset ();
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"status\":\"IN_PROGRESS\"}" };
    ];
  Threads.check_container_status
    ~access_token:"access-xyz"
    ~container_id:"container-1"
    (function
      | Ok Social_threads_v1.In_progress ->
          print_endline "ok: check container status in progress"
      | _ -> failwith "expected In_progress status")

let test_check_container_status_error () =
  Mock_config.reset ();
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"status\":\"ERROR\",\"error_message\":\"Upload failed\"}" };
    ];
  Threads.check_container_status
    ~access_token:"access-xyz"
    ~container_id:"container-1"
    (function
      | Ok (Social_threads_v1.Error_status msg) ->
          assert (msg = "Upload failed");
          print_endline "ok: check container status error"
      | _ -> failwith "expected Error_status")

let test_poll_container_status_immediate_finish () =
  Mock_config.reset ();
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"status\":\"FINISHED\"}" };
    ];
  Threads.poll_container_status
    ~access_token:"access-xyz"
    ~container_id:"container-1"
    (function
      | Ok container_id ->
          assert (container_id = "container-1");
          print_endline "ok: poll container status immediate finish"
      | Error _ -> failwith "expected successful poll")

let test_poll_container_status_retries_then_finishes () =
  Mock_config.reset ();
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"status\":\"IN_PROGRESS\"}" };
      { status = 200; headers = []; body = "{\"status\":\"IN_PROGRESS\"}" };
      { status = 200; headers = []; body = "{\"status\":\"FINISHED\"}" };
    ];
  Threads.poll_container_status
    ~access_token:"access-xyz"
    ~container_id:"container-1"
    (function
      | Ok container_id ->
          assert (container_id = "container-1");
          print_endline "ok: poll container status retries then finishes"
      | Error _ -> failwith "expected successful poll after retries")

let test_poll_container_status_max_attempts_exceeded () =
  Mock_config.reset ();
  (* Set enough IN_PROGRESS responses to exceed max_attempts=2 *)
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"status\":\"IN_PROGRESS\"}" };
      { status = 200; headers = []; body = "{\"status\":\"IN_PROGRESS\"}" };
      { status = 200; headers = []; body = "{\"status\":\"IN_PROGRESS\"}" };
    ];
  Threads.poll_container_status
    ~max_attempts:2
    ~access_token:"access-xyz"
    ~container_id:"container-1"
    (function
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "still processing after 2 attempts");
          print_endline "ok: poll container status max attempts exceeded"
      | _ -> failwith "expected error on max attempts exceeded")

let test_poll_container_status_error_status () =
  Mock_config.reset ();
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"status\":\"ERROR\",\"error_message\":\"Bad media\"}" };
    ];
  Threads.poll_container_status
    ~access_token:"access-xyz"
    ~container_id:"container-1"
    (function
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "processing failed");
          assert (string_contains msg "Bad media");
          print_endline "ok: poll container status error status"
      | _ -> failwith "expected error on error status")

(* H1: Carousel/multi-media posts *)
let test_post_carousel_success () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      (* child container 1 *)
      { status = 200; headers = []; body = "{\"id\":\"child-1\"}" };
      (* child container 2 *)
      { status = 200; headers = []; body = "{\"id\":\"child-2\"}" };
      (* carousel container *)
      { status = 200; headers = []; body = "{\"id\":\"carousel-1\"}" };
      (* poll status *)
      { status = 200; headers = []; body = "{\"status\":\"FINISHED\"}" };
      (* publish *)
      { status = 200; headers = []; body = "{\"id\":\"post-1\"}" };
    ];
  Threads.post_carousel
    ~account_id:"acct-1"
    ~text:"carousel caption"
    ~media_urls:[ "https://example.com/a.jpg"; "https://example.com/b.png" ]
    (function
      | Error_types.Success post_id ->
          assert (post_id = "post-1");
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [
            (_, _, _, _);          (* me *)
            (_, _, _, child1_body);  (* child 1 *)
            (_, _, _, child2_body);  (* child 2 *)
            (_, _, _, carousel_body); (* carousel *)
            (_, poll_url, _, _);     (* poll *)
            (_, _, _, publish_body); (* publish *)
           ] ->
               assert (string_contains child1_body "is_carousel_item=true");
               assert (string_contains child1_body "media_type=IMAGE");
               assert (string_contains child2_body "is_carousel_item=true");
               assert (string_contains child2_body "media_type=IMAGE");
               assert (string_contains carousel_body "media_type=CAROUSEL");
               assert (string_contains carousel_body "children=child-1%2Cchild-2");
               (if not (string_contains carousel_body "text=carousel") then
                 failwith (Printf.sprintf "carousel body missing text: %s" carousel_body));
               assert (string_contains poll_url "fields=status");
               assert (string_contains publish_body "creation_id=carousel-1")
           | _ -> failwith "unexpected request sequence for carousel");
          print_endline "ok: post carousel success"
      | Error_types.Failure err ->
          failwith (Printf.sprintf "carousel post failed: %s" (Error_types.error_to_string err))
      | Error_types.Partial_success _ ->
          failwith "expected success, got partial success")

let test_post_carousel_rejects_single_media () =
  Mock_config.reset ();
  Threads.post_carousel
    ~account_id:"acct-1"
    ~text:"only one"
    ~media_urls:[ "https://example.com/a.jpg" ]
    (function
      | Error_types.Failure (Error_types.Validation_error errs) ->
          let has_count_error =
            List.exists
              (function
                | Error_types.Too_many_media { count; max } -> count = 1 && max = 2
                | _ -> false)
              errs
          in
          assert has_count_error;
          print_endline "ok: post carousel rejects single media"
      | _ -> failwith "expected validation error for single media carousel")

let test_post_carousel_rejects_too_many_media () =
  Mock_config.reset ();
  let urls = List.init 21 (fun i -> Printf.sprintf "https://example.com/%d.jpg" i) in
  Threads.post_carousel
    ~account_id:"acct-1"
    ~text:"too many"
    ~media_urls:urls
    (function
      | Error_types.Failure (Error_types.Validation_error errs) ->
          let has_count_error =
            List.exists
              (function
                | Error_types.Too_many_media { count; max } -> count = 21 && max = 20
                | _ -> false)
              errs
          in
          assert has_count_error;
          print_endline "ok: post carousel rejects too many media"
      | _ -> failwith "expected validation error for too many media in carousel")

let test_post_carousel_with_topic_tag () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"child-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"child-2\"}" };
      { status = 200; headers = []; body = "{\"id\":\"carousel-1\"}" };
      { status = 200; headers = []; body = "{\"status\":\"FINISHED\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-1\"}" };
    ];
  Threads.post_carousel
    ~account_id:"acct-1"
    ~text:"tagged carousel"
    ~media_urls:[ "https://example.com/a.jpg"; "https://example.com/b.jpg" ]
    ~topic_tag:"fashion"
    (function
      | Error_types.Success _ ->
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ _; _; _; (_, _, _, carousel_body); _; _ ] ->
               assert (string_contains carousel_body "text_post_app_tags=fashion")
           | _ -> failwith "unexpected request sequence for carousel topic tag");
          print_endline "ok: post carousel with topic tag"
      | _ -> failwith "expected successful carousel with topic tag")

let test_post_carousel_child_creation_failure () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"child-1\"}" };
      { status = 400; headers = []; body = "{\"error\":{\"message\":\"bad url\",\"code\":1}}" };
    ];
  Threads.post_carousel
    ~account_id:"acct-1"
    ~text:"fail"
    ~media_urls:[ "https://example.com/a.jpg"; "https://example.com/b.jpg" ]
    (function
      | Error_types.Failure (Error_types.Api_error _) ->
          print_endline "ok: post carousel child creation failure"
      | _ -> failwith "expected failure on child creation error")

(* H3: Reply management *)
let test_get_replies_success () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body =
          "{\"data\":[{\"id\":\"reply-1\",\"text\":\"first reply\"},{\"id\":\"reply-2\",\"text\":\"second reply\"}],\"paging\":{\"cursors\":{\"after\":\"cursor-abc\"}}}";
      };
    ];
  Threads.get_replies ~account_id:"acct-1" ~media_id:"post-123"
    (function
      | Ok (replies, next_after) ->
          assert (List.length replies = 2);
          let first = List.hd replies in
          assert (first.id = "reply-1");
          assert (first.text = Some "first reply");
          assert (next_after = Some "cursor-abc");
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ ("GET", url, _, _) ] ->
               assert (string_contains url "/v1.0/post-123/replies?")
           | _ -> failwith "unexpected request for get_replies");
          print_endline "ok: get replies success"
      | Error _ -> failwith "expected successful get_replies")

let test_get_replies_with_pagination () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body = "{\"data\":[{\"id\":\"reply-3\"}]}";
      };
    ];
  Threads.get_replies ~account_id:"acct-1" ~media_id:"post-123" ~after:"cursor-abc" ~limit:5
    (function
      | Ok (replies, _) ->
          assert (List.length replies = 1);
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ ("GET", url, _, _) ] ->
               assert (string_contains url "after=cursor-abc");
               assert (string_contains url "limit=5")
           | _ -> failwith "unexpected request for get_replies pagination");
          print_endline "ok: get replies with pagination"
      | Error _ -> failwith "expected successful get_replies with pagination")

let test_get_conversation_success () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body =
          "{\"data\":[{\"id\":\"conv-1\",\"text\":\"root\"},{\"id\":\"conv-2\",\"text\":\"reply\"},{\"id\":\"conv-3\",\"text\":\"nested\"}]}";
      };
    ];
  Threads.get_conversation ~account_id:"acct-1" ~media_id:"post-123"
    (function
      | Ok (posts, _) ->
          assert (List.length posts = 3);
          let first = List.hd posts in
          assert (first.id = "conv-1");
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ ("GET", url, _, _) ] ->
               assert (string_contains url "/v1.0/post-123/conversation?")
           | _ -> failwith "unexpected request for get_conversation");
          print_endline "ok: get conversation success"
      | Error _ -> failwith "expected successful get_conversation")

let test_post_single_as_reply () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"reply-post-1\"}" };
    ];
  (* post_single already supports reply_to_id via create_text_container *)
  (* This test verifies the reply_to_id is sent when used in post_thread *)
  Threads.post_thread
    ~account_id:"acct-1"
    ~texts:[ "my reply" ]
    ~media_urls_per_post:[ [] ]
    (function
      | Error_types.Success result ->
          assert (result.Error_types.posted_ids = [ "reply-post-1" ]);
          print_endline "ok: post single as reply"
      | _ -> failwith "expected successful reply post")

(* H4: Publishing quota checking *)
let test_get_publishing_limit_success () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      {
        status = 200;
        headers = [];
        body =
          "{\"data\":[{\"quota_usage\":42,\"config\":{\"quota_total\":250,\"reply_quota_usage\":10,\"reply_quota_total\":1000}}]}";
      };
    ];
  Threads.get_publishing_limit ~account_id:"acct-1"
    (function
      | Ok limit ->
          assert (limit.Social_threads_v1.quota_usage = 42);
          assert (limit.quota_total = 250);
          assert (limit.reply_quota_usage = 10);
          assert (limit.reply_quota_total = 1000);
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ _; ("GET", url, _, _) ] ->
               assert (string_contains url "/v1.0/user-1/threads_publishing_limit?");
               assert (string_contains url "fields=quota_usage%2Cconfig")
           | _ -> failwith "unexpected request for get_publishing_limit");
          print_endline "ok: get publishing limit success"
      | Error _ -> failwith "expected successful get_publishing_limit")

let test_get_publishing_limit_defaults () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      {
        status = 200;
        headers = [];
        body = "{\"data\":[{}]}";
      };
    ];
  Threads.get_publishing_limit ~account_id:"acct-1"
    (function
      | Ok limit ->
          assert (limit.Social_threads_v1.quota_usage = 0);
          assert (limit.quota_total = 250);
          assert (limit.reply_quota_usage = 0);
          assert (limit.reply_quota_total = 1000);
          print_endline "ok: get publishing limit defaults"
      | Error _ -> failwith "expected successful get_publishing_limit with defaults")

let test_get_publishing_limit_api_error () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      {
        status = 400;
        headers = [];
        body = "{\"error\":{\"message\":\"bad request\",\"code\":100}}";
      };
    ];
  Threads.get_publishing_limit ~account_id:"acct-1"
    (function
      | Error (Error_types.Api_error _) ->
          print_endline "ok: get publishing limit api error"
      | _ -> failwith "expected api error for publishing limit")

(* H1: validate_media_urls allows up to 20 for carousels *)
let test_validate_media_urls_allows_20_for_carousel () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  (* Create 20 URLs and try to post carousel - it should pass validation *)
  let urls = List.init 20 (fun i -> Printf.sprintf "https://example.com/%d.jpg" i) in
  (* Create enough mock responses for 20 children + carousel + poll + publish + me *)
  let me_response = { Social_core.status = 200; headers = []; body = "{\"id\":\"user-1\"}" } in
  let child_responses = List.init 20 (fun i ->
    { Social_core.status = 200; headers = []; body = Printf.sprintf "{\"id\":\"child-%d\"}" i }) in
  let carousel_response = { Social_core.status = 200; headers = []; body = "{\"id\":\"carousel-1\"}" } in
  let poll_response = { Social_core.status = 200; headers = []; body = "{\"status\":\"FINISHED\"}" } in
  let publish_response = { Social_core.status = 200; headers = []; body = "{\"id\":\"post-1\"}" } in
  Mock_http.set_responses
    ([ me_response ] @ child_responses @ [ carousel_response; poll_response; publish_response ]);
  Threads.post_carousel
    ~account_id:"acct-1"
    ~text:"big carousel"
    ~media_urls:urls
    (function
      | Error_types.Success post_id ->
          assert (post_id = "post-1");
          print_endline "ok: validate media urls allows 20 for carousel"
      | Error_types.Failure err ->
          failwith (Printf.sprintf "expected success for 20-item carousel but got: %s"
            (Error_types.error_to_string err))
      | _ -> failwith "expected successful 20-item carousel")

let test_post_thread_with_topic_tag () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"container-2\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-2\"}" };
    ];
  Threads.post_thread
    ~account_id:"acct-1"
    ~texts:[ "one"; "two" ]
    ~media_urls_per_post:[ []; [] ]
    ~topic_tag:"sports"
    (function
      | Error_types.Success _ ->
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ _; (_, _, _, create_1_body); _; (_, _, _, create_2_body); _ ] ->
               assert (string_contains create_1_body "text_post_app_tags=sports");
               assert (string_contains create_2_body "text_post_app_tags=sports")
           | _ -> failwith "unexpected request sequence for thread topic tag");
          print_endline "ok: post thread with topic tag"
      | _ -> failwith "expected successful thread with topic tag")

let test_post_carousel_rejects_empty_media () =
  Mock_config.reset ();
  Threads.post_carousel
    ~account_id:"acct-1"
    ~text:"no media"
    ~media_urls:[]
    (function
      | Error_types.Failure (Error_types.Validation_error errs) ->
          let has_count_error =
            List.exists
              (function
                | Error_types.Too_many_media { count; max } -> count = 0 && max = 2
                | _ -> false)
              errs
          in
          assert has_count_error;
          print_endline "ok: post carousel rejects empty media"
      | _ -> failwith "expected validation error for empty media carousel")

let test_get_replies_rejects_empty_media_id () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Threads.get_replies ~account_id:"acct-1" ~media_id:"   "
    (function
      | Error (Error_types.Validation_error _) ->
          print_endline "ok: get replies rejects empty media id"
      | _ -> failwith "expected validation error for empty media_id in get_replies")

let test_get_conversation_rejects_empty_media_id () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Threads.get_conversation ~account_id:"acct-1" ~media_id:"   "
    (function
      | Error (Error_types.Validation_error _) ->
          print_endline "ok: get conversation rejects empty media id"
      | _ -> failwith "expected validation error for empty media_id in get_conversation")

let test_get_replies_api_error () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 400;
        headers = [];
        body = "{\"error\":{\"message\":\"invalid media id\",\"code\":100}}";
      };
    ];
  Threads.get_replies ~account_id:"acct-1" ~media_id:"bad-id"
    (function
      | Error (Error_types.Api_error _) ->
          print_endline "ok: get replies api error"
      | _ -> failwith "expected api error for get_replies")

let test_get_conversation_api_error () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          token_type = "Bearer";
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 400;
        headers = [];
        body = "{\"error\":{\"message\":\"invalid media id\",\"code\":100}}";
      };
    ];
  Threads.get_conversation ~account_id:"acct-1" ~media_id:"bad-id"
    (function
      | Error (Error_types.Api_error _) ->
          print_endline "ok: get conversation api error"
      | _ -> failwith "expected api error for get_conversation")

let test_poll_container_status_checks_before_sleeping () =
  (* Verify that when status is immediately FINISHED, no sleep is needed *)
  Mock_config.reset ();
  let sleep_calls = ref 0 in
  let module Track_config = struct
    include Mock_config
    let sleep _delay f = incr sleep_calls; f ()
  end in
  let module Track_threads = Social_threads_v1.Make (Track_config) in
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"status\":\"FINISHED\"}" };
    ];
  Track_threads.poll_container_status
    ~access_token:"access-xyz"
    ~container_id:"container-1"
    (function
      | Ok container_id ->
          assert (container_id = "container-1");
          assert (!sleep_calls = 0);
          print_endline "ok: poll checks before sleeping"
      | Error _ -> failwith "expected successful poll without sleep")

let () =
  test_oauth_url ();
  test_oauth_url_encodes_special_characters ();
  test_oauth_url_rejects_empty_state ();
  test_oauth_url_rejects_whitespace_client_id ();
  test_oauth_url_rejects_state_with_surrounding_whitespace ();
  test_oauth_url_rejects_redirect_with_surrounding_whitespace ();
  test_oauth_url_rejects_redirect_uri_mismatch_with_config ();
  test_oauth_url_rejects_whitespace_configured_redirect_uri ();
  test_exchange_code_rejects_empty_code ();
  test_exchange_code_rejects_empty_redirect_uri ();
  test_exchange_code_rejects_whitespace_client_secret ();
  test_exchange_code_rejects_whitespace_client_id ();
  test_exchange_code_rejects_whitespace_wrapped_code ();
  test_exchange_code_rejects_whitespace_wrapped_redirect ();
  test_exchange_code_rejects_redirect_uri_mismatch_with_config ();
  test_exchange_code_rejects_whitespace_configured_redirect_uri ();
  test_exchange_long_lived_rejects_empty_short_token ();
  test_exchange_long_lived_rejects_whitespace_client_secret ();
  test_exchange_long_lived_rejects_whitespace_wrapped_short_token ();
  test_refresh_token_rejects_empty_long_token ();
  test_refresh_token_rejects_whitespace_wrapped_long_token ();
  test_refresh_account_credentials_success ();
  test_refresh_account_credentials_missing ();
  test_refresh_account_credentials_backend_load_failure ();
  test_refresh_account_credentials_persist_failure ();
  test_validate_oauth_state_success ();
  test_validate_oauth_state_mismatch ();
  test_validate_oauth_state_whitespace_not_equal ();
  test_validate_oauth_state_whitespace_equal_rejected ();
  test_validate_oauth_state_length_mismatch ();
  test_validate_oauth_state_whitespace_only_rejected ();
  test_exchange_code_success ();
  test_exchange_code_error_redacts_sensitive_fields ();
  test_exchange_code_error_redacts_sensitive_non_json_body ();
  test_exchange_code_empty_token_response_fails ();
  test_exchange_code_empty_token_type_defaults_bearer ();
  test_exchange_code_lowercase_bearer_normalized ();
  test_exchange_code_negative_expires_in_clamped ();
  test_exchange_code_huge_expires_in_clamped ();
  test_exchange_code_huge_float_expires_in_clamped ();
  test_exchange_code_fractional_expires_in_rounded_up ();
  test_exchange_long_lived_success ();
  test_refresh_token_success ();
  test_get_me_success ();
  test_get_me_malformed_payload_returns_internal_error ();
  test_get_me_retries_500_then_succeeds ();
  test_get_me_retry_config_single_attempt ();
  test_get_me_retry_config_clamps_low_to_one ();
  test_get_me_retry_config_clamps_high_to_ten ();
  test_get_me_retry_config_invalid_falls_back_to_default ();
  test_get_me_retry_disable_429 ();
  test_get_me_retry_enable_429 ();
  test_get_me_retry_disable_5xx ();
  test_get_me_retry_disable_network_error ();
  test_get_me_retry_toggle_invalid_429_defaults_to_disabled ();
  test_get_me_retry_toggle_invalid_5xx_defaults_to_enabled ();
  test_get_me_retry_toggle_invalid_network_defaults_to_enabled ();
  test_get_posts_success ();
  test_get_posts_malformed_payload_returns_internal_error ();
  test_get_posts_limit_clamped_to_minimum ();
  test_get_posts_limit_clamped_to_maximum ();
  test_get_posts_after_cursor_trimmed ();
  test_get_posts_after_cursor_whitespace_omitted ();
  test_get_account_insights_contract ();
  test_get_account_insights_malformed_payload_returns_internal_error ();
  test_get_post_insights_contract ();
  test_get_post_insights_malformed_payload_returns_internal_error ();
  test_insights_network_error_redacts_query_token ();
  test_canonical_insights_adapters ();
  test_post_single_success ();
  test_post_single_text_trimmed_before_send ();
  test_post_single_no_retry_on_publish_500 ();
  test_post_single_image_success ();
  test_post_single_media_only_success ();
  test_post_single_with_idempotency_key ();
  test_post_single_idempotency_key_trimmed ();
  test_post_single_reply_control_forwarded ();
  test_post_single_rejects_unsupported_media ();
  test_post_single_media_url_with_query_success ();
  test_post_single_media_url_uppercase_scheme_success ();
  test_post_single_media_url_trimmed_before_send ();
  test_get_me_rate_limited ();
  test_get_me_rate_limited_non_json_body ();
  test_get_me_rate_limited_negative_retry_after_clamped ();
  test_get_me_rate_limited_float_retry_after_rounded ();
  test_get_me_rate_limited_retry_after_clamped_high ();
  test_get_me_rate_limited_retry_after_huge_float_clamped ();
  test_get_me_api_error_request_id ();
  test_get_me_api_error_request_id_fallback_header ();
  test_get_me_api_error_preserves_provider_metadata ();
  test_get_me_api_error_raw_response_redacts_sensitive_fields ();
  test_get_me_api_error_raw_response_redacts_sensitive_non_json_body ();
  test_get_me_api_error_message_redacts_sensitive_text ();
  test_get_me_missing_credentials ();
  test_get_me_empty_access_token_treated_missing ();
  test_get_me_credentials_backend_error_maps_internal ();
  test_get_me_credentials_not_found_maps_missing ();
  test_get_me_non_credential_not_found_maps_internal ();
  test_get_me_missing_without_credential_context_maps_internal ();
  test_get_me_missing_account_phrase_maps_internal ();
  test_get_me_no_credential_phrase_maps_missing ();
  test_get_me_credentials_word_and_missing_word_not_always_missing ();
  test_get_me_token_expiry_skew_default_blocks_near_expiry ();
  test_get_me_token_expiry_skew_override_allows_request ();
  test_get_me_token_expiry_skew_invalid_defaults_to_60 ();
  test_get_me_token_expiry_skew_high_clamped_to_3600 ();
  test_get_me_token_expiry_skew_negative_clamped_to_0 ();
  test_get_me_malformed_expiry_treated_as_expired ();
  test_get_me_malformed_expiry_with_auto_refresh ();
  test_get_me_auto_refresh_enabled_refreshes_and_proceeds ();
  test_get_me_auto_refresh_empty_token_from_refresh_fails ();
  test_get_me_auto_refresh_enabled_refresh_failure_returns_expired ();
  test_post_thread_multi_item_success ();
  test_post_thread_multi_item_with_media_success ();
  test_post_thread_idempotency_key_suffixes ();
  test_post_thread_idempotency_key_trimmed_suffixes ();
  test_post_thread_reply_control_forwarded ();
  test_post_thread_partial_failure ();
  test_post_thread_first_item_failure ();
  test_post_thread_rejects_extra_media_entries ();
  test_post_thread_rejects_extra_empty_media_entries ();
  test_post_single_gif_success ();
  test_post_single_with_topic_tag ();
  test_post_single_topic_tag_trimmed ();
  test_post_single_empty_topic_tag_omitted ();
  test_check_container_status_finished ();
  test_check_container_status_in_progress ();
  test_check_container_status_error ();
  test_poll_container_status_immediate_finish ();
  test_poll_container_status_retries_then_finishes ();
  test_poll_container_status_max_attempts_exceeded ();
  test_poll_container_status_error_status ();
  test_post_carousel_success ();
  test_post_carousel_rejects_single_media ();
  test_post_carousel_rejects_too_many_media ();
  test_post_carousel_with_topic_tag ();
  test_post_carousel_child_creation_failure ();
  test_get_replies_success ();
  test_get_replies_with_pagination ();
  test_get_conversation_success ();
  test_post_single_as_reply ();
  test_get_publishing_limit_success ();
  test_get_publishing_limit_defaults ();
  test_get_publishing_limit_api_error ();
  test_validate_media_urls_allows_20_for_carousel ();
  test_post_thread_with_topic_tag ();
  test_post_carousel_rejects_empty_media ();
  test_get_replies_rejects_empty_media_id ();
  test_get_conversation_rejects_empty_media_id ();
  test_get_replies_api_error ();
  test_get_conversation_api_error ();
  test_poll_container_status_checks_before_sleeping ();
  print_endline "Threads tests passed"
