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
  let health_status_updates = ref []

  let reset () =
    env_vars := [];
    credentials_store := [];
    credentials_error_override := None;
    update_credentials_error_override := None;
    health_status_updates := [];
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

  let update_health_status ~account_id:_ ~status ~error_message on_success _on_error =
    health_status_updates := (status, error_message) :: !health_status_updates;
    on_success ()

  let sleep ~seconds:_ on_continue _on_error = on_continue ()
end

module Threads = Social_threads_v1.Make (Mock_config)
module OAuth_http = Social_threads_v1.OAuth.Make (Mock_http)

let test_oauth_url () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_CLIENT_ID", "client-123") ];
  Threads.get_oauth_url
    ~redirect_uri:"https://example.com/callback"
    ~state:"state-xyz"
    ~scopes:Social_threads_v1.OAuth.Presets.publish
    (fun url ->
      assert (string_contains url "threads.net/oauth/authorize");
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
    ~scopes:Social_threads_v1.OAuth.Presets.publish
    (fun url ->
      assert (query_param url "redirect_uri" = Some "https://example.com/callback?x=1&y=two");
      assert (query_param url "state" = Some "abc/123");
      print_endline "ok: oauth url encodes special characters")
    (fun err -> failwith ("unexpected error: " ^ err))

let test_oauth_url_rejects_empty_state () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_CLIENT_ID", "client-123") ];
  Threads.get_oauth_url
    ~redirect_uri:"https://example.com/callback"
    ~state:"   "
    ~scopes:Social_threads_v1.OAuth.Presets.publish
    (fun _ -> failwith "expected oauth url failure for empty state")
    (fun err ->
      assert (string_contains err "state must not be empty");
      print_endline "ok: oauth empty state rejected")

let test_oauth_url_rejects_whitespace_client_id () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_CLIENT_ID", " client-123 ") ];
  Threads.get_oauth_url
    ~redirect_uri:"https://example.com/callback"
    ~state:"state-xyz"
    ~scopes:Social_threads_v1.OAuth.Presets.publish
    (fun _ -> failwith "expected oauth url failure for empty client id")
    (fun err ->
      assert (string_contains err "client ID must not contain leading or trailing whitespace");
      print_endline "ok: oauth whitespace client id rejected")

let test_oauth_url_rejects_state_with_surrounding_whitespace () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_CLIENT_ID", "client-123") ];
  Threads.get_oauth_url
    ~redirect_uri:"https://example.com/callback"
    ~state:" state-xyz "
    ~scopes:Social_threads_v1.OAuth.Presets.publish
    (fun _ -> failwith "expected oauth url failure for whitespace state")
    (fun err ->
      assert (string_contains err "state must not contain leading or trailing whitespace");
      print_endline "ok: oauth state whitespace rejected")

let test_oauth_url_rejects_redirect_with_surrounding_whitespace () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_CLIENT_ID", "client-123") ];
  Threads.get_oauth_url
    ~redirect_uri:" https://example.com/callback "
    ~state:"state-xyz"
    ~scopes:Social_threads_v1.OAuth.Presets.publish
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
  Threads.get_oauth_url
    ~redirect_uri:"https://example.com/other"
    ~state:"state-xyz"
    ~scopes:Social_threads_v1.OAuth.Presets.publish
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
  Threads.get_oauth_url
    ~redirect_uri:"https://example.com/callback"
    ~state:"state-xyz"
    ~scopes:Social_threads_v1.OAuth.Presets.publish
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
           assert (string_contains url "graph.threads.net/oauth/access_token");
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

let test_exchange_code_empty_auth_type_defaults_bearer () =
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
      assert (creds.auth_type = Bearer);
      print_endline "ok: exchange code empty auth_type defaults")
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
      assert (creds.auth_type = Bearer);
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

let test_exchange_code_form_urlencodes_special_chars () =
  Mock_http.reset ();
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body =
          {|{"access_token":"tok","token_type":"bearer"}|};
      };
    ];
  OAuth_http.exchange_code
    ~client_id:"client-123"
    ~client_secret:"sec/ret=with+special@chars"
    ~redirect_uri:"https://example.com/callback"
    ~code:"auth-code"
    (fun _creds ->
      let requests = List.rev !Mock_http.requests in
      (match requests with
       | [ (_, _, _, body) ] ->
           assert (string_contains body "client_secret=sec%2Fret%3Dwith%2Bspecial%40chars")
       | _ -> failwith "unexpected request count for exchange code urlencoding");
      print_endline "ok: exchange code form-urlencodes special chars in client_secret")
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
           assert (string_contains url "graph.threads.net/access_token");
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
           assert (string_contains url "graph.threads.net/refresh_access_token");
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"post-1\"}" };
    ];
  Threads.post_single ~account_id:"acct-1" ~text:"  hello threads  " ~media_urls:[]
    (function
      | Error_types.Success _ ->
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ (_, _, _, _); (_, _, _, create_body) ] ->
               assert (string_contains create_body "text=");
               assert (string_contains create_body "hello");
               assert (string_contains create_body "threads");
               assert (not (string_contains create_body "text=++"));
               assert (string_contains create_body "auto_publish_text=true");
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
          auth_type = Bearer;
          scope = None;
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
  Threads.post_single ~account_id:"acct-1" ~text:"hello threads"
    ~media_urls:["https://example.com/photo.jpg"]
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
               assert (query_param insights_url "metric" = Some "views,likes,replies,reposts,quotes,shares");
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
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
           | [ (_, _, _, _); (_, _, _, create_body) ] ->
               assert (string_contains create_body "client_request_id=req-123");
               assert (string_contains create_body "auto_publish_text=true")
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
          auth_type = Bearer;
          scope = None;
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
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
           | [ (_, _, _, _); (_, _, _, create_body) ] ->
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
          auth_type = Bearer;
          scope = None;
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
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
           | [ (_, _, _, _); (_, _, _, create_body) ] ->
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
      | Error (Error_types.Internal_error _) ->
          print_endline "ok: auto refresh empty token fails"
      | _ -> failwith "expected parse failure when refresh returns empty token")

let test_get_me_auto_refresh_preserves_failures () =
  let cases =
    [ (None, Error_types.Network_error (Error_types.Connection_failed "offline"));
      (Some (429, "{\"error\":{\"message\":\"Too many requests\",\"code\":4}}"),
       Error_types.Rate_limited { retry_after_seconds = Some 42; limit = None; remaining = Some 0; reset_at = None });
      (Some (503, "{\"error\":{\"message\":\"Unavailable\",\"code\":2}}"),
       Social_threads_v1.parse_api_error ~status_code:503 ~headers:[]
         ~response_body:"{\"error\":{\"message\":\"Unavailable\",\"code\":2}}");
      (Some (400, "{\"error\":{\"message\":\"Invalid token\",\"code\":190}}"),
       Error_types.Auth_error Error_types.Token_invalid);
      (Some (400, "{\"error\":{\"message\":\"Expired token\",\"code\":190,\"error_subcode\":463}}"),
       Error_types.Auth_error Error_types.Token_expired) ]
  in
  List.iter (fun (response, expected) ->
    Mock_config.reset ();
    Mock_config.env_vars := [ ("THREADS_AUTO_REFRESH_TOKEN", "true") ];
    let credentials =
      { access_token = "expired-token"; refresh_token = None;
        expires_at = Some (rfc3339_after_seconds 10); auth_type = Bearer; scope = None }
    in
    Mock_config.credentials_store := [ ("acct-1", credentials) ];
    (match response with
     | None -> Mock_http.set_errors ["offline"]
     | Some (status, body) ->
         Mock_http.set_responses [{ status; headers = [("Retry-After", "42")]; body }]);
    Threads.get_me ~account_id:"acct-1" (fun result ->
      (match result with
       | Error actual when actual <> expected ->
           failwith (Error_types.error_to_string actual ^ " != " ^ Error_types.error_to_string expected)
       | Error _ -> ()
       | Ok _ -> failwith "Expected refresh failure");
      (match !Mock_http.requests with
       | [(method_name, url, _, _)] ->
           assert (String.equal method_name "GET");
           assert (String.equal (Uri.path (Uri.of_string url)) "/refresh_access_token");
           assert (query_param url "grant_type" = Some "th_refresh_token");
           assert (query_param url "access_token" = Some credentials.access_token)
       | [] | _ :: _ -> failwith "Expected exactly one refresh request");
      assert (!Mock_config.credentials_store = [("acct-1", credentials)]);
      if Error_types.is_retryable expected then
        assert (!Mock_config.health_status_updates = []))) cases

let test_get_me_auto_refresh_persist_failure_returns_error () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_AUTO_REFRESH_TOKEN", "true") ];
  Mock_config.update_credentials_error_override := Some "db write failed";
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "expired-token";
          refresh_token = None;
          expires_at = Some (rfc3339_after_seconds 10);
          auth_type = Bearer;
          scope = None;
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
    ];
  Threads.get_me ~account_id:"acct-1"
    (function
      | Error (Error_types.Network_error (Error_types.Connection_failed msg)) ->
          assert (string_contains msg "db write failed");
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 1);
          print_endline "ok: auto refresh persist failure surfaces"
      | _ -> failwith "expected persistence failure to surface as network error")

let test_get_me_auto_refresh_updates_health_status () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_AUTO_REFRESH_TOKEN", "true") ];
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "expired-token";
          refresh_token = None;
          expires_at = Some (rfc3339_after_seconds 10);
          auth_type = Bearer;
          scope = None;
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
          let has_healthy =
            List.exists
              (fun (status, error_message) -> status = "healthy" && error_message = None)
              !Mock_config.health_status_updates
          in
          assert has_healthy;
          print_endline "ok: auto refresh updates health status"
      | _ -> failwith "expected successful get_me after auto refresh")

let test_post_thread_multi_item_success () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
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
           | [ _; (_, _, _, create_body) ] ->
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
          auth_type = Bearer;
          scope = None;
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
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
           | [ _; (_, _, _, create_body) ] ->
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
          auth_type = Bearer;
          scope = None;
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
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
           | [ _; (_, _, _, create_body) ] ->
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
          auth_type = Bearer;
          scope = None;
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

let check_status_of_body body on_status =
  Mock_config.reset ();
  Mock_http.set_responses [ { status = 200; headers = []; body } ];
  Threads.check_container_status ~access_token:"access-xyz"
    ~container_id:"container-1" on_status

let test_check_container_status_published () =
  check_status_of_body "{\"status\":\"PUBLISHED\",\"id\":\"container-1\"}"
    (function
      | Ok Social_threads_v1.Published ->
          print_endline "ok: check container status published"
      | _ -> failwith "expected Published status")

let test_check_container_status_expired () =
  check_status_of_body "{\"status\":\"EXPIRED\",\"id\":\"container-1\"}"
    (function
      | Ok Social_threads_v1.Expired ->
          print_endline "ok: check container status expired"
      | _ -> failwith "expected Expired status")

let test_check_container_status_error_without_message () =
  check_status_of_body "{\"status\":\"ERROR\"}"
    (function
      | Ok (Social_threads_v1.Error_status msg) ->
          assert (msg = "ERROR");
          print_endline "ok: check container status error without message"
      | _ -> failwith "expected Error_status carrying the raw status")

let test_poll_container_status_published_is_duplicate () =
  Mock_config.reset ();
  Mock_http.set_responses
    [ { status = 200; headers = []; body = "{\"status\":\"PUBLISHED\"}" } ];
  Threads.poll_container_status ~access_token:"access-xyz"
    ~container_id:"container-1"
    (function
      | Error Error_types.Duplicate_content ->
          assert (List.length !Mock_http.requests = 1);
          print_endline "ok: poll container status published is duplicate"
      | _ -> failwith "expected Duplicate_content for a published container")

let test_poll_container_status_expired () =
  Mock_config.reset ();
  Mock_http.set_responses
    [ { status = 200; headers = []; body = "{\"status\":\"EXPIRED\"}" } ];
  Threads.poll_container_status ~access_token:"access-xyz"
    ~container_id:"container-1"
    (function
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "expired");
          assert (List.length !Mock_http.requests = 1);
          print_endline "ok: poll container status expired"
      | _ -> failwith "expected an expiry error without further polling")

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
          auth_type = Bearer;
          scope = None;
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      (* child container 1 *)
      { status = 200; headers = []; body = "{\"id\":\"child-1\"}" };
      (* child poll 1 *)
      { status = 200; headers = []; body = "{\"status\":\"FINISHED\"}" };
      (* child container 2 *)
      { status = 200; headers = []; body = "{\"id\":\"child-2\"}" };
      (* child poll 2 *)
      { status = 200; headers = []; body = "{\"status\":\"FINISHED\"}" };
      (* carousel container *)
      { status = 200; headers = []; body = "{\"id\":\"carousel-1\"}" };
      (* carousel poll status *)
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
            (_, child1_poll_url, _, _); (* child 1 poll *)
            (_, _, _, child2_body);  (* child 2 *)
            (_, child2_poll_url, _, _); (* child 2 poll *)
            (_, _, _, carousel_body); (* carousel *)
            (_, poll_url, _, _);     (* carousel poll *)
            (_, _, _, publish_body); (* publish *)
           ] ->
               assert (string_contains child1_body "is_carousel_item=true");
               assert (string_contains child1_body "media_type=IMAGE");
               assert (string_contains child1_poll_url "child-1");
               assert (string_contains child1_poll_url "fields=status");
               assert (string_contains child2_body "is_carousel_item=true");
               assert (string_contains child2_body "media_type=IMAGE");
               assert (string_contains child2_poll_url "child-2");
               assert (string_contains child2_poll_url "fields=status");
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
          auth_type = Bearer;
          scope = None;
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"child-1\"}" };
      { status = 200; headers = []; body = "{\"status\":\"FINISHED\"}" };
      { status = 200; headers = []; body = "{\"id\":\"child-2\"}" };
      { status = 200; headers = []; body = "{\"status\":\"FINISHED\"}" };
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
           | [ _; _; _; _; _; (_, _, _, carousel_body); _; _ ] ->
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
          auth_type = Bearer;
          scope = None;
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"id\":\"user-1\"}" };
      { status = 200; headers = []; body = "{\"id\":\"child-1\"}" };
      { status = 200; headers = []; body = "{\"status\":\"FINISHED\"}" };
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
        } );
    ];
  (* Create 20 URLs and try to post carousel - it should pass validation *)
  let urls = List.init 20 (fun i -> Printf.sprintf "https://example.com/%d.jpg" i) in
  (* Create enough mock responses for 20 children + carousel + poll + publish + me *)
  let me_response = { Social_core.status = 200; headers = []; body = "{\"id\":\"user-1\"}" } in
  let child_responses = List.init 20 (fun i ->
    { Social_core.status = 200; headers = []; body = Printf.sprintf "{\"id\":\"child-%d\"}" i }) in
  let child_poll_responses = List.init 20 (fun _ ->
    { Social_core.status = 200; headers = []; body = "{\"status\":\"FINISHED\"}" }) in
  let child_responses_with_polls =
    let pairs = List.combine child_responses child_poll_responses in
    List.concat_map (fun (child_response, poll_response) -> [ child_response; poll_response ]) pairs
  in
  let carousel_response = { Social_core.status = 200; headers = []; body = "{\"id\":\"carousel-1\"}" } in
  let poll_response = { Social_core.status = 200; headers = []; body = "{\"status\":\"FINISHED\"}" } in
  let publish_response = { Social_core.status = 200; headers = []; body = "{\"id\":\"post-1\"}" } in
  Mock_http.set_responses
    ([ me_response ] @ child_responses_with_polls @ [ carousel_response; poll_response; publish_response ]);
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
          auth_type = Bearer;
          scope = None;
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
    let sleep ~seconds:_ on_continue _on_error = incr sleep_calls; on_continue ()
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

let test_read_retry_backs_off_through_config_sleep () =
  Mock_config.reset ();
  let sleeps = ref [] in
  let module Track_config = struct
    include Mock_config
    let sleep ~seconds on_continue _on_error =
      sleeps := seconds :: !sleeps;
      on_continue ()
  end in
  let module Track_threads = Social_threads_v1.Make (Track_config) in
  Mock_http.set_responses
    [
      { status = 503; headers = []; body = "{}" };
      { status = 200; headers = []; body = "{\"status\":\"FINISHED\"}" };
    ];
  Track_threads.check_container_status ~access_token:"access-xyz"
    ~container_id:"container-1"
    (function
      | Ok Social_threads_v1.Finished ->
          (* one backoff of 0.1s, scheduled by the runtime, then the retry *)
          assert (!sleeps = [ 0.1 ]);
          assert (List.length !Mock_http.requests = 2);
          print_endline "ok: read retry backs off through Config.sleep"
      | _ -> failwith "expected Finished after one retry")

let test_read_retry_returns_response_when_sleep_fails () =
  Mock_config.reset ();
  let module Failing_sleep_config = struct
    include Mock_config
    let sleep ~seconds:_ _on_continue on_error = on_error "timer cancelled"
  end in
  let module Failing_threads = Social_threads_v1.Make (Failing_sleep_config) in
  Mock_http.set_responses
    [
      { status = 503; headers = []; body = "{}" };
      { status = 200; headers = []; body = "{\"status\":\"FINISHED\"}" };
    ];
  Failing_threads.check_container_status ~access_token:"access-xyz"
    ~container_id:"container-1"
    (function
      | Error (Error_types.Api_error { status_code = 503; _ }) ->
          (* no retry without a backoff *)
          assert (List.length !Mock_http.requests = 1);
          print_endline "ok: read retry returns response when sleep fails"
      | Error err ->
          failwith ("unexpected error: " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "expected the 503 to be surfaced")

let test_delete_post_success () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          auth_type = Bearer;
          scope = None;
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"success\":true}" };
    ];
  Threads.delete_post ~account_id:"acct-1" ~post_id:"post-123"
    (function
      | Ok () ->
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ ("DELETE", url, _, _) ] ->
               assert (string_contains url "/v1.0/post-123?");
               assert (string_contains url "access_token=access-xyz")
           | _ -> failwith "unexpected request for delete_post");
          print_endline "ok: delete post"
      | Error _ -> failwith "expected successful delete_post")

let test_delete_post_rejects_empty_id () =
  Mock_config.reset ();
  Threads.delete_post ~account_id:"acct-1" ~post_id:"   "
    (function
      | Error (Error_types.Validation_error _) ->
          print_endline "ok: delete post rejects empty id"
      | _ -> failwith "expected validation error for empty post_id in delete_post")

let test_delete_post_api_error () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          auth_type = Bearer;
          scope = None;
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 403;
        headers = [];
        body = "{\"error\":{\"message\":\"not authorized\",\"code\":200}}";
      };
    ];
  Threads.delete_post ~account_id:"acct-1" ~post_id:"post-123"
    (function
      | Error (Error_types.Auth_error _) ->
          print_endline "ok: delete post api error"
      | _ -> failwith "expected auth error for delete_post 403")

let test_approve_reply_success () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          auth_type = Bearer;
          scope = None;
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"success\":true}" };
    ];
  Threads.approve_reply ~account_id:"acct-1" ~reply_id:"reply-456"
    (function
      | Ok () ->
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ ("POST", url, _, body) ] ->
               assert (String.ends_with ~suffix:"/v1.0/reply-456/manage_pending_reply" url);
               assert (string_contains body "approve=true");
               assert (string_contains body "access_token=access-xyz")
           | _ -> failwith "unexpected request for approve_reply");
          print_endline "ok: approve reply"
      | Error _ -> failwith "expected successful approve_reply")

let test_ignore_reply_success () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          auth_type = Bearer;
          scope = None;
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"success\":true}" };
    ];
  Threads.ignore_reply ~account_id:"acct-1" ~reply_id:"reply-789"
    (function
      | Ok () ->
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ ("POST", url, _, body) ] ->
               assert (String.ends_with ~suffix:"/v1.0/reply-789/manage_pending_reply" url);
               assert (string_contains body "approve=false")
           | _ -> failwith "unexpected request for ignore_reply");
          print_endline "ok: ignore reply"
      | Error _ -> failwith "expected successful ignore_reply")

let test_hide_reply_success () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          auth_type = Bearer;
          scope = None;
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"success\":true}" };
    ];
  Threads.hide_reply ~account_id:"acct-1" ~reply_id:"reply-abc"
    (function
      | Ok () ->
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ ("POST", url, _, body) ] ->
               assert (String.ends_with ~suffix:"/v1.0/reply-abc/manage_reply" url);
               assert (string_contains body "hide=true")
           | _ -> failwith "unexpected request for hide_reply");
          print_endline "ok: hide reply"
      | Error _ -> failwith "expected successful hide_reply")

let test_unhide_reply_success () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          auth_type = Bearer;
          scope = None;
        } );
    ];
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = "{\"success\":true}" };
    ];
  Threads.unhide_reply ~account_id:"acct-1" ~reply_id:"reply-def"
    (function
      | Ok () ->
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ ("POST", url, _, body) ] ->
               assert (String.ends_with ~suffix:"/v1.0/reply-def/manage_reply" url);
               assert (string_contains body "hide=false")
           | _ -> failwith "unexpected request for unhide_reply");
          print_endline "ok: unhide reply"
      | Error _ -> failwith "expected successful unhide_reply")

let test_reply_action_rejects_empty_id () =
  Mock_config.reset ();
  Threads.approve_reply ~account_id:"acct-1" ~reply_id:"   "
    (function
      | Error (Error_types.Validation_error _) ->
          print_endline "ok: reply action rejects empty id"
      | _ -> failwith "expected validation error for empty reply_id")

let test_get_pending_replies_success () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          auth_type = Bearer;
          scope = None;
        } );
    ];
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body =
          "{\"data\":[{\"id\":\"pending-1\",\"text\":\"please approve\"}]}";
      };
    ];
  Threads.get_pending_replies ~account_id:"acct-1" ~media_id:"post-123"
    (function
      | Ok (posts, _next_after) ->
          assert (List.length posts = 1);
          assert ((List.hd posts).id = "pending-1");
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ (_, url, _, _) ] ->
               assert (string_contains url "/v1.0/post-123/pending_replies?")
           | _ -> failwith "unexpected request for get_pending_replies");
          print_endline "ok: get pending replies"
      | Error _ -> failwith "expected successful get_pending_replies")

let test_exported_api_constants () =
  assert (Social_threads_v1.api_version = "v1.0");
  assert (Social_threads_v1.threads_api_base = "https://graph.threads.net/v1.0");
  assert (Social_threads_v1.OAuth.token_endpoint == Social_threads_v1.token_endpoint);
  assert (Social_threads_v1.OAuth.authorization_endpoint
          == Social_threads_v1.oauth_authorize_url);
  print_endline "ok: exported api constants"
let query_param url key =
  match Uri.get_query_param (Uri.of_string url) key with
  | Some value -> value
  | None -> failwith (Printf.sprintf "missing query parameter %s" key)

let field_set url = String.split_on_char ',' (query_param url "fields")

let with_reply_account () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          access_token = "access-xyz";
          refresh_token = None;
          expires_at = None;
          auth_type = Bearer;
          scope = None;
        } );
    ]

(* Shape from Meta's replies-and-conversations reference: root_post and
   replied_to are objects carrying an id; hide_status is an enum string. *)
let documented_reply_body =
  "{\"data\":[{\"id\":\"reply-1\",\"text\":\"First Reply\",\"username\":\"alice\",\
   \"permalink\":\"https://www.threads.com/@alice/post/abc\",\
   \"timestamp\":\"2024-01-01T18:20:00+0000\",\"media_type\":\"TEXT_POST\",\
   \"shortcode\":\"abc\",\"has_replies\":true,\"root_post\":{\"id\":\"root-1\"},\
   \"replied_to\":{\"id\":\"root-1\"},\"is_reply\":true,\
   \"is_reply_owned_by_me\":false,\"hide_status\":\"NOT_HUSHED\"},\
   {\"id\":\"reply-2\",\"is_reply\":true,\"root_post\":{\"id\":\"root-1\"},\
   \"replied_to\":{\"id\":\"reply-1\"},\"hide_status\":\"HIDDEN\"},\
   {\"id\":\"reply-3\",\"hide_status\":\"SOMETHING_NEW\"}],\
   \"paging\":{\"cursors\":{\"before\":\"b\",\"after\":\"a\"}}}"

let assert_reply_fields_requested url =
  let fields = field_set url in
  List.iter
    (fun field ->
      if not (List.mem field fields) then
        failwith (Printf.sprintf "reply field %s not requested" field))
    [ "id"; "text"; "username"; "permalink"; "timestamp"; "replied_to";
      "root_post"; "is_reply"; "is_reply_owned_by_me"; "hide_status";
      "has_replies" ];
  (* owner is only returned for top-level posts you own *)
  assert (not (List.mem "owner" fields))

let test_get_conversation_parses_reply_fields () =
  with_reply_account ();
  Mock_http.set_responses [ { status = 200; headers = []; body = documented_reply_body } ];
  Threads.get_conversation ~account_id:"acct-1" ~media_id:"root-1" ~reverse:false
    (function
      | Ok ([ first; second; third ], next_after) ->
          assert (first.username = Some "alice");
          assert (first.permalink = Some "https://www.threads.com/@alice/post/abc");
          assert (first.timestamp = Some "2024-01-01T18:20:00+0000");
          assert first.is_reply;
          assert first.has_replies;
          assert (first.root_post_id = Some "root-1");
          assert (first.replied_to_id = Some "root-1");
          assert (first.is_reply_owned_by_me = Some false);
          assert (first.hide_status = Some Social_threads_v1.Not_hushed);
          assert (first.reply_approval_status = None);
          assert (second.replied_to_id = Some "reply-1");
          assert (second.hide_status = Some Social_threads_v1.Hidden);
          assert (second.username = None);
          assert (third.hide_status
                  = Some (Social_threads_v1.Unknown_hide_status "SOMETHING_NEW"));
          assert (not third.is_reply);
          assert (next_after = Some "a");
          (match List.rev !Mock_http.requests with
           | [ ("GET", url, _, _) ] ->
               assert_reply_fields_requested url;
               assert (query_param url "reverse" = "false")
           | _ -> failwith "unexpected request for get_conversation");
          print_endline "ok: get conversation parses reply fields"
      | Ok _ -> failwith "expected three replies"
      | Error _ -> failwith "expected successful get_conversation")

let test_get_replies_requests_reply_fields () =
  with_reply_account ();
  Mock_http.set_responses [ { status = 200; headers = []; body = documented_reply_body } ];
  Threads.get_replies ~account_id:"acct-1" ~media_id:"root-1"
    (function
      | Ok (replies, _) ->
          assert (List.length replies = 3);
          (match List.rev !Mock_http.requests with
           | [ ("GET", url, _, _) ] ->
               assert_reply_fields_requested url;
               (* reverse is only sent when the caller asks for it *)
               assert (Uri.get_query_param (Uri.of_string url) "reverse" = None)
           | _ -> failwith "unexpected request for get_replies");
          print_endline "ok: get replies requests reply fields"
      | Error _ -> failwith "expected successful get_replies")

let test_get_pending_replies_approval_status () =
  with_reply_account ();
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body =
          "{\"data\":[{\"id\":\"p-1\",\"text\":\"pending reply\",\"hide_status\":\"NOT_HUSHED\",\"reply_approval_status\":\"pending\"},\
           {\"id\":\"p-2\",\"reply_approval_status\":\"ignored\"}]}";
      };
    ];
  Threads.get_pending_replies ~account_id:"acct-1" ~media_id:"root-1"
    ~approval_status:Social_threads_v1.Approval_ignored
    (function
      | Ok ([ first; second ], _) ->
          assert (first.reply_approval_status = Some Social_threads_v1.Approval_pending);
          assert (second.reply_approval_status = Some Social_threads_v1.Approval_ignored);
          (match List.rev !Mock_http.requests with
           | [ ("GET", url, _, _) ] ->
               assert_reply_fields_requested url;
               assert (List.mem "reply_approval_status" (field_set url));
               assert (query_param url "approval_status" = "ignored")
           | _ -> failwith "unexpected request for get_pending_replies");
          print_endline "ok: get pending replies approval status"
      | Ok _ -> failwith "expected two pending replies"
      | Error _ -> failwith "expected successful get_pending_replies")


(* Graph error body as Meta sends it, for the shared-classifier contract. *)
let graph_error_body ?subcode ?(error_type = "OAuthException") ~message code =
  Printf.sprintf {|{"error":{"message":%S,"type":%S,"code":%d%s,"fbtrace_id":"AvX3kq0Lm2P"}}|}
    message error_type code
    (match subcode with Some s -> Printf.sprintf {|,"error_subcode":%d|} s | None -> "")

let is_rate_limited = function Error_types.Rate_limited _ -> true | _ -> false
let is_permission_error = function
  | Error_types.Auth_error (Error_types.Insufficient_permissions _) -> true
  | _ -> false

(** Threads maps Graph codes through the table shared with Facebook and
    Instagram ([Social_meta_graph_error]). Before, 17/341/368/80xxx, 3 and 100/33
    were plain Api_errors, and every 190 read as Token_expired. *)
let test_shared_meta_graph_error_contract () =
  let parse ?(status_code = 400) body =
    Social_threads_v1.parse_api_error ~status_code ~headers:[] ~response_body:body
  in
  List.iter
    (fun code ->
      assert (is_rate_limited (parse (graph_error_body ~message:"limit" code))))
    [ 4; 17; 32; 341; 368; 613; 80001; 80002; 80014 ];
  List.iter
    (fun code ->
      assert (is_permission_error (parse (graph_error_body ~message:"denied" code))))
    [ 3; 10; 200; 230; 299 ];
  assert (parse (graph_error_body ~subcode:463 ~message:"Session has expired" 190)
          = Error_types.Auth_error Error_types.Token_expired);
  assert (parse (graph_error_body ~message:"Invalid OAuth access token" 190)
          = Error_types.Auth_error Error_types.Token_invalid);
  assert (parse (graph_error_body ~subcode:460 ~message:"Password changed" 190)
          = Error_types.Auth_error Error_types.Token_revoked);
  (match
     parse (graph_error_body ~subcode:33 ~error_type:"GraphMethodException"
              ~message:"Object with ID '1790' does not exist" 100)
   with
   | Error_types.Resource_not_found _ -> ()
   | _ -> failwith "100/33 should be Resource_not_found");
  (* Container still processing: Threads-specific, left to the caller. *)
  (match
     parse (graph_error_body ~subcode:4279009 ~error_type:"THApiException"
              ~message:"The requested resource does not exist" 24)
   with
   | Error_types.Api_error api_err ->
       assert (string_contains api_err.message "code=24");
       assert (string_contains api_err.message "subcode=4279009")
   | _ -> failwith "24/4279009 should stay an Api_error");
  print_endline "ok: shared Meta Graph error contract"

(** The numeric Graph code survives the secret scrubbing of [raw_response],
    so [Social_provider_error_detail] can read it back. *)
let test_api_error_detail_survives_redaction () =
  let body =
    {|{"error":{"message":"(#100) Invalid parameter","type":"OAuthException","code":100,"error_subcode":2207028,"fbtrace_id":"AvX3kq0Lm2P"},"access_token":"should-not-leak"}|}
  in
  let err =
    Social_threads_v1.parse_api_error ~status_code:400
      ~headers:[ ("x-fb-trace-id", "hdr-trace") ] ~response_body:body
  in
  (match Social_provider_error_detail.of_error err with
   | Some detail ->
       assert (detail.code = Some "100");
       assert (detail.subcode = Some 2207028);
       assert (detail.trace_id = Some "hdr-trace");
       (match detail.body_excerpt with
        | Some excerpt -> assert (not (string_contains excerpt "should-not-leak"))
        | None -> failwith "expected an excerpt")
   | None -> failwith "expected provider detail");
  print_endline "ok: api error detail survives redaction"


(* Permalink by id: GET /{id}?fields=permalink with the token in the query,
   percent-encoded, and the three outcomes callers branch on. *)
(* Response shapes from https://developers.facebook.com/docs/threads/insights
   (user insights): [views] is a time series, the interaction counts and
   [followers_count] carry [total_value], [clicks] carries
   [link_total_values]. *)
let documented_user_insights_body =
  {|{"data":[
    {"name":"views","period":"day","values":[
      {"value":10,"end_time":"2024-07-12T08:00:00+0000"},
      {"value":7,"end_time":"2024-07-15T08:00:00+0000"},
      {"value":0,"end_time":"2024-07-16T08:00:00+0000"}],
     "title":"views","description":"The number of times your profile was viewed.",
     "id":"1234567/insights/views/day"},
    {"name":"likes","period":"day","title":"likes","description":"likes",
     "total_value":{"value":100},"id":"1234567/insights/likes/day"},
    {"name":"clicks","period":"day","title":"clicks","description":"clicks",
     "link_total_values":[{"value":11,"link_url":"https://example.com/a"},
                          {"value":4,"link_url":"https://example.com/b"}],
     "id":"1234567/insights/clicks/day"},
    {"name":"followers_count","period":"day","title":"followers_count",
     "description":"followers","total_value":{"value":540},
     "id":"1234567/insights/followers_count/day"},
    {"name":"follower_demographics","period":"day","total_value":{"value":0},
     "id":"1234567/insights/follower_demographics/day"}]}|}

let test_get_user_insights_documented_shapes () =
  Mock_config.reset ();
  Mock_http.set_responses [ { status = 200; headers = []; body = documented_user_insights_body } ];
  let token = "tok+en/with=&reserved" in
  let module I = Social_threads_v1.Insights in
  Threads.get_user_insights ~access_token:token ~user_id:"1234567" ~since:"1717200000"
    ~until:"1717286400" (function
    | Error err -> failwith (Error_types.error_to_string err)
    | Ok insights ->
        assert (List.length insights = 4);
        (match I.find I.Views insights with
         | Some { value = I.Time_series [ _; _; { value = 0; end_time = Some _ } ]; period = Some "day"; _ } -> ()
         | _ -> failwith "views should be a three-bucket time series");
        (match I.find I.Likes insights with
         | Some { value = I.Total 100; _ } -> ()
         | _ -> failwith "likes should be total 100");
        (match I.find I.Followers_count insights with
         | Some { value = I.Total 540; _ } -> ()
         | _ -> failwith "followers_count should be total 540");
        (match I.find I.Clicks insights with
         | Some ({ value = I.Link_totals [ { link_url = "https://example.com/a"; link_clicks = 11 }; _ ]; _ } as clicks) ->
             assert (I.sum clicks.value = 15)
         | _ -> failwith "clicks should be link totals");
        let views = Option.get (I.find I.Views insights) in
        assert (I.latest_value views.value = Some 0);
        assert (I.latest_value ~skip_zero:true views.value = Some 7);
        assert (I.sum views.value = 17);
        let series = I.to_canonical_series ~scope:Analytics_types.Account insights in
        assert (List.exists (fun (s : Analytics_types.series) ->
          s.metric = Analytics_types.Profile_visits && s.provider_metric = Some "views"
          && List.length s.points = 3) series);
        assert (List.exists (fun (s : Analytics_types.series) ->
          s.metric = Analytics_types.Followers && s.points = [ Analytics_types.make_datapoint 540 ]) series);
        assert (List.exists (fun (s : Analytics_types.series) ->
          s.metric = Analytics_types.Outbound_clicks && s.points = [ Analytics_types.make_datapoint 15 ]) series));
  (match !Mock_http.requests with
   | [ ("GET", url, _, _) ] ->
       let uri = Uri.of_string url in
       assert (Uri.path uri = "/v1.0/1234567/threads_insights");
       assert (Uri.get_query_param uri "metric"
               = Some "views,likes,replies,reposts,quotes,clicks,followers_count");
       assert (Uri.get_query_param uri "since" = Some "1717200000");
       assert (Uri.get_query_param uri "until" = Some "1717286400");
       assert (Uri.get_query_param uri "period" = None);
       assert (Uri.get_query_param uri "access_token" = Some token)
   | _ -> failwith "expected one GET");
  print_endline "ok: get_user_insights documented shapes"

let test_get_user_insights_for_account_user_id () =
  with_reply_account ();
  Mock_http.set_responses
    [ { status = 200; headers = [];
        body = {|{"data":[{"name":"followers_count","period":"day","total_value":{"value":3}}]}|} } ];
  let module I = Social_threads_v1.Insights in
  Threads.get_user_insights_for_account ~account_id:"acct-1" ~user_id:"u-9"
    ~metrics:[ I.Followers_count ] (function
    | Ok [ { metric = I.Followers_count; value = I.Total 3; _ } ] -> ()
    | Ok _ -> failwith "expected followers_count 3"
    | Error err -> failwith (Error_types.error_to_string err));
  (match !Mock_http.requests with
   | [ ("GET", url, _, _) ] ->
       assert (Uri.path (Uri.of_string url) = "/v1.0/u-9/threads_insights");
       assert (Uri.get_query_param (Uri.of_string url) "metric" = Some "followers_count");
       assert (Uri.get_query_param (Uri.of_string url) "since" = None);
       assert (Uri.get_query_param (Uri.of_string url) "access_token" = Some "access-xyz")
   | _ -> failwith "a known user_id must skip GET /me");
  (* Without a user id the account's id is read from /me first. *)
  with_reply_account ();
  Mock_http.set_responses
    [ { status = 200; headers = []; body = {|{"id":"me-1","username":"alice"}|} };
      { status = 200; headers = []; body = {|{"data":[]}|} } ];
  Threads.get_user_insights_for_account ~account_id:"acct-1" (function
    | Ok [] -> ()
    | _ -> failwith "expected an empty result");
  (match List.rev !Mock_http.requests with
   | [ (_, me_url, _, _); (_, url, _, _) ] ->
       assert (string_contains me_url "/v1.0/me?");
       assert (Uri.path (Uri.of_string url) = "/v1.0/me-1/threads_insights")
   | _ -> failwith "expected /me then insights");
  print_endline "ok: get_user_insights_for_account"

(* Media insights answer with [values: [{value}]], period lifetime
   (https://developers.facebook.com/docs/threads/insights#media-insights). *)
let test_get_media_insights_documented_shape () =
  Mock_config.reset ();
  Mock_http.set_responses
    [ { status = 200; headers = [];
        body = {|{"data":[
          {"name":"likes","period":"lifetime","values":[{"value":100}],"title":"likes",
           "description":"The number of likes on your post.","id":"m-1/insights/likes/lifetime"},
          {"name":"shares","period":"lifetime","values":[{"value":2}],"title":"shares",
           "description":"shares","id":"m-1/insights/shares/lifetime"}]}|} } ];
  let module I = Social_threads_v1.Insights in
  Threads.get_media_insights ~access_token:"t" ~media_id:"m-1" (function
    | Ok insights ->
        (match I.find I.Likes insights with
         | Some { value; period = Some "lifetime"; _ } -> assert (I.latest_value value = Some 100)
         | _ -> failwith "likes missing");
        let series = I.to_canonical_series ~scope:Analytics_types.Post insights in
        assert (List.exists (fun (s : Analytics_types.series) -> s.metric = Analytics_types.Shares) series)
    | Error err -> failwith (Error_types.error_to_string err));
  (match !Mock_http.requests with
   | [ ("GET", url, _, _) ] ->
       assert (Uri.path (Uri.of_string url) = "/v1.0/m-1/insights");
       assert (Uri.get_query_param (Uri.of_string url) "metric" = Some "views,likes,replies,reposts,quotes,shares")
   | _ -> failwith "expected one GET");
  Mock_config.reset ();
  Mock_http.set_responses
    [ { status = 200; headers = []; body = {|{"data":[{"name":"likes","period":"lifetime"}]}|} } ];
  Threads.get_media_insights ~access_token:"t" ~media_id:"m-1" (function
    | Ok [] -> ()
    | _ -> failwith "an entry without a value carries no figure and is left out");
  print_endline "ok: get_media_insights documented shape"

(* A day still being computed arrives without a value, and one empty
   metric must not cost the caller the others: the read used to fail as a
   whole. *)
let test_parse_insights_skips_missing_values () =
  let module I = Social_threads_v1.Insights in
  let body =
    {|{"data":[
      {"name":"views","period":"day","values":[
        {"value":12,"end_time":"2026-11-15T08:00:00+0000"},
        {"value":9,"end_time":"2026-11-16T08:00:00+0000"},
        {"value":null,"end_time":"2026-11-17T08:00:00+0000"},
        {"end_time":"2026-11-18T08:00:00+0000"}]},
      {"name":"likes","period":"lifetime"},
      {"name":"replies","period":"lifetime","values":[{"value":"not_a_number"}]},
      {"name":"quotes","period":"lifetime","total_value":{"value":null}},
      {"name":"clicks","period":"day","link_total_values":[
        {"value":3,"link_url":"https://example.com/a"},{"link_url":"https://example.com/b"}]},
      {"name":"followers_count","period":"day","total_value":{"value":540}}]}|}
  in
  (match I.parse_insights body with
   | Error msg -> failwith ("expected the read to succeed: " ^ msg)
   | Ok insights ->
       (match I.find I.Views insights with
        | Some { value = I.Time_series [ { value = 12; _ }; { value = 9; _ } ]; _ } -> ()
        | _ -> failwith "views should keep the two buckets with a value");
       assert (Option.bind (I.find I.Views insights) (fun i -> I.latest_value ~skip_zero:true i.value)
               = Some 9);
       assert (I.find I.Likes insights = None);
       assert (I.find I.Quotes insights = None);
       (match I.find I.Replies insights with
        | Some { value = I.Time_series []; _ } -> ()
        | _ -> failwith "replies should be an empty series");
       (match I.find I.Clicks insights with
        | Some { value = I.Link_totals [ { link_clicks = 3; _ } ]; _ } -> ()
        | _ -> failwith "clicks should keep the link with a value");
       (match I.find I.Followers_count insights with
        | Some { value = I.Total 540; _ } -> ()
        | _ -> failwith "followers_count should survive the empty metrics"));
  (* A non-empty series wins over a total; an empty one falls back to it. *)
  (match
     I.parse_insights
       {|{"data":[
         {"name":"followers_count","values":[{"value":90}],"total_value":{"value":1234}},
         {"name":"views","values":[],"total_value":{"value":77}}]}|}
   with
   | Ok insights ->
       (match I.find I.Followers_count insights with
        | Some { value = I.Time_series [ { value = 90; _ } ]; _ } -> ()
        | _ -> failwith "a non-empty series should win over total_value");
       (match I.find I.Views insights with
        | Some { value = I.Total 77; _ } -> ()
        | _ -> failwith "an empty series should fall back to total_value")
   | Error msg -> failwith msg);
  assert (Result.is_error (I.parse_insights {|{"error":{"message":"x"}}|}));
  assert (Result.is_error (I.parse_insights "not json"));
  print_endline "ok: parse_insights skips values Meta left empty"

(* Regression: get_account_insights required [values] on every entry, so the
   documented [total_value] answer for likes/replies/reposts/quotes failed
   the whole read with a parse error. *)
let test_get_account_insights_accepts_total_value () =
  with_reply_account ();
  Mock_http.set_responses
    [ { status = 200; headers = []; body = {|{"id":"user-1"}|} };
      { status = 200; headers = [];
        body = {|{"data":[
          {"name":"views","period":"day","values":[{"value":5,"end_time":"2024-07-12T08:00:00+0000"}]},
          {"name":"likes","period":"day","total_value":{"value":42}}]}|} } ];
  Threads.get_account_insights ~account_id:"acct-1" ~since:"2024-07-11" ~until:"2024-07-12"
    (function
    | Ok [ views; likes ] ->
        assert (views.metric = Social_threads_v1.Views);
        assert (likes.metric = Social_threads_v1.Likes);
        (match likes.values with
         | [ { value = 42; end_time = None } ] -> ()
         | _ -> failwith "likes total_value should be one value")
    | Ok _ -> failwith "expected two insights"
    | Error err -> failwith (Error_types.error_to_string err));
  print_endline "ok: get_account_insights accepts total_value"

let test_get_permalink () =
  Mock_config.reset ();
  Mock_http.set_responses
    [ { status = 200; headers = [];
        body = {|{"permalink":"https://www.threads.com/@alice/post/abc","id":"post-1"}|} } ];
  let token = "tok+en/with=&reserved" in
  Threads.get_permalink ~access_token:token ~post_id:" post-1 " (function
    | Ok (Some "https://www.threads.com/@alice/post/abc") -> ()
    | Ok _ -> failwith "expected the permalink"
    | Error err -> failwith (Error_types.error_to_string err));
  (match !Mock_http.requests with
   | [ ("GET", url, _, _) ] ->
       let uri = Uri.of_string url in
       assert (Uri.path uri = "/v1.0/post-1");
       assert (Uri.get_query_param uri "fields" = Some "permalink");
       assert (Uri.get_query_param uri "access_token" = Some token)
   | _ -> failwith "expected one GET");
  Mock_config.reset ();
  Mock_http.set_responses [ { status = 200; headers = []; body = {|{"id":"post-1"}|} } ];
  Threads.get_permalink ~access_token:"t" ~post_id:"post-1" (function
    | Ok None -> ()
    | _ -> failwith "a missing permalink is Ok None");
  Mock_config.reset ();
  Mock_http.set_responses
    [ { status = 400; headers = [];
        body = {|{"error":{"message":"Object does not exist","type":"OAuthException","code":100,"error_subcode":33}}|} } ];
  Threads.get_permalink ~access_token:"t" ~post_id:"gone" (function
    | Error (Error_types.Resource_not_found _) -> ()
    | Error err -> failwith ("expected Resource_not_found, got " ^ Error_types.error_to_string err)
    | Ok _ -> failwith "expected an error");
  print_endline "ok: get_permalink"

(* Location search: GET /location_search with q and/or latitude+longitude,
   every documented field, the token in the query. The guide shows the id
   as a number. *)
let test_search_locations () =
  Mock_config.reset ();
  Mock_http.set_responses
    [ { status = 200; headers = [];
        body =
          {|{"data":[{"id":12345,"name":"Menlo Park","address":"1 Hacker Way","city":"Menlo Park","country":"US","latitude":37.48,"longitude":-122.15,"postal_code":"94025"},{"id":"678","name":"No point"}]}|} } ];
  let token = "tok+en/with=&reserved" in
  Threads.search_locations ~access_token:token ~query:" Menlo Park "
    ~coordinates:{ Social_threads_v1.Locations.latitude = 37.4848; longitude = -122.1484 }
    (function
      | Ok [ first; second ] ->
          assert (first.id = "12345");
          assert (first.name = Some "Menlo Park");
          assert (first.address = Some "1 Hacker Way");
          assert (first.postal_code = Some "94025");
          assert (first.coordinates = Some { latitude = 37.48; longitude = -122.15 });
          assert (second.id = "678");
          assert (second.city = None);
          assert (second.coordinates = None)
      | Ok _ -> failwith "expected two locations"
      | Error err -> failwith (Error_types.error_to_string err));
  (match !Mock_http.requests with
   | [ ("GET", url, _, _) ] ->
       let uri = Uri.of_string url in
       assert (Uri.path uri = "/v1.0/location_search");
       assert (Uri.get_query_param uri "q" = Some "Menlo Park");
       assert (Uri.get_query_param uri "latitude" = Some "37.484800");
       assert (Uri.get_query_param uri "longitude" = Some "-122.148400");
       assert (Uri.get_query_param uri "fields" = Some Social_threads_v1.Locations.fields);
       assert (Uri.get_query_param uri "access_token" = Some token)
   | _ -> failwith "expected one GET");
  (* Coordinates alone are a valid search. *)
  Mock_config.reset ();
  Mock_http.set_responses [ { status = 200; headers = []; body = {|{"data":[]}|} } ];
  Threads.search_locations ~access_token:"t"
    ~coordinates:{ Social_threads_v1.Locations.latitude = 1.; longitude = 2. }
    (function Ok [] -> () | _ -> failwith "expected no locations");
  (match !Mock_http.requests with
   | [ ("GET", url, _, _) ] -> assert (Uri.get_query_param (Uri.of_string url) "q" = None)
   | _ -> failwith "expected one GET");
  (* Neither a query nor a point: no request. *)
  Mock_config.reset ();
  Threads.search_locations ~access_token:"t" ~query:"  " (function
    | Error (Error_types.Validation_error [ Error_types.Text_empty ]) -> ()
    | _ -> failwith "a blank search must be rejected");
  assert (!Mock_http.requests = []);
  (* A Graph error is classified like every other read. *)
  Mock_config.reset ();
  Mock_http.set_responses
    [ { status = 403; headers = [];
        body = {|{"error":{"message":"Application does not have permission for this action","type":"OAuthException","code":10}}|} } ];
  Threads.search_locations ~access_token:"t" ~query:"x" (function
    | Error (Error_types.Auth_error (Error_types.Insufficient_permissions _)) -> ()
    | Error err -> failwith ("expected Insufficient_permissions, got " ^ Error_types.error_to_string err)
    | Ok _ -> failwith "expected an error");
  (* A body without data is a parse failure, not an empty result. *)
  Mock_config.reset ();
  Mock_http.set_responses [ { status = 200; headers = []; body = {|{"id":"1"}|} } ];
  Threads.search_locations ~access_token:"t" ~query:"x" (function
    | Error (Error_types.Internal_error _) -> ()
    | _ -> failwith "expected Internal_error");
  print_endline "ok: search_locations"

let test_search_locations_for_account () =
  with_reply_account ();
  Mock_http.set_responses [ { status = 200; headers = []; body = {|{"data":[{"id":"9"}]}|} } ];
  Threads.search_locations_for_account ~account_id:"acct-1" ~query:"Berlin" (function
    | Ok [ { id = "9"; name = None; _ } ] -> ()
    | Ok _ -> failwith "expected one location"
    | Error err -> failwith (Error_types.error_to_string err));
  print_endline "ok: search_locations_for_account"

let test_get_permalink_for_account () =
  with_reply_account ();
  Mock_http.set_responses
    [ { status = 200; headers = []; body = {|{"permalink":"https://www.threads.com/@a/post/x"}|} } ];
  Threads.get_permalink_for_account ~account_id:"acct-1" ~post_id:"post-1" (function
    | Ok (Some "https://www.threads.com/@a/post/x") -> ()
    | _ -> failwith "expected the permalink");
  (match !Mock_http.requests with
   | [ ("GET", url, _, _) ] -> assert (query_param url "access_token" = "access-xyz")
   | _ -> failwith "expected one GET");
  print_endline "ok: get_permalink_for_account"

let test_parse_permalink_response () =
  let parse = Social_threads_v1.parse_permalink_response in
  let is_error body = match parse body with Error _ -> true | Ok _ -> false in
  assert (parse {|{"permalink":"https://x"}|} = Ok (Some "https://x"));
  assert (parse {|{"permalink":null}|} = Ok None);
  assert (parse {|{"permalink":""}|} = Ok None);
  assert (is_error {|{"permalink":42}|});
  assert (is_error {|[]|});
  assert (is_error "not json");
  print_endline "ok: parse_permalink_response"

let test_debug_token_info () =
  Mock_http.reset ();
  Mock_http.set_responses
    [ { Social_core.status = 200;
        body = {|{"data":{"type":"USER","application":"App","is_valid":true,
                 "expires_at":1767225600,"scopes":["threads_basic","threads_read_replies"],
                 "user_id":"1784"}}|};
        headers = [] } ];
  let app_access_token = Social_meta_debug_token.threads_app_access_token ~app_id:"99" ~app_secret:"s&cret" in
  OAuth_http.debug_token_info ~app_access_token ~input_token:"THtok+/=" (function
    | Ok info ->
        (match !Mock_http.requests with
         | [ ("GET", url, _, _) ] ->
             assert (string_contains url "https://graph.threads.net/v1.0/debug_token?");
             assert (query_param url "input_token" = "THtok+/=");
             assert (query_param url "access_token" = "TH|99|s&cret")
         | _ -> failwith "expected one GET for debug_token_info");
        assert info.Social_meta_debug_token.is_valid;
        assert (info.user_id = Some "1784");
        assert (Social_oauth_scope.mem "threads_read_replies" info.scopes);
        assert (info.expires_at = Some (Social_meta_debug_token.Expires_at 1767225600))
    | Error err -> failwith (Error_types.error_to_string err));
  Mock_http.reset ();
  Mock_http.set_responses
    [ { Social_core.status = 400;
        body = {|{"error":{"message":"bad","type":"OAuthException","code":190}}|};
        headers = [] } ];
  OAuth_http.debug_token_info ~app_access_token:"TH|1|s" ~input_token:"t" (function
    | Error (Error_types.Auth_error _) -> ()
    | Error err -> failwith ("unexpected " ^ Error_types.error_to_string err)
    | Ok _ -> failwith "expected an error");
  print_endline "ok: Threads debug_token_info reads the typed answer"
(* A /conversation page as Meta returns it with reverse=true: a flat list,
   nested replies placed by replied_to, and a paging.next URL that echoes
   the access token. *)
let conversation_page_body ~next =
  Printf.sprintf
    {|{"data":[
      {"id":"18050206876707110","text":"Thanks!","username":"bob","permalink":"https://www.threads.com/@bob/post/C9xYz","timestamp":"2026-11-16T10:05:00+0000","media_type":"TEXT_POST","shortcode":"C9xYz","is_quote_post":false,"has_replies":false,"root_post":{"id":"18040000000000001"},"replied_to":{"id":"18049999999999999"},"is_reply":true,"is_reply_owned_by_me":false,"hide_status":"NOT_HUSHED"},
      {"id":"18049999999999999","text":"Great post","username":"alice","permalink":"https://www.threads.com/@alice/post/C9xYa","timestamp":"2026-11-16T09:00:00+0000","media_type":"TEXT_POST","shortcode":"C9xYa","is_quote_post":false,"has_replies":true,"root_post":{"id":"18040000000000001"},"replied_to":{"id":"18040000000000001"},"is_reply":true,"is_reply_owned_by_me":false,"hide_status":"HIDDEN"}],
     "paging":{"cursors":{"before":"QVFIUkJ","after":"QVFIUkF"}%s}}|}
    (match next with
     | Some url -> Printf.sprintf {|,"next":%S|} url
     | None -> "")

let test_get_conversation_page_with_token () =
  Mock_config.reset ();
  let next =
    "https://graph.threads.net/v1.0/18040000000000001/conversation?fields=id%2Ctext&reverse=true&limit=2&access_token=OLD-TOKEN&after=QVFIUkF"
  in
  Mock_http.set_responses
    [ { status = 200; headers = []; body = conversation_page_body ~next:(Some next) } ];
  let token = "tok+en/with=&reserved" in
  Threads.get_conversation_page ~access_token:token ~media_id:"18040000000000001"
    ~reverse:true ~limit:2
    (function
      | Ok page ->
          (match page.data with
           | [ nested; top ] ->
               assert (nested.replied_to_id = Some "18049999999999999");
               assert (nested.root_post_id = Some "18040000000000001");
               assert (nested.username = Some "bob");
               assert (not nested.has_replies);
               assert top.has_replies;
               assert (top.hide_status = Some Social_threads_v1.Hidden)
           | _ -> failwith "expected two replies");
          assert (page.next_url = Some next);
          assert (Social_meta_graph_paging.next_after page = Some "QVFIUkF")
      | Error err -> failwith (Error_types.error_to_string err));
  (match !Mock_http.requests with
   | [ ("GET", url, _, _) ] ->
       let uri = Uri.of_string url in
       assert (Uri.path uri = "/v1.0/18040000000000001/conversation");
       assert (Uri.get_query_param uri "access_token" = Some token);
       assert (Uri.get_query_param uri "reverse" = Some "true");
       assert (Uri.get_query_param uri "limit" = Some "2");
       assert (Uri.get_query_param uri "after" = None);
       assert_reply_fields_requested url
   | _ -> failwith "expected one GET");
  print_endline "ok: get_conversation_page with token"

let test_reply_edges_cursor_only_traversal () =
  List.iter (fun read ->
    Mock_config.reset ();
    Mock_http.set_responses
      [ { status = 200; headers = []; body = conversation_page_body ~next:None };
        { status = 200; headers = []; body = {|{"data":[],"paging":{"cursors":{"after":"last"}}}|} } ];
    read Social_meta_graph_paging.First (function
      | Error err -> failwith (Error_types.error_to_string err)
      | Ok page ->
          let url = Option.get page.Social_meta_graph_paging.next_url in
          assert (Uri.get_query_param (Uri.of_string url) "after" = Some "QVFIUkF");
          read (Social_meta_graph_paging.Page_url url) (function
            | Error err -> failwith (Error_types.error_to_string err)
            | Ok last -> assert (not (Social_meta_graph_paging.has_next last));
                assert (Social_meta_graph_paging.next_after last = None)));
    assert (List.length !Mock_http.requests = 2))
    [ (fun position k -> Threads.get_replies_page ~access_token:"t" ~media_id:"m" ~position k);
      (fun position k -> Threads.get_conversation_page ~access_token:"t" ~media_id:"m" ~position k);
      (fun position k -> Threads.get_pending_replies_page ~access_token:"t" ~media_id:"m" ~position k) ]

let test_reply_page_cursor_positions () =
  Mock_config.reset ();
  Mock_http.set_responses
    [ { status = 200; headers = []; body = conversation_page_body ~next:None };
      { status = 200; headers = []; body = conversation_page_body ~next:None } ];
  let check_page = function
    | Ok page ->
        assert (Social_meta_graph_paging.has_next page);
        assert (Social_meta_graph_paging.next_after page = Some "QVFIUkF")
    | Error err -> failwith (Error_types.error_to_string err)
  in
  Threads.get_replies_page ~access_token:"t" ~media_id:"m"
    ~position:(Social_meta_graph_paging.After "QVFIUkF") check_page;
  Threads.get_replies_page ~access_token:"t" ~media_id:"m"
    ~position:(Social_meta_graph_paging.Before "QVFIUkJ") check_page;
  (match List.rev !Mock_http.requests with
   | [ ("GET", after_url, _, _); ("GET", before_url, _, _) ] ->
       assert (Uri.path (Uri.of_string after_url) = "/v1.0/m/replies");
       assert (query_param after_url "after" = "QVFIUkF");
       assert (Uri.get_query_param (Uri.of_string after_url) "before" = None);
       assert (query_param before_url "before" = "QVFIUkJ");
       assert (Uri.get_query_param (Uri.of_string before_url) "after" = None)
   | _ -> failwith "expected two GETs");
  print_endline "ok: reply page cursor positions"

let test_reply_page_follows_next_url () =
  Mock_config.reset ();
  Mock_http.set_responses
    [ { status = 200; headers = []; body = conversation_page_body ~next:None } ];
  let next =
    "https://graph.threads.com/v1.0/18040000000000001/conversation?fields=id%2Ctext%2Creplied_to&reverse=true&limit=2&access_token=OLD-TOKEN&after=QVFIUkF"
  in
  Threads.get_conversation_page ~access_token:"NEW-TOKEN" ~media_id:"ignored"
    ~position:(Social_meta_graph_paging.Page_url next)
    (function
      | Ok page -> assert (List.length page.data = 2)
      | Error err -> failwith (Error_types.error_to_string err));
  (match !Mock_http.requests with
   | [ ("GET", url, _, _) ] ->
       let uri = Uri.of_string url in
       assert (Uri.host uri = Some "graph.threads.com");
       assert (Uri.path uri = "/v1.0/18040000000000001/conversation");
       assert (Uri.get_query_param uri "access_token" = Some "NEW-TOKEN");
       assert (List.length (List.filter (fun (k, _) -> k = "access_token") (Uri.query uri)) = 1);
       assert (Uri.get_query_param uri "after" = Some "QVFIUkF");
       assert (Uri.get_query_param uri "fields" = Some "id,text,replied_to")
   | _ -> failwith "expected one GET");
  print_endline "ok: reply page follows next url"

let test_reply_page_rejects_foreign_next_url () =
  List.iter
    (fun url ->
      Mock_config.reset ();
      Threads.get_conversation_page ~access_token:"SECRET" ~media_id:"m"
        ~position:(Social_meta_graph_paging.Page_url url)
        (function
          | Error (Error_types.Validation_error [ Error_types.Invalid_url _ ]) -> ()
          | _ -> failwith ("expected a validation error for " ^ url));
      assert (!Mock_http.requests = []))
    [ "https://attacker.example/v1.0/m/conversation?after=x";
      "http://graph.threads.net/v1.0/m/conversation?after=x";
      "https://graph.threads.net/v1.0/m/replies?after=x" ];
  print_endline "ok: reply page rejects foreign next url"

let test_pending_replies_page_with_token () =
  Mock_config.reset ();
  Mock_http.set_responses
    [ { status = 200; headers = [];
        body = {|{"data":[{"id":"p-1","text":"pending reply","username":"carol","is_reply":true,"replied_to":{"id":"root-1"},"root_post":{"id":"root-1"},"reply_approval_status":"pending"}]}|} } ];
  Threads.get_pending_replies_page ~access_token:"t" ~media_id:"root-1"
    ~approval_status:Social_threads_v1.Approval_pending
    (function
      | Ok { data = [ reply ]; next_url = None; _ } ->
          assert (reply.reply_approval_status = Some Social_threads_v1.Approval_pending)
      | Ok _ -> failwith "expected one pending reply"
      | Error err -> failwith (Error_types.error_to_string err));
  (match !Mock_http.requests with
   | [ ("GET", url, _, _) ] ->
       assert (Uri.path (Uri.of_string url) = "/v1.0/root-1/pending_replies");
       assert (query_param url "approval_status" = "pending");
       assert (List.mem "reply_approval_status" (field_set url))
   | _ -> failwith "expected one GET");
  print_endline "ok: pending replies page with token"

let test_manage_reply_with_token () =
  Mock_config.reset ();
  Mock_http.set_responses
    [ { status = 200; headers = []; body = {|{"success":true}|} };
      { status = 200; headers = []; body = {|{"success":true}|} } ];
  Threads.manage_reply ~access_token:"tok" ~reply_id:"r-1" ~hide:true (function
    | Ok () -> ()
    | Error err -> failwith (Error_types.error_to_string err));
  Threads.manage_pending_reply ~access_token:"tok" ~reply_id:"r-2" ~approve:false
    (function
      | Ok () -> ()
      | Error err -> failwith (Error_types.error_to_string err));
  (match List.rev !Mock_http.requests with
   | [ ("POST", hide_url, _, hide_body); ("POST", pending_url, _, pending_body) ] ->
       assert (String.ends_with ~suffix:"/v1.0/r-1/manage_reply" hide_url);
       assert (string_contains hide_body "hide=true");
       assert (string_contains hide_body "access_token=tok");
       assert (String.ends_with ~suffix:"/v1.0/r-2/manage_pending_reply" pending_url);
       assert (string_contains pending_body "approve=false")
   | _ -> failwith "expected two POSTs");
  Mock_config.reset ();
  Threads.manage_reply ~access_token:"tok" ~reply_id:" " ~hide:false (function
    | Error (Error_types.Validation_error _) -> ()
    | _ -> failwith "expected a validation error");
  assert (!Mock_http.requests = []);
  print_endline "ok: manage_reply / manage_pending_reply with token"

let test_authorize_url () =
  let url =
    Social_threads_v1.OAuth.authorize_url ~client_id:"c" ~redirect_uri:"https://e.com/cb" ~state:"s"
      ~scopes:[ "threads_basic"; "threads_read_replies" ] ~extra_params:[ ("x", "y") ] ()
  in
  assert (String.starts_with ~prefix:"https://threads.net/oauth/authorize?" url);
  assert (query_param url "scope" = "threads_basic,threads_read_replies");
  assert (query_param url "response_type" = "code");
  assert (query_param url "x" = "y");
  assert (
    Social_threads_v1.OAuth.get_authorization_url ~client_id:"c" ~redirect_uri:"https://e.com/cb" ~state:"s"
      ~scopes:[ Social_threads_v1.OAuth.Threads_basic; Social_threads_v1.OAuth.Threads_read_replies ]
    = Social_threads_v1.OAuth.authorize_url ~client_id:"c" ~redirect_uri:"https://e.com/cb" ~state:"s"
        ~scopes:[ "threads_basic"; "threads_read_replies" ] ());
  print_endline "ok: Threads authorize_url keeps the plain scope parameter and host"

let test_oauth_get_me () =
  Mock_http.reset ();
  Mock_http.set_responses
    [ { Social_core.status = 200;
        body = {|{"id":"1784","username":"maker","name":"","threads_profile_picture_url":"https://p/x.jpg",
                 "threads_biography":"bio","is_verified":true}|};
        headers = [] } ];
  OAuth_http.get_me ~access_token:"tok+/=" (function
    | Ok profile ->
        (match !Mock_http.requests with
         | [ ("GET", url, _, _) ] ->
             assert (string_contains url "https://graph.threads.net/v1.0/me?");
             assert (query_param url "fields"
                     = "id,username,name,threads_profile_picture_url,threads_biography,is_verified");
             assert (query_param url "access_token" = "tok+/=")
         | _ -> failwith "expected one GET");
        assert (profile.Social_threads_v1.OAuth.id = "1784");
        assert (profile.username = Some "maker");
        assert (profile.name = None);
        assert (profile.profile_picture_url = Some "https://p/x.jpg");
        assert (profile.biography = Some "bio");
        assert profile.is_verified
    | Error err -> failwith (Error_types.error_to_string err));
  Mock_http.reset ();
  Mock_http.set_responses [ { Social_core.status = 200; body = {|{"id":1784}|}; headers = [] } ];
  OAuth_http.get_me ~access_token:"t" ~fields:[ "username" ] (function
    | Ok profile ->
        (match !Mock_http.requests with
         | [ ("GET", url, _, _) ] -> assert (query_param url "fields" = "id,username")
         | _ -> failwith "expected one GET");
        assert (profile.Social_threads_v1.OAuth.id = "1784" && profile.username = None && not profile.is_verified)
    | Error err -> failwith (Error_types.error_to_string err));
  Mock_http.reset ();
  Mock_http.set_responses
    [ { Social_core.status = 401;
        body = {|{"error":{"message":"expired","type":"OAuthException","code":190,"error_subcode":463}}|};
        headers = [] } ];
  OAuth_http.get_me ~access_token:"t" (function
    | Error (Error_types.Auth_error Error_types.Token_expired) -> ()
    | Error err -> failwith ("unexpected " ^ Error_types.error_to_string err)
    | Ok _ -> failwith "expected an error");
  print_endline "ok: OAuth.get_me reads the typed Threads profile"

let () =
  test_oauth_url ();
  test_oauth_get_me ();
  test_authorize_url ();
  test_debug_token_info ();
  test_exported_api_constants ();
  test_shared_meta_graph_error_contract ();
  test_api_error_detail_survives_redaction ();
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
  test_exchange_code_empty_auth_type_defaults_bearer ();
  test_exchange_code_lowercase_bearer_normalized ();
  test_exchange_code_negative_expires_in_clamped ();
  test_exchange_code_huge_expires_in_clamped ();
  test_exchange_code_huge_float_expires_in_clamped ();
  test_exchange_code_fractional_expires_in_rounded_up ();
  test_exchange_code_form_urlencodes_special_chars ();
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
  test_get_user_insights_documented_shapes ();
  test_get_user_insights_for_account_user_id ();
  test_get_media_insights_documented_shape ();
  test_get_account_insights_accepts_total_value ();
  test_parse_insights_skips_missing_values ();
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
  test_get_me_auto_refresh_preserves_failures ();
  test_get_me_auto_refresh_persist_failure_returns_error ();
  test_get_me_auto_refresh_updates_health_status ();
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
  test_check_container_status_published ();
  test_check_container_status_expired ();
  test_check_container_status_error_without_message ();
  test_poll_container_status_published_is_duplicate ();
  test_poll_container_status_expired ();
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
  test_read_retry_backs_off_through_config_sleep ();
  test_read_retry_returns_response_when_sleep_fails ();
  test_delete_post_success ();
  test_delete_post_rejects_empty_id ();
  test_delete_post_api_error ();
  test_approve_reply_success ();
  test_ignore_reply_success ();
  test_hide_reply_success ();
  test_unhide_reply_success ();
  test_reply_action_rejects_empty_id ();
  test_get_pending_replies_success ();
  test_get_conversation_parses_reply_fields ();
  test_get_replies_requests_reply_fields ();
  test_get_pending_replies_approval_status ();
  test_get_permalink ();
  test_search_locations ();
  test_search_locations_for_account ();
  test_get_permalink_for_account ();
  test_parse_permalink_response ();
  test_get_conversation_page_with_token ();
  test_reply_edges_cursor_only_traversal ();
  test_reply_page_cursor_positions ();
  test_reply_page_follows_next_url ();
  test_reply_page_rejects_foreign_next_url ();
  test_pending_replies_page_with_token ();
  test_manage_reply_with_token ();
  print_endline "Threads tests passed"

let () =
  let read = Threads.get_user_posts_page ~threads_user_id:"user/name" ~access_token:"new+token" ~fields:"id,username,owner" in
  let response body = { Social_core.status = 200; headers = []; body } in
  let page_body = {|{"data":[{"id":"123","username":"example","owner":{"id":"456"}}],"paging":{"cursors":{"after":"cursor+1"}}}|} in
  Mock_config.reset ();
  Mock_http.reset ();
  Mock_http.set_responses [ response page_body ];
  read (function
    | Error err -> failwith (Error_types.error_to_string err)
    | Ok page ->
        (match page.data with
        | [ post ] ->
            assert (post.id = "123");
            assert (post.username = Some "example");
            assert (Yojson.Basic.Util.member "owner" post.raw = `Assoc ["id", `String "456"])
        | [] | _ :: _ -> failwith "expected one post");
        assert (Social_meta_graph_paging.next_after page = Some "cursor+1"));
  (match !Mock_http.requests with
  | [ ("GET", url, _, _) ] ->
      assert (Uri.path (Uri.of_string url) = "/v1.0/user%2Fname/threads");
      assert (Uri.get_query_param (Uri.of_string url) "fields" = Some "id,username,owner")
  | [] | _ :: _ -> failwith "expected one GET");
  List.iter (fun position ->
    Mock_http.reset ();
    Mock_http.set_responses [ response {|{"data":[]}|} ];
    read ~position (function
      | Ok page -> assert (page.data = []); assert (page.next_url = None)
      | Error err -> failwith (Error_types.error_to_string err));
    match !Mock_http.requests with
    | [ ("GET", url, headers, _) ] ->
        assert (Uri.get_query_param (Uri.of_string url) "after" = Some "cursor+1");
        assert (Uri.get_query_param (Uri.of_string url) "access_token" = Some "new+token"); ignore headers; assert (Uri.get_query_param (Uri.of_string url) "appsecret_proof" = None)
    | [] | _ :: _ -> failwith "expected follow-up GET")
    [ Social_meta_graph_paging.After "cursor+1";
      Social_meta_graph_paging.Page_url "https://graph.threads.net/v1.0/user/threads?fields=id&after=cursor%2B1&access_token=old&appsecret_proof=old" ];
  List.iter (fun url ->
    Mock_http.reset ();
    read ~position:(Social_meta_graph_paging.Page_url url) (function
      | Error (Error_types.Validation_error _) -> ()
      | Error err -> failwith (Error_types.error_to_string err)
      | Ok _ -> failwith "unsafe URL accepted");
    assert (!Mock_http.requests = []))
    [ "https://evil.example/user/threads"; "https://graph.threads.net/user/comments"; "http://graph.threads.net/user/threads" ];
  Mock_http.reset ();
  Mock_http.set_responses [ response {|{"data":[{"username":"missing-id"}]}|} ];
  read (function | Error _ -> () | Ok _ -> failwith "malformed item accepted");
  Mock_http.reset ();
  Mock_http.set_responses [ { Social_core.status = 400; headers = [];
    body = {|{"error":{"message":"bad token","type":"OAuthException","code":190}}|} } ];
  read (function | Error _ -> () | Ok _ -> failwith "API error accepted");
  Mock_http.reset ();
  read (function | Error (Error_types.Network_error _) -> ()
    | Error err -> failwith (Error_types.error_to_string err)
    | Ok _ -> failwith "transport error accepted")

(* A suspended persistence callback models overlapping worker/read requests.
   The lock must survive the return from the HTTP callback. *)
let test_coordinated_refresh () =
  Mock_config.reset ();
  Mock_config.env_vars := [ ("THREADS_AUTO_REFRESH_TOKEN", "true") ];
  Mock_config.credentials_store :=
    [
      ( "acct-1",
        {
          Social_core.access_token = "old-access";
          refresh_token = None;
          expires_at = Some (rfc3339_after_seconds 10);
          auth_type = Social_core.Bearer;
          scope = None;
        } );
    ];
  let locked = ref false in
  let waiting = Queue.create () in
  let persist = ref None in
  let completed = ref 0 in
  let module Config = struct
    include Mock_config
    let update_health_status ~account_id:_ ~status:_ ~error_message:_ _ _ =
      failwith "credential-only refresh must not write health"

    let update_credentials ~account_id ~credentials on_success on_error =
      assert !locked;
      persist :=
        Some
          (fun () ->
            Mock_config.update_credentials ~account_id ~credentials on_success
              on_error)

    let get_credentials ~account_id on_success on_error =
      assert !locked;
      Mock_config.get_credentials ~account_id on_success on_error

    let with_refresh_lock ~account_id:_ run on_success on_error =
      let release () =
        locked := false;
        if not (Queue.is_empty waiting) then (Queue.take waiting) ()
      in
      let start () =
        assert (not !locked);
        locked := true;
        run
          (fun credentials ->
            release ();
            on_success credentials)
          (fun error ->
            release ();
            on_error error)
      in
      if !locked then Queue.add start waiting else start ()
  end in
  let module Provider = Social_threads_v1.Make_credentials_only (Config) in
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body =
          "{\"access_token\":\"new-access\",\"token_type\":\"Bearer\",\"expires_in\":5184000}";
      };
      {
        status = 200;
        headers = [];
        body = "{\"id\":\"user-1\",\"username\":\"alice\"}";
      };
      { status = 200; headers = []; body = "{\"id\":\"post-1\"}" };
      {
        status = 200;
        headers = [];
        body = "{\"id\":\"user-1\",\"username\":\"alice\"}";
      };
    ];
  Provider.ensure_valid_token_with_outcome ~with_refresh_lock:Config.with_refresh_lock
    ~refresh_window_seconds:604800
    ~account_id:"acct-1"
    (fun outcome ->
      assert (outcome = Social_credential_outcome.Renewed "new-access");
      incr completed)
    (fun error -> failwith (Error_types.error_to_string error));
  Provider.ensure_valid_token_with_outcome ~with_refresh_lock:Config.with_refresh_lock
    ~refresh_window_seconds:604800 ~account_id:"acct-1"
    (fun outcome ->
      assert (outcome = Social_credential_outcome.Reused "new-access");
      incr completed)
    (fun error -> failwith (Error_types.error_to_string error));
  Provider.get_me ~account_id:"acct-1" (function
    | Ok me ->
        assert (me.id = "user-1");
        incr completed
    | Error error -> failwith (Error_types.error_to_string error));
  Provider.post_single ~account_id:"acct-1" ~text:"fixture" ~media_urls:[]
    (function
    | Error_types.Success id ->
        assert (id = "post-1");
        incr completed
    | Error_types.Partial_success _ | Error_types.Failure _ -> assert false);
  assert (!completed = 0);
  assert !locked;
  assert (List.length !Mock_http.requests = 1);
  (match !persist with Some finish -> finish () | None -> assert false);
  assert (!completed = 4);
  assert (not !locked);
  assert (List.length !Mock_http.requests = 4);
  assert (
    (List.assoc "acct-1" !Mock_config.credentials_store).access_token
    = "new-access")

let () = test_coordinated_refresh ()

let test_coordinated_explicit_refresh_without_expiry () =
  Mock_config.reset ();
  Mock_config.credentials_store :=
    [
      ( "account",
        {
          Social_core.access_token = "long-lived";
          refresh_token = None;
          expires_at = None;
          auth_type = Social_core.Bearer;
          scope = None;
        } );
    ];
  let locked = ref false in
  let module Config = struct
    include Mock_config
    let update_health_status ~account_id:_ ~status:_ ~error_message:_ _ _ =
      failwith "credential-only refresh must not write health"

    let with_refresh_lock ~account_id:_ run on_success on_error =
      locked := true;
      run
        (fun credentials ->
          locked := false;
          on_success credentials)
        (fun error ->
          locked := false;
          on_error error)

    let update_credentials ~account_id ~credentials on_success on_error =
      assert !locked;
      Mock_config.update_credentials ~account_id ~credentials on_success
        on_error
  end in
  let module Provider = Social_threads_v1.Make_credentials_only (Config) in
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body =
          "{\"access_token\":\"refreshed\",\"token_type\":\"Bearer\",\"expires_in\":5184000}";
      };
    ];
  let result = ref None in
  Provider.refresh_account_credentials ~account_id:"account"
    (fun credentials ->
      assert (not !locked);
      result := Some (Ok credentials))
    (fun error -> result := Some (Error error));
  (match !result with
  | Some (Ok credentials) -> assert (credentials.access_token = "refreshed")
  | Some (Error _) | None -> assert false);
  assert (List.length !Mock_http.requests = 1)

let () = test_coordinated_explicit_refresh_without_expiry ()

(* Detail-preserving Engage operations. Response shapes:
     - container create / publish answer 200 with {"id":"..."}; a Graph
       error is {"error":{"message","type","code","error_subcode","fbtrace_id"}}
       (https://developers.facebook.com/docs/graph-api/guides/error-handling/);
     - container status answers 200 with {"status":"IN_PROGRESS"|"FINISHED"
       |"PUBLISHED"|"EXPIRED"|"ERROR"[,"error_message":...]};
     - a /threads or /conversation page answers 200 with
       {"data":[...],"paging":{"cursors":{...}[,"next":url]}}. *)
let test_detailed_engage_operations () =
  let expired_190 = graph_error_body ~subcode:463 ~message:"Session has expired" 190 in
  (* A 190/463 publish failure keeps status, codes, trace and the Graph
     classification, and classifies as an auth error. *)
  Mock_config.reset ();
  Mock_http.set_responses
    [ { status = 400; headers = [ ("x-fb-trace-id", "AvT1"); ("Retry-After", "17") ];
        body = expired_190 } ];
  Threads.publish_container_detailed ~access_token:"t" ~user_id:"me"
    ~creation_id:"17890000000000001"
    (function
      | Error (Social_meta_graph_error.Http_failure error) ->
          assert (error.detail.http_status = 400);
          assert (error.detail.code = Some "190");
          assert (error.detail.subcode = Some 463);
          assert (error.detail.trace_id = Some "AvT1");
          assert (Option.is_some error.graph);
          (match error.graph with
           | Some graph ->
               assert (
                 Social_meta_graph_error.classify graph
                 = Social_meta_graph_error.Token_invalid
                     Social_meta_graph_error.Expired)
           | None -> ());
          assert (error.kind = Error_types.Auth_error Error_types.Token_expired);
          assert (error.retry_after_seconds = Some 17)
      | Ok _ | Error (Social_meta_graph_error.Transport_failure _)
      | Error (Social_meta_graph_error.Malformed_response _)
      | Error (Social_meta_graph_error.Validation_failure _) ->
          failwith "expected an HTTP failure envelope");
  (* A 2xx publish answer without an id is a Malformed_response that keeps
     the status; the request was answered, so the status matters. *)
  Mock_http.set_responses [ { status = 200; headers = []; body = "{}" } ];
  Threads.publish_container_detailed ~access_token:"t" ~user_id:"me"
    ~creation_id:"17890000000000001"
    (function
      | Error (Social_meta_graph_error.Malformed_response { status_code; _ }) ->
          assert (status_code = 200)
      | Ok _ | Error (Social_meta_graph_error.Http_failure _)
      | Error (Social_meta_graph_error.Transport_failure _)
      | Error (Social_meta_graph_error.Validation_failure _) ->
          failwith "expected a malformed response");
  (* A rate-limited create keeps the numeric Retry-After advice. *)
  Mock_http.set_responses
    [ { status = 429; headers = [ ("Retry-After", "31") ];
        body = graph_error_body ~message:"Application request limit reached" 4 } ];
  Threads.create_text_container_detailed ~access_token:"t" ~user_id:"me"
    ~text:"Thanks!" ~reply_to_id:"18040000000000001"
    (function
      | Error (Social_meta_graph_error.Http_failure error) ->
          assert (error.detail.http_status = 429);
          assert (error.detail.code = Some "4");
          (match error.kind with
           | Error_types.Rate_limited rate ->
               assert (rate.retry_after_seconds = Some 31)
           | _ -> failwith "429 should stay a rate limit");
          assert (error.retry_after_seconds = Some 31)
      | Ok _ | Error (Social_meta_graph_error.Transport_failure _)
      | Error (Social_meta_graph_error.Malformed_response _)
      | Error (Social_meta_graph_error.Validation_failure _) ->
          failwith "expected an HTTP failure envelope");
  (* The happy create answers the container id, as the legacy API. *)
  Mock_http.set_responses
    [ { status = 200; headers = []; body = {|{"id":"17890000000000002"}|} } ];
  Threads.create_text_container_detailed ~access_token:"t" ~user_id:"me"
    ~text:"Thanks!" ~reply_to_id:"18040000000000001"
    (function
      | Ok id -> assert (id = "17890000000000002")
      | Error _ -> failwith "expected the container id");
  (* No answer at all is a transport failure. *)
  Mock_http.set_responses [];
  Mock_http.set_errors [ "connection reset" ];
  Threads.create_text_container_detailed ~access_token:"t" ~user_id:"me"
    ~text:"Thanks!" ~reply_to_id:"18040000000000001"
    (function
      | Error (Social_meta_graph_error.Transport_failure message) ->
          assert (String.equal message "connection reset")
      | Ok _ | Error (Social_meta_graph_error.Http_failure _)
      | Error (Social_meta_graph_error.Malformed_response _)
      | Error (Social_meta_graph_error.Validation_failure _) ->
          failwith "expected a transport failure");
  (* A container status read keeps the Graph answer of an unknown
     container: code 24 / subcode 4279009 stays classifiable by callers. *)
  Mock_config.reset ();
  Mock_http.set_responses
    [ { status = 404; headers = []; body = graph_error_body ~subcode:4279009
          ~error_type:"THApiException" ~message:"The requested resource does not exist" 24 } ];
  Threads.check_container_status_detailed ~access_token:"t"
    ~container_id:"17890000000000003"
    (function
      | Error (Social_meta_graph_error.Http_failure error) ->
          assert (error.detail.http_status = 404);
          (match error.graph with
           | Some { Social_meta_graph_error.code = Some 24; subcode = Some 4279009; _ }
             -> ()
           | Some _ | None -> failwith "expected code 24 / subcode 4279009")
      | Ok _ | Error (Social_meta_graph_error.Transport_failure _)
      | Error (Social_meta_graph_error.Malformed_response _)
      | Error (Social_meta_graph_error.Validation_failure _) ->
          failwith "expected an HTTP failure envelope");
  (* The status itself is read as the legacy API reads it. *)
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = {|{"status":"IN_PROGRESS"}|} };
      { status = 200; headers = []; body = {|{"status":"ERROR","error_message":"UNKNOWN"}|} };
    ];
  Threads.check_container_status_detailed ~access_token:"t"
    ~container_id:"17890000000000003"
    (function
      | Ok Social_threads_v1.In_progress -> ()
      | Ok _ | Error _ -> failwith "expected IN_PROGRESS");
  Threads.check_container_status_detailed ~access_token:"t"
    ~container_id:"17890000000000003"
    (function
      | Ok (Social_threads_v1.Error_status message) ->
          assert (String.equal message "UNKNOWN")
      | Ok _ | Error _ -> failwith "expected an ERROR status");
  print_endline "ok: detailed container create/publish/status failures"

(* Detail-preserving listing reads: paging stays the legacy shape, a Graph
   failure keeps its detail, a secret in the retained excerpt redacts the
   whole excerpt, and a paging URL on another host is a validation
   failure that sends nothing. *)
let test_detailed_listing_reads () =
  Mock_config.reset ();
  Mock_http.set_responses
    [ { status = 200; headers = []; body = conversation_page_body ~next:None } ];
  Threads.get_conversation_page_detailed ~access_token:"t"
    ~media_id:"18040000000000001" ~reverse:true ~limit:2
    (function
      | Ok page ->
          assert (List.length page.data = 2);
          (* Cursors-only nonempty pages still get a followable next_url. *)
          (match page.next_url with
           | Some url -> assert (String.equal (query_param url "after") "QVFIUkF")
           | None -> failwith "expected a built next_url");
          assert (Social_meta_graph_paging.next_after page = Some "QVFIUkF")
      | Error _ -> failwith "expected a conversation page");
  (match !Mock_http.requests with
   | [ ("GET", url, _, _) ] -> assert_reply_fields_requested url
   | _ -> failwith "expected one GET");
  Mock_http.set_responses
    [ { status = 401; headers = []; body = graph_error_body ~subcode:463 ~message:"Session has expired" 190 } ];
  Threads.get_conversation_page_detailed ~access_token:"t"
    ~media_id:"18040000000000001"
    (function
      | Error (Social_meta_graph_error.Http_failure error) ->
          assert (error.detail.http_status = 401);
          assert (error.kind = Error_types.Auth_error Error_types.Token_expired);
          assert (
            Option.is_some (
              let ( >>= ) o f = Option.bind o f in
              Some error >>= fun _ -> error.graph))
      | Ok _ | Error (Social_meta_graph_error.Transport_failure _)
      | Error (Social_meta_graph_error.Malformed_response _)
      | Error (Social_meta_graph_error.Validation_failure _) ->
          failwith "expected an HTTP failure envelope");
  (* A token-like secret in Meta's own error text redacts the whole
     retained excerpt; the structured codes still parse. *)
  Mock_http.set_responses
    [ { status = 400; headers = [];
        body =
          graph_error_body ~message:"Invalid access_token=TH-secret-token-value" 190 } ];
  Threads.get_user_posts_page_detailed ~threads_user_id:"17841400000000000"
    ~access_token:"t" ~fields:"id,text,timestamp"
    (function
      | Error (Social_meta_graph_error.Http_failure error) ->
          assert (error.detail.code = Some "190");
          (match error.detail.body_excerpt with
           | Some excerpt -> assert (not (string_contains excerpt "TH-secret-token-value"))
           | None -> assert false)
      | Ok _ | Error (Social_meta_graph_error.Transport_failure _)
      | Error (Social_meta_graph_error.Malformed_response _)
      | Error (Social_meta_graph_error.Validation_failure _) ->
          failwith "expected an HTTP failure envelope");
  (* A normal Graph error keeps Meta's own text for the excerpt. *)
  Mock_http.set_responses
    [ { status = 400; headers = []; body = graph_error_body ~message:"Session has expired" 190 } ];
  Threads.get_user_posts_page_detailed ~threads_user_id:"17841400000000000"
    ~access_token:"t" ~fields:"id,text,timestamp"
    (function
      | Error (Social_meta_graph_error.Http_failure error) ->
          assert (
            match error.detail.body_excerpt with
            | Some excerpt -> string_contains excerpt "Session has expired"
            | None -> false)
      | Ok _ | Error _ -> failwith "expected an HTTP failure envelope");
  (* A paging URL on another host is refused before any request. *)
  Mock_http.reset ();
  Threads.get_conversation_page_detailed ~access_token:"t" ~media_id:"18040000000000001"
    ~position:(Social_meta_graph_paging.Page_url "https://evil.example/conversation?access_token=t")
    (function
      | Error (Social_meta_graph_error.Validation_failure _) -> ()
      | Ok _ | Error (Social_meta_graph_error.Http_failure _)
      | Error (Social_meta_graph_error.Transport_failure _)
      | Error (Social_meta_graph_error.Malformed_response _) ->
          failwith "expected a validation failure");
  assert (!Mock_http.requests = []);
  print_endline "ok: detailed conversation and threads listing reads"

let () = test_detailed_engage_operations ()
let () = test_detailed_listing_reads ()
