(** Focused OAuth contract tests for Mastodon provider. *)

module Mock_http : Social_core.HTTP_CLIENT = struct
  let get ?headers:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }

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
    on_success {
      Social_core.access_token =
        {|{"access_token":"test_access_token","instance_url":"https://mastodon.social"}|};
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

module Mastodon = Social_mastodon_v1.Make (Mock_config)

module Contract_http = struct
  let last_post_url : string option ref = ref None
  let last_post_headers : (string * string) list ref = ref []
  let last_post_body : string option ref = ref None
  let next_post_response : Social_core.response option ref = ref None

  let set_next_post_response resp =
    next_post_response := Some resp

  let reset () =
    last_post_url := None;
    last_post_headers := [];
    last_post_body := None;
    next_post_response := None

  let get ?headers:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }

  let post ?headers ?body url on_success _on_error =
    last_post_url := Some url;
    last_post_headers := Option.value ~default:[] headers;
    last_post_body := body;
    let response =
      match !next_post_response with
      | Some resp -> resp
      | None -> { Social_core.status = 200; headers = []; body = {|{"access_token":"token","token_type":"Bearer","scope":"read write follow"}|} }
    in
    next_post_response := None;
    on_success response

  let post_multipart ?headers:_ ~parts:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }

  let put ?headers:_ ?body:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }

  let delete ?headers:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }
end

module Contract_config = struct
  module Http = Contract_http

  let get_env _key = None
  let get_credentials ~account_id:_ _on_success on_error = on_error "not-used"
  let update_credentials ~account_id:_ ~credentials:_ on_success _on_error = on_success ()
  let encrypt _data on_success _on_error = on_success "encrypted"
  let decrypt _data on_success _on_error = on_success "decrypted"
  let sleep ~seconds:_ on_success _on_error = on_success ()
  let update_health_status ~account_id:_ ~status:_ ~error_message:_ on_success _on_error = on_success ()
end

module Mastodon_contract = Social_mastodon_v1.Make (Contract_config)

let get_single_query_param uri key =
  match Uri.get_query_param uri key with
  | Some value -> value
  | None -> failwith (Printf.sprintf "Missing query parameter: %s" key)

let test_oauth_url_encoding () =
  Printf.printf "Test: OAuth URL encoding contract... ";
  let redirect_uri = "https://example.com/callback?foo=bar&baz=qux" in
  let state = "state with spaces/+=" in
  let url =
    Mastodon.get_oauth_url
      ~instance_url:"https://mastodon.social"
      ~client_id:"client_123"
      ~redirect_uri
      ~scopes:"read write follow"
      ~state:(Some state)
      ()
  in
  let uri = Uri.of_string url in
  let client_id = get_single_query_param uri "client_id" in
  let parsed_redirect = get_single_query_param uri "redirect_uri" in
  let parsed_response_type = get_single_query_param uri "response_type" in
  let parsed_scope = get_single_query_param uri "scope" in
  let parsed_state = get_single_query_param uri "state" in
  assert (String.equal client_id "client_123");
  assert (String.equal parsed_redirect redirect_uri);
  assert (String.equal parsed_response_type "code");
  assert (String.equal parsed_scope "read write follow");
  assert (String.equal parsed_state state);
  Printf.printf "✓\n"

let test_oauth_url_pkce_contract () =
  Printf.printf "Test: OAuth URL PKCE contract... ";
  let url =
    Mastodon.get_oauth_url
      ~instance_url:"https://mastodon.social"
      ~client_id:"client_123"
      ~redirect_uri:"urn:ietf:wg:oauth:2.0:oob"
      ~scopes:"read write"
      ~code_challenge:(Some "challenge_value")
      ()
  in
  let uri = Uri.of_string url in
  let challenge = get_single_query_param uri "code_challenge" in
  let method_ = get_single_query_param uri "code_challenge_method" in
  assert (String.equal challenge "challenge_value");
  assert (String.equal method_ "S256");
  Printf.printf "✓\n"

let test_verify_oauth_state_helper () =
  Printf.printf "Test: OAuth callback state verifier... ";
  assert (Mastodon.verify_oauth_state ~expected_state:"abc123" ~returned_state:"abc123");
  assert (not (Mastodon.verify_oauth_state ~expected_state:"abc123" ~returned_state:"other"));
  Printf.printf "✓\n"

let test_exchange_code_request_contract () =
  Printf.printf "Test: OAuth token exchange request contract... ";
  Contract_http.reset ();
  let got_result = ref false in
  Mastodon_contract.exchange_code
    ~instance_url:"https://mastodon.social"
    ~client_id:"client_123"
    ~client_secret:"secret_456"
    ~redirect_uri:"https://example.com/callback"
    ~code:"code_789"
    ~code_verifier:(Some "verifier_abc")
    (fun result ->
      got_result := true;
      match result with
      | Ok _ -> ()
      | Error err ->
          failwith (Printf.sprintf "Unexpected exchange error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  let url = Option.get !(Contract_http.last_post_url) in
  let headers = !(Contract_http.last_post_headers) in
  let body = Option.get !(Contract_http.last_post_body) in
  assert (String.equal url "https://mastodon.social/oauth/token");
  assert (List.mem ("Content-Type", "application/json") headers);
  let json = Yojson.Basic.from_string body in
  let open Yojson.Basic.Util in
  assert (json |> member "client_id" |> to_string = "client_123");
  assert (json |> member "client_secret" |> to_string = "secret_456");
  assert (json |> member "redirect_uri" |> to_string = "https://example.com/callback");
  assert (json |> member "grant_type" |> to_string = "authorization_code");
  assert (json |> member "code" |> to_string = "code_789");
  assert (json |> member "code_verifier" |> to_string = "verifier_abc");
  assert (json |> member "scope" = `Null);
  Printf.printf "✓\n"

let test_exchange_code_missing_required_scope () =
  Printf.printf "Test: OAuth exchange missing scope mapping... ";
  Contract_http.reset ();
  Contract_http.set_next_post_response {
    Social_core.status = 200;
    headers = [];
    body = {|{"access_token":"token","token_type":"Bearer","scope":"read"}|};
  };
  let got_result = ref false in
  Mastodon_contract.exchange_code
    ~instance_url:"https://mastodon.social"
    ~client_id:"client_123"
    ~client_secret:"secret_456"
    ~redirect_uri:"https://example.com/callback"
    ~code:"code_789"
    (fun result ->
      got_result := true;
      match result with
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions { required = missing; _ })) ->
          assert (List.mem "write" missing);
          assert (List.mem "follow" missing)
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err))
      | Ok _ ->
          failwith "Expected scope failure, got success");
  assert !got_result;
  Printf.printf "✓\n"

let test_exchange_code_http_error_mapping () =
  Printf.printf "Test: OAuth exchange HTTP error mapping... ";
  Contract_http.reset ();
  Contract_http.set_next_post_response {
    Social_core.status = 401;
    headers = [];
    body = {|{"error":"The access token is invalid"}|};
  };
  let got_result = ref false in
  Mastodon_contract.exchange_code
    ~instance_url:"https://mastodon.social"
    ~client_id:"client_123"
    ~client_secret:"secret_456"
    ~redirect_uri:"https://example.com/callback"
    ~code:"code_789"
    (fun result ->
      got_result := true;
      match result with
      | Error (Error_types.Auth_error Error_types.Token_invalid) -> ()
      | Error err ->
          failwith (Printf.sprintf "Unexpected mapped error: %s" (Error_types.error_to_string err))
      | Ok _ ->
          failwith "Expected auth error, got success");
  assert !got_result;
  Printf.printf "✓\n"

let test_exchange_code_missing_scope_tolerated () =
  Printf.printf "Test: OAuth exchange missing scope tolerated... ";
  Contract_http.reset ();
  Contract_http.set_next_post_response {
    Social_core.status = 200;
    headers = [];
    body = {|{"access_token":"token","token_type":"Bearer"}|};
  };
  let got_result = ref false in
  Mastodon_contract.exchange_code
    ~instance_url:"https://mastodon.social"
    ~client_id:"client_123"
    ~client_secret:"secret_456"
    ~redirect_uri:"https://example.com/callback"
    ~code:"code_789"
    (fun result ->
      got_result := true;
      match result with
      | Ok creds -> assert (String.equal creds.access_token "token")
      | Error err -> failwith (Printf.sprintf "Unexpected missing-scope error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  Printf.printf "✓\n"

let test_exchange_code_scope_order_tolerated () =
  Printf.printf "Test: OAuth exchange scope order tolerated... ";
  Contract_http.reset ();
  Contract_http.set_next_post_response {
    Social_core.status = 200;
    headers = [];
    body = {|{"access_token":"token","token_type":"Bearer","scope":"follow read write"}|};
  };
  let got_result = ref false in
  Mastodon_contract.exchange_code
    ~instance_url:"https://mastodon.social"
    ~client_id:"client_123"
    ~client_secret:"secret_456"
    ~redirect_uri:"https://example.com/callback"
    ~code:"code_789"
    (fun result ->
      got_result := true;
      match result with
      | Ok _ -> ()
      | Error err -> failwith (Printf.sprintf "Unexpected scope-order error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  Printf.printf "✓\n"

let test_exchange_code_missing_auth_type_defaults () =
  Printf.printf "Test: OAuth exchange missing auth_type defaults... ";
  Contract_http.reset ();
  Contract_http.set_next_post_response {
    Social_core.status = 200;
    headers = [];
    body = {|{"access_token":"token","scope":"read write follow"}|};
  };
  let got_result = ref false in
  Mastodon_contract.exchange_code
    ~instance_url:"https://mastodon.social"
    ~client_id:"client_123"
    ~client_secret:"secret_456"
    ~redirect_uri:"https://example.com/callback"
    ~code:"code_789"
    (fun result ->
      got_result := true;
      match result with
      | Ok creds -> assert (creds.auth_type = Social_core.Bearer)
      | Error err -> failwith (Printf.sprintf "Unexpected missing-auth_type error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  Printf.printf "✓\n"

let () =
  Printf.printf "\n=== Mastodon OAuth Contract Tests ===\n\n";
  test_oauth_url_encoding ();
  test_oauth_url_pkce_contract ();
  test_verify_oauth_state_helper ();
  test_exchange_code_request_contract ();
  test_exchange_code_missing_required_scope ();
  test_exchange_code_http_error_mapping ();
  test_exchange_code_missing_scope_tolerated ();
  test_exchange_code_scope_order_tolerated ();
  test_exchange_code_missing_auth_type_defaults ();
  Printf.printf "\n✓ OAuth contract tests passed\n"
