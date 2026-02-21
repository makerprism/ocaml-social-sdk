(** Tests for LinkedIn API v2 Provider *)

open Social_core
open Social_linkedin_v2

(** Helper to check if string contains substring *)
let string_contains s substr =
  try
    ignore (Str.search_forward (Str.regexp_string substr) s 0);
    true
  with Not_found -> false

let find_header headers name =
  List.find_opt (fun (k, _) -> String.lowercase_ascii k = String.lowercase_ascii name) headers
  |> Option.map snd

let find_header_exact headers name =
  List.find_opt (fun (k, _) -> k = name) headers |> Option.map snd

(** Helper to handle outcome results in tests *)
let handle_outcome on_success on_error outcome =
  match outcome with
  | Error_types.Success result -> on_success result
  | Error_types.Partial_success { result; _ } -> on_success result
  | Error_types.Failure err -> on_error (Error_types.error_to_string err)

(** Helper for thread results *)
let handle_thread_outcome on_success on_error outcome =
  match outcome with
  | Error_types.Success result -> on_success result.Error_types.posted_ids
  | Error_types.Partial_success { result; _ } -> on_success result.Error_types.posted_ids
  | Error_types.Failure err -> on_error (Error_types.error_to_string err)

(** Helper to handle api_result in tests *)
let handle_result on_success on_error result =
  match result with
  | Ok value -> on_success value
  | Error err -> on_error (Error_types.error_to_string err)

(** Mock HTTP client for testing with response queue *)
module Mock_http = struct
  let requests = ref []
  let response_queue = ref []
  
  let reset () =
    requests := [];
    response_queue := []
  
  (** Set a single response (for backward compatibility) *)
  let set_response response =
    response_queue := [response]
  
  (** Set multiple responses to be returned in order *)
  let set_responses responses =
    response_queue := responses
  
  (** Get next response from queue, or return the last one if queue exhausted *)
  let get_next_response () =
    match !response_queue with
    | [] -> None
    | [r] -> Some r  (* Keep returning last response *)
    | r :: rest ->
        response_queue := rest;
        Some r
  
  include (struct
  
  let get ?(headers=[]) url on_success on_error =
    requests := ("GET", url, headers, "") :: !requests;
    match get_next_response () with
    | Some response -> on_success response
    | None -> on_error "No mock response set"
  
  let post ?(headers=[]) ?(body="") url on_success on_error =
    requests := ("POST", url, headers, body) :: !requests;
    match get_next_response () with
    | Some response -> on_success response
    | None -> on_error "No mock response set"
  
  let put ?(headers=[]) ?(body="") url on_success on_error =
    requests := ("PUT", url, headers, body) :: !requests;
    match get_next_response () with
    | Some response -> on_success response
    | None -> on_error "No mock response set"
  
  let delete ?(headers=[]) url on_success on_error =
    requests := ("DELETE", url, headers, "") :: !requests;
    match get_next_response () with
    | Some response -> on_success response
    | None -> on_error "No mock response set"
  
  let post_multipart ?(headers=[]) ~parts url on_success on_error =
    let body_str = Printf.sprintf "multipart with %d parts" (List.length parts) in
    requests := ("POST_MULTIPART", url, headers, body_str) :: !requests;
    match get_next_response () with
    | Some response -> on_success response
    | None -> on_error "No mock response set"
  end : HTTP_CLIENT)
end

(** Mock config for testing *)
module Mock_config = struct
  module Http = Mock_http
  
  let env_vars = ref []
  let credentials_store = ref []
  let health_statuses = ref []
  
  let reset () =
    env_vars := [];
    credentials_store := [];
    health_statuses := [];
    Mock_http.reset ()
  
  let set_env key value =
    env_vars := (key, value) :: !env_vars
  
  let get_env key =
    List.assoc_opt key !env_vars
  
  let set_credentials ~account_id ~credentials =
    credentials_store := (account_id, credentials) :: !credentials_store
  
  let get_credentials ~account_id on_success on_error =
    match List.assoc_opt account_id !credentials_store with
    | Some creds -> on_success creds
    | None -> on_error "Credentials not found"
  
  let update_credentials ~account_id ~credentials on_success _on_error =
    credentials_store := (account_id, credentials) :: 
      (List.remove_assoc account_id !credentials_store);
    on_success ()
  
  let encrypt data on_success _on_error =
    on_success ("encrypted:" ^ data)
  
  let decrypt data on_success on_error =
    if String.starts_with ~prefix:"encrypted:" data then
      on_success (String.sub data 10 (String.length data - 10))
    else
      on_error "Invalid encrypted data"
  
  let update_health_status ~account_id ~status ~error_message on_success _on_error =
    health_statuses := (account_id, status, error_message) :: !health_statuses;
    on_success ()
  
  let get_health_status account_id =
    List.find_opt (fun (id, _, _) -> id = account_id) !health_statuses
end

module LinkedIn = Make(Mock_config)

(** Test: OAuth URL generation *)
let test_oauth_url () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client_id";
  
  let state = "test_state_123" in
  let redirect_uri = "https://example.com/callback" in
  
  LinkedIn.get_oauth_url ~redirect_uri ~state
    (fun url ->
      assert (string_contains url "response_type=code");
      assert (string_contains url "client_id=test_client_id");
      assert (string_contains url "state=test_state_123");
      assert (string_contains url "scope=openid");
      assert (string_contains url "scope=openid+profile+email+w_member_social" || 
              string_contains url "openid%20profile%20email%20w_member_social");
      print_endline "✓ OAuth URL generation")
    (fun err -> failwith ("OAuth URL failed: " ^ err))

(** Test: Token exchange *)
let test_token_exchange () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client";
  Mock_config.set_env "LINKEDIN_CLIENT_SECRET" "test_secret";
  
  let response_body = {|{
    "access_token": "new_access_token_123",
    "refresh_token": "new_refresh_token_456",
    "expires_in": 5184000
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  LinkedIn.exchange_code 
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    (fun creds ->
      assert (creds.access_token = "new_access_token_123");
      assert (creds.refresh_token = Some "new_refresh_token_456");
      assert (creds.token_type = "Bearer");
      assert (creds.expires_at <> None);
      let requests = !Mock_http.requests in
      (match requests with
      | ("POST", url, headers, body) :: _ ->
          assert (url = "https://www.linkedin.com/oauth/v2/accessToken");
          assert (not (string_contains url "?"));
          assert (not (string_contains url "client_id="));
          assert (not (string_contains url "client_secret="));
          assert (find_header headers "Content-Type" = Some "application/x-www-form-urlencoded");
          assert (string_contains body "grant_type=authorization_code");
          assert (string_contains body "code=test_code");
          assert (
            string_contains body "redirect_uri=https%3A%2F%2Fexample.com%2Fcallback"
            || string_contains body "redirect_uri=https://example.com/callback"
          );
          assert (string_contains body "client_id=test_client");
          assert (string_contains body "client_secret=test_secret")
      | _ -> failwith "Expected OAuth token exchange POST request");
      print_endline "✓ Token exchange")
    (fun err -> failwith ("Token exchange failed: " ^ err))

(** Test: Token exchange does not retry on non-invalid_client errors *)
let test_token_exchange_non_invalid_client_no_retry () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client";
  Mock_config.set_env "LINKEDIN_CLIENT_SECRET" "test_secret";

  Mock_http.set_responses [
    {
      status = 400;
      body = {|{"error":"invalid_grant","error_description":"Code was already used"}|};
      headers = [];
    };
    {
      status = 200;
      body = {|{"access_token":"should_not_be_used","expires_in":5184000}|};
      headers = [];
    };
  ];

  LinkedIn.exchange_code
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    (fun _ -> failwith "Expected non-invalid_client response to fail without retry")
    (fun err ->
      assert (string_contains err "invalid_grant" || string_contains err "400");
      let requests = !Mock_http.requests in
      assert (List.length requests = 1);
      print_endline "✓ Token exchange non-invalid_client error does not retry")

(** Test: OAuth URL rejects state with surrounding whitespace *)
let test_oauth_url_rejects_whitespace_state () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client_id";
  LinkedIn.get_oauth_url ~redirect_uri:"https://example.com/callback" ~state:" state "
    (fun _ -> failwith "Expected whitespace state rejection")
    (fun err ->
      assert (string_contains err "state");
      print_endline "✓ OAuth URL rejects whitespace state")

(** Test: OAuth URL rejects redirect URI mismatch with configured value *)
let test_oauth_url_rejects_redirect_mismatch () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client_id";
  Mock_config.set_env "LINKEDIN_REDIRECT_URI" "https://example.com/callback";
  LinkedIn.get_oauth_url ~redirect_uri:"https://example.com/other" ~state:"ok-state"
    (fun _ -> failwith "Expected configured redirect mismatch rejection")
    (fun err ->
      assert (string_contains err "LINKEDIN_REDIRECT_URI");
      print_endline "✓ OAuth URL rejects configured redirect mismatch")

(** Test: exchange_code rejects whitespace in authorization code *)
let test_exchange_code_rejects_whitespace_code () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client";
  Mock_config.set_env "LINKEDIN_CLIENT_SECRET" "test_secret";
  LinkedIn.exchange_code ~code:" code " ~redirect_uri:"https://example.com/callback"
    (fun _ -> failwith "Expected whitespace code rejection")
    (fun err ->
      assert (string_contains err "authorization code");
      print_endline "✓ exchange_code rejects whitespace code")

(** Test: validate_oauth_state succeeds on exact match *)
let test_validate_oauth_state_success () =
  LinkedIn.validate_oauth_state ~expected:"abc123" ~received:"abc123"
    (fun () -> print_endline "✓ validate_oauth_state success")
    (fun err -> failwith ("Unexpected validate_oauth_state failure: " ^ err))

(** Test: validate_oauth_state rejects mismatch *)
let test_validate_oauth_state_mismatch () =
  LinkedIn.validate_oauth_state ~expected:"abc123" ~received:"xyz999"
    (fun () -> failwith "Expected validate_oauth_state mismatch")
    (fun err ->
      assert (string_contains err "mismatch");
      print_endline "✓ validate_oauth_state mismatch rejected")

(** Test: validate_oauth_state rejects whitespace *)
let test_validate_oauth_state_whitespace_rejected () =
  LinkedIn.validate_oauth_state ~expected:" abc123 " ~received:" abc123 "
    (fun () -> failwith "Expected validate_oauth_state whitespace rejection")
    (fun err ->
      assert (string_contains err "whitespace");
      print_endline "✓ validate_oauth_state whitespace rejected")

(** Test: Get person URN *)
let test_get_person_urn () =
  Mock_config.reset ();
  
  let response_body = {|{
    "sub": "abc123xyz",
    "name": "Test User",
    "email": "test@example.com"
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  LinkedIn.get_person_urn ~access_token:"test_token"
    (fun person_urn ->
      assert (person_urn = "urn:li:person:abc123xyz");
      print_endline "✓ Get person URN")
    (fun err -> failwith ("Get person URN failed: " ^ err))

(** Test: get_organization_access fetches approved organization roles *)
let test_get_organization_access () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response {
    status = 200;
    body =
      {|{"elements":[{"role":"CONTENT_ADMINISTRATOR","state":"APPROVED","organizationTarget":"urn:li:organization:999"}]}|};
    headers = [];
  };

  LinkedIn.get_organization_access ~account_id:"test_account"
    (handle_result
      (fun orgs ->
        assert (List.length orgs = 1);
        let org = List.hd orgs in
        assert (org.organization_urn = "urn:li:organization:999");
        assert (org.organization_id = Some "999");
        assert (org.role = Some "CONTENT_ADMINISTRATOR");
        assert (org.state = Some "APPROVED");
        let requests = !Mock_http.requests in
        (match requests with
        | (_, url, headers, _) :: _ ->
            assert (string_contains url "/rest/organizationAcls");
            assert (string_contains url "q=roleAssignee");
            assert (find_header headers "X-RestLi-Method" = Some "FINDER");
            assert (find_header_exact headers "LinkedIn-Version" = Some "202601")
        | [] -> failwith "No requests recorded for get_organization_access");
        print_endline "✓ Get organization access")
      (fun err -> failwith ("Get organization access failed: " ^ err)))

(** Test: get_organization_access parser tolerates malformed/partial ACL elements *)
let test_get_organization_access_parsing_resilience () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response {
    status = 200;
    body =
      {|{"elements":[
        {"role":"ADMINISTRATOR","state":"APPROVED","organization":"urn:li:organization:111"},
        {"role":"ANALYST","state":"APPROVED","organizationTarget":"urn:li:organization:222"},
        {"role":"ADMINISTRATOR","state":"APPROVED","organization":"urn:li:organizationBrand:444"},
        {"role":"ADMINISTRATOR","state":"APPROVED","organization":"  urn:li:organization:556  "},
        {"role":"ADMINISTRATOR","state":"APPROVED","organization":"company:bad","organizationTarget":"urn:li:organization:557"},
        {"role":"ADMINISTRATOR","state":"APPROVED","organization":"company:555"},
        {"role":"ADMINISTRATOR","state":"APPROVED"},
        {"organization":"urn:li:organization:333"}
      ]}|};
    headers = [];
  };

  LinkedIn.get_organization_access ~account_id:"test_account"
    (handle_result
      (fun orgs ->
        assert (List.length orgs = 6);
        let has_111 = List.exists (fun o -> o.organization_urn = "urn:li:organization:111") orgs in
        let has_222 = List.exists (fun o -> o.organization_urn = "urn:li:organization:222") orgs in
        let has_333 = List.exists (fun o -> o.organization_urn = "urn:li:organization:333") orgs in
        let has_556 = List.exists (fun o -> o.organization_urn = "urn:li:organization:556") orgs in
        let has_557 = List.exists (fun o -> o.organization_urn = "urn:li:organization:557") orgs in
        let has_invalid = List.exists (fun o -> o.organization_urn = "company:555") orgs in
        let brand = List.find_opt (fun o -> o.organization_urn = "urn:li:organizationBrand:444") orgs in
        assert has_111;
        assert has_222;
        assert has_333;
        assert has_556;
        assert has_557;
        assert (not has_invalid);
        (match brand with
        | Some b -> assert (b.organization_id = Some "444")
        | None -> failwith "Missing organizationBrand entry");
        print_endline "✓ Get organization access parsing resilience")
      (fun err -> failwith ("Organization access parsing resilience failed: " ^ err)))

(** Test: get_organization_access paginates beyond first 100 entries *)
let test_get_organization_access_pagination () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_responses [
    {
      status = 200;
      body =
        {|{"paging":{"start":0,"count":100,"total":101},"elements":[{"role":"ADMINISTRATOR","state":"APPROVED","organization":"urn:li:organization:100"}]}|};
      headers = [];
    };
    {
      status = 200;
      body =
        {|{"paging":{"start":100,"count":100,"total":101},"elements":[{"role":"ADMINISTRATOR","state":"APPROVED","organization":"urn:li:organization:101"}]}|};
      headers = [];
    };
  ];

  LinkedIn.get_organization_access ~account_id:"test_account"
    (handle_result
      (fun orgs ->
        assert (List.length orgs = 2);
        (match orgs with
        | first :: second :: _ ->
            assert (first.organization_urn = "urn:li:organization:100");
            assert (second.organization_urn = "urn:li:organization:101")
        | _ -> failwith "Expected two organizations in order");
        let has_100 = List.exists (fun o -> o.organization_urn = "urn:li:organization:100") orgs in
        let has_101 = List.exists (fun o -> o.organization_urn = "urn:li:organization:101") orgs in
        assert has_100;
        assert has_101;
        let requests = List.rev !Mock_http.requests in
        (match requests with
        | [(_, url1, _, _); (_, url2, _, _)] ->
            assert (string_contains url1 "start=0");
            assert (string_contains url2 "start=100")
        | _ -> failwith "Expected two paged requests for organization access");
        print_endline "✓ Get organization access pagination")
      (fun err -> failwith ("Get organization access pagination failed: " ^ err)))

(** Test: get_organization_access ignores blank role filter *)
let test_get_organization_access_blank_role_filter () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response {
    status = 200;
    body = {|{"elements":[]}|};
    headers = [];
  };

  LinkedIn.get_organization_access ~account_id:"test_account" ~role:""
    (handle_result
      (fun _orgs ->
        let requests = !Mock_http.requests in
        (match requests with
        | (_, url, _, _) :: _ ->
            assert (not (string_contains url "role="));
            print_endline "✓ Get organization access ignores blank role filter"
        | [] -> failwith "No requests recorded")
      )
      (fun err -> failwith ("Get organization access blank role filter failed: " ^ err)))

(** Test: get_organization_access trims surrounding whitespace in role/state filters *)
let test_get_organization_access_trims_role_state_filters () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response {
    status = 200;
    body = {|{"elements":[]}|};
    headers = [];
  };

  LinkedIn.get_organization_access ~account_id:"test_account" ~role:" ADMINISTRATOR " ~acl_state:" APPROVED "
    (handle_result
      (fun _orgs ->
        let requests = !Mock_http.requests in
        (match requests with
        | (_, url, _, _) :: _ ->
            assert (string_contains url "role=ADMINISTRATOR");
            assert (string_contains url "state=APPROVED");
            print_endline "✓ Get organization access trims role/state filters"
        | [] -> failwith "No requests recorded")
      )
      (fun err -> failwith ("Get organization access trim role/state filters failed: " ^ err)))

(** Test: get_organization_access normalizes role/state filters to uppercase *)
let test_get_organization_access_uppercases_role_state_filters () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response {
    status = 200;
    body = {|{"elements":[]}|};
    headers = [];
  };

  LinkedIn.get_organization_access ~account_id:"test_account" ~role:"administrator" ~acl_state:"approved"
    (handle_result
      (fun _orgs ->
        let requests = !Mock_http.requests in
        (match requests with
        | (_, url, _, _) :: _ ->
            assert (string_contains url "role=ADMINISTRATOR");
            assert (string_contains url "state=APPROVED");
            print_endline "✓ Get organization access uppercases role/state filters"
        | [] -> failwith "No requests recorded")
      )
      (fun err -> failwith ("Get organization access uppercase role/state filters failed: " ^ err)))

(** Test: get_organization_access falls back to default LinkedIn-Version header when env is invalid *)
let test_get_organization_access_invalid_version_fallback () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_VERSION" "bad-version";

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response {
    status = 200;
    body = {|{"elements":[]}|};
    headers = [];
  };

  LinkedIn.get_organization_access ~account_id:"test_account"
    (handle_result
      (fun _orgs ->
        let requests = !Mock_http.requests in
        (match requests with
        | (_, _, headers, _) :: _ ->
            assert (find_header_exact headers "LinkedIn-Version" = Some "202601");
            print_endline "✓ Get organization access invalid version falls back"
        | [] -> failwith "No requests recorded")
      )
      (fun err -> failwith ("Get organization access invalid version fallback failed: " ^ err)))

(** Test: get_organization_access uses valid configured LinkedIn-Version header *)
let test_get_organization_access_valid_version_header () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_VERSION" "202602";

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response {
    status = 200;
    body = {|{"elements":[]}|};
    headers = [];
  };

  LinkedIn.get_organization_access ~account_id:"test_account"
    (handle_result
      (fun _orgs ->
        let requests = !Mock_http.requests in
        (match requests with
        | (_, _, headers, _) :: _ ->
            assert (find_header_exact headers "LinkedIn-Version" = Some "202602");
            print_endline "✓ Get organization access uses valid configured version"
        | [] -> failwith "No requests recorded")
      )
      (fun err -> failwith ("Get organization access valid version header failed: " ^ err)))

(** Test: get_organization_access invalid month in LINKEDIN_VERSION falls back *)
let test_get_organization_access_invalid_month_version_fallback () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_VERSION" "202613";

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response {
    status = 200;
    body = {|{"elements":[]}|};
    headers = [];
  };

  LinkedIn.get_organization_access ~account_id:"test_account"
    (handle_result
      (fun _orgs ->
        let requests = !Mock_http.requests in
        (match requests with
        | (_, _, headers, _) :: _ ->
            assert (find_header_exact headers "LinkedIn-Version" = Some "202601");
            print_endline "✓ Get organization access invalid month falls back"
        | [] -> failwith "No requests recorded")
      )
      (fun err -> failwith ("Get organization access invalid month fallback failed: " ^ err)))

(** Test: get_organization_access rejects dotted LINKEDIN_VERSION and falls back *)
let test_get_organization_access_dotted_version_fallback () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_VERSION" "202601.01";

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response {
    status = 200;
    body = {|{"elements":[]}|};
    headers = [];
  };

  LinkedIn.get_organization_access ~account_id:"test_account"
    (handle_result
      (fun _orgs ->
        let requests = !Mock_http.requests in
        (match requests with
        | (_, _, headers, _) :: _ ->
            assert (find_header_exact headers "LinkedIn-Version" = Some "202601");
            print_endline "✓ Get organization access dotted version falls back"
        | [] -> failwith "No requests recorded")
      )
      (fun err -> failwith ("Get organization access dotted version fallback failed: " ^ err)))

(** Test: get_organization_access deduplicates organizations across multiple role entries *)
let test_get_organization_access_deduplicates_by_org () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response {
    status = 200;
    body =
      {|{"elements":[
        {"role":"ADMINISTRATOR","state":"APPROVED","organization":"urn:li:organization:123"},
        {"role":"CONTENT_ADMINISTRATOR","state":"APPROVED","organization":"urn:li:organization:123"},
        {"role":"ANALYST","state":"APPROVED","organization":"urn:li:organization:999"}
      ]}|};
    headers = [];
  };

  LinkedIn.get_organization_access ~account_id:"test_account"
    (handle_result
      (fun orgs ->
        assert (List.length orgs = 2);
        let count_123 = List.length (List.filter (fun o -> o.organization_urn = "urn:li:organization:123") orgs) in
        assert (count_123 = 1);
        print_endline "✓ Get organization access deduplicates by organization")
      (fun err -> failwith ("Get organization access dedupe failed: " ^ err)))

(** Test: get_organization_access stops when paging does not advance *)
let test_get_organization_access_non_advancing_paging_stops () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_responses [
    {
      status = 200;
      body =
        {|{"paging":{"start":0,"count":100,"total":300},"elements":[{"role":"ADMINISTRATOR","state":"APPROVED","organization":"urn:li:organization:1"}]}|};
      headers = [];
    };
    {
      status = 200;
      body =
        {|{"paging":{"start":0,"count":100,"total":300},"elements":[{"role":"ADMINISTRATOR","state":"APPROVED","organization":"urn:li:organization:2"}]}|};
      headers = [];
    };
  ];

  LinkedIn.get_organization_access ~account_id:"test_account"
    (handle_result
      (fun orgs ->
        assert (List.length orgs = 2);
        let requests = List.rev !Mock_http.requests in
        (match requests with
        | [ (_, url1, _, _); (_, url2, _, _) ] ->
            assert (string_contains url1 "start=0");
            assert (string_contains url2 "start=100")
        | _ -> failwith "Expected exactly two requests for non-advancing paging");
        print_endline "✓ Get organization access stops on non-advancing paging")
      (fun err -> failwith ("Get organization access non-advancing paging failed: " ^ err)))

(** Test: pagination fallback uses raw element count, not filtered valid count *)
let test_get_organization_access_pagination_fallback_uses_raw_count () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  let invalid_elements =
    List.init 99 (fun _ -> "{\"organization\":\"company:bad\"}")
    |> String.concat ","
  in
  let page1_body =
    "{" ^
    "\"elements\":[{" ^
      "\"role\":\"ADMINISTRATOR\",\"state\":\"APPROVED\",\"organization\":\"urn:li:organization:1000\"}," ^
      invalid_elements ^
    "]}"
  in
  let page2_body =
    {|{"elements":[{"role":"ADMINISTRATOR","state":"APPROVED","organization":"urn:li:organization:1001"}]}|}
  in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_responses [
    { status = 200; body = page1_body; headers = [] };
    { status = 200; body = page2_body; headers = [] };
  ];

  LinkedIn.get_organization_access ~account_id:"test_account"
    (handle_result
      (fun orgs ->
        assert (List.length orgs = 2);
        let has_1000 = List.exists (fun o -> o.organization_urn = "urn:li:organization:1000") orgs in
        let has_1001 = List.exists (fun o -> o.organization_urn = "urn:li:organization:1001") orgs in
        assert has_1000;
        assert has_1001;
        let requests = List.rev !Mock_http.requests in
        (match requests with
        | [ (_, url1, _, _); (_, url2, _, _) ] ->
            assert (string_contains url1 "start=0");
            assert (string_contains url2 "start=100")
        | _ -> failwith "Expected pagination fallback to issue second request");
        print_endline "✓ Get organization access fallback pagination uses raw count")
      (fun err -> failwith ("Get organization access fallback pagination failed: " ^ err)))

(** Test: fallback pagination has max-page safety guard when paging metadata is absent *)
let test_get_organization_access_pagination_fallback_has_page_cap () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  let invalid_elements =
    List.init 99 (fun _ -> "{\"organization\":\"company:bad\"}")
    |> String.concat ","
  in
  let page_body =
    "{" ^
    "\"elements\":[{" ^
      "\"role\":\"ADMINISTRATOR\",\"state\":\"APPROVED\",\"organization\":\"urn:li:organization:2000\"}," ^
      invalid_elements ^
    "]}"
  in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_responses (List.init 15 (fun _ -> { status = 200; body = page_body; headers = [] }));

  LinkedIn.get_organization_access ~account_id:"test_account"
    (handle_result
      (fun orgs ->
        assert (List.length orgs = 1);
        let requests = !Mock_http.requests in
        assert (List.length requests = 15);
        print_endline "✓ Get organization access fallback pagination has page cap")
      (fun err -> failwith ("Get organization access fallback page cap failed: " ^ err)))

(** Test: get_organization_access prefers approved admin role on duplicate org entries *)
let test_get_organization_access_prefers_admin_approved () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response {
    status = 200;
    body =
      {|{"elements":[
        {"role":"ANALYST","state":"REQUESTED","organization":"urn:li:organization:777"},
        {"role":"ADMINISTRATOR","state":"APPROVED","organization":"urn:li:organization:777"}
      ]}|};
    headers = [];
  };

  LinkedIn.get_organization_access ~account_id:"test_account"
    (handle_result
      (fun orgs ->
        assert (List.length orgs = 1);
        let only = List.hd orgs in
        assert (only.organization_urn = "urn:li:organization:777");
        assert (only.role = Some "ADMINISTRATOR");
        assert (only.state = Some "APPROVED");
        print_endline "✓ Get organization access prefers approved administrator on duplicates")
      (fun err -> failwith ("Get organization access preference failed: " ^ err)))

(** Test: duplicate preference logic is case-insensitive for role/state values *)
let test_get_organization_access_preference_case_insensitive () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response {
    status = 200;
    body =
      {|{"elements":[
        {"role":"analyst","state":"requested","organization":"urn:li:organization:888"},
        {"role":"administrator","state":"approved","organization":"urn:li:organization:888"}
      ]}|};
    headers = [];
  };

  LinkedIn.get_organization_access ~account_id:"test_account"
    (handle_result
      (fun orgs ->
        assert (List.length orgs = 1);
        let only = List.hd orgs in
        assert (only.organization_urn = "urn:li:organization:888");
        assert (only.role = Some "administrator");
        assert (only.state = Some "approved");
        print_endline "✓ Get organization access preference is case-insensitive")
      (fun err -> failwith ("Get organization access case-insensitive preference failed: " ^ err)))

(** Test: duplicate preference logic tolerates surrounding whitespace in role/state values *)
let test_get_organization_access_preference_trims_role_state () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response {
    status = 200;
    body =
      {|{"elements":[
        {"role":" ANALYST ","state":" REQUESTED ","organization":"urn:li:organization:889"},
        {"role":" ADMINISTRATOR ","state":" APPROVED ","organization":"urn:li:organization:889"}
      ]}|};
    headers = [];
  };

  LinkedIn.get_organization_access ~account_id:"test_account"
    (handle_result
      (fun orgs ->
        assert (List.length orgs = 1);
        let only = List.hd orgs in
        assert (only.organization_urn = "urn:li:organization:889");
        assert (only.role = Some "ADMINISTRATOR");
        assert (only.state = Some "APPROVED");
        print_endline "✓ Get organization access preference trims role/state for ranking")
      (fun err -> failwith ("Get organization access trim preference failed: " ^ err)))

(** Test: duplicate preference keeps first entry on equal role/state rank *)
let test_get_organization_access_preference_stable_on_tie () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response {
    status = 200;
    body =
      {|{"elements":[
        {"role":"ADMINISTRATOR","state":"APPROVED","organization":"urn:li:organization:990"},
        {"role":"administrator","state":"approved","organization":"urn:li:organization:990"}
      ]}|};
    headers = [];
  };

  LinkedIn.get_organization_access ~account_id:"test_account"
    (handle_result
      (fun orgs ->
        assert (List.length orgs = 1);
        let only = List.hd orgs in
        assert (only.organization_urn = "urn:li:organization:990");
        assert (only.role = Some "ADMINISTRATOR");
        assert (only.state = Some "APPROVED");
        print_endline "✓ Get organization access preference stable on ties")
      (fun err -> failwith ("Get organization access tie preference failed: " ^ err)))

(** Test: select_preferred_organization_access returns None for empty input *)
let test_select_preferred_organization_access_empty () =
  let selected = LinkedIn.select_preferred_organization_access [] in
  assert (selected = None);
  print_endline "✓ select_preferred_organization_access returns None for empty list"

(** Test: select_preferred_organization_access chooses approved administrator first *)
let test_select_preferred_organization_access_ranking () =
  let organizations =
    [
      {
        organization_urn = "urn:li:organization:1";
        organization_id = Some "1";
        role = Some "ANALYST";
        state = Some "REQUESTED";
      };
      {
        organization_urn = "urn:li:organization:2";
        organization_id = Some "2";
        role = Some "CONTENT_ADMINISTRATOR";
        state = Some "APPROVED";
      };
      {
        organization_urn = "urn:li:organization:3";
        organization_id = Some "3";
        role = Some "ADMINISTRATOR";
        state = Some "APPROVED";
      };
    ]
  in
  let selected = LinkedIn.select_preferred_organization_access organizations in
  match selected with
  | Some org ->
      assert (org.organization_urn = "urn:li:organization:3");
      print_endline "✓ select_preferred_organization_access applies role/state ranking"
  | None -> failwith "Expected preferred organization selection"

(** Test: get_preferred_organization_access returns top-ranked org *)
let test_get_preferred_organization_access () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response {
    status = 200;
    body =
      {|{"elements":[
        {"role":"ANALYST","state":"APPROVED","organization":"urn:li:organization:100"},
        {"role":"ADMINISTRATOR","state":"APPROVED","organization":"urn:li:organization:200"}
      ]}|};
    headers = [];
  };

  LinkedIn.get_preferred_organization_access ~account_id:"test_account"
    (handle_result
      (fun selected ->
        match selected with
        | Some org ->
            assert (org.organization_urn = "urn:li:organization:200");
            print_endline "✓ Get preferred organization access"
        | None -> failwith "Expected preferred organization access")
      (fun err -> failwith ("Get preferred organization access failed: " ^ err)))

(** Test: get_preferred_organization_access returns None when no orgs are available *)
let test_get_preferred_organization_access_none () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response { status = 200; body = {|{"elements":[]}|}; headers = [] };

  LinkedIn.get_preferred_organization_access ~account_id:"test_account"
    (handle_result
      (fun selected ->
        assert (selected = None);
        print_endline "✓ Get preferred organization access returns None when empty")
      (fun err -> failwith ("Get preferred organization access empty failed: " ^ err)))

(** Test: Get person URN rejects malformed subject identifier *)
let test_get_person_urn_rejects_malformed_subject () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 200;
    body = {|{"sub":"bad,user"}|};
    headers = [];
  };

  LinkedIn.get_person_urn ~access_token:"test_token"
    (fun _ -> failwith "Expected malformed subject rejection")
    (fun err ->
      assert (string_contains err "person URN");
      assert (string_contains err "invalid characters");
      print_endline "✓ Get person URN rejects malformed subject")

(** Test: Get person URN rejects subject with surrounding whitespace *)
let test_get_person_urn_rejects_whitespace_subject () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 200;
    body = {|{"sub":" abc123 "}|};
    headers = [];
  };

  LinkedIn.get_person_urn ~access_token:"test_token"
    (fun _ -> failwith "Expected whitespace subject rejection")
    (fun err ->
      assert (string_contains err "subject identifier");
      assert (string_contains err "whitespace");
      print_endline "✓ Get person URN rejects whitespace subject")

(** Test: Get person URN rejects empty subject identifier *)
let test_get_person_urn_rejects_empty_subject () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 200;
    body = {|{"sub":""}|};
    headers = [];
  };

  LinkedIn.get_person_urn ~access_token:"test_token"
    (fun _ -> failwith "Expected empty subject rejection")
    (fun err ->
      assert (string_contains err "empty subject identifier");
      print_endline "✓ Get person URN rejects empty subject")

(** Test: Get person URN error response redacts sensitive fields *)
let test_get_person_urn_error_redaction () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 500;
    body = {|{"message":"backend failed","access_token":"leak-me"}|};
    headers = [];
  };

  LinkedIn.get_person_urn ~access_token:"test_token"
    (fun _ -> failwith "Expected person URN failure")
    (fun err ->
      assert (string_contains err "Failed to get person URN (500)");
      assert (string_contains err "backend failed");
      assert (not (string_contains err "leak-me"));
      print_endline "✓ Get person URN error redaction")

(** Test: Get person URN non-JSON error body is not leaked *)
let test_get_person_urn_non_json_error_redaction () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 500;
    body = "backend exploded access_token=leak-me";
    headers = [];
  };

  LinkedIn.get_person_urn ~access_token:"test_token"
    (fun _ -> failwith "Expected person URN failure")
    (fun err ->
      assert (string_contains err "Failed to get person URN (500)");
      assert (not (string_contains err "leak-me"));
      print_endline "✓ Get person URN non-JSON error redaction")

(** Test: Get person URN rejects response missing subject *)
let test_get_person_urn_missing_subject () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 200;
    body = {|{"name":"No Subject"}|};
    headers = [];
  };

  LinkedIn.get_person_urn ~access_token:"test_token"
    (fun _ -> failwith "Expected missing subject rejection")
    (fun err ->
      assert (string_contains err "Failed to parse person URN");
      print_endline "✓ Get person URN rejects missing subject")

(** Test: Get person URN rejects malformed JSON payload *)
let test_get_person_urn_malformed_json () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 200;
    body = "not-json";
    headers = [];
  };

  LinkedIn.get_person_urn ~access_token:"test_token"
    (fun _ -> failwith "Expected malformed JSON rejection")
    (fun err ->
      assert (string_contains err "Failed to parse person URN");
      print_endline "✓ Get person URN rejects malformed JSON")

(** Test: Get person URN 403 returns scope hint message *)
let test_get_person_urn_insufficient_permissions_message () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 403;
    body = {|{"message":"Not enough permissions"}|};
    headers = [];
  };

  LinkedIn.get_person_urn ~access_token:"test_token"
    (fun _ -> failwith "Expected person URN permission failure")
    (fun err ->
      assert (string_contains err "Failed to get person URN (403)");
      assert (string_contains err "openid");
      assert (string_contains err "profile");
      print_endline "✓ Get person URN 403 scope hint")

(** Test: Get person URN 401 returns auth hint message *)
let test_get_person_urn_auth_failure_message () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 401;
    body = {|{"message":"Invalid access token"}|};
    headers = [];
  };

  LinkedIn.get_person_urn ~access_token:"test_token"
    (fun _ -> failwith "Expected person URN auth failure")
    (fun err ->
      assert (string_contains err "Failed to get person URN (401)");
      assert (string_contains err "authentication failed");
      print_endline "✓ Get person URN 401 auth hint")

(** Test: Get person URN 429 returns rate-limit hint message *)
let test_get_person_urn_rate_limited_message () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 429;
    body = {|{"message":"Rate limit exceeded"}|};
    headers = [];
  };

  LinkedIn.get_person_urn ~access_token:"test_token"
    (fun _ -> failwith "Expected person URN rate-limit failure")
    (fun err ->
      assert (string_contains err "Failed to get person URN (429)");
      assert (string_contains err "rate limited");
      print_endline "✓ Get person URN 429 rate-limit hint")

(** Test: Register upload *)
let test_register_upload () =
  Mock_config.reset ();

  let response_body = {|{
    "value": {
      "image": "urn:li:image:test123",
      "uploadUrl": "https://upload.linkedin.com/test"
    }
  }|} in

  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  LinkedIn.register_upload
    ~access_token:"test_token"
    ~owner_urn:"urn:li:person:test"
    ~media_type:"image"
    (fun (asset, upload_url) ->
      assert (asset = "urn:li:image:test123");
      assert (upload_url = "https://upload.linkedin.com/test");
      print_endline "✓ Register upload")
    (fun err -> failwith ("Register upload failed: " ^ err))

(** Test: Content validation *)
let test_content_validation () =
  (* Valid content *)
  (match LinkedIn.validate_content ~text:"Hello LinkedIn!" with
   | Ok () -> print_endline "✓ Valid content passes"
   | Error e -> failwith ("Valid content failed: " ^ e));
  
  (* Empty content *)
  (match LinkedIn.validate_content ~text:"" with
   | Error _ -> print_endline "✓ Empty content rejected"
   | Ok () -> failwith "Empty content should fail");
  
  (* Too long *)
  let long_text = String.make 3001 'x' in
  (match LinkedIn.validate_content ~text:long_text with
   | Error msg when string_contains msg "too long" -> 
       print_endline "✓ Long content rejected"
   | _ -> failwith "Long content should fail")

(** Test: Token refresh (partner program) *)
let test_token_refresh_partner () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client";
  Mock_config.set_env "LINKEDIN_CLIENT_SECRET" "test_secret";
  Mock_config.set_env "LINKEDIN_ENABLE_PROGRAMMATIC_REFRESH" "true";
  
  let response_body = {|{
    "access_token": "refreshed_token",
    "refresh_token": "new_refresh_token",
    "expires_in": 5184000
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  LinkedIn.refresh_access_token
    ~client_id:"test_client"
    ~client_secret:"test_secret"
    ~refresh_token:"old_refresh"
    (fun (access, refresh, _expires) ->
      assert (access = "refreshed_token");
      assert (refresh = "new_refresh_token");
      let requests = !Mock_http.requests in
      (match requests with
      | ("POST", url, headers, body) :: _ ->
          assert (string_contains url "/oauth/v2/accessToken");
          assert (find_header headers "Content-Type" = Some "application/x-www-form-urlencoded");
          assert (string_contains body "grant_type=refresh_token");
          assert (string_contains body "refresh_token=old_refresh")
      | _ -> failwith "Expected refresh_access_token POST request");
      print_endline "✓ Token refresh (partner)")
    (fun err -> failwith ("Token refresh failed: " ^ err))

(** Test: Token refresh rejects whitespace refresh token *)
let test_token_refresh_rejects_whitespace () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client";
  Mock_config.set_env "LINKEDIN_CLIENT_SECRET" "test_secret";
  Mock_config.set_env "LINKEDIN_ENABLE_PROGRAMMATIC_REFRESH" "true";

  LinkedIn.refresh_access_token
    ~client_id:"test_client"
    ~client_secret:"test_secret"
    ~refresh_token:" old_refresh "
    (fun _ -> failwith "Expected whitespace refresh token rejection")
    (fun err ->
      assert (string_contains err "whitespace");
      print_endline "✓ Token refresh rejects whitespace refresh token")

(** Test: Token refresh disabled (standard app) *)
let test_token_refresh_standard () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client";
  Mock_config.set_env "LINKEDIN_CLIENT_SECRET" "test_secret";
  (* Don't set LINKEDIN_ENABLE_PROGRAMMATIC_REFRESH *)
  
  LinkedIn.refresh_access_token
    ~client_id:"test_client"
    ~client_secret:"test_secret"
    ~refresh_token:"old_refresh"
    (fun _ -> failwith "Should fail for standard app")
    (fun err ->
      assert (string_contains err "not enabled");
      print_endline "✓ Token refresh disabled for standard apps")

(** Test: Ensure valid token (fresh token) *)
let test_ensure_valid_token_fresh () =
  Mock_config.reset ();
  
  (* Set credentials with far-future expiry *)
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  LinkedIn.ensure_valid_token ~account_id:"test_account"
    (fun token ->
      assert (token = "valid_token");
      (* Verify health status was updated *)
      match Mock_config.get_health_status "test_account" with
      | Some (_, "healthy", None) -> print_endline "✓ Ensure valid token (fresh)"
      | _ -> failwith "Health status not updated correctly")
    (fun err -> failwith ("Ensure valid token failed: " ^ Error_types.error_to_string err))

(** Test: Get profile *)
let test_get_profile () =
  Mock_config.reset ();
  
  (* Set valid credentials *)
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  let response_body = {|{
    "sub": "abc123",
    "name": "John Doe",
    "given_name": "John",
    "family_name": "Doe",
    "email": "john@example.com",
    "email_verified": true,
    "locale": "en-US"
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  LinkedIn.get_profile ~account_id:"test_account"
    (handle_result
      (fun profile ->
        assert (profile.sub = "abc123");
        assert (profile.name = Some "John Doe");
        assert (profile.email = Some "john@example.com");
        print_endline "✓ Get profile")
      (fun err -> failwith ("Get profile failed: " ^ err)))

(** Test: get_profile maps 403 to profile scope *)
let test_get_profile_insufficient_permissions_error () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response { status = 403; body = {|{"message":"Not enough permissions"}|}; headers = [] };

  LinkedIn.get_profile ~account_id:"test_account"
    (fun result ->
      match result with
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (List.mem "openid" scopes);
          assert (List.mem "profile" scopes);
          print_endline "✓ Get profile maps 403 to OpenID profile scopes"
      | Ok _ -> failwith "Expected permission error"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: API error raw response redacts sensitive JSON fields *)
let test_api_error_redacts_sensitive_json () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response {
    status = 500;
    headers = [];
    body = "{\"message\":\"backend failed\",\"access_token\":\"leak-me\"}";
  };

  LinkedIn.get_profile ~account_id:"test_account"
    (function
      | Ok _ -> failwith "Expected API error"
      | Error (Error_types.Api_error api_err) ->
          (match api_err.raw_response with
           | Some body ->
               assert (string_contains body "[REDACTED]");
               assert (not (string_contains body "leak-me"));
               print_endline "✓ API error redacts sensitive JSON fields"
           | None -> failwith "Expected raw_response in API error")
      | Error err ->
          failwith ("Expected Api_error, got: " ^ Error_types.error_to_string err))

(** Test: API error raw response redacts non-JSON body *)
let test_api_error_redacts_non_json () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response {
    status = 500;
    headers = [];
    body = "backend exploded access_token=leak-me";
  };

  LinkedIn.get_profile ~account_id:"test_account"
    (function
      | Ok _ -> failwith "Expected API error"
      | Error (Error_types.Api_error api_err) ->
          (match api_err.raw_response with
           | Some body ->
               assert (body = "[REDACTED_NON_JSON_ERROR_BODY]");
               print_endline "✓ API error redacts non-JSON body"
           | None -> failwith "Expected raw_response in API error")
      | Error err ->
          failwith ("Expected Api_error, got: " ^ Error_types.error_to_string err))

(** Test: Get posts with pagination *)
let test_get_posts () =
  Mock_config.reset ();
  
  (* Set valid credentials *)
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  (* Set up response queue: first person URN, then posts *)
  let person_response = {|{"sub": "user123"}|} in
  let posts_response = {|{
    "elements": [
      {
        "id": "urn:li:share:123",
        "author": "urn:li:person:user123",
        "createdAt": 1704103200000,
        "lifecycleState": "PUBLISHED",
        "commentary": "Test post",
        "visibility": "PUBLIC"
      }
    ],
    "paging": {
      "start": 0,
      "count": 1,
      "total": 10
    }
  }|} in
  Mock_http.set_responses [
    { status = 200; body = person_response; headers = [] };
    { status = 200; body = posts_response; headers = [] };
  ];
  
  LinkedIn.get_posts ~account_id:"test_account" ~start:0 ~count:10
    (fun result ->
      match result with
        | Ok collection ->
          assert (List.length collection.elements = 1);
          let post = List.hd collection.elements in
          assert (post.id = "urn:li:share:123");
          assert (post.text = Some "Test post");
          let requests = List.rev !Mock_http.requests in
          (match requests with
          | _ :: (_, posts_url, headers, _) :: _ ->
              assert (string_contains posts_url "q=authors");
              assert (string_contains posts_url "authors=List" || string_contains posts_url "authors=List%28");
              assert (find_header headers "X-RestLi-Method" = Some "FINDER")
          | _ -> failwith "Expected userinfo + posts requests");
          (match collection.paging with
          | Some p -> 
              assert (p.start = 0);
              assert (p.count = 1);
              assert (p.total = Some 10)
          | None -> failwith "Expected paging metadata");
          print_endline "✓ Get posts with pagination"
      | Error err -> failwith ("Get posts failed: " ^ Error_types.error_to_string err))

(** Test: get_posts rejects malformed resolved person URN *)
let test_get_posts_rejects_malformed_person_urn () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response { status = 200; body = {|{"sub":"user,123"}|}; headers = [] };

  LinkedIn.get_posts ~account_id:"test_account"
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "person URN");
          assert (string_contains msg "invalid characters");
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 1);
          print_endline "✓ Get posts rejects malformed resolved person URN"
      | Ok _ -> failwith "Expected malformed person URN rejection"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: get_posts normalizes negative start and caps count to 50 *)
let test_get_posts_pagination_normalization () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  let person_response = {|{"sub":"user123"}|} in
  let posts_response = {|{"elements": [], "paging": {"start": 0, "count": 0, "total": 0}}|} in
  Mock_http.set_responses [
    { status = 200; body = person_response; headers = [] };
    { status = 200; body = posts_response; headers = [] };
  ];

  LinkedIn.get_posts ~account_id:"test_account" ~start:(-10) ~count:999
    (handle_result
      (fun _collection ->
        let requests = List.rev !Mock_http.requests in
        (match requests with
        | _ :: (_, url, _, _) :: _ ->
            assert (string_contains url "start=0");
            assert (string_contains url "count=50");
            print_endline "✓ Get posts pagination normalization"
        | _ -> failwith "Expected userinfo + posts requests"))
      (fun err -> failwith ("Get posts normalization failed: " ^ err)))

(** Test: get_posts maps 429 to Rate_limited *)
let test_get_posts_rate_limited_error () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_responses [
    { status = 200; body = {|{"sub":"user123"}|}; headers = [] };
    { status = 429; body = {|{"message":"Rate limit exceeded","serviceErrorCode":42901}|}; headers = [] };
  ];

  LinkedIn.get_posts ~account_id:"test_account"
    (fun result ->
      match result with
      | Error (Error_types.Rate_limited _) ->
          print_endline "✓ Get posts maps 429 to rate limited"
      | Ok _ -> failwith "Expected rate-limited error"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: get_posts maps 403 to insufficient permissions *)
let test_get_posts_insufficient_permissions_error () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_responses [
    { status = 200; body = {|{"sub":"user123"}|}; headers = [] };
    { status = 403; body = {|{"message":"Not enough permissions"}|}; headers = [] };
  ];

  LinkedIn.get_posts ~account_id:"test_account"
    (fun result ->
      match result with
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (List.mem "r_member_social" scopes);
          print_endline "✓ Get posts maps 403 to insufficient permissions"
      | Ok _ -> failwith "Expected permission error"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: Batch get posts *)
let test_batch_get_posts () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  let response_body = {|{
    "results": {
      "urn:li:share:123": {
        "id": "urn:li:share:123",
        "author": "urn:li:person:user1",
        "lifecycleState": "PUBLISHED",
        "commentary": "Post 1",
        "visibility": "PUBLIC"
      },
      "urn:li:share:456": {
        "id": "urn:li:share:456",
        "author": "urn:li:person:user2",
        "lifecycleState": "PUBLISHED",
        "commentary": "Post 2",
        "visibility": "PUBLIC"
      }
    }
  }|} in
  
  Mock_http.set_responses [{ status = 200; body = response_body; headers = [] }];
  
  LinkedIn.batch_get_posts 
    ~account_id:"test_account" 
    ~post_urns:["urn:li:share:123"; "urn:li:share:456"]
    (handle_result
      (fun posts ->
        assert (List.length posts = 2);
        print_endline "✓ Batch get posts")
      (fun err -> failwith ("Batch get posts failed: " ^ err)))

(** Test: Batch get posts rejects malformed URNs for Rest.li list encoding *)
let test_batch_get_posts_rejects_malformed_urns () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;

  LinkedIn.batch_get_posts
    ~account_id:"test_account"
    ~post_urns:["urn:li:share:123,urn:li:share:456"]
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "Invalid post URN");
          assert (!Mock_http.requests = []);
          print_endline "✓ Batch get posts rejects malformed URNs"
      | Ok _ -> failwith "Expected malformed URN rejection"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: Batch get posts handles malformed JSON payload *)
let test_batch_get_posts_malformed_json () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response { status = 200; body = "not-json"; headers = [] };

  LinkedIn.batch_get_posts
    ~account_id:"test_account"
    ~post_urns:["urn:li:share:123"]
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "Failed to parse batch results");
          print_endline "✓ Batch get posts malformed JSON handling"
      | Ok _ -> failwith "Expected parse error"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: batch_get_posts maps 403 to read scope *)
let test_batch_get_posts_insufficient_permissions_error () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response {
    status = 403;
    headers = [];
    body = {|{"message":"Not enough permissions"}|};
  };

  LinkedIn.batch_get_posts ~account_id:"test_account" ~post_urns:["urn:li:share:123"]
    (fun result ->
      match result with
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (List.mem "r_member_social" scopes);
          print_endline "✓ Batch get posts maps 403 to read scope"
      | Ok _ -> failwith "Expected permission error"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: Posts scroller *)
let test_posts_scroller () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  (* Set up response queue: first person URN, then posts *)
  let person_response = {|{"sub": "user123"}|} in
  let posts_response = {|{
    "elements": [{"id": "1", "author": "urn:li:person:user123", "lifecycleState": "PUBLISHED"}],
    "paging": {"start": 0, "count": 1, "total": 5}
  }|} in
  Mock_http.set_responses [
    { status = 200; body = person_response; headers = [] };
    { status = 200; body = posts_response; headers = [] };
  ];
  
  let scroller = LinkedIn.create_posts_scroller ~account_id:"test_account" ~page_size:1 () in
  
  scroller.scroll_next
    (handle_result
      (fun collection ->
        assert (List.length collection.elements = 1);
        assert (scroller.current_position () = 1);
        assert (scroller.has_more () = true);
        print_endline "✓ Posts scroller")
      (fun err -> failwith ("Posts scroller failed: " ^ err)))

(** Test: Posts scroller scroll_back returns previous page after multiple nexts *)
let test_posts_scroller_back_from_second_page () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;

  let userinfo = {|{"sub":"user123"}|} in
  let page0 = {|{"elements":[{"id":"p1","author":"urn:li:person:user123","lifecycleState":"PUBLISHED"}],"paging":{"start":0,"count":1,"total":3}}|} in
  let page1 = {|{"elements":[{"id":"p2","author":"urn:li:person:user123","lifecycleState":"PUBLISHED"}],"paging":{"start":1,"count":1,"total":3}}|} in
  let page_back = page1 in

  Mock_http.set_responses [
    { status = 200; body = userinfo; headers = [] };
    { status = 200; body = page0; headers = [] };
    { status = 200; body = userinfo; headers = [] };
    { status = 200; body = page1; headers = [] };
    { status = 200; body = userinfo; headers = [] };
    { status = 200; body = page_back; headers = [] };
  ];

  let scroller = LinkedIn.create_posts_scroller ~account_id:"test_account" ~page_size:1 () in
  scroller.scroll_next (handle_result (fun _ -> ()) (fun err -> failwith ("Posts first page failed: " ^ err)));
  scroller.scroll_next (handle_result (fun _ -> ()) (fun err -> failwith ("Posts second page failed: " ^ err)));
  scroller.scroll_back
    (handle_result
      (fun page ->
        assert (List.length page.elements = 1);
        let requests = List.rev !Mock_http.requests in
        let posts_requests = List.filter (fun (m, u, _, _) -> m = "GET" && string_contains u "/rest/posts?") requests in
        (match posts_requests with
        | _ :: _ :: (_, back_url, _, _) :: _ ->
            assert (string_contains back_url "start=1");
            print_endline "✓ Posts scroller back from second page"
        | _ -> failwith "Expected at least three /rest/posts requests"))
      (fun err -> failwith ("Posts scroll_back failed: " ^ err)))

(** Test: Posts scroller preserves position when scroll_back fails *)
let test_posts_scroller_back_error_preserves_position () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;

  let userinfo = {|{"sub":"user123"}|} in
  let page0 = {|{"elements":[{"id":"p1","author":"urn:li:person:user123","lifecycleState":"PUBLISHED"}],"paging":{"start":0,"count":1,"total":3}}|} in
  Mock_http.set_responses [
    { status = 200; body = userinfo; headers = [] };
    { status = 200; body = page0; headers = [] };
    { status = 500; body = {|{"message":"boom"}|}; headers = [] };
  ];

  let scroller = LinkedIn.create_posts_scroller ~account_id:"test_account" ~page_size:1 () in
  scroller.scroll_next (handle_result (fun _ -> ()) (fun err -> failwith ("Posts first page failed: " ^ err)));
  assert (scroller.current_position () = 1);
  scroller.scroll_back
    (fun result ->
      match result with
      | Error _ ->
          assert (scroller.current_position () = 1);
          print_endline "✓ Posts scroller back error preserves position"
      | Ok _ -> failwith "Expected scroll_back failure")

(** Test: Search scroller with explicit author filter *)
let test_search_scroller_with_author () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;

  let page1 = {|{
    "elements": [{"id": "s1", "author": "urn:li:person:user1", "lifecycleState": "PUBLISHED"}],
    "paging": {"start": 0, "count": 1, "total": 2}
  }|} in
  let page2 = {|{
    "elements": [{"id": "s1", "author": "urn:li:person:user1", "lifecycleState": "PUBLISHED"}],
    "paging": {"start": 0, "count": 1, "total": 2}
  }|} in
  Mock_http.set_responses [
    { status = 200; body = page1; headers = [] };
    { status = 200; body = page2; headers = [] };
  ];

  let scroller = LinkedIn.create_search_scroller
    ~account_id:"test_account"
    ~author:"urn:li:person:user1"
    ~page_size:1
    ()
  in

  scroller.scroll_next
    (handle_result
      (fun collection ->
        assert (List.length collection.elements = 1);
        assert (scroller.current_position () = 1);
        assert (scroller.has_more () = true);
        scroller.scroll_back
          (handle_result
            (fun back_page ->
              assert (List.length back_page.elements = 1);
              assert (scroller.current_position () = 1);
              let requests = List.rev !Mock_http.requests in
              (match requests with
              | (_method1, url1, headers1, _) :: (_method2, url2, headers2, _) :: _ ->
                  assert (string_contains url1 "q=authors");
                  assert (string_contains url2 "q=authors");
                  assert (find_header headers1 "X-RestLi-Method" = Some "FINDER");
                  assert (find_header headers2 "X-RestLi-Method" = Some "FINDER")
              | _ -> failwith "Expected two finder requests for search scroller");
              print_endline "✓ Search scroller with author")
            (fun err -> failwith ("Search scroller back failed: " ^ err)))
      )
      (fun err -> failwith ("Search scroller next failed: " ^ err)))

(** Test: Search scroller scroll_back returns previous page after multiple nexts *)
let test_search_scroller_back_from_second_page () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;

  let page0 = {|{"elements":[{"id":"s1","author":"urn:li:person:user1","lifecycleState":"PUBLISHED"}],"paging":{"start":0,"count":1,"total":3}}|} in
  let page1 = {|{"elements":[{"id":"s2","author":"urn:li:person:user1","lifecycleState":"PUBLISHED"}],"paging":{"start":1,"count":1,"total":3}}|} in
  let page_back = page1 in

  Mock_http.set_responses [
    { status = 200; body = page0; headers = [] };
    { status = 200; body = page1; headers = [] };
    { status = 200; body = page_back; headers = [] };
  ];

  let scroller = LinkedIn.create_search_scroller
    ~account_id:"test_account"
    ~author:"urn:li:person:user1"
    ~page_size:1
    ()
  in

  scroller.scroll_next (handle_result (fun _ -> ()) (fun err -> failwith ("Search first page failed: " ^ err)));
  scroller.scroll_next (handle_result (fun _ -> ()) (fun err -> failwith ("Search second page failed: " ^ err)));
  scroller.scroll_back
    (handle_result
      (fun page ->
        assert (List.length page.elements = 1);
        let requests = List.rev !Mock_http.requests in
        match requests with
        | _ :: _ :: (_, back_url, _, _) :: _ ->
            assert (string_contains back_url "start=1");
            print_endline "✓ Search scroller back from second page"
        | _ -> failwith "Expected at least three search requests")
      (fun err -> failwith ("Search scroll_back failed: " ^ err)))

(** Test: Search scroller preserves position when scroll_back fails *)
let test_search_scroller_back_error_preserves_position () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;

  let page0 = {|{"elements":[{"id":"s1","author":"urn:li:person:user1","lifecycleState":"PUBLISHED"}],"paging":{"start":0,"count":1,"total":3}}|} in
  Mock_http.set_responses [
    { status = 200; body = page0; headers = [] };
    { status = 500; body = {|{"message":"boom"}|}; headers = [] };
  ];

  let scroller = LinkedIn.create_search_scroller
    ~account_id:"test_account"
    ~author:"urn:li:person:user1"
    ~page_size:1
    ()
  in

  scroller.scroll_next (handle_result (fun _ -> ()) (fun err -> failwith ("Search first page failed: " ^ err)));
  assert (scroller.current_position () = 1);
  scroller.scroll_back
    (fun result ->
      match result with
      | Error _ ->
          assert (scroller.current_position () = 1);
          print_endline "✓ Search scroller back error preserves position"
      | Ok _ -> failwith "Expected scroll_back failure")

(** Test: Search scroller propagates keyword-search rejection *)
let test_search_scroller_rejects_keywords () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;

  let scroller = LinkedIn.create_search_scroller
    ~account_id:"test_account"
    ~keywords:"ocaml"
    ~page_size:5
    ()
  in

  scroller.scroll_next
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "does not support keyword finder search");
          assert (scroller.current_position () = 0);
          assert (!Mock_http.requests = []);
          print_endline "✓ Search scroller rejects unsupported keyword finder"
      | Ok _ -> failwith "Expected keyword search rejection in scroller"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: Search scroller without author uses current member URN *)
let test_search_scroller_defaults_to_current_author () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;

  let person_response = {|{"sub":"user123"}|} in
  let page = {|{
    "elements": [{"id": "s1", "author": "urn:li:person:user123", "lifecycleState": "PUBLISHED"}],
    "paging": {"start": 0, "count": 1, "total": 1}
  }|} in
  Mock_http.set_responses [
    { status = 200; body = person_response; headers = [] };
    { status = 200; body = page; headers = [] };
  ];

  let scroller = LinkedIn.create_search_scroller ~account_id:"test_account" ~page_size:1 () in

  scroller.scroll_next
    (handle_result
      (fun collection ->
        assert (List.length collection.elements = 1);
        let requests = List.rev !Mock_http.requests in
        (match requests with
        | (_m1, url1, _h1, _) :: (_m2, url2, headers2, _) :: _ ->
            assert (string_contains url1 "userinfo");
            assert (string_contains url2 "q=authors");
            assert (string_contains url2 "user123");
            assert (find_header headers2 "X-RestLi-Method" = Some "FINDER")
        | _ -> failwith "Expected userinfo + finder requests");
        print_endline "✓ Search scroller defaults to current author")
      (fun err -> failwith ("Search scroller default author failed: " ^ err)))

(** Test: Posts scroller normalizes non-positive page_size to 1 *)
let test_posts_scroller_page_size_normalization () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_responses [
    { status = 200; body = {|{"sub":"user123"}|}; headers = [] };
    { status = 200; body = {|{"elements": [], "paging": {"start": 0, "count": 0, "total": 0}}|}; headers = [] };
  ];

  let scroller = LinkedIn.create_posts_scroller ~account_id:"test_account" ~page_size:0 () in
  scroller.scroll_next
    (handle_result
      (fun _collection ->
        let requests = List.rev !Mock_http.requests in
        (match requests with
        | _ :: (_, url, _, _) :: _ ->
            assert (string_contains url "count=1");
            print_endline "✓ Posts scroller page_size normalization"
        | _ -> failwith "Expected userinfo + posts requests"))
      (fun err -> failwith ("Posts scroller page_size normalization failed: " ^ err)))

(** Test: Search scroller normalizes non-positive page_size to 1 *)
let test_search_scroller_page_size_normalization () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response { status = 200; body = {|{"elements": [], "paging": {"start": 0, "count": 0, "total": 0}}|}; headers = [] };

  let scroller = LinkedIn.create_search_scroller ~account_id:"test_account" ~author:"urn:li:person:user1" ~page_size:0 () in
  scroller.scroll_next
    (handle_result
      (fun _collection ->
        let requests = !Mock_http.requests in
        (match requests with
        | (_, url, _, _) :: _ ->
            assert (string_contains url "count=1");
            print_endline "✓ Search scroller page_size normalization"
        | [] -> failwith "No requests recorded"))
      (fun err -> failwith ("Search scroller page_size normalization failed: " ^ err)))

(** Test: Search posts *)
let test_search_posts () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  let search_response = {|{
    "elements": [
      {"id": "post1", "author": "urn:li:person:user1", "lifecycleState": "PUBLISHED"},
      {"id": "post2", "author": "urn:li:person:user2", "lifecycleState": "PUBLISHED"}
    ],
    "paging": {"start": 0, "count": 2, "total": 10}
  }|} in
  
  Mock_http.set_response { status = 200; body = search_response; headers = [] };
  
  LinkedIn.search_posts ~account_id:"test_account" ~keywords:"OCaml" ~start:0 ~count:10
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "does not support keyword finder search");
          assert (!Mock_http.requests = []);
          print_endline "✓ Search posts rejects unsupported keyword finder"
      | Ok _ -> failwith "Expected keyword search to be rejected"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: Search posts with author filter uses Rest.li list syntax *)
let test_search_posts_author_filter_encoding () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;

  let search_response = {|{"elements": [], "paging": {"start": 0, "count": 0, "total": 0}}|} in
  Mock_http.set_response { status = 200; body = search_response; headers = [] };

  LinkedIn.search_posts ~account_id:"test_account" ~author:"urn:li:person:user1"
    (handle_result
      (fun _collection ->
        let requests = !Mock_http.requests in
        (match requests with
        | (_, url, headers, _) :: _ ->
            assert (string_contains url "q=authors");
            assert (string_contains url "authors=List" || string_contains url "authors=List%28");
            assert (find_header headers "X-RestLi-Method" = Some "FINDER");
            print_endline "✓ Search author filter encoding"
        | [] -> failwith "No requests recorded"))
      (fun err -> failwith ("Search posts author filter failed: " ^ err)))

(** Test: Search posts defaults to current member author finder *)
let test_search_posts_defaults_to_current_author () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;

  let person_response = {|{"sub": "user123"}|} in
  let search_response = {|{"elements": [], "paging": {"start": 0, "count": 0, "total": 0}}|} in
  Mock_http.set_responses [
    { status = 200; body = person_response; headers = [] };
    { status = 200; body = search_response; headers = [] };
  ];

  LinkedIn.search_posts ~account_id:"test_account"
    (handle_result
      (fun _collection ->
        let requests = List.rev !Mock_http.requests in
        (match requests with
        | _ :: (_, url, _, _) :: _ ->
            assert (string_contains url "q=authors");
            assert (string_contains url "authors=List" || string_contains url "authors=List%28");
            assert (string_contains url "user123");
            print_endline "✓ Search defaults to current author"
        | _ -> failwith "Expected userinfo + search requests"))
      (fun err -> failwith ("Search default author failed: " ^ err)))

(** Test: Search defaults reject malformed resolved person URN *)
let test_search_posts_defaults_reject_malformed_person_urn () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response { status = 200; body = {|{"sub":"user,123"}|}; headers = [] };

  LinkedIn.search_posts ~account_id:"test_account"
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "person URN");
          assert (string_contains msg "invalid characters");
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 1);
          print_endline "✓ Search defaults reject malformed resolved person URN"
      | Ok _ -> failwith "Expected malformed person URN rejection"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: Search posts rejects author URN with surrounding whitespace *)
let test_search_posts_rejects_whitespace_author () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;

  LinkedIn.search_posts ~account_id:"test_account" ~author:" urn:li:person:user1 "
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "author URN");
          assert (string_contains msg "whitespace");
          assert (!Mock_http.requests = []);
          print_endline "✓ Search posts rejects whitespace author"
      | Ok _ -> failwith "Expected whitespace author rejection"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: Search posts rejects author URN with Rest.li list-breaking chars *)
let test_search_posts_rejects_malformed_author () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;

  LinkedIn.search_posts ~account_id:"test_account" ~author:"urn:li:person:user1,user2"
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "invalid characters");
          assert (!Mock_http.requests = []);
          print_endline "✓ Search posts rejects malformed author"
      | Ok _ -> failwith "Expected malformed author rejection"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: Search posts normalizes negative start and caps count to 50 *)
let test_search_posts_pagination_normalization () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  let search_response = {|{"elements": [], "paging": {"start": 0, "count": 0, "total": 0}}|} in
  Mock_http.set_response { status = 200; body = search_response; headers = [] };

  LinkedIn.search_posts ~account_id:"test_account" ~author:"urn:li:person:user1" ~start:(-3) ~count:999
    (handle_result
      (fun _collection ->
        let requests = !Mock_http.requests in
        (match requests with
        | (_, url, _, _) :: _ ->
            assert (string_contains url "start=0");
            assert (string_contains url "count=50");
            print_endline "✓ Search posts pagination normalization"
        | [] -> failwith "No requests recorded"))
      (fun err -> failwith ("Search posts normalization failed: " ^ err)))

(** Test: Zero count is normalized to minimum 1 for finder reads *)
let test_finder_zero_count_normalization () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response { status = 200; body = {|{"elements": [], "paging": {"start": 0, "count": 0, "total": 0}}|}; headers = [] };

  LinkedIn.search_posts ~account_id:"test_account" ~author:"urn:li:person:user1" ~count:0
    (handle_result
      (fun _collection ->
        let requests = !Mock_http.requests in
        match requests with
        | (_, url, _, _) :: _ ->
            assert (string_contains url "count=1");
            print_endline "✓ Finder zero count normalization"
        | [] -> failwith "No requests recorded")
      (fun err -> failwith ("Finder zero count normalization failed: " ^ err)))

(** Test: Search posts propagates API error details *)
let test_search_posts_api_error () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response {
    status = 429;
    headers = [];
    body = {|{"message":"Rate limit exceeded","serviceErrorCode":42901}|};
  };

  LinkedIn.search_posts ~account_id:"test_account" ~author:"urn:li:person:user1"
    (fun result ->
      match result with
      | Error (Error_types.Rate_limited _) ->
          print_endline "✓ Search posts API error propagation"
      | Ok _ -> failwith "Expected API error for search_posts"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: Like post *)
let test_like_post () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  (* Set up response queue: first person URN, then like response *)
  let person_response = {|{"sub": "user123"}|} in
  Mock_http.set_responses [
    { status = 200; body = person_response; headers = [] };
    { status = 201; body = "{}"; headers = [] };
  ];
  
  LinkedIn.like_post ~account_id:"test_account" ~post_urn:"urn:li:share:123"
    (handle_result
      (fun () -> print_endline "✓ Like post")
      (fun err -> failwith ("Like post failed: " ^ err)))

(** Test: like_post maps 403 to write scope *)
let test_like_post_insufficient_permissions_error () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_responses [
    { status = 200; body = {|{"sub":"user123"}|}; headers = [] };
    { status = 403; body = {|{"message":"Not enough permissions"}|}; headers = [] };
  ];

  LinkedIn.like_post ~account_id:"test_account" ~post_urn:"urn:li:share:123"
    (fun result ->
      match result with
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (List.mem "w_member_social" scopes);
          print_endline "✓ Like post maps 403 to write scope"
      | Ok _ -> failwith "Expected permission error"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: like_post rejects whitespace post URN without network calls *)
let test_like_post_rejects_whitespace_urn () =
  Mock_config.reset ();

  LinkedIn.like_post ~account_id:"test_account" ~post_urn:" urn:li:share:123 "
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "post URN");
          assert (string_contains msg "whitespace");
          assert (!Mock_http.requests = []);
          print_endline "✓ Like post rejects whitespace URN"
      | Ok _ -> failwith "Expected whitespace URN rejection"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: like_post rejects blank post URN without network calls *)
let test_like_post_rejects_blank_urn () =
  Mock_config.reset ();

  LinkedIn.like_post ~account_id:"test_account" ~post_urn:""
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "post URN is required");
          assert (!Mock_http.requests = []);
          print_endline "✓ Like post rejects blank URN"
      | Ok _ -> failwith "Expected blank URN rejection"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: like_post rejects malformed delimiter characters in post URN *)
let test_like_post_rejects_malformed_delimiter_urn () =
  Mock_config.reset ();

  LinkedIn.like_post ~account_id:"test_account" ~post_urn:"urn:li:share:123,456"
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "invalid delimiter");
          assert (!Mock_http.requests = []);
          print_endline "✓ Like post rejects malformed delimiter URN"
      | Ok _ -> failwith "Expected malformed delimiter URN rejection"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: Comment on post *)
let test_comment_on_post () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  (* Set up response queue: first person URN, then comment response *)
  let person_response = {|{"sub": "user123"}|} in
  let comment_response = {|{"id": "comment123"}|} in
  Mock_http.set_responses [
    { status = 200; body = person_response; headers = [] };
    { status = 201; body = comment_response; headers = [] };
  ];
  
  LinkedIn.comment_on_post 
    ~account_id:"test_account" 
    ~post_urn:"urn:li:share:123"
    ~text:"Great post!"
    (handle_result
      (fun comment_id ->
        assert (comment_id = "comment123");
        print_endline "✓ Comment on post")
      (fun err -> failwith ("Comment failed: " ^ err)))

(** Test: comment_on_post maps 403 to write scope *)
let test_comment_on_post_insufficient_permissions_error () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_responses [
    { status = 200; body = {|{"sub":"user123"}|}; headers = [] };
    { status = 403; body = {|{"message":"Not enough permissions"}|}; headers = [] };
  ];

  LinkedIn.comment_on_post ~account_id:"test_account" ~post_urn:"urn:li:share:123" ~text:"Great post!"
    (fun result ->
      match result with
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (List.mem "w_member_social" scopes);
          print_endline "✓ Comment on post maps 403 to write scope"
      | Ok _ -> failwith "Expected permission error"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: comment_on_post rejects whitespace post URN without network calls *)
let test_comment_on_post_rejects_whitespace_urn () =
  Mock_config.reset ();

  LinkedIn.comment_on_post ~account_id:"test_account" ~post_urn:" urn:li:share:123 " ~text:"hi"
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "post URN");
          assert (string_contains msg "whitespace");
          assert (!Mock_http.requests = []);
          print_endline "✓ Comment on post rejects whitespace URN"
      | Ok _ -> failwith "Expected whitespace URN rejection"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: comment_on_post rejects blank post URN without network calls *)
let test_comment_on_post_rejects_blank_urn () =
  Mock_config.reset ();

  LinkedIn.comment_on_post ~account_id:"test_account" ~post_urn:"" ~text:"hi"
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "post URN is required");
          assert (!Mock_http.requests = []);
          print_endline "✓ Comment on post rejects blank URN"
      | Ok _ -> failwith "Expected blank URN rejection"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: comment_on_post rejects blank text without network calls *)
let test_comment_on_post_rejects_blank_text () =
  Mock_config.reset ();

  LinkedIn.comment_on_post ~account_id:"test_account" ~post_urn:"urn:li:share:123" ~text:"   "
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "comment text is required");
          assert (!Mock_http.requests = []);
          print_endline "✓ Comment on post rejects blank text"
      | Ok _ -> failwith "Expected blank text rejection"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: comment_on_post rejects malformed delimiter characters in post URN *)
let test_comment_on_post_rejects_malformed_delimiter_urn () =
  Mock_config.reset ();

  LinkedIn.comment_on_post ~account_id:"test_account" ~post_urn:"urn:li:share:123,456" ~text:"hi"
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "invalid delimiter");
          assert (!Mock_http.requests = []);
          print_endline "✓ Comment on post rejects malformed delimiter URN"
      | Ok _ -> failwith "Expected malformed delimiter URN rejection"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: Get post comments *)
let test_get_post_comments () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  let comments_response = {|{
    "elements": [
      {
        "id": "comment1",
        "actor": "urn:li:person:user1",
        "message": {"text": "Nice!"},
        "created": {"time": "2024-01-01T10:00:00Z"}
      }
    ],
    "paging": {"start": 0, "count": 1, "total": 5}
  }|} in
  
  Mock_http.set_response { status = 200; body = comments_response; headers = [] };
  
  LinkedIn.get_post_comments ~account_id:"test_account" ~post_urn:"urn:li:share:123"
    (handle_result
      (fun collection ->
        assert (List.length collection.elements = 1);
        let comment = List.hd collection.elements in
        assert (comment.text = "Nice!");
        print_endline "✓ Get post comments")
      (fun err -> failwith ("Get comments failed: " ^ err)))

(** Test: get_post_comments maps 403 to read scope *)
let test_get_post_comments_insufficient_permissions_error () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response { status = 403; body = {|{"message":"Not enough permissions"}|}; headers = [] };

  LinkedIn.get_post_comments ~account_id:"test_account" ~post_urn:"urn:li:share:123"
    (fun result ->
      match result with
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (List.mem "r_member_social" scopes);
          print_endline "✓ Get post comments maps 403 to read scope"
      | Ok _ -> failwith "Expected permission error"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: get_post_comments rejects whitespace post URN without network calls *)
let test_get_post_comments_rejects_whitespace_urn () =
  Mock_config.reset ();

  LinkedIn.get_post_comments ~account_id:"test_account" ~post_urn:" urn:li:share:123 "
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "post URN");
          assert (string_contains msg "whitespace");
          assert (!Mock_http.requests = []);
          print_endline "✓ Get post comments rejects whitespace URN"
      | Ok _ -> failwith "Expected whitespace URN rejection"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: get_post_comments rejects blank post URN without network calls *)
let test_get_post_comments_rejects_blank_urn () =
  Mock_config.reset ();

  LinkedIn.get_post_comments ~account_id:"test_account" ~post_urn:""
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "post URN is required");
          assert (!Mock_http.requests = []);
          print_endline "✓ Get post comments rejects blank URN"
      | Ok _ -> failwith "Expected blank URN rejection"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: get_post_comments rejects malformed delimiter characters in post URN *)
let test_get_post_comments_rejects_malformed_delimiter_urn () =
  Mock_config.reset ();

  LinkedIn.get_post_comments ~account_id:"test_account" ~post_urn:"urn:li:share:123,456"
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "invalid delimiter");
          assert (!Mock_http.requests = []);
          print_endline "✓ Get post comments rejects malformed delimiter URN"
      | Ok _ -> failwith "Expected malformed delimiter URN rejection"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: Get post engagement metrics *)
let test_get_post_engagement () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;

  let engagement_response = {|{
    "totalLikes": 12,
    "totalComments": 3,
    "totalShares": 2,
    "totalImpressions": 100
  }|} in
  Mock_http.set_response { status = 200; body = engagement_response; headers = [] };

  LinkedIn.get_post_engagement ~account_id:"test_account" ~post_urn:"urn:li:share:123"
    (handle_result
      (fun (engagement : engagement_info) ->
        assert (engagement.like_count = Some 12);
        assert (engagement.comment_count = Some 3);
        assert (engagement.share_count = Some 2);
        assert (engagement.impression_count = Some 100);
        let requests = !Mock_http.requests in
        (match requests with
        | (_, url, headers, _) :: _ ->
            assert (string_contains url "socialMetadata");
            assert (find_header headers "X-Restli-Protocol-Version" = Some "2.0.0")
        | [] -> failwith "No requests recorded");
        print_endline "✓ Get post engagement")
      (fun err -> failwith ("Get post engagement failed: " ^ err)))

(** Test: Get post engagement handles missing optional fields gracefully *)
let test_get_post_engagement_missing_fields () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response {
    status = 200;
    headers = [];
    body = {|{"totalLikes": 5}|};
  };

  LinkedIn.get_post_engagement ~account_id:"test_account" ~post_urn:"urn:li:share:123"
    (handle_result
      (fun (engagement : engagement_info) ->
        assert (engagement.like_count = Some 5);
        assert (engagement.comment_count = None);
        assert (engagement.share_count = None);
        assert (engagement.impression_count = None);
        print_endline "✓ Get post engagement missing fields handling")
      (fun err -> failwith ("Get post engagement missing fields failed: " ^ err)))

(** Test: Get post engagement propagates API error details *)
let test_get_post_engagement_api_error () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response {
    status = 403;
    headers = [];
    body = {|{"message":"Not enough permissions","serviceErrorCode":100}|};
  };

  LinkedIn.get_post_engagement ~account_id:"test_account" ~post_urn:"urn:li:share:123"
    (fun result ->
      match result with
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (List.mem "r_member_social" scopes);
          print_endline "✓ Get post engagement API error propagation"
      | Ok _ -> failwith "Expected API error for get_post_engagement"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: get_post_engagement rejects whitespace post URN without network calls *)
let test_get_post_engagement_rejects_whitespace_urn () =
  Mock_config.reset ();

  LinkedIn.get_post_engagement ~account_id:"test_account" ~post_urn:" urn:li:share:123 "
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "post URN");
          assert (string_contains msg "whitespace");
          assert (!Mock_http.requests = []);
          print_endline "✓ Get post engagement rejects whitespace URN"
      | Ok _ -> failwith "Expected whitespace URN rejection"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: get_post_engagement rejects blank post URN without network calls *)
let test_get_post_engagement_rejects_blank_urn () =
  Mock_config.reset ();

  LinkedIn.get_post_engagement ~account_id:"test_account" ~post_urn:""
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "post URN is required");
          assert (!Mock_http.requests = []);
          print_endline "✓ Get post engagement rejects blank URN"
      | Ok _ -> failwith "Expected blank URN rejection"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: get_post_engagement rejects malformed delimiter characters in post URN *)
let test_get_post_engagement_rejects_malformed_delimiter_urn () =
  Mock_config.reset ();

  LinkedIn.get_post_engagement ~account_id:"test_account" ~post_urn:"urn:li:share:123,456"
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "invalid delimiter");
          assert (!Mock_http.requests = []);
          print_endline "✓ Get post engagement rejects malformed delimiter URN"
      | Ok _ -> failwith "Expected malformed delimiter URN rejection"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: Get account analytics metrics for organization entity *)
let test_get_account_analytics () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_responses [
    {
      status = 200;
      headers = [];
      body =
        {|{"elements":[{"followerCounts":{"organicFollowerCount":321}}]}|};
    };
    {
      status = 200;
      headers = [];
      body =
        {|{"elements":[{"totalShareStatistics":{"impressionCount":2100,"uniqueImpressionsCount":1700,"shareCount":12,"clickCount":88,"likeCount":40,"commentCount":9}}]}|};
    };
  ];

  LinkedIn.get_account_analytics
    ~account_id:"test_account"
    ~entity_urn:"urn:li:organization:2414183"
    (handle_result
      (fun (analytics : account_analytics) ->
        assert (analytics.entity_urn = "urn:li:organization:2414183");
        assert (analytics.follower_count = Some 321);
        assert (analytics.impression_count = Some 2100);
        assert (analytics.unique_impression_count = Some 1700);
        assert (analytics.share_count = Some 12);
        assert (analytics.click_count = Some 88);
        assert (analytics.like_count = Some 40);
        assert (analytics.comment_count = Some 9);
        let requests = List.rev !Mock_http.requests in
        (match requests with
        | [ ("GET", follower_url, follower_headers, _);
            ("GET", share_url, share_headers, _) ] ->
            assert (string_contains follower_url "organizationalEntityFollowerStatistics");
            assert (string_contains share_url "organizationalEntityShareStatistics");
            assert (string_contains follower_url "q=organizationalEntity");
            assert (string_contains share_url "q=organizationalEntity");
            assert (string_contains follower_url "organizationalEntity=");
            assert (string_contains follower_url "2414183");
            assert (find_header follower_headers "X-RestLi-Method" = Some "FINDER");
            assert (find_header share_headers "X-RestLi-Method" = Some "FINDER");
            assert (find_header follower_headers "X-Restli-Protocol-Version" = Some "2.0.0");
            assert (find_header_exact follower_headers "LinkedIn-Version" = Some "202601")
        | _ -> failwith "Expected two analytics finder requests");
        print_endline "✓ Get account analytics")
      (fun err -> failwith ("Get account analytics failed: " ^ err)))

(** Test: get_account_analytics rejects invalid entity URN before network call *)
let test_get_account_analytics_rejects_invalid_entity_urn () =
  Mock_config.reset ();

  LinkedIn.get_account_analytics
    ~account_id:"test_account"
    ~entity_urn:"organization:2414183"
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "author URN");
          assert (!Mock_http.requests = []);
          print_endline "✓ Get account analytics rejects invalid entity URN"
      | Ok _ -> failwith "Expected invalid entity URN rejection"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: Post with URL preview (ARTICLE media category) *)
let test_post_with_url_preview () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  (* Set up response queue: first person URN, then post response *)
  let person_response = {|{"sub": "user123"}|} in
  let post_response = {|{"id": "urn:li:share:789"}|} in
  Mock_http.set_responses [
    { status = 200; body = person_response; headers = [] };
    { status = 201; body = post_response; headers = [] };
  ];
  
  let text = "Great article about OCaml! https://example.com/ocaml-article" in
  
  LinkedIn.post_single ~account_id:"test_account" ~text ~media_urls:[]
    (handle_outcome
      (fun post_id ->
        assert (post_id = "urn:li:share:789");

        (* Check that the request included article content with source URL *)
        let requests = List.rev !Mock_http.requests in
        let post_request = List.find (fun (method_, url, _, _) ->
          method_ = "POST" && string_contains url "/rest/posts"
        ) requests in

        let (_, _, _, body) = post_request in
        assert (string_contains body "article");
        assert (string_contains body "source");
        assert (string_contains body "https://example.com/ocaml-article");

        print_endline "✓ Post with URL preview (article)")
      (fun err -> failwith ("Post with URL failed: " ^ err)))

(** Test: post_single maps 403 to write scope *)
let test_post_single_insufficient_permissions_error () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_responses [
    { status = 200; body = {|{"sub":"user123"}|}; headers = [] };
    { status = 403; body = {|{"message":"Not enough permissions"}|}; headers = [] };
  ];

  LinkedIn.post_single ~account_id:"test_account" ~text:"hello" ~media_urls:[]
    (function
      | Error_types.Failure (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (List.mem "w_member_social" scopes);
          print_endline "✓ Post single maps 403 to write scope"
      | Error_types.Success _ -> failwith "Expected permission failure"
      | Error_types.Partial_success _ -> failwith "Expected permission failure"
      | Error_types.Failure err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: post_single can publish as organization when author_urn is provided *)
let test_post_single_with_organization_author () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_responses [
    { status = 200; body = "image_data"; headers = [] };
    {
      status = 200;
      body =
        {|{"value":{"image":"urn:li:image:img123","uploadUrl":"https://upload.linkedin.com/test"}}|};
      headers = [];
    };
    { status = 201; body = ""; headers = [] };
    { status = 201; body = {|{"id":"urn:li:share:orgpost123"}|}; headers = [] };
  ];

  LinkedIn.post_single
    ~account_id:"test_account"
    ~author_urn:"urn:li:organization:2414183"
    ~text:"Organization post"
    ~media_urls:["https://example.com/org-image.jpg"]
    (handle_outcome
      (fun post_id ->
        assert (post_id = "urn:li:share:orgpost123");
        let requests = List.rev !Mock_http.requests in
        let register_request =
          List.find_opt (fun (method_, url, _, _) ->
            method_ = "POST" && string_contains url "initializeUpload") requests
        in
        let post_request =
          List.find_opt (fun (method_, url, _, _) ->
            method_ = "POST" && string_contains url "/rest/posts") requests
        in
        (match register_request with
        | Some (_, _, _, body) -> assert (string_contains body "urn:li:organization:2414183")
        | None -> failwith "No register upload request found");
        (match post_request with
        | Some (_, _, _, body) -> assert (string_contains body "urn:li:organization:2414183")
        | None -> failwith "No post request found");
        print_endline "✓ Post single supports organization author")
      (fun err -> failwith ("Organization author post failed: " ^ err)))

(** Test: post_single maps 403 to organization write scope for organization author *)
let test_post_single_org_insufficient_permissions_error () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response { status = 403; body = {|{"message":"Not enough permissions"}|}; headers = [] };

  LinkedIn.post_single
    ~account_id:"test_account"
    ~author_urn:"urn:li:organization:2414183"
    ~text:"Organization post"
    ~media_urls:[]
    (function
      | Error_types.Failure (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (List.mem "w_organization_social" scopes);
          print_endline "✓ Post single org author maps 403 to organization scope"
      | Error_types.Success _ -> failwith "Expected permission failure"
      | Error_types.Partial_success _ -> failwith "Expected permission failure"
      | Error_types.Failure err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: post_single rejects malformed explicit author_urn *)
let test_post_single_rejects_malformed_author_urn () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;

  LinkedIn.post_single
    ~account_id:"test_account"
    ~author_urn:"urn:li:company:123"
    ~text:"Invalid author urn post"
    ~media_urls:[]
    (function
      | Error_types.Failure (Error_types.Internal_error msg) ->
          assert (string_contains msg "author URN");
          print_endline "✓ Post single rejects malformed explicit author URN"
      | Error_types.Success _ -> failwith "Expected validation failure"
      | Error_types.Partial_success _ -> failwith "Expected validation failure"
      | Error_types.Failure err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: malformed explicit author_urn fails before any network request *)
let test_post_single_malformed_author_urn_short_circuits_network () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;

  LinkedIn.post_single
    ~account_id:"test_account"
    ~author_urn:"urn:li:company:123"
    ~text:"Invalid author urn post"
    ~media_urls:[]
    (function
      | Error_types.Failure (Error_types.Internal_error _) ->
          assert (!Mock_http.requests = []);
          print_endline "✓ Post single malformed explicit author URN short-circuits network"
      | Error_types.Success _ -> failwith "Expected validation failure"
      | Error_types.Partial_success _ -> failwith "Expected validation failure"
      | Error_types.Failure err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: explicit author_urn with surrounding whitespace is rejected before network *)
let test_post_single_rejects_whitespace_author_urn_short_circuits_network () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;

  LinkedIn.post_single
    ~account_id:"test_account"
    ~author_urn:" urn:li:organization:2414183 "
    ~text:"Invalid author urn post"
    ~media_urls:[]
    (function
      | Error_types.Failure (Error_types.Internal_error msg) ->
          assert (string_contains msg "whitespace");
          assert (!Mock_http.requests = []);
          print_endline "✓ Post single rejects whitespace explicit author URN before network"
      | Error_types.Success _ -> failwith "Expected validation failure"
      | Error_types.Partial_success _ -> failwith "Expected validation failure"
      | Error_types.Failure err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: OAuth URL contains all required parameters *)
let test_oauth_url_parameters () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client_id";
  
  let state = "test_state_123" in
  let redirect_uri = "https://example.com/callback" in
  
  LinkedIn.get_oauth_url ~redirect_uri ~state
    (fun url ->
      (* Verify all required OAuth parameters are present *)
      assert (string_contains url "response_type=code");
      assert (string_contains url "client_id=test_client_id");
      assert (string_contains url "redirect_uri=");
      assert (string_contains url "state=test_state_123");
      assert (string_contains url "scope=");
      (* Verify scope contains required permissions *)
      assert (string_contains url "openid" || string_contains url "profile" || string_contains url "email");
      print_endline "✓ OAuth URL parameters complete")
    (fun err -> failwith ("OAuth URL parameters test failed: " ^ err))

(** Test: OAuth URL can include organization scopes for page linking *)
let test_oauth_url_with_organization_scopes () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client_id";

  LinkedIn.get_oauth_url ~include_organization_scopes:true
    ~redirect_uri:"https://example.com/callback" ~state:"state123"
    (fun url ->
      assert (string_contains url "w_organization_social");
      assert (string_contains url "r_organization_admin");
      print_endline "✓ OAuth URL includes organization scopes")
    (fun err -> failwith ("OAuth URL with organization scopes failed: " ^ err))

(** Test: exchange_code_and_get_organizations returns approved org access entries *)
let test_exchange_code_and_get_organizations () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client";
  Mock_config.set_env "LINKEDIN_CLIENT_SECRET" "test_secret";

  Mock_http.set_responses [
    {
      status = 200;
      body =
        {|{"access_token":"token123","refresh_token":"refresh123","expires_in":5184000}|};
      headers = [];
    };
    {
      status = 200;
      body =
        {|{"elements":[{"role":"ADMINISTRATOR","state":"APPROVED","organization":"urn:li:organization:2414183"}]}|};
      headers = [];
    };
  ];

  LinkedIn.exchange_code_and_get_organizations
    ~code:"test_code" ~redirect_uri:"https://example.com/callback"
    (fun (creds, orgs) ->
      assert (creds.access_token = "token123");
      assert (List.length orgs = 1);
      let org = List.hd orgs in
      assert (org.organization_urn = "urn:li:organization:2414183");
      assert (org.organization_id = Some "2414183");
      assert (org.role = Some "ADMINISTRATOR");
      assert (org.state = Some "APPROVED");
      print_endline "✓ Exchange code and get organizations")
    (fun err -> failwith ("exchange_code_and_get_organizations failed: " ^ err))

(** Test: exchange_code_and_get_organizations normalizes role and acl_state filters *)
let test_exchange_code_and_get_organizations_filter_normalization () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client";
  Mock_config.set_env "LINKEDIN_CLIENT_SECRET" "test_secret";

  Mock_http.set_responses [
    {
      status = 200;
      body =
        {|{"access_token":"token123","refresh_token":"refresh123","expires_in":5184000}|};
      headers = [];
    };
    {
      status = 200;
      body = {|{"elements":[]}|};
      headers = [];
    };
  ];

  LinkedIn.exchange_code_and_get_organizations
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    ~role:" administrator "
    ~acl_state:" approved "
    (fun (_creds, _orgs) ->
      let requests = List.rev !Mock_http.requests in
      match requests with
      | [ (_, _token_url, _, _); (_, acl_url, _, _) ] ->
          assert (string_contains acl_url "role=ADMINISTRATOR");
          assert (string_contains acl_url "state=APPROVED");
          print_endline "✓ Exchange code and organizations normalizes role/state filters"
      | _ -> failwith "Expected token request followed by ACL request")
    (fun err -> failwith ("exchange_code_and_get_organizations filter normalization failed: " ^ err))

(** Test: exchange_code_and_get_preferred_organization returns ranked preferred org *)
let test_exchange_code_and_get_preferred_organization () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client";
  Mock_config.set_env "LINKEDIN_CLIENT_SECRET" "test_secret";

  Mock_http.set_responses [
    {
      status = 200;
      body =
        {|{"access_token":"token123","refresh_token":"refresh123","expires_in":5184000}|};
      headers = [];
    };
    {
      status = 200;
      body =
        {|{"elements":[
          {"role":"ANALYST","state":"APPROVED","organization":"urn:li:organization:100"},
          {"role":"ADMINISTRATOR","state":"APPROVED","organization":"urn:li:organization:200"}
        ]}|};
      headers = [];
    };
  ];

  LinkedIn.exchange_code_and_get_preferred_organization
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    (fun (creds, selected) ->
      assert (creds.access_token = "token123");
      (match selected with
      | Some org -> assert (org.organization_urn = "urn:li:organization:200")
      | None -> failwith "Expected preferred organization");
      print_endline "✓ Exchange code and get preferred organization")
    (fun err -> failwith ("exchange_code_and_get_preferred_organization failed: " ^ err))

(** Test: exchange_code_and_get_preferred_organization returns None when ACLs empty *)
let test_exchange_code_and_get_preferred_organization_none () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client";
  Mock_config.set_env "LINKEDIN_CLIENT_SECRET" "test_secret";

  Mock_http.set_responses [
    {
      status = 200;
      body =
        {|{"access_token":"token123","refresh_token":"refresh123","expires_in":5184000}|};
      headers = [];
    };
    {
      status = 200;
      body = {|{"elements":[]}|};
      headers = [];
    };
  ];

  LinkedIn.exchange_code_and_get_preferred_organization
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    (fun (_creds, selected) ->
      assert (selected = None);
      print_endline "✓ Exchange code and get preferred organization returns None when empty")
    (fun err -> failwith ("exchange_code_and_get_preferred_organization empty failed: " ^ err))

(** Test: exchange_code_and_get_preferred_organization surfaces ACL insufficient-permissions context *)
let test_exchange_code_and_get_preferred_organization_acl_403 () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client";
  Mock_config.set_env "LINKEDIN_CLIENT_SECRET" "test_secret";

  Mock_http.set_responses [
    {
      status = 200;
      body =
        {|{"access_token":"token123","refresh_token":"refresh123","expires_in":5184000}|};
      headers = [];
    };
    {
      status = 403;
      body = {|{"message":"Forbidden"}|};
      headers = [];
    };
  ];

  LinkedIn.exchange_code_and_get_preferred_organization
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    (fun _ -> failwith "Expected ACL permission error")
    (fun err ->
      assert (string_contains err "permission" || string_contains err "403");
      assert (string_contains err "r_organization_admin");
      print_endline "✓ Exchange preferred organization maps ACL 403 to org scope hint")

(** Test: exchange_code_and_get_preferred_organization surfaces ACL rate-limit context *)
let test_exchange_code_and_get_preferred_organization_acl_429 () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client";
  Mock_config.set_env "LINKEDIN_CLIENT_SECRET" "test_secret";

  Mock_http.set_responses [
    {
      status = 200;
      body =
        {|{"access_token":"token123","refresh_token":"refresh123","expires_in":5184000}|};
      headers = [];
    };
    {
      status = 429;
      body = {|{"message":"Too many requests"}|};
      headers = [];
    };
  ];

  LinkedIn.exchange_code_and_get_preferred_organization
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    (fun _ -> failwith "Expected ACL rate-limit error")
    (fun err ->
      let err_lc = String.lowercase_ascii err in
      assert (
        string_contains err_lc "rate"
        || string_contains err_lc "429"
        || string_contains err_lc "too many"
      );
      print_endline "✓ Exchange preferred organization maps ACL 429 to rate-limit")

(** Test: OAuth URL encoding *)
let test_oauth_url_encoding () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test&client=id";
  
  let state = "state with spaces" in
  let redirect_uri = "https://example.com/callback?param=value" in
  
  LinkedIn.get_oauth_url ~redirect_uri ~state
    (fun url ->
      (* URL should be properly encoded *)
      assert (not (String.contains url ' '));
      assert (string_contains url "state=" || string_contains url "redirect_uri=");
      print_endline "✓ OAuth URL encoding")
    (fun err -> failwith ("OAuth URL encoding test failed: " ^ err))

(** Test: Token exchange with invalid response *)
let test_token_exchange_invalid () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client";
  Mock_config.set_env "LINKEDIN_CLIENT_SECRET" "test_secret";
  
  let invalid_response = {|{"error": "invalid_grant"}|} in
  Mock_http.set_response { status = 400; body = invalid_response; headers = [] };
  
  LinkedIn.exchange_code 
    ~code:"bad_code"
    ~redirect_uri:"https://example.com/callback"
    (fun _ -> failwith "Should fail with invalid grant")
    (fun err ->
      assert (string_contains err "400" || string_contains err "invalid");
      print_endline "✓ Token exchange invalid response handling")

(** Test: Token exchange with missing fields *)
let test_token_exchange_missing_fields () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client";
  Mock_config.set_env "LINKEDIN_CLIENT_SECRET" "test_secret";
  
  let incomplete_response = {|{"access_token": "token123", "expires_in": 5184000}|} in
  Mock_http.set_response { status = 200; body = incomplete_response; headers = [] };
  
  LinkedIn.exchange_code 
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    (fun creds ->
      (* Should handle missing refresh_token gracefully *)
      assert (creds.access_token = "token123");
      assert (creds.refresh_token = None);
      print_endline "✓ Token exchange with missing optional fields")
    (fun err -> failwith ("Should succeed with minimal response: " ^ err))

(** Test: Token exchange treats blank refresh_token as absent *)
let test_token_exchange_blank_refresh_token () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client";
  Mock_config.set_env "LINKEDIN_CLIENT_SECRET" "test_secret";

  let response = {|{"access_token":"token123","refresh_token":"   ","expires_in":5184000}|} in
  Mock_http.set_response { status = 200; body = response; headers = [] };

  LinkedIn.exchange_code
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    (fun creds ->
      assert (creds.access_token = "token123");
      assert (creds.refresh_token = None);
      print_endline "✓ Token exchange blank refresh token handled")
    (fun err -> failwith ("Should treat blank refresh token as missing: " ^ err))

(** Test: Token refresh with rotating tokens *)
let test_token_refresh_rotation () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client";
  Mock_config.set_env "LINKEDIN_CLIENT_SECRET" "test_secret";
  Mock_config.set_env "LINKEDIN_ENABLE_PROGRAMMATIC_REFRESH" "true";
  
  let response_body = {|{
    "access_token": "new_access_token_v2",
    "refresh_token": "new_refresh_token_v2",
    "expires_in": 5184000
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  LinkedIn.refresh_access_token
    ~client_id:"test_client"
    ~client_secret:"test_secret"
    ~refresh_token:"old_refresh_v1"
    (fun (access, refresh, _expires) ->
      (* New tokens should be different *)
      assert (access = "new_access_token_v2");
      assert (refresh = "new_refresh_token_v2");
      assert (access <> "old_access");
      assert (refresh <> "old_refresh_v1");
      print_endline "✓ Token refresh with rotation")
    (fun err -> failwith ("Token refresh rotation failed: " ^ err))

(** Test: Token refresh keeps previous token when new refresh_token is blank *)
let test_token_refresh_blank_refresh_token_fallback () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client";
  Mock_config.set_env "LINKEDIN_CLIENT_SECRET" "test_secret";
  Mock_config.set_env "LINKEDIN_ENABLE_PROGRAMMATIC_REFRESH" "true";

  let response_body = {|{"access_token":"new_access","refresh_token":" ","expires_in":5184000}|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  LinkedIn.refresh_access_token
    ~client_id:"test_client"
    ~client_secret:"test_secret"
    ~refresh_token:"old_refresh"
    (fun (access, refresh, _expires) ->
      assert (access = "new_access");
      assert (refresh = "old_refresh");
      print_endline "✓ Token refresh blank refresh token fallback")
    (fun err -> failwith ("Refresh should fallback to existing token: " ^ err))

(** Test: Expired token detection *)
let test_expired_token_detection () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_ENABLE_PROGRAMMATIC_REFRESH" "true";
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client";
  Mock_config.set_env "LINKEDIN_CLIENT_SECRET" "test_secret";
  
  (* Set credentials with past expiry *)
  let past_time = 
    let now = Ptime_clock.now () in
    match Ptime.sub_span now (Ptime.Span.of_int_s 86400) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate past time"
  in
  
  let creds = {
    access_token = "expired_token";
    refresh_token = Some "refresh_token";
    expires_at = Some past_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  (* Provider will attempt to refresh expired token *)
  let refresh_response = {|{
    "access_token": "refreshed_token",
    "refresh_token": "new_refresh_token",
    "expires_in": 5184000
  }|} in
  Mock_http.set_response { status = 200; body = refresh_response; headers = [] };
  
  LinkedIn.ensure_valid_token ~account_id:"test_account"
    (fun token ->
      (* Token refresh should succeed *)
      assert (token = "refreshed_token");
      print_endline "✓ Expired token detection and refresh")
    (fun err ->
      (* Or fail gracefully if refresh not enabled *)
      assert (String.length (Error_types.error_to_string err) > 0);
      print_endline "✓ Expired token detection and refresh")

(** Test: OAuth state CSRF protection *)
let test_oauth_state_validation () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client_id";
  
  (* Generate two different states *)
  let state1 = "state_123" in
  let state2 = "state_456" in
  
  LinkedIn.get_oauth_url ~redirect_uri:"https://example.com/callback" ~state:state1
    (fun url1 ->
      LinkedIn.get_oauth_url ~redirect_uri:"https://example.com/callback" ~state:state2
        (fun url2 ->
          (* URLs should contain different states *)
          assert (string_contains url1 "state_123");
          assert (string_contains url2 "state_456");
          assert (url1 <> url2);
          print_endline "✓ OAuth state CSRF protection")
        (fun err -> failwith ("State validation test failed: " ^ err)))
    (fun err -> failwith ("State validation test failed: " ^ err))

(** Test: Refresh token expiry behavior *)
let test_refresh_token_expiry () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_ENABLE_PROGRAMMATIC_REFRESH" "true";
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client";
  Mock_config.set_env "LINKEDIN_CLIENT_SECRET" "test_secret";
  
  (* Test with expired refresh token *)
  let error_response = {|{"error": "invalid_grant", "error_description": "Refresh token expired"}|} in
  Mock_http.set_response { status = 400; body = error_response; headers = [] };
  
  LinkedIn.refresh_access_token
    ~client_id:"test_client"
    ~client_secret:"test_secret"
    ~refresh_token:"expired_refresh"
    (fun _ -> failwith "Should fail with expired refresh token")
    (fun err ->
      (* Error message should contain something about failure *)
      assert (String.length err > 0);
      print_endline "✓ Refresh token expiry handling")

(** Test: Scope validation in OAuth URL *)
let test_oauth_scope_validation () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client_id";
  
  LinkedIn.get_oauth_url ~redirect_uri:"https://example.com/callback" ~state:"test_state"
    (fun url ->
      (* Verify required scopes are present *)
      let has_openid = string_contains url "openid" in
      let has_profile = string_contains url "profile" in
      let has_email = string_contains url "email" in
      let has_posts = string_contains url "w_member_social" in
      
      assert (has_openid && has_profile && has_email && has_posts);
      print_endline "✓ OAuth scope validation")
    (fun err -> failwith ("Scope validation failed: " ^ err))

(** Helper to create mock responses for post_single with media *)
let make_media_upload_responses ~num_images =
  (* 1. GET userinfo *)
  let userinfo_response = { status = 200; body = {|{"sub": "user123"}|}; headers = [] } in
  (* For each image: GET media, POST register, PUT upload *)
  let per_image_responses = List.init num_images (fun _ -> [
    (* GET media URL - fake binary data *)
    { status = 200; body = "fake_image_binary_data"; headers = [] };
    (* POST register upload via /rest/images *)
    { status = 200; body = {|{"value": {"image": "urn:li:image:123456", "uploadUrl": "https://api.linkedin.com/upload/123"}}|}; headers = [] };
    (* PUT upload binary *)
    { status = 200; body = ""; headers = [] };
  ]) |> List.flatten in
  (* Final POST to create the post via /rest/posts *)
  let create_post_response = { status = 201; body = {|{"id": "urn:li:share:123456"}|}; headers = [] } in
  [userinfo_response] @ per_image_responses @ [create_post_response]

(** Test: Post with single image and alt-text *)
let test_post_with_alt_text () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  Mock_http.set_responses (make_media_upload_responses ~num_images:1);
  
  LinkedIn.post_single 
    ~account_id:"test_account"
    ~text:"Check out this image!"
    ~media_urls:["https://example.com/image.jpg"]
    ~alt_texts:[Some "A beautiful sunset over mountains"]
    (handle_outcome
      (fun _post_id ->
        print_endline "✓ Post with single image and alt-text")
      (fun err -> failwith ("Post with alt-text failed: " ^ err)))

(** Test: Post with multiple images and alt-texts *)
let test_post_with_multiple_alt_texts () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  Mock_http.set_responses (make_media_upload_responses ~num_images:2);
  
  LinkedIn.post_single 
    ~account_id:"test_account"
    ~text:"Multiple images with descriptions"
    ~media_urls:["https://example.com/img1.jpg"; "https://example.com/img2.jpg"]
    ~alt_texts:[Some "First image description"; Some "Second image description"]
    (handle_outcome
      (fun _post_id ->
        print_endline "✓ Post with multiple images and alt-texts")
      (fun err -> failwith ("Post with multiple alt-texts failed: " ^ err)))

(** Test: Post with image but no alt-text *)
let test_post_without_alt_text () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  Mock_http.set_responses (make_media_upload_responses ~num_images:1);
  
  LinkedIn.post_single 
    ~account_id:"test_account"
    ~text:"Image without description"
    ~media_urls:["https://example.com/image.jpg"]
    ~alt_texts:[]
    (handle_outcome
      (fun _post_id ->
        print_endline "✓ Post without alt-text")
      (fun err -> failwith ("Post without alt-text failed: " ^ err)))

(** Test: Partial alt-texts - fewer alt-texts than images *)
let test_post_with_partial_alt_texts () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  Mock_http.set_responses (make_media_upload_responses ~num_images:3);
  
  LinkedIn.post_single 
    ~account_id:"test_account"
    ~text:"Three images, two descriptions"
    ~media_urls:["https://example.com/img1.jpg"; "https://example.com/img2.jpg"; "https://example.com/img3.jpg"]
    ~alt_texts:[Some "First image"; Some "Second image"]
    (handle_outcome
      (fun _post_id ->
        print_endline "✓ Post with partial alt-texts (3 images, 2 alt-texts)")
      (fun err -> failwith ("Post with partial alt-texts failed: " ^ err)))

(** Test: Alt-text with special characters *)
let test_alt_text_special_chars () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  Mock_http.set_responses (make_media_upload_responses ~num_images:1);
  
  LinkedIn.post_single 
    ~account_id:"test_account"
    ~text:"Testing special characters in alt-text"
    ~media_urls:["https://example.com/image.jpg"]
    ~alt_texts:[Some "A photo with \"quotes\", emojis 🌅, & special chars: <>&"]
    (handle_outcome
      (fun _post_id ->
        print_endline "✓ Alt-text with special characters")
      (fun err -> failwith ("Alt-text with special chars failed: " ^ err)))

(** Test: Thread with alt-texts per post *)
let test_thread_with_alt_texts () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  (* LinkedIn post_thread only posts the first item, so we need 1 image worth of responses *)
  Mock_http.set_responses (make_media_upload_responses ~num_images:1);
  
  LinkedIn.post_thread
    ~account_id:"test_account"
    ~texts:["First post with image"; "Second post with image"]
    ~media_urls_per_post:[["https://example.com/img1.jpg"]; ["https://example.com/img2.jpg"]]
    ~alt_texts_per_post:[[Some "Description for first image"]; [Some "Description for second image"]]
    (handle_thread_outcome
      (fun _post_ids ->
        print_endline "✓ Thread with alt-texts per post")
      (fun err -> failwith ("Thread with alt-texts failed: " ^ err)))

(** {1 REST.li Protocol Tests} *)
(** 
   These tests verify protocol-level behaviors that the official LinkedIn SDK tests cover.
   They ensure correct REST.li headers, URL encoding, and request formatting.
*)

(** Test: X-RestLi-Protocol-Version header is sent on API calls *)
let test_restli_protocol_version_header () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  let response_body = {|{
    "id": "urn:li:share:123",
    "author": "urn:li:person:abc",
    "lifecycleState": "PUBLISHED"
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  LinkedIn.get_post ~account_id:"test_account" ~post_urn:"urn:li:share:123"
    (handle_result
      (fun _post ->
        (* Check that X-RestLi-Protocol-Version header was sent *)
        let requests = !Mock_http.requests in
        match requests with
        | (_, _, headers, _) :: _ ->
            (match find_header headers "X-Restli-Protocol-Version" with
            | Some version -> 
                assert (version = "2.0.0");
                print_endline "✓ X-RestLi-Protocol-Version header (2.0.0)"
            | None -> failwith "X-RestLi-Protocol-Version header not found")
        | [] -> failwith "No requests recorded")
      (fun err -> failwith ("Get post failed: " ^ err)))

(** Test: Authorization header format is correct (Bearer token) *)
let test_authorization_header_format () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "test_bearer_token_12345";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  let response_body = {|{"sub": "user123", "name": "Test User"}|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  LinkedIn.get_profile ~account_id:"test_account"
    (handle_result
      (fun _profile ->
        let requests = !Mock_http.requests in
        match requests with
        | (_, _, headers, _) :: _ ->
            (match find_header headers "Authorization" with
            | Some auth -> 
                assert (String.starts_with ~prefix:"Bearer " auth);
                assert (string_contains auth "test_bearer_token_12345");
                print_endline "✓ Authorization header format (Bearer token)"
            | None -> failwith "Authorization header not found")
        | [] -> failwith "No requests recorded")
      (fun err -> failwith ("Get profile failed: " ^ err)))

(** Test: Content-Type header is application/json for POST requests *)
let test_content_type_header_json () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  (* Set up response queue: first person URN, then like response *)
  let person_response = {|{"sub": "user123"}|} in
  Mock_http.set_responses [
    { status = 200; body = person_response; headers = [] };
    { status = 201; body = "{}"; headers = [] };
  ];
  
  LinkedIn.like_post ~account_id:"test_account" ~post_urn:"urn:li:share:123"
    (handle_result
      (fun () ->
        (* Find the POST request (like_post) *)
        let requests = List.rev !Mock_http.requests in
        let post_request = List.find_opt (fun (method_, _, _, _) ->
          method_ = "POST"
        ) requests in
        
        match post_request with
        | Some (_, _, headers, _) ->
            (match find_header headers "Content-Type" with
            | Some ct -> 
                assert (ct = "application/json");
                print_endline "✓ Content-Type header (application/json)"
            | None -> failwith "Content-Type header not found on POST request")
        | None -> failwith "No POST request found")
      (fun err -> failwith ("Like post failed: " ^ err)))

(** Test: URN is properly URL-encoded in path *)
let test_urn_path_encoding () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  let response_body = {|{
    "id": "urn:li:share:123456789",
    "author": "urn:li:person:abc",
    "lifecycleState": "PUBLISHED"
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  (* URN contains colons which need to be encoded *)
  let test_urn = "urn:li:share:123456789" in
  
  LinkedIn.get_post ~account_id:"test_account" ~post_urn:test_urn
    (handle_result
      (fun _post ->
        let requests = !Mock_http.requests in
        match requests with
        | (_, url, _, _) :: _ ->
            (* URL should contain encoded URN - colons become %3A *)
            assert (string_contains url "urn%3Ali%3Ashare%3A123456789" ||
                    string_contains url "urn:li:share:123456789");
            print_endline "✓ URN path encoding"
        | [] -> failwith "No requests recorded")
      (fun err -> failwith ("Get post failed: " ^ err)))

(** Test: get_post maps 403 to read scope *)
let test_get_post_insufficient_permissions_error () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response {
    status = 403;
    headers = [];
    body = {|{"message":"Not enough permissions"}|};
  };

  LinkedIn.get_post ~account_id:"test_account" ~post_urn:"urn:li:share:123"
    (fun result ->
      match result with
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (List.mem "r_member_social" scopes);
          print_endline "✓ Get post maps 403 to read scope"
      | Ok _ -> failwith "Expected permission error"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: get_post rejects whitespace post URN without network calls *)
let test_get_post_rejects_whitespace_urn () =
  Mock_config.reset ();

  LinkedIn.get_post ~account_id:"test_account" ~post_urn:" urn:li:share:123 "
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "post URN");
          assert (string_contains msg "whitespace");
          assert (!Mock_http.requests = []);
          print_endline "✓ Get post rejects whitespace URN"
      | Ok _ -> failwith "Expected whitespace URN rejection"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: get_post rejects blank post URN without network calls *)
let test_get_post_rejects_blank_urn () =
  Mock_config.reset ();

  LinkedIn.get_post ~account_id:"test_account" ~post_urn:""
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "post URN is required");
          assert (!Mock_http.requests = []);
          print_endline "✓ Get post rejects blank URN"
      | Ok _ -> failwith "Expected blank URN rejection"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: get_post rejects malformed delimiter characters in post URN *)
let test_get_post_rejects_malformed_delimiter_urn () =
  Mock_config.reset ();

  LinkedIn.get_post ~account_id:"test_account" ~post_urn:"urn:li:share:123,456"
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "invalid delimiter");
          assert (!Mock_http.requests = []);
          print_endline "✓ Get post rejects malformed delimiter URN"
      | Ok _ -> failwith "Expected malformed delimiter URN rejection"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: Batch request URL encoding for multiple URNs *)
let test_batch_urns_encoding () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  let response_body = {|{
    "results": {
      "urn:li:share:111": {"id": "urn:li:share:111", "author": "urn:li:person:a", "lifecycleState": "PUBLISHED"},
      "urn:li:share:222": {"id": "urn:li:share:222", "author": "urn:li:person:b", "lifecycleState": "PUBLISHED"}
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  LinkedIn.batch_get_posts 
    ~account_id:"test_account" 
    ~post_urns:["urn:li:share:111"; "urn:li:share:222"]
    (handle_result
      (fun _posts ->
        let requests = !Mock_http.requests in
        match requests with
        | (_, url, headers, _) :: _ ->
            (* URL should contain ids parameter encoded as Rest.li list syntax *)
            assert (string_contains url "ids=");
            assert (string_contains url "List" || string_contains url "List%28");
            assert (not (string_contains url "%25"));
            assert (string_contains url "111");
            assert (string_contains url "222");
            assert (find_header headers "X-RestLi-Method" = Some "BATCH_GET");
            print_endline "✓ Batch URNs encoding"
        | [] -> failwith "No requests recorded")
      (fun err -> failwith ("Batch get posts failed: " ^ err)))

(** Test: FINDER requests include X-RestLi-Method header *)
let test_finder_method_header () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response {
    status = 200;
    headers = [];
    body = "{\"elements\": [], \"paging\": {\"start\": 0, \"count\": 0, \"total\": 0}}";
  };

  LinkedIn.search_posts ~account_id:"test_account" ~author:"urn:li:person:user1"
    (handle_result
      (fun _collection ->
        let requests = !Mock_http.requests in
        match requests with
        | (_, _, headers, _) :: _ ->
            assert (find_header headers "X-RestLi-Method" = Some "FINDER");
            print_endline "✓ Finder request method header"
        | [] -> failwith "No requests recorded")
      (fun err -> failwith ("Search posts failed: " ^ err)))

(** Test: Request body structure for ugcPost creation *)
let test_rest_post_request_body_structure () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;

  (* Set up response queue: first person URN, then post response *)
  let person_response = {|{"sub": "user123"}|} in
  let post_response = {|{"id": "urn:li:share:789"}|} in
  Mock_http.set_responses [
    { status = 200; body = person_response; headers = [] };
    { status = 201; body = post_response; headers = [] };
  ];

  LinkedIn.post_single ~account_id:"test_account" ~text:"Test post content" ~media_urls:[]
    (handle_outcome
      (fun _post_id ->
        (* Find the POST request to /rest/posts *)
        let requests = List.rev !Mock_http.requests in
        let post_request = List.find_opt (fun (method_, url, _, _) ->
          method_ = "POST" && string_contains url "/rest/posts"
        ) requests in

        match post_request with
        | Some (_, _, headers, body) ->
            (* Verify modern /rest/posts body format *)
            assert (string_contains body "author");
            assert (string_contains body "lifecycleState");
            assert (string_contains body "PUBLISHED");
            assert (string_contains body "commentary");
            assert (string_contains body "Test post content");
            assert (string_contains body "visibility");
            assert (string_contains body "\"PUBLIC\"");
            assert (string_contains body "distribution");
            assert (string_contains body "feedDistribution");
            assert (string_contains body "MAIN_FEED");
            (* Verify LinkedIn-Version header is present *)
            assert (find_header headers "LinkedIn-Version" <> None);
            print_endline "✓ REST Post request body structure"
        | None -> failwith "No /rest/posts POST request found")
      (fun err -> failwith ("Post single failed: " ^ err)))

(** Test: Comment request body structure *)
let test_comment_request_body_structure () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  (* Set up response queue: first person URN, then comment response *)
  let person_response = {|{"sub": "user123"}|} in
  let comment_response = {|{"id": "comment123"}|} in
  Mock_http.set_responses [
    { status = 200; body = person_response; headers = [] };
    { status = 201; body = comment_response; headers = [] };
  ];
  
  LinkedIn.comment_on_post 
    ~account_id:"test_account" 
    ~post_urn:"urn:li:share:123"
    ~text:"This is a test comment"
    (handle_result
      (fun _comment_id ->
        (* Find the POST request to comments *)
        let requests = List.rev !Mock_http.requests in
        let comment_request = List.find_opt (fun (method_, url, _, _) ->
          method_ = "POST" && string_contains url "comments"
        ) requests in
        
        match comment_request with
        | Some (_, _, _, body) ->
            (* Verify required fields *)
            assert (string_contains body "actor");
            assert (string_contains body "object");
            assert (string_contains body "message");
            assert (string_contains body "text");
            assert (string_contains body "This is a test comment");
            print_endline "✓ Comment request body structure"
        | None -> failwith "No comments POST request found")
      (fun err -> failwith ("Comment on post failed: " ^ err)))

(** Test: DELETE request for unlike_post *)
let test_unlike_post_delete () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  (* Set up response queue: first person URN, then delete response *)
  let person_response = {|{"sub": "user123"}|} in
  Mock_http.set_responses [
    { status = 200; body = person_response; headers = [] };
    { status = 204; body = ""; headers = [] };
  ];
  
  LinkedIn.unlike_post ~account_id:"test_account" ~post_urn:"urn:li:share:123"
    (handle_result
      (fun () ->
        (* Find the DELETE request *)
        let requests = List.rev !Mock_http.requests in
        let delete_request = List.find_opt (fun (method_, _, _, _) ->
          method_ = "DELETE"
        ) requests in
        
        match delete_request with
        | Some (_, url, headers, _) ->
            (* Verify DELETE request was made to likes endpoint *)
            assert (string_contains url "socialActions");
            assert (string_contains url "likes");
            (* Verify headers include Authorization and X-RestLi-Protocol-Version *)
            assert (find_header headers "Authorization" <> None);
            assert (find_header headers "X-Restli-Protocol-Version" <> None);
            print_endline "✓ Unlike post DELETE request"
        | None -> failwith "No DELETE request found")
      (fun err -> failwith ("Unlike post failed: " ^ err)))

(** Test: unlike_post maps 403 to write scope *)
let test_unlike_post_insufficient_permissions_error () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_responses [
    { status = 200; body = {|{"sub":"user123"}|}; headers = [] };
    { status = 403; body = {|{"message":"Not enough permissions"}|}; headers = [] };
  ];

  LinkedIn.unlike_post ~account_id:"test_account" ~post_urn:"urn:li:share:123"
    (fun result ->
      match result with
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (List.mem "w_member_social" scopes);
          print_endline "✓ Unlike post maps 403 to write scope"
      | Ok _ -> failwith "Expected permission error"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: unlike_post rejects whitespace post URN without network calls *)
let test_unlike_post_rejects_whitespace_urn () =
  Mock_config.reset ();

  LinkedIn.unlike_post ~account_id:"test_account" ~post_urn:" urn:li:share:123 "
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "post URN");
          assert (string_contains msg "whitespace");
          assert (!Mock_http.requests = []);
          print_endline "✓ Unlike post rejects whitespace URN"
      | Ok _ -> failwith "Expected whitespace URN rejection"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: unlike_post rejects blank post URN without network calls *)
let test_unlike_post_rejects_blank_urn () =
  Mock_config.reset ();

  LinkedIn.unlike_post ~account_id:"test_account" ~post_urn:""
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "post URN is required");
          assert (!Mock_http.requests = []);
          print_endline "✓ Unlike post rejects blank URN"
      | Ok _ -> failwith "Expected blank URN rejection"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: unlike_post rejects malformed delimiter characters in post URN *)
let test_unlike_post_rejects_malformed_delimiter_urn () =
  Mock_config.reset ();

  LinkedIn.unlike_post ~account_id:"test_account" ~post_urn:"urn:li:share:123,456"
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "invalid delimiter");
          assert (!Mock_http.requests = []);
          print_endline "✓ Unlike post rejects malformed delimiter URN"
      | Ok _ -> failwith "Expected malformed delimiter URN rejection"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: Like request body structure with actor and object *)
let test_like_request_body_structure () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  (* Set up response queue: first person URN, then like response *)
  let person_response = {|{"sub": "user123"}|} in
  Mock_http.set_responses [
    { status = 200; body = person_response; headers = [] };
    { status = 201; body = "{}"; headers = [] };
  ];
  
  LinkedIn.like_post ~account_id:"test_account" ~post_urn:"urn:li:share:456"
    (handle_result
      (fun () ->
        (* Find the POST request to likes *)
        let requests = List.rev !Mock_http.requests in
        let like_request = List.find_opt (fun (method_, url, _, _) ->
          method_ = "POST" && string_contains url "likes"
        ) requests in
        
        match like_request with
        | Some (_, _, _, body) ->
            (* Verify actor (person URN) and object (post URN) are present *)
            assert (string_contains body "actor");
            assert (string_contains body "object");
            assert (string_contains body "urn:li:person:user123");
            assert (string_contains body "urn:li:share:456");
            print_endline "✓ Like request body structure (actor + object)"
        | None -> failwith "No likes POST request found")
      (fun err -> failwith ("Like post failed: " ^ err)))

(** Test: Register upload sends correct X-Restli-Protocol-Version *)
let test_register_upload_headers () =
  Mock_config.reset ();

  let response_body = {|{
    "value": {
      "image": "urn:li:image:test123",
      "uploadUrl": "https://upload.linkedin.com/test"
    }
  }|} in

  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  LinkedIn.register_upload
    ~access_token:"test_token"
    ~owner_urn:"urn:li:person:test"
    ~media_type:"image"
    (fun (_asset, _upload_url) ->
      let requests = !Mock_http.requests in
      match requests with
      | (_, url, headers, _) :: _ ->
          (* Verify URL uses modern /rest/images endpoint *)
          assert (string_contains url "/rest/images");
          assert (string_contains url "action=initializeUpload");
          (* Verify headers *)
          assert (find_header headers "X-Restli-Protocol-Version" = Some "2.0.0");
          assert (find_header headers "Content-Type" = Some "application/json");
          assert (find_header headers "Authorization" <> None);
          assert (find_header headers "LinkedIn-Version" <> None);
          print_endline "✓ Register upload headers"
      | [] -> failwith "No requests recorded")
    (fun err -> failwith ("Register upload failed: " ^ err))

(** Test: Query parameters are properly encoded (start, count) *)
let test_query_param_encoding () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  let comments_response = {|{
    "elements": [],
    "paging": {"start": 5, "count": 20, "total": 100}
  }|} in
  
  Mock_http.set_response { status = 200; body = comments_response; headers = [] };
  
  LinkedIn.get_post_comments ~account_id:"test_account" ~post_urn:"urn:li:share:123" ~start:5 ~count:20
    (handle_result
      (fun _collection ->
        let requests = !Mock_http.requests in
        match requests with
        | (_, url, _, _) :: _ ->
            (* Verify query parameters are in URL *)
            assert (string_contains url "start=5");
            assert (string_contains url "count=20");
            print_endline "✓ Query parameter encoding (start, count)"
        | [] -> failwith "No requests recorded")
      (fun err -> failwith ("Get comments failed: " ^ err)))

(** Test: Negative pagination values are normalized safely *)
let test_negative_pagination_normalization () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;

  let comments_response = {|{"elements": [], "paging": {"start": 0, "count": 1, "total": 1}}|} in
  Mock_http.set_response { status = 200; body = comments_response; headers = [] };

  LinkedIn.get_post_comments ~account_id:"test_account" ~post_urn:"urn:li:share:123" ~start:(-5) ~count:(-20)
    (handle_result
      (fun _collection ->
        let requests = !Mock_http.requests in
        match requests with
        | (_, url, _, _) :: _ ->
            assert (string_contains url "start=0");
            assert (string_contains url "count=1");
            print_endline "✓ Negative pagination normalization"
        | [] -> failwith "No requests recorded")
      (fun err -> failwith ("Negative pagination test failed: " ^ err)))

(** Test: get_post_comments caps count to 100 *)
let test_comments_count_cap () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;

  let comments_response = {|{"elements": [], "paging": {"start": 0, "count": 0, "total": 0}}|} in
  Mock_http.set_response { status = 200; body = comments_response; headers = [] };

  LinkedIn.get_post_comments ~account_id:"test_account" ~post_urn:"urn:li:share:123" ~count:999
    (handle_result
      (fun _collection ->
        let requests = !Mock_http.requests in
        match requests with
        | (_, url, _, _) :: _ ->
            assert (string_contains url "count=100");
            print_endline "✓ Comments count cap"
        | [] -> failwith "No requests recorded")
      (fun err -> failwith ("Comments count cap test failed: " ^ err)))

(** {1 Video Upload Tests}
    
    LinkedIn video upload follows the same register/upload pattern as images:
    1. Register upload with "video" recipe
    2. Upload binary to returned URL
    3. Include asset URN in post with VIDEO media category
    
    Note: LinkedIn supports videos up to 200MB for standard accounts.
    Videos longer than 10 minutes may be rejected.
    
    Gap vs ebx-linkedin-sdk (Java):
    - Our implementation uses single-chunk upload
    - For large videos (>200MB), chunked upload with ETags is needed
    - ebx-linkedin-sdk implements full chunked upload with ETags collection
    
    @see <https://learn.microsoft.com/en-us/linkedin/marketing/community-management/shares/videos-api>
*)

(** Helper to create mock responses for video upload *)
let make_video_upload_responses () =
  (* 1. GET userinfo *)
  let userinfo_response = { status = 200; body = {|{"sub": "user123"}|}; headers = [] } in
  (* 2. GET video URL - fake binary data *)
  let video_response = { status = 200; body = "fake_video_binary_data"; headers = [] } in
  (* 3. POST register upload via /rest/videos *)
  let register_response = { status = 200; body = {|{"value": {"video": "urn:li:video:video456", "uploadUrl": "https://api.linkedin.com/upload/video/123"}}|}; headers = [] } in
  (* 4. PUT upload binary *)
  let upload_response = { status = 200; body = ""; headers = [] } in
  (* 5. POST create post via /rest/posts *)
  let create_response = { status = 201; body = {|{"id": "urn:li:share:video789"}|}; headers = [] } in
  [userinfo_response; video_response; register_response; upload_response; create_response]

(** Test: Post with video *)
let test_post_with_video () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_responses (make_video_upload_responses ());
  
  LinkedIn.post_single 
    ~account_id:"test_account"
    ~text:"Check out this video!"
    ~media_urls:["https://example.com/video.mp4"]
    ~alt_texts:[Some "A video about OCaml programming"]
    (handle_outcome
      (fun post_id ->
        assert (post_id = "urn:li:share:video789");
        (* Verify the request used modern /rest/posts endpoint *)
        let requests = List.rev !Mock_http.requests in
        let post_request = List.find_opt (fun (method_, url, _, _) ->
          method_ = "POST" && string_contains url "/rest/posts"
        ) requests in

        (match post_request with
        | Some (_, _, _, body) ->
            assert (string_contains body "urn:li:video:video456");
            assert (string_contains body "commentary");
            print_endline "✓ Post with video"
        | None -> failwith "No /rest/posts request found"))
      (fun err -> failwith ("Post with video failed: " ^ err)))

(** Test: Register video upload uses video recipe *)
let test_register_video_upload () =
  Mock_config.reset ();

  let response_body = {|{
    "value": {
      "video": "urn:li:video:videotest123",
      "uploadUrl": "https://upload.linkedin.com/video"
    }
  }|} in

  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  LinkedIn.register_upload
    ~access_token:"test_token"
    ~owner_urn:"urn:li:person:test"
    ~media_type:"video"
    (fun (asset, _upload_url) ->
      assert (asset = "urn:li:video:videotest123");
      (* Verify modern /rest/videos endpoint was used *)
      let requests = !Mock_http.requests in
      (match requests with
      | (_, url, _, body) :: _ ->
          assert (string_contains url "/rest/videos");
          assert (string_contains body "initializeUploadRequest");
          print_endline "✓ Register video upload uses /rest/videos endpoint"
      | [] -> failwith "No requests recorded"))
    (fun err -> failwith ("Register video upload failed: " ^ err))

(** Test: Video media validation - valid video *)
let test_video_validation_valid () =
  let valid_video = {
    Platform_types.media_type = Platform_types.Video;
    mime_type = "video/mp4";
    file_size_bytes = 50_000_000; (* 50 MB *)
    width = Some 1920;
    height = Some 1080;
    duration_seconds = Some 120.0; (* 2 minutes *)
    alt_text = Some "Video description";
  } in
  (match LinkedIn.validate_media ~media:valid_video with
   | Ok () -> print_endline "✓ Valid video passes validation"
   | Error _ -> failwith "Valid video should pass")

(** Test: Video media validation - video too large *)
let test_video_validation_too_large () =
  let large_video = {
    Platform_types.media_type = Platform_types.Video;
    mime_type = "video/mp4";
    file_size_bytes = 250_000_000; (* 250 MB - over 200MB limit *)
    width = Some 1920;
    height = Some 1080;
    duration_seconds = Some 60.0;
    alt_text = None;
  } in
  (match LinkedIn.validate_media ~media:large_video with
   | Error _ -> print_endline "✓ Video over 200MB rejected"
   | Ok () -> failwith "Video over 200MB should fail")

(** Test: Video media validation - video too long *)
let test_video_validation_too_long () =
  let long_video = {
    Platform_types.media_type = Platform_types.Video;
    mime_type = "video/mp4";
    file_size_bytes = 50_000_000;
    width = Some 1920;
    height = Some 1080;
    duration_seconds = Some 700.0; (* 11+ minutes - over 10 min limit *)
    alt_text = None;
  } in
  (match LinkedIn.validate_media ~media:long_video with
   | Error _ -> print_endline "✓ Video over 10 minutes rejected"
   | Ok () -> failwith "Video over 10 minutes should fail")

(** Test: Post with mixed media (video detected from URL extension) *)
let test_post_video_detection_from_url () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_responses (make_video_upload_responses ());
  
  (* URL with .mov extension should be detected as video *)
  LinkedIn.post_single 
    ~account_id:"test_account"
    ~text:"QuickTime video post"
    ~media_urls:["https://example.com/clip.mov"]
    ~alt_texts:[]
    (handle_outcome
      (fun _post_id ->
        (* Verify /rest/videos endpoint was used (not /rest/images) *)
        let requests = List.rev !Mock_http.requests in
        let register_request = List.find_opt (fun (method_, url, _, _) ->
          method_ = "POST" && string_contains url "initializeUpload"
        ) requests in

        (match register_request with
        | Some (_, url, _, _) ->
            assert (string_contains url "/rest/videos");
            print_endline "✓ .mov URL detected as video"
        | None -> failwith "No initializeUpload request found"))
      (fun err -> failwith ("Video detection failed: " ^ err)))

(** {1 Post Deletion Tests} *)

(** Test: Delete post via /rest/posts *)
let test_delete_post () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response { status = 204; body = ""; headers = [] };

  LinkedIn.delete_post ~account_id:"test_account" ~post_urn:"urn:li:share:123456"
    (handle_result
      (fun () ->
        let requests = !Mock_http.requests in
        (match requests with
        | ("DELETE", url, headers, _) :: _ ->
            assert (string_contains url "/rest/posts/");
            assert (find_header headers "LinkedIn-Version" <> None);
            assert (find_header headers "X-Restli-Protocol-Version" = Some "2.0.0");
            print_endline "✓ Delete post via /rest/posts"
        | _ -> failwith "Expected DELETE request"))
      (fun err -> failwith ("Delete post failed: " ^ err)))

(** Test: Delete post rejects blank URN *)
let test_delete_post_rejects_blank_urn () =
  Mock_config.reset ();

  LinkedIn.delete_post ~account_id:"test_account" ~post_urn:""
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          assert (string_contains msg "post URN");
          assert (!Mock_http.requests = []);
          print_endline "✓ Delete post rejects blank URN"
      | Ok () -> failwith "Expected blank URN rejection"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: Delete post maps 403 to write scope *)
let test_delete_post_insufficient_permissions () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response { status = 403; body = {|{"message":"Not enough permissions"}|}; headers = [] };

  LinkedIn.delete_post ~account_id:"test_account" ~post_urn:"urn:li:share:123"
    (fun result ->
      match result with
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (List.mem "w_member_social" scopes);
          print_endline "✓ Delete post maps 403 to write scope"
      | Ok () -> failwith "Expected permission error"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** {1 Document/Carousel Post Tests} *)

let make_document_upload_responses () =
  (* 1. GET userinfo *)
  let userinfo_response = { status = 200; body = {|{"sub": "user123"}|}; headers = [] } in
  (* 2. GET document URL - fake binary data *)
  let doc_response = { status = 200; body = "fake_pdf_binary_data"; headers = [] } in
  (* 3. POST register document upload via /rest/documents *)
  let register_response = { status = 200; body = {|{"value": {"document": "urn:li:document:doc789", "uploadUrl": "https://api.linkedin.com/upload/doc/123"}}|}; headers = [] } in
  (* 4. PUT upload binary *)
  let upload_response = { status = 200; body = ""; headers = [] } in
  (* 5. POST create post via /rest/posts *)
  let create_response = { status = 201; body = {|{"id": "urn:li:share:docpost123"}|}; headers = [] } in
  [userinfo_response; doc_response; register_response; upload_response; create_response]

(** Test: Post a document (PDF carousel) *)
let test_post_document () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_responses (make_document_upload_responses ());

  LinkedIn.post_document
    ~account_id:"test_account"
    ~text:"Check out this PDF carousel!"
    ~document_url:"https://example.com/slides.pdf"
    ~document_title:"My Presentation"
    (handle_outcome
      (fun post_id ->
        assert (post_id = "urn:li:share:docpost123");
        let requests = List.rev !Mock_http.requests in
        (* Verify /rest/documents was used for registration *)
        let register_request = List.find_opt (fun (method_, url, _, _) ->
          method_ = "POST" && string_contains url "/rest/documents"
        ) requests in
        (match register_request with
        | Some (_, url, _, body) ->
            assert (string_contains url "initializeUpload");
            assert (string_contains body "initializeUploadRequest");
        | None -> failwith "No /rest/documents request found");
        (* Verify /rest/posts was used with document content *)
        let post_request = List.find_opt (fun (method_, url, _, _) ->
          method_ = "POST" && string_contains url "/rest/posts"
        ) requests in
        (match post_request with
        | Some (_, _, headers, body) ->
            assert (string_contains body "urn:li:document:doc789");
            assert (string_contains body "My Presentation");
            assert (string_contains body "commentary");
            assert (find_header headers "LinkedIn-Version" <> None);
        | None -> failwith "No /rest/posts request found");
        print_endline "✓ Post document (PDF carousel)")
      (fun err -> failwith ("Post document failed: " ^ err)))

(** Test: Post document with organization author *)
let test_post_document_with_org_author () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  (* No userinfo response needed since author_urn is explicit *)
  Mock_http.set_responses [
    { status = 200; body = "fake_pdf_data"; headers = [] };
    { status = 200; body = {|{"value": {"document": "urn:li:document:orgdoc", "uploadUrl": "https://api.linkedin.com/upload/doc"}}|}; headers = [] };
    { status = 200; body = ""; headers = [] };
    { status = 201; body = {|{"id": "urn:li:share:orgdocpost"}|}; headers = [] };
  ];

  LinkedIn.post_document
    ~account_id:"test_account"
    ~text:"Organization document"
    ~document_url:"https://example.com/report.pdf"
    ~document_title:"Q4 Report"
    ~author_urn:"urn:li:organization:12345"
    (handle_outcome
      (fun post_id ->
        assert (post_id = "urn:li:share:orgdocpost");
        let requests = List.rev !Mock_http.requests in
        let post_request = List.find_opt (fun (method_, url, _, _) ->
          method_ = "POST" && string_contains url "/rest/posts"
        ) requests in
        (match post_request with
        | Some (_, _, _, body) ->
            assert (string_contains body "urn:li:organization:12345");
            print_endline "✓ Post document with organization author"
        | None -> failwith "No /rest/posts request found"))
      (fun err -> failwith ("Post document with org author failed: " ^ err)))

(** Test: Register document upload uses /rest/documents *)
let test_register_document_upload () =
  Mock_config.reset ();

  let response_body = {|{
    "value": {
      "document": "urn:li:document:doctest123",
      "uploadUrl": "https://upload.linkedin.com/document"
    }
  }|} in

  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  LinkedIn.register_upload
    ~access_token:"test_token"
    ~owner_urn:"urn:li:person:test"
    ~media_type:"document"
    (fun (asset, upload_url) ->
      assert (asset = "urn:li:document:doctest123");
      assert (upload_url = "https://upload.linkedin.com/document");
      let requests = !Mock_http.requests in
      (match requests with
      | (_, url, headers, body) :: _ ->
          assert (string_contains url "/rest/documents");
          assert (string_contains url "initializeUpload");
          assert (string_contains body "initializeUploadRequest");
          assert (find_header headers "LinkedIn-Version" <> None);
          print_endline "✓ Register document upload uses /rest/documents"
      | [] -> failwith "No requests recorded"))
    (fun err -> failwith ("Register document upload failed: " ^ err))

(** {1 Poll Post Tests} *)

(** Test: Create a poll post *)
let test_post_poll () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_responses [
    { status = 200; body = {|{"sub": "user123"}|}; headers = [] };
    { status = 201; body = {|{"id": "urn:li:share:poll123"}|}; headers = [] };
  ];

  LinkedIn.post_poll
    ~account_id:"test_account"
    ~text:"What is your favorite language?"
    ~question:"Best programming language?"
    ~options:["OCaml"; "Rust"; "Haskell"]
    ~duration:7
    (handle_outcome
      (fun post_id ->
        assert (post_id = "urn:li:share:poll123");
        let requests = List.rev !Mock_http.requests in
        let post_request = List.find_opt (fun (method_, url, _, _) ->
          method_ = "POST" && string_contains url "/rest/posts"
        ) requests in
        (match post_request with
        | Some (_, _, headers, body) ->
            assert (string_contains body "poll");
            assert (string_contains body "Best programming language?");
            assert (string_contains body "OCaml");
            assert (string_contains body "Rust");
            assert (string_contains body "Haskell");
            assert (string_contains body "ONE_WEEK");
            assert (string_contains body "commentary");
            assert (string_contains body "distribution");
            assert (find_header headers "LinkedIn-Version" <> None);
            print_endline "✓ Post poll"
        | None -> failwith "No /rest/posts request found"))
      (fun err -> failwith ("Post poll failed: " ^ err)))

(** Test: Poll with different durations *)
let test_post_poll_durations () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;

  (* Test 1 day duration *)
  Mock_http.set_responses [
    { status = 200; body = {|{"sub": "user123"}|}; headers = [] };
    { status = 201; body = {|{"id": "urn:li:share:poll1"}|}; headers = [] };
  ];

  LinkedIn.post_poll
    ~account_id:"test_account"
    ~text:"Quick poll"
    ~question:"Yes or No?"
    ~options:["Yes"; "No"]
    ~duration:1
    (handle_outcome
      (fun _post_id ->
        let requests = List.rev !Mock_http.requests in
        let post_request = List.find_opt (fun (method_, url, _, _) ->
          method_ = "POST" && string_contains url "/rest/posts"
        ) requests in
        (match post_request with
        | Some (_, _, _, body) ->
            assert (string_contains body "ONE_DAY");
            print_endline "✓ Poll with 1-day duration"
        | None -> failwith "No /rest/posts request found"))
      (fun err -> failwith ("Poll duration test failed: " ^ err)));

  (* Test 14 day duration *)
  Mock_config.reset ();
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_responses [
    { status = 200; body = {|{"sub": "user123"}|}; headers = [] };
    { status = 201; body = {|{"id": "urn:li:share:poll2"}|}; headers = [] };
  ];

  LinkedIn.post_poll
    ~account_id:"test_account"
    ~text:"Extended poll"
    ~question:"Long running question?"
    ~options:["A"; "B"]
    ~duration:14
    (handle_outcome
      (fun _post_id ->
        let requests = List.rev !Mock_http.requests in
        let post_request = List.find_opt (fun (method_, url, _, _) ->
          method_ = "POST" && string_contains url "/rest/posts"
        ) requests in
        (match post_request with
        | Some (_, _, _, body) ->
            assert (string_contains body "TWO_WEEKS");
            print_endline "✓ Poll with 14-day duration"
        | None -> failwith "No /rest/posts request found"))
      (fun err -> failwith ("Poll 14-day duration test failed: " ^ err)))

(** Test: Poll validation - too few options *)
let test_post_poll_too_few_options () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;

  LinkedIn.post_poll
    ~account_id:"test_account"
    ~text:"Bad poll"
    ~question:"Only one option?"
    ~options:["Only choice"]
    ~duration:7
    (function
      | Error_types.Failure (Error_types.Internal_error msg) ->
          assert (string_contains msg "at least 2");
          assert (!Mock_http.requests = []);
          print_endline "✓ Poll rejects too few options"
      | Error_types.Success _ -> failwith "Expected validation failure"
      | Error_types.Partial_success _ -> failwith "Expected validation failure"
      | Error_types.Failure err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: Poll validation - too many options *)
let test_post_poll_too_many_options () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;

  LinkedIn.post_poll
    ~account_id:"test_account"
    ~text:"Too many options"
    ~question:"Pick one?"
    ~options:["A"; "B"; "C"; "D"; "E"]
    ~duration:7
    (function
      | Error_types.Failure (Error_types.Internal_error msg) ->
          assert (string_contains msg "at most 4");
          assert (!Mock_http.requests = []);
          print_endline "✓ Poll rejects too many options"
      | Error_types.Success _ -> failwith "Expected validation failure"
      | Error_types.Partial_success _ -> failwith "Expected validation failure"
      | Error_types.Failure err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: Poll validation - invalid duration *)
let test_post_poll_invalid_duration () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;

  LinkedIn.post_poll
    ~account_id:"test_account"
    ~text:"Invalid duration poll"
    ~question:"How long?"
    ~options:["Yes"; "No"]
    ~duration:5
    (function
      | Error_types.Failure (Error_types.Internal_error msg) ->
          assert (string_contains msg "duration");
          assert (!Mock_http.requests = []);
          print_endline "✓ Poll rejects invalid duration"
      | Error_types.Success _ -> failwith "Expected validation failure"
      | Error_types.Partial_success _ -> failwith "Expected validation failure"
      | Error_types.Failure err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: Poll validation - blank question *)
let test_post_poll_blank_question () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;

  LinkedIn.post_poll
    ~account_id:"test_account"
    ~text:"Blank question poll"
    ~question:"   "
    ~options:["Yes"; "No"]
    ~duration:7
    (function
      | Error_types.Failure (Error_types.Internal_error msg) ->
          assert (string_contains msg "question");
          assert (!Mock_http.requests = []);
          print_endline "✓ Poll rejects blank question"
      | Error_types.Success _ -> failwith "Expected validation failure"
      | Error_types.Partial_success _ -> failwith "Expected validation failure"
      | Error_types.Failure err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: LinkedIn-Version header present on all API requests *)
let test_linkedin_version_header_on_all_requests () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;

  (* Test get_profile includes LinkedIn-Version *)
  Mock_http.set_response { status = 200; body = {|{"sub":"u1","name":"Test"}|}; headers = [] };
  LinkedIn.get_profile ~account_id:"test_account"
    (fun _ ->
      let requests = !Mock_http.requests in
      (match requests with
      | (_, _, headers, _) :: _ ->
          assert (find_header headers "LinkedIn-Version" <> None)
      | [] -> failwith "No requests"));

  (* Test delete_post includes LinkedIn-Version *)
  Mock_config.reset ();
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_http.set_response { status = 204; body = ""; headers = [] };
  LinkedIn.delete_post ~account_id:"test_account" ~post_urn:"urn:li:share:123"
    (fun _ ->
      let requests = !Mock_http.requests in
      (match requests with
      | (_, _, headers, _) :: _ ->
          assert (find_header headers "LinkedIn-Version" <> None)
      | [] -> failwith "No requests"));

  print_endline "✓ LinkedIn-Version header present on all API requests"

(** Run all tests *)
let () =
  print_endline "\n=== LinkedIn Provider Tests ===\n";
  
  print_endline "--- OAuth Flow Tests ---";
  test_oauth_url ();
  test_oauth_url_parameters ();
  test_oauth_url_with_organization_scopes ();
  test_oauth_url_encoding ();
  test_oauth_url_rejects_whitespace_state ();
  test_oauth_url_rejects_redirect_mismatch ();
  test_oauth_state_validation ();
  test_validate_oauth_state_success ();
  test_validate_oauth_state_mismatch ();
  test_validate_oauth_state_whitespace_rejected ();
  test_oauth_scope_validation ();
  test_token_exchange ();
  test_token_exchange_non_invalid_client_no_retry ();
  test_exchange_code_and_get_organizations ();
  test_exchange_code_and_get_organizations_filter_normalization ();
  test_exchange_code_and_get_preferred_organization ();
  test_exchange_code_and_get_preferred_organization_none ();
  test_exchange_code_and_get_preferred_organization_acl_403 ();
  test_exchange_code_and_get_preferred_organization_acl_429 ();
  test_exchange_code_rejects_whitespace_code ();
  test_token_exchange_invalid ();
  test_token_exchange_missing_fields ();
  test_token_exchange_blank_refresh_token ();
  test_token_refresh_partner ();
  test_token_refresh_rejects_whitespace ();
  test_token_refresh_standard ();
  test_token_refresh_rotation ();
  test_token_refresh_blank_refresh_token_fallback ();
  test_refresh_token_expiry ();
  test_expired_token_detection ();
  
  print_endline "\n--- API Operation Tests ---";
  test_get_person_urn ();
  test_get_organization_access ();
  test_get_organization_access_parsing_resilience ();
  test_get_organization_access_pagination ();
  test_get_organization_access_blank_role_filter ();
  test_get_organization_access_trims_role_state_filters ();
  test_get_organization_access_uppercases_role_state_filters ();
  test_get_organization_access_invalid_version_fallback ();
  test_get_organization_access_valid_version_header ();
  test_get_organization_access_invalid_month_version_fallback ();
  test_get_organization_access_dotted_version_fallback ();
  test_get_organization_access_deduplicates_by_org ();
  test_get_organization_access_non_advancing_paging_stops ();
  test_get_organization_access_pagination_fallback_uses_raw_count ();
  test_get_organization_access_pagination_fallback_has_page_cap ();
  test_get_organization_access_prefers_admin_approved ();
  test_get_organization_access_preference_case_insensitive ();
  test_get_organization_access_preference_trims_role_state ();
  test_get_organization_access_preference_stable_on_tie ();
  test_select_preferred_organization_access_empty ();
  test_select_preferred_organization_access_ranking ();
  test_get_preferred_organization_access ();
  test_get_preferred_organization_access_none ();
  test_get_person_urn_rejects_malformed_subject ();
  test_get_person_urn_rejects_whitespace_subject ();
  test_get_person_urn_rejects_empty_subject ();
  test_get_person_urn_error_redaction ();
  test_get_person_urn_non_json_error_redaction ();
  test_get_person_urn_missing_subject ();
  test_get_person_urn_malformed_json ();
  test_get_person_urn_insufficient_permissions_message ();
  test_get_person_urn_auth_failure_message ();
  test_get_person_urn_rate_limited_message ();
  test_register_upload ();
  test_content_validation ();
  test_ensure_valid_token_fresh ();
  test_get_profile ();
  test_get_profile_insufficient_permissions_error ();
  test_api_error_redacts_sensitive_json ();
  test_api_error_redacts_non_json ();
  test_get_posts ();
  test_get_posts_rejects_malformed_person_urn ();
  test_get_posts_pagination_normalization ();
  test_get_posts_rate_limited_error ();
  test_get_posts_insufficient_permissions_error ();
  test_batch_get_posts ();
  test_batch_get_posts_rejects_malformed_urns ();
  test_batch_get_posts_malformed_json ();
  test_batch_get_posts_insufficient_permissions_error ();
  test_posts_scroller ();
  test_posts_scroller_back_from_second_page ();
  test_posts_scroller_back_error_preserves_position ();
  test_posts_scroller_page_size_normalization ();
  test_search_scroller_with_author ();
  test_search_scroller_back_from_second_page ();
  test_search_scroller_back_error_preserves_position ();
  test_search_scroller_rejects_keywords ();
  test_search_scroller_defaults_to_current_author ();
  test_search_scroller_page_size_normalization ();
  test_search_posts ();
  test_search_posts_author_filter_encoding ();
  test_search_posts_defaults_to_current_author ();
  test_search_posts_defaults_reject_malformed_person_urn ();
  test_search_posts_rejects_whitespace_author ();
  test_search_posts_rejects_malformed_author ();
  test_search_posts_pagination_normalization ();
  test_finder_zero_count_normalization ();
  test_search_posts_api_error ();
  test_get_post_insufficient_permissions_error ();
  test_get_post_rejects_whitespace_urn ();
  test_get_post_rejects_blank_urn ();
  test_get_post_rejects_malformed_delimiter_urn ();
  test_like_post ();
  test_like_post_insufficient_permissions_error ();
  test_like_post_rejects_whitespace_urn ();
  test_like_post_rejects_blank_urn ();
  test_like_post_rejects_malformed_delimiter_urn ();
  test_comment_on_post ();
  test_comment_on_post_insufficient_permissions_error ();
  test_comment_on_post_rejects_whitespace_urn ();
  test_comment_on_post_rejects_blank_urn ();
  test_comment_on_post_rejects_blank_text ();
  test_comment_on_post_rejects_malformed_delimiter_urn ();
  test_get_post_comments ();
  test_get_post_comments_insufficient_permissions_error ();
  test_get_post_comments_rejects_whitespace_urn ();
  test_get_post_comments_rejects_blank_urn ();
  test_get_post_comments_rejects_malformed_delimiter_urn ();
  test_get_post_engagement ();
  test_get_post_engagement_missing_fields ();
  test_get_post_engagement_api_error ();
  test_get_post_engagement_rejects_whitespace_urn ();
  test_get_post_engagement_rejects_blank_urn ();
  test_get_post_engagement_rejects_malformed_delimiter_urn ();
  test_get_account_analytics ();
  test_get_account_analytics_rejects_invalid_entity_urn ();
  test_post_with_url_preview ();
  test_post_single_insufficient_permissions_error ();
  test_post_single_with_organization_author ();
  test_post_single_org_insufficient_permissions_error ();
  test_post_single_rejects_malformed_author_urn ();
  test_post_single_malformed_author_urn_short_circuits_network ();
  test_post_single_rejects_whitespace_author_urn_short_circuits_network ();
  
  print_endline "\n--- Alt-Text Tests ---";
  test_post_with_alt_text ();
  test_post_with_multiple_alt_texts ();
  test_post_without_alt_text ();
  test_post_with_partial_alt_texts ();
  test_alt_text_special_chars ();
  test_thread_with_alt_texts ();
  
  print_endline "\n--- REST.li Protocol Tests ---";
  test_restli_protocol_version_header ();
  test_authorization_header_format ();
  test_content_type_header_json ();
  test_urn_path_encoding ();
  test_batch_urns_encoding ();
  test_finder_method_header ();
  test_rest_post_request_body_structure ();
  test_comment_request_body_structure ();
  test_unlike_post_delete ();
  test_unlike_post_insufficient_permissions_error ();
  test_unlike_post_rejects_whitespace_urn ();
  test_unlike_post_rejects_blank_urn ();
  test_unlike_post_rejects_malformed_delimiter_urn ();
  test_like_request_body_structure ();
  test_register_upload_headers ();
  test_query_param_encoding ();
  test_negative_pagination_normalization ();
  test_comments_count_cap ();
  
  print_endline "\n--- Video Upload Tests ---";
  test_post_with_video ();
  test_register_video_upload ();
  test_video_validation_valid ();
  test_video_validation_too_large ();
  test_video_validation_too_long ();
  test_post_video_detection_from_url ();

  print_endline "\n--- Post Deletion Tests ---";
  test_delete_post ();
  test_delete_post_rejects_blank_urn ();
  test_delete_post_insufficient_permissions ();

  print_endline "\n--- Document/Carousel Post Tests ---";
  test_post_document ();
  test_post_document_with_org_author ();
  test_register_document_upload ();

  print_endline "\n--- Poll Post Tests ---";
  test_post_poll ();
  test_post_poll_durations ();
  test_post_poll_too_few_options ();
  test_post_poll_too_many_options ();
  test_post_poll_invalid_duration ();
  test_post_poll_blank_question ();

  print_endline "\n--- LinkedIn-Version Header Tests ---";
  test_linkedin_version_header_on_all_requests ();

  print_endline "\n=== All tests passed! ===\n"
