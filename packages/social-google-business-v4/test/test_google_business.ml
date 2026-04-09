(** Tests for Google Business Profile API v4 Provider *)

open Social_core
open Social_google_business_v4

(** Helper to check if string contains substring *)
let string_contains s substr =
  try
    ignore (Str.search_forward (Str.regexp_string substr) s 0);
    true
  with Not_found -> false

(** Mock HTTP client for testing *)
module Mock_http = struct
  let requests = ref []
  let response_queue = ref []

  let reset () =
    requests := [];
    response_queue := []

  let set_responses responses =
    response_queue := responses

  let get_next_response () =
    match !response_queue with
    | [] -> None
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
end

module GoogleBusiness = Make(Mock_config)

let set_valid_credentials ~account_id =
  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s 3600) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    auth_type = Bearer;
  } in
  Mock_config.set_credentials ~account_id ~credentials:creds

(** Test: OAuth URL generation with PKCE *)
let test_oauth_url () =
  Mock_config.reset ();
  Mock_config.set_env "GOOGLE_BUSINESS_CLIENT_ID" "test_client_id";

  let state = "test_state_123" in
  let redirect_uri = "https://example.com/callback" in
  let code_verifier = "test_verifier_1234567890" in

  GoogleBusiness.get_oauth_url ~redirect_uri ~state ~code_verifier
    (fun url ->
      assert (string_contains url "client_id=test_client_id");
      assert (string_contains url "state=test_state_123");
      assert (string_contains url "code_challenge");
      assert (string_contains url "code_challenge_method=S256");
      assert (string_contains url "access_type=offline");
      assert (string_contains url "business.manage");
      print_endline "PASS OAuth URL generation with PKCE")
    (fun err -> failwith ("OAuth URL failed: " ^ err))

(** Test: Token exchange *)
let test_token_exchange () =
  Mock_config.reset ();
  Mock_config.set_env "GOOGLE_BUSINESS_CLIENT_ID" "test_client";
  Mock_config.set_env "GOOGLE_BUSINESS_CLIENT_SECRET" "test_secret";

  let response_body = {|{
    "access_token": "new_access_token_123",
    "refresh_token": "refresh_token_456",
    "expires_in": 3600,
    "token_type": "Bearer"
  }|} in

  Mock_http.set_responses [{ status = 200; body = response_body; headers = [] }];

  GoogleBusiness.exchange_code
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    ~code_verifier:"test_verifier"
    (fun creds ->
      assert (creds.access_token = "new_access_token_123");
      assert (creds.refresh_token = Some "refresh_token_456");
      assert (creds.auth_type = Bearer);
      assert (creds.expires_at <> None);
      print_endline "PASS Token exchange")
    (fun err -> failwith ("Token exchange failed: " ^ err))

(** Test: Token refresh via ensure_valid_token *)
let test_token_refresh () =
  Mock_config.reset ();
  Mock_config.set_env "GOOGLE_BUSINESS_CLIENT_ID" "test_client";
  Mock_config.set_env "GOOGLE_BUSINESS_CLIENT_SECRET" "test_secret";

  (* Set up expired credentials so ensure_valid_token triggers a refresh *)
  let past_time =
    let now = Ptime_clock.now () in
    match Ptime.sub_span now (Ptime.Span.of_int_s 3600) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate past time"
  in
  let expired_creds = {
    Social_core.access_token = "expired_token";
    refresh_token = Some "old_refresh";
    expires_at = Some past_time;
    auth_type = Social_core.Bearer;
  } in
  Mock_config.set_credentials ~account_id:"test" ~credentials:expired_creds;

  let response_body = {|{
    "access_token": "refreshed_token",
    "expires_in": 3600
  }|} in

  Mock_http.set_responses [{ status = 200; body = response_body; headers = [] }];

  GoogleBusiness.ensure_valid_token
    ~account_id:"test"
    (fun access_token ->
      assert (access_token = "refreshed_token");
      print_endline "PASS Token refresh")
    (fun _err -> failwith "Token refresh failed")

(** Test: Scope verification *)
let test_scope_verification () =
  assert (List.mem "https://www.googleapis.com/auth/business.manage"
    Social_google_business_v4.OAuth.Scopes.all);
  assert (List.mem "https://www.googleapis.com/auth/userinfo.profile"
    Social_google_business_v4.OAuth.Scopes.all);
  assert (List.mem "https://www.googleapis.com/auth/userinfo.email"
    Social_google_business_v4.OAuth.Scopes.all);
  let ops_scopes = Social_google_business_v4.OAuth.Scopes.for_operations
    [Social_google_business_v4.OAuth.Scopes.Post] in
  assert (List.mem "https://www.googleapis.com/auth/business.manage" ops_scopes);
  print_endline "PASS Scope verification"

(** Test: Text too long validation *)
let test_validation_text_too_long () =
  let long_text = String.make 1501 'x' in
  (match GoogleBusiness.validate_content ~text:long_text with
   | Error msg when string_contains msg "1500" ->
       print_endline "PASS Text too long rejected"
   | _ -> failwith "Long text should fail")

(** Test: Valid post *)
let test_validation_valid_post () =
  (match GoogleBusiness.validate_content ~text:"Check out our new menu!" with
   | Ok () -> print_endline "PASS Valid post accepted"
   | Error e -> failwith ("Valid post rejected: " ^ e))

(** Test: Empty text validation *)
let test_validation_empty_text () =
  (match GoogleBusiness.validate_content ~text:"" with
   | Error _ -> print_endline "PASS Empty text rejected"
   | Ok () -> failwith "Empty text should fail")

(** Test: Standard post (text only) *)
let test_post_standard_text_only () =
  Mock_config.reset ();
  set_valid_credentials ~account_id:"test_account";
  Mock_config.set_env "GOOGLE_BUSINESS_LOCATION_NAME" "locations/12345";

  let response_body = {|{
    "name": "locations/12345/localPosts/post_abc",
    "topicType": "STANDARD",
    "summary": "Hello from our business!"
  }|} in

  Mock_http.set_responses [{ status = 200; body = response_body; headers = [] }];

  GoogleBusiness.post_single
    ~account_id:"test_account"
    ~location_name:"locations/12345"
    ~text:"Hello from our business!"
    ~media_urls:[]
    (fun outcome ->
      match outcome with
      | Error_types.Success post_name ->
          assert (post_name = "locations/12345/localPosts/post_abc");
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [("POST", url, headers, body)] ->
               assert (string_contains url "mybusiness.googleapis.com/v4/locations/12345/localPosts");
               assert (List.assoc_opt "Authorization" headers = Some "Bearer valid_token");
               assert (List.assoc_opt "Content-Type" headers = Some "application/json");
               assert (string_contains body "Hello from our business!");
               assert (string_contains body "STANDARD");
               print_endline "PASS Standard post (text only)"
           | _ -> failwith "Expected exactly one POST request")
      | Error_types.Failure err ->
          failwith ("Post failed: " ^ Error_types.error_to_string err)
      | _ -> failwith "Unexpected outcome")

(** Test: Post with image *)
let test_post_with_image () =
  Mock_config.reset ();
  set_valid_credentials ~account_id:"test_account";
  Mock_config.set_env "GOOGLE_BUSINESS_LOCATION_NAME" "locations/12345";

  let response_body = {|{
    "name": "locations/12345/localPosts/post_img",
    "topicType": "STANDARD"
  }|} in

  Mock_http.set_responses [{ status = 200; body = response_body; headers = [] }];

  GoogleBusiness.post_single
    ~account_id:"test_account"
    ~location_name:"locations/12345"
    ~text:"Check out our new dish!"
    ~media_urls:["https://example.com/photo.jpg"]
    (fun outcome ->
      match outcome with
      | Error_types.Success _ ->
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [("POST", _, _, body)] ->
               assert (string_contains body "sourceUrl");
               assert (string_contains body "https://example.com/photo.jpg");
               assert (string_contains body "PHOTO");
               print_endline "PASS Post with image"
           | _ -> failwith "Expected exactly one POST request")
      | Error_types.Failure err ->
          failwith ("Post with image failed: " ^ Error_types.error_to_string err)
      | _ -> failwith "Unexpected outcome")

(** Test: Error handling - 401 auth error *)
let test_error_401_auth () =
  Mock_config.reset ();
  set_valid_credentials ~account_id:"test_account";
  Mock_config.set_env "GOOGLE_BUSINESS_LOCATION_NAME" "locations/12345";

  Mock_http.set_responses [{ status = 401; body = {|{"error":{"code":401,"message":"Token expired"}}|}; headers = [] }];

  GoogleBusiness.post_single
    ~account_id:"test_account"
    ~location_name:"locations/12345"
    ~text:"Test post"
    ~media_urls:[]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Auth_error Error_types.Token_invalid) ->
          print_endline "PASS 401 auth error handled"
      | Error_types.Failure _ ->
          print_endline "PASS 401 auth error handled (generic)"
      | _ -> failwith "Expected auth error")

(** Test: Error handling - 429 rate limit *)
let test_error_429_rate_limit () =
  Mock_config.reset ();
  set_valid_credentials ~account_id:"test_account";
  Mock_config.set_env "GOOGLE_BUSINESS_LOCATION_NAME" "locations/12345";

  Mock_http.set_responses [{ status = 429; body = {|{"error":{"code":429,"message":"Rate limit exceeded"}}|}; headers = [] }];

  GoogleBusiness.post_single
    ~account_id:"test_account"
    ~location_name:"locations/12345"
    ~text:"Test post"
    ~media_urls:[]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Rate_limited _) ->
          print_endline "PASS 429 rate limit handled"
      | _ -> failwith "Expected rate limit error")

(** Test: Error handling - 400 bad request *)
let test_error_400_bad_request () =
  Mock_config.reset ();
  set_valid_credentials ~account_id:"test_account";
  Mock_config.set_env "GOOGLE_BUSINESS_LOCATION_NAME" "locations/12345";

  Mock_http.set_responses [{ status = 400; body = {|{"error":{"code":400,"message":"Bad request"}}|}; headers = [] }];

  GoogleBusiness.post_single
    ~account_id:"test_account"
    ~location_name:"locations/12345"
    ~text:"Test post"
    ~media_urls:[]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Api_error { status_code = 400; _ }) ->
          print_endline "PASS 400 bad request handled"
      | _ -> failwith "Expected API error")

(** Test: Error handling - 500 server error *)
let test_error_500_server () =
  Mock_config.reset ();
  set_valid_credentials ~account_id:"test_account";
  Mock_config.set_env "GOOGLE_BUSINESS_LOCATION_NAME" "locations/12345";

  Mock_http.set_responses [{ status = 500; body = {|{"error":{"code":500,"message":"Internal error"}}|}; headers = [] }];

  GoogleBusiness.post_single
    ~account_id:"test_account"
    ~location_name:"locations/12345"
    ~text:"Test post"
    ~media_urls:[]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Api_error { status_code = 500; _ }) ->
          print_endline "PASS 500 server error handled"
      | _ -> failwith "Expected server error")

(** Test: List accounts *)
let test_list_accounts () =
  Mock_config.reset ();
  set_valid_credentials ~account_id:"test_account";

  let response_body = {|{
    "accounts": [
      {"name": "accounts/123", "accountName": "My Business"},
      {"name": "accounts/456", "accountName": "Second Business"}
    ]
  }|} in

  Mock_http.set_responses [{ status = 200; body = response_body; headers = [] }];

  GoogleBusiness.list_accounts ~account_id:"test_account"
    (function
      | Ok json ->
          let open Yojson.Basic.Util in
          let accounts = json |> member "accounts" |> to_list in
          assert (List.length accounts = 2);
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [("GET", url, headers, _)] ->
               assert (string_contains url "mybusinessaccountmanagement.googleapis.com/v1/accounts");
               assert (List.assoc_opt "Authorization" headers = Some "Bearer valid_token");
               print_endline "PASS List accounts"
           | _ -> failwith "Expected one GET request")
      | Error err ->
          failwith ("List accounts failed: " ^ Error_types.error_to_string err))

(** Test: List locations *)
let test_list_locations () =
  Mock_config.reset ();
  set_valid_credentials ~account_id:"test_account";

  let response_body = {|{
    "locations": [
      {"name": "locations/loc1", "title": "Main Store"},
      {"name": "locations/loc2", "title": "Branch Office"}
    ]
  }|} in

  Mock_http.set_responses [{ status = 200; body = response_body; headers = [] }];

  GoogleBusiness.list_locations ~account_id:"test_account" ~account_name:"accounts/123"
    (function
      | Ok json ->
          let open Yojson.Basic.Util in
          let locations = json |> member "locations" |> to_list in
          assert (List.length locations = 2);
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [("GET", url, _, _)] ->
               assert (string_contains url "mybusinessbusinessinformation.googleapis.com/v1/accounts/123/locations");
               print_endline "PASS List locations"
           | _ -> failwith "Expected one GET request")
      | Error err ->
          failwith ("List locations failed: " ^ Error_types.error_to_string err))

(** Test: Ensure valid token - fresh *)
let test_ensure_valid_token_fresh () =
  Mock_config.reset ();
  set_valid_credentials ~account_id:"test_account";

  GoogleBusiness.ensure_valid_token ~account_id:"test_account"
    (fun token ->
      assert (token = "valid_token");
      print_endline "PASS Ensure valid token (fresh)")
    (fun err -> failwith ("Ensure valid token failed: " ^ Error_types.error_to_string err))

(** Test: Ensure valid token - expired, auto-refresh *)
let test_ensure_valid_token_expired () =
  Mock_config.reset ();
  Mock_config.set_env "GOOGLE_BUSINESS_CLIENT_ID" "test_client";
  Mock_config.set_env "GOOGLE_BUSINESS_CLIENT_SECRET" "test_secret";

  let past_time =
    let now = Ptime_clock.now () in
    match Ptime.sub_span now (Ptime.Span.of_int_s 100) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate past time"
  in

  let creds = {
    access_token = "expired_token";
    refresh_token = Some "refresh_token";
    expires_at = Some past_time;
    auth_type = Bearer;
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;

  let response_body = {|{
    "access_token": "refreshed_token",
    "expires_in": 3600
  }|} in

  Mock_http.set_responses [{ status = 200; body = response_body; headers = [] }];

  GoogleBusiness.ensure_valid_token ~account_id:"test_account"
    (fun token ->
      assert (token = "refreshed_token");
      print_endline "PASS Ensure valid token (auto-refresh)")
    (fun err -> failwith ("Ensure valid token failed: " ^ Error_types.error_to_string err))

(** Test: Missing location name *)
let test_missing_location_name () =
  Mock_config.reset ();
  set_valid_credentials ~account_id:"test_account";
  (* Don't set GOOGLE_BUSINESS_LOCATION_NAME *)

  GoogleBusiness.post_single
    ~account_id:"test_account"
    ~location_name:""
    ~text:"Test post"
    ~media_urls:[]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Internal_error msg) ->
          assert (string_contains msg "location name");
          print_endline "PASS Missing location name handled"
      | _ -> failwith "Expected error for missing location name")

(** Test: Build post body with event *)
let test_build_post_body_event () =
  let content : post_content = {
    summary = "Join us for our grand opening!";
    topic = Event;
    event = Some {
      title = "Grand Opening";
      schedule = {
        start_date = "2026-04-01";
        start_time = Some "10:00";
        end_date = Some "2026-04-01";
        end_time = Some "18:00";
      };
    };
    offer = None;
    call_to_action = Some { cta_type = Learn_more; url = Some "https://example.com/event" };
    media_url = None;
    language_code = Some "en";
  } in
  let body = GoogleBusiness.build_post_body content in
  let body_str = Yojson.Basic.to_string body in
  assert (string_contains body_str "EVENT");
  assert (string_contains body_str "Grand Opening");
  assert (string_contains body_str "LEARN_MORE");
  assert (string_contains body_str "https://example.com/event");
  assert (string_contains body_str "\"en\"");
  print_endline "PASS Build post body with event"

(** Test: Build post body with offer *)
let test_build_post_body_offer () =
  let content : post_content = {
    summary = "20% off this weekend!";
    topic = Offer;
    event = None;
    offer = Some {
      coupon_code = Some "SAVE20";
      redeem_online_url = Some "https://example.com/redeem";
      terms_conditions = Some "Valid this weekend only";
    };
    call_to_action = Some { cta_type = Get_offer; url = Some "https://example.com/offer" };
    media_url = None;
    language_code = None;
  } in
  let body = GoogleBusiness.build_post_body content in
  let body_str = Yojson.Basic.to_string body in
  assert (string_contains body_str "OFFER");
  assert (string_contains body_str "SAVE20");
  assert (string_contains body_str "https://example.com/redeem");
  assert (string_contains body_str "Valid this weekend only");
  assert (string_contains body_str "GET_OFFER");
  print_endline "PASS Build post body with offer"

(** Run all tests *)
let () =
  print_endline "\n=== Google Business Profile Provider Tests ===\n";

  print_endline "--- OAuth Tests ---";
  test_oauth_url ();
  test_token_exchange ();
  test_token_refresh ();
  test_scope_verification ();

  print_endline "";
  print_endline "--- Validation Tests ---";
  test_validation_text_too_long ();
  test_validation_valid_post ();
  test_validation_empty_text ();

  print_endline "";
  print_endline "--- Posting Tests ---";
  test_post_standard_text_only ();
  test_post_with_image ();
  test_missing_location_name ();

  print_endline "";
  print_endline "--- Post Body Builder Tests ---";
  test_build_post_body_event ();
  test_build_post_body_offer ();

  print_endline "";
  print_endline "--- Error Handling Tests ---";
  test_error_401_auth ();
  test_error_429_rate_limit ();
  test_error_400_bad_request ();
  test_error_500_server ();

  print_endline "";
  print_endline "--- Location Discovery Tests ---";
  test_list_accounts ();
  test_list_locations ();

  print_endline "";
  print_endline "--- Token Management Tests ---";
  test_ensure_valid_token_fresh ();
  test_ensure_valid_token_expired ();

  print_endline "";
  print_endline "=== All tests passed! ===";
  print_endline "";
  print_endline "Test Coverage Summary:";
  print_endline "  - OAuth 2.0 with PKCE (4 tests)";
  print_endline "  - Content validation (3 tests)";
  print_endline "  - Posting contracts (3 tests)";
  print_endline "  - Post body building (2 tests)";
  print_endline "  - Error handling (4 tests)";
  print_endline "  - Location discovery (2 tests)";
  print_endline "  - Token management (2 tests)";
  print_endline "";
  print_endline "Total: 20 test functions"
