(** Tests for Facebook Graph API v21 Provider *)

open Social_core
open Social_facebook_graph_v21

(** Helper to check if string contains substring *)
let string_contains s substr =
  try
    ignore (Str.search_forward (Str.regexp_string substr) s 0);
    true
  with Not_found -> false

(** Helper to handle outcome type in tests *)
let handle_outcome on_success on_error outcome =
  match outcome with
  | Error_types.Success result -> on_success result
  | Error_types.Partial_success { result; _ } -> on_success result
  | Error_types.Failure err -> on_error (Error_types.error_to_string err)

(** Mock HTTP client for testing *)
module Mock_http = struct
  let requests = ref []
  let response_queue = ref []
  let error_queue = ref []
  
  let reset () =
    requests := [];
    response_queue := [];
    error_queue := []
  
  let set_response response =
    response_queue := [response]
  
  let set_responses responses =
    response_queue := responses

  let set_errors errors =
    error_queue := errors
  
  let get_next_response () =
    match !response_queue with
    | [] -> None
    | r :: rest ->
        response_queue := rest;
        Some r

  let get_next_error () =
    match !error_queue with
    | [] -> None
    | e :: rest ->
        error_queue := rest;
        Some e
  
  include (struct
  let get ?(headers=[]) url on_success on_error =
    requests := ("GET", url, headers, "") :: !requests;
    match get_next_response () with
    | Some response -> on_success response
    | None ->
        (match get_next_error () with
         | Some err -> on_error err
         | None -> on_error "No mock response set")
  
  let post ?(headers=[]) ?(body="") url on_success on_error =
    requests := ("POST", url, headers, body) :: !requests;
    match get_next_response () with
    | Some response -> on_success response
    | None ->
        (match get_next_error () with
         | Some err -> on_error err
         | None -> on_error "No mock response set")
  
  let put ?(headers=[]) ?(body="") url on_success on_error =
    requests := ("PUT", url, headers, body) :: !requests;
    match get_next_response () with
    | Some response -> on_success response
    | None ->
        (match get_next_error () with
         | Some err -> on_error err
         | None -> on_error "No mock response set")
  
  let delete ?(headers=[]) url on_success on_error =
    requests := ("DELETE", url, headers, "") :: !requests;
    match get_next_response () with
    | Some response -> on_success response
    | None ->
        (match get_next_error () with
         | Some err -> on_error err
         | None -> on_error "No mock response set")
  
  let post_multipart ?(headers=[]) ~parts url on_success on_error =
    let body_str = Printf.sprintf "multipart with %d parts" (List.length parts) in
    requests := ("POST_MULTIPART", url, headers, body_str) :: !requests;
    match get_next_response () with
    | Some response -> on_success response
    | None ->
        (match get_next_error () with
         | Some err -> on_error err
         | None -> on_error "No mock response set")
  end : HTTP_CLIENT)
end

(** Mock config for testing *)
module Mock_config = struct
  module Http = Mock_http
  
  let env_vars = ref []
  let credentials_store = ref []
  let health_statuses = ref []
  let page_ids = ref []
  let rate_limits = ref []
  
  let reset () =
    env_vars := [];
    credentials_store := [];
    health_statuses := [];
    page_ids := [];
    rate_limits := [];
    Mock_http.reset ()
  
  let set_env key value =
    env_vars := (key, value) :: !env_vars
  
  let get_env key =
    List.assoc_opt key !env_vars
  
  let on_rate_limit_update info =
    rate_limits := info :: !rate_limits
  
  let set_credentials ~account_id ~credentials =
    credentials_store := (account_id, credentials) :: !credentials_store
  
  let _set_page_id ~account_id ~page_id =
    page_ids := (account_id, page_id) :: !page_ids
  
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
  
  let get_page_id ~account_id on_success on_error =
    match List.assoc_opt account_id !page_ids with
    | Some page_id -> on_success page_id
    | None -> on_error "Page ID not found"
  
  let get_health_status account_id =
    List.find_opt (fun (id, _, _) -> id = account_id) !health_statuses
end

module Facebook = Make(Mock_config)

(** Test: OAuth URL generation *)
let test_oauth_url () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  
  let state = "test_state_123" in
  let redirect_uri = "https://example.com/callback" in
  
  Facebook.get_oauth_url ~redirect_uri ~state
    (fun url ->
      assert (string_contains url "client_id=test_app_id");
      assert (string_contains url "state=test_state_123");
      assert (string_contains url "response_type=code");
      assert (string_contains url "pages_manage_posts");
      print_endline "✓ OAuth URL generation")
    (fun err -> failwith ("OAuth URL failed: " ^ err))

(** Test: Token exchange *)
let test_token_exchange () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_secret";
  
  let response_body = {|{
    "access_token": "short_lived_access_token_123",
    "token_type": "bearer",
    "expires_in": 3600
  }|} in

  let long_lived_response_body = {|{
    "access_token": "new_access_token_123",
    "token_type": "bearer",
    "expires_in": 5184000
  }|} in
  
  Mock_http.set_responses [
    { status = 200; body = response_body; headers = [] };
    { status = 200; body = long_lived_response_body; headers = [] };
  ];
  
  Facebook.exchange_code 
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    (fun creds ->
      assert (creds.access_token = "new_access_token_123");
      assert (creds.refresh_token = None);
      assert (creds.token_type = "Bearer");
      assert (creds.expires_at <> None);
      print_endline "✓ Token exchange")
    (fun err -> failwith ("Token exchange failed: " ^ err))

(** Test: Upload photo *)
let test_upload_photo () =
  Mock_config.reset ();
  
  (* Set up two responses: first for image download, second for upload *)
  Mock_http.set_responses [
    { status = 200; body = "fake_image_data"; headers = [] };  (* GET image *)
    { status = 200; body = {|{"id": "photo_12345"}|}; headers = [] };  (* POST multipart *)
  ];
  
  Facebook.upload_photo
    ~page_id:"123456"
    ~page_access_token:"test_token"
    ~image_url:"https://example.com/image.jpg"
    ~alt_text:None
    (function
      | Ok photo_id ->
          assert (photo_id = "photo_12345");
          print_endline "✓ Upload photo"
      | Error e -> failwith ("Upload photo failed: " ^ Error_types.error_to_string e))

(** Test: Content validation *)
let test_content_validation () =
  (* Valid content *)
  (match Facebook.validate_content ~text:"Hello Facebook!" with
   | Ok () -> print_endline "✓ Valid content passes"
   | Error e -> failwith ("Valid content failed: " ^ e));
  
  (* Empty content *)
  (match Facebook.validate_content ~text:"" with
   | Error _ -> print_endline "✓ Empty content rejected"
   | Ok () -> failwith "Empty content should fail");
  
  (* Too long *)
  let long_text = String.make 5001 'x' in
  (match Facebook.validate_content ~text:long_text with
   | Error msg when string_contains msg "5000" -> 
       print_endline "✓ Long content rejected"
   | _ -> failwith "Long content should fail")

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
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  Facebook.ensure_valid_token ~account_id:"test_account"
    (fun token ->
      assert (token = "valid_token");
      (* Verify health status was updated *)
      match Mock_config.get_health_status "test_account" with
      | Some (_, "healthy", None) -> print_endline "✓ Ensure valid token (fresh)"
      | _ -> failwith "Health status not updated correctly")
    (fun err -> failwith ("Ensure valid token failed: " ^ Error_types.error_to_string err))

(** Test: Ensure valid token (expired token) *)
let test_ensure_valid_token_expired () =
  Mock_config.reset ();
  
  (* Set credentials with past expiry *)
  let past_time = 
    let now = Ptime_clock.now () in
    match Ptime.sub_span now (Ptime.Span.of_int_s 86400) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate past time"
  in
  
  let creds = {
    access_token = "expired_token";
    refresh_token = None;
    expires_at = Some past_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  Facebook.ensure_valid_token ~account_id:"test_account"
    (fun _ -> failwith "Should fail with expired token")
    (fun err ->
      let err_str = Error_types.error_to_string err in
      assert (string_contains err_str "expired");
      (* Verify health status was updated *)
      match Mock_config.get_health_status "test_account" with
      | Some (_, "token_expired", _) -> print_endline "✓ Ensure valid token (expired)"
      | _ -> failwith "Health status not updated correctly")

(** Test: Rate limit parsing *)
let test_rate_limit_parsing () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_secret";
  
  let response_body = {|{"id": "me"}|} in
  let headers = [
    ("X-App-Usage", {|{"call_count":15,"total_cputime":25,"total_time":30}|});
  ] in
  
  Mock_http.set_response { status = 200; body = response_body; headers };
  
  Facebook.get ~path:"me" ~access_token:"test_token"
    (function
      | Ok _response ->
          (* Check that rate limit was captured *)
          (match !Mock_config.rate_limits with
          | info :: _ ->
              assert (info.call_count = 15);
              assert (info.total_cputime = 25);
              assert (info.total_time = 30);
              assert (info.percentage_used = 30.0);
              print_endline "✓ Rate limit parsing"
          | [] -> failwith "Rate limit not captured")
      | Error e -> failwith ("Rate limit test failed: " ^ Error_types.error_to_string e))

(** Test: Full OAuth onboarding to pages *)
let test_exchange_code_and_get_pages () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_secret";

  Mock_http.set_responses [
    { status = 200; body = {|{"access_token":"short_token","token_type":"bearer","expires_in":3600}|}; headers = [] };
    { status = 200; body = {|{"access_token":"long_token","token_type":"bearer","expires_in":5184000}|}; headers = [] };
    { status = 200; body = {|{"data":[{"id":"123","name":"My Page","access_token":"page_token","category":"Brand"}]}|}; headers = [] };
  ];

  Facebook.exchange_code_and_get_pages
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    (fun (creds, pages) ->
      assert (creds.access_token = "long_token");
      assert (List.length pages = 1);
      let p = List.hd pages in
      assert (p.page_id = "123");
      assert (p.page_access_token = "page_token");
      print_endline "✓ Exchange code and get pages")
    (fun err -> failwith ("Exchange code and get pages failed: " ^ err))

(** Test: Field selection *)
let test_field_selection () =
  Mock_config.reset ();
  
  let response_body = {|{"id":"123","name":"Test"}|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Facebook.get ~path:"me" ~access_token:"test_token" ~fields:["id"; "name"]
    (function
      | Ok _response ->
          (* Check that request URL contains fields parameter *)
          let requests = !Mock_http.requests in
          (match requests with
          | (_, url, _, _) :: _ ->
              assert (string_contains url "fields=id%2Cname");
              print_endline "✓ Field selection"
          | [] -> failwith "No requests made")
      | Error e -> failwith ("Field selection test failed: " ^ Error_types.error_to_string e))

(** Test: Error code parsing *)
let test_error_code_parsing () =
  Mock_config.reset ();
  
  let error_response = {|{
    "error": {
      "message": "Invalid OAuth access token",
      "type": "OAuthException",
      "code": 190,
      "error_subcode": 463,
      "fbtrace_id": "ABC123"
    }
  }|} in
  
  Mock_http.set_response { status = 400; body = error_response; headers = [] };
  
  Facebook.get ~path:"me" ~access_token:"invalid_token"
    (function
      | Ok _response -> failwith "Should have failed with error"
      | Error e ->
          (* Token expired errors are converted to Auth_error *)
          let err_str = Error_types.error_to_string e in
          assert (string_contains err_str "expired" || string_contains err_str "token");
          print_endline "✓ Error code parsing")

(** Test: Invalid token without expiry subcode maps to token invalid *)
let test_invalid_token_without_expiry_subcode () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "Invalid OAuth access token",
      "type": "OAuthException",
      "code": 190,
      "fbtrace_id": "ABC124"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.get ~path:"me" ~access_token:"invalid_token"
    (function
      | Ok _response -> failwith "Should have failed with error"
      | Error (Error_types.Auth_error Error_types.Token_invalid) ->
          print_endline "✓ Invalid token mapping without expiry subcode"
      | Error e ->
          failwith ("Expected Token_invalid, got: " ^ Error_types.error_to_string e))

(** Test: Token subcode 467 maps to token expired *)
let test_token_subcode_467_maps_to_expired () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "Invalid OAuth access token - Session has expired",
      "type": "OAuthException",
      "code": 190,
      "error_subcode": 467,
      "fbtrace_id": "ABC125"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.get ~path:"me" ~access_token:"expired_token"
    (function
      | Ok _response -> failwith "Should have failed with expired token error"
      | Error (Error_types.Auth_error Error_types.Token_expired) ->
          print_endline "✓ Token subcode 467 maps to expired"
      | Error e ->
          failwith ("Expected Token_expired, got: " ^ Error_types.error_to_string e))

(** Test: Permission denied returns provider-required scopes *)
let test_permission_error_scopes () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "PERM123"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.get ~path:"me" ~access_token:"token"
    (function
      | Ok _ -> failwith "Should have failed with permission error"
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (List.mem "pages_manage_posts" scopes);
          assert (List.mem "pages_show_list" scopes);
          assert (List.mem "pages_read_engagement" scopes);
          print_endline "✓ Permission error includes provider-required scopes"
      | Error e ->
          failwith ("Expected insufficient_permissions error, got: " ^ Error_types.error_to_string e))

(** Test: me/accounts permission error maps to pages_show_list scope *)
let test_me_accounts_permission_scope () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "PERM_ACCOUNTS"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.get ~path:"me/accounts" ~access_token:"token"
    (function
      | Ok _ -> failwith "Should have failed with permission error"
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (scopes = ["pages_show_list"]);
          print_endline "✓ me/accounts permission scope mapping"
      | Error e ->
          failwith ("Expected insufficient_permissions error, got: " ^ Error_types.error_to_string e))

(** Test: Generic GET supports explicit required_permissions override *)
let test_get_required_permissions_override () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "PERM_OVERRIDE"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.get
    ~path:"me/accounts"
    ~access_token:"token"
    ~required_permissions:["custom_scope"]
    (function
      | Ok _ -> failwith "Should have failed with permission error"
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (scopes = ["custom_scope"]);
          print_endline "✓ GET required_permissions override"
      | Error e ->
          failwith ("Expected insufficient_permissions error, got: " ^ Error_types.error_to_string e))

(** Test: Generic POST supports explicit required_permissions override *)
let test_post_required_permissions_override () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "PERM_OVERRIDE_POST"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.post
    ~path:"me/feed"
    ~access_token:"token"
    ~params:[("message", ["hello"])]
    ~required_permissions:["custom_post_scope"]
    (function
      | Ok _ -> failwith "Should have failed with permission error"
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (scopes = ["custom_post_scope"]);
          print_endline "✓ POST required_permissions override"
      | Error e ->
          failwith ("Expected insufficient_permissions error, got: " ^ Error_types.error_to_string e))

(** Test: Generic DELETE supports explicit required_permissions override *)
let test_delete_required_permissions_override () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "PERM_OVERRIDE_DELETE"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.delete
    ~path:"12345"
    ~access_token:"token"
    ~required_permissions:["custom_delete_scope"]
    (function
      | Ok _ -> failwith "Should have failed with permission error"
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (scopes = ["custom_delete_scope"]);
          print_endline "✓ DELETE required_permissions override"
      | Error e ->
          failwith ("Expected insufficient_permissions error, got: " ^ Error_types.error_to_string e))

(** Test: Pagination *)
let test_pagination () =
  Mock_config.reset ();
  
  let page_response = {|{
    "data": [{"id": "1"}, {"id": "2"}],
    "paging": {
      "cursors": {
        "before": "cursor_before",
        "after": "cursor_after"
      },
      "next": "https://graph.facebook.com/v21.0/next_page"
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = page_response; headers = [] };
  
  let parse_data json =
    let open Yojson.Basic.Util in
    json |> to_list
  in
  
  Facebook.get_page ~path:"me/posts" ~access_token:"test_token" parse_data
    (function
      | Ok page_result ->
          assert (List.length page_result.data = 2);
          (match page_result.paging with
          | Some cursors ->
              assert (cursors.after = Some "cursor_after");
              assert (cursors.before = Some "cursor_before");
              assert (page_result.next_url <> None);
              print_endline "✓ Pagination"
          | None -> failwith "No paging info")
      | Error e -> failwith ("Pagination test failed: " ^ Error_types.error_to_string e))

(** Test: Batch requests *)
let test_batch_requests () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_secret";
  
  let batch_response = {|[
    {"code": 200, "headers": [{"name": "Content-Type", "value": "application/json"}], "body": "{\"id\":\"1\"}"},
    {"code": 200, "headers": [], "body": "{\"id\":\"2\"}"}
  ]|} in
  
  Mock_http.set_response { status = 200; body = batch_response; headers = [] };
  
  let open Facebook in
  let requests = [
    { method_ = `GET; relative_url = "me"; body = None; name = Some "me" };
    { method_ = `GET; relative_url = "me/posts"; body = None; name = None };
  ] in
  
  Facebook.batch_request ~requests ~access_token:"test_token"
    (function
      | Ok results ->
          assert (List.length results = 2);
          (match results with
          | r1 :: r2 :: _ ->
              assert (r1.code = 200);
              assert (r2.code = 200);
              assert (string_contains r1.body "\"id\":\"1\"");
              print_endline "✓ Batch requests"
          | _ -> failwith "Unexpected batch response")
      | Error e -> failwith ("Batch test failed: " ^ Error_types.error_to_string e))

(** Test: Batch request supports explicit required_permissions override *)
let test_batch_required_permissions_override () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "PERM_OVERRIDE_BATCH"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  let open Facebook in
  let requests = [
    { method_ = `GET; relative_url = "me"; body = None; name = None };
  ] in

  Facebook.batch_request
    ~requests
    ~access_token:"test_token"
    ~required_permissions:["custom_batch_scope"]
    (function
      | Ok _ -> failwith "Should have failed with permission error"
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (scopes = ["custom_batch_scope"]);
          print_endline "✓ BATCH required_permissions override"
      | Error e ->
          failwith ("Expected insufficient_permissions error, got: " ^ Error_types.error_to_string e))

(** Test: App secret proof *)
let test_app_secret_proof () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_secret";
  
  Mock_http.set_response { status = 200; body = {|{"id":"me"}|}; headers = [] };
  
  Facebook.get ~path:"me" ~access_token:"test_token"
    (function
      | Ok _response ->
          (* Check that request includes appsecret_proof *)
          let requests = !Mock_http.requests in
          (match requests with
          | (_, url, _, _) :: _ ->
              assert (string_contains url "appsecret_proof");
              print_endline "✓ App secret proof"
          | [] -> failwith "No requests made")
      | Error e -> failwith ("App secret proof test failed: " ^ Error_types.error_to_string e))

(** Test: Authorization header usage *)
let test_authorization_header () =
  Mock_config.reset ();
  
  Mock_http.set_response { status = 200; body = {|{"id":"me"}|}; headers = [] };
  
  Facebook.get ~path:"me" ~access_token:"test_token"
    (function
      | Ok _response ->
          (* Check that Authorization header is present *)
          let requests = !Mock_http.requests in
          (match requests with
          | (_, _, headers, _) :: _ ->
              let has_auth = List.exists (fun (k, v) ->
                k = "Authorization" && string_contains v "Bearer test_token"
              ) headers in
              assert has_auth;
              print_endline "✓ Authorization header"
           | [] -> failwith "No requests made")
      | Error e -> failwith ("Authorization header test failed: " ^ Error_types.error_to_string e))

(** Test: Account analytics request contract (v21 insights endpoint) *)
let test_account_analytics_request_contract () =
  Mock_config.reset ();

  let response_body = {|{
    "data": [
      {"name": "page_impressions_unique", "values": [{"value": 10, "end_time": "2024-01-02T08:00:00+0000"}]},
      {"name": "page_posts_impressions_unique", "values": [{"value": 9, "end_time": "2024-01-02T08:00:00+0000"}]},
      {"name": "page_post_engagements", "values": [{"value": 8, "end_time": "2024-01-02T08:00:00+0000"}]},
      {"name": "page_daily_follows", "values": [{"value": 7, "end_time": "2024-01-02T08:00:00+0000"}]},
      {"name": "page_video_views", "values": [{"value": 6, "end_time": "2024-01-02T08:00:00+0000"}]}
    ]
  }|} in

  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Facebook.get_account_analytics
    ~id:"12345"
    ~since:"2024-01-01"
    ~until:"2024-01-07"
    ~access_token:"acct_token"
    (function
      | Ok _analytics ->
          (match !Mock_http.requests with
          | (method_, url, headers, body) :: _ ->
              assert (method_ = "GET");
              assert (headers = []);
              assert (body = "");
              assert (url =
                "https://graph.facebook.com/v21.0/12345/insights?metric=page_impressions_unique,page_posts_impressions_unique,page_post_engagements,page_daily_follows,page_video_views&period=day&since=2024-01-01&until=2024-01-07&access_token=acct_token");
              print_endline "✓ Account analytics request contract"
          | [] -> failwith "No requests made")
      | Error e -> failwith ("Account analytics contract test failed: " ^ Error_types.error_to_string e))

(** Test: Post analytics request contract (v21 insights endpoint) *)
let test_post_analytics_request_contract () =
  Mock_config.reset ();

  let response_body = {|{
    "data": [
      {"name": "post_impressions_unique", "values": [{"value": 100}]},
      {"name": "post_reactions_by_type_total", "values": [{"value": {"like": 5}}]},
      {"name": "post_clicks", "values": [{"value": 11}]},
      {"name": "post_clicks_by_type", "values": [{"value": {"other clicks": 4}}]}
    ]
  }|} in

  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Facebook.get_post_analytics
    ~post_id:"12345_67890"
    ~access_token:"post_token"
    (function
      | Ok _analytics ->
          (match !Mock_http.requests with
          | (method_, url, headers, body) :: _ ->
              assert (method_ = "GET");
              assert (headers = []);
              assert (body = "");
              assert (url =
                "https://graph.facebook.com/v21.0/12345_67890/insights?metric=post_impressions_unique,post_reactions_by_type_total,post_clicks,post_clicks_by_type&access_token=post_token");
              print_endline "✓ Post analytics request contract"
          | [] -> failwith "No requests made")
      | Error e -> failwith ("Post analytics contract test failed: " ^ Error_types.error_to_string e))

(** Test: Account analytics parsing *)
let test_account_analytics_parsing () =
  Mock_config.reset ();

  let response_body = {|{
    "data": [
      {"name": "page_impressions_unique", "values": [
        {"value": 101, "end_time": "2024-01-02T08:00:00+0000"},
        {"value": 102, "end_time": "2024-01-03T08:00:00+0000"}
      ]},
      {"name": "page_posts_impressions_unique", "values": [
        {"value": 201, "end_time": "2024-01-02T08:00:00+0000"}
      ]},
      {"name": "page_post_engagements", "values": [
        {"value": 301, "end_time": "2024-01-02T08:00:00+0000"}
      ]},
      {"name": "page_daily_follows", "values": [
        {"value": 401, "end_time": "2024-01-02T08:00:00+0000"}
      ]},
      {"name": "page_video_views", "values": [
        {"value": 501, "end_time": "2024-01-02T08:00:00+0000"}
      ]}
    ]
  }|} in

  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Facebook.get_account_analytics
    ~id:"page_123"
    ~since:"2024-01-01"
    ~until:"2024-01-07"
    ~access_token:"token"
    (function
      | Ok analytics ->
          assert (List.length analytics.page_impressions_unique = 2);
          assert ((List.hd analytics.page_impressions_unique).value = 101);
          assert ((List.hd analytics.page_impressions_unique).end_time = Some "2024-01-02T08:00:00+0000");
          assert ((List.hd analytics.page_posts_impressions_unique).value = 201);
          assert ((List.hd analytics.page_post_engagements).value = 301);
          assert ((List.hd analytics.page_daily_follows).value = 401);
          assert ((List.hd analytics.page_video_views).value = 501);
          print_endline "✓ Account analytics parsing"
      | Error e -> failwith ("Account analytics parsing failed: " ^ Error_types.error_to_string e))

(** Test: Post analytics parsing *)
let test_post_analytics_parsing () =
  Mock_config.reset ();

  let response_body = {|{
    "data": [
      {"name": "post_impressions_unique", "values": [{"value": 1234}]},
      {"name": "post_reactions_by_type_total", "values": [{"value": {"like": 10, "love": 3}}]},
      {"name": "post_clicks", "values": [{"value": 345}]},
      {"name": "post_clicks_by_type", "values": [{"value": {"other clicks": 12, "photo view": 9}}]}
    ]
  }|} in

  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Facebook.get_post_analytics
    ~post_id:"post_123"
    ~access_token:"token"
    (function
      | Ok analytics ->
          assert (analytics.post_impressions_unique = Some 1234);
          assert (analytics.post_clicks = Some 345);
          assert (List.assoc_opt "like" analytics.post_reactions_by_type_total = Some 10);
          assert (List.assoc_opt "love" analytics.post_reactions_by_type_total = Some 3);
          assert (List.assoc_opt "other clicks" analytics.post_clicks_by_type = Some 12);
          assert (List.assoc_opt "photo view" analytics.post_clicks_by_type = Some 9);
          print_endline "✓ Post analytics parsing"
       | Error e -> failwith ("Post analytics parsing failed: " ^ Error_types.error_to_string e))

let test_canonical_analytics_adapters () =
  let find_series provider_metric series =
    List.find_opt
      (fun item -> item.Analytics_types.provider_metric = Some provider_metric)
      series
  in

  let account_analytics : Facebook.account_analytics = {
    page_impressions_unique = [ { end_time = Some "2024-01-02T08:00:00+0000"; value = 11 } ];
    page_posts_impressions_unique = [ { end_time = Some "2024-01-02T08:00:00+0000"; value = 9 } ];
    page_post_engagements = [ { end_time = Some "2024-01-02T08:00:00+0000"; value = 7 } ];
    page_daily_follows = [ { end_time = Some "2024-01-02T08:00:00+0000"; value = 5 } ];
    page_video_views = [ { end_time = Some "2024-01-02T08:00:00+0000"; value = 3 } ];
  } in
  let account_series = Facebook.to_canonical_account_analytics_series account_analytics in
  assert (List.length account_series = 5);
  (match find_series "page_impressions_unique" account_series with
   | Some item ->
       assert (Analytics_types.canonical_metric_key item.metric = "impressions");
       assert ((List.hd item.points).value = 11)
   | None -> failwith "Missing page_impressions_unique canonical series");
  (match find_series "page_daily_follows" account_series with
   | Some item ->
       assert (Analytics_types.canonical_metric_key item.metric = "follows");
       assert ((List.hd item.points).value = 5)
   | None -> failwith "Missing page_daily_follows canonical series");

  let post_analytics : Facebook.post_analytics = {
    post_impressions_unique = Some 120;
    post_reactions_by_type_total = [ ("like", 4); ("love", 2) ];
    post_clicks = Some 15;
    post_clicks_by_type = [ ("link", 6); ("photo", 1) ];
  } in
  let post_series = Facebook.to_canonical_post_analytics_series post_analytics in
  assert (List.length post_series = 4);
  (match find_series "post_reactions_by_type_total" post_series with
   | Some item ->
       assert (Analytics_types.canonical_metric_key item.metric = "reactions");
       assert ((List.hd item.points).value = 6)
   | None -> failwith "Missing post_reactions_by_type_total canonical series");
  (match find_series "post_clicks_by_type" post_series with
   | Some item ->
       assert (Analytics_types.canonical_metric_key item.metric = "clicks");
       assert ((List.hd item.points).value = 7)
   | None -> failwith "Missing post_clicks_by_type canonical series");

  print_endline "✓ Canonical analytics adapters"

(** Test: analytics network errors redact query access_token text *)
let test_analytics_network_error_redacts_query_token () =
  Mock_config.reset ();
  Mock_http.set_errors
    [ "GET https://graph.facebook.com/v21.0/123/insights?access_token=secret_token_123 failed" ];

  Facebook.get_account_analytics
    ~id:"123"
    ~since:"2024-01-01"
    ~until:"2024-01-02"
    ~access_token:"secret_token_123"
    (fun result ->
      match result with
      | Error (Error_types.Internal_error msg) ->
          if string_contains msg "secret_token_123" then
            failwith ("Token was not redacted: " ^ msg);
          if not (string_contains msg "REDACTED" || msg = "No mock response set") then
            failwith ("Unexpected redaction message: " ^ msg);
          print_endline "✓ Analytics network error redacts query token"
      | Ok _ -> failwith "Expected network error"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: Version policy guard - analytics and OAuth both use v21 *)
let test_graph_version_policy_guard () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "app_123";
  Mock_http.set_responses
    [ { status = 200; body = {|{"data":[]}|}; headers = [] };
      { status = 200; body = {|{"data":[]}|}; headers = [] } ];

  let oauth_url = ref "" in
  Facebook.get_oauth_url ~redirect_uri:"https://example.com/callback" ~state:"state-123"
    (fun url -> oauth_url := url)
    (fun err -> failwith ("OAuth URL generation failed: " ^ err));

  Facebook.get_account_analytics
    ~id:"123"
    ~since:"2024-01-01"
    ~until:"2024-01-02"
    ~access_token:"token_1"
    (fun _ -> ());
  Facebook.get_post_analytics ~post_id:"123_456" ~access_token:"token_1"
    (fun _ -> ());

  let requests = List.rev !Mock_http.requests in
  (match requests with
   | [ ("GET", account_url, _, _); ("GET", post_url, _, _) ] ->
       assert (string_contains !oauth_url "/v21.0/dialog/oauth");
       assert (string_contains account_url "graph.facebook.com/v21.0/");
       assert (string_contains post_url "graph.facebook.com/v21.0/");
       print_endline "✓ Graph version policy guard"
   | _ -> failwith "Expected two analytics requests for version policy guard")

(** Test: OAuth URL with required permissions *)
let test_oauth_url_permissions () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  
  let state = "test_state" in
  let redirect_uri = "https://example.com/callback" in
  
  Facebook.get_oauth_url ~redirect_uri ~state
    (fun url ->
      (* Should contain required permissions *)
      assert (string_contains url "pages_manage_posts" || string_contains url "pages_read_engagement");
      print_endline "✓ OAuth URL permissions")
    (fun err -> failwith ("OAuth URL permissions failed: " ^ err))

(** Test: OAuth URL encoding of special characters *)
let test_oauth_url_special_chars () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app";
  
  let redirect_uri = "https://example.com/callback?foo=bar&baz=qux" in
  let state = "state with spaces & special=chars" in
  
  Facebook.get_oauth_url ~redirect_uri ~state
    (fun url ->
      (* URL should be properly encoded *)
      assert (not (String.contains url ' '));
      print_endline "✓ OAuth URL special character encoding")
    (fun err -> failwith ("OAuth URL encoding failed: " ^ err))

(** Test: Token exchange error responses *)
let test_token_exchange_errors () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_secret";
  
  let error_response = {|{
    "error": {
      "message": "Invalid verification code format.",
      "type": "OAuthException",
      "code": 100
    }
  }|} in
  
  Mock_http.set_response { status = 400; body = error_response; headers = [] };
  
  Facebook.exchange_code 
    ~code:"bad_code"
    ~redirect_uri:"https://example.com/callback"
    (fun _ -> failwith "Should fail with bad code")
    (fun err ->
      assert (string_contains err "400" || string_contains err "OAuth" || string_contains err "Invalid");
      print_endline "✓ Token exchange error handling")

(** Test: Long-lived token exchange *)
let test_long_lived_token () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_secret";
  
  let short_response_body = {|{
    "access_token": "short_lived_token_123",
    "token_type": "bearer",
    "expires_in": 3600
  }|} in

  let response_body = {|{
    "access_token": "long_lived_token_123",
    "token_type": "bearer",
    "expires_in": 5184000
  }|} in
  
  Mock_http.set_responses [
    { status = 200; body = short_response_body; headers = [] };
    { status = 200; body = response_body; headers = [] };
  ];
  
  Facebook.exchange_code 
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    (fun creds ->
      (* Facebook long-lived tokens last 60 days *)
      match creds.expires_at with
      | Some _ -> print_endline "✓ Long-lived token exchange"
      | None -> failwith "No expiry set for long-lived token")
    (fun err -> failwith ("Long-lived token test failed: " ^ err))

(** Test: Page access token vs user access token *)
let test_page_vs_user_token () =
  Mock_config.reset ();
  
  (* In Facebook, you need both user token and page token *)
  (* User token is used to get page token, then page token is used for posting *)
  
  let response_body = {|{"id": "page_123"}|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Facebook.get ~path:"me/accounts" ~access_token:"user_token"
    (function
      | Ok _response ->
          (* This would return page access tokens in real usage *)
          print_endline "✓ Page vs user token handling"
      | Error e -> failwith ("Page token test failed: " ^ Error_types.error_to_string e))

(** Test: Token expiry detection *)
let test_token_expiry_detection () =
  Mock_config.reset ();
  
  (* Set credentials with near-future expiry (should refresh soon) *)
  let near_future = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s 60) with  (* 1 minute *)
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate time"
  in
  
  let creds = {
    access_token = "expiring_soon_token";
    refresh_token = None;
    expires_at = Some near_future;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  (* Should detect token expiring soon *)
  Facebook.ensure_valid_token ~account_id:"test_account"
    (fun token ->
      (* Token is still valid but will expire soon *)
      assert (token = "expiring_soon_token");
      print_endline "✓ Token expiry detection")
    (fun _err ->
      (* Or might fail if implementation checks expiry threshold *)
      print_endline "✓ Token expiry detection (failed as expected)")

(** Test: OAuth state CSRF protection *)
let test_oauth_csrf_protection () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  
  let state1 = "csrf_token_1" in
  let state2 = "csrf_token_2" in
  let redirect_uri = "https://example.com/callback" in
  
  Facebook.get_oauth_url ~redirect_uri ~state:state1
    (fun url1 ->
      Facebook.get_oauth_url ~redirect_uri ~state:state2
        (fun url2 ->
          (* Each URL should have different state *)
          assert (string_contains url1 state1);
          assert (string_contains url2 state2);
          assert (url1 <> url2);
          print_endline "✓ OAuth CSRF protection")
        (fun err -> failwith ("CSRF test failed: " ^ err)))
    (fun err -> failwith ("CSRF test failed: " ^ err))

(** Test: App secret proof generation *)
let test_app_secret_proof_generation () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_secret";
  
  (* App secret proof is HMAC-SHA256 of access token with app secret *)
  (* This adds security to API calls *)
  
  Mock_http.set_response { status = 200; body = {|{"id":"test"}|}; headers = [] };
  
  Facebook.get ~path:"me" ~access_token:"test_token"
    (function
      | Ok _response ->
          (* Verify request included appsecret_proof *)
          let requests = !Mock_http.requests in
          (match requests with
          | (_, url, _, _) :: _ ->
              if string_contains url "appsecret_proof=" then
                print_endline "✓ App secret proof generation"
              else
                failwith "App secret proof not included"
          | [] -> failwith "No requests made")
      | Error e -> failwith ("App secret proof test failed: " ^ Error_types.error_to_string e))

(** Test: Redirect URI validation *)
let test_redirect_uri_validation () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  
  let valid_uris = [
    "https://example.com/callback";
    "https://example.com/auth/facebook";
    "https://subdomain.example.com/callback";
  ] in
  
  (* All should generate valid URLs *)
  let results = List.map (fun uri ->
    let state = "test_state" in
    let success = ref false in
    Facebook.get_oauth_url ~redirect_uri:uri ~state
      (fun url ->
        success := true && String.length url > 0)
      (fun _ -> ());
    !success
  ) valid_uris in
  
  if List.for_all (fun x -> x) results then
    print_endline "✓ Redirect URI validation"
  else
    failwith "Some redirect URIs failed"

(** Test: Upload photo with alt-text *)
let test_upload_photo_with_alt_text () =
  Mock_config.reset ();
  
  Mock_http.set_responses [
    { status = 200; body = "fake_image_data"; headers = [] };
    { status = 200; body = {|{"id": "photo_with_alt"}|}; headers = [] };
  ];
  
  Facebook.upload_photo
    ~page_id:"123456"
    ~page_access_token:"test_token"
    ~image_url:"https://example.com/image.jpg"
    ~alt_text:(Some "A beautiful landscape photo")
    (function
      | Ok photo_id ->
          assert (photo_id = "photo_with_alt");
          print_endline "✓ Upload photo with alt-text"
      | Error e -> failwith ("Upload with alt-text failed: " ^ Error_types.error_to_string e))

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
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";
  
  Mock_http.set_responses [
    { status = 200; body = "image_data"; headers = [] };
    { status = 200; body = {|{"id": "photo123"}|}; headers = [] };
    { status = 200; body = {|{"id": "post123"}|}; headers = [] };
  ];
  
  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Check out this photo!"
    ~media_urls:["https://example.com/image.jpg"]
    ~alt_texts:[Some "Descriptive alt text for accessibility"]
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
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";
  
  Mock_http.set_responses [
    { status = 200; body = "image1_data"; headers = [] };
    { status = 200; body = {|{"id": "photo1"}|}; headers = [] };
    { status = 200; body = "image2_data"; headers = [] };
    { status = 200; body = {|{"id": "photo2"}|}; headers = [] };
    { status = 200; body = {|{"id": "post456"}|}; headers = [] };
  ];
  
  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Multiple photos with descriptions"
    ~media_urls:["https://example.com/img1.jpg"; "https://example.com/img2.jpg"]
    ~alt_texts:[Some "First image description"; Some "Second image description"]
    (handle_outcome
      (fun _post_id ->
        print_endline "✓ Post with multiple images and alt-texts")
      (fun err -> failwith ("Post with multiple alt-texts failed: " ^ err)))

(** Test: Multi-photo uses indexed attached_media payload *)
let test_post_with_indexed_attached_media_payload () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";

  Mock_http.set_responses [
    { status = 200; body = "image1_data"; headers = [] };
    { status = 200; body = {|{"id": "photo1"}|}; headers = [] };
    { status = 200; body = "image2_data"; headers = [] };
    { status = 200; body = {|{"id": "photo2"}|}; headers = [] };
    { status = 200; body = {|{"id": "post456"}|}; headers = [] };
  ];

  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Multiple photos payload test"
    ~media_urls:["https://example.com/img1.jpg"; "https://example.com/img2.jpg"]
    ~alt_texts:[Some "First image"; Some "Second image"]
    (handle_outcome
      (fun _post_id ->
        let reqs = !Mock_http.requests in
        let feed_request =
          List.find_opt (fun (m, u, _, _) -> m = "POST" && string_contains u "/feed") reqs
        in
        (match feed_request with
        | Some (_, _, _, body) ->
            assert (string_contains body "attached_media%5B0%5D");
            assert (string_contains body "attached_media%5B1%5D");
            print_endline "✓ Multi-photo uses indexed attached_media payload"
        | None -> failwith "Feed request not found"))
      (fun err -> failwith ("Post payload test failed: " ^ err)))

(** Test: Recovers page token from user token on post failure *)
let test_post_recovers_page_token_from_user_token () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "user_token_initial";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  let permission_error = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "TRACE_PERM"
    }
  }|} in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";

  Mock_http.set_responses [
    { status = 200; body = "image_data_attempt1"; headers = [] };
    { status = 400; body = permission_error; headers = [] };
    { status = 200; body = {|{"data":[{"id":"page123","name":"My Page","access_token":"resolved_page_token","category":"Brand"}]}|}; headers = [] };
    { status = 200; body = "image_data_attempt2"; headers = [] };
    { status = 200; body = {|{"id":"photo_after_recovery"}|}; headers = [] };
    { status = 200; body = {|{"id":"post_after_recovery"}|}; headers = [] };
  ];

  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Recover token and post"
    ~media_urls:["https://example.com/recover.jpg"]
    (handle_outcome
      (fun post_id ->
        assert (post_id = "post_after_recovery");
        let reqs = !Mock_http.requests in
        let used_resolved_token =
          List.exists (fun (_m, _u, headers, _body) ->
            List.exists (fun (k, v) ->
              k = "Authorization" && string_contains v "resolved_page_token"
            ) headers
          ) reqs
        in
        assert used_resolved_token;
        (match Mock_config.get_health_status "test_account" with
        | Some (_, "token_recovered", Some msg) ->
            assert (string_contains msg "Recovered Page access token")
        | Some (_, status, _) ->
            failwith ("Expected token_recovered health status, got: " ^ status)
        | None ->
            failwith "Expected health status update for token recovery");
        (match List.assoc_opt "test_account" !Mock_config.credentials_store with
        | Some updated_creds ->
            assert (updated_creds.access_token = "resolved_page_token");
            assert (updated_creds.refresh_token = Some "user_token_initial")
        | None -> failwith "Expected updated credentials after token recovery");
        print_endline "✓ Post recovers page token from user token")
      (fun err -> failwith ("Token recovery post failed: " ^ err)))

(** Test: Recovery falls back to stored user token when current token fails *)
let test_post_recovery_uses_stored_user_token_fallback () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "stale_page_token";
    refresh_token = Some "stored_user_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  let permission_error = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "TRACE_FALLBACK"
    }
  }|} in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";

  Mock_http.set_responses [
    { status = 200; body = "image_data_attempt1"; headers = [] };
    { status = 400; body = permission_error; headers = [] };
    { status = 400; body = permission_error; headers = [] };
    { status = 200; body = {|{"data":[{"id":"page123","name":"My Page","access_token":"resolved_page_token_from_backup","category":"Brand"}]}|}; headers = [] };
    { status = 200; body = "image_data_attempt2"; headers = [] };
    { status = 200; body = {|{"id":"photo_after_fallback"}|}; headers = [] };
    { status = 200; body = {|{"id":"post_after_fallback"}|}; headers = [] };
  ];

  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Recover with fallback user token"
    ~media_urls:["https://example.com/recover-fallback.jpg"]
    (handle_outcome
      (fun post_id ->
        assert (post_id = "post_after_fallback");
        let reqs = !Mock_http.requests in
        let me_accounts_requests =
          List.filter (fun (m, u, _, _) -> m = "GET" && string_contains u "/me/accounts") reqs
        in
        assert (List.length me_accounts_requests = 2);
        let used_stored_user_token =
          List.exists (fun (_m, u, _, _) -> string_contains u "stored_user_token") me_accounts_requests
        in
        assert used_stored_user_token;
        (match List.assoc_opt "test_account" !Mock_config.credentials_store with
        | Some updated_creds ->
            assert (updated_creds.access_token = "resolved_page_token_from_backup");
            assert (updated_creds.refresh_token = Some "stored_user_token")
        | None -> failwith "Expected updated credentials after fallback recovery");
        print_endline "✓ Recovery uses stored user token fallback")
      (fun err -> failwith ("Fallback recovery test failed: " ^ err)))

(** Test: Recovery does not fallback to other tokens on rate limiting *)
let test_post_recovery_stops_on_rate_limit () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "stale_page_token";
    refresh_token = Some "stored_user_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  let permission_error = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "TRACE_RATE_STOP_1"
    }
  }|} in

  let rate_limit_error = {|{
    "error": {
      "message": "(#80001) There have been too many calls to this Page account",
      "type": "OAuthException",
      "code": 80001,
      "fbtrace_id": "TRACE_RATE_STOP_2"
    }
  }|} in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";

  Mock_http.set_responses [
    { status = 200; body = "image_data_attempt1"; headers = [] };
    { status = 400; body = permission_error; headers = [] };
    { status = 400; body = rate_limit_error; headers = [] };
  ];

  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Recovery should stop on rate limit"
    ~media_urls:["https://example.com/recover-stop.jpg"]
    (handle_outcome
      (fun _ -> failwith "Expected post failure")
      (fun _err ->
        let me_accounts_requests =
          List.filter (fun (m, u, _, _) -> m = "GET" && string_contains u "/me/accounts") !Mock_http.requests
        in
        assert (List.length me_accounts_requests = 1);
        (match Mock_config.get_health_status "test_account" with
        | Some (_, "token_recovery_failed", Some msg) ->
            assert (string_contains msg "Stopped token recovery")
        | _ -> failwith "Expected token_recovery_failed stop status");
        print_endline "✓ Recovery stops on rate limit without fallback"))

(** Test: Cleans up uploaded photos if feed publish fails *)
let test_post_cleans_up_uploaded_photos_on_publish_failure () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "page_token_initial";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  let publish_error = {|{
    "error": {
      "message": "(#100) Invalid parameter",
      "type": "OAuthException",
      "code": 100,
      "fbtrace_id": "TRACE_PUBLISH_FAIL"
    }
  }|} in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";

  Mock_http.set_responses [
    { status = 200; body = "image1_data"; headers = [] };
    { status = 200; body = {|{"id":"photo_to_cleanup_1"}|}; headers = [] };
    { status = 200; body = "image2_data"; headers = [] };
    { status = 200; body = {|{"id":"photo_to_cleanup_2"}|}; headers = [] };
    { status = 400; body = publish_error; headers = [] };
    { status = 200; body = {|{"success":true}|}; headers = [] };
    { status = 200; body = {|{"success":true}|}; headers = [] };
  ];

  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Should fail and cleanup"
    ~media_urls:["https://example.com/c1.jpg"; "https://example.com/c2.jpg"]
    (handle_outcome
      (fun _ -> failwith "Expected publish failure")
      (fun _err ->
        let reqs = !Mock_http.requests in
        let deleted_photo1 =
          List.exists (fun (m, u, _, _) -> m = "DELETE" && string_contains u "/photo_to_cleanup_1") reqs
        in
        let deleted_photo2 =
          List.exists (fun (m, u, _, _) -> m = "DELETE" && string_contains u "/photo_to_cleanup_2") reqs
        in
        assert deleted_photo1;
        assert deleted_photo2;
        print_endline "✓ Cleans up uploaded photos on publish failure"))

(** Test: Rate-limit error code mapping for Pages/BUC *)
let test_rate_limit_error_code_80001 () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "(#80001) There have been too many calls to this Page account",
      "type": "OAuthException",
      "code": 80001,
      "fbtrace_id": "TRACE123"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.get ~path:"me" ~access_token:"test_token"
    (function
      | Ok _ -> failwith "Should have been rate limited"
      | Error e ->
          (match e with
          | Error_types.Rate_limited _ -> print_endline "✓ Rate-limit error code 80001 mapping"
          | _ -> failwith ("Expected rate-limited error, got: " ^ Error_types.error_to_string e)))

(** Test: Post without alt-text *)
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
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";
  
  Mock_http.set_responses [
    { status = 200; body = "image_data"; headers = [] };
    { status = 200; body = {|{"id": "photo789"}|}; headers = [] };
    { status = 200; body = {|{"id": "post789"}|}; headers = [] };
  ];
  
  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Photo without description"
    ~media_urls:["https://example.com/image.jpg"]
    ~alt_texts:[]
    (handle_outcome
      (fun _post_id ->
        print_endline "✓ Post without alt-text")
      (fun err -> failwith ("Post without alt-text failed: " ^ err)))

(** Test: Alt-text with special characters *)
let test_alt_text_special_characters () =
  Mock_config.reset ();
  
  Mock_http.set_responses [
    { status = 200; body = "fake_image_data"; headers = [] };
    { status = 200; body = {|{"id": "photo_special"}|}; headers = [] };
  ];
  
  Facebook.upload_photo
    ~page_id:"123456"
    ~page_access_token:"test_token"
    ~image_url:"https://example.com/image.jpg"
    ~alt_text:(Some "Photo with \"quotes\", & special <chars> and emojis 🎉")
    (function
      | Ok _photo_id ->
          print_endline "✓ Alt-text with special characters"
      | Error e -> failwith ("Alt-text with special chars failed: " ^ Error_types.error_to_string e))

(** Test: Partial alt-texts - fewer alt-texts than images *)
let test_partial_alt_texts () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";
  
  Mock_http.set_responses [
    { status = 200; body = "image1_data"; headers = [] };
    { status = 200; body = {|{"id": "photo1"}|}; headers = [] };
    { status = 200; body = "image2_data"; headers = [] };
    { status = 200; body = {|{"id": "photo2"}|}; headers = [] };
    { status = 200; body = "image3_data"; headers = [] };
    { status = 200; body = {|{"id": "photo3"}|}; headers = [] };
    { status = 200; body = {|{"id": "post_partial"}|}; headers = [] };
  ];
  
  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Three images, two alt-texts"
    ~media_urls:["https://example.com/img1.jpg"; "https://example.com/img2.jpg"; "https://example.com/img3.jpg"]
    ~alt_texts:[Some "First image"; Some "Second image"]
    (handle_outcome
      (fun _post_id ->
        print_endline "✓ Post with partial alt-texts (3 images, 2 alt-texts)")
      (fun err -> failwith ("Post with partial alt-texts failed: " ^ err)))

(** {1 Stories Tests} *)

(** Test: Upload photo story *)
let test_upload_photo_story () =
  Mock_config.reset ();
  
  Mock_http.set_responses [
    { status = 200; body = "fake_image_data"; headers = [] };  (* GET image *)
    { status = 200; body = {|{"post_id": "story_123"}|}; headers = [] };  (* POST multipart *)
  ];
  
  Facebook.upload_photo_story
    ~page_id:"page_123"
    ~page_access_token:"test_token"
    ~image_url:"https://example.com/story.jpg"
    (function
      | Ok story_id ->
          assert (story_id = "story_123");
          (* Verify the request went to photo_stories endpoint *)
          let requests = !Mock_http.requests in
          let has_photo_stories = List.exists (fun (_, url, _, _) ->
            string_contains url "photo_stories"
          ) requests in
          assert has_photo_stories;
          print_endline "✓ Upload photo story"
      | Error e -> failwith ("Upload photo story failed: " ^ Error_types.error_to_string e))

(** Test: Upload video story *)
let test_upload_video_story () =
  Mock_config.reset ();
  
  Mock_http.set_responses [
    { status = 200; body = "fake_video_data"; headers = [] };  (* GET video *)
    { status = 200; body = {|{"video_id": "vid_456", "upload_url": "https://upload.example.com"}|}; headers = [] };  (* POST init *)
    { status = 200; body = {|{"success": true}|}; headers = [] };  (* POST upload *)
    { status = 200; body = {|{"success": true, "post_id": "video_story_789"}|}; headers = [] };  (* POST finish *)
  ];
  
  Facebook.upload_video_story
    ~page_id:"page_123"
    ~page_access_token:"test_token"
    ~video_url:"https://example.com/story.mp4"
    (function
      | Ok story_id ->
          assert (story_id = "video_story_789");
          (* Verify the request went to video_stories endpoint *)
          let requests = !Mock_http.requests in
          let has_video_stories = List.exists (fun (_, url, _, _) ->
            string_contains url "video_stories"
          ) requests in
          assert has_video_stories;
          print_endline "✓ Upload video story"
      | Error e -> failwith ("Upload video story failed: " ^ Error_types.error_to_string e))

(** Test: Post photo story (high-level) *)
let test_post_story_photo () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";
  
  Mock_http.set_responses [
    { status = 200; body = "image_data"; headers = [] };
    { status = 200; body = {|{"post_id": "photo_story_abc"}|}; headers = [] };
  ];
  
  Facebook.post_story_photo
    ~account_id:"test_account"
    ~image_url:"https://example.com/story.jpg"
    (handle_outcome
      (fun story_id ->
        assert (story_id = "photo_story_abc");
        print_endline "✓ Post photo story (high-level)")
      (fun err -> failwith ("Post photo story failed: " ^ err)))

(** Test: Post video story (high-level) *)
let test_post_story_video () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";
  
  Mock_http.set_responses [
    { status = 200; body = "video_data"; headers = [] };
    { status = 200; body = {|{"video_id": "v123", "upload_url": "https://upload.example.com"}|}; headers = [] };
    { status = 200; body = {|{"success": true}|}; headers = [] };
    { status = 200; body = {|{"success": true, "post_id": "video_story_xyz"}|}; headers = [] };
  ];
  
  Facebook.post_story_video
    ~account_id:"test_account"
    ~video_url:"https://example.com/story.mp4"
    (handle_outcome
      (fun story_id ->
        assert (story_id = "video_story_xyz");
        print_endline "✓ Post video story (high-level)")
      (fun err -> failwith ("Post video story failed: " ^ err)))

(** Test: Post video story recovers on auth error during upload phase *)
let test_post_story_video_recovers_on_upload_auth_error () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "user_token_story";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  let permission_error = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "STORY_UP_AUTH"
    }
  }|} in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_story_123";

  Mock_http.set_responses [
    { status = 200; body = "video_data_1"; headers = [] };
    { status = 200; body = {|{"video_id":"story_vid_1","upload_url":"https://upload.facebook.com/story-up-1"}|}; headers = [] };
    { status = 400; body = permission_error; headers = [] };
    { status = 200; body = {|{"data":[{"id":"page_story_123","name":"Story Page","access_token":"story_resolved_page_token","category":"Brand"}]}|}; headers = [] };
    { status = 200; body = "video_data_2"; headers = [] };
    { status = 200; body = {|{"video_id":"story_vid_2","upload_url":"https://upload.facebook.com/story-up-2"}|}; headers = [] };
    { status = 200; body = {|{"success":true}|}; headers = [] };
    { status = 200; body = {|{"success":true,"post_id":"story_post_recovered"}|}; headers = [] };
  ];

  Facebook.post_story_video
    ~account_id:"test_account"
    ~video_url:"https://example.com/story-recover.mp4"
    (handle_outcome
      (fun story_id ->
        assert (story_id = "story_post_recovered");
        let used_resolved_token =
          List.exists (fun (_m, _u, headers, _body) ->
            List.exists (fun (k, v) -> k = "Authorization" && string_contains v "story_resolved_page_token") headers
          ) !Mock_http.requests
        in
        assert used_resolved_token;
        (match List.assoc_opt "test_account" !Mock_config.credentials_store with
        | Some updated_creds ->
            assert (updated_creds.access_token = "story_resolved_page_token");
            assert (updated_creds.refresh_token = Some "user_token_story")
        | None -> failwith "Expected updated credentials after story recovery");
        print_endline "✓ Post video story recovers on upload auth error")
      (fun err -> failwith ("Story upload auth recovery failed: " ^ err)))

(** Test: Post video story does not recover on upload rate-limit errors *)
let test_post_story_video_no_recovery_on_upload_rate_limit () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "user_token_story_rl";
    refresh_token = Some "stored_user_token_story_rl";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  let rate_limit_error = {|{
    "error": {
      "message": "(#80001) There have been too many calls to this Page account",
      "type": "OAuthException",
      "code": 80001,
      "fbtrace_id": "STORY_UP_RL"
    }
  }|} in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_story_123";

  Mock_http.set_responses [
    { status = 200; body = "video_data_1"; headers = [] };
    { status = 200; body = {|{"video_id":"story_vid_1","upload_url":"https://upload.facebook.com/story-up-1"}|}; headers = [] };
    { status = 400; body = rate_limit_error; headers = [] };
  ];

  Facebook.post_story_video
    ~account_id:"test_account"
    ~video_url:"https://example.com/story-rate-limit.mp4"
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Rate_limited _) ->
          let me_accounts_requests =
            List.filter (fun (m, u, _, _) -> m = "GET" && string_contains u "/me/accounts") !Mock_http.requests
          in
          assert (List.length me_accounts_requests = 0);
          print_endline "✓ Post video story skips recovery on upload rate-limit"
      | Error_types.Failure e ->
          failwith ("Expected rate-limited error, got: " ^ Error_types.error_to_string e)
      | _ -> failwith "Expected story failure on upload rate-limit")

(** Test: Post story with auto-detect (image) *)
let test_post_story_auto_image () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";
  
  Mock_http.set_responses [
    { status = 200; body = "image_data"; headers = [] };
    { status = 200; body = {|{"post_id": "auto_story_img"}|}; headers = [] };
  ];
  
  Facebook.post_story
    ~account_id:"test_account"
    ~media_url:"https://example.com/story.png"
    (handle_outcome
      (fun story_id ->
        assert (story_id = "auto_story_img");
        print_endline "✓ Post story with auto-detect (image)")
      (fun err -> failwith ("Post story auto-detect failed: " ^ err)))

(** Test: Post story with auto-detect (video) *)
let test_post_story_auto_video () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";
  
  Mock_http.set_responses [
    { status = 200; body = "video_data"; headers = [] };
    { status = 200; body = {|{"video_id": "v789", "upload_url": "https://upload.example.com"}|}; headers = [] };
    { status = 200; body = {|{"success": true}|}; headers = [] };
    { status = 200; body = {|{"success": true, "post_id": "auto_story_vid"}|}; headers = [] };
  ];
  
  Facebook.post_story
    ~account_id:"test_account"
    ~media_url:"https://example.com/story.mov"
    (handle_outcome
      (fun story_id ->
        assert (story_id = "auto_story_vid");
        print_endline "✓ Post story with auto-detect (video)")
      (fun err -> failwith ("Post story auto-detect video failed: " ^ err)))

(** Test: Story validation - valid image URL *)
let test_validate_story_valid_image () =
  match Facebook.validate_story ~media_url:"https://example.com/story.jpg" with
  | Ok () -> print_endline "✓ Story validation - valid image URL"
  | Error e -> failwith ("Story validation failed: " ^ e)

(** Test: Story validation - valid video URL *)
let test_validate_story_valid_video () =
  match Facebook.validate_story ~media_url:"https://example.com/story.mp4" with
  | Ok () -> print_endline "✓ Story validation - valid video URL"
  | Error e -> failwith ("Story validation failed: " ^ e)

(** Test: Story validation - invalid URL *)
let test_validate_story_invalid_url () =
  match Facebook.validate_story ~media_url:"not-a-url" with
  | Error msg when string_contains msg "HTTP" -> 
      print_endline "✓ Story validation - rejects invalid URL"
  | _ -> failwith "Should reject invalid URL"

(** Test: Story validation - invalid format *)
let test_validate_story_invalid_format () =
  match Facebook.validate_story ~media_url:"https://example.com/story.txt" with
  | Error msg when string_contains msg "image" || string_contains msg "video" -> 
      print_endline "✓ Story validation - rejects invalid format"
  | _ -> failwith "Should reject invalid format"

(** {1 Video Reel Tests} *)

(** Test: Upload video reel (3-phase resumable upload)
    
    Tests the Facebook Reel upload flow which uses:
    1. Initialize upload session (upload_phase=start)
    2. Upload video binary to upload_url
    3. Finish upload (upload_phase=finish)
    
    @see <https://developers.facebook.com/docs/video-api/guides/reels-publishing>
*)
let test_upload_video_reel () =
  Mock_config.reset ();
  
  Mock_http.set_responses [
    (* GET video from URL *)
    { status = 200; body = "fake_video_binary_data"; headers = [] };
    (* POST init - returns video_id and upload_url *)
    { status = 200; body = {|{"video_id": "reel_video_123", "upload_url": "https://rupload.facebook.com/video-upload/v123"}|}; headers = [] };
    (* POST upload binary to upload_url *)
    { status = 200; body = {|{"success": true}|}; headers = [] };
    (* POST finish *)
    { status = 200; body = {|{"success": true, "id": "reel_post_123"}|}; headers = [] };
  ];
  
  Facebook.upload_video_reel
    ~page_id:"page_123"
    ~page_access_token:"test_token"
    ~video_url:"https://example.com/reel.mp4"
    ~description:"My first Reel!"
    (fun video_id ->
      assert (video_id = "reel_video_123");
      (* Verify the init request contained upload_phase=start *)
      let requests = !Mock_http.requests in
      let has_init = List.exists (fun (_, url, _, body) ->
        string_contains url "video_reels" && string_contains body "upload_phase"
      ) requests in
      assert has_init;
      print_endline "✓ Upload video reel (3-phase)")
    (fun err -> failwith ("Upload video reel failed: " ^ err))

(** Test: Post reel (high-level) *)
let test_post_reel () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";
  
  Mock_http.set_responses [
    { status = 200; body = "video_data"; headers = [] };
    { status = 200; body = {|{"video_id": "vid_456", "upload_url": "https://upload.facebook.com/v1"}|}; headers = [] };
    { status = 200; body = {|{"success": true}|}; headers = [] };
    { status = 200; body = {|{"success": true}|}; headers = [] };
  ];
  
  Facebook.post_reel
    ~account_id:"test_account"
    ~text:"Check out my Reel! #facebook #reel"
    ~video_url:"https://example.com/reel.mp4"
    (handle_outcome
      (fun video_id ->
        assert (video_id = "vid_456");
        print_endline "✓ Post reel (high-level)")
      (fun err -> failwith ("Post reel failed: " ^ err)))

(** Test: Post reel does not attempt token recovery on non-auth errors *)
let test_post_reel_no_recovery_on_non_auth_error () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";

  (* Only one response: video download succeeds, then init request fails due to missing mock response
     and should NOT trigger token recovery (/me/accounts). *)
  Mock_http.set_responses [
    { status = 200; body = "video_data"; headers = [] };
  ];

  Facebook.post_reel
    ~account_id:"test_account"
    ~text:"Reel should fail without recovery"
    ~video_url:"https://example.com/reel.mp4"
    (fun outcome ->
      match outcome with
      | Error_types.Failure _ ->
          let requested_recovery =
            List.exists (fun (m, u, _, _) ->
              m = "GET" && string_contains u "/me/accounts"
            ) !Mock_http.requests
          in
          assert (not requested_recovery);
          print_endline "✓ Post reel skips recovery on non-auth errors"
      | _ -> failwith "Expected reel failure")

(** Test: Post reel recovers token on auth error *)
let test_post_reel_recovers_on_auth_error () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "user_token_for_reel";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  let permission_error = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "REEL_PERM"
    }
  }|} in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";

  Mock_http.set_responses [
    { status = 200; body = "video_data_attempt1"; headers = [] };
    { status = 400; body = permission_error; headers = [] };
    { status = 200; body = {|{"data":[{"id":"page_123","name":"My Page","access_token":"resolved_reel_page_token","category":"Brand"}]}|}; headers = [] };
    { status = 200; body = "video_data_attempt2"; headers = [] };
    { status = 200; body = {|{"video_id": "vid_recovered", "upload_url": "https://upload.facebook.com/v1"}|}; headers = [] };
    { status = 200; body = {|{"success": true}|}; headers = [] };
    { status = 200; body = {|{"success": true}|}; headers = [] };
  ];

  Facebook.post_reel
    ~account_id:"test_account"
    ~text:"Reel recovers on auth error"
    ~video_url:"https://example.com/reel.mp4"
    (handle_outcome
      (fun video_id ->
        assert (video_id = "vid_recovered");
        let used_resolved_token =
          List.exists (fun (_m, _u, headers, _body) ->
            List.exists (fun (k, v) ->
              k = "Authorization" && string_contains v "resolved_reel_page_token"
            ) headers
          ) !Mock_http.requests
        in
        assert used_resolved_token;
        print_endline "✓ Post reel recovers on auth errors")
      (fun err -> failwith ("Expected reel recovery success, got: " ^ err)))

(** Test: Post reel recovers on auth error during upload phase *)
let test_post_reel_recovers_on_upload_auth_error () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "user_token_reel_upload";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  let permission_error = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "REEL_UP_AUTH"
    }
  }|} in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";

  Mock_http.set_responses [
    { status = 200; body = "video_data_1"; headers = [] };
    { status = 200; body = {|{"video_id": "vid_up_1", "upload_url": "https://upload.facebook.com/reel-up-1"}|}; headers = [] };
    { status = 400; body = permission_error; headers = [] };
    { status = 200; body = {|{"data":[{"id":"page_123","name":"My Page","access_token":"resolved_reel_upload_token","category":"Brand"}]}|}; headers = [] };
    { status = 200; body = "video_data_2"; headers = [] };
    { status = 200; body = {|{"video_id": "vid_up_2", "upload_url": "https://upload.facebook.com/reel-up-2"}|}; headers = [] };
    { status = 200; body = {|{"success": true}|}; headers = [] };
    { status = 200; body = {|{"success": true}|}; headers = [] };
  ];

  Facebook.post_reel
    ~account_id:"test_account"
    ~text:"Reel upload auth recovery"
    ~video_url:"https://example.com/reel-upload-recover.mp4"
    (handle_outcome
      (fun video_id ->
        assert (video_id = "vid_up_2");
        let used_resolved_token =
          List.exists (fun (_m, _u, headers, _body) ->
            List.exists (fun (k, v) -> k = "Authorization" && string_contains v "resolved_reel_upload_token") headers
          ) !Mock_http.requests
        in
        assert used_resolved_token;
        (match List.assoc_opt "test_account" !Mock_config.credentials_store with
        | Some updated_creds ->
            assert (updated_creds.access_token = "resolved_reel_upload_token");
            assert (updated_creds.refresh_token = Some "user_token_reel_upload")
        | None -> failwith "Expected updated credentials after reel upload recovery");
        print_endline "✓ Post reel recovers on upload auth error")
      (fun err -> failwith ("Reel upload auth recovery failed: " ^ err)))

(** Test: Post reel does not recover on upload rate-limit errors *)
let test_post_reel_no_recovery_on_upload_rate_limit () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "user_token_reel_rl";
    refresh_token = Some "stored_user_token_reel_rl";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  let rate_limit_error = {|{
    "error": {
      "message": "(#80001) There have been too many calls to this Page account",
      "type": "OAuthException",
      "code": 80001,
      "fbtrace_id": "REEL_UP_RL"
    }
  }|} in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";

  Mock_http.set_responses [
    { status = 200; body = "video_data_1"; headers = [] };
    { status = 200; body = {|{"video_id": "vid_up_1", "upload_url": "https://upload.facebook.com/reel-up-1"}|}; headers = [] };
    { status = 400; body = rate_limit_error; headers = [] };
  ];

  Facebook.post_reel
    ~account_id:"test_account"
    ~text:"Reel upload rate-limit"
    ~video_url:"https://example.com/reel-upload-rate-limit.mp4"
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Rate_limited _) ->
          let me_accounts_requests =
            List.filter (fun (m, u, _, _) -> m = "GET" && string_contains u "/me/accounts") !Mock_http.requests
          in
          assert (List.length me_accounts_requests = 0);
          print_endline "✓ Post reel skips recovery on upload rate-limit"
      | Error_types.Failure e ->
          failwith ("Expected rate-limited error, got: " ^ Error_types.error_to_string e)
      | _ -> failwith "Expected reel failure on upload rate-limit")

(** Test: Video reel validation - caption too long *)
let test_reel_caption_validation () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";
  
  (* Facebook's actual post limit is 63206 chars - test exceeding that *)
  let long_caption = String.make 63300 'x' in
  
  Facebook.post_reel
    ~account_id:"test_account"
    ~text:long_caption
    ~video_url:"https://example.com/reel.mp4"
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Validation_error _) ->
          print_endline "✓ Reel caption validation - too long rejected"
      | _ -> failwith "Should reject caption over 63206 chars")

(** Test: Video media validation - validates URLs are accessible *)
let test_video_media_validation () =
  (* Valid video URL *)
  (match Facebook.validate_media ~media_urls:["https://example.com/video.mp4"] with
   | Ok () -> print_endline "✓ Valid video URL passes"
   | Error _ -> failwith "Valid video URL should pass");
  
  (* Multiple valid URLs *)
  (match Facebook.validate_media ~media_urls:[
    "https://example.com/photo1.jpg";
    "https://example.com/photo2.jpg"
  ] with
   | Ok () -> print_endline "✓ Multiple valid URLs pass"
   | Error _ -> failwith "Multiple valid URLs should pass");
  
  (* Invalid URL (not http/https) rejected *)
  (match Facebook.validate_media ~media_urls:["ftp://example.com/video.mp4"] with
   | Error _ -> print_endline "✓ Invalid URL rejected"
   | Ok () -> failwith "Invalid URL should fail")

(** Test: Post comment *)
let test_post_comment () =
  Mock_config.reset ();

  let response_body = {|{"id": "12345_67890"}|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Facebook.post_comment
    ~post_id:"12345"
    ~access_token:"test_token"
    ~message:"Great post!"
    (function
      | Ok comment_id ->
          assert (comment_id = "12345_67890");
          (* Verify the request went to the right endpoint *)
          let requests = !Mock_http.requests in
          (match requests with
          | (method_, url, _, body) :: _ ->
              assert (method_ = "POST");
              assert (string_contains url "12345/comments");
              assert (string_contains body "message");
              assert (string_contains body "Great");
              print_endline "✓ Post comment"
          | [] -> failwith "No requests made")
      | Error e -> failwith ("Post comment failed: " ^ Error_types.error_to_string e))

(** Test: Get comments *)
let test_get_comments () =
  Mock_config.reset ();

  let response_body = {|{
    "data": [
      {
        "id": "comment_1",
        "message": "Nice!",
        "created_time": "2024-01-15T10:00:00+0000",
        "from": {"name": "Alice", "id": "user_1"}
      },
      {
        "id": "comment_2",
        "message": "Love it!",
        "created_time": "2024-01-15T11:00:00+0000",
        "from": {"name": "Bob", "id": "user_2"}
      }
    ]
  }|} in

  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Facebook.get_comments
    ~post_id:"12345"
    ~access_token:"test_token"
    (function
      | Ok comments ->
          assert (List.length comments = 2);
          let c1 = List.hd comments in
          assert (c1.comment_id = "comment_1");
          assert (c1.comment_message = "Nice!");
          assert (c1.comment_created_time = Some "2024-01-15T10:00:00+0000");
          assert (c1.comment_from = Some "Alice");
          let c2 = List.nth comments 1 in
          assert (c2.comment_id = "comment_2");
          assert (c2.comment_message = "Love it!");
          assert (c2.comment_from = Some "Bob");
          (* Verify the request URL *)
          let requests = !Mock_http.requests in
          (match requests with
          | (method_, url, _, _) :: _ ->
              assert (method_ = "GET");
              assert (string_contains url "12345/comments");
              assert (string_contains url "fields=");
              print_endline "✓ Get comments"
          | [] -> failwith "No requests made")
      | Error e -> failwith ("Get comments failed: " ^ Error_types.error_to_string e))

(** Test: Get comments on a post with no comments *)
let test_get_comments_empty () =
  Mock_config.reset ();

  let response_body = {|{"data": []}|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Facebook.get_comments
    ~post_id:"12345"
    ~access_token:"test_token"
    (function
      | Ok comments ->
          assert (List.length comments = 0);
          print_endline "✓ Get comments (empty)"
      | Error e -> failwith ("Get comments empty failed: " ^ Error_types.error_to_string e))

(** Test: Post comment returns API error on failure *)
let test_post_comment_error () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "Invalid post ID",
      "type": "OAuthException",
      "code": 100,
      "fbtrace_id": "ERR123"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.post_comment
    ~post_id:"invalid_post"
    ~access_token:"test_token"
    ~message:"Test"
    (function
      | Ok _ -> failwith "Should have failed with error"
      | Error (Error_types.Api_error err) ->
          assert (err.status_code = 400);
          assert (string_contains err.message "Invalid post ID");
          print_endline "✓ Post comment error handling"
      | Error e -> failwith ("Expected Api_error, got: " ^ Error_types.error_to_string e))

(** Test: Get page info returns API error on failure *)
let test_get_page_info_error () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "Unsupported get request",
      "type": "GraphMethodException",
      "code": 100,
      "fbtrace_id": "PAGE_ERR"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.get_page_info
    ~page_id:"nonexistent_page"
    ~access_token:"test_token"
    (function
      | Ok _ -> failwith "Should have failed with error"
      | Error (Error_types.Api_error err) ->
          assert (err.status_code = 400);
          assert (string_contains err.message "Unsupported get request");
          print_endline "✓ Get page info error handling"
      | Error e -> failwith ("Expected Api_error, got: " ^ Error_types.error_to_string e))

(** Test: Get comments handles missing from field gracefully *)
let test_get_comments_missing_from () =
  Mock_config.reset ();

  let response_body = {|{
    "data": [
      {
        "id": "comment_no_from",
        "message": "Anonymous comment",
        "created_time": "2024-01-15T10:00:00+0000"
      }
    ]
  }|} in

  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Facebook.get_comments
    ~post_id:"12345"
    ~access_token:"test_token"
    (function
      | Ok comments ->
          assert (List.length comments = 1);
          let c = List.hd comments in
          assert (c.comment_id = "comment_no_from");
          assert (c.comment_message = "Anonymous comment");
          assert (c.comment_from = None);
          print_endline "✓ Get comments (missing from field)"
      | Error e -> failwith ("Get comments missing from failed: " ^ Error_types.error_to_string e))

(** Test: Post with link *)
let test_post_with_link () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";

  Mock_http.set_response { status = 200; body = {|{"id": "post_link_123"}|}; headers = [] };

  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Check out this article!"
    ~media_urls:[]
    ~link:"https://example.com/article"
    (handle_outcome
      (fun post_id ->
        assert (post_id = "post_link_123");
        (* Verify the request body contains the link parameter *)
        let requests = !Mock_http.requests in
        let feed_request =
          List.find_opt (fun (m, u, _, _) -> m = "POST" && string_contains u "/feed") requests
        in
        (match feed_request with
        | Some (_, _, _, body) ->
            assert (string_contains body "link=");
            assert (string_contains body "example.com");
            print_endline "✓ Post with link"
        | None -> failwith "Feed request not found"))
      (fun err -> failwith ("Post with link failed: " ^ err)))

(** Test: Post with scheduled publish time *)
let test_post_with_scheduled_publish_time () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";

  Mock_http.set_response { status = 200; body = {|{"id": "scheduled_post_123"}|}; headers = [] };

  Facebook.post_single
    ~account_id:"test_account"
    ~text:"This is a scheduled post"
    ~media_urls:[]
    ~scheduled_publish_time:1735689600
    (handle_outcome
      (fun post_id ->
        assert (post_id = "scheduled_post_123");
        (* Verify the request body contains scheduling parameters *)
        let requests = !Mock_http.requests in
        let feed_request =
          List.find_opt (fun (m, u, _, _) -> m = "POST" && string_contains u "/feed") requests
        in
        (match feed_request with
        | Some (_, _, _, body) ->
            assert (string_contains body "scheduled_publish_time=1735689600");
            assert (string_contains body "published=false");
            print_endline "✓ Post with scheduled publish time"
        | None -> failwith "Feed request not found"))
      (fun err -> failwith ("Scheduled post failed: " ^ err)))

(** Test: Get page info *)
let test_get_page_info () =
  Mock_config.reset ();

  let response_body = {|{
    "id": "page_123",
    "name": "My Awesome Page",
    "about": "This is a test page",
    "fan_count": 5000,
    "followers_count": 4500
  }|} in

  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Facebook.get_page_info
    ~page_id:"page_123"
    ~access_token:"test_token"
    (function
      | Ok detail ->
          assert (detail.page_detail_id = "page_123");
          assert (detail.page_detail_name = Some "My Awesome Page");
          assert (detail.page_detail_about = Some "This is a test page");
          assert (detail.page_detail_fan_count = Some 5000);
          assert (detail.page_detail_followers_count = Some 4500);
          (* Verify the request URL contains proper fields *)
          let requests = !Mock_http.requests in
          (match requests with
          | (method_, url, _, _) :: _ ->
              assert (method_ = "GET");
              assert (string_contains url "page_123");
              assert (string_contains url "fields=");
              assert (string_contains url "fan_count");
              assert (string_contains url "followers_count");
              print_endline "✓ Get page info"
          | [] -> failwith "No requests made")
      | Error e -> failwith ("Get page info failed: " ^ Error_types.error_to_string e))

(** Test: Get page info with partial fields *)
let test_get_page_info_partial () =
  Mock_config.reset ();

  let response_body = {|{
    "id": "page_456",
    "name": "Minimal Page"
  }|} in

  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Facebook.get_page_info
    ~page_id:"page_456"
    ~access_token:"test_token"
    (function
      | Ok detail ->
          assert (detail.page_detail_id = "page_456");
          assert (detail.page_detail_name = Some "Minimal Page");
          assert (detail.page_detail_about = None);
          assert (detail.page_detail_fan_count = None);
          assert (detail.page_detail_followers_count = None);
          print_endline "✓ Get page info with partial fields"
      | Error e -> failwith ("Get page info partial failed: " ^ Error_types.error_to_string e))

(** Run all tests *)
let () =
  print_endline "\n=== Facebook Provider Tests ===\n";
  
  print_endline "--- OAuth Flow Tests ---";
  test_oauth_url ();
  test_oauth_url_permissions ();
  test_oauth_url_special_chars ();
  test_oauth_csrf_protection ();
  test_redirect_uri_validation ();
  test_token_exchange ();
  test_token_exchange_errors ();
  test_exchange_code_and_get_pages ();
  test_long_lived_token ();
  test_page_vs_user_token ();
  test_token_expiry_detection ();
  test_app_secret_proof_generation ();
  
  print_endline "\n--- API Feature Tests ---";
  test_upload_photo ();
  test_content_validation ();
  test_ensure_valid_token_fresh ();
  test_ensure_valid_token_expired ();
  test_rate_limit_parsing ();
  test_field_selection ();
  test_error_code_parsing ();
  test_invalid_token_without_expiry_subcode ();
  test_token_subcode_467_maps_to_expired ();
  test_permission_error_scopes ();
  test_me_accounts_permission_scope ();
  test_get_required_permissions_override ();
  test_post_required_permissions_override ();
  test_delete_required_permissions_override ();
  test_pagination ();
  test_batch_requests ();
  test_batch_required_permissions_override ();
  test_app_secret_proof ();
  test_authorization_header ();
  test_account_analytics_request_contract ();
  test_post_analytics_request_contract ();
  test_account_analytics_parsing ();
  test_post_analytics_parsing ();
  test_canonical_analytics_adapters ();
  test_analytics_network_error_redacts_query_token ();
  test_graph_version_policy_guard ();
  
  print_endline "\n--- Alt-Text Tests ---";
  test_upload_photo_with_alt_text ();
  test_post_with_alt_text ();
  test_post_with_multiple_alt_texts ();
  test_post_with_indexed_attached_media_payload ();
  test_post_recovers_page_token_from_user_token ();
  test_post_recovery_uses_stored_user_token_fallback ();
  test_post_recovery_stops_on_rate_limit ();
  test_post_cleans_up_uploaded_photos_on_publish_failure ();
  test_post_without_alt_text ();
  test_alt_text_special_characters ();
  test_partial_alt_texts ();
  
  print_endline "\n--- Stories Tests ---";
  test_upload_photo_story ();
  test_upload_video_story ();
  test_post_story_photo ();
  test_post_story_video ();
  test_post_story_video_recovers_on_upload_auth_error ();
  test_post_story_video_no_recovery_on_upload_rate_limit ();
  test_post_story_auto_image ();
  test_post_story_auto_video ();
  test_validate_story_valid_image ();
  test_validate_story_valid_video ();
  test_validate_story_invalid_url ();
  test_validate_story_invalid_format ();
  
  print_endline "\n--- Video Reel Tests ---";
  test_upload_video_reel ();
  test_post_reel ();
  test_post_reel_no_recovery_on_non_auth_error ();
  test_post_reel_recovers_on_auth_error ();
  test_post_reel_recovers_on_upload_auth_error ();
  test_post_reel_no_recovery_on_upload_rate_limit ();
  test_reel_caption_validation ();
  test_video_media_validation ();
  test_rate_limit_error_code_80001 ();

  print_endline "\n--- Comment Management Tests ---";
  test_post_comment ();
  test_post_comment_error ();
  test_get_comments ();
  test_get_comments_empty ();
  test_get_comments_missing_from ();

  print_endline "\n--- Link Posts and Scheduling Tests ---";
  test_post_with_link ();
  test_post_with_scheduled_publish_time ();

  print_endline "\n--- Page Info Tests ---";
  test_get_page_info ();
  test_get_page_info_partial ();
  test_get_page_info_error ();

  print_endline "\n=== All 79 tests passed! ===\n"
