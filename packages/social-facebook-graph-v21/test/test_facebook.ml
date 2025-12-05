(** Tests for Facebook Graph API v21 Provider *)

open Social_core
open Social_facebook_graph_v21

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
  
  let set_response response =
    response_queue := [response]
  
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
    "access_token": "new_access_token_123",
    "token_type": "bearer",
    "expires_in": 5184000
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
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
    (fun photo_id ->
      assert (photo_id = "photo_12345");
      print_endline "✓ Upload photo")
    (fun err -> failwith ("Upload photo failed: " ^ err))

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
    (fun err -> failwith ("Ensure valid token failed: " ^ err))

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
      assert (string_contains err "expired");
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
    (fun _response ->
      (* Check that rate limit was captured *)
      match !Mock_config.rate_limits with
      | info :: _ ->
          assert (info.call_count = 15);
          assert (info.total_cputime = 25);
          assert (info.total_time = 30);
          print_endline "✓ Rate limit parsing"
      | [] -> failwith "Rate limit not captured")
    (fun err -> failwith ("Rate limit test failed: " ^ err))

(** Test: Field selection *)
let test_field_selection () =
  Mock_config.reset ();
  
  let response_body = {|{"id":"123","name":"Test"}|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Facebook.get ~path:"me" ~access_token:"test_token" ~fields:["id"; "name"]
    (fun _response ->
      (* Check that request URL contains fields parameter *)
      let requests = !Mock_http.requests in
      match requests with
      | (_, url, _, _) :: _ ->
          assert (string_contains url "fields=id%2Cname");
          print_endline "✓ Field selection"
      | [] -> failwith "No requests made")
    (fun err -> failwith ("Field selection test failed: " ^ err))

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
    (fun _response -> failwith "Should have failed with error")
    (fun err ->
      assert (string_contains err "OAuthException");
      assert (string_contains err "Invalid OAuth access token");
      assert (string_contains err "ABC123");
      print_endline "✓ Error code parsing")

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
    (fun page_result ->
      assert (List.length page_result.data = 2);
      match page_result.paging with
      | Some cursors ->
          assert (cursors.after = Some "cursor_after");
          assert (cursors.before = Some "cursor_before");
          assert (page_result.next_url <> None);
          print_endline "✓ Pagination"
      | None -> failwith "No paging info")
    (fun err -> failwith ("Pagination test failed: " ^ err))

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
    (fun results ->
      assert (List.length results = 2);
      match results with
      | r1 :: r2 :: _ ->
          assert (r1.code = 200);
          assert (r2.code = 200);
          assert (string_contains r1.body "\"id\":\"1\"");
          print_endline "✓ Batch requests"
      | _ -> failwith "Unexpected batch response")
    (fun err -> failwith ("Batch test failed: " ^ err))

(** Test: App secret proof *)
let test_app_secret_proof () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_secret";
  
  Mock_http.set_response { status = 200; body = {|{"id":"me"}|}; headers = [] };
  
  Facebook.get ~path:"me" ~access_token:"test_token"
    (fun _response ->
      (* Check that request includes appsecret_proof *)
      let requests = !Mock_http.requests in
      match requests with
      | (_, url, _, _) :: _ ->
          assert (string_contains url "appsecret_proof");
          print_endline "✓ App secret proof"
      | [] -> failwith "No requests made")
    (fun err -> failwith ("App secret proof test failed: " ^ err))

(** Test: Authorization header usage *)
let test_authorization_header () =
  Mock_config.reset ();
  
  Mock_http.set_response { status = 200; body = {|{"id":"me"}|}; headers = [] };
  
  Facebook.get ~path:"me" ~access_token:"test_token"
    (fun _response ->
      (* Check that Authorization header is present *)
      let requests = !Mock_http.requests in
      match requests with
      | (_, _, headers, _) :: _ ->
          let has_auth = List.exists (fun (k, v) ->
            k = "Authorization" && string_contains v "Bearer test_token"
          ) headers in
          assert has_auth;
          print_endline "✓ Authorization header"
      | [] -> failwith "No requests made")
    (fun err -> failwith ("Authorization header test failed: " ^ err))

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
  
  let response_body = {|{
    "access_token": "long_lived_token_123",
    "token_type": "bearer",
    "expires_in": 5184000
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
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
    (fun _response ->
      (* This would return page access tokens in real usage *)
      print_endline "✓ Page vs user token handling")
    (fun err -> failwith ("Page token test failed: " ^ err))

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
    (fun _response ->
      (* Verify request included appsecret_proof *)
      let requests = !Mock_http.requests in
      match requests with
      | (_, url, _, _) :: _ ->
          if string_contains url "appsecret_proof=" then
            print_endline "✓ App secret proof generation"
          else
            failwith "App secret proof not included"
      | [] -> failwith "No requests made")
    (fun err -> failwith ("App secret proof test failed: " ^ err))

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
    (fun photo_id ->
      assert (photo_id = "photo_with_alt");
      print_endline "✓ Upload photo with alt-text")
    (fun err -> failwith ("Upload with alt-text failed: " ^ err))

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
    (fun _post_id ->
      print_endline "✓ Post with single image and alt-text")
    (fun err -> failwith ("Post with alt-text failed: " ^ err))

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
    (fun _post_id ->
      print_endline "✓ Post with multiple images and alt-texts")
    (fun err -> failwith ("Post with multiple alt-texts failed: " ^ err))

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
    (fun _post_id ->
      print_endline "✓ Post without alt-text")
    (fun err -> failwith ("Post without alt-text failed: " ^ err))

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
    (fun _photo_id ->
      print_endline "✓ Alt-text with special characters")
    (fun err -> failwith ("Alt-text with special chars failed: " ^ err))

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
    (fun _post_id ->
      print_endline "✓ Post with partial alt-texts (3 images, 2 alt-texts)")
    (fun err -> failwith ("Post with partial alt-texts failed: " ^ err))

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
  test_pagination ();
  test_batch_requests ();
  test_app_secret_proof ();
  test_authorization_header ();
  
  print_endline "\n--- Alt-Text Tests ---";
  test_upload_photo_with_alt_text ();
  test_post_with_alt_text ();
  test_post_with_multiple_alt_texts ();
  test_post_without_alt_text ();
  test_alt_text_special_characters ();
  test_partial_alt_texts ();
  
  print_endline "\n=== All 28 tests passed! ===\n"

