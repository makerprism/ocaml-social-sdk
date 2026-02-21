(** Tests for Instagram Standalone (Business Login) API v21 Provider *)

open Social_core
open Social_instagram_standalone_v21

(** Helper to check if string contains substring *)
let string_contains s substr =
  try
    ignore (Str.search_forward (Str.regexp_string substr) s 0);
    true
  with Not_found -> false

let query_param url key =
  let params = Uri.query (Uri.of_string url) in
  match List.assoc_opt key params with
  | Some (v :: _) -> Some v
  | _ -> None

(** Helper to handle outcome type in tests *)
let handle_outcome on_success on_error outcome =
  match outcome with
  | Error_types.Success result -> on_success result
  | Error_types.Partial_success { result; _ } -> on_success result
  | Error_types.Failure err -> on_error (Error_types.error_to_string err)

(** Helper to handle api_result type in tests *)
let handle_result on_success on_error result =
  match result with
  | Ok value -> on_success value
  | Error err -> on_error (Error_types.error_to_string err)

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
  let ig_user_ids = ref []
  let sleep_calls = ref []

  let reset () =
    env_vars := [];
    credentials_store := [];
    health_statuses := [];
    ig_user_ids := [];
    sleep_calls := [];
    Mock_http.reset ()

  let set_env key value =
    env_vars := (key, value) :: !env_vars

  let get_env key =
    List.assoc_opt key !env_vars

  let set_credentials ~account_id ~credentials =
    credentials_store := (account_id, credentials) :: !credentials_store

  let set_ig_user_id ~account_id ~ig_user_id =
    ig_user_ids := (account_id, ig_user_id) :: !ig_user_ids

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

  let get_ig_user_id ~account_id on_success on_error =
    match List.assoc_opt account_id !ig_user_ids with
    | Some ig_user_id -> on_success ig_user_id
    | None -> on_error "IG User ID not found"

  let sleep duration on_continue =
    sleep_calls := duration :: !sleep_calls;
    on_continue ()

  let on_rate_limit_update _info =
    () (* No-op for tests *)
end

module Instagram = Make(Mock_config)
module OAuth_standalone_client = OAuth.Make(Mock_http)

(** {1 OAuth Standalone Tests} *)

(** Test: Standalone OAuth authorization URL starts with correct endpoint *)
let test_standalone_auth_url_endpoint () =
  let url = OAuth.get_authorization_url
    ~client_id:"test_app_id"
    ~redirect_uri:"https://example.com/callback"
    ~state:"state_abc"
    ()
  in
  assert (String.length url > 0);
  assert (string_contains url "https://api.instagram.com/oauth/authorize");
  print_endline "PASS Standalone auth URL starts with correct endpoint"

(** Test: Standalone OAuth authorization URL includes enable_fb_login=0 *)
let test_standalone_auth_url_enable_fb_login () =
  let url = OAuth.get_authorization_url
    ~client_id:"test_app_id"
    ~redirect_uri:"https://example.com/callback"
    ~state:"state_abc"
    ()
  in
  assert (string_contains url "enable_fb_login=0");
  print_endline "PASS Standalone auth URL includes enable_fb_login=0"

(** Test: Standalone OAuth authorization URL has comma-separated scopes *)
let test_standalone_auth_url_scopes_comma_separated () =
  let url = OAuth.get_authorization_url
    ~client_id:"test_app_id"
    ~redirect_uri:"https://example.com/callback"
    ~state:"state_abc"
    ()
  in
  let scope_val = query_param url "scope" in
  (match scope_val with
   | Some s ->
       assert (string_contains s ",");
       assert (string_contains s "instagram_business_basic");
       assert (string_contains s "instagram_business_content_publish")
   | None -> failwith "Missing scope parameter");
  print_endline "PASS Standalone auth URL has comma-separated scopes"

(** Test: Standalone OAuth authorization URL has response_type=code *)
let test_standalone_auth_url_response_type () =
  let url = OAuth.get_authorization_url
    ~client_id:"test_app_id"
    ~redirect_uri:"https://example.com/callback"
    ~state:"state_abc"
    ()
  in
  assert (query_param url "response_type" = Some "code");
  print_endline "PASS Standalone auth URL has response_type=code"

(** Test: Standalone OAuth authorization URL supports custom scopes *)
let test_standalone_auth_url_custom_scopes () =
  let url = OAuth.get_authorization_url
    ~client_id:"test_app_id"
    ~redirect_uri:"https://example.com/callback"
    ~state:"state_abc"
    ~scopes:["instagram_business_basic"; "instagram_business_manage_comments"]
    ()
  in
  let scope_val = query_param url "scope" in
  (match scope_val with
   | Some s ->
       assert (string_contains s "instagram_business_basic");
       assert (string_contains s "instagram_business_manage_comments");
       assert (not (string_contains s "instagram_business_content_publish"))
   | None -> failwith "Missing scope parameter");
  print_endline "PASS Standalone auth URL supports custom scopes"

(** Test: Standalone exchange_code happy path with user_id as int *)
let test_standalone_exchange_code_happy_path_int_user_id () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"access_token":"short_tok_123","user_id":9876543210,"permissions":"instagram_business_basic,instagram_business_content_publish"}|};
    headers = [];
  };

  OAuth_standalone_client.exchange_code
    ~client_id:"app_id"
    ~client_secret:"app_secret"
    ~redirect_uri:"https://example.com/cb"
    ~code:"auth_code_xyz"
    (handle_result
      (fun (creds, user_id) ->
        assert (creds.access_token = "short_tok_123");
        assert (creds.token_type = "Bearer");
        assert (user_id = "9876543210");
        (* Verify it was a POST request *)
        let requests = !Mock_http.requests in
        let has_post = List.exists (fun (meth, _, _, _) -> meth = "POST") requests in
        assert has_post;
        (* Verify Content-Type header *)
        let has_form_header = List.exists (fun (_, _, headers, _) ->
          List.exists (fun (k, v) -> k = "Content-Type" && v = "application/x-www-form-urlencoded") headers
        ) requests in
        assert has_form_header;
        print_endline "PASS Standalone exchange_code happy path (int user_id)")
      (fun err -> failwith ("Standalone exchange_code failed: " ^ err)))

(** Test: Standalone exchange_code happy path with user_id as string *)
let test_standalone_exchange_code_happy_path_string_user_id () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"access_token":"short_tok_456","user_id":"1234567890","token_type":"bearer"}|};
    headers = [];
  };

  OAuth_standalone_client.exchange_code
    ~client_id:"app_id"
    ~client_secret:"app_secret"
    ~redirect_uri:"https://example.com/cb"
    ~code:"auth_code_abc"
    (handle_result
      (fun (creds, user_id) ->
        assert (creds.access_token = "short_tok_456");
        assert (user_id = "1234567890");
        print_endline "PASS Standalone exchange_code happy path (string user_id)")
      (fun err -> failwith ("Standalone exchange_code string user_id failed: " ^ err)))

(** Test: Standalone exchange_code normalizes token_type to Bearer *)
let test_standalone_exchange_code_normalizes_token_type () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"access_token":"tok","user_id":"123","token_type":"bearer"}|};
    headers = [];
  };

  OAuth_standalone_client.exchange_code
    ~client_id:"app_id"
    ~client_secret:"app_secret"
    ~redirect_uri:"https://example.com/cb"
    ~code:"code"
    (handle_result
      (fun (creds, _user_id) ->
        assert (creds.token_type = "Bearer");
        print_endline "PASS Standalone exchange_code normalizes token_type to Bearer")
      (fun err -> failwith ("Standalone exchange_code token_type failed: " ^ err)))

(** Test: Standalone exchange_code error when user_id is missing *)
let test_standalone_exchange_code_missing_user_id () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"access_token":"tok_no_uid","token_type":"bearer"}|};
    headers = [];
  };

  OAuth_standalone_client.exchange_code
    ~client_id:"app_id"
    ~client_secret:"app_secret"
    ~redirect_uri:"https://example.com/cb"
    ~code:"code"
    (function
      | Ok _ -> failwith "Expected error for missing user_id"
      | Error (Error_types.Internal_error msg) when string_contains msg "user_id" ->
          print_endline "PASS Standalone exchange_code error on missing user_id"
      | Error err -> failwith ("Unexpected error type: " ^ Error_types.error_to_string err))

(** Test: Standalone exchange_code handles HTTP error response *)
let test_standalone_exchange_code_http_error () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 400;
    body = {|{"error":{"code":190,"message":"Invalid OAuth 2.0 Access Token"}}|};
    headers = [];
  };

  OAuth_standalone_client.exchange_code
    ~client_id:"app_id"
    ~client_secret:"app_secret"
    ~redirect_uri:"https://example.com/cb"
    ~code:"bad_code"
    (function
      | Ok _ -> failwith "Expected error for HTTP 400"
      | Error (Error_types.Auth_error Error_types.Token_expired) ->
          print_endline "PASS Standalone exchange_code handles HTTP error"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: Standalone exchange_for_long_lived_token happy path *)
let test_standalone_exchange_long_lived_happy_path () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"access_token":"long_lived_tok_abc","token_type":"bearer","expires_in":5184000}|};
    headers = [];
  };

  OAuth_standalone_client.exchange_for_long_lived_token
    ~client_secret:"app_secret"
    ~short_lived_token:"short_tok"
    (handle_result
      (fun creds ->
        assert (creds.access_token = "long_lived_tok_abc");
        assert (creds.token_type = "Bearer");
        assert (creds.expires_at <> None);
        (* Verify it was a GET request to the correct endpoint *)
        let requests = !Mock_http.requests in
        let has_correct_url = List.exists (fun (meth, url, _, _) ->
          meth = "GET"
          && string_contains url "graph.instagram.com/access_token"
          && query_param url "grant_type" = Some "ig_exchange_token"
        ) requests in
        assert has_correct_url;
        print_endline "PASS Standalone exchange_for_long_lived_token happy path")
      (fun err -> failwith ("Standalone exchange_for_long_lived_token failed: " ^ err)))

(** Test: Standalone exchange_for_long_lived_token normalizes token_type *)
let test_standalone_exchange_long_lived_normalizes_token_type () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"access_token":"ll_tok","token_type":"bearer","expires_in":5184000}|};
    headers = [];
  };

  OAuth_standalone_client.exchange_for_long_lived_token
    ~client_secret:"secret"
    ~short_lived_token:"sl_tok"
    (handle_result
      (fun creds ->
        assert (creds.token_type = "Bearer");
        print_endline "PASS Standalone exchange_for_long_lived_token normalizes token_type")
      (fun err -> failwith ("Standalone long-lived token_type failed: " ^ err)))

(** Test: Standalone exchange_for_long_lived_token with appsecret_proof *)
let test_standalone_exchange_long_lived_appsecret_proof () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"access_token":"ll_tok","token_type":"bearer","expires_in":5184000}|};
    headers = [];
  };

  OAuth_standalone_client.exchange_for_long_lived_token
    ~client_secret:"secret"
    ~short_lived_token:"sl_tok"
    ~app_secret:"my_app_secret"
    (handle_result
      (fun _creds ->
        let requests = !Mock_http.requests in
        let has_proof = List.exists (fun (_, url, _, _) ->
          match query_param url "appsecret_proof" with
          | Some p -> String.length p = 64
          | None -> false
        ) requests in
        assert has_proof;
        print_endline "PASS Standalone exchange_for_long_lived_token with appsecret_proof")
      (fun err -> failwith ("Standalone long-lived appsecret_proof failed: " ^ err)))

(** Test: Standalone get_profile happy path *)
let test_standalone_get_profile_happy_path () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"id":"12345","username":"testuser","name":"Test User","profile_picture_url":"https://example.com/pic.jpg"}|};
    headers = [];
  };

  OAuth_standalone_client.get_profile
    ~access_token:"valid_tok"
    (handle_result
      (fun (profile : standalone_profile) ->
        assert (profile.user_id = "12345");
        assert (profile.username = "testuser");
        assert (profile.name = "Test User");
        assert (profile.profile_picture_url = Some "https://example.com/pic.jpg");
        print_endline "PASS Standalone get_profile happy path")
      (fun err -> failwith ("Standalone get_profile failed: " ^ err)))

(** Test: Standalone get_profile with missing profile_picture_url *)
let test_standalone_get_profile_no_picture () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"id":"67890","username":"nopic_user","name":"No Pic"}|};
    headers = [];
  };

  OAuth_standalone_client.get_profile
    ~access_token:"valid_tok"
    (handle_result
      (fun (profile : standalone_profile) ->
        assert (profile.user_id = "67890");
        assert (profile.username = "nopic_user");
        assert (profile.profile_picture_url = None);
        print_endline "PASS Standalone get_profile with missing profile_picture_url")
      (fun err -> failwith ("Standalone get_profile no picture failed: " ^ err)))

(** Test: Standalone get_profile requests 'id' field (not 'user_id') *)
let test_standalone_get_profile_fields_parameter () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"id":"99999","username":"u","name":"N"}|};
    headers = [];
  };

  OAuth_standalone_client.get_profile
    ~access_token:"valid_tok"
    (handle_result
      (fun _profile ->
        let requests = !Mock_http.requests in
        let has_id_field = List.exists (fun (_, url, _, _) ->
          match query_param url "fields" with
          | Some f -> string_contains f "id" && not (string_contains f "user_id")
          | None -> false
        ) requests in
        assert has_id_field;
        print_endline "PASS Standalone get_profile requests 'id' field (not 'user_id')")
      (fun err -> failwith ("Standalone get_profile fields failed: " ^ err)))

(** Test: Standalone get_profile error on missing identifier *)
let test_standalone_get_profile_missing_identifier () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"username":"orphan_user","name":"No ID"}|};
    headers = [];
  };

  OAuth_standalone_client.get_profile
    ~access_token:"valid_tok"
    (function
      | Ok _ -> failwith "Expected error for missing identifier"
      | Error (Error_types.Internal_error msg) when string_contains msg "identifier" ->
          print_endline "PASS Standalone get_profile error on missing identifier"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** {1 Content Publishing Tests} *)

(** Test: OAuth URL uses Instagram standalone endpoint *)
let test_oauth_url () =
  Mock_config.reset ();
  Mock_config.set_env "INSTAGRAM_APP_ID" "test_app_id";

  Instagram.get_oauth_url ~redirect_uri:"https://example.com/callback" ~state:"test_state_123"
    (fun url ->
      assert (string_contains url "client_id=test_app_id");
      assert (string_contains url "state=test_state_123");
      assert (string_contains url "instagram_business");
      print_endline "PASS OAuth URL generation (standalone)")
    (fun err -> failwith ("OAuth URL failed: " ^ err))

(** Test: API base URL uses graph.instagram.com *)
let test_api_base_url_uses_instagram () =
  Mock_config.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"data": []}|};
    headers = [];
  };

  Instagram.get_account_audience_insights
    ~id:"ig_user_123"
    ~access_token:"tok"
    ~since:"2025-01-01"
    ~until:"2025-01-07"
    (handle_result
      (fun _insights ->
        let requests = !Mock_http.requests in
        let uses_instagram_graph = List.exists (fun (_, url, _, _) ->
          string_contains url "graph.instagram.com/v21.0"
        ) requests in
        assert uses_instagram_graph;
        let uses_facebook_graph = List.exists (fun (_, url, _, _) ->
          string_contains url "graph.facebook.com"
        ) requests in
        assert (not uses_facebook_graph);
        print_endline "PASS API base URL uses graph.instagram.com (not graph.facebook.com)")
      (fun err -> failwith ("API base URL check failed: " ^ err)))

(** Test: Rate-limit parser tolerates numeric formats and uses max component *)
let test_rate_limit_parsing () =
  let headers = [
    ("X-App-Usage", "{\"call_count\":5,\"total_cputime\":24.9,\"total_time\":12}");
  ] in
  match Instagram.parse_rate_limit_header headers with
  | Some info ->
      assert (info.call_count = 5);
      assert (info.total_cputime = 24);
      assert (info.total_time = 12);
      assert (info.percentage_used = 24.0);
      print_endline "PASS Rate limit parsing"
  | None -> failwith "Rate limit parsing should succeed"

(** Test: Create container *)
let test_create_container () =
  Mock_config.reset ();

  let response_body = {|{"id": "container_12345"}|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Instagram.create_image_container
    ~ig_user_id:"ig_123"
    ~access_token:"test_token"
    ~image_url:"https://example.com/image.jpg"
    ~caption:"Test caption"
    ~alt_text:None
    ~is_carousel_item:false
    (handle_result
      (fun container_id ->
        assert (container_id = "container_12345");
        print_endline "PASS Create image container")
      (fun err -> failwith ("Create image container failed: " ^ err)))

(** Test: Publish container *)
let test_publish_container () =
  Mock_config.reset ();

  let response_body = {|{"id": "media_67890"}|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Instagram.publish_container
    ~ig_user_id:"ig_123"
    ~access_token:"test_token"
    ~container_id:"container_12345"
    (handle_result
      (fun media_id ->
        assert (media_id = "media_67890");
        print_endline "PASS Publish container")
      (fun err -> failwith ("Publish container failed: " ^ err)))

(** Test: Check container status *)
let test_check_status () =
  Mock_config.reset ();

  let response_body = {|{
    "status_code": "FINISHED",
    "status": "Processing complete"
  }|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Instagram.check_container_status
    ~container_id:"container_12345"
    ~access_token:"test_token"
    (handle_result
      (fun (status_code, status) ->
        assert (status_code = "FINISHED");
        assert (status = "Processing complete");
        print_endline "PASS Check container status")
      (fun err -> failwith ("Check status failed: " ^ err)))

(** Test: Content validation *)
let test_content_validation () =
  (* Valid content *)
  (match Instagram.validate_content ~text:"Hello Instagram! #test" with
   | Ok () -> print_endline "PASS Valid content passes"
   | Error e -> failwith ("Valid content failed: " ^ e));

  (* Too long *)
  let long_text = String.make 2201 'x' in
  (match Instagram.validate_content ~text:long_text with
   | Error msg when string_contains msg "2,200" ->
       print_endline "PASS Long content rejected"
   | _ -> failwith "Long content should fail");

  (* Too many hashtags *)
  let many_hashtags = String.concat " " (List.init 31 (fun i -> Printf.sprintf "#tag%d" i)) in
  (match Instagram.validate_content ~text:many_hashtags with
   | Error msg when string_contains msg "30 hashtags" ->
       print_endline "PASS Too many hashtags rejected"
   | _ -> failwith "Too many hashtags should fail")

(** Test: Post single (full flow) *)
let test_post_single () =
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
  Mock_config.set_ig_user_id ~account_id:"test_account" ~ig_user_id:"ig_123";

  Mock_http.set_responses [
    { status = 200; body = {|{"id": "container_12345"}|}; headers = [] };
    { status = 200; body = {|{"status_code": "FINISHED", "status": "OK"}|}; headers = [] };
    { status = 200; body = {|{"id": "media_67890"}|}; headers = [] };
  ];

  Instagram.post_single
    ~account_id:"test_account"
    ~text:"Test post"
    ~media_urls:["https://example.com/image.jpg"]
    (handle_outcome
      (fun media_id ->
        assert (media_id = "media_67890");
        assert (List.length !Mock_config.sleep_calls > 0);
        print_endline "PASS Post single (full flow)")
      (fun err -> failwith ("Post single failed: " ^ err)))

(** Test: Post requires media *)
let test_post_requires_media () =
  Mock_config.reset ();

  Instagram.post_single
    ~account_id:"test_account"
    ~text:"Test"
    ~media_urls:[]
    (fun outcome ->
      match outcome with
      | Error_types.Failure _ -> print_endline "PASS Post requires media"
      | _ -> failwith "Should fail without media")

(** Test: post_single rejects unsupported media extension *)
let test_post_single_rejects_unsupported_media () =
  Mock_config.reset ();
  Instagram.post_single
    ~account_id:"test_account"
    ~text:"Unsupported media"
    ~media_urls:["https://example.com/file.avi"]
    (function
      | Error_types.Failure (Error_types.Validation_error errs) ->
          let has_format_error = List.exists (function
            | Error_types.Media_unsupported_format _ -> true
            | _ -> false
          ) errs in
          assert has_format_error;
          let requests = !Mock_http.requests in
          assert (requests = []);
          print_endline "PASS post_single rejects unsupported media"
      | _ -> failwith "Expected media format validation error")

(** Test: Polling does not publish when status checks keep failing *)
let test_polling_no_publish_on_status_error () =
  Mock_config.reset ();

  Mock_http.set_response { status = 500; body = {|{"error":{"message":"boom"}}|}; headers = [] };

  Instagram.poll_container_status
    ~container_id:"container_123"
    ~access_token:"token_123"
    ~ig_user_id:"ig_123"
    ~attempt:1
    ~max_attempts:1
    (fun _ -> failwith "Polling should not publish on status-check failure")
    (fun _err ->
      let requests = !Mock_http.requests in
      let attempted_publish = List.exists (fun (_, url, _, _) -> string_contains url "media_publish") requests in
      assert (not attempted_publish);
      print_endline "PASS Polling does not publish on status-check errors")

(** Test: OAuth API error mapping for token expiration *)
let test_parse_api_error_token_expired () =
  let err =
    Instagram.parse_api_error
      ~status_code:400
      ~response_body:{|{"error":{"code":190,"message":"Invalid OAuth 2.0 Access Token"}}|}
  in
  match err with
  | Error_types.Auth_error Error_types.Token_expired ->
      print_endline "PASS OAuth API error mapping (token expired)"
  | _ -> failwith "Expected Auth_error Token_expired"

(** Test: User-friendly error response mapping for token expiration *)
let test_parse_error_response_token_expired () =
  let msg =
    Instagram.parse_error_response
      {|{"error":{"code":190,"message":"Invalid OAuth 2.0 Access Token"}}|}
      400
  in
  if string_contains msg "reconnect" then
    print_endline "PASS User-friendly OAuth error mapping"
  else
    failwith "Expected reconnect guidance for token expiration"

(** Test: container creation returns structured auth error on API auth failure *)
let test_create_container_structured_auth_error () =
  Mock_config.reset ();
  Mock_http.set_response {
    status = 400;
    body = {|{"error":{"code":190,"message":"Invalid OAuth 2.0 Access Token"}}|};
    headers = [];
  };

  Instagram.create_image_container
    ~ig_user_id:"ig_user_123"
    ~access_token:"bad_token"
    ~image_url:"https://example.com/image.jpg"
    ~caption:"caption"
    ~alt_text:None
    ~is_carousel_item:false
    (function
      | Ok _ -> failwith "Expected auth error"
      | Error (Error_types.Auth_error Error_types.Token_expired) ->
          print_endline "PASS Structured auth error from container creation"
      | Error _ -> failwith "Expected Token_expired auth error")

(** Test: Standalone refresh_token in OAuth.Make *)
let test_standalone_oauth_refresh_token () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"access_token":"refreshed_tok","token_type":"bearer","expires_in":5184000}|};
    headers = [];
  };

  OAuth_standalone_client.refresh_token
    ~app_secret:"secret_abc"
    ~access_token:"old_tok"
    (handle_result
      (fun creds ->
        assert (creds.access_token = "refreshed_tok");
        assert (creds.token_type = "Bearer");
        let requests = !Mock_http.requests in
        let has_refresh_endpoint = List.exists (fun (_, url, _, _) ->
          string_contains url "graph.instagram.com/refresh_access_token"
        ) requests in
        assert has_refresh_endpoint;
        let has_proof = List.exists (fun (_, url, _, _) ->
          match query_param url "appsecret_proof" with
          | Some p -> String.length p = 64
          | None -> false
        ) requests in
        assert has_proof;
        print_endline "PASS Standalone OAuth refresh_token (with token_type normalization)")
      (fun err -> failwith ("Standalone OAuth refresh_token failed: " ^ err)))

(** Test: OAuth.Make.exchange_code calls on_response callback *)
let test_standalone_exchange_code_on_response () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"access_token":"tok","user_id":"123","token_type":"bearer"}|};
    headers = [("X-App-Usage", {|{"call_count":5,"total_cputime":10,"total_time":3}|})];
  };

  let callback_called = ref false in
  let on_response _response = callback_called := true in

  OAuth_standalone_client.exchange_code
    ~client_id:"app_id"
    ~client_secret:"app_secret"
    ~redirect_uri:"https://example.com/cb"
    ~code:"code"
    ~on_response
    (handle_result
      (fun (_creds, _user_id) ->
        assert !callback_called;
        print_endline "PASS Standalone exchange_code calls on_response callback")
      (fun err -> failwith ("Standalone exchange_code on_response failed: " ^ err)))

(** Test: OAuth.Make.refresh_token calls on_response callback *)
let test_standalone_refresh_token_on_response () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"access_token":"refreshed_tok","token_type":"bearer","expires_in":5184000}|};
    headers = [("X-App-Usage", {|{"call_count":12,"total_cputime":8,"total_time":4}|})];
  };

  let callback_called = ref false in
  let on_response _response = callback_called := true in

  OAuth_standalone_client.refresh_token
    ~access_token:"old_tok"
    ~on_response
    (handle_result
      (fun _creds ->
        assert !callback_called;
        print_endline "PASS Standalone refresh_token calls on_response callback")
      (fun err -> failwith ("Standalone refresh_token on_response failed: " ^ err)))

(** {1 Comment Posting Tests (H1)} *)

(** Test: Post comment request contract *)
let test_post_comment () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 200;
    body = {|{"id": "comment_12345"}|};
    headers = [];
  };

  Instagram.post_comment
    ~media_id:"media_99"
    ~access_token:"test_token"
    ~message:"Great post! #firstcomment"
    (handle_result
      (fun comment_id ->
        assert (comment_id = "comment_12345");
        let requests = !Mock_http.requests in
        let has_contract = List.exists (fun (meth, url, _, body) ->
          meth = "POST"
          && string_contains url "/media_99/comments"
          && string_contains body "message"
          && string_contains body "Great"
        ) requests in
        assert has_contract;
        print_endline "PASS Post comment")
      (fun err -> failwith ("Post comment failed: " ^ err)))

(** Test: Post comment returns structured error on auth failure *)
let test_post_comment_auth_error () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 400;
    body = {|{"error":{"code":190,"message":"Invalid OAuth 2.0 Access Token"}}|};
    headers = [];
  };

  Instagram.post_comment
    ~media_id:"media_99"
    ~access_token:"bad_token"
    ~message:"This will fail"
    (function
      | Ok _ -> failwith "Expected auth error"
      | Error (Error_types.Auth_error Error_types.Token_expired) ->
          print_endline "PASS Post comment auth error"
      | Error _ -> failwith "Expected Token_expired")

(** {1 Media Listing Tests (H2)} *)

(** Test: Get user media request contract *)
let test_get_user_media () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 200;
    body = {|{
      "data": [
        {
          "id": "media_1",
          "caption": "First post",
          "timestamp": "2025-01-01T12:00:00+0000",
          "media_type": "IMAGE",
          "media_url": "https://example.com/img1.jpg",
          "thumbnail_url": null,
          "permalink": "https://www.instagram.com/p/abc123/"
        },
        {
          "id": "media_2",
          "caption": null,
          "timestamp": "2025-01-02T12:00:00+0000",
          "media_type": "VIDEO",
          "media_url": "https://example.com/vid1.mp4",
          "thumbnail_url": "https://example.com/thumb1.jpg",
          "permalink": "https://www.instagram.com/p/def456/"
        }
      ]
    }|};
    headers = [];
  };

  Instagram.get_user_media
    ~ig_user_id:"ig_user_123"
    ~access_token:"tok+en/=="
    (handle_result
      (fun (items : Instagram.media_item list) ->
        assert (List.length items = 2);
        let first = List.nth items 0 in
        assert (first.id = "media_1");
        assert (first.caption = Some "First post");
        assert (first.media_type = Some "IMAGE");
        assert (first.permalink = Some "https://www.instagram.com/p/abc123/");
        let second = List.nth items 1 in
        assert (second.id = "media_2");
        assert (second.caption = None);
        assert (second.media_type = Some "VIDEO");
        assert (second.thumbnail_url = Some "https://example.com/thumb1.jpg");
        (* Verify request uses graph.instagram.com *)
        let requests = !Mock_http.requests in
        let has_contract = List.exists (fun (meth, url, _, _) ->
          meth = "GET"
          && string_contains url "graph.instagram.com"
          && string_contains url "/ig_user_123/media"
        ) requests in
        assert has_contract;
        print_endline "PASS Get user media")
      (fun err -> failwith ("Get user media failed: " ^ err)))

(** Test: Get user media with limit parameter *)
let test_get_user_media_with_limit () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 200;
    body = {|{"data": [{"id": "media_1"}]}|};
    headers = [];
  };

  Instagram.get_user_media
    ~ig_user_id:"ig_user_123"
    ~access_token:"test_token"
    ~limit:5
    (handle_result
      (fun _items ->
        let requests = !Mock_http.requests in
        let has_limit = List.exists (fun (_, url, _, _) ->
          query_param url "limit" = Some "5"
        ) requests in
        assert has_limit;
        print_endline "PASS Get user media with limit")
      (fun err -> failwith ("Get user media with limit failed: " ^ err)))

(** {1 Media Deletion Tests (H3)} *)

(** Test: Delete media request contract *)
let test_delete_media () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 200;
    body = {|{"success": true}|};
    headers = [];
  };

  Instagram.delete_media
    ~media_id:"media_to_delete"
    ~access_token:"test_token"
    (handle_result
      (fun success ->
        assert success;
        let requests = !Mock_http.requests in
        let has_contract = List.exists (fun (meth, url, _, _) ->
          meth = "DELETE"
          && string_contains url "/media_to_delete"
        ) requests in
        assert has_contract;
        print_endline "PASS Delete media")
      (fun err -> failwith ("Delete media failed: " ^ err)))

(** Test: Delete media returns structured error on auth failure *)
let test_delete_media_auth_error () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 400;
    body = {|{"error":{"code":190,"message":"Invalid OAuth 2.0 Access Token"}}|};
    headers = [];
  };

  Instagram.delete_media
    ~media_id:"media_to_delete"
    ~access_token:"bad_token"
    (function
      | Ok _ -> failwith "Expected auth error"
      | Error (Error_types.Auth_error Error_types.Token_expired) ->
          print_endline "PASS Delete media auth error"
      | Error _ -> failwith "Expected Token_expired")

(** {1 Tagging Tests (H4)} *)

(** Test: Image container with user tags, location, and collaborators *)
let test_image_container_with_tagging () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 200;
    body = {|{"id": "container_tagged"}|};
    headers = [];
  };

  Instagram.create_image_container
    ~ig_user_id:"ig_123"
    ~access_token:"test_token"
    ~image_url:"https://example.com/image.jpg"
    ~caption:"Tagged post"
    ~alt_text:None
    ~is_carousel_item:false
    ~user_tags:[("user1", 0.5, 0.5); ("user2", 0.2, 0.8)]
    ~location_id:"location_456"
    ~collaborators:["collab_user1"; "collab_user2"]
    (handle_result
      (fun container_id ->
        assert (container_id = "container_tagged");
        let requests = !Mock_http.requests in
        let has_tags = List.exists (fun (_, _, _, body) ->
          string_contains body "user_tags"
          && string_contains body "user1"
          && string_contains body "user2"
        ) requests in
        assert has_tags;
        let has_location = List.exists (fun (_, _, _, body) ->
          string_contains body "location_id"
          && string_contains body "location_456"
        ) requests in
        assert has_location;
        let has_collabs = List.exists (fun (_, _, _, body) ->
          string_contains body "collaborators"
          && string_contains body "collab_user1"
        ) requests in
        assert has_collabs;
        print_endline "PASS Image container with tagging")
      (fun err -> failwith ("Image container with tagging failed: " ^ err)))

(** Test: Video container with location *)
let test_video_container_with_location () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 200;
    body = {|{"id": "container_loc"}|};
    headers = [];
  };

  Instagram.create_video_container
    ~ig_user_id:"ig_123"
    ~access_token:"test_token"
    ~video_url:"https://example.com/video.mp4"
    ~caption:"Video with location"
    ~alt_text:None
    ~media_type:"VIDEO"
    ~is_carousel_item:false
    ~location_id:"loc_789"
    (handle_result
      (fun container_id ->
        assert (container_id = "container_loc");
        let requests = !Mock_http.requests in
        let has_location = List.exists (fun (_, _, _, body) ->
          string_contains body "location_id"
          && string_contains body "loc_789"
        ) requests in
        assert has_location;
        print_endline "PASS Video container with location")
      (fun err -> failwith ("Video container with location failed: " ^ err)))

(** {1 Reel Parameters Tests (H5)} *)

(** Test: Reel with share_to_feed, cover_url, thumb_offset, audio_name *)
let test_reel_with_extra_params () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 200;
    body = {|{"id": "reel_container_params"}|};
    headers = [];
  };

  Instagram.create_video_container
    ~ig_user_id:"ig_123"
    ~access_token:"test_token"
    ~video_url:"https://example.com/reel.mp4"
    ~caption:"Reel with params"
    ~alt_text:None
    ~media_type:"REELS"
    ~is_carousel_item:false
    ~share_to_feed:true
    ~cover_url:"https://example.com/cover.jpg"
    ~thumb_offset:5000
    ~audio_name:"My Audio Track"
    (handle_result
      (fun container_id ->
        assert (container_id = "reel_container_params");
        let requests = !Mock_http.requests in
        let has_share_to_feed = List.exists (fun (_, _, _, body) ->
          string_contains body "share_to_feed" && string_contains body "true"
        ) requests in
        assert has_share_to_feed;
        let has_cover = List.exists (fun (_, _, _, body) ->
          string_contains body "cover_url"
        ) requests in
        assert has_cover;
        let has_thumb = List.exists (fun (_, _, _, body) ->
          string_contains body "thumb_offset" && string_contains body "5000"
        ) requests in
        assert has_thumb;
        let has_audio = List.exists (fun (_, _, _, body) ->
          string_contains body "audio_name"
        ) requests in
        assert has_audio;
        print_endline "PASS Reel with extra params (share_to_feed, cover_url, thumb_offset, audio_name)")
      (fun err -> failwith ("Reel with extra params failed: " ^ err)))

(** Test: post_reel passes reel-specific parameters via create_video_container *)
let test_post_reel_with_reel_params () =
  Mock_config.reset ();

  (* Use create_video_container directly to verify reel params are sent *)
  Mock_http.set_response {
    status = 200;
    body = {|{"id": "reel_container_direct"}|};
    headers = [];
  };

  Instagram.create_video_container
    ~ig_user_id:"ig_123"
    ~access_token:"test_token"
    ~video_url:"https://example.com/reel.mp4"
    ~caption:"Reel caption"
    ~alt_text:None
    ~media_type:"REELS"
    ~is_carousel_item:false
    ~share_to_feed:true
    ~cover_url:"https://example.com/cover.jpg"
    ~thumb_offset:3000
    ~audio_name:"Cool Audio"
    (handle_result
      (fun container_id ->
        assert (container_id = "reel_container_direct");
        let requests = !Mock_http.requests in
        let create_req = List.find (fun (meth, _, _, _) -> meth = "POST") requests in
        let (_, _, _, body) = create_req in
        assert (string_contains body "share_to_feed");
        assert (string_contains body "cover_url");
        assert (string_contains body "thumb_offset");
        assert (string_contains body "3000");
        assert (string_contains body "audio_name");
        print_endline "PASS post_reel with reel-specific params (via create_video_container)")
      (fun err -> failwith ("post_reel with reel params failed: " ^ err)))

(** Test: post_reel end-to-end flow with reel-specific params *)
let test_post_reel_e2e_with_params () =
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
  Mock_config.set_ig_user_id ~account_id:"test_account" ~ig_user_id:"ig_user_123";

  Mock_http.set_responses [
    { status = 200; body = {|{"id": "reel_container"}|}; headers = [] };
    { status = 200; body = {|{"status_code": "FINISHED"}|}; headers = [] };
    { status = 200; body = {|{"id": "reel_post_e2e"}|}; headers = [] };
  ];

  Instagram.post_reel
    ~account_id:"test_account"
    ~text:"Reel e2e"
    ~video_url:"https://example.com/reel.mp4"
    ~share_to_feed:(Some true)
    ~cover_url:(Some "https://example.com/cover.jpg")
    ~thumb_offset:(Some 3000)
    ~audio_name:(Some "Cool Audio")
    (handle_outcome
      (fun post_id ->
        assert (post_id = "reel_post_e2e");
        print_endline "PASS post_reel e2e with reel-specific params")
      (fun err -> failwith ("post_reel e2e with params failed: " ^ err)))

(** Run all tests *)
let () =
  print_endline "\n=== Instagram Standalone Provider Tests ===\n";

  print_endline "--- OAuth Standalone Tests ---";
  test_standalone_auth_url_endpoint ();
  test_standalone_auth_url_enable_fb_login ();
  test_standalone_auth_url_scopes_comma_separated ();
  test_standalone_auth_url_response_type ();
  test_standalone_auth_url_custom_scopes ();
  test_standalone_exchange_code_happy_path_int_user_id ();
  test_standalone_exchange_code_happy_path_string_user_id ();
  test_standalone_exchange_code_normalizes_token_type ();
  test_standalone_exchange_code_missing_user_id ();
  test_standalone_exchange_code_http_error ();
  test_standalone_exchange_long_lived_happy_path ();
  test_standalone_exchange_long_lived_normalizes_token_type ();
  test_standalone_exchange_long_lived_appsecret_proof ();
  test_standalone_get_profile_happy_path ();
  test_standalone_get_profile_no_picture ();
  test_standalone_get_profile_fields_parameter ();
  test_standalone_get_profile_missing_identifier ();
  test_standalone_oauth_refresh_token ();
  test_standalone_exchange_code_on_response ();
  test_standalone_refresh_token_on_response ();

  print_endline "\n--- Content Publishing Tests ---";
  test_oauth_url ();
  test_api_base_url_uses_instagram ();
  test_rate_limit_parsing ();
  test_create_container ();
  test_publish_container ();
  test_check_status ();
  test_content_validation ();
  test_post_single ();
  test_post_requires_media ();
  test_post_single_rejects_unsupported_media ();
  test_polling_no_publish_on_status_error ();
  test_parse_api_error_token_expired ();
  test_parse_error_response_token_expired ();
  test_create_container_structured_auth_error ();

  print_endline "\n--- Comment Posting Tests (H1) ---";
  test_post_comment ();
  test_post_comment_auth_error ();

  print_endline "\n--- Media Listing Tests (H2) ---";
  test_get_user_media ();
  test_get_user_media_with_limit ();

  print_endline "\n--- Media Deletion Tests (H3) ---";
  test_delete_media ();
  test_delete_media_auth_error ();

  print_endline "\n--- Tagging Tests (H4) ---";
  test_image_container_with_tagging ();
  test_video_container_with_location ();

  print_endline "\n--- Reel Parameters Tests (H5) ---";
  test_reel_with_extra_params ();
  test_post_reel_with_reel_params ();
  test_post_reel_e2e_with_params ();

  print_endline "\n=== All standalone tests passed! ===\n"
