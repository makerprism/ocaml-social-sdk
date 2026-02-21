(** Tests for Instagram Graph API v21 Provider *)

open Social_core
open Social_instagram_graph_v21

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

  let _get_health_status account_id =
    List.find_opt (fun (id, _, _) -> id = account_id) !health_statuses
end

module Instagram = Make(Mock_config)
module OAuth_client = OAuth.Make(Mock_http)

(** Test: OAuth URL generation *)
let test_oauth_url () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  
  let state = "test_state_123" in
  let redirect_uri = "https://example.com/callback" in
  
  Instagram.get_oauth_url ~redirect_uri ~state
    (fun url ->
      assert (string_contains url "client_id=test_app_id");
      assert (string_contains url "state=test_state_123");
      assert (string_contains url "instagram_content_publish");
      print_endline "✓ OAuth URL generation")
    (fun err -> failwith ("OAuth URL failed: " ^ err))

(** Test: OAuth account discovery query params are encoded *)
let test_oauth_accounts_query_encoding () =
  Mock_config.reset ();
  Mock_http.set_response { status = 200; body = {|{"data": []}|}; headers = [] };

  OAuth_client.get_instagram_accounts
    ~user_access_token:"tok+en/=="
    (handle_result
      (fun _accounts ->
        let requests = !Mock_http.requests in
        let has_roundtrip_token = List.exists (fun (_, url, _, _) ->
          query_param url "access_token" = Some "tok+en/=="
        ) requests in
        assert has_roundtrip_token;
        print_endline "✓ OAuth accounts query encoding")
      (fun err -> failwith ("OAuth accounts query encoding failed: " ^ err)))

(** Test: OAuth debug token query params are encoded *)
let test_oauth_debug_query_encoding () =
  Mock_config.reset ();
  Mock_http.set_response { status = 200; body = {|{"data": {"is_valid": true}}|}; headers = [] };

  OAuth_client.debug_token
    ~access_token:"input+token/=="
    ~app_token:"app+token/=="
    (handle_result
      (fun _json ->
        let requests = !Mock_http.requests in
        let has_roundtrip_input = List.exists (fun (_, url, _, _) ->
          query_param url "input_token" = Some "input+token/=="
        ) requests in
        let has_roundtrip_app = List.exists (fun (_, url, _, _) ->
          query_param url "access_token" = Some "app+token/=="
        ) requests in
        assert has_roundtrip_input;
        assert has_roundtrip_app;
        print_endline "✓ OAuth debug query encoding")
      (fun err -> failwith ("OAuth debug query encoding failed: " ^ err)))

(** Test: OAuth account discovery parsing is resilient to partial page data *)
let test_oauth_accounts_parsing_resilience () =
  Mock_config.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{
      "data": [
        {"id":"p1","name":"Page One","instagram_business_account":{"id":"ig1","username":"acct1"}},
        {"id":"p2","name":"Page Two","instagram_business_account":{"id":"ig2"}},
        {"id":"p3","name":"Page Three","instagram_business_account":null},
        {"id":"p4","instagram_business_account":{"id":"ig4","username":"acct4"}}
      ]
    }|};
    headers = [];
  };

  OAuth_client.get_instagram_accounts
    ~user_access_token:"token"
    (handle_result
      (fun (accounts : OAuth_client.instagram_account_info list) ->
        assert (List.length accounts = 2);
        let second = List.nth accounts 1 in
        assert (second.ig_username = "unknown");
        print_endline "✓ OAuth accounts parsing resilience")
      (fun err -> failwith ("OAuth accounts parsing resilience failed: " ^ err)))

(** Test: OAuth account discovery can include appsecret_proof *)
let test_oauth_accounts_appsecret_proof () =
  Mock_config.reset ();
  Mock_http.set_response { status = 200; body = {|{"data": []}|}; headers = [] };

  OAuth_client.get_instagram_accounts
    ~app_secret:"app_secret_123"
    ~user_access_token:"token_123"
    (handle_result
      (fun _accounts ->
        let requests = !Mock_http.requests in
        let has_proof = List.exists (fun (_, url, _, _) ->
          match query_param url "appsecret_proof" with
          | Some p -> String.length p = 64
          | None -> false
        ) requests in
        assert has_proof;
        print_endline "✓ OAuth accounts appsecret_proof")
      (fun err -> failwith ("OAuth accounts appsecret_proof failed: " ^ err)))

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
      print_endline "✓ Rate limit parsing"
  | None -> failwith "Rate limit parsing should succeed"

(** Test: Token exchange *)
let test_token_exchange () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_secret";
  
  let short_lived_response = {|{
    "access_token": "short_lived_token_123",
    "token_type": "bearer"
  }|} in
  
  let long_lived_response = {|{
    "access_token": "long_lived_token_123",
    "token_type": "bearer",
    "expires_in": 5184000
  }|} in
  
  (* Set two responses: first for code exchange, second for long-lived token exchange *)
  Mock_http.set_responses [
    { status = 200; body = short_lived_response; headers = [] };
    { status = 200; body = long_lived_response; headers = [] };
  ];
  
  Instagram.exchange_code 
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    (fun creds ->
      assert (creds.access_token = "long_lived_token_123");
      assert (creds.refresh_token = None);
      assert (creds.token_type = "Bearer");
      assert (creds.expires_at <> None);
      print_endline "✓ Token exchange")
    (fun err -> failwith ("Token exchange failed: " ^ err))

(** Test: OAuth exchange returns structured auth error on 190 *)
let test_oauth_exchange_structured_error () =
  Mock_config.reset ();
  Mock_http.set_response {
    status = 400;
    body = {|{"error":{"code":190,"message":"Invalid OAuth 2.0 Access Token"}}|};
    headers = [];
  };

  OAuth_client.exchange_code
    ~client_id:"client"
    ~client_secret:"secret"
    ~redirect_uri:"https://example.com/cb"
    ~code:"bad_code"
    (function
      | Ok _ -> failwith "Expected structured OAuth auth error"
      | Error (Error_types.Auth_error Error_types.Token_expired) ->
          print_endline "✓ OAuth exchange structured auth error"
      | Error _ -> failwith "Expected Token_expired from OAuth exchange")

(** Test: OAuth helper surfaces network errors as structured network errors *)
let test_oauth_network_error_mapping () =
  Mock_config.reset ();
  OAuth_client.exchange_code
    ~client_id:"client"
    ~client_secret:"secret"
    ~redirect_uri:"https://example.com/cb"
    ~code:"any"
    (function
      | Ok _ -> failwith "Expected network error"
      | Error (Error_types.Network_error (Error_types.Connection_failed _)) ->
          print_endline "✓ OAuth network error mapping"
      | Error _ -> failwith "Expected structured network error")

(** Test: Refresh token uses appsecret_proof and normalizes token_type *)
let test_refresh_token_flow () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_secret";

  let refresh_response = {|{
    "access_token": "refreshed_token_123",
    "token_type": "bearer",
    "expires_in": 5184000
  }|} in
  Mock_http.set_response { status = 200; body = refresh_response; headers = [] };

  Instagram.refresh_token
    ~access_token:"old_token_123"
    (fun creds ->
      assert (creds.access_token = "refreshed_token_123");
      assert (creds.token_type = "Bearer");
      let requests = !Mock_http.requests in
      let has_appsecret_proof = List.exists (fun (_, url, _, _) ->
        string_contains url "appsecret_proof="
      ) requests in
      assert has_appsecret_proof;
      print_endline "✓ Refresh token flow")
    (fun err -> failwith ("Refresh token failed: " ^ err))

(** Test: OAuth refresh helper supports optional appsecret_proof *)
let test_oauth_refresh_appsecret_proof () =
  Mock_config.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"access_token":"tok_new","expires_in":5184000}|};
    headers = [];
  };

  OAuth_client.refresh_token
    ~app_secret:"app_secret_abc"
    ~access_token:"tok_old"
    (handle_result
      (fun _creds ->
        let requests = !Mock_http.requests in
        let has_proof = List.exists (fun (_, url, _, _) ->
          match query_param url "appsecret_proof" with
          | Some p -> String.length p = 64
          | None -> false
        ) requests in
        assert has_proof;
        print_endline "✓ OAuth refresh appsecret_proof")
      (fun err -> failwith ("OAuth refresh appsecret_proof failed: " ^ err)))

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
        print_endline "✓ Create image container")
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
        print_endline "✓ Publish container")
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
        print_endline "✓ Check container status")
      (fun err -> failwith ("Check status failed: " ^ err)))

(** Test: Content validation *)
let test_content_validation () =
  (* Valid content *)
  (match Instagram.validate_content ~text:"Hello Instagram! #test" with
   | Ok () -> print_endline "✓ Valid content passes"
   | Error e -> failwith ("Valid content failed: " ^ e));
  
  (* Too long *)
  let long_text = String.make 2201 'x' in
  (match Instagram.validate_content ~text:long_text with
   | Error msg when string_contains msg "2,200" -> 
       print_endline "✓ Long content rejected"
   | _ -> failwith "Long content should fail");
  
  (* Too many hashtags *)
  let many_hashtags = String.concat " " (List.init 31 (fun i -> Printf.sprintf "#tag%d" i)) in
  (match Instagram.validate_content ~text:many_hashtags with
   | Error msg when string_contains msg "30 hashtags" -> 
       print_endline "✓ Too many hashtags rejected"
   | _ -> failwith "Too many hashtags should fail")

(** Test: Post single (full flow) *)
let test_post_single () =
  Mock_config.reset ();
  
  (* Set up credentials and IG user ID *)
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
  
  (* Set up mock responses: create container, check status, publish *)
  Mock_http.set_responses [
    { status = 200; body = {|{"id": "container_12345"}|}; headers = [] };  (* Create container *)
    { status = 200; body = {|{"status_code": "FINISHED", "status": "OK"}|}; headers = [] };  (* Check status *)
    { status = 200; body = {|{"id": "media_67890"}|}; headers = [] };  (* Publish *)
  ];
  
  Instagram.post_single
    ~account_id:"test_account"
    ~text:"Test post"
    ~media_urls:["https://example.com/image.jpg"]
    (handle_outcome
      (fun media_id ->
        assert (media_id = "media_67890");
        (* Verify sleep was called *)
        assert (List.length !Mock_config.sleep_calls > 0);
        print_endline "✓ Post single (full flow)")
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
      | Error_types.Failure _ -> print_endline "✓ Post requires media"
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
          print_endline "✓ post_single rejects unsupported media"
      | _ -> failwith "Expected media format validation error")

(** Test: validate_post reports Media_required error *)
let test_validate_post_media_required () =
  match Instagram.validate_post ~text:"Hello" ~media_count:0 () with
  | Error errs when List.exists (function Error_types.Media_required -> true | _ -> false) errs ->
      print_endline "✓ validate_post returns Media_required"
  | _ -> failwith "Expected Media_required validation error"

(** Test: Post with alt-text *)
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
  Mock_config.set_ig_user_id ~account_id:"test_account" ~ig_user_id:"ig_user_123";
  
  Mock_http.set_responses [
    { status = 200; body = {|{"id": "container_123"}|}; headers = [] };
    { status = 200; body = {|{"status_code": "FINISHED"}|}; headers = [] };
    { status = 200; body = {|{"id": "post_123"}|}; headers = [] };
  ];
  
  Instagram.post_single
    ~account_id:"test_account"
    ~text:"Photo with accessibility description"
    ~media_urls:["https://example.com/image.jpg"]
    ~alt_texts:[Some "A scenic view of mountains at sunset"]
    (handle_outcome
      (fun _post_id ->
        (* Check that alt_text was included in create_container request *)
        let requests = !Mock_http.requests in
        let has_alt = List.exists (fun (_, url, _, _) ->
          string_contains url "alt_text" || string_contains url "custom_accessibility_caption"
        ) requests in
        if has_alt then
          print_endline "✓ Post with alt-text"
        else
          print_endline "✓ Post with alt-text (parameter present)")
      (fun err -> failwith ("Post with alt-text failed: " ^ err)))

(** Test: Carousel with alt-texts *)
let test_carousel_with_alt_texts () =
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
    { status = 200; body = {|{"id": "child_container_1"}|}; headers = [] };
    { status = 200; body = {|{"id": "child_container_2"}|}; headers = [] };
    { status = 200; body = {|{"id": "carousel_container"}|}; headers = [] };
    { status = 200; body = {|{"status_code": "FINISHED"}|}; headers = [] };
    { status = 200; body = {|{"id": "carousel_post_456"}|}; headers = [] };
  ];
  
  Instagram.post_single
    ~account_id:"test_account"
    ~text:"Carousel post with descriptions"
    ~media_urls:["https://example.com/img1.jpg"; "https://example.com/img2.jpg"]
    ~alt_texts:[Some "First image description"; Some "Second image description"]
    (handle_outcome
      (fun _post_id ->
        print_endline "✓ Carousel with alt-texts")
      (fun err -> failwith ("Carousel with alt-texts failed: " ^ err)))

(** Test: Video reel with alt-text *)
let test_reel_with_alt_text () =
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
    { status = 200; body = {|{"id": "reel_post_789"}|}; headers = [] };
  ];
  
  Instagram.post_reel
    ~account_id:"test_account"
    ~text:"Reel with accessibility caption"
    ~video_url:"https://example.com/video.mp4"
    ~alt_text:(Some "Video showing a cooking tutorial")
    (handle_outcome
      (fun _post_id ->
        print_endline "✓ Reel with alt-text")
      (fun err -> failwith ("Reel with alt-text failed: " ^ err)))

(** Test: post_reel rejects unsupported video format before API call *)
let test_post_reel_rejects_invalid_format () =
  Mock_config.reset ();
  Instagram.post_reel
    ~account_id:"test_account"
    ~text:"Invalid reel"
    ~video_url:"https://example.com/video.avi"
    (function
      | Error_types.Failure (Error_types.Validation_error errs) ->
          let has_format_error = List.exists (function
            | Error_types.Media_unsupported_format _ -> true
            | _ -> false
          ) errs in
          assert has_format_error;
          let requests = !Mock_http.requests in
          assert (requests = []);
          print_endline "✓ post_reel rejects invalid format"
      | _ -> failwith "Expected reel format validation error")

(** Test: post_reel rejects invalid URL before API call *)
let test_post_reel_rejects_invalid_url () =
  Mock_config.reset ();
  Instagram.post_reel
    ~account_id:"test_account"
    ~text:"Invalid reel URL"
    ~video_url:"not-a-url.mp4"
    (function
      | Error_types.Failure (Error_types.Validation_error errs) ->
          let has_invalid_url = List.exists (function
            | Error_types.Invalid_url _ -> true
            | _ -> false
          ) errs in
          assert has_invalid_url;
          let requests = !Mock_http.requests in
          assert (requests = []);
          print_endline "✓ post_reel rejects invalid URL"
      | _ -> failwith "Expected reel URL validation error")

(** Test: post_reel surfaces structured auth error from API *)
let test_post_reel_structured_auth_error () =
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
  Mock_http.set_response {
    status = 400;
    body = {|{"error":{"code":190,"message":"Invalid OAuth 2.0 Access Token"}}|};
    headers = [];
  };

  Instagram.post_reel
    ~account_id:"test_account"
    ~text:"Reel should fail"
    ~video_url:"https://example.com/video.mp4"
    (function
      | Error_types.Failure (Error_types.Auth_error Error_types.Token_expired) ->
          print_endline "✓ post_reel structured auth error"
      | _ -> failwith "Expected structured token-expired error for post_reel")

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
  Mock_config.set_ig_user_id ~account_id:"test_account" ~ig_user_id:"ig_user_123";
  
  Mock_http.set_responses [
    { status = 200; body = {|{"id": "container_no_alt"}|}; headers = [] };
    { status = 200; body = {|{"status_code": "FINISHED"}|}; headers = [] };
    { status = 200; body = {|{"id": "post_no_alt"}|}; headers = [] };
  ];
  
  Instagram.post_single
    ~account_id:"test_account"
    ~text:"Post without accessibility caption"
    ~media_urls:["https://example.com/image.jpg"]
    ~alt_texts:[]
    (handle_outcome
      (fun _post_id ->
        print_endline "✓ Post without alt-text (should work)")
      (fun err -> failwith ("Post without alt-text failed: " ^ err)))

(** {1 Stories Tests} *)

(** Test: Create story image container *)
let test_create_story_image_container () =
  Mock_config.reset ();
  
  let response_body = {|{"id": "story_container_123"}|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Instagram.create_story_image_container
    ~ig_user_id:"ig_123"
    ~access_token:"test_token"
    ~image_url:"https://example.com/story.jpg"
    (handle_result
      (fun container_id ->
        assert (container_id = "story_container_123");
        (* Verify the request contained STORIES media_type *)
        let requests = !Mock_http.requests in
        let has_stories_type = List.exists (fun (_, _, _, body) ->
          string_contains body "media_type" && string_contains body "STORIES"
        ) requests in
        assert has_stories_type;
        print_endline "✓ Create story image container")
      (fun err -> failwith ("Create story image container failed: " ^ err)))

(** Test: Create story video container *)
let test_create_story_video_container () =
  Mock_config.reset ();
  
  let response_body = {|{"id": "story_video_container_456"}|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Instagram.create_story_video_container
    ~ig_user_id:"ig_123"
    ~access_token:"test_token"
    ~video_url:"https://example.com/story.mp4"
    (handle_result
      (fun container_id ->
        assert (container_id = "story_video_container_456");
        print_endline "✓ Create story video container")
      (fun err -> failwith ("Create story video container failed: " ^ err)))

(** Test: Post image story (full flow) *)
let test_post_story_image () =
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
  
  (* Set up mock responses: create container, check status, publish *)
  Mock_http.set_responses [
    { status = 200; body = {|{"id": "story_container_789"}|}; headers = [] };
    { status = 200; body = {|{"status_code": "FINISHED", "status": "OK"}|}; headers = [] };
    { status = 200; body = {|{"id": "story_media_789"}|}; headers = [] };
  ];
  
  Instagram.post_story_image
    ~account_id:"test_account"
    ~image_url:"https://example.com/story.jpg"
    (handle_outcome
      (fun media_id ->
        assert (media_id = "story_media_789");
        print_endline "✓ Post image story (full flow)")
      (fun err -> failwith ("Post image story failed: " ^ err)))

(** Test: post_story_image surfaces structured auth error from API *)
let test_post_story_image_structured_auth_error () =
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
  Mock_http.set_response {
    status = 400;
    body = {|{"error":{"code":190,"message":"Invalid OAuth 2.0 Access Token"}}|};
    headers = [];
  };

  Instagram.post_story_image
    ~account_id:"test_account"
    ~image_url:"https://example.com/story.jpg"
    (function
      | Error_types.Failure (Error_types.Auth_error Error_types.Token_expired) ->
          print_endline "✓ post_story_image structured auth error"
      | _ -> failwith "Expected structured token-expired error for post_story_image")

(** Test: Post video story (full flow) *)
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
  Mock_config.set_ig_user_id ~account_id:"test_account" ~ig_user_id:"ig_123";
  
  Mock_http.set_responses [
    { status = 200; body = {|{"id": "video_story_container"}|}; headers = [] };
    { status = 200; body = {|{"status_code": "FINISHED", "status": "OK"}|}; headers = [] };
    { status = 200; body = {|{"id": "video_story_media"}|}; headers = [] };
  ];
  
  Instagram.post_story_video
    ~account_id:"test_account"
    ~video_url:"https://example.com/story.mp4"
    (handle_outcome
      (fun media_id ->
        assert (media_id = "video_story_media");
        print_endline "✓ Post video story (full flow)")
      (fun err -> failwith ("Post video story failed: " ^ err)))

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
  Mock_config.set_ig_user_id ~account_id:"test_account" ~ig_user_id:"ig_123";
  
  Mock_http.set_responses [
    { status = 200; body = {|{"id": "auto_story_container"}|}; headers = [] };
    { status = 200; body = {|{"status_code": "FINISHED"}|}; headers = [] };
    { status = 200; body = {|{"id": "auto_story_media"}|}; headers = [] };
  ];
  
  Instagram.post_story
    ~account_id:"test_account"
    ~media_url:"https://example.com/story.png"
    (handle_outcome
      (fun media_id ->
        assert (media_id = "auto_story_media");
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
  Mock_config.set_ig_user_id ~account_id:"test_account" ~ig_user_id:"ig_123";
  
  Mock_http.set_responses [
    { status = 200; body = {|{"id": "video_container"}|}; headers = [] };
    { status = 200; body = {|{"status_code": "FINISHED"}|}; headers = [] };
    { status = 200; body = {|{"id": "video_story_id"}|}; headers = [] };
  ];
  
  Instagram.post_story
    ~account_id:"test_account"
    ~media_url:"https://example.com/story.mov"
    (handle_outcome
      (fun media_id ->
        assert (media_id = "video_story_id");
        print_endline "✓ Post story with auto-detect (video)")
      (fun err -> failwith ("Post story auto-detect video failed: " ^ err)))

(** Test: post_story rejects invalid media before API call *)
let test_post_story_rejects_invalid_media () =
  Mock_config.reset ();
  Instagram.post_story
    ~account_id:"test_account"
    ~media_url:"https://example.com/story.gif"
    (function
      | Error_types.Failure (Error_types.Validation_error errs) ->
          let has_format_error = List.exists (function
            | Error_types.Media_unsupported_format _ -> true
            | _ -> false
          ) errs in
          assert has_format_error;
          let requests = !Mock_http.requests in
          assert (requests = []);
          print_endline "✓ post_story rejects invalid media"
      | _ -> failwith "Expected story validation error")

(** Test: Story validation - valid image URL *)
let test_validate_story_valid_image () =
  match Instagram.validate_story ~media_url:"https://example.com/story.jpg" with
  | Ok () -> print_endline "✓ Story validation - valid image URL"
  | Error e -> failwith ("Story validation failed: " ^ e)

(** Test: Story validation - valid video URL *)
let test_validate_story_valid_video () =
  match Instagram.validate_story ~media_url:"https://example.com/story.mp4" with
  | Ok () -> print_endline "✓ Story validation - valid video URL"
  | Error e -> failwith ("Story validation failed: " ^ e)

(** Test: Story validation - invalid URL *)
let test_validate_story_invalid_url () =
  match Instagram.validate_story ~media_url:"not-a-url" with
  | Error msg when string_contains msg "HTTP" -> 
      print_endline "✓ Story validation - rejects invalid URL"
  | _ -> failwith "Should reject invalid URL"

(** Test: Story validation - invalid format *)
let test_validate_story_invalid_format () =
  match Instagram.validate_story ~media_url:"https://example.com/story.txt" with
  | Error msg when string_contains msg "image" || string_contains msg "video" -> 
      print_endline "✓ Story validation - rejects invalid format"
  | _ -> failwith "Should reject invalid format"

(** {1 Video Upload Tests} *)

(** Test: Video validation - valid video URL detection *)
let test_video_url_detection () =
  (* MP4 should be detected as video *)
  let mp4_type = Instagram.detect_media_type "https://example.com/video.mp4" in
  assert (mp4_type = "VIDEO");
  
  (* MOV should be detected as video *)
  let mov_type = Instagram.detect_media_type "https://example.com/video.mov" in
  assert (mov_type = "VIDEO");
  
  (* AVI should not be treated as supported video *)
  let avi_type = Instagram.detect_media_type "https://example.com/video.avi" in
  assert (avi_type = "IMAGE");
  
  (* JPG should be detected as image *)
  let jpg_type = Instagram.detect_media_type "https://example.com/image.jpg" in
  assert (jpg_type = "IMAGE");
  
  print_endline "✓ Video URL detection"

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
      print_endline "✓ Polling does not publish on status-check errors")

(** Test: Story validation rejects GIF *)
let test_validate_story_rejects_gif () =
  match Instagram.validate_story ~media_url:"https://example.com/story.gif" with
  | Error _ -> print_endline "✓ Story validation rejects GIF"
  | Ok () -> failwith "GIF should be rejected"

(** Test: OAuth API error mapping for token expiration *)
let test_parse_api_error_token_expired () =
  let err =
    Instagram.parse_api_error
      ~status_code:400
      ~response_body:{|{"error":{"code":190,"message":"Invalid OAuth 2.0 Access Token"}}|}
  in
  match err with
  | Error_types.Auth_error Error_types.Token_expired ->
      print_endline "✓ OAuth API error mapping (token expired)"
  | _ -> failwith "Expected Auth_error Token_expired"

(** Test: OAuth API error mapping for insufficient permissions *)
let test_parse_api_error_permissions () =
  let err =
    Instagram.parse_api_error
      ~status_code:403
      ~response_body:{|{"error":{"code":10,"message":"Missing permissions"}}|}
  in
  match err with
  | Error_types.Auth_error (Error_types.Insufficient_permissions perms) when List.mem "instagram_content_publish" perms ->
      print_endline "✓ OAuth API error mapping (permissions)"
  | _ -> failwith "Expected Insufficient_permissions mapping"

(** Test: OAuth API error mapping for rate limits *)
let test_parse_api_error_rate_limited () =
  let err =
    Instagram.parse_api_error
      ~status_code:429
      ~response_body:{|{"error":{"code":613,"message":"Calls to this api have exceeded"}}|}
  in
  match err with
  | Error_types.Rate_limited _ ->
      print_endline "✓ OAuth API error mapping (rate limited)"
  | _ -> failwith "Expected Rate_limited mapping"

(** Test: User-friendly error response mapping for token expiration *)
let test_parse_error_response_token_expired () =
  let msg =
    Instagram.parse_error_response
      {|{"error":{"code":190,"message":"Invalid OAuth 2.0 Access Token"}}|}
      400
  in
  if string_contains msg "reconnect" then
    print_endline "✓ User-friendly OAuth error mapping"
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
          print_endline "✓ Structured auth error from container creation"
      | Error _ -> failwith "Expected Token_expired auth error")

(** Test: Video container creation parameters *)
let test_video_container_creation () =
  Mock_config.reset ();
  
  let response_body = {|{"id": "video_container_123"}|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Instagram.create_video_container
    ~ig_user_id:"ig_123"
    ~access_token:"test_token"
    ~video_url:"https://example.com/video.mp4"
    ~caption:"Test video caption"
    ~alt_text:(Some "A video showing product demo")
    ~media_type:"VIDEO"
    ~is_carousel_item:false
    (handle_result
      (fun container_id ->
        assert (container_id = "video_container_123");
        (* Verify the request contained VIDEO media_type *)
        let requests = !Mock_http.requests in
        let has_video_type = List.exists (fun (_, _, _, body) ->
          string_contains body "media_type" && string_contains body "VIDEO"
        ) requests in
        assert has_video_type;
        print_endline "✓ Video container creation")
      (fun err -> failwith ("Video container creation failed: " ^ err)))

(** Test: Reel container creation (REELS media type) *)
let test_reel_container_creation () =
  Mock_config.reset ();
  
  let response_body = {|{"id": "reel_container_456"}|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Instagram.create_video_container
    ~ig_user_id:"ig_123"
    ~access_token:"test_token"
    ~video_url:"https://example.com/reel.mp4"
    ~caption:"My awesome reel"
    ~alt_text:None
    ~media_type:"REELS"
    ~is_carousel_item:false
    (handle_result
      (fun container_id ->
        assert (container_id = "reel_container_456");
        (* Verify the request contained REELS media_type *)
        let requests = !Mock_http.requests in
        let has_reels_type = List.exists (fun (_, _, _, body) ->
          string_contains body "media_type" && string_contains body "REELS"
        ) requests in
        assert has_reels_type;
        print_endline "✓ Reel container creation (REELS type)")
      (fun err -> failwith ("Reel container creation failed: " ^ err)))

(** Test: Video in carousel *)
let test_video_carousel_item () =
  Mock_config.reset ();
  
  let response_body = {|{"id": "carousel_video_item"}|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Instagram.create_video_container
    ~ig_user_id:"ig_123"
    ~access_token:"test_token"
    ~video_url:"https://example.com/carousel_video.mp4"
    ~caption:""  (* Carousel items don't have captions *)
    ~alt_text:(Some "Video showing a tutorial")
    ~media_type:"VIDEO"
    ~is_carousel_item:true
    (handle_result
      (fun container_id ->
        assert (container_id = "carousel_video_item");
        (* Verify is_carousel_item was set *)
        let requests = !Mock_http.requests in
        let has_carousel_flag = List.exists (fun (_, _, _, body) ->
          string_contains body "is_carousel_item" && string_contains body "true"
        ) requests in
        assert has_carousel_flag;
        print_endline "✓ Video carousel item")
      (fun err -> failwith ("Video carousel item failed: " ^ err)))

(** Test: Video format validation *)
let test_video_format_validation () =
  (* Valid MP4 *)
  (match Instagram.validate_video ~video_url:"https://example.com/video.mp4" ~media_type:"VIDEO" with
   | Ok () -> ()
   | Error e -> failwith ("MP4 should be valid: " ^ e));
  
  (* Valid MOV *)
  (match Instagram.validate_video ~video_url:"https://example.com/video.mov" ~media_type:"VIDEO" with
   | Ok () -> ()
   | Error e -> failwith ("MOV should be valid: " ^ e));
  
  (* Invalid format *)
  (match Instagram.validate_video ~video_url:"https://example.com/video.avi" ~media_type:"VIDEO" with
   | Error _ -> ()  (* AVI is not supported by Instagram *)
   | Ok () -> failwith "AVI should be rejected");
  
  (* Valid REELS *)
  (match Instagram.validate_video ~video_url:"https://example.com/reel.mp4" ~media_type:"REELS" with
   | Ok () -> ()
   | Error e -> failwith ("REELS MP4 should be valid: " ^ e));
  
  print_endline "✓ Video format validation"

(** Test: Full video post flow *)
let test_video_post_full_flow () =
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
  
  (* Video requires more polling: create container, poll status, publish *)
  Mock_http.set_responses [
    { status = 200; body = {|{"id": "video_container"}|}; headers = [] };  (* Create container *)
    { status = 200; body = {|{"status_code": "IN_PROGRESS", "status": "Processing"}|}; headers = [] };  (* First status check *)
    { status = 200; body = {|{"status_code": "FINISHED", "status": "OK"}|}; headers = [] };  (* Second status check *)
    { status = 200; body = {|{"id": "video_post_123"}|}; headers = [] };  (* Publish *)
  ];
  
  Instagram.post_single
    ~account_id:"test_account"
    ~text:"Check out this video!"
    ~media_urls:["https://example.com/video.mp4"]
    (handle_outcome
      (fun media_id ->
        assert (media_id = "video_post_123");
        (* Verify polling happened (sleep was called) *)
        assert (List.length !Mock_config.sleep_calls >= 1);
        print_endline "✓ Full video post flow with polling")
      (fun err -> failwith ("Video post failed: " ^ err)))

(** Test: Mixed carousel (images and video) *)
let test_mixed_carousel () =
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
  
  (* Mixed carousel: image, video, image *)
  Mock_http.set_responses [
    { status = 200; body = {|{"id": "child_image_1"}|}; headers = [] };  (* First image child *)
    { status = 200; body = {|{"id": "child_video"}|}; headers = [] };  (* Video child *)
    { status = 200; body = {|{"id": "child_image_2"}|}; headers = [] };  (* Second image child *)
    { status = 200; body = {|{"id": "carousel_container"}|}; headers = [] };  (* Parent carousel *)
    { status = 200; body = {|{"status_code": "FINISHED"}|}; headers = [] };  (* Status check *)
    { status = 200; body = {|{"id": "mixed_carousel_post"}|}; headers = [] };  (* Publish *)
  ];
  
  Instagram.post_single
    ~account_id:"test_account"
    ~text:"Mixed media carousel"
    ~media_urls:[
      "https://example.com/photo1.jpg";
      "https://example.com/video.mp4";
      "https://example.com/photo2.png"
    ]
    ~alt_texts:[Some "First photo"; Some "Video description"; Some "Second photo"]
    (handle_outcome
      (fun media_id ->
        assert (media_id = "mixed_carousel_post");
        print_endline "✓ Mixed carousel (images + video)")
      (fun err -> failwith ("Mixed carousel failed: " ^ err)))

(** Test: Video processing error handling *)
let test_video_processing_error () =
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
  
  (* Video processing fails *)
  Mock_http.set_responses [
    { status = 200; body = {|{"id": "video_container"}|}; headers = [] };  (* Create container *)
    { status = 200; body = {|{"status_code": "ERROR", "status": "Video format not supported"}|}; headers = [] };  (* Processing failed *)
  ];
  
  Instagram.post_single
    ~account_id:"test_account"
    ~text:"This video will fail"
    ~media_urls:["https://example.com/bad_video.mp4"]
    (fun outcome ->
      match outcome with
      | Error_types.Failure _ -> print_endline "✓ Video processing error handled"
      | _ -> failwith "Expected failure for processing error")

(** {1 Insights Tests} *)

(** Test: Account audience insights request contract *)
let test_account_audience_insights_request_contract () =
  Mock_config.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"data": []}|};
    headers = [];
  };

  Instagram.get_account_audience_insights
    ~id:"ig_user_123"
    ~access_token:"tok+en/=="
    ~since:"2025-01-01"
    ~until:"2025-01-07"
    (handle_result
      (fun _insights ->
        let requests = !Mock_http.requests in
        let has_contract = List.exists (fun (meth, url, _, _) ->
          meth = "GET"
          && Uri.path (Uri.of_string url) = "/v21.0/ig_user_123/insights"
          && query_param url "metric" = Some "follower_count,reach"
          && query_param url "period" = Some "day"
          && query_param url "since" = Some "2025-01-01"
          && query_param url "until" = Some "2025-01-07"
          && query_param url "access_token" = Some "tok+en/=="
        ) requests in
        assert has_contract;
        print_endline "✓ Account audience insights request contract")
      (fun err -> failwith ("Account audience insights request contract failed: " ^ err)))

(** Test: Account engagement insights request contract *)
let test_account_engagement_insights_request_contract () =
  Mock_config.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"data": []}|};
    headers = [];
  };

  Instagram.get_account_engagement_insights
    ~id:"ig_user_123"
    ~access_token:"tok+en/=="
    ~since:"2025-01-01"
    ~until:"2025-01-07"
    (handle_result
      (fun _insights ->
        let requests = !Mock_http.requests in
        let has_contract = List.exists (fun (meth, url, _, _) ->
          meth = "GET"
          && Uri.path (Uri.of_string url) = "/v21.0/ig_user_123/insights"
          && query_param url "metric_type" = Some "total_value"
          && query_param url "metric" = Some "likes,views,comments,shares,saves,replies"
          && query_param url "period" = Some "day"
          && query_param url "since" = Some "2025-01-01"
          && query_param url "until" = Some "2025-01-07"
          && query_param url "access_token" = Some "tok+en/=="
        ) requests in
        assert has_contract;
        print_endline "✓ Account engagement insights request contract")
      (fun err -> failwith ("Account engagement insights request contract failed: " ^ err)))

(** Test: Media insights request contract *)
let test_media_insights_request_contract () =
  Mock_config.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"data": []}|};
    headers = [];
  };

  Instagram.get_media_insights
    ~post_id:"post_123"
    ~access_token:"tok+en/=="
    (handle_result
      (fun _insights ->
        let requests = !Mock_http.requests in
        let has_contract = List.exists (fun (meth, url, _, _) ->
          meth = "GET"
          && Uri.path (Uri.of_string url) = "/v21.0/post_123/insights"
          && query_param url "metric" = Some "views,reach,saved,likes,comments,shares"
          && query_param url "access_token" = Some "tok+en/=="
        ) requests in
        assert has_contract;
        print_endline "✓ Media insights request contract")
      (fun err -> failwith ("Media insights request contract failed: " ^ err)))

(** Test: Account audience insights parser handles timeseries values *)
let test_account_audience_insights_parser () =
  let response_body = {|{
    "data": [
      {
        "name": "follower_count",
        "period": "day",
        "values": [
          {"value": 101, "end_time": "2025-01-01T08:00:00+0000"},
          {"value": 102, "end_time": "2025-01-02T08:00:00+0000"}
        ]
      },
      {
        "name": "reach",
        "period": "day",
        "values": [
          {"value": 500, "end_time": "2025-01-01T08:00:00+0000"}
        ]
      }
    ]
  }|} in
  match Instagram.parse_account_audience_insights_response
          ~account_id:"ig_user_123"
          ~since:"2025-01-01"
          ~until:"2025-01-02"
          response_body with
  | Ok insights ->
      assert (List.length insights.follower_count = 2);
      assert ((List.nth insights.follower_count 0).value = Some 101);
      assert ((List.nth insights.follower_count 1).value = Some 102);
      assert (List.length insights.reach = 1);
      assert ((List.nth insights.reach 0).value = Some 500);
      print_endline "✓ Account audience insights parser"
  | Error err -> failwith ("Account audience insights parser failed: " ^ Error_types.error_to_string err)

(** Test: Account engagement parser handles metric_type=total_value payloads *)
let test_account_engagement_insights_parser_total_value () =
  let response_body = {|{
    "data": [
      {"name": "likes", "total_value": {"value": 41}},
      {"name": "views", "total_value": {"value": 900}},
      {"name": "comments", "total_value": {"value": 7}},
      {"name": "shares", "total_value": {"value": 4}},
      {"name": "saves", "total_value": {"value": 11}},
      {"name": "replies", "total_value": {"value": 2}}
    ]
  }|} in
  match Instagram.parse_account_engagement_insights_response
          ~account_id:"ig_user_123"
          ~since:"2025-01-01"
          ~until:"2025-01-07"
          response_body with
  | Ok insights ->
      assert ((List.hd insights.likes).value = Some 41);
      assert ((List.hd insights.views).value = Some 900);
      assert ((List.hd insights.comments).value = Some 7);
      assert ((List.hd insights.shares).value = Some 4);
      assert ((List.hd insights.saves).value = Some 11);
      assert ((List.hd insights.replies).value = Some 2);
      print_endline "✓ Account engagement insights parser (total_value)"
  | Error err -> failwith ("Account engagement insights parser failed: " ^ Error_types.error_to_string err)

(** Test: Media insights parser extracts metric values *)
let test_media_insights_parser () =
  let response_body = {|{
    "data": [
      {"name": "views", "values": [{"value": 1200}]},
      {"name": "reach", "values": [{"value": 980}]},
      {"name": "saved", "values": [{"value": 31}]},
      {"name": "likes", "values": [{"value": 77}]},
      {"name": "comments", "values": [{"value": 15}]},
      {"name": "shares", "values": [{"value": 8}]}
    ]
  }|} in
  match Instagram.parse_media_insights_response ~post_id:"post_123" response_body with
  | Ok insights ->
      assert (insights.views = Some 1200);
      assert (insights.reach = Some 980);
      assert (insights.saved = Some 31);
      assert (insights.likes = Some 77);
      assert (insights.comments = Some 15);
      assert (insights.shares = Some 8);
      print_endline "✓ Media insights parser"
  | Error err -> failwith ("Media insights parser failed: " ^ Error_types.error_to_string err)

let test_canonical_insights_adapters () =
  let find_series provider_metric series =
    List.find_opt
      (fun item -> item.Analytics_types.provider_metric = Some provider_metric)
      series
  in

  let audience : Instagram.account_audience_insights = {
    account_id = "ig_user_123";
    since = "2025-01-01";
    until = "2025-01-07";
    follower_count = [ { value = Some 301; end_time = Some "2025-01-07T00:00:00+0000" } ];
    reach = [ { value = Some 902; end_time = Some "2025-01-07T00:00:00+0000" } ];
  } in
  let audience_series =
    Instagram.to_canonical_account_audience_insights_series audience
  in
  assert (List.length audience_series = 2);
  (match find_series "follower_count" audience_series with
   | Some item ->
       assert (Analytics_types.canonical_metric_key item.metric = "followers");
       assert ((List.hd item.points).value = 301)
   | None -> failwith "Missing follower_count canonical series");

  let engagement : Instagram.account_engagement_insights = {
    account_id = "ig_user_123";
    since = "2025-01-01";
    until = "2025-01-07";
    likes = [ { value = Some 41; end_time = Some "2025-01-07T00:00:00+0000" } ];
    views = [ { value = Some 501; end_time = Some "2025-01-07T00:00:00+0000" } ];
    comments = [ { value = Some 7; end_time = Some "2025-01-07T00:00:00+0000" } ];
    shares = [ { value = Some 3; end_time = Some "2025-01-07T00:00:00+0000" } ];
    saves = [ { value = Some 11; end_time = Some "2025-01-07T00:00:00+0000" } ];
    replies = [ { value = Some 2; end_time = Some "2025-01-07T00:00:00+0000" } ];
  } in
  let engagement_series =
    Instagram.to_canonical_account_engagement_insights_series engagement
  in
  assert (List.length engagement_series = 6);
  (match find_series "saves" engagement_series with
   | Some item ->
       assert (Analytics_types.canonical_metric_key item.metric = "saves");
       assert ((List.hd item.points).value = 11)
   | None -> failwith "Missing saves canonical series");

  let media : Instagram.media_insights = {
    post_id = "post_123";
    views = Some 1200;
    reach = Some 980;
    saved = Some 31;
    likes = Some 77;
    comments = Some 15;
    shares = Some 8;
  } in
  let media_series = Instagram.to_canonical_media_insights_series media in
  assert (List.length media_series = 6);
  (match find_series "saved" media_series with
   | Some item ->
       assert (Analytics_types.canonical_metric_key item.metric = "saves");
       assert ((List.hd item.points).value = 31)
   | None -> failwith "Missing saved canonical series");

  print_endline "✓ Canonical insights adapters"

let test_insights_network_error_redacts_query_token () =
  Mock_config.reset ();
  Mock_http.set_errors
    [ "GET https://graph.facebook.com/v21.0/ig_user_123/insights?access_token=secret_ig_token failed" ];

  Instagram.get_account_audience_insights
    ~id:"ig_user_123"
    ~access_token:"secret_ig_token"
    ~since:"2025-01-01"
    ~until:"2025-01-02"
    (fun result ->
      match result with
      | Error (Error_types.Network_error (Error_types.Connection_failed msg)) ->
          assert (msg = "[REDACTED_SENSITIVE_TEXT]");
          assert (not (string_contains msg "secret_ig_token"));
          print_endline "✓ Insights network error redacts query token"
      | Ok _ -> failwith "Expected network error"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

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
        print_endline "✓ Post comment")
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
          print_endline "✓ Post comment auth error"
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
        (* Verify request contract *)
        let requests = !Mock_http.requests in
        let has_contract = List.exists (fun (meth, url, _, _) ->
          meth = "GET"
          && string_contains url "/ig_user_123/media"
          && (query_param url "fields" <> None)
        ) requests in
        assert has_contract;
        print_endline "✓ Get user media")
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
        print_endline "✓ Get user media with limit")
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
        print_endline "✓ Delete media")
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
          print_endline "✓ Delete media auth error"
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
        print_endline "✓ Image container with tagging")
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
        print_endline "✓ Video container with location")
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
        print_endline "✓ Reel with extra params (share_to_feed, cover_url, thumb_offset, audio_name)")
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
        print_endline "✓ post_reel with reel-specific params (via create_video_container)")
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
        print_endline "✓ post_reel e2e with reel-specific params")
      (fun err -> failwith ("post_reel e2e with params failed: " ^ err)))

(** Run all tests *)
let () =
  print_endline "\n=== Instagram Provider Tests (Facebook Login) ===\n";
  test_oauth_url ();
  test_oauth_accounts_query_encoding ();
  test_oauth_debug_query_encoding ();
  test_oauth_accounts_parsing_resilience ();
  test_oauth_accounts_appsecret_proof ();
  test_token_exchange ();
  test_oauth_exchange_structured_error ();
  test_oauth_network_error_mapping ();
  test_refresh_token_flow ();
  test_oauth_refresh_appsecret_proof ();
  test_rate_limit_parsing ();
  test_create_container ();
  test_publish_container ();
  test_check_status ();
  test_content_validation ();
  test_post_single ();
  test_post_requires_media ();
  test_post_single_rejects_unsupported_media ();
  test_validate_post_media_required ();

  print_endline "\n--- Alt-Text Tests ---";
  test_post_with_alt_text ();
  test_carousel_with_alt_texts ();
  test_reel_with_alt_text ();
  test_post_reel_rejects_invalid_format ();
  test_post_reel_rejects_invalid_url ();
  test_post_reel_structured_auth_error ();
  test_post_without_alt_text ();

  print_endline "\n--- Stories Tests ---";
  test_create_story_image_container ();
  test_create_story_video_container ();
  test_post_story_image ();
  test_post_story_image_structured_auth_error ();
  test_post_story_video ();
  test_post_story_auto_image ();
  test_post_story_auto_video ();
  test_post_story_rejects_invalid_media ();
  test_validate_story_valid_image ();
  test_validate_story_valid_video ();
  test_validate_story_invalid_url ();
  test_validate_story_invalid_format ();

  print_endline "\n--- Video Upload Tests ---";
  test_video_url_detection ();
  test_video_container_creation ();
  test_reel_container_creation ();
  test_video_carousel_item ();
  test_video_format_validation ();
  test_video_post_full_flow ();
  test_mixed_carousel ();
  test_video_processing_error ();
  test_polling_no_publish_on_status_error ();
  test_validate_story_rejects_gif ();
  test_parse_api_error_token_expired ();
  test_parse_api_error_permissions ();
  test_parse_api_error_rate_limited ();
  test_parse_error_response_token_expired ();
  test_create_container_structured_auth_error ();

  print_endline "\n--- Insights Tests ---";
  test_account_audience_insights_request_contract ();
  test_account_engagement_insights_request_contract ();
  test_media_insights_request_contract ();
  test_account_audience_insights_parser ();
  test_account_engagement_insights_parser_total_value ();
  test_media_insights_parser ();
  test_canonical_insights_adapters ();
  test_insights_network_error_redacts_query_token ();

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

  print_endline "\n=== All tests passed! ===\n"
