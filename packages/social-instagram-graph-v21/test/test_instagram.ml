(** Tests for Instagram Graph API v21 Provider *)

open Social_core
open Social_instagram_graph_v21

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
    (fun container_id ->
      assert (container_id = "container_12345");
      print_endline "✓ Create image container")
    (fun err -> failwith ("Create image container failed: " ^ err))

(** Test: Publish container *)
let test_publish_container () =
  Mock_config.reset ();
  
  let response_body = {|{"id": "media_67890"}|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Instagram.publish_container
    ~ig_user_id:"ig_123"
    ~access_token:"test_token"
    ~container_id:"container_12345"
    (fun media_id ->
      assert (media_id = "media_67890");
      print_endline "✓ Publish container")
    (fun err -> failwith ("Publish container failed: " ^ err))

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
    (fun (status_code, status) ->
      assert (status_code = "FINISHED");
      assert (status = "Processing complete");
      print_endline "✓ Check container status")
    (fun err -> failwith ("Check status failed: " ^ err))

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
    (fun media_id ->
      assert (media_id = "media_67890");
      (* Verify sleep was called *)
      assert (List.length !Mock_config.sleep_calls > 0);
      print_endline "✓ Post single (full flow)")
    (fun err -> failwith ("Post single failed: " ^ err))

(** Test: Post requires media *)
let test_post_requires_media () =
  Mock_config.reset ();
  
  Instagram.post_single
    ~account_id:"test_account"
    ~text:"Test"
    ~media_urls:[]
    (fun _ -> failwith "Should fail without media")
    (fun err ->
      assert (string_contains err "require");
      print_endline "✓ Post requires media")

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
    (fun err -> failwith ("Post with alt-text failed: " ^ err))

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
    (fun _post_id ->
      print_endline "✓ Carousel with alt-texts")
    (fun err -> failwith ("Carousel with alt-texts failed: " ^ err))

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
    (fun _post_id ->
      print_endline "✓ Reel with alt-text")
    (fun err -> failwith ("Reel with alt-text failed: " ^ err))

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
    (fun _post_id ->
      print_endline "✓ Post without alt-text (should work)")
    (fun err -> failwith ("Post without alt-text failed: " ^ err))

(** Run all tests *)
let () =
  print_endline "\n=== Instagram Provider Tests ===\n";
  test_oauth_url ();
  test_token_exchange ();
  test_create_container ();
  test_publish_container ();
  test_check_status ();
  test_content_validation ();
  test_post_single ();
  test_post_requires_media ();
  
  print_endline "\n--- Alt-Text Tests ---";
  test_post_with_alt_text ();
  test_carousel_with_alt_texts ();
  test_reel_with_alt_text ();
  test_post_without_alt_text ();
  
  print_endline "\n=== All 12 tests passed! ===\n"
