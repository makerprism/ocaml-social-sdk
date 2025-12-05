(** Tests for YouTube Data API v3 Provider *)

open Social_provider_core
open Social_youtube_data_v3

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
  
  let _get_health_status account_id =
    List.find_opt (fun (id, _, _) -> id = account_id) !health_statuses
end

module YouTube = Make(Mock_config)

(** Test: OAuth URL generation with PKCE *)
let test_oauth_url () =
  Mock_config.reset ();
  Mock_config.set_env "YOUTUBE_CLIENT_ID" "test_client_id";
  
  let state = "test_state_123" in
  let redirect_uri = "https://example.com/callback" in
  let code_verifier = "test_verifier_1234567890" in
  
  YouTube.get_oauth_url ~redirect_uri ~state ~code_verifier
    (fun url ->
      assert (string_contains url "client_id=test_client_id");
      assert (string_contains url "state=test_state_123");
      assert (string_contains url "code_challenge");
      assert (string_contains url "code_challenge_method=S256");
      assert (string_contains url "access_type=offline");
      print_endline "✓ OAuth URL generation with PKCE")
    (fun err -> failwith ("OAuth URL failed: " ^ err))

(** Test: Token exchange *)
let test_token_exchange () =
  Mock_config.reset ();
  Mock_config.set_env "YOUTUBE_CLIENT_ID" "test_client";
  Mock_config.set_env "YOUTUBE_CLIENT_SECRET" "test_secret";
  
  let response_body = {|{
    "access_token": "new_access_token_123",
    "refresh_token": "refresh_token_456",
    "expires_in": 3600,
    "token_type": "Bearer"
  }|} in
  
  Mock_http.set_responses [{ status = 200; body = response_body; headers = [] }];
  
  YouTube.exchange_code 
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    ~code_verifier:"test_verifier"
    (fun creds ->
      assert (creds.access_token = "new_access_token_123");
      assert (creds.refresh_token = Some "refresh_token_456");
      assert (creds.token_type = "Bearer");
      assert (creds.expires_at <> None);
      print_endline "✓ Token exchange")
    (fun err -> failwith ("Token exchange failed: " ^ err))

(** Test: Token refresh *)
let test_token_refresh () =
  Mock_config.reset ();
  Mock_config.set_env "YOUTUBE_CLIENT_ID" "test_client";
  Mock_config.set_env "YOUTUBE_CLIENT_SECRET" "test_secret";
  
  let response_body = {|{
    "access_token": "refreshed_token",
    "expires_in": 3600
  }|} in
  
  Mock_http.set_responses [{ status = 200; body = response_body; headers = [] }];
  
  YouTube.refresh_access_token
    ~client_id:"test_client"
    ~client_secret:"test_secret"
    ~refresh_token:"old_refresh"
    (fun (access, refresh, _expires) ->
      assert (access = "refreshed_token");
      assert (refresh = "old_refresh");  (* Google doesn't return new refresh *)
      print_endline "✓ Token refresh")
    (fun err -> failwith ("Token refresh failed: " ^ err))

(** Test: Content validation *)
let test_content_validation () =
  (* Valid content *)
  (match YouTube.validate_content ~text:"Check out my YouTube Short!" with
   | Ok () -> print_endline "✓ Valid content passes"
   | Error e -> failwith ("Valid content failed: " ^ e));
  
  (* Empty content *)
  (match YouTube.validate_content ~text:"" with
   | Error _ -> print_endline "✓ Empty content rejected"
   | Ok () -> failwith "Empty content should fail");
  
  (* Too long *)
  let long_text = String.make 5001 'x' in
  (match YouTube.validate_content ~text:long_text with
   | Error msg when string_contains msg "5000" -> 
       print_endline "✓ Long content rejected"
   | _ -> failwith "Long content should fail")

(** Test: Ensure valid token (fresh token) *)
let test_ensure_valid_token_fresh () =
  Mock_config.reset ();
  
  (* Set credentials with far-future expiry *)
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s 3600) with  (* 1 hour *)
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
  
  YouTube.ensure_valid_token ~account_id:"test_account"
    (fun token ->
      assert (token = "valid_token");
      print_endline "✓ Ensure valid token (fresh)")
    (fun err -> failwith ("Ensure valid token failed: " ^ err))

(** Test: Ensure valid token (expired, needs refresh) *)
let test_ensure_valid_token_expired () =
  Mock_config.reset ();
  Mock_config.set_env "YOUTUBE_CLIENT_ID" "test_client";
  Mock_config.set_env "YOUTUBE_CLIENT_SECRET" "test_secret";
  
  (* Set credentials with past expiry *)
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
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  (* Mock refresh response *)
  let response_body = {|{
    "access_token": "refreshed_token",
    "expires_in": 3600
  }|} in
  
  Mock_http.set_responses [{ status = 200; body = response_body; headers = [] }];
  
  YouTube.ensure_valid_token ~account_id:"test_account"
    (fun token ->
      assert (token = "refreshed_token");
      print_endline "✓ Ensure valid token (auto-refresh)")
    (fun err -> failwith ("Ensure valid token failed: " ^ err))

(** Test: Post requires video *)
let test_post_requires_video () =
  Mock_config.reset ();
  
  YouTube.post_single
    ~account_id:"test_account"
    ~text:"Test"
    ~media_urls:[]
    (fun _ -> failwith "Should fail without video")
    (fun err ->
      assert (string_contains err "video");
      print_endline "✓ Post requires video")

(** Run all tests *)
let () =
  print_endline "\n=== YouTube Provider Tests ===\n";
  test_oauth_url ();
  test_token_exchange ();
  test_token_refresh ();
  test_content_validation ();
  test_ensure_valid_token_fresh ();
  test_ensure_valid_token_expired ();
  test_post_requires_video ();
  print_endline "\n=== All tests passed! ===\n"
