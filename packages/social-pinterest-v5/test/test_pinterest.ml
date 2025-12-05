(** Tests for Pinterest API v5 Provider *)

open Social_provider_core
open Social_pinterest_v5

(** Helper to check if string contains substring *)
let string_contains s substr =
  try
    ignore (Str.search_forward (Str.regexp_string substr) s 0);
    true
  with Not_found -> false

(** Mock HTTP client *)
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
    let _body_str = Printf.sprintf "multipart with %d parts" (List.length parts) in
    requests := ("POST_MULTIPART", url, headers, "") :: !requests;
    match get_next_response () with
    | Some response -> on_success response
    | None -> on_error "No mock response set"
  end : HTTP_CLIENT)
end

(** Mock config *)
module Mock_config = struct
  module Http = Mock_http
  
  let env_vars = ref []
  let credentials_store = ref []
  let health_statuses = ref []
  let logs = ref []
  let cache = ref []
  
  let reset () =
    env_vars := [];
    credentials_store := [];
    health_statuses := [];
    logs := [];
    cache := [];
    Mock_http.reset ()
  
  let set_env key value =
    env_vars := (key, value) :: !env_vars
  
  let get_env key =
    List.assoc_opt key !env_vars
  
  let _set_credentials ~account_id ~credentials =
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
  
  (* New enhanced config functions *)
  let _log level message =
    logs := (level, message) :: !logs
  
  let _get_cache key =
    List.assoc_opt key !cache
  
  let _set_cache key value _ttl =
    cache := (key, value) :: List.remove_assoc key !cache
  
  let _current_time () =
    Unix.time ()
end

module Pinterest = Make(Mock_config)

(** Test: OAuth URL generation *)
let test_oauth_url () =
  Mock_config.reset ();
  Mock_config.set_env "PINTEREST_CLIENT_ID" "test_client_id";
  
  let state = "test_state_123" in
  let redirect_uri = "https://example.com/callback" in
  
  Pinterest.get_oauth_url ~redirect_uri ~state
    (fun url ->
      assert (string_contains url "client_id=test_client_id");
      assert (string_contains url "state=test_state_123");
      assert (string_contains url "pins:write");
      print_endline "✓ OAuth URL generation")
    (fun err -> failwith ("OAuth URL failed: " ^ err))

(** Test: Token exchange *)
let test_token_exchange () =
  Mock_config.reset ();
  Mock_config.set_env "PINTEREST_CLIENT_ID" "test_client";
  Mock_config.set_env "PINTEREST_CLIENT_SECRET" "test_secret";
  
  let response_body = {|{
    "access_token": "new_access_token_123",
    "refresh_token": "refresh_token_456",
    "token_type": "bearer",
    "expires_in": 2592000
  }|} in
  
  Mock_http.set_responses [{ status = 200; body = response_body; headers = [] }];
  
  Pinterest.exchange_code 
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    (fun creds ->
      assert (creds.access_token = "new_access_token_123");
      assert (creds.refresh_token = Some "refresh_token_456");
      assert (creds.token_type = "Bearer");
      (* Enhanced version calculates expiry time *)
      assert (creds.expires_at <> None);
      print_endline "✓ Token exchange")
    (fun err -> failwith ("Token exchange failed: " ^ err))

(** Test: Get all boards *)
(* TODO: Function get_all_boards not implemented yet
let test_get_all_boards () =
  Mock_config.reset ();
  
  let response_body = {|{
    "items": [
      {"id": "board_123", "name": "My Board", "privacy": "PUBLIC"},
      {"id": "board_456", "name": "Another Board", "privacy": "PRIVATE"}
    ]
  }|} in
  
  Mock_http.set_responses [{ status = 200; body = response_body; headers = [] }];
  
  Pinterest.get_all_boards ~access_token:"test_token"
    (fun boards ->
      assert (List.length boards = 2);
      assert ((List.nth boards 0).id = "board_123");
      assert ((List.nth boards 0).name = "My Board");
      print_endline "✓ Get all boards")
    (fun err -> failwith ("Get boards failed: " ^ err))
*)

(** Test: Content validation *)
let test_content_validation () =
  (* Valid content *)
  (match Pinterest.validate_content ~text:"Check out this pin!" with
   | Ok () -> print_endline "✓ Valid content passes"
   | Error e -> failwith ("Valid content failed: " ^ e));
  
  (* Empty content *)
  (match Pinterest.validate_content ~text:"" with
   | Error _ -> print_endline "✓ Empty content rejected"
   | Ok () -> failwith "Empty content should fail");
  
  (* Too long *)
  let long_text = String.make 501 'x' in
  (match Pinterest.validate_content ~text:long_text with
   | Error msg when string_contains msg "500" -> 
       print_endline "✓ Long content rejected"
   | _ -> failwith "Long content should fail")

(** Test: Post requires image *)
let test_post_requires_image () =
  Mock_config.reset ();
  
  Pinterest.post_single
    ~account_id:"test_account"
    ~text:"Test"
    ~media_urls:[]
    (fun _ -> failwith "Should fail without image")
    (fun err ->
      assert (string_contains err "image");
      print_endline "✓ Post requires image")

(** Run all tests *)
let () =
  print_endline "\n=== Pinterest Provider Tests ===\n";
  test_oauth_url ();
  test_token_exchange ();
  (* test_get_all_boards (); *) (* TODO: Function not implemented *)
  test_content_validation ();
  test_post_requires_image ();
  print_endline "\n=== All tests passed! ===\n"
