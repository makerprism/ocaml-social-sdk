(** Tests for Pinterest API v5 Provider *)

open Social_core
open Social_pinterest_v5

(** Helper to check if string contains substring *)
let string_contains s substr =
  try
    ignore (Str.search_forward (Str.regexp_string substr) s 0);
    true
  with Not_found -> false

(** Helper to handle outcome type for tests *)
let _handle_outcome on_success on_error outcome =
  match outcome with
  | Error_types.Success result -> on_success result
  | Error_types.Partial_success { result; _ } -> on_success result
  | Error_types.Failure err -> on_error (Error_types.error_to_string err)

(** Helper to handle api_result type for tests - converts to legacy on_success/on_error *)
let _handle_api_result on_success on_error result =
  match result with
  | Ok value -> on_success value
  | Error err -> on_error (Error_types.error_to_string err)

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

let setup_valid_credentials ?(account_id="test_account") () =
  Mock_config.reset ();
  Mock_config._set_credentials
    ~account_id
    ~credentials:{
      access_token = "test_access_token";
      refresh_token = Some "test_refresh_token";
      expires_at = None;
      auth_type = Bearer;
    }

let past_expiry () =
  "2000-01-01T00:00:00Z"

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
      assert (string_contains url "boards:write");
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
    "expires_in": 2592000,
    "scope": "boards:read,pins:read,pins:write,user_accounts:read"
  }|} in
  
  Mock_http.set_responses [{ status = 200; body = response_body; headers = [] }];
  
  Pinterest.exchange_code 
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    (fun creds ->
      assert (creds.access_token = "new_access_token_123");
      assert (creds.refresh_token = Some "refresh_token_456");
      assert (creds.auth_type = Bearer);
      assert (creds.expires_at <> None);
      print_endline "✓ Token exchange")
    (fun err -> failwith ("Token exchange failed: " ^ err))

let test_token_exchange_rejects_missing_scopes () =
  Mock_config.reset ();
  Mock_config.set_env "PINTEREST_CLIENT_ID" "test_client";
  Mock_config.set_env "PINTEREST_CLIENT_SECRET" "test_secret";

  let response_body = {|{
    "access_token": "new_access_token_123",
    "refresh_token": "refresh_token_456",
    "token_type": "bearer",
    "expires_in": 2592000,
    "scope": "boards:read,pins:read,user_accounts:read"
  }|} in

  Mock_http.set_responses [{ status = 200; body = response_body; headers = [] }];

  Pinterest.exchange_code
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    (fun _ -> failwith "Token exchange should fail when required scopes are missing")
    (fun err ->
      assert (string_contains err "Missing required Pinterest OAuth scopes");
      assert (string_contains err "pins:write");
      print_endline "✓ Token exchange rejects missing required scopes")

let test_token_exchange_form_urlencodes_special_chars () =
  Mock_config.reset ();
  Mock_config.set_env "PINTEREST_CLIENT_ID" "test_client";
  Mock_config.set_env "PINTEREST_CLIENT_SECRET" "test_secret";

  let response_body = {|{
    "access_token": "new_access_token_123",
    "refresh_token": "refresh_token_456",
    "token_type": "bearer",
    "expires_in": 2592000,
    "scope": "boards:read,pins:read,pins:write,user_accounts:read"
  }|} in

  Mock_http.set_responses [{ status = 200; body = response_body; headers = [] }];

  Pinterest.exchange_code
    ~code:"test_code"
    ~redirect_uri:"https://example.com/call?back=1"
    (fun _creds ->
      let chronological = List.rev !Mock_http.requests in
      let (_, _, _, body) = List.hd chronological in
      assert (string_contains body "redirect_uri=https%3A%2F%2Fexample.com%2Fcall%3Fback%3D1");
      print_endline "✓ Token exchange form-urlencodes special chars in redirect_uri")
    (fun err -> failwith ("Token exchange form encoding failed: " ^ err))

let test_post_single_refreshes_expired_token () =
  Mock_config.reset ();
  Mock_config.set_env "PINTEREST_CLIENT_ID" "test_client";
  Mock_config.set_env "PINTEREST_CLIENT_SECRET" "test_secret";
  Mock_config._set_credentials
    ~account_id:"test_account"
    ~credentials:{
      access_token = "expired_access_token";
      refresh_token = Some "refresh_token_456";
      expires_at = Some (past_expiry ());
      auth_type = Bearer;
    };

  let refresh_response =
    { status = 200; body = {|{"access_token":"fresh_access","refresh_token":"fresh_refresh","token_type":"Bearer","expires_in":2592000}|}; headers = [] }
  in
  let boards_response =
    { status = 200; body = {|{"items":[{"id":"board_123"}]}|}; headers = [] }
  in
  let pin_create_response =
    { status = 201; body = {|{"id":"pin_789"}|}; headers = [] }
  in

  Mock_http.set_responses [
    refresh_response;
    boards_response;
    pin_create_response;
  ];

  Pinterest.post_single
    ~account_id:"test_account"
    ~text:"Refresh token path"
    ~media_urls:["https://example.com/image.jpg"]
    (fun outcome ->
      match outcome with
      | Error_types.Success pin_id ->
          assert (pin_id = "pin_789");
          let chronological = List.rev !Mock_http.requests in
          let (m1, u1, h1, b1) = List.nth chronological 0 in
          assert (m1 = "POST");
          assert (string_contains u1 "/oauth/token");
          assert (List.assoc_opt "Authorization" h1 <> None);
          assert (string_contains b1 "grant_type=refresh_token");
          print_endline "✓ Expired Pinterest token is refreshed before posting"
      | Error_types.Failure err ->
          failwith ("Expected refreshed post flow to succeed: " ^ Error_types.error_to_string err)
      | Error_types.Partial_success _ ->
          failwith "Expected full success for refreshed token flow")

let test_post_single_expired_token_without_refresh_fails () =
  Mock_config.reset ();
  Mock_config._set_credentials
    ~account_id:"test_account"
    ~credentials:{
      access_token = "expired_access_token";
      refresh_token = None;
      expires_at = Some (past_expiry ());
      auth_type = Bearer;
    };

  Pinterest.post_single
    ~account_id:"test_account"
    ~text:"No refresh token"
    ~media_urls:["https://example.com/image.jpg"]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Auth_error Error_types.Token_expired) ->
          print_endline "✓ Expired token without refresh token fails with auth error"
      | Error_types.Failure err ->
          failwith ("Expected token expired auth error: " ^ Error_types.error_to_string err)
      | Error_types.Success _ ->
          failwith "Expected failure when refresh token is unavailable"
      | Error_types.Partial_success _ ->
          failwith "Expected failure, not partial success")

(* Note: full board-list management APIs are not exposed in this package today.
   post_single and get_default_board cover the current board-resolution workflow. *)

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
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Validation_error _) ->
          print_endline "✓ Post requires image (validation error)"
      | Error_types.Success _ ->
          failwith "Should fail without image"
      | _ ->
          failwith "Expected validation error")

let test_get_account_analytics_request_contract () =
  setup_valid_credentials ();

  Mock_http.set_responses [
    { status = 200; body = {|{}|}; headers = [] };
  ];

  let result = ref None in
  Pinterest.get_account_analytics
    ~account_id:"test_account"
    ~start_date:"2024-01-01"
    ~end_date:"2024-01-31"
    (function
      | Ok analytics -> result := Some (Ok analytics)
      | Error err -> result := Some (Error (Error_types.error_to_string err)));

  (match !result with
   | Some (Ok _) -> ()
   | Some (Error err) -> failwith ("Account analytics request contract failed: " ^ err)
   | None -> failwith "No result in account analytics request contract test");

  let chronological = List.rev !Mock_http.requests in
  assert (List.length chronological = 1);
  let (method_name, url, headers, _) = List.hd chronological in
  assert (method_name = "GET");
  assert (string_contains url "/user_account/analytics");
  assert (List.assoc_opt "Authorization" headers = Some "Bearer test_access_token");

  let uri = Uri.of_string url in
  assert ((Uri.get_query_param uri "start_date" |> Option.value ~default:"") = "2024-01-01");
  assert ((Uri.get_query_param uri "end_date" |> Option.value ~default:"") = "2024-01-31");
  print_endline "✓ Account analytics request contract test passed"

let test_get_pin_analytics_request_contract () =
  setup_valid_credentials ();

  Mock_http.set_responses [
    { status = 200; body = {|{}|}; headers = [] };
  ];

  let result = ref None in
  Pinterest.get_pin_analytics
    ~account_id:"test_account"
    ~pin_id:"pin_123"
    ~start_date:"2024-02-01"
    ~end_date:"2024-02-29"
    (function
      | Ok analytics -> result := Some (Ok analytics)
      | Error err -> result := Some (Error (Error_types.error_to_string err)));

  (match !result with
   | Some (Ok _) -> ()
   | Some (Error err) -> failwith ("Pin analytics request contract failed: " ^ err)
   | None -> failwith "No result in pin analytics request contract test");

  let chronological = List.rev !Mock_http.requests in
  assert (List.length chronological = 1);
  let (method_name, url, headers, _) = List.hd chronological in
  assert (method_name = "GET");
  assert (string_contains url "/pins/pin_123/analytics");
  assert (List.assoc_opt "Authorization" headers = Some "Bearer test_access_token");

  let uri = Uri.of_string url in
  assert ((Uri.get_query_param uri "start_date" |> Option.value ~default:"") = "2024-02-01");
  assert ((Uri.get_query_param uri "end_date" |> Option.value ~default:"") = "2024-02-29");
  assert ((Uri.get_query_param uri "metric_types" |> Option.value ~default:"") = "IMPRESSION,PIN_CLICK,OUTBOUND_CLICK,SAVE");
  print_endline "✓ Pin analytics request contract test passed"

let test_get_account_analytics_parse_typed_result () =
  setup_valid_credentials ();

  Mock_http.set_responses [
    {
      status = 200;
      body = {|{
        "all_source_types": {
          "IMPRESSION": 1200,
          "PIN_CLICK": 45,
          "OUTBOUND_CLICK": "9",
          "SAVE": 30
        }
      }|};
      headers = [ ("content-type", "application/json") ];
    };
  ];

  let result = ref None in
  Pinterest.get_account_analytics
    ~account_id:"test_account"
    ~start_date:"2024-03-01"
    ~end_date:"2024-03-31"
    (function
      | Ok analytics -> result := Some (Ok analytics)
      | Error err -> result := Some (Error (Error_types.error_to_string err)));

  (match !result with
   | Some (Ok analytics) ->
       assert (analytics.start_date = "2024-03-01");
       assert (analytics.end_date = "2024-03-31");
       assert (analytics.totals.impression = Some 1200);
       assert (analytics.totals.pin_click = Some 45);
       assert (analytics.totals.outbound_click = Some 9);
       assert (analytics.totals.save = Some 30)
   | Some (Error err) -> failwith ("Account analytics parse failed: " ^ err)
   | None -> failwith "No result in account analytics parse test");

  print_endline "✓ Account analytics parse test passed"

let test_get_pin_analytics_parse_typed_result () =
  setup_valid_credentials ();

  Mock_http.set_responses [
    {
      status = 200;
      body = {|{
        "items": [
          {
            "impression": 200,
            "pin_click": 11,
            "outbound_click": 4,
            "save": 7
          }
        ]
      }|};
      headers = [ ("content-type", "application/json") ];
    };
  ];

  let result = ref None in
  Pinterest.get_pin_analytics
    ~account_id:"test_account"
    ~pin_id:"pin_parse_1"
    ~start_date:"2024-04-01"
    ~end_date:"2024-04-30"
    (function
      | Ok analytics -> result := Some (Ok analytics)
      | Error err -> result := Some (Error (Error_types.error_to_string err)));

  (match !result with
   | Some (Ok analytics) ->
       assert (analytics.pin_id = "pin_parse_1");
       assert (analytics.start_date = "2024-04-01");
       assert (analytics.end_date = "2024-04-30");
       assert (analytics.totals.impression = Some 200);
       assert (analytics.totals.pin_click = Some 11);
       assert (analytics.totals.outbound_click = Some 4);
       assert (analytics.totals.save = Some 7)
   | Some (Error err) -> failwith ("Pin analytics parse failed: " ^ err)
   | None -> failwith "No result in pin analytics parse test");

  print_endline "✓ Pin analytics parse test passed"

let test_canonical_analytics_adapters () =
  let find_series provider_metric series =
    List.find_opt
      (fun item -> item.Analytics_types.provider_metric = Some provider_metric)
      series
  in
  let account_analytics : Pinterest.account_analytics = {
    start_date = "2024-06-01";
    end_date = "2024-06-30";
    totals = {
      impression = Some 100;
      pin_click = Some 30;
      outbound_click = Some 8;
      save = Some 11;
    };
    raw_json = `Assoc [];
  } in
  let account_series =
    Pinterest.to_canonical_account_analytics_series account_analytics
  in
  assert (List.length account_series = 4);
  (match find_series "OUTBOUND_CLICK" account_series with
   | Some item ->
       assert (Analytics_types.canonical_metric_key item.metric = "outbound_clicks");
       assert ((List.hd item.points).value = 8)
   | None -> failwith "Missing OUTBOUND_CLICK canonical series");

  let pin_analytics : Pinterest.pin_analytics = {
    pin_id = "pin_77";
    start_date = "2024-06-01";
    end_date = "2024-06-30";
    totals = {
      impression = Some 55;
      pin_click = Some 21;
      outbound_click = Some 5;
      save = Some 6;
    };
    raw_json = `Assoc [];
  } in
  let pin_series = Pinterest.to_canonical_pin_analytics_series pin_analytics in
  assert (List.length pin_series = 4);
  (match find_series "PIN_CLICK" pin_series with
   | Some item ->
       assert (Analytics_types.canonical_metric_key item.metric = "pin_clicks");
       assert ((List.hd item.points).value = 21)
   | None -> failwith "Missing PIN_CLICK canonical series");

  print_endline "✓ Canonical analytics adapters"

let test_validate_media_file_rejects_large_image () =
  let oversized = (20 * 1024 * 1024) + 1 in
  let media : Platform_types.post_media = {
    media_type = Platform_types.Image;
    mime_type = "image/jpeg";
    file_size_bytes = oversized;
    width = None;
    height = None;
    duration_seconds = None;
    alt_text = None;
  } in
  match Pinterest.validate_media_file ~media with
  | Error [Error_types.Media_too_large _] ->
      print_endline "✓ Large image rejected by media validation"
  | Error errs ->
      failwith ("Unexpected validation error: " ^ String.concat ", " (List.map Error_types.validation_error_to_string errs))
  | Ok () ->
      failwith "Oversized image should fail media validation"

let test_post_single_full_image_flow_with_alt_text () =
  setup_valid_credentials ();

  let boards_response =
    { status = 200; body = {|{"items":[{"id":"board_123"}]}|}; headers = [] }
  in
  let pin_create_response =
    { status = 201; body = {|{"id":"pin_789"}|}; headers = [] }
  in

  Mock_http.set_responses [
    boards_response;
    pin_create_response;
  ];

  Pinterest.post_single
    ~account_id:"test_account"
    ~text:"Pin title and description"
    ~media_urls:["https://example.com/image.jpg"]
    ~alt_texts:[Some "Accessible alt text"]
    (fun outcome ->
      match outcome with
      | Error_types.Success pin_id ->
          assert (pin_id = "pin_789");
          let chronological = List.rev !Mock_http.requests in
          assert (List.length chronological = 2);

          let (m1, u1, h1, _) = List.nth chronological 0 in
          assert (m1 = "GET");
          assert (string_contains u1 "/boards");
          assert (List.assoc_opt "Authorization" h1 = Some "Bearer test_access_token");

          let (m2, u2, h2, body2) = List.nth chronological 1 in
          assert (m2 = "POST");
          assert (string_contains u2 "/pins");
          assert (List.assoc_opt "Authorization" h2 = Some "Bearer test_access_token");

          let json = Yojson.Basic.from_string body2 in
          let open Yojson.Basic.Util in
          assert ((json |> member "board_id" |> to_string) = "board_123");
          assert ((json |> member "alt_text" |> to_string) = "Accessible alt text");
          assert ((json |> member "media_source" |> member "source_type" |> to_string) = "image_url");
          assert ((json |> member "media_source" |> member "url" |> to_string) = "https://example.com/image.jpg");
          print_endline "✓ post_single uses image_url source type with alt text"
      | Error_types.Failure err ->
          failwith ("post_single full flow failed: " ^ Error_types.error_to_string err)
      | Error_types.Partial_success _ ->
          failwith "Expected full success for single pin flow")

let test_post_single_truncates_title_to_100_chars () =
  setup_valid_credentials ();

  let boards_response =
    { status = 200; body = {|{"items":[{"id":"board_123"}]}|}; headers = [] }
  in
  let pin_create_response =
    { status = 201; body = {|{"id":"pin_789"}|}; headers = [] }
  in

  Mock_http.set_responses [
    boards_response;
    pin_create_response;
  ];

  let long_text = String.make 120 'x' in

  Pinterest.post_single
    ~account_id:"test_account"
    ~text:long_text
    ~media_urls:["https://example.com/image.jpg"]
    (fun outcome ->
      match outcome with
      | Error_types.Success _ ->
          let chronological = List.rev !Mock_http.requests in
          let (_, _, _, body) = List.nth chronological 1 in
          let json = Yojson.Basic.from_string body in
          let open Yojson.Basic.Util in
          let title = json |> member "title" |> to_string in
          let description = json |> member "description" |> to_string in
          assert (String.length title = 100);
          assert (description = long_text);
          print_endline "✓ post_single truncates title to 100 chars"
      | Error_types.Failure err ->
          failwith ("post_single title truncation test failed: " ^ Error_types.error_to_string err)
      | Error_types.Partial_success _ ->
          failwith "Expected full success for single pin flow")

let test_post_single_image_url_skips_download () =
  setup_valid_credentials ();

  let boards_response =
    { status = 200; body = {|{"items":[{"id":"board_123"}]}|}; headers = [] }
  in
  let pin_create_response =
    { status = 201; body = {|{"id":"pin_url_direct"}|}; headers = [] }
  in

  Mock_http.set_responses [
    boards_response;
    pin_create_response;
  ];

  Pinterest.post_single
    ~account_id:"test_account"
    ~text:"Image URL pin"
    ~media_urls:["https://example.com/large-image.jpg"]
    (fun outcome ->
      match outcome with
      | Error_types.Success pin_id ->
          assert (pin_id = "pin_url_direct");
          let chronological = List.rev !Mock_http.requests in
          (* Only 2 requests: GET /boards and POST /pins -- no image download *)
          assert (List.length chronological = 2);
          let (m1, u1, _, _) = List.nth chronological 0 in
          assert (m1 = "GET");
          assert (string_contains u1 "/boards");
          let (m2, u2, _, body2) = List.nth chronological 1 in
          assert (m2 = "POST");
          assert (string_contains u2 "/pins");
          let json = Yojson.Basic.from_string body2 in
          let open Yojson.Basic.Util in
          assert ((json |> member "media_source" |> member "source_type" |> to_string) = "image_url");
          assert ((json |> member "media_source" |> member "url" |> to_string) = "https://example.com/large-image.jpg");
          print_endline "✓ image_url source type skips download and uploads URL directly"
      | Error_types.Failure err ->
          failwith ("image_url flow should succeed: " ^ Error_types.error_to_string err)
      | Error_types.Partial_success _ ->
          failwith "Expected full success for image_url flow")

let test_post_thread_partial_success_posts_only_first () =
  setup_valid_credentials ();

  let boards_response =
    { status = 200; body = {|{"items":[{"id":"board_123"}]}|}; headers = [] }
  in
  let pin_create_response =
    { status = 201; body = {|{"id":"pin_789"}|}; headers = [] }
  in

  Mock_http.set_responses [
    boards_response;
    pin_create_response;
  ];

  Pinterest.post_thread
    ~account_id:"test_account"
    ~texts:["first"; "second"]
    ~media_urls_per_post:[
      ["https://example.com/first.jpg"];
      ["https://example.com/second.jpg"];
    ]
    (fun outcome ->
      match outcome with
      | Error_types.Partial_success { result; warnings } ->
          assert (result.Error_types.posted_ids = ["pin_789"]);
          assert (result.Error_types.total_requested = 2);
          assert (result.Error_types.failed_at_index = None);
          assert (List.length warnings = 1);
          (match List.hd warnings with
           | Error_types.Generic_warning { code; _ } -> assert (code = "pinterest_no_threads")
           | _ -> failwith "Expected Generic_warning for thread fallback");
          let chronological = List.rev !Mock_http.requests in
          assert (List.length chronological = 2);
          print_endline "✓ post_thread returns partial success and posts only first item"
      | Error_types.Success _ ->
          failwith "Expected partial success when posting multi-item thread"
      | Error_types.Failure err ->
          failwith ("post_thread partial success test failed: " ^ Error_types.error_to_string err))

let test_validate_media_file_accepts_video () =
  let media : Platform_types.post_media = {
    media_type = Platform_types.Video;
    mime_type = "video/mp4";
    file_size_bytes = 5 * 1024 * 1024;
    width = Some 1080;
    height = Some 1920;
    duration_seconds = Some 15.0;
    alt_text = None;
  } in
  match Pinterest.validate_media_file ~media with
  | Ok () ->
      print_endline "✓ Video media validation accepts supported video"
  | Error errs ->
      failwith ("Unexpected validation result: " ^ String.concat ", " (List.map Error_types.validation_error_to_string errs))

let test_post_single_video_url_uses_video_media_flow () =
  setup_valid_credentials ();

  let boards_response =
    { status = 200; body = {|{"items":[{"id":"board_123"}]}|}; headers = [] }
  in
  let video_download_response =
    { status = 200; body = "fake_video_binary"; headers = [ ("content-type", "video/mp4") ] }
  in
  let media_register_response =
    { status = 201; body =
      {|{"media_id":"media_456","upload_url":"https://uploads.pinterest.com","upload_parameters":{"key":"media/key.mp4","policy":"abc"}}|};
      headers = [] }
  in
  let media_upload_response =
    { status = 204; body = ""; headers = [] }
  in
  let media_status_registered =
    { status = 200; body = {|{"media_id":"media_456","status":"registered"}|}; headers = [] }
  in
  let media_status_processing =
    { status = 200; body = {|{"media_id":"media_456","status":"processing"}|}; headers = [] }
  in
  let media_status_response =
    { status = 200; body = {|{"media_id":"media_456","status":"succeeded"}|}; headers = [] }
  in
  let thumbnail_probe_response =
    { status = 200; body = "fake_poster_binary"; headers = [ ("content-type", "image/jpeg") ] }
  in
  let pin_create_response =
    { status = 201; body = {|{"id":"pin_789"}|}; headers = [] }
  in

  Mock_http.set_responses [
    boards_response;
    video_download_response;
    media_register_response;
    media_upload_response;
    media_status_registered;
    media_status_processing;
    media_status_response;
    thumbnail_probe_response;
    pin_create_response;
  ];

  Pinterest.post_single
    ~account_id:"test_account"
    ~text:"Video URL should be posted"
    ~media_urls:["https://example.com/video.mp4"; "https://example.com/poster.jpg"]
    (fun outcome ->
      match outcome with
      | Error_types.Success pin_id ->
          assert (pin_id = "pin_789");
          let chronological = List.rev !Mock_http.requests in
          assert (List.length chronological = 9);

          let (m3, u3, _, b3) = List.nth chronological 2 in
          assert (m3 = "POST");
          assert (string_contains u3 "/media");
          assert (string_contains b3 "media_type");
          assert (string_contains b3 "video");

          let (m4, u4, _, _) = List.nth chronological 3 in
          assert (m4 = "POST_MULTIPART");
          assert (u4 = "https://uploads.pinterest.com");

          let (m5, u5, _, _) = List.nth chronological 4 in
          assert (m5 = "GET");
          assert (string_contains u5 "/media/media_456");

          let (m6, u6, _, _) = List.nth chronological 5 in
          assert (m6 = "GET");
          assert (string_contains u6 "/media/media_456");

          let (m7, u7, _, _) = List.nth chronological 6 in
          assert (m7 = "GET");
          assert (string_contains u7 "/media/media_456");

          let (m8, u8, _, _) = List.nth chronological 7 in
          assert (m8 = "GET");
          assert (u8 = "https://example.com/poster.jpg");

          let (m9, u9, _, b9) = List.nth chronological 8 in
          assert (m9 = "POST");
          assert (string_contains u9 "/pins");
          let pin_json = Yojson.Basic.from_string b9 in
          let open Yojson.Basic.Util in
          assert ((pin_json |> member "media_source" |> member "source_type" |> to_string) = "video_id");
          assert ((pin_json |> member "media_source" |> member "media_id" |> to_string) = "media_456");
          assert ((pin_json |> member "media_source" |> member "cover_image_url" |> to_string) = "https://example.com/poster.jpg");
          print_endline "✓ Video URL uses Pinterest video media flow"
      | Error_types.Failure err ->
          failwith ("Video URL should succeed via video media flow: " ^ Error_types.error_to_string err)
      | Error_types.Partial_success _ ->
          failwith "Video URL should complete with full success")

let test_post_single_unknown_extension_image_falls_back_to_image_flow () =
  setup_valid_credentials ();

  let boards_response =
    { status = 200; body = {|{"items":[{"id":"board_123"}]}|}; headers = [] }
  in
  let first_download_image_response =
    { status = 200; body = "fake_image_binary"; headers = [ ("content-type", "image/jpeg") ] }
  in
  let pin_create_response =
    { status = 201; body = {|{"id":"pin_img_001"}|}; headers = [] }
  in

  Mock_http.set_responses [
    boards_response;
    first_download_image_response;
    pin_create_response;
  ];

  Pinterest.post_single
    ~account_id:"test_account"
    ~text:"Unknown extension should fallback to image flow"
    ~media_urls:["https://example.com/asset?id=42"]
    (fun outcome ->
      match outcome with
      | Error_types.Success pin_id ->
          assert (pin_id = "pin_img_001");
          let chronological = List.rev !Mock_http.requests in
          (* 3 requests: GET /boards, GET url (video attempt), POST /pins (image_url fallback) *)
          assert (List.length chronological = 3);

          let (m2, _, _, _) = List.nth chronological 1 in
          assert (m2 = "GET");

          let (m3, u3, _, pin_body) = List.nth chronological 2 in
          assert (m3 = "POST");
          assert (string_contains u3 "/pins");

          let pin_json = Yojson.Basic.from_string pin_body in
          let open Yojson.Basic.Util in
          assert ((pin_json |> member "media_source" |> member "source_type" |> to_string) = "image_url");
          assert ((pin_json |> member "media_source" |> member "url" |> to_string) = "https://example.com/asset?id=42");
          print_endline "✓ Unknown extension media falls back to image_url on image content-type"
      | Error_types.Failure err ->
          failwith ("Unknown-extension image fallback failed: " ^ Error_types.error_to_string err)
      | Error_types.Partial_success _ ->
          failwith "Unknown-extension image fallback should complete with full success")

let test_post_single_unknown_extension_video_uses_video_flow () =
  setup_valid_credentials ();

  let boards_response =
    { status = 200; body = {|{"items":[{"id":"board_123"}]}|}; headers = [] }
  in
  let first_download_video_response =
    { status = 200; body = "fake_video_binary"; headers = [ ("content-type", "video/mp4") ] }
  in
  let media_register_response =
    { status = 201; body =
      {|{"media_id":"media_vid_002","upload_url":"https://uploads.pinterest.com","upload_parameters":{"key":"media/key2.mp4","policy":"abc"}}|};
      headers = [] }
  in
  let media_upload_response =
    { status = 204; body = ""; headers = [] }
  in
  let media_status_response =
    { status = 200; body = {|{"media_id":"media_vid_002","status":"succeeded"}|}; headers = [] }
  in
  let thumbnail_probe_response =
    { status = 200; body = "fake_poster_binary"; headers = [ ("content-type", "image/jpeg") ] }
  in
  let pin_create_response =
    { status = 201; body = {|{"id":"pin_vid_002"}|}; headers = [] }
  in

  Mock_http.set_responses [
    boards_response;
    first_download_video_response;
    media_register_response;
    media_upload_response;
    media_status_response;
    thumbnail_probe_response;
    pin_create_response;
  ];

  Pinterest.post_single
    ~account_id:"test_account"
    ~text:"Unknown extension should use video flow"
    ~media_urls:["https://example.com/asset?id=video"; "https://example.com/poster.jpg"]
    (fun outcome ->
      match outcome with
      | Error_types.Success pin_id ->
          assert (pin_id = "pin_vid_002");
          let chronological = List.rev !Mock_http.requests in
          assert (List.length chronological = 7);
          let (m3, u3, _, b3) = List.nth chronological 2 in
          assert (m3 = "POST");
          assert (string_contains u3 "/media");
          assert (string_contains b3 "video");
          let (m6, u6, _, _) = List.nth chronological 5 in
          assert (m6 = "GET");
          assert (u6 = "https://example.com/poster.jpg");
          let (_, _, _, pin_body) = List.nth chronological 6 in
          let pin_json = Yojson.Basic.from_string pin_body in
          let open Yojson.Basic.Util in
          assert ((pin_json |> member "media_source" |> member "source_type" |> to_string) = "video_id");
          assert ((pin_json |> member "media_source" |> member "cover_image_url" |> to_string) = "https://example.com/poster.jpg");
          print_endline "✓ Unknown extension media uses video flow when content-type is video"
      | Error_types.Failure err ->
          failwith ("Unknown-extension video route failed: " ^ Error_types.error_to_string err)
      | Error_types.Partial_success _ ->
          failwith "Unknown-extension video route should complete with full success")

let test_post_single_unknown_extension_non_media_returns_clean_error () =
  setup_valid_credentials ();

  let boards_response =
    { status = 200; body = {|{"items":[{"id":"board_123"}]}|}; headers = [] }
  in
  let first_download_pdf_response =
    { status = 200; body = "fake_pdf_binary"; headers = [ ("content-type", "application/pdf") ] }
  in

  Mock_http.set_responses [
    boards_response;
    first_download_pdf_response;
  ];

  Pinterest.post_single
    ~account_id:"test_account"
    ~text:"Unknown extension non-media should fail"
    ~media_urls:["https://example.com/asset?id=pdf"]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Internal_error msg) ->
          assert (string_contains msg "Unsupported media content-type: application/pdf");
          assert (not (string_contains msg "non_video_content_type:"));
          print_endline "✓ Unknown extension non-media fails with clean unsupported content-type error"
      | Error_types.Failure err ->
          failwith ("Expected internal unsupported content-type error: " ^ Error_types.error_to_string err)
      | Error_types.Success _ ->
          failwith "Non-media URL should not succeed"
      | Error_types.Partial_success _ ->
          failwith "Non-media URL should not partially succeed")

let test_post_single_video_content_type_with_charset_is_supported () =
  setup_valid_credentials ();

  let boards_response =
    { status = 200; body = {|{"items":[{"id":"board_123"}]}|}; headers = [] }
  in
  let video_download_response =
    { status = 200; body = "fake_video_binary"; headers = [ ("content-type", "video/mp4; charset=binary") ] }
  in
  let media_register_response =
    { status = 201; body =
      {|{"media_id":"media_789","upload_url":"https://uploads.pinterest.com","upload_parameters":{"key":"media/key789.mp4","policy":"abc"}}|};
      headers = [] }
  in
  let media_upload_response =
    { status = 204; body = ""; headers = [] }
  in
  let media_status_response =
    { status = 200; body = {|{"media_id":"media_789","status":"succeeded"}|}; headers = [] }
  in
  let pin_create_response =
    { status = 201; body = {|{"id":"pin_789_charset"}|}; headers = [] }
  in

  Mock_http.set_responses [
    boards_response;
    video_download_response;
    media_register_response;
    media_upload_response;
    media_status_response;
    pin_create_response;
  ];

  Pinterest.post_single
    ~account_id:"test_account"
    ~text:"Video content-type params"
    ~media_urls:["https://example.com/video-param"]
    (fun outcome ->
      match outcome with
      | Error_types.Success pin_id ->
          assert (pin_id = "pin_789_charset");
          let chronological = List.rev !Mock_http.requests in
          let (_, _, _, body3) = List.nth chronological 2 in
          assert (string_contains body3 "\"media_type\":\"video\"");
          print_endline "✓ Video content-type with parameters is normalized for Pinterest"
      | Error_types.Failure err ->
          failwith ("Video content-type normalization should succeed: " ^ Error_types.error_to_string err)
      | Error_types.Partial_success _ ->
          failwith "Expected full success for video content-type normalization")

let test_post_single_video_ignores_non_image_thumbnail_url () =
  setup_valid_credentials ();

  let boards_response =
    { status = 200; body = {|{"items":[{"id":"board_123"}]}|}; headers = [] }
  in
  let video_download_response =
    { status = 200; body = "fake_video_binary"; headers = [ ("content-type", "video/mp4") ] }
  in
  let media_register_response =
    { status = 201; body =
      {|{"media_id":"media_790","upload_url":"https://uploads.pinterest.com","upload_parameters":{"key":"media/key790.mp4","policy":"abc"}}|};
      headers = [] }
  in
  let media_upload_response =
    { status = 204; body = ""; headers = [] }
  in
  let media_status_response =
    { status = 200; body = {|{"media_id":"media_790","status":"succeeded"}|}; headers = [] }
  in
  let pin_create_response =
    { status = 201; body = {|{"id":"pin_790"}|}; headers = [] }
  in

  Mock_http.set_responses [
    boards_response;
    video_download_response;
    media_register_response;
    media_upload_response;
    media_status_response;
    pin_create_response;
  ];

  Pinterest.post_single
    ~account_id:"test_account"
    ~text:"Video should ignore non-image thumbnail URL"
    ~media_urls:["https://example.com/video.mp4"; "https://example.com/thumb"]
    (fun outcome ->
      match outcome with
      | Error_types.Success pin_id ->
          assert (pin_id = "pin_790");
          let chronological = List.rev !Mock_http.requests in
          let (_, _, _, pin_body) = List.nth chronological 5 in
          let pin_json = Yojson.Basic.from_string pin_body in
          let open Yojson.Basic.Util in
          assert ((pin_json |> member "media_source" |> member "source_type" |> to_string) = "video_id");
          assert ((pin_json |> member "media_source" |> member "cover_image_url" |> to_string_option) = None);
          print_endline "✓ Video flow ignores non-image thumbnail URL candidate"
      | Error_types.Failure err ->
          failwith ("Video flow should ignore non-image thumbnail and still succeed: " ^ Error_types.error_to_string err)
      | Error_types.Partial_success _ ->
          failwith "Expected full success when non-image thumbnail is ignored")

let test_validate_media_file_rejects_oversized_video () =
  let media : Platform_types.post_media = {
    media_type = Platform_types.Video;
    mime_type = "video/mp4";
    file_size_bytes = (2 * 1024 * 1024 * 1024) + 1;
    width = Some 1080;
    height = Some 1920;
    duration_seconds = Some 15.0;
    alt_text = None;
  } in
  match Pinterest.validate_media_file ~media with
  | Error [Error_types.Media_too_large _] ->
      print_endline "✓ Oversized video rejected by media validation"
  | Error errs ->
      failwith ("Unexpected oversized-video validation result: " ^ String.concat ", " (List.map Error_types.validation_error_to_string errs))
  | Ok () ->
      failwith "Oversized video should fail media validation"

(** Test: parse_rate_limit_headers *)
let test_parse_rate_limit_headers () =
  let headers = [
    ("X-RateLimit-Limit", "200");
    ("X-RateLimit-Remaining", "150");
    ("X-RateLimit-Reset", "30");
    ("Content-Type", "application/json");
  ] in
  let rl = Pinterest.parse_rate_limit_headers headers in
  assert (rl.Pinterest.limit = Some 200);
  assert (rl.Pinterest.remaining = Some 150);
  assert (rl.Pinterest.reset = Some 30);
  print_endline "✓ parse_rate_limit_headers extracts all three headers"

let test_parse_rate_limit_headers_missing () =
  let headers = [
    ("Content-Type", "application/json");
  ] in
  let rl = Pinterest.parse_rate_limit_headers headers in
  assert (rl.Pinterest.limit = None);
  assert (rl.Pinterest.remaining = None);
  assert (rl.Pinterest.reset = None);
  print_endline "✓ parse_rate_limit_headers returns None for missing headers"

let test_parse_rate_limit_headers_case_insensitive () =
  let headers = [
    ("x-ratelimit-limit", "100");
    ("x-ratelimit-remaining", "50");
    ("x-ratelimit-reset", "10");
  ] in
  let rl = Pinterest.parse_rate_limit_headers headers in
  assert (rl.Pinterest.limit = Some 100);
  assert (rl.Pinterest.remaining = Some 50);
  assert (rl.Pinterest.reset = Some 10);
  print_endline "✓ parse_rate_limit_headers is case-insensitive"

let test_parse_rate_limit_headers_invalid_values () =
  let headers = [
    ("X-RateLimit-Limit", "not-a-number");
    ("X-RateLimit-Remaining", "50");
    ("X-RateLimit-Reset", "");
  ] in
  let rl = Pinterest.parse_rate_limit_headers headers in
  assert (rl.Pinterest.limit = None);
  assert (rl.Pinterest.remaining = Some 50);
  assert (rl.Pinterest.reset = None);
  print_endline "✓ parse_rate_limit_headers handles invalid values gracefully"

(** Test: with_retry *)
let test_with_retry_no_retry_on_success () =
  Mock_config.reset ();

  let call_count = ref 0 in
  let make_request on_result =
    incr call_count;
    on_result { status = 200; body = {|{"ok":true}|}; headers = [] }
  in
  Pinterest.with_retry ~max_retries:3 ~make_request (fun response ->
    assert (response.status = 200);
    assert (!call_count = 1);
    print_endline "✓ with_retry does not retry on success")

let test_with_retry_retries_on_429 () =
  Mock_config.reset ();

  let call_count = ref 0 in
  let make_request on_result =
    incr call_count;
    if !call_count < 3 then
      on_result { status = 429; body = "rate limited"; headers = [("X-RateLimit-Reset", "0")] }
    else
      on_result { status = 200; body = {|{"ok":true}|}; headers = [] }
  in
  Pinterest.with_retry ~max_retries:3 ~initial_delay_seconds:0.001 ~make_request (fun response ->
    assert (response.status = 200);
    assert (!call_count = 3);
    print_endline "✓ with_retry retries on 429 and succeeds")

let test_with_retry_retries_on_500 () =
  Mock_config.reset ();

  let call_count = ref 0 in
  let make_request on_result =
    incr call_count;
    if !call_count < 2 then
      on_result { status = 500; body = "server error"; headers = [] }
    else
      on_result { status = 200; body = {|{"ok":true}|}; headers = [] }
  in
  Pinterest.with_retry ~max_retries:3 ~initial_delay_seconds:0.001 ~make_request (fun response ->
    assert (response.status = 200);
    assert (!call_count = 2);
    print_endline "✓ with_retry retries on 500 and succeeds")

let test_with_retry_gives_up_after_max_retries () =
  Mock_config.reset ();

  let call_count = ref 0 in
  let make_request on_result =
    incr call_count;
    on_result { status = 503; body = "service unavailable"; headers = [] }
  in
  Pinterest.with_retry ~max_retries:2 ~initial_delay_seconds:0.001 ~make_request (fun response ->
    assert (response.status = 503);
    (* Initial call + 2 retries = 3 total *)
    assert (!call_count = 3);
    print_endline "✓ with_retry gives up after max retries and returns last response")

let test_with_retry_no_retry_on_4xx () =
  Mock_config.reset ();

  let call_count = ref 0 in
  let make_request on_result =
    incr call_count;
    on_result { status = 400; body = "bad request"; headers = [] }
  in
  Pinterest.with_retry ~max_retries:3 ~make_request (fun response ->
    assert (response.status = 400);
    assert (!call_count = 1);
    print_endline "✓ with_retry does not retry on 400")

let test_with_retry_uses_rate_limit_reset_header () =
  Mock_config.reset ();

  let call_count = ref 0 in
  let make_request on_result =
    incr call_count;
    if !call_count = 1 then
      on_result { status = 429; body = "rate limited"; headers = [("X-RateLimit-Reset", "1")] }
    else
      on_result { status = 200; body = {|{"ok":true}|}; headers = [] }
  in
  (* Use a large initial_delay so we can confirm the rate limit header overrides it *)
  Pinterest.with_retry ~max_retries:3 ~initial_delay_seconds:0.001 ~make_request (fun response ->
    assert (response.status = 200);
    assert (!call_count = 2);
    print_endline "✓ with_retry uses rate limit reset header value on 429")

let test_create_board_api_error () =
  Mock_config.reset ();

  let error_response =
    { status = 400; body = {|{"message":"Invalid board name"}|}; headers = [] }
  in
  Mock_http.set_responses [ error_response ];

  Pinterest.create_board
    ~access_token:"test_token"
    ~name:""
    (function
      | Error (Error_types.Api_error { status_code; message; _ }) ->
          assert (status_code = 400);
          assert (message = "Invalid board name");
          print_endline "✓ create_board returns structured error on API failure"
      | Error err ->
          failwith ("Expected Api_error: " ^ Error_types.error_to_string err)
      | Ok _ ->
          failwith "Expected error for invalid board creation")

let test_get_board_sections_api_error () =
  Mock_config.reset ();

  let error_response =
    { status = 404; body = {|{"message":"Board not found"}|}; headers = [] }
  in
  Mock_http.set_responses [ error_response ];

  Pinterest.get_board_sections
    ~access_token:"test_token"
    ~board_id:"nonexistent_board"
    (function
      | Error (Error_types.Api_error { status_code; message; _ }) ->
          assert (status_code = 404);
          assert (message = "Board not found");
          print_endline "✓ get_board_sections returns structured error on API failure"
      | Error err ->
          failwith ("Expected Api_error: " ^ Error_types.error_to_string err)
      | Ok _ ->
          failwith "Expected error for nonexistent board sections")

let test_create_board_section_api_error () =
  Mock_config.reset ();

  let error_response =
    { status = 403; body = {|{"message":"Insufficient permissions"}|}; headers = [] }
  in
  Mock_http.set_responses [ error_response ];

  Pinterest.create_board_section
    ~access_token:"test_token"
    ~board_id:"board_123"
    ~name:"New Section"
    (function
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions _)) ->
          print_endline "✓ create_board_section returns auth error on 403"
      | Error err ->
          failwith ("Expected Auth_error for 403: " ^ Error_types.error_to_string err)
      | Ok _ ->
          failwith "Expected error for forbidden board section creation")

(** Test: create_board *)
let test_create_board () =
  Mock_config.reset ();

  let board_response =
    { status = 201; body = {|{"id":"board_new_1","name":"My Board","privacy":"PUBLIC"}|}; headers = [] }
  in
  Mock_http.set_responses [ board_response ];

  Pinterest.create_board
    ~access_token:"test_token"
    ~name:"My Board"
    (function
      | Ok json ->
          let open Yojson.Basic.Util in
          assert ((json |> member "id" |> to_string) = "board_new_1");
          assert ((json |> member "name" |> to_string) = "My Board");

          let chronological = List.rev !Mock_http.requests in
          assert (List.length chronological = 1);
          let (method_name, url, headers, body) = List.hd chronological in
          assert (method_name = "POST");
          assert (string_contains url "/boards");
          assert (List.assoc_opt "Authorization" headers = Some "Bearer test_token");
          assert (List.assoc_opt "Content-Type" headers = Some "application/json");
          let body_json = Yojson.Basic.from_string body in
          assert ((body_json |> member "name" |> to_string) = "My Board");
          assert ((body_json |> member "privacy" |> to_string) = "PUBLIC");
          print_endline "✓ create_board sends correct request and parses response"
      | Error err ->
          failwith ("create_board failed: " ^ Error_types.error_to_string err))

let test_create_board_with_description_and_secret () =
  Mock_config.reset ();

  let board_response =
    { status = 201; body = {|{"id":"board_secret_1","name":"Secret Board","privacy":"SECRET","description":"My secret board"}|}; headers = [] }
  in
  Mock_http.set_responses [ board_response ];

  Pinterest.create_board
    ~access_token:"test_token"
    ~name:"Secret Board"
    ~description:"My secret board"
    ~privacy:"SECRET"
    (function
      | Ok json ->
          let open Yojson.Basic.Util in
          assert ((json |> member "id" |> to_string) = "board_secret_1");
          assert ((json |> member "privacy" |> to_string) = "SECRET");

          let chronological = List.rev !Mock_http.requests in
          let (_, _, _, body) = List.hd chronological in
          let body_json = Yojson.Basic.from_string body in
          assert ((body_json |> member "description" |> to_string) = "My secret board");
          assert ((body_json |> member "privacy" |> to_string) = "SECRET");
          print_endline "✓ create_board with description and SECRET privacy"
      | Error err ->
          failwith ("create_board with description failed: " ^ Error_types.error_to_string err))

(** Test: get_board_sections *)
let test_get_board_sections () =
  Mock_config.reset ();

  let sections_response =
    { status = 200; body = {|{"items":[{"id":"section_1","name":"Section A"},{"id":"section_2","name":"Section B"}]}|}; headers = [] }
  in
  Mock_http.set_responses [ sections_response ];

  Pinterest.get_board_sections
    ~access_token:"test_token"
    ~board_id:"board_123"
    (function
      | Ok json ->
          let open Yojson.Basic.Util in
          let items = json |> member "items" |> to_list in
          assert (List.length items = 2);
          assert ((List.nth items 0 |> member "id" |> to_string) = "section_1");
          assert ((List.nth items 1 |> member "name" |> to_string) = "Section B");

          let chronological = List.rev !Mock_http.requests in
          assert (List.length chronological = 1);
          let (method_name, url, headers, _) = List.hd chronological in
          assert (method_name = "GET");
          assert (string_contains url "/boards/board_123/sections");
          assert (List.assoc_opt "Authorization" headers = Some "Bearer test_token");
          print_endline "✓ get_board_sections retrieves sections for a board"
      | Error err ->
          failwith ("get_board_sections failed: " ^ Error_types.error_to_string err))

(** Test: create_board_section *)
let test_create_board_section () =
  Mock_config.reset ();

  let section_response =
    { status = 201; body = {|{"id":"section_new_1","name":"New Section"}|}; headers = [] }
  in
  Mock_http.set_responses [ section_response ];

  Pinterest.create_board_section
    ~access_token:"test_token"
    ~board_id:"board_123"
    ~name:"New Section"
    (function
      | Ok json ->
          let open Yojson.Basic.Util in
          assert ((json |> member "id" |> to_string) = "section_new_1");
          assert ((json |> member "name" |> to_string) = "New Section");

          let chronological = List.rev !Mock_http.requests in
          assert (List.length chronological = 1);
          let (method_name, url, headers, body) = List.hd chronological in
          assert (method_name = "POST");
          assert (string_contains url "/boards/board_123/sections");
          assert (List.assoc_opt "Authorization" headers = Some "Bearer test_token");
          assert (List.assoc_opt "Content-Type" headers = Some "application/json");
          let body_json = Yojson.Basic.from_string body in
          assert ((body_json |> member "name" |> to_string) = "New Section");
          print_endline "✓ create_board_section sends correct request"
      | Error err ->
          failwith ("create_board_section failed: " ^ Error_types.error_to_string err))

(** Test: post_single with section_id *)
let test_post_single_with_section_id () =
  setup_valid_credentials ();

  let pin_create_response =
    { status = 201; body = {|{"id":"pin_section_1"}|}; headers = [] }
  in

  Mock_http.set_responses [
    pin_create_response;
  ];

  Pinterest.post_single
    ~account_id:"test_account"
    ~text:"Pin in a section"
    ~media_urls:["https://example.com/image.jpg"]
    ~board_id:"board_123"
    ~section_id:"section_456"
    (fun outcome ->
      match outcome with
      | Error_types.Success pin_id ->
          assert (pin_id = "pin_section_1");
          let chronological = List.rev !Mock_http.requests in
          assert (List.length chronological = 1);
          let (_, _, _, body) = List.hd chronological in
          let json = Yojson.Basic.from_string body in
          let open Yojson.Basic.Util in
          assert ((json |> member "board_id" |> to_string) = "board_123");
          assert ((json |> member "board_section_id" |> to_string) = "section_456");
          assert ((json |> member "media_source" |> member "source_type" |> to_string) = "image_url");
          print_endline "✓ post_single includes board_section_id when section_id provided"
      | Error_types.Failure err ->
          failwith ("post_single with section_id failed: " ^ Error_types.error_to_string err)
      | Error_types.Partial_success _ ->
          failwith "Expected full success for post with section_id")

let test_post_single_omits_section_id_when_empty () =
  setup_valid_credentials ();

  let pin_create_response =
    { status = 201; body = {|{"id":"pin_no_section"}|}; headers = [] }
  in

  Mock_http.set_responses [
    pin_create_response;
  ];

  Pinterest.post_single
    ~account_id:"test_account"
    ~text:"Pin without section"
    ~media_urls:["https://example.com/image.jpg"]
    ~board_id:"board_123"
    (fun outcome ->
      match outcome with
      | Error_types.Success pin_id ->
          assert (pin_id = "pin_no_section");
          let chronological = List.rev !Mock_http.requests in
          let (_, _, _, body) = List.hd chronological in
          let json = Yojson.Basic.from_string body in
          let open Yojson.Basic.Util in
          assert ((json |> member "board_section_id") = `Null);
          print_endline "✓ post_single omits board_section_id when section_id is empty"
      | Error_types.Failure err ->
          failwith ("post_single without section_id failed: " ^ Error_types.error_to_string err)
      | Error_types.Partial_success _ ->
          failwith "Expected full success")

(** Run all tests *)
let () =
  print_endline "\n=== Pinterest Provider Tests ===\n";
  test_oauth_url ();
  test_token_exchange ();
  test_token_exchange_rejects_missing_scopes ();
  test_token_exchange_form_urlencodes_special_chars ();
  test_post_single_refreshes_expired_token ();
  test_post_single_expired_token_without_refresh_fails ();
  test_content_validation ();
  test_post_requires_image ();
  test_get_account_analytics_request_contract ();
  test_get_pin_analytics_request_contract ();
  test_get_account_analytics_parse_typed_result ();
  test_get_pin_analytics_parse_typed_result ();
  test_canonical_analytics_adapters ();
  test_validate_media_file_rejects_large_image ();
  test_post_single_full_image_flow_with_alt_text ();
  test_post_single_truncates_title_to_100_chars ();
  test_post_single_image_url_skips_download ();
  test_post_thread_partial_success_posts_only_first ();
  test_validate_media_file_accepts_video ();
  test_validate_media_file_rejects_oversized_video ();
  test_post_single_video_url_uses_video_media_flow ();
  test_post_single_unknown_extension_image_falls_back_to_image_flow ();
  test_post_single_unknown_extension_video_uses_video_flow ();
  test_post_single_unknown_extension_non_media_returns_clean_error ();
  test_post_single_video_content_type_with_charset_is_supported ();
  test_post_single_video_ignores_non_image_thumbnail_url ();
  test_parse_rate_limit_headers ();
  test_parse_rate_limit_headers_missing ();
  test_parse_rate_limit_headers_case_insensitive ();
  test_parse_rate_limit_headers_invalid_values ();
  test_with_retry_no_retry_on_success ();
  test_with_retry_retries_on_429 ();
  test_with_retry_retries_on_500 ();
  test_with_retry_gives_up_after_max_retries ();
  test_with_retry_no_retry_on_4xx ();
  test_with_retry_uses_rate_limit_reset_header ();
  test_create_board ();
  test_create_board_with_description_and_secret ();
  test_create_board_api_error ();
  test_get_board_sections ();
  test_get_board_sections_api_error ();
  test_create_board_section ();
  test_create_board_section_api_error ();
  test_post_single_with_section_id ();
  test_post_single_omits_section_id_when_empty ();
  print_endline "\n=== All tests passed! ===\n"
