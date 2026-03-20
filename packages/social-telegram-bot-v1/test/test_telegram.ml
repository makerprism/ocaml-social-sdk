open Social_core

let string_contains s substr =
  try
    ignore (Str.search_forward (Str.regexp_string substr) s 0);
    true
  with Not_found -> false

module Mock_http = struct
  let requests = ref []
  let response_queue = ref []
  let error_queue = ref []

  let reset () =
    requests := [];
    response_queue := [];
    error_queue := []

  let set_responses responses =
    response_queue := responses

  let set_errors errors =
    error_queue := errors

  let next_response () =
    match !response_queue with
    | [] -> None
    | response :: rest ->
        response_queue := rest;
        Some response

  let next_error () =
    match !error_queue with
    | [] -> None
    | err :: rest ->
        error_queue := rest;
        Some err

  include
    (struct
      let get ?(headers = []) url on_success on_error =
        requests := ("GET", url, headers, "") :: !requests;
        match next_response () with
        | Some response -> on_success response
        | None ->
            (match next_error () with
             | Some err -> on_error err
             | None -> on_error "No mock response set")

      let post ?(headers = []) ?(body = "") url on_success on_error =
        requests := ("POST", url, headers, body) :: !requests;
        match next_response () with
        | Some response -> on_success response
        | None ->
            (match next_error () with
             | Some err -> on_error err
             | None -> on_error "No mock response set")

      let post_multipart ?(headers = []) ~parts:_ url on_success on_error =
        requests := ("POST_MULTIPART", url, headers, "") :: !requests;
        match next_response () with
        | Some response -> on_success response
        | None ->
            (match next_error () with
             | Some err -> on_error err
             | None -> on_error "No mock response set")

      let put ?(headers = []) ?(body = "") url on_success on_error =
        requests := ("PUT", url, headers, body) :: !requests;
        match next_response () with
        | Some response -> on_success response
        | None ->
            (match next_error () with
             | Some err -> on_error err
             | None -> on_error "No mock response set")

      let delete ?(headers = []) url on_success on_error =
        requests := ("DELETE", url, headers, "") :: !requests;
        match next_response () with
        | Some response -> on_success response
        | None ->
            (match next_error () with
             | Some err -> on_error err
             | None -> on_error "No mock response set")
    end : HTTP_CLIENT)
end

module Mock_config = struct
  module Http = Mock_http

  let env_vars = ref []
  let credentials_store = ref []
  let chat_ids = ref []
  let resolver_calls = ref []
  let health_updates = ref []

  let reset () =
    env_vars := [];
    credentials_store := [];
    chat_ids := [];
    resolver_calls := [];
    health_updates := [];
    Mock_http.reset ()

  let get_env key =
    List.assoc_opt key !env_vars

  let get_credentials ~account_id on_success on_error =
    match List.assoc_opt account_id !credentials_store with
    | Some creds -> on_success creds
    | None -> on_error "missing credentials"

  let update_credentials ~account_id ~credentials on_success _on_error =
    credentials_store := (account_id, credentials) :: List.remove_assoc account_id !credentials_store;
    on_success ()

  let encrypt value on_success _on_error = on_success value
  let decrypt value on_success _on_error = on_success value

  let update_health_status ~account_id ~status ~error_message on_success _on_error =
    health_updates := (account_id, status, error_message) :: !health_updates;
    on_success ()

  let get_chat_id ~account_id ~target on_success on_error =
    resolver_calls := (account_id, target) :: !resolver_calls;
    match List.assoc_opt target !chat_ids with
    | Some chat_id -> on_success chat_id
    | None -> on_error "chat target not found"
end

module Telegram = Social_telegram_bot_v1.Make (Mock_config)

let default_credentials = {
  access_token = "12345:ABCDEF_secret_token";
  refresh_token = None;
  expires_at = None;
  auth_type = Bearer;
}

let setup () =
  Mock_config.reset ();
  Mock_config.credentials_store := [ ("acct-1", default_credentials) ];
  Mock_config.chat_ids := [ ("default", "-100000001"); ("@teamchannel", "-100222333") ]

let expect_single_post_request ~expected_method ~check_body =
  let requests = List.rev !Mock_http.requests in
  match requests with
  | [ ("POST", url, _headers, body) ] ->
      assert (string_contains url expected_method);
      check_body body
  | _ -> failwith "unexpected number of HTTP requests"

let test_post_single_send_message_contract () =
  setup ();
  Mock_http.set_responses
    [ { status = 200; headers = []; body = {|{"ok":true,"result":{"message_id":101}}|} } ];
  Telegram.post_single ~account_id:"acct-1" ~text:"hello" ~media_urls:[] ~target:"@teamchannel"
    (function
      | Error_types.Success post_id ->
          assert (post_id = "101");
          expect_single_post_request
            ~expected_method:"/sendMessage"
            ~check_body:(fun body ->
              assert (string_contains body {|"chat_id":"-100222333"|});
              assert (string_contains body {|"text":"hello"|}));
          print_endline "ok: sendMessage contract"
      | _ -> failwith "expected successful sendMessage post")

let test_post_single_send_photo_contract () =
  setup ();
  Mock_http.set_responses
    [ { status = 200; headers = []; body = {|{"ok":true,"result":{"message_id":102}}|} } ];
  Telegram.post_single
    ~account_id:"acct-1"
    ~text:"caption"
    ~media_urls:[ "https://cdn.example.com/photo.jpg" ]
    ~target:"@teamchannel"
    (function
      | Error_types.Success post_id ->
          assert (post_id = "102");
          expect_single_post_request
            ~expected_method:"/sendPhoto"
            ~check_body:(fun body ->
              assert (string_contains body {|"chat_id":"-100222333"|});
              assert (string_contains body {|"photo":"https://cdn.example.com/photo.jpg"|});
              assert (string_contains body {|"caption":"caption"|}));
          print_endline "ok: sendPhoto contract"
      | _ -> failwith "expected successful sendPhoto post")

let test_post_single_send_video_contract () =
  setup ();
  Mock_http.set_responses
    [ { status = 200; headers = []; body = {|{"ok":true,"result":{"message_id":103}}|} } ];
  Telegram.post_single
    ~account_id:"acct-1"
    ~text:"clip"
    ~media_urls:[ "https://cdn.example.com/video.mp4" ]
    ~target:"@teamchannel"
    (function
      | Error_types.Success post_id ->
          assert (post_id = "103");
          expect_single_post_request
            ~expected_method:"/sendVideo"
            ~check_body:(fun body ->
              assert (string_contains body {|"chat_id":"-100222333"|});
              assert (string_contains body {|"video":"https://cdn.example.com/video.mp4"|});
              assert (string_contains body {|"caption":"clip"|}));
          print_endline "ok: sendVideo contract"
      | _ -> failwith "expected successful sendVideo post")

let test_target_resolution_uses_resolver_for_named_target () =
  setup ();
  Mock_http.set_responses
    [ { status = 200; headers = []; body = {|{"ok":true,"result":{"message_id":120}}|} } ];
  Telegram.post_single ~account_id:"acct-1" ~text:"hello" ~media_urls:[] ~target:"@teamchannel"
    (function
      | Error_types.Success _ ->
          assert (!Mock_config.resolver_calls = [ ("acct-1", "@teamchannel") ]);
          print_endline "ok: named target resolved"
      | _ -> failwith "expected post success with resolved target")

let test_target_resolution_skips_resolver_for_negative_chat_id () =
  setup ();
  Mock_http.set_responses
    [ { status = 200; headers = []; body = {|{"ok":true,"result":{"message_id":121}}|} } ];
  Telegram.post_single ~account_id:"acct-1" ~text:"hello" ~media_urls:[] ~target:"-100999"
    (function
      | Error_types.Success _ ->
          assert (!Mock_config.resolver_calls = []);
          print_endline "ok: negative chat id bypasses resolver"
      | _ -> failwith "expected post success for direct negative chat id")

let test_target_resolution_rejects_likely_dm_target () =
  setup ();
  Telegram.post_single ~account_id:"acct-1" ~text:"hello" ~media_urls:[] ~target:"123456789"
    (function
      | Error_types.Failure (Error_types.Validation_error [ Error_types.Invalid_url _ ]) ->
          print_endline "ok: dm target rejected"
      | _ -> failwith "expected validation failure for likely DM target")

let test_validation_rejects_empty_payload () =
  setup ();
  Telegram.post_single ~account_id:"acct-1" ~text:"  " ~media_urls:[]
    (function
      | Error_types.Failure (Error_types.Validation_error [ Error_types.Text_empty ]) ->
          print_endline "ok: empty payload rejected"
      | _ -> failwith "expected text empty validation error")

let test_validation_rejects_too_many_media () =
  setup ();
  Telegram.post_single
    ~account_id:"acct-1"
    ~text:"hello"
    ~media_urls:[ "https://cdn.example.com/a.jpg"; "https://cdn.example.com/b.jpg" ]
    (function
      | Error_types.Failure (Error_types.Validation_error [ Error_types.Too_many_media _ ]) ->
          print_endline "ok: too many media rejected"
      | _ -> failwith "expected too-many-media validation error")

let test_validation_rejects_unsupported_media_route () =
  setup ();
  Telegram.post_single
    ~account_id:"acct-1"
    ~text:"hello"
    ~media_urls:[ "https://cdn.example.com/file.bin" ]
    (function
      | Error_types.Failure (Error_types.Validation_error [ Error_types.Media_unsupported_format _ ]) ->
          print_endline "ok: unsupported media format rejected"
      | _ -> failwith "expected unsupported media validation error")

let test_error_mapping_401_to_token_invalid () =
  setup ();
  Mock_http.set_responses
    [ { status = 401; headers = []; body = {|{"ok":false,"description":"unauthorized"}|} } ];
  Telegram.post_single ~account_id:"acct-1" ~text:"hello" ~media_urls:[]
    (function
      | Error_types.Failure (Error_types.Auth_error Error_types.Token_invalid) ->
          print_endline "ok: 401 mapped token invalid"
      | _ -> failwith "expected token_invalid auth error")

let test_error_mapping_403_to_insufficient_permissions () =
  setup ();
  Mock_http.set_responses
    [ { status = 403; headers = []; body = {|{"ok":false,"description":"forbidden"}|} } ];
  Telegram.post_single ~account_id:"acct-1" ~text:"hello" ~media_urls:[]
    (function
      | Error_types.Failure (Error_types.Auth_error (Error_types.Insufficient_permissions _)) ->
          print_endline "ok: 403 mapped insufficient permissions"
      | _ -> failwith "expected insufficient permissions auth error")

let test_error_mapping_429_with_retry_after_payload () =
  setup ();
  Mock_http.set_responses
    [
      {
        status = 200;
        headers = [];
        body =
          {|{"ok":false,"error_code":429,"description":"too many requests","parameters":{"retry_after":17}}|};
      };
    ];
  Telegram.post_single ~account_id:"acct-1" ~text:"hello" ~media_urls:[]
    (function
      | Error_types.Failure (Error_types.Rate_limited info) ->
          assert (info.retry_after_seconds = Some 17);
          print_endline "ok: 429 mapped with retry_after"
      | _ -> failwith "expected rate-limited error")

let test_error_mapping_http_429_prefers_payload_retry_after () =
  setup ();
  Mock_http.set_responses
    [
      {
        status = 429;
        headers = [ ("Retry-After", "60") ];
        body =
          {|{"ok":false,"error_code":429,"description":"too many requests","parameters":{"retry_after":5}}|};
      };
    ];
  Telegram.post_single ~account_id:"acct-1" ~text:"hello" ~media_urls:[]
    (function
      | Error_types.Failure (Error_types.Rate_limited info) ->
          assert (info.retry_after_seconds = Some 5);
          print_endline "ok: http 429 payload retry_after preferred"
      | _ -> failwith "expected rate-limited error for HTTP 429")

let test_error_mapping_http_429_falls_back_to_header_retry_after () =
  setup ();
  Mock_http.set_responses
    [
      {
        status = 429;
        headers = [ ("Retry-After", "42") ];
        body = {|{"ok":false,"error_code":429,"description":"too many requests"}|};
      };
    ];
  Telegram.post_single ~account_id:"acct-1" ~text:"hello" ~media_urls:[]
    (function
      | Error_types.Failure (Error_types.Rate_limited info) ->
          assert (info.retry_after_seconds = Some 42);
          print_endline "ok: http 429 header retry_after fallback"
      | _ -> failwith "expected rate-limited error for HTTP 429 header fallback")

let test_error_mapping_http_429_invalid_header_retry_after_none () =
  setup ();
  Mock_http.set_responses
    [
      {
        status = 429;
        headers = [ ("Retry-After", "abc") ];
        body = {|{"ok":false,"error_code":429,"description":"too many requests"}|};
      };
    ];
  Telegram.post_single ~account_id:"acct-1" ~text:"hello" ~media_urls:[]
    (function
      | Error_types.Failure (Error_types.Rate_limited info) ->
          assert (info.retry_after_seconds = None);
          print_endline "ok: http 429 invalid header retry_after ignored"
      | _ -> failwith "expected rate-limited error with no retry_after")

let test_invalid_access_token_format_maps_token_invalid () =
  setup ();
  Mock_config.credentials_store :=
    [ ("acct-1", { default_credentials with access_token = "not-a-bot-token" }) ];
  Telegram.post_single ~account_id:"acct-1" ~text:"hello" ~media_urls:[]
    (function
      | Error_types.Failure (Error_types.Auth_error Error_types.Token_invalid) ->
          let requests = List.rev !Mock_http.requests in
          assert (requests = []);
          print_endline "ok: invalid token format rejected before network"
      | _ -> failwith "expected token_invalid for malformed bot token")

let test_error_mapping_malformed_success_payload () =
  setup ();
  Mock_http.set_responses
    [ { status = 200; headers = []; body = {|{"ok":true,"result":{"chat":{"id":1}}}|} } ];
  Telegram.post_single ~account_id:"acct-1" ~text:"hello" ~media_urls:[]
    (function
      | Error_types.Failure (Error_types.Internal_error msg) ->
          assert (string_contains msg "missing result.message_id");
          print_endline "ok: malformed success payload mapped internal"
      | _ -> failwith "expected internal error on malformed success payload")

let test_error_redaction_no_token_leakage () =
  setup ();
  Mock_http.set_responses
    [
      {
        status = 500;
        headers = [];
        body =
          {|{"ok":false,"description":"token 12345:ABCDEF_secret_token leaked"}|};
      };
    ];
  Telegram.post_single ~account_id:"acct-1" ~text:"hello" ~media_urls:[]
    (function
      | Error_types.Failure (Error_types.Api_error api_err) ->
          (match api_err.raw_response with
           | Some raw ->
               assert (string_contains raw "[REDACTED_TOKEN]");
               assert (not (string_contains raw "ABCDEF_secret_token"))
           | None -> failwith "expected raw_response for API error");
          print_endline "ok: token redacted in api error"
      | _ -> failwith "expected api error for 500 response")

let test_post_thread_success () =
  setup ();
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = {|{"ok":true,"result":{"message_id":1}}|} };
      { status = 200; headers = []; body = {|{"ok":true,"result":{"message_id":2}}|} };
      { status = 200; headers = []; body = {|{"ok":true,"result":{"message_id":3}}|} };
    ];
  Telegram.post_thread
    ~account_id:"acct-1"
    ~texts:[ "one"; "two"; "three" ]
    ~media_urls_per_post:[ []; []; [] ]
    (function
      | Error_types.Success thread_result ->
          assert (thread_result.posted_ids = [ "1"; "2"; "3" ]);
          assert (thread_result.failed_at_index = None);
          assert (thread_result.total_requested = 3);
          print_endline "ok: post_thread success"
      | _ -> failwith "expected successful thread outcome")

let test_post_thread_partial_success_on_mid_failure () =
  setup ();
  Mock_http.set_responses
    [
      { status = 200; headers = []; body = {|{"ok":true,"result":{"message_id":10}}|} };
      { status = 403; headers = []; body = {|{"ok":false,"description":"forbidden"}|} };
    ];
  Telegram.post_thread
    ~account_id:"acct-1"
    ~texts:[ "one"; "two" ]
    ~media_urls_per_post:[ []; [] ]
    (function
      | Error_types.Partial_success { result; warnings } ->
          assert (result.posted_ids = [ "10" ]);
          assert (result.failed_at_index = Some 1);
          assert (result.total_requested = 2);
          assert (List.length warnings = 1);
          print_endline "ok: post_thread partial success"
      | _ -> failwith "expected partial success for mid-thread failure")

let test_post_thread_first_failure_is_failure () =
  setup ();
  Mock_http.set_responses
    [ { status = 403; headers = []; body = {|{"ok":false,"description":"forbidden"}|} } ];
  Telegram.post_thread
    ~account_id:"acct-1"
    ~texts:[ "one"; "two" ]
    ~media_urls_per_post:[ []; [] ]
    (function
      | Error_types.Failure (Error_types.Auth_error (Error_types.Insufficient_permissions _)) ->
          let requests = List.rev !Mock_http.requests in
          assert (List.length requests = 1);
          print_endline "ok: post_thread first failure"
      | _ -> failwith "expected direct failure when first post fails")

let test_post_thread_rejects_length_mismatch () =
  setup ();
  Telegram.post_thread
    ~account_id:"acct-1"
    ~texts:[ "one"; "two" ]
    ~media_urls_per_post:[ [] ]
    (function
      | Error_types.Failure (Error_types.Validation_error [ Error_types.Thread_post_invalid _ ]) ->
          let requests = List.rev !Mock_http.requests in
          assert (requests = []);
          print_endline "ok: post_thread length mismatch rejected"
      | _ -> failwith "expected validation failure for post_thread input length mismatch")

let test_resolver_rejects_dm_chat_id_resolution () =
  setup ();
  Mock_config.chat_ids := [ ("@bad", "123456") ];
  Telegram.post_single ~account_id:"acct-1" ~text:"hello" ~media_urls:[] ~target:"@bad"
    (function
      | Error_types.Failure (Error_types.Validation_error [ Error_types.Invalid_url _ ]) ->
          let requests = List.rev !Mock_http.requests in
          assert (requests = []);
          print_endline "ok: resolver DM chat-id rejected"
      | _ -> failwith "expected validation failure for resolver DM target")

let test_missing_credentials_fails_without_network_call () =
  setup ();
  Mock_config.credentials_store := [];
  Telegram.post_single ~account_id:"acct-1" ~text:"hello" ~media_urls:[]
    (function
      | Error_types.Failure (Error_types.Auth_error Error_types.Missing_credentials) ->
          let requests = List.rev !Mock_http.requests in
          assert (requests = []);
          print_endline "ok: missing credentials short-circuits network"
      | _ -> failwith "expected missing credentials auth error")

let test_validate_access_rejects_dm_target_without_network () =
  setup ();
  Telegram.validate_access ~account_id:"acct-1" ~target:"123456789"
    (function
      | Error_types.Failure (Error_types.Validation_error [ Error_types.Invalid_url _ ]) ->
          let requests = List.rev !Mock_http.requests in
          assert (requests = []);
          print_endline "ok: validate_access rejects dm target"
      | _ -> failwith "expected validate_access dm-target validation error")

let test_validate_access_maps_401_to_token_invalid () =
  setup ();
  Mock_http.set_responses
    [ { status = 401; headers = []; body = {|{"ok":false,"description":"unauthorized"}|} } ];
  Telegram.validate_access ~account_id:"acct-1" ~target:"@teamchannel"
    (function
      | Error_types.Failure (Error_types.Auth_error Error_types.Token_invalid) ->
          print_endline "ok: validate_access maps 401 token invalid"
      | _ -> failwith "expected token_invalid from validate_access")

let test_validate_access_payload_error_maps_api_error () =
  setup ();
  Mock_http.set_responses
    [ { status = 200; headers = []; body = {|{"ok":false,"error_code":400,"description":"bad request"}|} } ];
  Telegram.validate_access ~account_id:"acct-1" ~target:"@teamchannel"
    (function
      | Error_types.Failure (Error_types.Api_error api_err) ->
          assert (api_err.status_code = 200);
          assert (string_contains api_err.message "bad request");
          print_endline "ok: validate_access payload error maps api_error"
      | _ -> failwith "expected api_error for validate_access payload-level failure")

let test_post_single_network_error_redacts_token () =
  setup ();
  Mock_http.set_errors [ "network failure for 12345:ABCDEF_secret_token" ];
  Telegram.post_single ~account_id:"acct-1" ~text:"hello" ~media_urls:[]
    (function
      | Error_types.Failure (Error_types.Network_error (Error_types.Connection_failed msg)) ->
          assert (string_contains msg "[REDACTED_TOKEN]");
          assert (not (string_contains msg "ABCDEF_secret_token"));
          print_endline "ok: network error token redacted"
      | _ -> failwith "expected redacted network error")

let test_health_status_updated_on_successful_post () =
  setup ();
  Mock_http.set_responses
    [ { status = 200; headers = []; body = {|{"ok":true,"result":{"message_id":777}}|} } ];
  Telegram.post_single ~account_id:"acct-1" ~text:"hello" ~media_urls:[]
    (function
      | Error_types.Success _ ->
          (match !Mock_config.health_updates with
           | (account_id, status, error_message) :: _ ->
               assert (account_id = "acct-1");
               assert (status = "healthy");
               assert (error_message = None)
           | [] -> failwith "expected health status update on success");
          print_endline "ok: health set healthy on success"
      | _ -> failwith "expected success for health status test")

let test_health_status_updated_on_auth_failure () =
  setup ();
  Mock_http.set_responses
    [ { status = 401; headers = []; body = {|{"ok":false,"description":"unauthorized"}|} } ];
  Telegram.post_single ~account_id:"acct-1" ~text:"hello" ~media_urls:[]
    (function
      | Error_types.Failure (Error_types.Auth_error Error_types.Token_invalid) ->
          (match !Mock_config.health_updates with
           | (account_id, status, error_message) :: _ ->
               assert (account_id = "acct-1");
               assert (status = "token_invalid");
               assert (error_message = Some "Telegram token is invalid")
           | [] -> failwith "expected health status update on auth failure");
          print_endline "ok: health set token_invalid on auth failure"
      | _ -> failwith "expected token_invalid auth failure")

let test_validate_access_preflight_get_me () =
  setup ();
  Mock_http.set_responses
    [ { status = 200; headers = []; body = {|{"ok":true,"result":{"id":1}}|} } ];
  Telegram.validate_access ~account_id:"acct-1" ~target:"@teamchannel"
    (function
      | Error_types.Success () ->
          let requests = List.rev !Mock_http.requests in
          (match requests with
           | [ ("GET", url, _, "") ] ->
               assert (string_contains url "/getMe");
               print_endline "ok: validate_access preflight"
           | _ -> failwith "unexpected request pattern for validate_access")
      | _ -> failwith "expected validate_access success")

let () =
  test_post_single_send_message_contract ();
  test_post_single_send_photo_contract ();
  test_post_single_send_video_contract ();
  test_target_resolution_uses_resolver_for_named_target ();
  test_target_resolution_skips_resolver_for_negative_chat_id ();
  test_target_resolution_rejects_likely_dm_target ();
  test_validation_rejects_empty_payload ();
  test_validation_rejects_too_many_media ();
  test_validation_rejects_unsupported_media_route ();
  test_error_mapping_401_to_token_invalid ();
  test_error_mapping_403_to_insufficient_permissions ();
  test_error_mapping_429_with_retry_after_payload ();
  test_error_mapping_http_429_prefers_payload_retry_after ();
  test_error_mapping_http_429_falls_back_to_header_retry_after ();
  test_error_mapping_http_429_invalid_header_retry_after_none ();
  test_invalid_access_token_format_maps_token_invalid ();
  test_error_mapping_malformed_success_payload ();
  test_error_redaction_no_token_leakage ();
  test_post_thread_success ();
  test_post_thread_partial_success_on_mid_failure ();
  test_post_thread_first_failure_is_failure ();
  test_post_thread_rejects_length_mismatch ();
  test_resolver_rejects_dm_chat_id_resolution ();
  test_missing_credentials_fails_without_network_call ();
  test_validate_access_rejects_dm_target_without_network ();
  test_validate_access_maps_401_to_token_invalid ();
  test_validate_access_payload_error_maps_api_error ();
  test_post_single_network_error_redacts_token ();
  test_health_status_updated_on_successful_post ();
  test_health_status_updated_on_auth_failure ();
  test_validate_access_preflight_get_me ();
  print_endline "All Telegram tests passed"
