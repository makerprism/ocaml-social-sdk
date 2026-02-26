open Social_core
open Social_telegram_bot_v1

let string_contains s needle =
  try
    ignore (Str.search_forward (Str.regexp_string needle) s 0);
    true
  with Not_found -> false

module Mock_http = struct
  type post_call = {
    url : string;
    headers : (string * string) list;
    body : string option;
  }

  let post_calls : post_call list ref = ref []
  let response_queue : response list ref = ref []

  let reset () =
    post_calls := [];
    response_queue := []

  let set_responses responses =
    response_queue := responses

  let next_response () =
    match !response_queue with
    | [] -> None
    | x :: xs ->
        response_queue := xs;
        Some x

  let get ?headers:_ _url _on_success on_error =
    on_error "Mock GET not configured"

  let post ?(headers=[]) ?body url on_success on_error =
    post_calls := { url; headers; body } :: !post_calls;
    match next_response () with
    | Some resp -> on_success resp
    | None -> on_error "No mock response set"

  let put ?headers:_ ?body:_ _url _on_success on_error =
    on_error "Mock PUT not configured"

  let delete ?headers:_ _url _on_success on_error =
    on_error "Mock DELETE not configured"

  let post_multipart ?headers:_ ~parts:_ _url _on_success on_error =
    on_error "Mock multipart POST not configured"
end

module Mock_config = struct
  module Http = Mock_http

  let env_vars : (string * string) list ref = ref []
  let creds = ref {
    access_token = "12345:telegram_token_for_tests";
    refresh_token = None;
    expires_at = None;
    token_type = "Bearer";
  }
  let chat_id = ref "-100123456"
  let health_statuses : (string * string * string option) list ref = ref []

  let reset () =
    env_vars := [];
    creds := {
      access_token = "12345:telegram_token_for_tests";
      refresh_token = None;
      expires_at = None;
      token_type = "Bearer";
    };
    chat_id := "-100123456";
    health_statuses := [];
    Mock_http.reset ()

  let get_env key = List.assoc_opt key !env_vars
  let get_credentials ~account_id:_ on_success _on_error = on_success !creds
  let update_credentials ~account_id:_ ~credentials on_success _on_error =
    creds := credentials;
    on_success ()
  let encrypt s on_success _on_error = on_success s
  let decrypt s on_success _on_error = on_success s
  let get_chat_id ~account_id:_ on_success _on_error = on_success !chat_id
  let update_health_status ~account_id ~status ~error_message on_success _on_error =
    health_statuses := (account_id, status, error_message) :: !health_statuses;
    on_success ()
end

module Telegram = Make (Mock_config)

let decode_form_body = function
  | None -> []
  | Some body -> Uri.query_of_encoded body

let latest_post_call () =
  match Mock_http.post_calls with
  | { contents = call :: _ } -> call
  | _ -> failwith "Expected at least one POST call"

let test_send_message_contract () =
  Mock_config.reset ();
  Mock_http.set_responses [{ status = 200; headers = []; body = {|{"ok":true,"result":{"message_id":101}}|} }];
  Telegram.post_single
    ~account_id:"acc"
    ~text:"hello channel"
    ~media_urls:[]
    (fun outcome ->
      match outcome with
      | Error_types.Success id -> assert (id = "101")
      | _ -> failwith "Expected success");

  let call = latest_post_call () in
  assert (String.ends_with ~suffix:"/sendMessage" call.url);
  assert (List.assoc "Content-Type" call.headers = "application/x-www-form-urlencoded");
  let form = decode_form_body call.body in
  let chat_id = List.assoc "chat_id" form |> List.hd in
  let text = List.assoc "text" form |> List.hd in
  assert (chat_id = "-100123456");
  assert (text = "hello channel")

let test_send_photo_contract () =
  Mock_config.reset ();
  Mock_http.set_responses [{ status = 200; headers = []; body = {|{"ok":true,"result":{"message_id":202}}|} }];
  Telegram.post_single
    ~account_id:"acc"
    ~text:"photo caption"
    ~media_urls:["https://example.com/photo.jpg"]
    (fun outcome ->
      match outcome with
      | Error_types.Success id -> assert (id = "202")
      | _ -> failwith "Expected success");

  let call = latest_post_call () in
  assert (String.ends_with ~suffix:"/sendPhoto" call.url);
  let form = decode_form_body call.body in
  assert (List.assoc "caption" form |> List.hd = "photo caption");
  assert (List.assoc "photo" form |> List.hd = "https://example.com/photo.jpg")

let test_send_video_contract () =
  Mock_config.reset ();
  Mock_http.set_responses [{ status = 200; headers = []; body = {|{"ok":true,"result":{"message_id":303}}|} }];
  Telegram.post_single
    ~account_id:"acc"
    ~text:"video caption"
    ~media_urls:["https://example.com/video.mp4"]
    (fun outcome ->
      match outcome with
      | Error_types.Success id -> assert (id = "303")
      | _ -> failwith "Expected success");

  let call = latest_post_call () in
  assert (String.ends_with ~suffix:"/sendVideo" call.url);
  let form = decode_form_body call.body in
  assert (List.assoc "caption" form |> List.hd = "video caption");
  assert (List.assoc "video" form |> List.hd = "https://example.com/video.mp4")

let test_target_resolution_rejects_likely_dm () =
  Mock_config.reset ();
  Mock_config.chat_id := "1234567";
  Telegram.post_single
    ~account_id:"acc"
    ~text:"hi"
    ~media_urls:[]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Internal_error msg) ->
          assert (string_contains msg "Broadcast posting")
      | _ -> failwith "Expected DM-target rejection")

let test_target_resolution_rejects_username_target () =
  Mock_config.reset ();
  Mock_config.chat_id := "@channel_or_user";
  Telegram.post_single
    ~account_id:"acc"
    ~text:"hi"
    ~media_urls:[]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Internal_error msg) ->
          assert (string_contains msg "negative channel/group chat ids")
      | _ -> failwith "Expected username-target rejection")

let test_validation_too_many_media () =
  Mock_config.reset ();
  Telegram.post_single
    ~account_id:"acc"
    ~text:"hello"
    ~media_urls:["https://example.com/a.jpg"; "https://example.com/b.jpg"]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Validation_error errs) ->
          assert (List.exists (function Error_types.Too_many_media _ -> true | _ -> false) errs)
      | _ -> failwith "Expected validation error")

let test_validation_caption_too_long_for_media () =
  Mock_config.reset ();
  let long_caption = String.make 1025 'x' in
  Telegram.post_single
    ~account_id:"acc"
    ~text:long_caption
    ~media_urls:["https://example.com/a.jpg"]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Validation_error errs) ->
          assert (List.exists (function Error_types.Text_too_long _ -> true | _ -> false) errs)
      | _ -> failwith "Expected validation error")

let test_error_mapping_401 () =
  Mock_config.reset ();
  Mock_http.set_responses [{ status = 401; headers = []; body = {|{"ok":false,"error_code":401,"description":"Unauthorized"}|} }];
  Telegram.post_single
    ~account_id:"acc"
    ~text:"hello"
    ~media_urls:[]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Auth_error Error_types.Token_invalid) -> ()
      | _ -> failwith "Expected Token_invalid")

let test_error_mapping_403 () =
  Mock_config.reset ();
  Mock_http.set_responses [{ status = 403; headers = []; body = {|{"ok":false,"error_code":403,"description":"Forbidden: bot is not admin"}|} }];
  Telegram.post_single
    ~account_id:"acc"
    ~text:"hello"
    ~media_urls:[]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Auth_error (Error_types.Insufficient_permissions _)) -> ()
      | _ -> failwith "Expected Insufficient_permissions")

let test_error_mapping_429_with_retry_after () =
  Mock_config.reset ();
  Mock_http.set_responses [{ status = 429; headers = []; body = {|{"ok":false,"error_code":429,"description":"Too Many Requests","parameters":{"retry_after":17}}|} }];
  Telegram.post_single
    ~account_id:"acc"
    ~text:"hello"
    ~media_urls:[]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Rate_limited info) -> assert (info.retry_after_seconds = Some 17)
      | _ -> failwith "Expected Rate_limited")

let test_ok_false_is_error_even_with_200 () =
  Mock_config.reset ();
  Mock_http.set_responses [{ status = 200; headers = []; body = {|{"ok":false,"error_code":403,"description":"Forbidden"}|} }];
  Telegram.post_single
    ~account_id:"acc"
    ~text:"hello"
    ~media_urls:[]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Auth_error (Error_types.Insufficient_permissions _)) -> ()
      | _ -> failwith "Expected mapped error from ok=false")

let test_malformed_payload_maps_to_internal_error () =
  Mock_config.reset ();
  Mock_http.set_responses [{ status = 200; headers = []; body = "not-json" }];
  Telegram.post_single
    ~account_id:"acc"
    ~text:"hello"
    ~media_urls:[]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Internal_error msg) ->
          assert (string_contains msg "Failed to parse Telegram response payload")
      | _ -> failwith "Expected Internal_error")

let test_token_redaction_in_errors () =
  Mock_config.reset ();
  Mock_http.set_responses [{
    status = 500;
    headers = [];
    body = "upstream error for token 12345:telegram_token_for_tests";
  }];
  Telegram.post_single
    ~account_id:"acc"
    ~text:"hello"
    ~media_urls:[]
    (fun outcome ->
      match outcome with
      | Error_types.Failure err ->
          let msg = Error_types.error_to_string err in
          assert (not (string_contains msg "12345:telegram_token_for_tests"));
          assert (string_contains msg "[REDACTED]")
      | _ -> failwith "Expected failure")

let test_post_thread_success () =
  Mock_config.reset ();
  Mock_http.set_responses [
    { status = 200; headers = []; body = {|{"ok":true,"result":{"message_id":1}}|} };
    { status = 200; headers = []; body = {|{"ok":true,"result":{"message_id":2}}|} };
  ];
  Telegram.post_thread
    ~account_id:"acc"
    ~texts:["one"; "two"]
    ~media_urls_per_post:[[]; []]
    (fun outcome ->
      match outcome with
      | Error_types.Success result -> assert (result.Error_types.posted_ids = ["1"; "2"])
      | _ -> failwith "Expected thread success")

let test_post_thread_partial_success () =
  Mock_config.reset ();
  Mock_http.set_responses [
    { status = 200; headers = []; body = {|{"ok":true,"result":{"message_id":1}}|} };
    { status = 403; headers = []; body = {|{"ok":false,"error_code":403,"description":"Forbidden"}|} };
  ];
  Telegram.post_thread
    ~account_id:"acc"
    ~texts:["one"; "two"]
    ~media_urls_per_post:[[]; []]
    (fun outcome ->
      match outcome with
      | Error_types.Partial_success { result; _ } ->
          assert (result.Error_types.posted_ids = ["1"]);
          assert (result.Error_types.failed_at_index = Some 1)
      | _ -> failwith "Expected partial success")

let test_post_thread_first_failure () =
  Mock_config.reset ();
  Mock_http.set_responses [
    { status = 403; headers = []; body = {|{"ok":false,"error_code":403,"description":"Forbidden"}|} };
  ];
  Telegram.post_thread
    ~account_id:"acc"
    ~texts:["one"]
    ~media_urls_per_post:[[]]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Auth_error (Error_types.Insufficient_permissions _)) -> ()
      | _ -> failwith "Expected first-post failure")

let test_post_thread_unsupported_media_rejected () =
  Mock_config.reset ();
  Telegram.post_thread
    ~account_id:"acc"
    ~texts:["one"]
    ~media_urls_per_post:[["https://example.com/file.bin"]]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Validation_error errs) ->
          assert (List.exists (function Error_types.Thread_post_invalid _ -> true | _ -> false) errs)
      | _ -> failwith "Expected validation error")

let test_validate_access_preflight () =
  Mock_config.reset ();
  Mock_http.set_responses [{ status = 200; headers = []; body = {|{"ok":true,"result":{"id":999,"is_bot":true}}|} }];
  Telegram.validate_access
    ~account_id:"acc"
    (function
      | Ok () -> ()
      | Error _ -> failwith "Expected validate_access success")

let run name f =
  Printf.printf "Test: %s... " name;
  f ();
  print_endline "PASSED"

let () =
  run "sendMessage request contract" test_send_message_contract;
  run "sendPhoto request contract" test_send_photo_contract;
  run "sendVideo request contract" test_send_video_contract;
  run "target resolution rejects likely DM" test_target_resolution_rejects_likely_dm;
  run "target resolution rejects username" test_target_resolution_rejects_username_target;
  run "validation too many media" test_validation_too_many_media;
  run "validation caption length" test_validation_caption_too_long_for_media;
  run "error mapping 401" test_error_mapping_401;
  run "error mapping 403" test_error_mapping_403;
  run "error mapping 429 retry_after" test_error_mapping_429_with_retry_after;
  run "ok=false mapping" test_ok_false_is_error_even_with_200;
  run "malformed payload mapping" test_malformed_payload_maps_to_internal_error;
  run "token redaction" test_token_redaction_in_errors;
  run "post_thread success" test_post_thread_success;
  run "post_thread partial success" test_post_thread_partial_success;
  run "post_thread first failure" test_post_thread_first_failure;
  run "post_thread unsupported media" test_post_thread_unsupported_media_rejected;
  run "validate_access preflight" test_validate_access_preflight
