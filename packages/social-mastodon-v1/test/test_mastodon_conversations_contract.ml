(** Focused conversations API contract tests for Mastodon provider. *)

module Conversations_http = struct
  let string_contains s sub =
    try
      let _ = Str.search_forward (Str.regexp_string sub) s 0 in
      true
    with Not_found -> false

  let last_get_url : string option ref = ref None
  let last_post_url : string option ref = ref None
  let next_post_response : Social_core.response option ref = ref None

  let reset () =
    last_get_url := None;
    last_post_url := None;
    next_post_response := None

  let set_next_post_response resp =
    next_post_response := Some resp

  let get ?headers:_ url on_success _on_error =
    last_get_url := Some url;
    if String.ends_with ~suffix:"/api/v1/accounts/verify_credentials" url then
      on_success { Social_core.status = 200; headers = []; body = {|{"id":"123"}|} }
    else if string_contains url "/api/v1/conversations" then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body =
          {|[{"id":"c1","unread":true,"accounts":[{"id":"22","acct":"tester@example.com","username":"tester"}],"last_status":{"id":"s1","content":"<p>dm</p>","account":{"acct":"tester"}}}]|};
      }
    else
      on_success { Social_core.status = 404; headers = []; body = "not found" }

  let post ?headers:_ ?body:_ url on_success _on_error =
    last_post_url := Some url;
    match !next_post_response with
    | Some resp ->
        next_post_response := None;
        on_success resp
    | None ->
        on_success { Social_core.status = 200; headers = []; body = "{}" }

  let post_multipart ?headers:_ ~parts:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }

  let put ?headers:_ ?body:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }

  let delete ?headers:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }
end

module Conversations_config = struct
  module Http = Conversations_http

  let get_env _key = None
  let get_credentials ~account_id:_ on_success _on_error =
    on_success {
      Social_core.access_token = {|{"access_token":"dm_token","instance_url":"https://mastodon.social"}|};
      refresh_token = None;
      expires_at = None;
      auth_type = Social_core.Bearer;
      scope = None;
    }
  let update_credentials ~account_id:_ ~credentials:_ on_success _on_error = on_success ()
  let encrypt _data on_success _on_error = on_success "encrypted"
  let decrypt _data on_success _on_error = on_success "decrypted"
  let sleep ~seconds:_ on_success _on_error = on_success ()
  let update_health_status ~account_id:_ ~status:_ ~error_message:_ on_success _on_error = on_success ()
end

module Mastodon = Social_mastodon_v1.Make (Conversations_config)

let string_contains s sub =
  try
    let _ = Str.search_forward (Str.regexp_string sub) s 0 in
    true
  with Not_found -> false

let test_get_conversations_contract () =
  Printf.printf "Test: get conversations query contract... ";
  Conversations_http.reset ();
  let got_result = ref false in
  Mastodon.get_conversations
    ~account_id:"acct"
    ~limit:15
    ~max_id:(Some "200")
    ~since_id:(Some "150")
    ~min_id:(Some "160")
    ~unread_only:true
    (function
      | Ok conversations ->
          got_result := true;
          assert (List.length conversations = 1);
          let c = List.hd conversations in
          assert (String.equal c.id "c1");
          assert c.unread;
          assert (List.length c.accounts = 1)
      | Error err -> failwith (Error_types.error_to_string err));
  assert !got_result;
  let url = Option.get !(Conversations_http.last_get_url) in
  assert (string_contains url "/api/v1/conversations");
  assert (string_contains url "limit=15");
  assert (string_contains url "max_id=200");
  assert (string_contains url "since_id=150");
  assert (string_contains url "min_id=160");
  assert (string_contains url "unread=true");
  Printf.printf "✓\n"

let test_conversation_actions_contract () =
  Printf.printf "Test: conversation read/remove contract... ";
  Conversations_http.reset ();

  let got_read = ref false in
  Mastodon.mark_conversation_read
    ~account_id:"acct"
    ~conversation_id:"c1"
    (function
      | Ok () -> got_read := true
      | Error err -> failwith (Error_types.error_to_string err));
  assert !got_read;
  let read_url = Option.get !(Conversations_http.last_post_url) in
  assert (String.ends_with ~suffix:"/api/v1/conversations/c1/read" read_url);

  let got_remove = ref false in
  Mastodon.remove_conversation
    ~account_id:"acct"
    ~conversation_id:"c1"
    (function
      | Ok () -> got_remove := true
      | Error err -> failwith (Error_types.error_to_string err));
  assert !got_remove;
  let remove_url = Option.get !(Conversations_http.last_post_url) in
  assert (String.ends_with ~suffix:"/api/v1/conversations/c1/remove" remove_url);
  Printf.printf "✓\n"

let test_conversation_error_mapping_contract () =
  Printf.printf "Test: conversation actions error mapping contract... ";
  Conversations_http.reset ();

  Conversations_http.set_next_post_response {
    Social_core.status = 401;
    headers = [];
    body = {|{"error":"The access token is invalid"}|};
  };
  let got_read_auth_error = ref false in
  Mastodon.mark_conversation_read
    ~account_id:"acct"
    ~conversation_id:"c1"
    (function
      | Error (Error_types.Auth_error Error_types.Token_invalid) ->
          got_read_auth_error := true
      | Error err -> failwith (Printf.sprintf "Unexpected mark_conversation_read error: %s" (Error_types.error_to_string err))
      | Ok () -> failwith "Expected mark_conversation_read auth error");
  assert !got_read_auth_error;

  Conversations_http.set_next_post_response {
    Social_core.status = 429;
    headers = [ ("Retry-After", "9") ];
    body = {|{"error":"rate limit exceeded"}|};
  };
  let got_remove_rate_limit = ref false in
  Mastodon.remove_conversation
    ~account_id:"acct"
    ~conversation_id:"c1"
    (function
      | Error (Error_types.Rate_limited { retry_after_seconds = Some 9; _ }) ->
          got_remove_rate_limit := true
      | Error err -> failwith (Printf.sprintf "Unexpected remove_conversation error: %s" (Error_types.error_to_string err))
      | Ok () -> failwith "Expected remove_conversation rate-limit error");
  assert !got_remove_rate_limit;
  Printf.printf "✓\n"

let () =
  Printf.printf "\n=== Mastodon Conversations Contract Tests ===\n\n";
  test_get_conversations_contract ();
  test_conversation_actions_contract ();
  test_conversation_error_mapping_contract ();
  Printf.printf "\n✓ Conversations contract tests passed\n"
