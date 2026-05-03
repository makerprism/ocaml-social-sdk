(** Focused list operations contract tests for Mastodon provider. *)

module List_ops_http = struct
  let string_contains s sub =
    try
      let _ = Str.search_forward (Str.regexp_string sub) s 0 in
      true
    with Not_found -> false

  let last_get_url : string option ref = ref None
  let last_post_url : string option ref = ref None
  let last_post_body : string option ref = ref None
  let last_put_url : string option ref = ref None
  let last_put_body : string option ref = ref None
  let last_delete_url : string option ref = ref None
  let next_post_response : Social_core.response option ref = ref None
  let next_put_response : Social_core.response option ref = ref None
  let next_delete_response : Social_core.response option ref = ref None

  let reset () =
    last_get_url := None;
    last_post_url := None;
    last_post_body := None;
    last_put_url := None;
    last_put_body := None;
    last_delete_url := None;
    next_post_response := None;
    next_put_response := None;
    next_delete_response := None

  let set_next_post_response resp =
    next_post_response := Some resp

  let set_next_put_response resp =
    next_put_response := Some resp

  let set_next_delete_response resp =
    next_delete_response := Some resp

  let get ?headers:_ url on_success _on_error =
    last_get_url := Some url;
    if String.ends_with ~suffix:"/api/v1/accounts/verify_credentials" url then
      on_success { Social_core.status = 200; headers = []; body = {|{"id":"123"}|} }
    else if String.ends_with ~suffix:"/api/v1/lists" url then
      on_success { Social_core.status = 200; headers = []; body = {|[{"id":"l1","title":"Friends"}]|} }
    else if string_contains url "/api/v1/timelines/list/l1" then
      on_success { Social_core.status = 200; headers = []; body = {|[{"id":"s1","content":"<p>List post</p>"}]|} }
    else
      on_success { Social_core.status = 404; headers = []; body = "not found" }

  let post ?headers:_ ?body url on_success _on_error =
    last_post_url := Some url;
    last_post_body := body;
    match !next_post_response with
    | Some resp ->
        next_post_response := None;
        on_success resp
    | None ->
        if string_contains url "/api/v1/lists" then
          on_success { Social_core.status = 200; headers = []; body = {|{"id":"l1","title":"Friends"}|} }
        else
          on_success { Social_core.status = 200; headers = []; body = "{}" }

  let post_multipart ?headers:_ ~parts:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }

  let put ?headers:_ ?body url on_success _on_error =
    last_put_url := Some url;
    last_put_body := body;
    (match !next_put_response with
    | Some resp ->
        next_put_response := None;
        on_success resp
    | None ->
        on_success { Social_core.status = 200; headers = []; body = {|{"id":"l1","title":"Updated"}|} })

  let delete ?headers:_ url on_success _on_error =
    last_delete_url := Some url;
    (match !next_delete_response with
    | Some resp ->
        next_delete_response := None;
        on_success resp
    | None ->
        on_success { Social_core.status = 200; headers = []; body = "{}" })
end

module List_ops_config = struct
  module Http = List_ops_http

  let get_env _key = None
  let get_credentials ~account_id:_ on_success _on_error =
    on_success {
      Social_core.access_token = {|{"access_token":"list_token","instance_url":"https://mastodon.social"}|};
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

module Mastodon = Social_mastodon_v1.Make (List_ops_config)

let test_create_update_delete_list_contract () =
  Printf.printf "Test: create/update/delete list contract... ";
  List_ops_http.reset ();

  let created = ref false in
  Mastodon.create_list ~account_id:"acct" ~title:"Friends" ~replies_policy:(Some "list") ~exclusive:(Some true)
    (function
      | Ok l -> created := true; assert (String.equal l.id "l1")
      | Error err -> failwith (Error_types.error_to_string err));
  assert !created;
  let create_url = Option.get !(List_ops_http.last_post_url) in
  let create_body = Option.get !(List_ops_http.last_post_body) in
  assert (String.ends_with ~suffix:"/api/v1/lists" create_url);
  let create_json = Yojson.Basic.from_string create_body in
  let open Yojson.Basic.Util in
  assert (create_json |> member "title" |> to_string = "Friends");
  assert (create_json |> member "replies_policy" |> to_string = "list");
  assert (create_json |> member "exclusive" |> to_bool);

  let updated = ref false in
  Mastodon.update_list ~account_id:"acct" ~list_id:"l1" ~title:(Some "Updated")
    (function
      | Ok l -> updated := true; assert (String.equal l.id "l1")
      | Error err -> failwith (Error_types.error_to_string err));
  assert !updated;
  let update_url = Option.get !(List_ops_http.last_put_url) in
  assert (String.ends_with ~suffix:"/api/v1/lists/l1" update_url);

  let deleted = ref false in
  Mastodon.delete_list ~account_id:"acct" ~list_id:"l1"
    (function
      | Ok () -> deleted := true
      | Error err -> failwith (Error_types.error_to_string err));
  assert !deleted;
  let delete_url = Option.get !(List_ops_http.last_delete_url) in
  assert (String.ends_with ~suffix:"/api/v1/lists/l1" delete_url);
  Printf.printf "✓\n"

let test_list_accounts_membership_contract () =
  Printf.printf "Test: list add/remove accounts contract... ";
  List_ops_http.reset ();

  let added = ref false in
  Mastodon.add_accounts_to_list ~account_id:"acct" ~list_id:"l1" ~target_account_ids:["a1"; "a2"]
    (function
      | Ok () -> added := true
      | Error err -> failwith (Error_types.error_to_string err));
  assert !added;
  let add_url = Option.get !(List_ops_http.last_post_url) in
  let add_body = Option.get !(List_ops_http.last_post_body) in
  assert (String.ends_with ~suffix:"/api/v1/lists/l1/accounts" add_url);
  let add_json = Yojson.Basic.from_string add_body in
  let open Yojson.Basic.Util in
  let ids = add_json |> member "account_ids" |> to_list |> List.map to_string in
  assert (List.mem "a1" ids && List.mem "a2" ids);

  let removed = ref false in
  Mastodon.remove_accounts_from_list ~account_id:"acct" ~list_id:"l1" ~target_account_ids:["a1"; "a2"]
    (function
      | Ok () -> removed := true
      | Error err -> failwith (Error_types.error_to_string err));
  assert !removed;
  let remove_url = Option.get !(List_ops_http.last_delete_url) in
  assert (String.contains remove_url '?');
  let query_ids = Uri.of_string remove_url |> Uri.query |> List.assoc_opt "account_ids[]" |> Option.value ~default:[] in
  assert (List.mem "a1" query_ids && List.mem "a2" query_ids);
  Printf.printf "✓\n"

let test_list_ops_error_mapping_contract () =
  Printf.printf "Test: list ops error mapping contract... ";
  List_ops_http.reset ();

  List_ops_http.set_next_post_response {
    Social_core.status = 401;
    headers = [];
    body = {|{"error":"The access token is invalid"}|};
  };
  let got_create_error = ref false in
  Mastodon.create_list
    ~account_id:"acct"
    ~title:"Friends"
    (function
      | Error (Error_types.Auth_error Error_types.Token_invalid) -> got_create_error := true
      | Error err -> failwith (Printf.sprintf "Unexpected create_list error: %s" (Error_types.error_to_string err))
      | Ok _ -> failwith "Expected create_list auth error");
  assert !got_create_error;

  List_ops_http.set_next_put_response {
    Social_core.status = 422;
    headers = [];
    body = {|{"error":"Validation failed"}|};
  };
  let got_update_error = ref false in
  Mastodon.update_list
    ~account_id:"acct"
    ~list_id:"l1"
    ~title:(Some "")
    (function
      | Error (Error_types.Validation_error errs) ->
          got_update_error := true;
          assert (errs <> [])
      | Error (Error_types.Api_error { status_code = 422; _ }) ->
          got_update_error := true
      | Error err -> failwith (Printf.sprintf "Unexpected update_list error: %s" (Error_types.error_to_string err))
      | Ok _ -> failwith "Expected update_list validation error");
  assert !got_update_error;

  List_ops_http.set_next_delete_response {
    Social_core.status = 404;
    headers = [];
    body = {|{"error":"Record not found"}|};
  };
  let got_delete_error = ref false in
  Mastodon.delete_list
    ~account_id:"acct"
    ~list_id:"missing"
    (function
      | Error (Error_types.Api_error { status_code = 404; _ }) -> got_delete_error := true
      | Error err -> failwith (Printf.sprintf "Unexpected delete_list error: %s" (Error_types.error_to_string err))
      | Ok () -> failwith "Expected delete_list not-found error");
  assert !got_delete_error;

  List_ops_http.set_next_post_response {
    Social_core.status = 429;
    headers = [ ("Retry-After", "12") ];
    body = {|{"error":"rate limit exceeded"}|};
  };
  let got_add_rate_limited = ref false in
  Mastodon.add_accounts_to_list
    ~account_id:"acct"
    ~list_id:"l1"
    ~target_account_ids:["a1"]
    (function
      | Error (Error_types.Rate_limited { retry_after_seconds = Some 12; _ }) ->
          got_add_rate_limited := true
      | Error err -> failwith (Printf.sprintf "Unexpected add_accounts rate-limit error: %s" (Error_types.error_to_string err))
      | Ok () -> failwith "Expected add_accounts_to_list rate-limit error");
  assert !got_add_rate_limited;

  List_ops_http.set_next_delete_response {
    Social_core.status = 403;
    headers = [];
    body = {|{"error":"Forbidden"}|};
  };
  let got_remove_forbidden = ref false in
  Mastodon.remove_accounts_from_list
    ~account_id:"acct"
    ~list_id:"l1"
    ~target_account_ids:["a1"]
    (function
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions _)) ->
          got_remove_forbidden := true
      | Error err -> failwith (Printf.sprintf "Unexpected remove_accounts forbidden error: %s" (Error_types.error_to_string err))
      | Ok () -> failwith "Expected remove_accounts_from_list forbidden error");
  assert !got_remove_forbidden;
  Printf.printf "✓\n"

let () =
  Printf.printf "\n=== Mastodon List Ops Contract Tests ===\n\n";
  test_create_update_delete_list_contract ();
  test_list_accounts_membership_contract ();
  test_list_ops_error_mapping_contract ();
  Printf.printf "\n✓ List ops contract tests passed\n"
