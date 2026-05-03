(** Focused filters API contract tests for Mastodon provider. *)

module Filters_http = struct
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

  let set_next_post_response resp = next_post_response := Some resp
  let set_next_put_response resp = next_put_response := Some resp
  let set_next_delete_response resp = next_delete_response := Some resp

  let get ?headers:_ url on_success _on_error =
    last_get_url := Some url;
    if String.ends_with ~suffix:"/api/v1/accounts/verify_credentials" url then
      on_success { Social_core.status = 200; headers = []; body = {|{"id":"123"}|} }
    else if String.ends_with ~suffix:"/api/v2/filters" url then
      on_success {
        Social_core.status = 200;
        headers = [];
        body =
          {|[{"id":"f1","title":"Spam links","context":["home","notifications"],"filter_action":"warn","expires_at":null}]|};
      }
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
        on_success {
          Social_core.status = 200;
          headers = [];
          body =
            {|{"id":"f2","title":"Muted words","context":["home"],"filter_action":"hide","expires_at":"2026-02-14T00:00:00Z"}|};
        }

  let post_multipart ?headers:_ ~parts:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }

  let put ?headers:_ ?body url on_success _on_error =
    last_put_url := Some url;
    last_put_body := body;
    match !next_put_response with
    | Some resp ->
        next_put_response := None;
        on_success resp
    | None ->
        on_success {
          Social_core.status = 200;
          headers = [];
          body =
            {|{"id":"f2","title":"Muted words updated","context":["home","public"],"filter_action":"warn","expires_at":null}|};
        }

  let delete ?headers:_ url on_success _on_error =
    last_delete_url := Some url;
    match !next_delete_response with
    | Some resp ->
        next_delete_response := None;
        on_success resp
    | None ->
        on_success { Social_core.status = 200; headers = []; body = "{}" }
end

module Filters_config = struct
  module Http = Filters_http

  let get_env _key = None
  let get_credentials ~account_id:_ on_success _on_error =
    on_success {
      Social_core.access_token = {|{"access_token":"filters_token","instance_url":"https://mastodon.social"}|};
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

module Mastodon = Social_mastodon_v1.Make (Filters_config)

let test_get_filters_contract () =
  Printf.printf "Test: get filters contract... ";
  Filters_http.reset ();
  let got_result = ref false in
  Mastodon.get_filters ~account_id:"acct"
    (function
      | Ok filters ->
          got_result := true;
          assert (List.length filters = 1);
          let f = List.hd filters in
          assert (String.equal f.id "f1");
          assert (String.equal f.title "Spam links");
          assert (List.mem "home" f.contexts);
          assert (f.filter_action = Some "warn")
      | Error err -> failwith (Error_types.error_to_string err));
  assert !got_result;
  let url = Option.get !(Filters_http.last_get_url) in
  assert (String.ends_with ~suffix:"/api/v2/filters" url);
  Printf.printf "✓\n"

let test_create_update_delete_filter_contract () =
  Printf.printf "Test: create/update/delete filter contract... ";
  Filters_http.reset ();

  let created = ref false in
  Mastodon.create_filter
    ~account_id:"acct"
    ~title:"Muted words"
    ~contexts:["home"]
    ~filter_action:(Some "hide")
    ~expires_in:(Some 3600)
    ~keywords:[{ keyword = "spoiler"; whole_word = Some true }]
    ~statuses:["109999"]
    (function
      | Ok f ->
          created := true;
          assert (String.equal f.id "f2")
      | Error err -> failwith (Error_types.error_to_string err));
  assert !created;
  let create_url = Option.get !(Filters_http.last_post_url) in
  assert (String.ends_with ~suffix:"/api/v2/filters" create_url);
  let create_body = Option.get !(Filters_http.last_post_body) in
  let create_json = Yojson.Basic.from_string create_body in
  let open Yojson.Basic.Util in
  assert (create_json |> member "title" |> to_string = "Muted words");
  assert (create_json |> member "context" |> to_list |> List.map to_string = ["home"]);
  assert (create_json |> member "filter_action" |> to_string = "hide");
  assert (create_json |> member "expires_in" |> to_int = 3600);
  assert (create_json |> member "keywords_attributes" |> to_list |> List.length = 1);
  assert (create_json |> member "statuses_attributes" |> to_list |> List.length = 1);

  let updated = ref false in
  Mastodon.update_filter
    ~account_id:"acct"
    ~filter_id:"f2"
    ~title:(Some "Muted words updated")
    ~contexts:(Some ["home"; "public"])
    ~filter_action:(Some "warn")
    ~keywords:(Some [{ keyword = "leak"; whole_word = None }])
    ~statuses:(Some ["110001"; "110002"])
    (function
      | Ok f ->
          updated := true;
          assert (String.equal f.title "Muted words updated")
      | Error err -> failwith (Error_types.error_to_string err));
  assert !updated;
  let update_url = Option.get !(Filters_http.last_put_url) in
  assert (String.ends_with ~suffix:"/api/v2/filters/f2" update_url);
  let update_body = Option.get !(Filters_http.last_put_body) in
  let update_json = Yojson.Basic.from_string update_body in
  assert (update_json |> member "title" |> to_string = "Muted words updated");
  assert (update_json |> member "context" |> to_list |> List.length = 2);
  assert (update_json |> member "keywords_attributes" |> to_list |> List.length = 1);
  assert (update_json |> member "statuses_attributes" |> to_list |> List.length = 2);

  let deleted = ref false in
  Mastodon.delete_filter
    ~account_id:"acct"
    ~filter_id:"f2"
    (function
      | Ok () -> deleted := true
      | Error err -> failwith (Error_types.error_to_string err));
  assert !deleted;
  let delete_url = Option.get !(Filters_http.last_delete_url) in
  assert (String.ends_with ~suffix:"/api/v2/filters/f2" delete_url);
  Printf.printf "✓\n"

let test_filter_ops_error_mapping_contract () =
  Printf.printf "Test: filter ops error mapping contract... ";
  Filters_http.reset ();

  Filters_http.set_next_post_response {
    Social_core.status = 403;
    headers = [];
    body = {|{"error":"Forbidden"}|};
  };
  let got_create_error = ref false in
  Mastodon.create_filter
    ~account_id:"acct"
    ~title:"x"
    ~contexts:["home"]
    (function
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions _)) ->
          got_create_error := true
      | Error err -> failwith (Printf.sprintf "Unexpected create_filter error: %s" (Error_types.error_to_string err))
      | Ok _ -> failwith "Expected create_filter forbidden error");
  assert !got_create_error;

  Filters_http.set_next_put_response {
    Social_core.status = 422;
    headers = [];
    body = {|{"error":"Validation failed"}|};
  };
  let got_update_error = ref false in
  Mastodon.update_filter
    ~account_id:"acct"
    ~filter_id:"f2"
    ~title:(Some "")
    (function
      | Error (Error_types.Validation_error errs) ->
          got_update_error := true;
          assert (errs <> [])
      | Error (Error_types.Api_error { status_code = 422; _ }) ->
          got_update_error := true
      | Error err -> failwith (Printf.sprintf "Unexpected update_filter error: %s" (Error_types.error_to_string err))
      | Ok _ -> failwith "Expected update_filter validation error");
  assert !got_update_error;

  Filters_http.set_next_delete_response {
    Social_core.status = 429;
    headers = [ ("Retry-After", "7") ];
    body = {|{"error":"rate limit exceeded"}|};
  };
  let got_delete_rate_limited = ref false in
  Mastodon.delete_filter
    ~account_id:"acct"
    ~filter_id:"f2"
    (function
      | Error (Error_types.Rate_limited { retry_after_seconds = Some 7; _ }) ->
          got_delete_rate_limited := true
      | Error err -> failwith (Printf.sprintf "Unexpected delete_filter error: %s" (Error_types.error_to_string err))
      | Ok () -> failwith "Expected delete_filter rate-limit error");
  assert !got_delete_rate_limited;
  Printf.printf "✓\n"

let () =
  Printf.printf "\n=== Mastodon Filters Contract Tests ===\n\n";
  test_get_filters_contract ();
  test_create_update_delete_filter_contract ();
  test_filter_ops_error_mapping_contract ();
  Printf.printf "\n✓ Filters contract tests passed\n"
