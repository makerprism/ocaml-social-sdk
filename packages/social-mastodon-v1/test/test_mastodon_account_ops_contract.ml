(** Focused account operations contract tests for Mastodon provider. *)

module Account_ops_http = struct
  let string_contains s sub =
    try
      let _ = Str.search_forward (Str.regexp_string sub) s 0 in
      true
    with Not_found -> false

  let last_get_url : string option ref = ref None
  let last_post_url : string option ref = ref None
  let last_post_body : string option ref = ref None
  let next_post_status : int option ref = ref None

  let reset () =
    last_get_url := None;
    last_post_url := None;
    last_post_body := None;
    next_post_status := None

  let get ?headers:_ url on_success _on_error =
    last_get_url := Some url;
    if String.ends_with ~suffix:"/api/v1/accounts/verify_credentials" url then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body = {|{"id":"123","username":"tester"}|};
      }
    else if String.contains url '/' && string_contains url "follow_requests" then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body = {|[{"id":"55","acct":"requester@example.com","username":"requester","display_name":"Requester","url":"https://mastodon.social/@requester"}]|};
      }
    else
      on_success { Social_core.status = 404; headers = []; body = "not found" }

  let post ?headers:_ ?body url on_success _on_error =
    last_post_url := Some url;
    last_post_body := body;
    let status = match !next_post_status with Some s -> s | None -> 200 in
    next_post_status := None;
    if String.contains url '/' then
      on_success {
        Social_core.status = status;
        headers = [ ("content-type", "application/json") ];
        body = {|{"id":"22","following":true,"followed_by":false,"blocking":false,"muting":false}|};
      }
    else
      on_success { Social_core.status = 404; headers = []; body = "not found" }

  let post_multipart ?headers:_ ~parts:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }

  let put ?headers:_ ?body:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }

  let delete ?headers:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }
end

module Account_ops_config = struct
  module Http = Account_ops_http

  let get_env _key = None

  let get_credentials ~account_id:_ on_success _on_error =
    on_success {
      Social_core.access_token =
        {|{"access_token":"ops_token","instance_url":"https://mastodon.social"}|};
      refresh_token = None;
      expires_at = None;
      auth_type = Social_core.Bearer;
    }

  let update_credentials ~account_id:_ ~credentials:_ on_success _on_error = on_success ()
  let encrypt _data on_success _on_error = on_success "encrypted"
  let decrypt _data on_success _on_error = on_success "decrypted"
  let sleep ~seconds:_ on_success _on_error = on_success ()
  let update_health_status ~account_id:_ ~status:_ ~error_message:_ on_success _on_error = on_success ()
end

module Mastodon = Social_mastodon_v1.Make (Account_ops_config)

let test_follow_account_contract () =
  Printf.printf "Test: follow account request contract... ";
  Account_ops_http.reset ();
  let got_result = ref false in
  Mastodon.follow_account
    ~account_id:"acct"
    ~target_account_id:"22"
    ~reblogs:false
    ~notify:true
    ~languages:["en"; "fr"]
    (fun result ->
      got_result := true;
      match result with
      | Ok relationship ->
          assert (String.equal relationship.id "22");
          assert relationship.following
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  let url = Option.get !(Account_ops_http.last_post_url) in
  let body = Option.get !(Account_ops_http.last_post_body) in
  assert (String.ends_with ~suffix:"/api/v1/accounts/22/follow" url);
  let json = Yojson.Basic.from_string body in
  let open Yojson.Basic.Util in
  assert (json |> member "reblogs" |> to_bool = false);
  assert (json |> member "notify" |> to_bool = true);
  assert (json |> member "languages" |> to_list |> List.length = 2);
  Printf.printf "✓\n"

let test_unfollow_account_contract () =
  Printf.printf "Test: unfollow account request contract... ";
  Account_ops_http.reset ();
  let got_result = ref false in
  Mastodon.unfollow_account
    ~account_id:"acct"
    ~target_account_id:"22"
    (fun result ->
      got_result := true;
      match result with
      | Ok relationship ->
          assert (String.equal relationship.id "22")
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  let url = Option.get !(Account_ops_http.last_post_url) in
  assert (String.ends_with ~suffix:"/api/v1/accounts/22/unfollow" url);
  Printf.printf "✓\n"

let test_block_unblock_contract () =
  Printf.printf "Test: block/unblock account request contract... ";
  Account_ops_http.reset ();
  let got_block = ref false in
  Mastodon.block_account
    ~account_id:"acct"
    ~target_account_id:"22"
    (fun result ->
      got_block := true;
      match result with
      | Ok relationship -> assert (String.equal relationship.id "22")
      | Error err -> failwith (Printf.sprintf "Unexpected block error: %s" (Error_types.error_to_string err)));
  assert !got_block;
  let block_url = Option.get !(Account_ops_http.last_post_url) in
  assert (String.ends_with ~suffix:"/api/v1/accounts/22/block" block_url);

  Account_ops_http.reset ();
  let got_unblock = ref false in
  Mastodon.unblock_account
    ~account_id:"acct"
    ~target_account_id:"22"
    (fun result ->
      got_unblock := true;
      match result with
      | Ok relationship -> assert (String.equal relationship.id "22")
      | Error err -> failwith (Printf.sprintf "Unexpected unblock error: %s" (Error_types.error_to_string err)));
  assert !got_unblock;
  let unblock_url = Option.get !(Account_ops_http.last_post_url) in
  assert (String.ends_with ~suffix:"/api/v1/accounts/22/unblock" unblock_url);
  Printf.printf "✓\n"

let test_mute_unmute_contract () =
  Printf.printf "Test: mute/unmute account request contract... ";
  Account_ops_http.reset ();
  let got_mute = ref false in
  Mastodon.mute_account
    ~account_id:"acct"
    ~target_account_id:"22"
    ~notifications:false
    ~duration:(Some 7)
    (fun result ->
      got_mute := true;
      match result with
      | Ok relationship -> assert (String.equal relationship.id "22")
      | Error err -> failwith (Printf.sprintf "Unexpected mute error: %s" (Error_types.error_to_string err)));
  assert !got_mute;
  let mute_url = Option.get !(Account_ops_http.last_post_url) in
  let mute_body = Option.get !(Account_ops_http.last_post_body) in
  assert (String.ends_with ~suffix:"/api/v1/accounts/22/mute" mute_url);
  let mute_json = Yojson.Basic.from_string mute_body in
  let open Yojson.Basic.Util in
  assert (mute_json |> member "notifications" |> to_bool = false);
  assert (mute_json |> member "duration" |> to_int = 7);

  Account_ops_http.reset ();
  let got_unmute = ref false in
  Mastodon.unmute_account
    ~account_id:"acct"
    ~target_account_id:"22"
    (fun result ->
      got_unmute := true;
      match result with
      | Ok relationship -> assert (String.equal relationship.id "22")
      | Error err -> failwith (Printf.sprintf "Unexpected unmute error: %s" (Error_types.error_to_string err)));
  assert !got_unmute;
  let unmute_url = Option.get !(Account_ops_http.last_post_url) in
  assert (String.ends_with ~suffix:"/api/v1/accounts/22/unmute" unmute_url);
  Printf.printf "✓\n"

let test_notifications_dismiss_and_clear_contract () =
  Printf.printf "Test: notifications dismiss/clear contract... ";
  Account_ops_http.reset ();
  let got_dismiss = ref false in
  Mastodon.dismiss_notification
    ~account_id:"acct"
    ~notification_id:"301"
    (fun result ->
      got_dismiss := true;
      match result with
      | Ok () -> ()
      | Error err -> failwith (Printf.sprintf "Unexpected dismiss error: %s" (Error_types.error_to_string err)));
  assert !got_dismiss;
  let dismiss_url = Option.get !(Account_ops_http.last_post_url) in
  assert (String.ends_with ~suffix:"/api/v1/notifications/301/dismiss" dismiss_url);

  Account_ops_http.reset ();
  let got_clear = ref false in
  Mastodon.clear_notifications
    ~account_id:"acct"
    (fun result ->
      got_clear := true;
      match result with
      | Ok () -> ()
      | Error err -> failwith (Printf.sprintf "Unexpected clear error: %s" (Error_types.error_to_string err)));
  assert !got_clear;
  let clear_url = Option.get !(Account_ops_http.last_post_url) in
  assert (String.ends_with ~suffix:"/api/v1/notifications/clear" clear_url);
  Printf.printf "✓\n"

let test_follow_requests_contract () =
  Printf.printf "Test: follow requests list/authorize/reject contract... ";
  Account_ops_http.reset ();
  let got_list = ref false in
  Mastodon.get_follow_requests
    ~account_id:"acct"
    ~limit:15
    ~max_id:(Some "500")
    ~since_id:(Some "450")
    (fun result ->
      got_list := true;
      match result with
      | Ok accounts ->
          assert (List.length accounts = 1);
          assert (String.equal (List.hd accounts).id "55")
      | Error err ->
          failwith (Printf.sprintf "Unexpected follow-requests error: %s" (Error_types.error_to_string err)));
  assert !got_list;
  let list_url = Option.get !(Account_ops_http.last_get_url) in
  assert (Account_ops_http.string_contains list_url "/api/v1/follow_requests");
  let limit_values =
    Uri.of_string list_url
    |> Uri.query
    |> List.assoc_opt "limit"
    |> Option.value ~default:[]
  in
  assert (List.mem "15" limit_values);
  let max_values =
    Uri.of_string list_url
    |> Uri.query
    |> List.assoc_opt "max_id"
    |> Option.value ~default:[]
  in
  assert (List.mem "500" max_values);
  let since_values =
    Uri.of_string list_url
    |> Uri.query
    |> List.assoc_opt "since_id"
    |> Option.value ~default:[]
  in
  assert (List.mem "450" since_values);

  let got_authorize = ref false in
  Mastodon.authorize_follow_request
    ~account_id:"acct"
    ~target_account_id:"55"
    (fun result ->
      got_authorize := true;
      match result with
      | Ok () -> ()
      | Error err -> failwith (Printf.sprintf "Unexpected authorize error: %s" (Error_types.error_to_string err)));
  assert !got_authorize;
  let authorize_url = Option.get !(Account_ops_http.last_post_url) in
  assert (String.ends_with ~suffix:"/api/v1/follow_requests/55/authorize" authorize_url);

  Account_ops_http.reset ();
  let got_reject = ref false in
  Mastodon.reject_follow_request
    ~account_id:"acct"
    ~target_account_id:"55"
    (fun result ->
      got_reject := true;
      match result with
      | Ok () -> ()
      | Error err -> failwith (Printf.sprintf "Unexpected reject error: %s" (Error_types.error_to_string err)));
  assert !got_reject;
  let reject_url = Option.get !(Account_ops_http.last_post_url) in
  assert (String.ends_with ~suffix:"/api/v1/follow_requests/55/reject" reject_url);
  Printf.printf "✓\n"

let () =
  Printf.printf "\n=== Mastodon Account Ops Contract Tests ===\n\n";
  test_follow_account_contract ();
  test_unfollow_account_contract ();
  test_block_unblock_contract ();
  test_mute_unmute_contract ();
  test_notifications_dismiss_and_clear_contract ();
  test_follow_requests_contract ();
  Printf.printf "\n✓ Account ops contract tests passed\n"
