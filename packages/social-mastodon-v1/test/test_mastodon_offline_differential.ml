(** Offline differential fixture tests for response compatibility variants. *)

module Diff_http = struct
  let string_contains s sub =
    try
      let _ = Str.search_forward (Str.regexp_string sub) s 0 in
      true
    with Not_found -> false

  let status_resp = ref { Social_core.status = 200; headers = []; body = "{}" }
  let notifications_resp = ref { Social_core.status = 200; headers = []; body = "[]" }
  let trends_tags_resp = ref { Social_core.status = 200; headers = []; body = "[]" }
  let search_resp = ref { Social_core.status = 200; headers = []; body = {|{"statuses":[]}|} }
  let search_all_resp = ref { Social_core.status = 200; headers = []; body = {|{"statuses":[],"accounts":[],"hashtags":[]}|} }
  let hashtag_search_resp = ref { Social_core.status = 200; headers = []; body = {|{"hashtags":[]}|} }
  let account_search_resp = ref { Social_core.status = 200; headers = []; body = "[]" }
  let account_lookup_resp = ref { Social_core.status = 200; headers = []; body = {|{"id":"a1","acct":"u@example.com"}|} }
  let account_featured_tags_resp = ref { Social_core.status = 200; headers = []; body = "[]" }
  let bookmarks_resp = ref { Social_core.status = 200; headers = []; body = "[]" }
  let favourites_resp = ref { Social_core.status = 200; headers = []; body = "[]" }
  let lists_resp = ref { Social_core.status = 200; headers = []; body = "[]" }
  let list_accounts_resp = ref { Social_core.status = 200; headers = []; body = "[]" }
  let account_lists_resp = ref { Social_core.status = 200; headers = []; body = "[]" }
  let list_timeline_resp = ref { Social_core.status = 200; headers = []; body = "[]" }

  let reset () =
    status_resp := { Social_core.status = 200; headers = []; body = "{}" };
    notifications_resp := { Social_core.status = 200; headers = []; body = "[]" };
    trends_tags_resp := { Social_core.status = 200; headers = []; body = "[]" };
    search_resp := { Social_core.status = 200; headers = []; body = {|{"statuses":[]}|} };
    search_all_resp := { Social_core.status = 200; headers = []; body = {|{"statuses":[],"accounts":[],"hashtags":[]}|} };
    hashtag_search_resp := { Social_core.status = 200; headers = []; body = {|{"hashtags":[]}|} };
    account_search_resp := { Social_core.status = 200; headers = []; body = "[]" };
    account_lookup_resp := { Social_core.status = 200; headers = []; body = {|{"id":"a1","acct":"u@example.com"}|} };
    account_featured_tags_resp := { Social_core.status = 200; headers = []; body = "[]" };
    bookmarks_resp := { Social_core.status = 200; headers = []; body = "[]" };
    favourites_resp := { Social_core.status = 200; headers = []; body = "[]" };
    lists_resp := { Social_core.status = 200; headers = []; body = "[]" };
    list_accounts_resp := { Social_core.status = 200; headers = []; body = "[]" };
    account_lists_resp := { Social_core.status = 200; headers = []; body = "[]" };
    list_timeline_resp := { Social_core.status = 200; headers = []; body = "[]" }

  let get ?headers:_ url on_success _on_error =
    if String.ends_with ~suffix:"/api/v1/accounts/verify_credentials" url then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body = {|{"id":"123","username":"tester"}|};
      }
    else if string_contains url "/api/v1/statuses/" then
      on_success !status_resp
    else if string_contains url "/api/v1/notifications" then
      on_success !notifications_resp
    else if string_contains url "/api/v1/trends/tags" then
      on_success !trends_tags_resp
    else if string_contains url "/api/v2/search" && string_contains url "type=hashtags" then
      on_success !hashtag_search_resp
    else if string_contains url "/api/v2/search" && not (string_contains url "type=") then
      on_success !search_all_resp
    else if string_contains url "/api/v1/accounts/search" then
      on_success !account_search_resp
    else if string_contains url "/api/v1/accounts/lookup" then
      on_success !account_lookup_resp
    else if string_contains url "/api/v1/accounts/" && string_contains url "/featured_tags" then
      on_success !account_featured_tags_resp
    else if string_contains url "/api/v1/bookmarks" then
      on_success !bookmarks_resp
    else if string_contains url "/api/v1/favourites" then
      on_success !favourites_resp
    else if string_contains url "/api/v1/timelines/list/" then
      on_success !list_timeline_resp
    else if string_contains url "/api/v1/lists/" && string_contains url "/accounts" then
      on_success !list_accounts_resp
    else if string_contains url "/api/v1/accounts/" && string_contains url "/lists" then
      on_success !account_lists_resp
    else if string_contains url "/api/v1/lists" then
      on_success !lists_resp
    else if string_contains url "/api/v2/search" then
      on_success !search_resp
    else
      on_success { Social_core.status = 404; headers = []; body = "not found" }

  let post ?headers:_ ?body:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }

  let post_multipart ?headers:_ ~parts:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }

  let put ?headers:_ ?body:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }

  let delete ?headers:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }
end

module Diff_config = struct
  module Http = Diff_http

  let get_env _key = None

  let get_credentials ~account_id:_ on_success _on_error =
    on_success {
      Social_core.access_token =
        {|{"access_token":"diff_token","instance_url":"https://mastodon.social"}|};
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

module Mastodon = Social_mastodon_v1.Make (Diff_config)

let test_status_payload_variants () =
  Printf.printf "Test: status payload compatibility variants... ";
  let variants = [
    ("full", {|{"id":"s1","content":"<p>Full</p>","url":"https://mastodon.social/@u/s1","created_at":"2024-01-01T00:00:00Z","account":{"acct":"u"}}|});
    ("missing_optional", {|{"id":"s2","content":"<p>Minimal</p>","account":{"acct":"u"}}|});
    ("null_optional", {|{"id":"s3","content":"<p>Nulls</p>","url":null,"created_at":null,"account":{}}|});
  ] in
  List.iter
    (fun (_name, body) ->
      Diff_http.reset ();
      Diff_http.status_resp := { Social_core.status = 200; headers = []; body };
      let got = ref false in
      Mastodon.get_status
        ~account_id:"acct"
        ~status_id:"x"
        (function
          | Ok s ->
              got := true;
              assert (String.length s.id > 0)
          | Error err ->
              failwith (Printf.sprintf "Unexpected status parse error: %s" (Error_types.error_to_string err)));
      assert !got)
    variants;
  Printf.printf "✓\n"

let test_notifications_payload_variants () =
  Printf.printf "Test: notifications payload compatibility variants... ";
  let variants = [
    {|[{"id":"n1","type":"mention","account":{"id":"a1","acct":"u"},"status":{"id":"s1","content":"<p>hi</p>"}}]|};
    {|[{"id":"n2","type":"admin.sign_up","account":null,"status":null}]|};
  ] in
  List.iter
    (fun body ->
      Diff_http.reset ();
      Diff_http.notifications_resp := { Social_core.status = 200; headers = []; body };
      let got = ref false in
      Mastodon.get_notifications
        ~account_id:"acct"
        (function
          | Ok items ->
              got := true;
              assert (List.length items = 1)
          | Error err ->
              failwith (Printf.sprintf "Unexpected notifications parse error: %s" (Error_types.error_to_string err)));
      assert !got)
    variants;
  Printf.printf "✓\n"

let test_trending_tags_uses_variants () =
  Printf.printf "Test: trending tags uses parsing variants... ";
  let variants = [
    ({|[{"name":"ocaml","history":[{"uses":"3"},{"uses":"4"}]}]|}, Some 7);
    ({|[{"name":"ocaml","history":[{"uses":2},{"uses":3}]}]|}, Some 5);
    ({|[{"name":"ocaml"}]|}, None);
  ] in
  List.iter
    (fun (body, expected_uses) ->
      Diff_http.reset ();
      Diff_http.trends_tags_resp := { Social_core.status = 200; headers = []; body };
      let got = ref false in
      Mastodon.get_trending_tags
        ~account_id:"acct"
        (function
          | Ok tags ->
              got := true;
              assert (List.length tags = 1);
              assert ((List.hd tags).usage_count = expected_uses)
          | Error err ->
              failwith (Printf.sprintf "Unexpected trends parse error: %s" (Error_types.error_to_string err)));
      assert !got)
    variants;
  Printf.printf "✓\n"

let test_search_statuses_empty_and_item () =
  Printf.printf "Test: status search empty/item variants... ";
  let variants = [
    ({|{"statuses":[]}|}, 0);
    ({|{"statuses":[{"id":"s9","content":"<p>hit</p>"}]}|}, 1);
  ] in
  List.iter
    (fun (body, expected_count) ->
      Diff_http.reset ();
      Diff_http.search_resp := { Social_core.status = 200; headers = []; body };
      let got = ref false in
      Mastodon.search_statuses
        ~account_id:"acct"
        ~query:"q"
        (function
          | Ok statuses ->
              got := true;
              assert (List.length statuses = expected_count)
          | Error err ->
              failwith (Printf.sprintf "Unexpected search parse error: %s" (Error_types.error_to_string err)));
      assert !got)
    variants;
  Printf.printf "✓\n"

let test_search_statuses_missing_statuses_key () =
  Printf.printf "Test: status search missing statuses key variant... ";
  Diff_http.reset ();
  Diff_http.search_resp := { Social_core.status = 200; headers = []; body = {|{"accounts":[],"hashtags":[]}|} };
  let got = ref false in
  Mastodon.search_statuses
    ~account_id:"acct"
    ~query:"q"
    (function
      | Ok statuses ->
          got := true;
          assert (statuses = [])
      | Error err ->
          failwith (Printf.sprintf "Unexpected missing-key search error: %s" (Error_types.error_to_string err)));
  assert !got;
  Printf.printf "✓\n"

let test_search_all_payload_variants () =
  Printf.printf "Test: search-all payload variants... ";
  let variants = [
    ({|{"statuses":[{"id":"s1","content":"<p>x</p>"}],"accounts":[{"id":"a1","acct":"u@example.com"}],"hashtags":[{"name":"ocaml","history":[{"uses":"1"}]}]}|}, (1, 1, 1));
    ({|{"statuses":null,"accounts":null,"hashtags":null}|}, (0, 0, 0));
    ({|{"statuses":[],"accounts":[]}|}, (0, 0, 0));
  ] in
  List.iter
    (fun (body, (status_count, account_count, hashtag_count)) ->
      Diff_http.reset ();
      Diff_http.search_all_resp := { Social_core.status = 200; headers = []; body };
      let got = ref false in
      Mastodon.search_all
        ~account_id:"acct"
        ~query:"q"
        (function
          | Ok payload ->
              got := true;
              assert (List.length payload.statuses = status_count);
              assert (List.length payload.accounts = account_count);
              assert (List.length payload.hashtags = hashtag_count)
          | Error err ->
              failwith (Printf.sprintf "Unexpected search-all parse error: %s" (Error_types.error_to_string err)));
      assert !got)
    variants;
  Printf.printf "✓\n"

let test_account_search_payload_variants () =
  Printf.printf "Test: account search payload variants... ";
  let variants = [
    ({|[{"id":"a1","acct":"user@example.com"}]|}, 1);
    ({|null|}, 0);
    ({|{"accounts":[]}|}, 0);
  ] in
  List.iter
    (fun (body, expected_count) ->
      Diff_http.reset ();
      Diff_http.account_search_resp := { Social_core.status = 200; headers = []; body };
      let got = ref false in
      Mastodon.search_accounts
        ~account_id:"acct"
        ~query:"u"
        (function
          | Ok accounts ->
              got := true;
              assert (List.length accounts = expected_count)
          | Error err ->
              failwith (Printf.sprintf "Unexpected account search parse error: %s" (Error_types.error_to_string err)));
      assert !got)
    variants;
  Printf.printf "✓\n"

let test_hashtag_search_payload_variants () =
  Printf.printf "Test: hashtag search payload variants... ";
  let variants = [
    ({|{"hashtags":[{"name":"ocaml","history":[{"uses":"3"}]}]}|}, 1);
    ({|{"hashtags":null}|}, 0);
    ({|{"statuses":[]}|}, 0);
  ] in
  List.iter
    (fun (body, expected_count) ->
      Diff_http.reset ();
      Diff_http.hashtag_search_resp := { Social_core.status = 200; headers = []; body };
      let got = ref false in
      Mastodon.search_hashtags
        ~account_id:"acct"
        ~query:"ocaml"
        (function
          | Ok tags ->
              got := true;
              assert (List.length tags = expected_count)
          | Error err ->
              failwith (Printf.sprintf "Unexpected hashtag search parse error: %s" (Error_types.error_to_string err)));
      assert !got)
    variants;
  Printf.printf "✓\n"

let test_account_lookup_payload_variants () =
  Printf.printf "Test: account lookup payload variants... ";
  let variants = [
    ({|{"id":"a1","acct":"u@example.com"}|}, "a1");
    ({|{"id":null,"acct":"u@example.com","username":null,"display_name":null}|}, "");
  ] in
  List.iter
    (fun (body, expected_id) ->
      Diff_http.reset ();
      Diff_http.account_lookup_resp := { Social_core.status = 200; headers = []; body };
      let got = ref false in
      Mastodon.lookup_account_by_acct
        ~account_id:"acct"
        ~acct:"u@example.com"
        (function
          | Ok account ->
              got := true;
              assert (String.equal account.id expected_id)
          | Error err ->
              failwith (Printf.sprintf "Unexpected account lookup parse error: %s" (Error_types.error_to_string err)));
      assert !got)
    variants;
  Printf.printf "✓\n"

let test_account_featured_tags_payload_variants () =
  Printf.printf "Test: account featured tags payload variants... ";
  let variants = [
    ({|[{"name":"featured","history":[{"uses":"4"}]}]|}, 1);
    ({|null|}, 0);
    ({|{"tags":[]}|}, 0);
  ] in
  List.iter
    (fun (body, expected_count) ->
      Diff_http.reset ();
      Diff_http.account_featured_tags_resp := { Social_core.status = 200; headers = []; body };
      let got = ref false in
      Mastodon.get_account_featured_tags
        ~account_id:"acct"
        ~target_account_id:"22"
        (function
          | Ok tags ->
              got := true;
              assert (List.length tags = expected_count)
          | Error err ->
              failwith (Printf.sprintf "Unexpected featured-tags parse error: %s" (Error_types.error_to_string err)));
      assert !got)
    variants;
  Printf.printf "✓\n"

let test_bookmarks_favourites_payload_variants () =
  Printf.printf "Test: bookmarks/favourites payload variants... ";
  Diff_http.reset ();
  Diff_http.bookmarks_resp := { Social_core.status = 200; headers = []; body = {|null|} };
  Diff_http.favourites_resp := { Social_core.status = 200; headers = []; body = {|{"items":[]}|} };
  let got_bookmarks = ref false in
  Mastodon.get_bookmarks
    ~account_id:"acct"
    (function
      | Ok statuses ->
          got_bookmarks := true;
          assert (statuses = [])
      | Error err ->
          failwith (Printf.sprintf "Unexpected bookmarks parse error: %s" (Error_types.error_to_string err)));
  assert !got_bookmarks;

  let got_favourites = ref false in
  Mastodon.get_favourites
    ~account_id:"acct"
    (function
      | Ok statuses ->
          got_favourites := true;
          assert (statuses = [])
      | Error err ->
          failwith (Printf.sprintf "Unexpected favourites parse error: %s" (Error_types.error_to_string err)));
  assert !got_favourites;
  Printf.printf "✓\n"

let test_lists_payload_variants () =
  Printf.printf "Test: lists payload compatibility variants... ";
  let variants = [
    {|[{"id":"l1","title":"Friends"}]|};
    {|[{"id":"l2","title":"Work","unknown":123}]|};
    {|[{"id":"l3"}]|};
  ] in
  List.iter
    (fun body ->
      Diff_http.reset ();
      Diff_http.lists_resp := { Social_core.status = 200; headers = []; body };
      let got = ref false in
      Mastodon.get_lists
        ~account_id:"acct"
        (function
          | Ok lists ->
              got := true;
              assert (List.length lists = 1);
              assert (String.length (List.hd lists).id > 0)
          | Error err ->
              failwith (Printf.sprintf "Unexpected lists parse error: %s" (Error_types.error_to_string err)));
      assert !got)
    variants;
  Printf.printf "✓\n"

let test_list_timeline_payload_variants () =
  Printf.printf "Test: list timeline payload compatibility variants... ";
  let variants = [
    {|[{"id":"s1","content":"<p>List</p>","account":{"acct":"u"}}]|};
    {|[{"id":"s2","content":"<p>List nulls</p>","url":null,"created_at":null}]|};
  ] in
  List.iter
    (fun body ->
      Diff_http.reset ();
      Diff_http.list_timeline_resp := { Social_core.status = 200; headers = []; body };
      let got = ref false in
      Mastodon.get_list_timeline
        ~account_id:"acct"
        ~list_id:"l1"
        (function
          | Ok statuses ->
              got := true;
              assert (List.length statuses = 1)
          | Error err ->
              failwith (Printf.sprintf "Unexpected list timeline parse error: %s" (Error_types.error_to_string err)));
      assert !got)
    variants;
  Printf.printf "✓\n"

let test_list_accounts_payload_variants () =
  Printf.printf "Test: list accounts payload variants... ";
  let variants = [
    ({|[{"id":"a1","acct":"u@example.com"}]|}, 1);
    ({|null|}, 0);
    ({|{"items":[]}|}, 0);
  ] in
  List.iter
    (fun (body, expected_count) ->
      Diff_http.reset ();
      Diff_http.list_accounts_resp := { Social_core.status = 200; headers = []; body };
      let got = ref false in
      Mastodon.get_list_accounts
        ~account_id:"acct"
        ~list_id:"l1"
        (function
          | Ok accounts ->
              got := true;
              assert (List.length accounts = expected_count)
          | Error err ->
              failwith (Printf.sprintf "Unexpected list accounts parse error: %s" (Error_types.error_to_string err)));
      assert !got)
    variants;
  Printf.printf "✓\n"

let test_account_lists_payload_variants () =
  Printf.printf "Test: account lists payload variants... ";
  let variants = [
    ({|[{"id":"l1","title":"Friends"}]|}, 1);
    ({|null|}, 0);
    ({|{"items":[]}|}, 0);
  ] in
  List.iter
    (fun (body, expected_count) ->
      Diff_http.reset ();
      Diff_http.account_lists_resp := { Social_core.status = 200; headers = []; body };
      let got = ref false in
      Mastodon.get_account_lists
        ~account_id:"acct"
        ~target_account_id:"22"
        (function
          | Ok lists ->
              got := true;
              assert (List.length lists = expected_count)
          | Error err ->
              failwith (Printf.sprintf "Unexpected account lists parse error: %s" (Error_types.error_to_string err)));
      assert !got)
    variants;
  Printf.printf "✓\n"

let test_error_payload_redaction_variant () =
  Printf.printf "Test: error payload redaction variant... ";
  Diff_http.reset ();
  Diff_http.status_resp := {
    Social_core.status = 500;
    headers = [];
    body = {|{"error":"boom","access_token":"super-secret"}|};
  };
  let got = ref false in
  Mastodon.get_status
    ~account_id:"acct"
    ~status_id:"x"
    (function
      | Error (Error_types.Api_error { raw_response = Some raw; _ }) ->
          got := true;
          assert (not (Diff_http.string_contains raw "super-secret"));
          assert (Diff_http.string_contains raw "[REDACTED]")
      | Error err ->
          failwith (Printf.sprintf "Unexpected error mapping: %s" (Error_types.error_to_string err))
      | Ok _ -> failwith "Expected error result");
  assert !got;
  Printf.printf "✓\n"

let () =
  Printf.printf "\n=== Mastodon Offline Differential Tests ===\n\n";
  test_status_payload_variants ();
  test_notifications_payload_variants ();
  test_trending_tags_uses_variants ();
  test_search_statuses_empty_and_item ();
  test_search_statuses_missing_statuses_key ();
  test_search_all_payload_variants ();
  test_account_search_payload_variants ();
  test_hashtag_search_payload_variants ();
  test_account_lookup_payload_variants ();
  test_account_featured_tags_payload_variants ();
  test_bookmarks_favourites_payload_variants ();
  test_lists_payload_variants ();
  test_list_timeline_payload_variants ();
  test_list_accounts_payload_variants ();
  test_account_lists_payload_variants ();
  test_error_payload_redaction_variant ();
  Printf.printf "\n✓ Offline differential tests passed\n"
