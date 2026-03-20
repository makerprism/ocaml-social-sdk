(** Focused read API contract tests for Mastodon provider. *)

module Read_http = struct
  let last_get_url : string option ref = ref None

  let string_contains s sub =
    try
      let _ = Str.search_forward (Str.regexp_string sub) s 0 in
      true
    with Not_found -> false

  let reset () =
    last_get_url := None

  let get ?headers:_ url on_success _on_error =
    last_get_url := Some url;
    if String.ends_with ~suffix:"/api/v1/accounts/verify_credentials" url then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body = {|{"id":"123","username":"tester"}|};
      }
    else if string_contains url "/api/v2/search" && string_contains url "type=hashtags" then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body =
          {|{"hashtags":[{"name":"ocaml","url":"https://mastodon.social/tags/ocaml","history":[{"uses":"2"},{"uses":"3"}]}]}|};
      }
    else if string_contains url "/api/v2/search" && not (string_contains url "type=") then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body =
          {|{"statuses":[{"id":"11","content":"<p>All search status</p>","account":{"acct":"tester"}}],"accounts":[{"id":"24","acct":"all@example.com"}],"hashtags":[{"name":"alltag","history":[{"uses":"5"}]}]}|};
      }
    else if string_contains url "/api/v2/search" then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body =
          {|{"statuses":[{"id":"9","content":"<p>Search hit</p>","url":"https://mastodon.social/@tester/9","created_at":"2024-01-01T00:00:00Z","account":{"acct":"tester"}}]}|};
      }
    else if string_contains url "/api/v1/accounts/lookup" then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body =
          {|{"id":"23","acct":"lookup@example.com","username":"lookup","display_name":"Lookup","url":"https://mastodon.social/@lookup"}|};
      }
    else if string_contains url "/api/v1/timelines/tag/" then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body =
          {|[{"id":"7","content":"<p>Tag hit</p>","url":"https://mastodon.social/@tester/7","created_at":"2024-01-01T00:00:00Z","account":{"acct":"tester"}}]|};
      }
    else if string_contains url "/api/v1/accounts/search" then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body =
          {|[{"id":"22","acct":"tester@example.com","username":"tester","display_name":"Tester","url":"https://mastodon.social/@tester"}]|};
      }
    else if string_contains url "/api/v1/accounts/relationships" then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body =
          {|[{"id":"22","following":true,"followed_by":false,"blocking":false,"muting":false}]|};
      }
    else if string_contains url "/api/v1/accounts/22/followers" then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body =
          {|[{"id":"33","acct":"follower@example.com","username":"follower","display_name":"Follower","url":"https://mastodon.social/@follower"}]|};
      }
    else if string_contains url "/api/v1/accounts/22/following" then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body =
          {|[{"id":"44","acct":"following@example.com","username":"following","display_name":"Following","url":"https://mastodon.social/@following"}]|};
      }
    else if string_contains url "/api/v1/accounts/22/featured_tags" then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body =
          {|[{"name":"featured","url":"https://mastodon.social/tags/featured","history":[{"uses":"3"}]}]|};
      }
    else if string_contains url "/api/v1/trends/tags" then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body =
          {|[{"name":"ocaml","url":"https://mastodon.social/tags/ocaml","history":[{"day":"1","uses":"3"},{"day":"2","uses":"4"}]}]|};
      }
    else if string_contains url "/api/v1/trends/statuses" then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body =
          {|[{"id":"77","content":"<p>Trending status</p>","url":"https://mastodon.social/@tester/77","created_at":"2024-01-01T00:00:00Z","account":{"acct":"tester"}}]|};
      }
    else if string_contains url "/api/v1/trends/links" then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body =
          {|[{"url":"https://example.com/post","title":"Example Post","description":"Example description"}]|};
      }
    else if string_contains url "/api/v1/bookmarks" then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body =
          {|[{"id":"88","content":"<p>Bookmarked status</p>","url":"https://mastodon.social/@tester/88","created_at":"2024-01-01T00:00:00Z","account":{"acct":"tester"}}]|};
      }
    else if string_contains url "/api/v1/favourites" then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body =
          {|[{"id":"89","content":"<p>Favourite status</p>","url":"https://mastodon.social/@tester/89","created_at":"2024-01-01T00:00:00Z","account":{"acct":"tester"}}]|};
      }
    else if string_contains url "/api/v1/lists/l1/accounts" then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body =
          {|[{"id":"55","acct":"listmember@example.com","username":"listmember","display_name":"List Member","url":"https://mastodon.social/@listmember"}]|};
      }
    else if string_contains url "/api/v1/lists" && not (string_contains url "/api/v1/timelines/list/") then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body = {|[{"id":"l1","title":"Friends"}]|};
      }
    else if string_contains url "/api/v1/timelines/list/" then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body =
          {|[{"id":"90","content":"<p>List status</p>","url":"https://mastodon.social/@tester/90","created_at":"2024-01-01T00:00:00Z","account":{"acct":"tester"}}]|};
      }
    else if string_contains url "/api/v1/accounts/22/lists" then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body = {|[{"id":"l2","title":"Mutuals"}]|};
      }
    else if string_contains url "/api/v1/accounts/22/statuses" then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body =
          {|[{"id":"44","content":"<p>Account status</p>","url":"https://mastodon.social/@tester/44","created_at":"2024-01-01T00:00:00Z","account":{"acct":"tester"}}]|};
      }
    else if string_contains url "/api/v1/accounts/22" then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body =
          {|{"id":"22","acct":"tester@example.com","username":"tester","display_name":"Tester","url":"https://mastodon.social/@tester"}|};
      }
    else if string_contains url "/api/v1/notifications" then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body =
          {|[{"id":"301","type":"mention","created_at":"2024-01-01T00:00:00Z","account":{"id":"22","acct":"tester@example.com","username":"tester"},"status":{"id":"99","content":"<p>@you hi</p>"}}]|};
      }
    else if String.contains url '?' then
      on_success {
        Social_core.status = 200;
        headers =
          if string_contains url "/api/v1/timelines/home" then
            [
              ("content-type", "application/json");
              ("link", "<https://mastodon.social/api/v1/timelines/home?max_id=90>; rel=\"next\", <https://mastodon.social/api/v1/timelines/home?min_id=110>; rel=\"prev\"");
            ]
          else
            [ ("content-type", "application/json") ];
        body =
          {|[{"id":"1","content":"<p>Hello</p>","url":"https://mastodon.social/@tester/1","created_at":"2024-01-01T00:00:00Z","account":{"acct":"tester"}}]|};
      }
    else
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body =
          {|{"id":"42","content":"<p>Status</p>","url":"https://mastodon.social/@tester/42","created_at":"2024-01-01T00:00:00Z","account":{"acct":"tester"}}|};
      }

  let post ?headers:_ ?body:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }

  let post_multipart ?headers:_ ~parts:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }

  let put ?headers:_ ?body:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }

  let delete ?headers:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }
end

module Read_config = struct
  module Http = Read_http

  let get_env _key = None

  let get_credentials ~account_id:_ on_success _on_error =
    on_success {
      Social_core.access_token =
        {|{"access_token":"read_token","instance_url":"https://mastodon.social"}|};
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

module Mastodon = Social_mastodon_v1.Make (Read_config)

let string_contains s sub =
  try
    let _ = Str.search_forward (Str.regexp_string sub) s 0 in
    true
  with Not_found -> false

let test_get_status_contract () =
  Printf.printf "Test: get_status parses required fields... ";
  Read_http.reset ();
  let got_result = ref false in
  Mastodon.get_status
    ~account_id:"acct"
    ~status_id:"42"
    (fun result ->
      got_result := true;
      match result with
      | Ok status ->
          assert (String.equal status.id "42");
          assert (String.equal status.content "<p>Status</p>");
          assert (status.url = Some "https://mastodon.social/@tester/42")
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  Printf.printf "✓\n"

let test_home_timeline_query_contract () =
  Printf.printf "Test: home timeline query contract... ";
  Read_http.reset ();
  let got_result = ref false in
  Mastodon.get_home_timeline
    ~account_id:"acct"
    ~limit:35
    ~max_id:(Some "100")
    ~since_id:(Some "50")
    ~min_id:(Some "60")
    (fun result ->
      got_result := true;
      match result with
      | Ok statuses ->
          assert (List.length statuses = 1);
          assert (String.equal (List.hd statuses).id "1")
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  let url = Option.get !(Read_http.last_get_url) in
  assert (string_contains url "/api/v1/timelines/home");
  assert (string_contains url "limit=35");
  assert (string_contains url "max_id=100");
  assert (string_contains url "since_id=50");
  assert (string_contains url "min_id=60");
  Printf.printf "✓\n"

let test_home_timeline_pagination_cursors_contract () =
  Printf.printf "Test: home timeline pagination cursors contract... ";
  Read_http.reset ();
  let got_result = ref false in
  Mastodon.get_home_timeline_with_pagination
    ~account_id:"acct"
    ~limit:20
    (fun result ->
      got_result := true;
      match result with
      | Ok page ->
          assert (List.length page.items = 1);
          assert (page.cursors.next_max_id = Some "90");
          assert (page.cursors.prev_min_id = Some "110")
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  Printf.printf "✓\n"

let test_public_timeline_query_contract () =
  Printf.printf "Test: public timeline query contract... ";
  Read_http.reset ();
  let got_result = ref false in
  Mastodon.get_public_timeline
    ~account_id:"acct"
    ~limit:40
    ~local:true
    ~max_id:(Some "200")
    (fun result ->
      got_result := true;
      match result with
      | Ok statuses -> assert (List.length statuses = 1)
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  let url = Option.get !(Read_http.last_get_url) in
  assert (string_contains url "/api/v1/timelines/public");
  assert (string_contains url "limit=40");
  assert (string_contains url "local=true");
  assert (string_contains url "max_id=200");
  Printf.printf "✓\n"

let test_tag_timeline_query_contract () =
  Printf.printf "Test: tag timeline query contract... ";
  Read_http.reset ();
  let got_result = ref false in
  Mastodon.get_tag_timeline
    ~account_id:"acct"
    ~tag:"ocaml"
    ~limit:15
    ~since_id:(Some "10")
    (fun result ->
      got_result := true;
      match result with
      | Ok statuses ->
          assert (List.length statuses = 1);
          assert (String.equal (List.hd statuses).id "7")
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  let url = Option.get !(Read_http.last_get_url) in
  assert (string_contains url "/api/v1/timelines/tag/ocaml");
  assert (string_contains url "limit=15");
  assert (string_contains url "since_id=10");
  Printf.printf "✓\n"

let test_search_statuses_query_contract () =
  Printf.printf "Test: statuses search query contract... ";
  Read_http.reset ();
  let got_result = ref false in
  Mastodon.search_statuses
    ~account_id:"acct"
    ~query:"ocaml mastodon"
    ~resolve:true
    ~limit:25
    (fun result ->
      got_result := true;
      match result with
      | Ok statuses ->
          assert (List.length statuses = 1);
          assert (String.equal (List.hd statuses).id "9")
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  let url = Option.get !(Read_http.last_get_url) in
  assert (string_contains url "/api/v2/search");
  assert (string_contains url "type=statuses");
  assert (string_contains url "resolve=true");
  assert (string_contains url "limit=25");
  Printf.printf "✓\n"

let test_search_accounts_query_contract () =
  Printf.printf "Test: account search query contract... ";
  Read_http.reset ();
  let got_result = ref false in
  Mastodon.search_accounts
    ~account_id:"acct"
    ~query:"tester"
    ~resolve:true
    ~limit:30
    (fun result ->
      got_result := true;
      match result with
      | Ok accounts ->
          assert (List.length accounts = 1);
          assert (String.equal (List.hd accounts).id "22");
          assert (String.equal (List.hd accounts).acct "tester@example.com")
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  let url = Option.get !(Read_http.last_get_url) in
  assert (string_contains url "/api/v1/accounts/search");
  assert (string_contains url "q=tester");
  assert (string_contains url "resolve=true");
  assert (string_contains url "limit=30");
  Printf.printf "✓\n"

let test_search_hashtags_query_contract () =
  Printf.printf "Test: hashtag search query contract... ";
  Read_http.reset ();
  let got_result = ref false in
  Mastodon.search_hashtags
    ~account_id:"acct"
    ~query:"ocaml"
    ~resolve:true
    ~limit:12
    (fun result ->
      got_result := true;
      match result with
      | Ok tags ->
          assert (List.length tags = 1);
          assert (String.equal (List.hd tags).name "ocaml")
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  let url = Option.get !(Read_http.last_get_url) in
  assert (string_contains url "/api/v2/search");
  assert (string_contains url "q=ocaml");
  assert (string_contains url "type=hashtags");
  assert (string_contains url "resolve=true");
  assert (string_contains url "limit=12");
  Printf.printf "✓\n"

let test_search_all_query_contract () =
  Printf.printf "Test: search all query contract... ";
  Read_http.reset ();
  let got_result = ref false in
  Mastodon.search_all
    ~account_id:"acct"
    ~query:"ocaml"
    ~resolve:true
    ~limit:14
    (fun result ->
      got_result := true;
      match result with
      | Ok payload ->
          assert (List.length payload.statuses = 1);
          assert (List.length payload.accounts = 1);
          assert (List.length payload.hashtags = 1)
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  let url = Option.get !(Read_http.last_get_url) in
  assert (string_contains url "/api/v2/search");
  assert (string_contains url "q=ocaml");
  assert (not (string_contains url "type="));
  assert (string_contains url "resolve=true");
  assert (string_contains url "limit=14");
  Printf.printf "✓\n"

let test_get_notifications_query_contract () =
  Printf.printf "Test: notifications query contract... ";
  Read_http.reset ();
  let got_result = ref false in
  Mastodon.get_notifications
    ~account_id:"acct"
    ~limit:12
    ~max_id:(Some "500")
    ~exclude_types:["follow"; "favourite"]
    (fun result ->
      got_result := true;
      match result with
      | Ok notifications ->
          assert (List.length notifications = 1);
          assert (String.equal (List.hd notifications).id "301");
          assert (String.equal (List.hd notifications).notification_type "mention")
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  let url = Option.get !(Read_http.last_get_url) in
  assert (string_contains url "/api/v1/notifications");
  assert (string_contains url "limit=12");
  assert (string_contains url "max_id=500");
  let excludes =
    Uri.of_string url
    |> Uri.query
    |> List.assoc_opt "exclude_types[]"
    |> Option.value ~default:[]
  in
  assert (List.mem "follow" excludes);
  assert (List.mem "favourite" excludes);
  Printf.printf "✓\n"

let test_get_account_contract () =
  Printf.printf "Test: get account by id contract... ";
  Read_http.reset ();
  let got_result = ref false in
  Mastodon.get_account
    ~account_id:"acct"
    ~target_account_id:"22"
    (fun result ->
      got_result := true;
      match result with
      | Ok account ->
          assert (String.equal account.id "22");
          assert (String.equal account.acct "tester@example.com")
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  let url = Option.get !(Read_http.last_get_url) in
  assert (string_contains url "/api/v1/accounts/22");
  Printf.printf "✓\n"

let test_lookup_account_by_acct_contract () =
  Printf.printf "Test: lookup account by acct contract... ";
  Read_http.reset ();
  let got_result = ref false in
  Mastodon.lookup_account_by_acct
    ~account_id:"acct"
    ~acct:"lookup@example.com"
    (fun result ->
      got_result := true;
      match result with
      | Ok account ->
          assert (String.equal account.id "23");
          assert (String.equal account.acct "lookup@example.com")
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  let url = Option.get !(Read_http.last_get_url) in
  assert (string_contains url "/api/v1/accounts/lookup");
  let acct_values =
    Uri.of_string url
    |> Uri.query
    |> List.assoc_opt "acct"
    |> Option.value ~default:[]
  in
  assert (List.mem "lookup@example.com" acct_values);
  Printf.printf "✓\n"

let test_get_account_statuses_query_contract () =
  Printf.printf "Test: account statuses query contract... ";
  Read_http.reset ();
  let got_result = ref false in
  Mastodon.get_account_statuses
    ~account_id:"acct"
    ~target_account_id:"22"
    ~limit:8
    ~exclude_replies:true
    ~exclude_reblogs:true
    ~max_id:(Some "90")
    (fun result ->
      got_result := true;
      match result with
      | Ok statuses ->
          assert (List.length statuses = 1);
          assert (String.equal (List.hd statuses).id "44")
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  let url = Option.get !(Read_http.last_get_url) in
  assert (string_contains url "/api/v1/accounts/22/statuses");
  assert (string_contains url "limit=8");
  assert (string_contains url "exclude_replies=true");
  assert (string_contains url "exclude_reblogs=true");
  assert (string_contains url "max_id=90");
  Printf.printf "✓\n"

let test_get_account_relationships_query_contract () =
  Printf.printf "Test: account relationships query contract... ";
  Read_http.reset ();
  let got_result = ref false in
  Mastodon.get_account_relationships
    ~account_id:"acct"
    ~target_account_ids:["22"]
    (fun result ->
      got_result := true;
      match result with
      | Ok relationships ->
          assert (List.length relationships = 1);
          assert (String.equal (List.hd relationships).id "22");
          assert ((List.hd relationships).following)
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  let url = Option.get !(Read_http.last_get_url) in
  assert (string_contains url "/api/v1/accounts/relationships");
  let ids =
    Uri.of_string url
    |> Uri.query
    |> List.assoc_opt "id[]"
    |> Option.value ~default:[]
  in
  assert (List.mem "22" ids);
  Printf.printf "✓\n"

let test_get_account_followers_query_contract () =
  Printf.printf "Test: account followers query contract... ";
  Read_http.reset ();
  let got_result = ref false in
  Mastodon.get_account_followers
    ~account_id:"acct"
    ~target_account_id:"22"
    ~limit:25
    ~max_id:(Some "500")
    (fun result ->
      got_result := true;
      match result with
      | Ok accounts ->
          assert (List.length accounts = 1);
          assert (String.equal (List.hd accounts).id "33")
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  let url = Option.get !(Read_http.last_get_url) in
  assert (string_contains url "/api/v1/accounts/22/followers");
  assert (string_contains url "limit=25");
  assert (string_contains url "max_id=500");
  Printf.printf "✓\n"

let test_get_account_following_query_contract () =
  Printf.printf "Test: account following query contract... ";
  Read_http.reset ();
  let got_result = ref false in
  Mastodon.get_account_following
    ~account_id:"acct"
    ~target_account_id:"22"
    ~limit:30
    ~since_id:(Some "10")
    (fun result ->
      got_result := true;
      match result with
      | Ok accounts ->
          assert (List.length accounts = 1);
          assert (String.equal (List.hd accounts).id "44")
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  let url = Option.get !(Read_http.last_get_url) in
  assert (string_contains url "/api/v1/accounts/22/following");
  assert (string_contains url "limit=30");
  assert (string_contains url "since_id=10");
  Printf.printf "✓\n"

let test_get_account_featured_tags_contract () =
  Printf.printf "Test: account featured tags contract... ";
  Read_http.reset ();
  let got_result = ref false in
  Mastodon.get_account_featured_tags
    ~account_id:"acct"
    ~target_account_id:"22"
    (fun result ->
      got_result := true;
      match result with
      | Ok tags ->
          assert (List.length tags = 1);
          assert (String.equal (List.hd tags).name "featured")
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  let url = Option.get !(Read_http.last_get_url) in
  assert (string_contains url "/api/v1/accounts/22/featured_tags");
  Printf.printf "✓\n"

let test_get_trending_tags_contract () =
  Printf.printf "Test: trending tags query contract... ";
  Read_http.reset ();
  let got_result = ref false in
  Mastodon.get_trending_tags
    ~account_id:"acct"
    ~limit:5
    (fun result ->
      got_result := true;
      match result with
      | Ok tags ->
          assert (List.length tags = 1);
          assert (String.equal (List.hd tags).name "ocaml")
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  let url = Option.get !(Read_http.last_get_url) in
  assert (string_contains url "/api/v1/trends/tags");
  assert (string_contains url "limit=5");
  Printf.printf "✓\n"

let test_get_trending_statuses_contract () =
  Printf.printf "Test: trending statuses query contract... ";
  Read_http.reset ();
  let got_result = ref false in
  Mastodon.get_trending_statuses
    ~account_id:"acct"
    ~limit:6
    (fun result ->
      got_result := true;
      match result with
      | Ok statuses ->
          assert (List.length statuses = 1);
          assert (String.equal (List.hd statuses).id "77")
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  let url = Option.get !(Read_http.last_get_url) in
  assert (string_contains url "/api/v1/trends/statuses");
  assert (string_contains url "limit=6");
  Printf.printf "✓\n"

let test_get_trending_links_contract () =
  Printf.printf "Test: trending links query contract... ";
  Read_http.reset ();
  let got_result = ref false in
  Mastodon.get_trending_links
    ~account_id:"acct"
    ~limit:7
    (fun result ->
      got_result := true;
      match result with
      | Ok links ->
          assert (List.length links = 1);
          assert (String.equal (List.hd links).url "https://example.com/post")
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  let url = Option.get !(Read_http.last_get_url) in
  assert (string_contains url "/api/v1/trends/links");
  assert (string_contains url "limit=7");
  Printf.printf "✓\n"

let test_get_bookmarks_query_contract () =
  Printf.printf "Test: bookmarks query contract... ";
  Read_http.reset ();
  let got_result = ref false in
  Mastodon.get_bookmarks
    ~account_id:"acct"
    ~limit:11
    ~max_id:(Some "300")
    (fun result ->
      got_result := true;
      match result with
      | Ok statuses ->
          assert (List.length statuses = 1);
          assert (String.equal (List.hd statuses).id "88")
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  let url = Option.get !(Read_http.last_get_url) in
  assert (string_contains url "/api/v1/bookmarks");
  assert (string_contains url "limit=11");
  assert (string_contains url "max_id=300");
  Printf.printf "✓\n"

let test_get_favourites_query_contract () =
  Printf.printf "Test: favourites query contract... ";
  Read_http.reset ();
  let got_result = ref false in
  Mastodon.get_favourites
    ~account_id:"acct"
    ~limit:9
    ~since_id:(Some "120")
    (fun result ->
      got_result := true;
      match result with
      | Ok statuses ->
          assert (List.length statuses = 1);
          assert (String.equal (List.hd statuses).id "89")
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  let url = Option.get !(Read_http.last_get_url) in
  assert (string_contains url "/api/v1/favourites");
  assert (string_contains url "limit=9");
  assert (string_contains url "since_id=120");
  Printf.printf "✓\n"

let test_get_lists_contract () =
  Printf.printf "Test: lists endpoint contract... ";
  Read_http.reset ();
  let got_result = ref false in
  Mastodon.get_lists
    ~account_id:"acct"
    (fun result ->
      got_result := true;
      match result with
      | Ok lists ->
          assert (List.length lists = 1);
          assert (String.equal (List.hd lists).id "l1");
          assert (String.equal (List.hd lists).title "Friends")
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  let url = Option.get !(Read_http.last_get_url) in
  assert (string_contains url "/api/v1/lists");
  Printf.printf "✓\n"

let test_get_list_accounts_query_contract () =
  Printf.printf "Test: list accounts query contract... ";
  Read_http.reset ();
  let got_result = ref false in
  Mastodon.get_list_accounts
    ~account_id:"acct"
    ~list_id:"l1"
    ~limit:16
    ~max_id:(Some "801")
    ~since_id:(Some "700")
    ~min_id:(Some "750")
    (fun result ->
      got_result := true;
      match result with
      | Ok accounts ->
          assert (List.length accounts = 1);
          assert (String.equal (List.hd accounts).id "55")
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  let url = Option.get !(Read_http.last_get_url) in
  assert (string_contains url "/api/v1/lists/l1/accounts");
  assert (string_contains url "limit=16");
  assert (string_contains url "max_id=801");
  assert (string_contains url "since_id=700");
  assert (string_contains url "min_id=750");
  Printf.printf "✓\n"

let test_get_account_lists_contract () =
  Printf.printf "Test: account lists contract... ";
  Read_http.reset ();
  let got_result = ref false in
  Mastodon.get_account_lists
    ~account_id:"acct"
    ~target_account_id:"22"
    (fun result ->
      got_result := true;
      match result with
      | Ok lists ->
          assert (List.length lists = 1);
          assert (String.equal (List.hd lists).id "l2")
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  let url = Option.get !(Read_http.last_get_url) in
  assert (string_contains url "/api/v1/accounts/22/lists");
  Printf.printf "✓\n"

let test_get_list_timeline_query_contract () =
  Printf.printf "Test: list timeline query contract... ";
  Read_http.reset ();
  let got_result = ref false in
  Mastodon.get_list_timeline
    ~account_id:"acct"
    ~list_id:"l1"
    ~limit:13
    ~max_id:(Some "700")
    (fun result ->
      got_result := true;
      match result with
      | Ok statuses ->
          assert (List.length statuses = 1);
          assert (String.equal (List.hd statuses).id "90")
      | Error err ->
          failwith (Printf.sprintf "Unexpected error: %s" (Error_types.error_to_string err)));
  assert !got_result;
  let url = Option.get !(Read_http.last_get_url) in
  assert (string_contains url "/api/v1/timelines/list/l1");
  assert (string_contains url "limit=13");
  assert (string_contains url "max_id=700");
  Printf.printf "✓\n"

let () =
  Printf.printf "\n=== Mastodon Read Contract Tests ===\n\n";
  test_get_status_contract ();
  test_home_timeline_query_contract ();
  test_home_timeline_pagination_cursors_contract ();
  test_public_timeline_query_contract ();
  test_tag_timeline_query_contract ();
  test_search_statuses_query_contract ();
  test_search_accounts_query_contract ();
  test_search_hashtags_query_contract ();
  test_search_all_query_contract ();
  test_get_notifications_query_contract ();
  test_get_account_contract ();
  test_lookup_account_by_acct_contract ();
  test_get_account_statuses_query_contract ();
  test_get_account_relationships_query_contract ();
  test_get_account_followers_query_contract ();
  test_get_account_following_query_contract ();
  test_get_account_featured_tags_contract ();
  test_get_trending_tags_contract ();
  test_get_trending_statuses_contract ();
  test_get_trending_links_contract ();
  test_get_bookmarks_query_contract ();
  test_get_favourites_query_contract ();
  test_get_lists_contract ();
  test_get_list_accounts_query_contract ();
  test_get_account_lists_contract ();
  test_get_list_timeline_query_contract ();
  Printf.printf "\n✓ Read contract tests passed\n"
