(** Contract tests for the typed status / context / notification / WebFinger
    reads ([Social_mastodon_v1.Reads], the [Make] wrappers and [Mastodon_entities]).

    Response bodies follow the examples on docs.joinmastodon.org (entities
    Status, Context and Notification, methods statuses#context,
    notifications#get, accounts#lookup and the WebFinger spec page), with
    every attribute the Status serializer emits. *)

module E = Social_mastodon_v1.Entities

(* {1 Mock HTTP} *)

type request = { url : string; headers : (string * string) list }

module Mock_http = struct
  let requests : request list ref = ref []

  (* (url substring, response) pairs, first match wins. *)
  let routes : (string * (Social_core.response, string) result) list ref = ref []

  let reset new_routes =
    requests := [];
    routes := new_routes

  let contains haystack needle =
    try
      ignore (Str.search_forward (Str.regexp_string needle) haystack 0);
      true
    with Not_found -> false

  let get ?(headers = []) url on_success on_error =
    requests := { url; headers } :: !requests;
    match List.find_opt (fun (needle, _) -> contains url needle) !routes with
    | Some (_, Ok response) -> on_success response
    | Some (_, Error message) -> on_error message
    | None -> failwith ("unexpected GET " ^ url)

  let post ?headers:_ ?body:_ url _ _ = failwith ("unexpected POST " ^ url)
  let post_multipart ?headers:_ ~parts:_ url _ _ = failwith ("unexpected POST " ^ url)
  let put ?headers:_ ?body:_ url _ _ = failwith ("unexpected PUT " ^ url)
  let delete ?headers:_ url _ _ = failwith ("unexpected DELETE " ^ url)
end

let respond ?(headers = []) status body =
  Ok
    {
      Social_core.status;
      headers = ("content-type", "application/json; charset=utf-8") :: headers;
      body;
    }

module Reads = Social_mastodon_v1.Reads.Make (Mock_http)

let instance_url = "https://mastodon.example"

(* {1 Fixtures} *)

let account_json ~id ~username ~acct ~display_name =
  Printf.sprintf
    {|{"id":"%s","username":"%s","acct":"%s","display_name":"%s","locked":false,"bot":false,"discoverable":true,"group":false,"created_at":"2016-03-16T00:00:00.000Z","note":"<p>Bio</p>","url":"https://mastodon.example/@%s","uri":"https://mastodon.example/users/%s","avatar":"https://files.mastodon.example/avatars/%s.png","avatar_static":"https://files.mastodon.example/avatars/%s.png","header":"https://mastodon.example/headers/original/missing.png","header_static":"https://mastodon.example/headers/original/missing.png","followers_count":10,"following_count":5,"statuses_count":42,"last_status_at":"2019-11-23","emojis":[],"fields":[]}|}
    id username acct display_name username username id id

let alice = account_json ~id:"14715" ~username:"alice" ~acct:"alice" ~display_name:"Alice"

let bob =
  account_json ~id:"971724" ~username:"bob" ~acct:"bob@remote.example"
    ~display_name:""

let status_json ?(in_reply_to = "null") ?(in_reply_to_account = "null")
    ?(visibility = "public") ?(reblog = "null") ?(mentions = "[]")
    ?(url = "") ~id ~account ~content () =
  let url =
    if url = "" then Printf.sprintf {|"https://mastodon.example/@alice/%s"|} id
    else url
  in
  Printf.sprintf
    {|{"id":"%s","created_at":"2019-11-23T19:44:00.124Z","in_reply_to_id":%s,"in_reply_to_account_id":%s,"sensitive":false,"spoiler_text":"","visibility":"%s","language":"en","uri":"https://mastodon.example/users/alice/statuses/%s","url":%s,"replies_count":1,"reblogs_count":2,"favourites_count":3,"quotes_count":0,"edited_at":null,"favourited":false,"reblogged":false,"muted":false,"bookmarked":false,"content":%s,"filtered":[],"reblog":%s,"application":{"name":"Web","website":null},"account":%s,"media_attachments":[],"mentions":%s,"tags":[],"emojis":[],"quote":null,"card":null,"poll":null,"quote_approval":{"automatic":[],"manual":[],"current_user":"automatic"}}|}
    id in_reply_to in_reply_to_account visibility id url
    (Yojson.Basic.to_string (`String content))
    reblog account mentions

let quoted value = Printf.sprintf {|"%s"|} value

let mention_of_alice =
  {|[{"id":"14715","username":"alice","url":"https://mastodon.example/@alice","acct":"alice"}]|}

let root = status_json ~id:"103188938570975982" ~account:alice ~content:"<p>Root</p>" ()

let reply =
  status_json ~id:"103188971072973252"
    ~in_reply_to:(quoted "103188938570975982")
    ~in_reply_to_account:(quoted "14715") ~mentions:mention_of_alice
    ~account:bob
    ~content:
      {|<p><span class="h-card"><a href="https://mastodon.example/@alice" class="u-url mention">@<span>alice</span></a></span> sup!</p>|}
    ()

let nested_reply =
  status_json ~id:"103188982235527758"
    ~in_reply_to:(quoted "103188971072973252")
    ~in_reply_to_account:(quoted "971724") ~account:alice
    ~content:"<p>Thanks &amp; welcome</p>" ()

let expect_ok what = function
  | Ok value -> value
  | Error err ->
      failwith (Printf.sprintf "%s: %s" what (Error_types.error_to_string err))

let capture call =
  let result = ref None in
  call (fun value -> result := Some value);
  match !result with Some value -> value | None -> failwith "callback not called"

let last_request () =
  match !Mock_http.requests with request :: _ -> request | [] -> failwith "no request"

(* Every value of [key], one per [key=value] pair. Uri.get_query_param'
   would also split a comma-joined value, which Rails does not. *)
let query_values url key =
  match
    Uri.query (Uri.of_string url)
    |> List.filter (fun (name, _) -> String.equal name key)
    |> List.concat_map (fun (_, values) ->
           List.iter (fun value -> assert (not (String.contains value ','))) values;
           values)
  with
  | [] -> None
  | values -> Some values

let query_pair_count url key =
  Uri.query (Uri.of_string url) |> List.filter (fun (name, _) -> String.equal name key) |> List.length

let header request name = List.assoc_opt name request.headers

(* {1 Status and context} *)

let test_status_of_docs_example () =
  Printf.printf "Test: Status entity from the docs example... ";
  let json =
    Yojson.Basic.from_string
      (status_json ~id:"103270115826048975"
         ~url:{|"https://mastodon.social/@Gargron/103270115826048975"|}
         ~account:
           (account_json ~id:"1" ~username:"Gargron" ~acct:"Gargron"
              ~display_name:"Eugen")
         ~content:
           {|<p>&quot;I lost my inheritance with one wrong digit on my sort code&quot;</p><p><a href="https://www.theguardian.com/money/2019/dec/07/x" rel="nofollow noopener noreferrer" target="_blank"><span class="invisible">https://www.</span><span class="ellipsis">theguardian.com/money/2019/dec</span><span class="invisible">/07/x</span></a></p>|}
         ())
  in
  let status =
    match E.status_of_json json with Ok s -> s | Error msg -> failwith msg
  in
  assert (status.id = "103270115826048975");
  assert (status.account.acct = "Gargron");
  assert (status.account.display_name = Some "Eugen");
  assert (status.account.url = Some "https://mastodon.example/@Gargron");
  assert (status.visibility = E.Visibility E.Public);
  assert (status.in_reply_to_id = None);
  assert (status.language = Some "en");
  assert (status.replies_count = 1 && status.reblogs_count = 2 && status.favourites_count = 3);
  assert (E.status_web_url status = "https://mastodon.social/@Gargron/103270115826048975");
  assert (
    E.html_to_plain_text status.content
    = "\"I lost my inheritance with one wrong digit on my sort code\"\n\n\
       https://www.theguardian.com/money/2019/dec/07/x");
  Printf.printf "✓\n"

let test_status_rejects_missing_required_field () =
  Printf.printf "Test: Status without a required attribute is an error... ";
  let json =
    match Yojson.Basic.from_string root with
    | `Assoc fields -> `Assoc (List.remove_assoc "account" fields)
    | _ -> assert false
  in
  (match E.status_of_json json with
  | Error msg -> assert (Mock_http.contains msg "account")
  | Ok _ -> failwith "expected an error");
  Printf.printf "✓\n"

let test_context_with_token () =
  Printf.printf "Test: get_status_context request, typed tree, async refresh... ";
  let body =
    Printf.sprintf {|{"ancestors":[],"descendants":[%s,%s]}|} reply nested_reply
  in
  Mock_http.reset
    [
      ( "/api/v1/statuses/103188938570975982/context",
        respond
          ~headers:
            [
              ( "Mastodon-Async-Refresh",
                {|id="ImNvbnRleHQ6MTEzNjQwNTczMzAzNzg1MTc4OnJlZnJlc2gi--c526259eb4a1f3ef0d4b91cf8c99bf501330a815", retry=5, result_count=2|}
              );
            ]
          200 body );
    ];
  let context =
    capture
      (Reads.get_status_context ~instance_url ~access_token:"user-token"
         ~status_id:"103188938570975982")
    |> expect_ok "context"
  in
  let request = last_request () in
  assert (
    request.url
    = "https://mastodon.example/api/v1/statuses/103188938570975982/context");
  assert (header request "Authorization" = Some "Bearer user-token");
  assert (context.ancestors = []);
  (match context.descendants with
  | [ first; second ] ->
      assert (first.in_reply_to_id = Some "103188938570975982");
      assert (first.account.acct = "bob@remote.example");
      assert (first.account.display_name = None);
      assert (first.mentions = [ { E.id = "14715"; username = "alice"; acct = "alice"; url = "https://mastodon.example/@alice" } ]);
      assert (E.html_to_plain_text first.content = "@alice sup!");
      assert (second.in_reply_to_id = Some "103188971072973252");
      assert (E.html_to_plain_text second.content = "Thanks & welcome")
  | _ -> failwith "expected two descendants");
  (match context.async_refresh with
  | Some refresh ->
      assert (refresh.retry_seconds = 5);
      assert (refresh.result_count = Some 2);
      assert (
        String.starts_with ~prefix:"ImNvbnRleHQ6" refresh.refresh_id
        && String.ends_with ~suffix:"bf501330a815" refresh.refresh_id)
  | None -> failwith "expected async refresh");
  Printf.printf "✓\n"

let test_context_public_and_not_found () =
  Printf.printf "Test: get_status_context without token; 404 is Resource_not_found... ";
  Mock_http.reset
    [
      ("/statuses/1/context", respond 200 {|{"ancestors":[],"descendants":[]}|});
      ("/statuses/2/context", respond 404 {|{"error":"Record not found"}|});
    ];
  let context =
    capture (Reads.get_status_context ~instance_url ~status_id:"1")
    |> expect_ok "context"
  in
  assert (header (last_request ()) "Authorization" = None);
  assert (context.async_refresh = None);
  (match capture (Reads.get_status_context ~instance_url ~status_id:"2") with
  | Error (Error_types.Resource_not_found "Record not found") -> ()
  | Error err -> failwith (Error_types.error_to_string err)
  | Ok _ -> failwith "expected 404");
  Printf.printf "✓\n"

let test_context_errors () =
  Printf.printf "Test: context 401/429/malformed/network errors... ";
  Mock_http.reset
    [
      ("/statuses/401/", respond 401 {|{"error":"The access token is invalid"}|});
      ( "/statuses/429/",
        respond ~headers:[ ("Retry-After", "17") ] 429 {|{"error":"Too many requests"}|} );
      ("/statuses/bad/", respond 200 {|{"ancestors":[{"id":"1"}],"descendants":[]}|});
      ("/statuses/down/", Error "connection refused");
    ];
  let get status_id =
    capture (Reads.get_status_context ~instance_url ~access_token:"t" ~status_id)
  in
  (match get "401" with
  | Error (Error_types.Auth_error Error_types.Token_invalid) -> ()
  | _ -> failwith "401");
  (match get "429" with
  | Error (Error_types.Rate_limited { retry_after_seconds = Some 17; _ }) -> ()
  | _ -> failwith "429");
  (match get "bad" with
  | Error (Error_types.Internal_error msg) -> assert (Mock_http.contains msg "status 1")
  | _ -> failwith "malformed");
  (match get "down" with
  | Error (Error_types.Network_error (Error_types.Connection_failed _)) -> ()
  | _ -> failwith "network");
  Printf.printf "✓\n"

let test_get_status_typed () =
  Printf.printf "Test: get_status returns the typed status, boost and odd visibility... ";
  let boost =
    status_json ~id:"500" ~account:bob ~content:"" ~visibility:"local"
      ~reblog:root ()
  in
  Mock_http.reset [ ("/api/v1/statuses/500", respond 200 boost) ];
  let status =
    capture (Reads.get_status ~instance_url ~access_token:"t" ~status_id:"500")
    |> expect_ok "status"
  in
  assert (status.visibility = E.Other_visibility "local");
  (match status.reblog with
  | Some original -> assert (original.id = "103188938570975982")
  | None -> failwith "expected reblog");
  Printf.printf "✓\n"

(* {1 Notifications} *)

let notifications_body =
  Printf.sprintf
    {|[{"id":"34975861","type":"mention","group_key":"ungrouped-34975861","created_at":"2019-11-23T07:49:02.064Z","account":%s,"status":%s},{"id":"34975535","type":"favourite","group_key":"favourite-1-2","created_at":"2019-11-23T07:29:18.903Z","account":%s,"status":%s},{"id":"34975000","type":"some_future_type","created_at":"2019-11-23T07:00:00.000Z","account":%s}]|}
    bob reply bob root alice

let test_notifications_request_and_parse () =
  Printf.printf "Test: get_notifications filters, Link cursors, typed items... ";
  Mock_http.reset
    [
      ( "/api/v1/notifications",
        respond
          ~headers:
            [
              ( "Link",
                {|<https://mastodon.example/api/v1/notifications?max_id=34975000>; rel="next", <https://mastodon.example/api/v1/notifications?min_id=34975861>; rel="prev"|}
              );
            ]
          200 notifications_body );
    ];
  let page =
    capture
      (Reads.get_notifications ~instance_url ~access_token:"user-token" ~limit:80
         ~min_id:"34974000"
         ~types:[ E.Mention; E.Admin_sign_up ]
         ~exclude_types:[ E.Follow ] ~from_account_id:"971724")
    |> expect_ok "notifications"
  in
  let request = last_request () in
  assert (String.starts_with ~prefix:"https://mastodon.example/api/v1/notifications?" request.url);
  assert (header request "Authorization" = Some "Bearer user-token");
  assert (query_values request.url "types[]" = Some [ "mention"; "admin.sign_up" ]);
  assert (query_pair_count request.url "types[]" = 2);
  assert (query_values request.url "exclude_types[]" = Some [ "follow" ]);
  assert (query_values request.url "limit" = Some [ "80" ]);
  assert (query_values request.url "min_id" = Some [ "34974000" ]);
  assert (query_values request.url "account_id" = Some [ "971724" ]);
  assert (query_values request.url "max_id" = None);
  assert (page.cursors.next_max_id = Some "34975000");
  assert (page.cursors.prev_min_id = Some "34975861");
  (match page.notifications with
  | [ mention; favourite; future ] ->
      assert (mention.notification_type = E.Mention);
      assert (mention.group_key = Some "ungrouped-34975861");
      assert (mention.account.acct = "bob@remote.example");
      (match mention.status with
      | Some status -> assert (status.in_reply_to_id = Some "103188938570975982")
      | None -> failwith "mention without status");
      assert (favourite.notification_type = E.Favourite);
      assert (future.notification_type = E.Other_notification_type "some_future_type");
      assert (future.status = None && future.group_key = None)
  | _ -> failwith "expected three notifications");
  Printf.printf "✓\n"

let test_notifications_last_page_and_scope_error () =
  Printf.printf "Test: notifications without Link header; 403 names read:notifications... ";
  Mock_http.reset [ ("/api/v1/notifications", respond 200 "[]") ];
  let page =
    capture (Reads.get_notifications ~instance_url ~access_token:"t") |> expect_ok "page"
  in
  assert (page.notifications = []);
  assert (page.cursors = { E.next_max_id = None; prev_since_id = None; prev_min_id = None });
  assert (Uri.query (Uri.of_string (last_request ()).url) = []);
  Mock_http.reset
    [ ("/api/v1/notifications", respond 403 {|{"error":"This action is outside the authorized scopes"}|}) ];
  (match capture (Reads.get_notifications ~instance_url ~access_token:"t") with
  | Error
      (Error_types.Auth_error
        (Error_types.Insufficient_permissions { required = [ "read:notifications" ]; _ })) ->
      ()
  | _ -> failwith "expected 403 mapping");
  Printf.printf "✓\n"

let test_notification_type_round_trip () =
  Printf.printf "Test: notification type wire values round-trip... ";
  let all =
    E.
      [
        Mention; Status; Reblog; Follow; Follow_request; Favourite; Poll; Update;
        Admin_sign_up; Admin_report; Severed_relationships; Moderation_warning;
        Quote; Quoted_update; Added_to_collection; Collection_update;
      ]
  in
  List.iter
    (fun t ->
      let wire = E.notification_type_to_string t in
      assert (E.notification_type_of_string wire = t);
      assert (
        match E.notification_type_of_string wire with
        | E.Other_notification_type _ -> false
        | _ -> true))
    all;
  assert (E.notification_type_to_string E.Admin_report = "admin.report");
  Printf.printf "✓\n"

(* {1 Account statuses and lookup} *)

let test_account_statuses () =
  Printf.printf "Test: get_account_statuses flags and cursors... ";
  Mock_http.reset
    [
      ( "/api/v1/accounts/14715/statuses",
        respond
          ~headers:[ ("link", {|<https://mastodon.example/api/v1/accounts/14715/statuses?max_id=103188938570975982>; rel="next"|}) ]
          200
          (Printf.sprintf "[%s]" root) );
    ];
  let page =
    capture
      (Reads.get_account_statuses ~instance_url ~target_account_id:"14715" ~limit:40
         ~exclude_replies:true ~exclude_reblogs:true)
    |> expect_ok "account statuses"
  in
  let url = (last_request ()).url in
  assert (query_values url "exclude_replies" = Some [ "true" ]);
  assert (query_values url "exclude_reblogs" = Some [ "true" ]);
  assert (query_values url "limit" = Some [ "40" ]);
  assert (query_values url "pinned" = None && query_values url "only_media" = None);
  assert (List.length page.statuses = 1);
  assert (page.cursors.next_max_id = Some "103188938570975982");
  Printf.printf "✓\n"

(* Account entity example from https://docs.joinmastodon.org/entities/Account/
   (counts are integers, never null; hide_collections is 4.3.0+). *)
let test_get_account_counts () =
  Printf.printf "Test: typed account with counts, public and own... ";
  Mock_http.reset [ ("/api/v1/accounts/14715", respond 200 alice) ];
  let account =
    capture (Reads.get_account ~instance_url ~target_account_id:"14715") |> expect_ok "account"
  in
  assert (account.E.summary.acct = "alice");
  assert (account.followers_count = 10 && account.following_count = 5 && account.statuses_count = 42);
  assert (account.last_status_at = Some "2019-11-23");
  assert (account.hide_collections = None && not account.locked);
  let request = last_request () in
  assert (request.url = "https://mastodon.example/api/v1/accounts/14715");
  assert (header request "Authorization" = None);
  let hidden =
    {|{"id":"1","username":"h","acct":"h","display_name":"","locked":true,"bot":false,"created_at":"2022-01-01T00:00:00.000Z","followers_count":0,"following_count":0,"statuses_count":3,"last_status_at":null,"hide_collections":true}|}
  in
  Mock_http.reset [ ("/api/v1/accounts/verify_credentials", respond 200 hidden) ];
  let own =
    capture (Reads.verify_credentials ~instance_url ~access_token:"tok") |> expect_ok "own"
  in
  assert (own.hide_collections = Some true && own.last_status_at = None && own.statuses_count = 3);
  assert (header (last_request ()) "Authorization" = Some "Bearer tok");
  Mock_http.reset
    [ ("/api/v1/accounts/1", respond 200 {|{"id":"1","acct":"h","created_at":"x","locked":false,"bot":false}|}) ];
  (match capture (Reads.get_account ~instance_url ~target_account_id:"1") with
   | Error (Error_types.Internal_error msg) -> assert (Mock_http.contains msg "followers_count")
   | _ -> failwith "an account without counts is a parse error");
  Mock_http.reset [ ("/api/v1/accounts/404", respond 404 {|{"error":"Record not found"}|}) ];
  (match capture (Reads.get_account ~instance_url ~target_account_id:"404") with
   | Error (Error_types.Resource_not_found "Record not found") -> ()
   | _ -> failwith "404 is Resource_not_found");
  Printf.printf "✓\n"

let test_lookup_account () =
  Printf.printf "Test: lookup_account ok and 404... ";
  Mock_http.reset
    [
      ("acct=ghost", respond 404 {|{"error":"Record not found"}|});
      ("/api/v1/accounts/lookup", respond 200 bob);
    ];
  let account =
    capture (Reads.lookup_account ~instance_url ~acct:"bob@remote.example")
    |> expect_ok "lookup"
  in
  assert (account.id = "971724" && account.acct = "bob@remote.example");
  assert (query_values (last_request ()).url "acct" = Some [ "bob@remote.example" ]);
  (match capture (Reads.lookup_account ~instance_url ~acct:"ghost") with
  | Error (Error_types.Resource_not_found _) -> ()
  | _ -> failwith "expected not found");
  Printf.printf "✓\n"

(* {1 WebFinger} *)

let gargron_jrd =
  {|{"subject":"acct:Gargron@mastodon.social","aliases":["https://mastodon.social/@Gargron","https://mastodon.social/users/Gargron"],"links":[{"rel":"http://webfinger.net/rel/profile-page","type":"text/html","href":"https://mastodon.social/@Gargron"},{"rel":"self","type":"application/activity+json","href":"https://mastodon.social/users/Gargron"},{"rel":"http://ostatus.org/schema/1.0/subscribe","template":"https://mastodon.social/authorize_interaction?uri={uri}"}]}|}

let test_webfinger () =
  Printf.printf "Test: webfinger request, actor and profile links, 404/410... ";
  Mock_http.reset
    [
      ("acct%3Agone%40mastodon.social", respond 410 "");
      ("acct:gone@mastodon.social", respond 410 "");
      ("acct%3Anobody%40mastodon.social", respond 404 "");
      ("acct:nobody@mastodon.social", respond 404 "");
      ("/.well-known/webfinger", respond 200 gargron_jrd);
    ];
  let jrd =
    capture (Reads.webfinger ~username:"Gargron" ~domain:"mastodon.social")
    |> expect_ok "webfinger"
  in
  let request = last_request () in
  let uri = Uri.of_string request.url in
  assert (Uri.scheme uri = Some "https");
  assert (Uri.host uri = Some "mastodon.social");
  assert (Uri.path uri = "/.well-known/webfinger");
  assert (Uri.get_query_param uri "resource" = Some "acct:Gargron@mastodon.social");
  assert (header request "Authorization" = None);
  assert (jrd.subject = "acct:Gargron@mastodon.social");
  assert (List.length jrd.aliases = 2);
  assert (E.webfinger_actor_url jrd = Some "https://mastodon.social/users/Gargron");
  assert (E.webfinger_profile_page jrd = Some "https://mastodon.social/@Gargron");
  (match capture (Reads.webfinger ~username:"gone" ~domain:"mastodon.social") with
  | Error (Error_types.Resource_not_found _) -> ()
  | _ -> failwith "410");
  (match capture (Reads.webfinger ~username:"nobody" ~domain:"mastodon.social") with
  | Error (Error_types.Resource_not_found _) -> ()
  | _ -> failwith "404");
  Printf.printf "✓\n"

let test_webfinger_pure () =
  Printf.printf "Test: webfinger_actor_url media types, split_acct... ";
  let ld_json =
    {|{"subject":"acct:a@b.example","links":[{"rel":"self","type":"text/html","href":"https://b.example/@a"},{"rel":"self","type":"application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"","href":"https://b.example/actors/a"},{"href":"https://no-rel.example"}]}|}
  in
  (match E.webfinger_of_json (Yojson.Basic.from_string ld_json) with
  | Ok jrd ->
      assert (List.length jrd.links = 2);
      assert (E.webfinger_actor_url jrd = Some "https://b.example/actors/a");
      assert (E.webfinger_profile_page jrd = None)
  | Error msg -> failwith msg);
  (match E.webfinger_of_json (Yojson.Basic.from_string {|{"links":[]}|}) with
  | Error _ -> ()
  | Ok _ -> failwith "subject is required");
  assert (E.split_acct "@alice@example.social" = Some ("alice", "example.social"));
  assert (E.split_acct "acct:alice@example.social" = Some ("alice", "example.social"));
  assert (E.split_acct " alice@example.social " = Some ("alice", "example.social"));
  assert (E.split_acct "alice" = None);
  assert (E.split_acct "alice@" = None);
  assert (E.split_acct "a@b@c" = None);
  assert (E.webfinger_resource ~username:"alice" ~domain:"example.social" = "acct:alice@example.social");
  Printf.printf "✓\n"

(* {1 Pure helpers} *)

let test_html_to_plain_text () =
  Printf.printf "Test: html_to_plain_text... ";
  let check html expected =
    let got = E.html_to_plain_text html in
    if got <> expected then
      failwith (Printf.sprintf "html_to_plain_text %S = %S, expected %S" html got expected)
  in
  check "<p>Line one<br>line two<br />line three</p><p>Second</p>"
    "Line one\nline two\nline three\n\nSecond";
  check "<p>It&#39;s &lt;fine&gt; &amp; &quot;ok&quot; &#x1F600;</p>" "It's <fine> & \"ok\" \xF0\x9F\x98\x80";
  check "<p>Fish &amp;amp; chips</p>" "Fish &amp; chips";
  check "<p>R&D & more &unknown; &#xZZ;</p>" "R&D & more &unknown; &#xZZ;";
  check "<ul><li>one</li><li>two</li></ul>" "one\ntwo";
  check "<p>a</p><p></p><p></p><p>b</p>" "a\n\nb";
  check "<P>Upper<BR>case</P>" "Upper\ncase";
  check "plain text" "plain text";
  check "unterminated <tag" "unterminated <tag";
  Printf.printf "✓\n"

let test_url_helpers () =
  Printf.printf "Test: status_id_of_url, normalize_instance_url, federated_acct... ";
  assert (E.status_id_of_url "https://mastodon.social/@alice/109" = Some "109");
  assert (E.status_id_of_url "https://mastodon.social/@alice/109/" = Some "109");
  assert (E.status_id_of_url "https://mastodon.social/@alice/109?x=1#top" = Some "109");
  assert (E.status_id_of_url "https://mastodon.social/users/alice/statuses/109" = Some "109");
  assert (E.status_id_of_url "https://gts.example/@alice/statuses/01HXYZ" = Some "01HXYZ");
  assert (E.status_id_of_url "https://mastodon.social/" = None);
  assert (E.status_id_of_url "109" = None);
  assert (E.status_id_of_url "" = None);
  assert (E.normalize_instance_url " mastodon.social/ " = Some "https://mastodon.social");
  assert (E.normalize_instance_url "http://localhost:3000//" = Some "http://localhost:3000");
  assert (E.normalize_instance_url "https://Mastodon.Social" = Some "https://Mastodon.Social");
  assert (E.normalize_instance_url "" = None);
  assert (E.normalize_instance_url "https://" = None);
  assert (E.federated_acct ~username:"@alice" ~instance_url:"https://Mastodon.Social" = "alice@mastodon.social");
  assert (E.federated_acct ~username:"bob@remote.example" ~instance_url:"https://mastodon.social" = "bob@remote.example");
  assert (E.federated_acct ~username:"alice" ~instance_url:"" = "alice");
  Printf.printf "✓\n"

(* {1 Account-bound wrappers} *)

module Config = struct
  module Http = Mock_http

  let get_env _ = None

  let get_credentials ~account_id:_ on_success _ =
    on_success
      {
        Social_core.access_token =
          {|{"access_token":"stored-token","instance_url":"https://mastodon.example"}|};
        refresh_token = None;
        expires_at = None;
        auth_type = Social_core.Bearer;
        scope = None;
      }

  let update_credentials ~account_id:_ ~credentials:_ on_success _ = on_success ()
  let encrypt data on_success _ = on_success data
  let decrypt data on_success _ = on_success data
  let sleep ~seconds:_ on_success _ = on_success ()
  let update_health_status ~account_id:_ ~status:_ ~error_message:_ on_success _ = on_success ()
end

module Mastodon = Social_mastodon_v1.Make (Config)

let test_account_wrappers () =
  Printf.printf "Test: account-bound wrappers use the stored instance and token... ";
  Mock_http.reset
    [
      ("/api/v1/accounts/verify_credentials", respond 200 alice);
      ("/context", respond 200 {|{"ancestors":[],"descendants":[]}|});
      ("/api/v1/notifications", respond 200 notifications_body);
      ("/api/v1/accounts/14715/statuses", respond 200 (Printf.sprintf "[%s]" root));
      ("/api/v1/statuses/", respond 200 root);
    ];
  let _ =
    capture (Mastodon.get_status_context ~account_id:"acc" ~status_id:"103188938570975982")
    |> expect_ok "context"
  in
  let request = last_request () in
  assert (request.url = "https://mastodon.example/api/v1/statuses/103188938570975982/context");
  assert (header request "Authorization" = Some "Bearer stored-token");
  let status =
    capture (Mastodon.get_status_entity ~account_id:"acc" ~status_id:"103188938570975982")
    |> expect_ok "status"
  in
  assert (status.uri = "https://mastodon.example/users/alice/statuses/103188938570975982");
  let page =
    capture (Mastodon.get_notifications_page ~account_id:"acc" ~types:[ E.Mention ])
    |> expect_ok "notifications"
  in
  assert (List.length page.notifications = 3);
  assert (query_values (last_request ()).url "types[]" = Some [ "mention" ]);
  let statuses =
    capture
      (Mastodon.get_account_statuses_page ~account_id:"acc" ~target_account_id:"14715"
         ~exclude_replies:true)
    |> expect_ok "account statuses"
  in
  assert (List.length statuses.statuses = 1);
  let own = capture (Mastodon.get_own_account ~account_id:"acc") |> expect_ok "own account" in
  assert (own.followers_count = 10);
  assert ((last_request ()).url = "https://mastodon.example/api/v1/accounts/verify_credentials");
  Printf.printf "✓\n"

let test_existing_readers_unchanged () =
  Printf.printf "Test: moved parsers keep the old lenient behaviour... ";
  let summary = Mastodon.parse_account_summary (Yojson.Basic.from_string {|{"display_name":""}|}) in
  assert (summary.id = "" && summary.acct = "" && summary.display_name = None);
  let cursors =
    Mastodon.parse_pagination_cursors
      [ ("LINK", {|<https://m.example/api/v1/timelines/home?max_id=5>; rel="next"|}) ]
  in
  assert (cursors.next_max_id = Some "5");
  assert (Social_mastodon_v1.visibility_to_string Social_mastodon_v1.Unlisted = "unlisted");
  Printf.printf "✓\n"

let () =
  Printf.printf "\n=== Mastodon Status/Notification/WebFinger Read Contract Tests ===\n\n";
  test_status_of_docs_example ();
  test_status_rejects_missing_required_field ();
  test_context_with_token ();
  test_context_public_and_not_found ();
  test_context_errors ();
  test_get_status_typed ();
  test_notifications_request_and_parse ();
  test_notifications_last_page_and_scope_error ();
  test_notification_type_round_trip ();
  test_account_statuses ();
  test_get_account_counts ();
  test_lookup_account ();
  test_webfinger ();
  test_webfinger_pure ();
  test_html_to_plain_text ();
  test_url_helpers ();
  test_account_wrappers ();
  test_existing_readers_unchanged ();
  Printf.printf "\n✓ Status read contract tests passed\n"
