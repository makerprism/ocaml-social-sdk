(** Contract tests for the typed reads ([Social_bluesky_v1.Reads], the
    [Make_with_options] wrappers and [Social_bluesky_v1.Views]).

    Response bodies follow the app.bsky lexicons (feed.defs postView,
    threadViewPost, notFoundPost, blockedPost, feedViewPost;
    notification.listNotifications), including the [$type] tags, viewer
    state and labels the AppView sends. *)

module V = Social_bluesky_v1.Views

type request = { url : string; headers : (string * string) list }

let requests : request list ref = ref []
let responder : (request -> (Social_core.response, string) result) ref = ref (fun _ -> Error "unset")

let reset f =
  requests := [];
  responder := f

module Mock_http : Social_core.HTTP_CLIENT = struct
  let handle request on_success on_error =
    requests := request :: !requests;
    match !responder request with
    | Ok response -> on_success response
    | Error message -> on_error message

  let get ?(headers = []) url on_success on_error = handle { url; headers } on_success on_error
  let post ?(headers = []) ?body:_ url on_success on_error = handle { url; headers } on_success on_error
  let post_multipart ?headers:_ ~parts:_ url _ _ = failwith ("unexpected multipart " ^ url)
  let put ?headers:_ ?body:_ url _ _ = failwith ("unexpected PUT " ^ url)
  let delete ?headers:_ url _ _ = failwith ("unexpected DELETE " ^ url)
end

module Reads = Social_bluesky_v1.Reads.Make (Mock_http)

let json_response ?(status = 200) body =
  Ok { Social_core.status; headers = [ ("content-type", "application/json; charset=utf-8") ]; body }

let contains haystack needle =
  try
    ignore (Str.search_forward (Str.regexp_string needle) haystack 0);
    true
  with Not_found -> false

let capture call =
  let result = ref None in
  call (fun value -> result := Some value);
  match !result with Some value -> value | None -> failwith "callback not called"

let expect_ok what = function
  | Ok value -> value
  | Error err -> failwith (Printf.sprintf "%s: %s" what (Error_types.error_to_string err))

let last_request () = match !requests with r :: _ -> r | [] -> failwith "no request"

(* Every (key, value) pair of the query, in order. *)
let query_pairs url =
  Uri.query (Uri.of_string url)
  |> List.concat_map (fun (key, values) -> List.map (fun value -> (key, value)) values)

let query_value url key = List.assoc_opt key (query_pairs url)

(* {1 Fixtures} *)

let author ~did ~handle ~name =
  Printf.sprintf
    {|{"did":"%s","handle":"%s","displayName":"%s","avatar":"https://cdn.bsky.app/img/avatar/plain/%s/bafkreiavatar@jpeg","associated":{"chat":{"allowIncoming":"following"}},"viewer":{"muted":false,"blockedBy":false},"labels":[],"createdAt":"2023-04-20T10:00:00.000Z"}|}
    did handle name did

let alice = author ~did:"did:plc:alice" ~handle:"alice.bsky.social" ~name:"Alice"
let bob = author ~did:"did:plc:bob" ~handle:"bob.test" ~name:"Bob"

let post_uri did rkey = Printf.sprintf "at://%s/app.bsky.feed.post/%s" did rkey
let root_uri = post_uri "did:plc:alice" "3kroot"
let reply_uri = post_uri "did:plc:bob" "3kreply"
let nested_uri = post_uri "did:plc:alice" "3knested"

let reply_record ~parent ~parent_cid =
  Printf.sprintf
    {|,"reply":{"root":{"uri":"%s","cid":"bafyroot"},"parent":{"uri":"%s","cid":"%s"}}|}
    root_uri parent parent_cid

let post_view ?(reply = "") ?(type_tag = "") ~uri ~cid ~author ~text () =
  Printf.sprintf
    {|{%s"uri":"%s","cid":"%s","author":%s,"record":{"$type":"app.bsky.feed.post","createdAt":"2026-11-17T09:00:00.000Z","langs":["en"],"text":"%s"%s},"replyCount":1,"repostCount":0,"likeCount":4,"quoteCount":0,"bookmarkCount":0,"indexedAt":"2026-11-17T09:00:01.123Z","viewer":{"bookmarked":false,"threadMuted":false,"embeddingDisabled":false},"labels":[]}|}
    type_tag uri cid author text reply

let root_post = post_view ~uri:root_uri ~cid:"bafyroot" ~author:alice ~text:"Root post" ()

let reply_post =
  post_view ~uri:reply_uri ~cid:"bafyreply" ~author:bob ~text:"Nice!"
    ~reply:(reply_record ~parent:root_uri ~parent_cid:"bafyroot") ()

let nested_post =
  post_view ~uri:nested_uri ~cid:"bafynested" ~author:alice ~text:"Thanks"
    ~reply:(reply_record ~parent:reply_uri ~parent_cid:"bafyreply") ()

let thread_body =
  Printf.sprintf
    {|{"thread":{"$type":"app.bsky.feed.defs#threadViewPost","post":%s,"replies":[{"$type":"app.bsky.feed.defs#threadViewPost","post":%s,"replies":[{"$type":"app.bsky.feed.defs#threadViewPost","post":%s,"replies":[]}]},{"$type":"app.bsky.feed.defs#notFoundPost","uri":"at://did:plc:gone/app.bsky.feed.post/3kgone","notFound":true},{"$type":"app.bsky.feed.defs#blockedPost","uri":"at://did:plc:troll/app.bsky.feed.post/3kblk","blocked":true,"author":{"did":"did:plc:troll","viewer":{"blocking":"at://did:plc:alice/app.bsky.graph.block/3kb"}}},{"$type":"app.bsky.feed.defs#someFutureView","uri":"at://x"}],"threadContext":{}},"threadgate":null}|}
    root_post reply_post nested_post

(* {1 getPostThread} *)

let test_post_thread () =
  Printf.printf "Test: getPostThread request and recursive typed thread... ";
  reset (fun _ -> json_response thread_body);
  let thread =
    capture
      (Reads.get_post_thread ~service_url:"https://pds.example.org/" ~access_jwt:"eyJaccess"
         ~post_uri:root_uri ~depth:100 ~parent_height:0)
    |> expect_ok "thread"
  in
  let request = last_request () in
  assert (String.starts_with ~prefix:"https://pds.example.org/xrpc/app.bsky.feed.getPostThread?" request.url);
  assert (query_value request.url "uri" = Some root_uri);
  assert (query_value request.url "depth" = Some "100");
  assert (query_value request.url "parentHeight" = Some "0");
  assert (List.assoc_opt "Authorization" request.headers = Some "Bearer eyJaccess");
  (match thread with
  | V.Post { post; parent = None; replies = [ V.Post reply; V.Not_found_post gone; V.Blocked_post blocked; V.Unknown_post unknown ] } ->
      assert (post.uri = root_uri && post.text = "Root post" && post.reply = None);
      assert (post.author.handle = "alice.bsky.social");
      assert (post.author.display_name = Some "Alice");
      assert (post.like_count = Some 4);
      assert (reply.post.author.did = "did:plc:bob");
      assert (
        reply.post.reply
        = Some
            {
              V.root = { V.uri = root_uri; cid = "bafyroot" };
              parent = { V.uri = root_uri; cid = "bafyroot" };
            });
      (match reply.replies with
      | [ V.Post nested ] ->
          assert (nested.post.text = "Thanks");
          assert (
            Option.map (fun (r : V.reply_ref) -> r.parent.uri) nested.post.reply = Some reply_uri)
      | _ -> failwith "nested reply");
      assert (gone = "at://did:plc:gone/app.bsky.feed.post/3kgone");
      assert (blocked.author_did = "did:plc:troll");
      assert (unknown = "app.bsky.feed.defs#someFutureView")
  | _ -> failwith "unexpected thread shape");
  Printf.printf "✓\n"

let test_post_thread_parent_chain_and_defaults () =
  Printf.printf "Test: getPostThread parent chain, no optional params, public host... ";
  let body =
    Printf.sprintf
      {|{"thread":{"$type":"app.bsky.feed.defs#threadViewPost","post":%s,"parent":{"$type":"app.bsky.feed.defs#threadViewPost","post":%s,"parent":{"$type":"app.bsky.feed.defs#threadViewPost","post":%s}}}}|}
      nested_post reply_post root_post
  in
  reset (fun _ -> json_response body);
  let thread =
    capture (Reads.get_post_thread ~service_url:Social_bluesky_v1.Reads.public_api_url ~post_uri:nested_uri)
    |> expect_ok "thread"
  in
  let request = last_request () in
  assert (String.starts_with ~prefix:"https://public.api.bsky.app/xrpc/app.bsky.feed.getPostThread?" request.url);
  assert (query_value request.url "depth" = None && query_value request.url "parentHeight" = None);
  assert (List.assoc_opt "Authorization" request.headers = None);
  (match thread with
  | V.Post { parent = Some (V.Post { parent = Some (V.Post root); _ }); replies = []; _ } ->
      assert (root.post.uri = root_uri && root.parent = None)
  | _ -> failwith "parent chain");
  Printf.printf "✓\n"

let test_post_thread_errors () =
  Printf.printf "Test: getPostThread NotFound / ExpiredToken / malformed / network... ";
  let call () =
    capture (Reads.get_post_thread ~service_url:"https://bsky.social" ~access_jwt:"t" ~post_uri:root_uri)
  in
  reset (fun _ -> json_response ~status:400 {|{"error":"NotFound","message":"Post not found: at://did:plc:alice/app.bsky.feed.post/3kroot"}|});
  (match call () with
  | Error (Error_types.Resource_not_found msg) -> assert (contains msg "Post not found")
  | _ -> failwith "NotFound");
  reset (fun _ -> json_response ~status:400 {|{"error":"ExpiredToken","message":"Token has expired"}|});
  (match call () with
  | Error (Error_types.Auth_error Error_types.Token_expired) -> ()
  | _ -> failwith "ExpiredToken");
  reset (fun _ ->
      json_response
        {|{"thread":{"$type":"app.bsky.feed.defs#threadViewPost","post":{"uri":"at://did:plc:a/app.bsky.feed.post/1"}}}|});
  (match call () with
  | Error (Error_types.Internal_error msg) -> assert (contains msg "cid")
  | _ -> failwith "malformed");
  reset (fun _ -> Error "connection reset");
  (match call () with
  | Error (Error_types.Network_error _) -> ()
  | _ -> failwith "network");
  Printf.printf "✓\n"

(* {1 getAuthorFeed} *)

let author_feed_body =
  Printf.sprintf
    {|{"cursor":"2026-11-17T08:00:00.000Z","feed":[{"post":%s,"reason":{"$type":"app.bsky.feed.defs#reasonPin"}},{"post":%s,"reply":{"root":%s,"parent":{"$type":"app.bsky.feed.defs#notFoundPost","uri":"at://did:plc:gone/app.bsky.feed.post/3kgone","notFound":true},"grandparentAuthor":%s}},{"post":%s,"reason":{"$type":"app.bsky.feed.defs#reasonRepost","by":%s,"uri":"at://did:plc:alice/app.bsky.feed.repost/3krp","cid":"bafyrp","indexedAt":"2026-11-17T09:30:00.000Z"}}]}|}
    root_post nested_post
    (post_view ~type_tag:{|"$type":"app.bsky.feed.defs#postView",|} ~uri:root_uri ~cid:"bafyroot" ~author:alice ~text:"Root post" ())
    bob reply_post alice

let test_author_feed () =
  Printf.printf "Test: getAuthorFeed params, reasons, reply context, cursor... ";
  reset (fun _ -> json_response author_feed_body);
  let page =
    capture
      (Reads.get_author_feed ~service_url:"https://bsky.social" ~access_jwt:"t" ~actor:"did:plc:alice"
         ~limit:25 ~cursor:"c1" ~filter:V.Posts_no_replies ~include_pins:true)
    |> expect_ok "feed"
  in
  let url = (last_request ()).url in
  assert (query_value url "actor" = Some "did:plc:alice");
  assert (query_value url "limit" = Some "25");
  assert (query_value url "cursor" = Some "c1");
  assert (query_value url "filter" = Some "posts_no_replies");
  assert (query_value url "includePins" = Some "true");
  assert (page.cursor = Some "2026-11-17T08:00:00.000Z");
  (match page.feed with
  | [ pinned; reply; repost ] ->
      assert (pinned.reason = Some V.Reason_pin);
      (match reply.reply with
      | Some { root = V.Post root; parent = V.Not_found_post _; grandparent_author = Some gp } ->
          assert (root.uri = root_uri && gp.did = "did:plc:bob")
      | _ -> failwith "reply context");
      (match repost.reason with
      | Some (V.Reason_repost { by; indexed_at }) ->
          assert (by.did = "did:plc:alice" && indexed_at = "2026-11-17T09:30:00.000Z")
      | _ -> failwith "repost reason");
      assert (repost.post.author.did = "did:plc:bob")
  | _ -> failwith "three items");
  List.iter
    (fun (filter, wire) -> assert (V.author_feed_filter_to_string filter = wire))
    V.
      [
        (Posts_with_replies, "posts_with_replies"); (Posts_no_replies, "posts_no_replies");
        (Posts_with_media, "posts_with_media"); (Posts_and_author_threads, "posts_and_author_threads");
        (Posts_with_video, "posts_with_video");
      ];
  Printf.printf "✓\n"

(* {1 getPosts} *)

let test_get_posts_batches () =
  Printf.printf "Test: getPosts sends repeated uris, at most 25 per call... ";
  let uris = List.init 27 (fun i -> post_uri "did:plc:alice" (Printf.sprintf "3k%02d" i)) in
  reset (fun request ->
      let requested = List.filter_map (fun (k, v) -> if k = "uris" then Some v else None) (query_pairs request.url) in
      (* The second one does not exist any more: the AppView leaves it out. *)
      let posts =
        List.filteri (fun i _ -> i <> 1) requested
        |> List.map (fun uri -> post_view ~uri ~cid:"bafy" ~author:alice ~text:"t" ())
      in
      json_response (Printf.sprintf {|{"posts":[%s]}|} (String.concat "," posts)));
  let posts =
    capture (Reads.get_posts ~service_url:"https://bsky.social" ~access_jwt:"t" ~post_uris:uris)
    |> expect_ok "posts"
  in
  (match List.rev !requests with
  | [ first; second ] ->
      let count r = List.length (List.filter (fun (k, _) -> k = "uris") (query_pairs r.url)) in
      assert (count first = 25 && count second = 2);
      assert (not (contains first.url "%2C") && not (contains first.url ","));
      assert (String.starts_with ~prefix:"https://bsky.social/xrpc/app.bsky.feed.getPosts?uris=" first.url)
  | _ -> failwith "expected two calls");
  assert (List.length posts = 25);
  assert ((List.hd posts).like_count = Some 4);
  reset (fun _ -> failwith "no call expected");
  assert (capture (Reads.get_posts ~service_url:"https://bsky.social" ~post_uris:[]) = Ok []);
  reset (fun request ->
      if List.length !requests = 1 then json_response {|{"posts":[]}|}
      else (ignore request; Ok { Social_core.status = 429;
        headers = [("RateLimit-Reset", "120")];
        body = {|{"error":"RateLimitExceeded","message":"Rate Limit Exceeded"}|} }));
  (match capture (Reads.get_posts ~service_url:"https://bsky.social" ~post_uris:uris) with
  | Error (Error_types.Rate_limited {retry_after_seconds = Some _; _}) -> ()
  | _ -> failwith "second batch failure must retain the reset delay");
  List.iter (fun (now, expected) ->
      match Social_bluesky_v1.parse_xrpc_response_error ~now
        { Social_core.status = 429; headers = [("RateLimit-Reset", "120")]; body = "{}" } with
      | Error_types.Rate_limited {retry_after_seconds = Some delay; _} ->
          assert (delay = expected)
      | error -> failwith (Error_types.error_to_string error))
    [ 100., 20; 130., 1 ];
  Printf.printf "✓\n"

(* {1 getProfile} *)

(* profileViewDetailed as an AppView sends it (app.bsky.actor.defs). *)
let profile_body =
  {|{"did":"did:plc:alice","handle":"alice.bsky.social","displayName":"Alice","description":"Hi",
     "avatar":"https://cdn.bsky.app/img/avatar/plain/did:plc:alice/bafkreiavatar@jpeg",
     "associated":{"lists":0,"feedgens":0,"starterPacks":0,"labeler":false},
     "indexedAt":"2024-01-01T00:00:00.000Z","createdAt":"2023-04-20T10:00:00.000Z",
     "viewer":{"muted":false,"blockedBy":false},"labels":[],
     "followersCount":1520,"followsCount":310,"postsCount":4821,
     "pinnedPost":{"uri":"at://did:plc:alice/app.bsky.feed.post/3kpin","cid":"bafypin"}}|}

let test_get_profile () =
  Printf.printf "Test: getProfile typed counts, public and with a session... ";
  reset (fun _ -> json_response profile_body);
  let profile =
    capture (Reads.get_profile ~service_url:Social_bluesky_v1.Reads.public_api_url ~actor:"alice.bsky.social")
    |> expect_ok "profile"
  in
  assert (profile.V.followers_count = Some 1520);
  assert (profile.follows_count = Some 310);
  assert (profile.posts_count = Some 4821);
  assert (profile.display_name = Some "Alice" && profile.banner = None);
  let request = last_request () in
  assert (String.starts_with ~prefix:"https://public.api.bsky.app/xrpc/app.bsky.actor.getProfile?" request.url);
  assert (query_value request.url "actor" = Some "alice.bsky.social");
  assert (List.assoc_opt "Authorization" request.headers = None);
  (* Counts are optional in the lexicon. *)
  reset (fun _ -> json_response {|{"did":"did:plc:x","handle":"x.test"}|});
  let bare = capture (Reads.get_profile ~service_url:"https://bsky.social" ~access_jwt:"t" ~actor:"did:plc:x") |> expect_ok "bare" in
  assert (bare.followers_count = None && bare.posts_count = None);
  assert (List.assoc_opt "Authorization" (last_request ()).headers = Some "Bearer t");
  reset (fun _ -> json_response {|{"handle":"x.test"}|});
  (match capture (Reads.get_profile ~service_url:"https://bsky.social" ~actor:"x.test") with
  | Error (Error_types.Internal_error msg) -> assert (contains msg "did")
  | _ -> failwith "a profile without did is a parse error");
  reset (fun _ -> json_response ~status:400 {|{"error":"InvalidRequest","message":"Profile not found"}|});
  (match capture (Reads.get_profile ~service_url:"https://bsky.social" ~actor:"gone.test") with
  | Error (Error_types.Api_error _ | Error_types.Resource_not_found _) -> ()
  | _ -> failwith "an unknown actor is an error");
  Printf.printf "✓\n"

(* {1 listNotifications} *)

let notifications_body =
  Printf.sprintf
    {|{"cursor":"000123","priority":false,"seenAt":"2026-11-17T08:00:00.000Z","notifications":[{"uri":"%s","cid":"bafyreply","author":%s,"reason":"reply","reasonSubject":"%s","record":{"$type":"app.bsky.feed.post","createdAt":"2026-11-17T09:00:00.000Z","text":"Nice!","reply":{"root":{"uri":"%s","cid":"bafyroot"},"parent":{"uri":"%s","cid":"bafyroot"}}},"isRead":false,"indexedAt":"2026-11-17T09:00:01.000Z","labels":[]},{"uri":"at://did:plc:bob/app.bsky.feed.like/3klike","cid":"bafylike","author":%s,"reason":"like","reasonSubject":"%s","record":{"$type":"app.bsky.feed.like","createdAt":"2026-11-17T09:01:00.000Z","subject":{"uri":"%s","cid":"bafyroot"}},"isRead":true,"indexedAt":"2026-11-17T09:01:01.000Z"},{"uri":"at://did:plc:bob/app.bsky.graph.follow/3kf","cid":"bafyf","author":%s,"reason":"some-new-reason","record":{"$type":"app.bsky.graph.follow","subject":"did:plc:alice","createdAt":"2026-11-17T09:02:00.000Z"},"isRead":false,"indexedAt":"2026-11-17T09:02:01.000Z"}]}|}
    reply_uri bob root_uri root_uri root_uri bob root_uri root_uri bob

let test_list_notifications () =
  Printf.printf "Test: listNotifications reasons filter, cursor, typed reasons... ";
  reset (fun _ -> json_response notifications_body);
  let page =
    capture
      (Reads.list_notifications ~service_url:"https://bsky.social" ~access_jwt:"eyJaccess" ~limit:50
         ~cursor:"000200" ~reasons:[ V.Reply; V.Mention; V.Quote ])
    |> expect_ok "notifications"
  in
  let request = last_request () in
  let pairs = query_pairs request.url in
  assert (List.filter (fun (k, _) -> k = "reasons") pairs = [ ("reasons", "reply"); ("reasons", "mention"); ("reasons", "quote") ]);
  assert (List.assoc_opt "cursor" pairs = Some "000200" && List.assoc_opt "limit" pairs = Some "50");
  assert (List.assoc_opt "Authorization" request.headers = Some "Bearer eyJaccess");
  assert (page.cursor = Some "000123" && page.seen_at = Some "2026-11-17T08:00:00.000Z");
  (match page.notifications with
  | [ reply; like; other ] ->
      assert (reply.reason = V.Reply && not reply.is_read);
      assert (V.post_record_text reply.record = Some "Nice!");
      assert (
        Option.map (fun (r : V.reply_ref) -> r.parent.uri) (V.post_record_reply reply.record)
        = Some root_uri);
      assert (like.reason = V.Like && like.is_read && like.reason_subject = Some root_uri);
      assert (V.post_record_reply like.record = None);
      assert (other.reason = V.Other_notification_reason "some-new-reason")
  | _ -> failwith "three notifications");
  reset (fun _ -> json_response {|{"notifications":[]}|});
  let empty = capture (Reads.list_notifications ~service_url:"https://bsky.social" ~access_jwt:"t") |> expect_ok "empty" in
  assert (empty.notifications = [] && empty.cursor = None);
  assert (Uri.query (Uri.of_string (last_request ()).url) = []);
  Printf.printf "✓\n"

let test_notification_reason_round_trip () =
  Printf.printf "Test: notification reason wire values round-trip... ";
  List.iter
    (fun reason ->
      let wire = V.notification_reason_to_string reason in
      assert (V.notification_reason_of_string wire = reason))
    V.
      [
        Like; Repost; Follow; Mention; Reply; Quote; Starterpack_joined; Verified; Unverified;
        Like_via_repost; Repost_via_repost; Subscribed_post; Contact_match;
      ];
  assert (V.notification_reason_to_string V.Starterpack_joined = "starterpack-joined");
  Printf.printf "✓\n"

(* {1 resolveHandle} *)

let test_resolve_handle () =
  Printf.printf "Test: resolveHandle ok, HandleNotFound / InvalidRequest are not found... ";
  reset (fun _ -> json_response {|{"did":"did:plc:z72i7hdynmk6r22z27h6tvur"}|});
  let did =
    capture (Reads.resolve_handle ~service_url:Social_bluesky_v1.Reads.public_api_url ~handle:"bsky.app")
    |> expect_ok "resolve"
  in
  assert (did = "did:plc:z72i7hdynmk6r22z27h6tvur");
  let request = last_request () in
  assert (request.url = "https://public.api.bsky.app/xrpc/com.atproto.identity.resolveHandle?handle=bsky.app");
  assert (request.headers = []);
  let resolve () = capture (Reads.resolve_handle ~service_url:"https://bsky.social" ~handle:"nobody.invalid") in
  reset (fun _ -> json_response ~status:400 {|{"error":"InvalidRequest","message":"Unable to resolve handle"}|});
  (match resolve () with Error (Error_types.Resource_not_found _) -> () | _ -> failwith "InvalidRequest");
  reset (fun _ -> json_response ~status:400 {|{"error":"HandleNotFound","message":"Handle not found"}|});
  (match resolve () with Error (Error_types.Resource_not_found _) -> () | _ -> failwith "HandleNotFound");
  reset (fun _ -> json_response ~status:400 {|{"error":"InvalidRequest","message":"Error: handle must be a valid handle"}|});
  (match resolve () with
  | Error (Error_types.Api_error { status_code = 400; _ }) -> ()
  | _ -> failwith "invalid handle syntax stays an Api_error");
  reset (fun _ -> json_response ~status:502 "<html>Bad Gateway</html>");
  (match resolve () with Error (Error_types.Api_error { status_code = 502; _ }) -> () | _ -> failwith "502");
  Printf.printf "✓\n"

(* {1 Pure helpers} *)

let test_url_helpers () =
  Printf.printf "Test: rkey_of_uri and post_web_url... ";
  assert (V.rkey_of_uri root_uri = Some "3kroot");
  assert (V.rkey_of_uri "at://did:plc:alice/app.bsky.feed.post/" = None);
  assert (V.rkey_of_uri "at://did:plc:alice" = None);
  assert (V.rkey_of_uri "https://bsky.app/profile/a/post/1" = None);
  assert (
    V.post_web_url ~handle:"alice.bsky.social" ~uri:root_uri
    = Some "https://bsky.app/profile/alice.bsky.social/post/3kroot");
  assert (V.post_web_url ~handle:"alice" ~uri:"garbage" = None);
  Printf.printf "✓\n"

(* {1 Account-bound wrappers} *)

let stored_credentials =
  ref
    {
      Social_core.access_token = "alice.bsky.social";
      refresh_token = Some "eyJrefresh1";
      expires_at = None;
      auth_type = Social_core.Bearer;
      scope = None;
    }

module Mock_config = struct
  module Http = Mock_http

  let get_env _ = None
  let get_credentials ~account_id:_ on_success _ = on_success !stored_credentials

  let update_credentials ~account_id:_ ~credentials on_success _ =
    stored_credentials := credentials;
    on_success ()

  let update_health_status ~account_id:_ ~status:_ ~error_message:_ on_success _ = on_success ()
  let resize_image ~data:_ ~mime_type:_ ~max_bytes:_ on_result = on_result None
end

module Self_hosted =
  Social_bluesky_v1.Make_with_options
    (Mock_config)
    (struct
      include Social_bluesky_v1.Default_options

      let service_url () = "https://pds.example.org/"
    end)

let session_body =
  {|{"did":"did:plc:alice","handle":"alice.bsky.social","accessJwt":"eyJaccess","refreshJwt":"eyJrefresh2","active":true}|}

let wrapper_responder request =
  if contains request.url "com.atproto.server.refreshSession" then json_response session_body
  else if contains request.url "getPostThread" then json_response thread_body
  else if contains request.url "getAuthorFeed" then json_response author_feed_body
  else if contains request.url "getPosts" then json_response (Printf.sprintf {|{"posts":[%s]}|} root_post)
  else if contains request.url "listNotifications" then json_response notifications_body
  else if contains request.url "getProfile" then json_response profile_body
  else json_response ~status:404 {|{"error":"NotFound"}|}

let test_account_wrappers () =
  Printf.printf "Test: account wrappers use the configured PDS and the session... ";
  reset wrapper_responder;
  Self_hosted.invalidate_session ~account_id:"acct";
  let _ =
    capture (Self_hosted.get_post_thread_view ~account_id:"acct" ~post_uri:root_uri ~depth:100)
    |> expect_ok "thread"
  in
  (match List.rev !requests with
  | [ refresh; thread ] ->
      assert (refresh.url = "https://pds.example.org/xrpc/com.atproto.server.refreshSession");
      assert (String.starts_with ~prefix:"https://pds.example.org/xrpc/app.bsky.feed.getPostThread?" thread.url);
      assert (List.assoc_opt "Authorization" thread.headers = Some "Bearer eyJaccess")
  | _ -> failwith "refresh then thread");
  let feed =
    capture (Self_hosted.get_author_feed_page ~account_id:"acct" ~actor:"did:plc:alice" ~filter:V.Posts_with_replies)
    |> expect_ok "feed"
  in
  assert (List.length feed.feed = 3);
  let posts = capture (Self_hosted.get_posts ~account_id:"acct" ~post_uris:[ root_uri ]) |> expect_ok "posts" in
  assert (List.length posts = 1);
  let page =
    capture (Self_hosted.list_notifications_page ~account_id:"acct" ~reasons:[ V.Reply ]) |> expect_ok "notifications"
  in
  assert (List.length page.notifications = 3);
  let profile = capture (Self_hosted.get_profile_view ~account_id:"acct" ~actor:"did:plc:alice") |> expect_ok "profile" in
  assert (profile.V.posts_count = Some 4821);
  assert (String.starts_with ~prefix:"https://pds.example.org/xrpc/app.bsky.actor.getProfile?" (last_request ()).url);
  assert (List.length (List.filter (fun r -> contains r.url "refreshSession") !requests) = 1);
  Printf.printf "✓\n"

let test_existing_names_unchanged () =
  Printf.printf "Test: hoisted error parser and re-exported refs keep their names... ";
  assert (
    Self_hosted.parse_api_error ~status_code:400 ~body:{|{"error":"NotFound","message":"gone"}|}
    = Error_types.Resource_not_found "gone");
  assert (Self_hosted.atproto_error_name {|{"error":"ExpiredToken"}|} = Some "ExpiredToken");
  let ref_ : Social_bluesky_v1.strong_ref = { uri = root_uri; cid = "bafyroot" } in
  assert (Social_bluesky_v1.post_id_of_strong_ref ref_ = root_uri ^ "|bafyroot");
  Printf.printf "✓\n"

let () =
  Printf.printf "\n=== Bluesky Typed Read Contract Tests ===\n\n";
  test_post_thread ();
  test_post_thread_parent_chain_and_defaults ();
  test_post_thread_errors ();
  test_author_feed ();
  test_get_posts_batches ();
  test_get_profile ();
  test_list_notifications ();
  test_notification_reason_round_trip ();
  test_resolve_handle ();
  test_url_helpers ();
  test_account_wrappers ();
  test_existing_names_unchanged ();
  Printf.printf "\n✓ Bluesky typed read contract tests passed\n"
