(** Request-shape and error-mapping contract tests for the Bluesky provider.

    Every request is recorded (method, URL, headers, body) and answered by a
    per-test responder, so a test states exactly what the server says and pins
    exactly what the SDK sends. *)

type request = {
  meth : string;
  url : string;
  headers : (string * string) list;
  body : string option;
}

let requests : request list ref = ref []

let jwt_credentials =
  {
    Social_core.access_token = "alice.test";
    refresh_token = Some "eyJrefresh1";
    expires_at = None;
    auth_type = Social_core.Bearer;
    scope = None;
  }

(** The credential row as the store holds it; [update_credentials] writes it. *)
let stored_credentials = ref jwt_credentials

let json_response ?(status = 200) body =
  Ok { Social_core.status; headers = [ ("content-type", "application/json") ]; body }

let session_body =
  {|{"did":"did:plc:alice","handle":"alice.test","accessJwt":"eyJaccess","refreshJwt":"eyJrefresh2"}|}

let created_body = {|{"uri":"at://did:plc:alice/app.bsky.feed.post/new1","cid":"bafynew1"}|}

(** Answers session and createRecord calls; everything else is a 404. *)
let default_responder request =
  if String.ends_with ~suffix:"com.atproto.server.refreshSession" request.url then
    json_response session_body
  else if String.ends_with ~suffix:"com.atproto.server.createSession" request.url then
    json_response session_body
  else if String.ends_with ~suffix:"com.atproto.repo.createRecord" request.url then
    json_response created_body
  else json_response ~status:404 {|{"error":"NotFound","message":"no route"}|}

let responder = ref default_responder

let reset () =
  requests := [];
  stored_credentials := jwt_credentials;
  responder := default_responder

let handle request on_success on_error =
  requests := request :: !requests;
  match !responder request with
  | Ok response -> on_success response
  | Error message -> on_error message

module Mock_http : Social_core.HTTP_CLIENT = struct
  let get ?(headers = []) url on_success on_error =
    handle { meth = "GET"; url; headers; body = None } on_success on_error

  let post ?(headers = []) ?body url on_success on_error =
    handle { meth = "POST"; url; headers; body } on_success on_error

  let post_multipart ?(headers = []) ~parts:_ url on_success on_error =
    handle { meth = "POST"; url; headers; body = None } on_success on_error

  let put ?(headers = []) ?body url on_success on_error =
    handle { meth = "PUT"; url; headers; body } on_success on_error

  let delete ?(headers = []) url on_success on_error =
    handle { meth = "DELETE"; url; headers; body = None } on_success on_error
end

module Mock_config = struct
  module Http = Mock_http

  let get_env _ = None

  let get_credentials ~account_id:_ on_success _on_error =
    on_success !stored_credentials

  let update_credentials ~account_id:_ ~credentials on_success _on_error =
    stored_credentials := credentials;
    on_success ()

  let encrypt data on_success _on_error = on_success data
  let decrypt data on_success _on_error = on_success data

  let update_health_status ~account_id:_ ~status:_ ~error_message:_ on_success
      _on_error =
    on_success ()

  let resize_image ~data:_ ~mime_type:_ ~max_bytes:_ on_result = on_result None
end

module Self_hosted_options = struct
  include Social_bluesky_v1.Default_options

  let service_url () = "https://pds.example.org/"
end

module Default_bluesky = Social_bluesky_v1.Make (Mock_config)

module Self_hosted_bluesky =
  Social_bluesky_v1.Make_with_options (Mock_config) (Self_hosted_options)

let sent () = List.rev !requests

let fail fmt = Printf.ksprintf failwith fmt

let expect_outcome_success name = function
  | Some (Error_types.Success value)
  | Some (Error_types.Partial_success { result = value; _ }) ->
      value
  | Some (Error_types.Failure err) ->
      fail "%s: expected success, got %s" name (Error_types.error_to_string err)
  | None -> fail "%s: callback never ran" name

(* -- Service URL ----------------------------------------------------------- *)

let test_default_service_url () =
  reset ();
  Default_bluesky.invalidate_session ~account_id:"acct";
  let result = ref None in
  Default_bluesky.post_single ~account_id:"acct" ~text:"hello" ~media_urls:[]
    ~skip_enrichments:true (fun outcome -> result := Some outcome);
  ignore (expect_outcome_success "default post" !result);
  match List.map (fun r -> r.url) (sent ()) with
  | [ refresh; create ] ->
      assert (refresh = "https://bsky.social/xrpc/com.atproto.server.refreshSession");
      assert (create = "https://bsky.social/xrpc/com.atproto.repo.createRecord");
      let record_text =
        match (List.nth (sent ()) 1).body with
        | Some body ->
            Yojson.Basic.from_string body
            |> Yojson.Basic.Util.member "record"
            |> Yojson.Basic.Util.member "text"
            |> Yojson.Basic.Util.to_string
        | None -> fail "default post: createRecord had no body"
      in
      assert (record_text = "hello");
      let create_request = List.nth (sent ()) 1 in
      assert (create_request.meth = "POST");
      assert
        (List.assoc_opt "Authorization" create_request.headers
        = Some "Bearer eyJaccess")
  | urls -> fail "default post: unexpected requests %s" (String.concat ", " urls)

let test_self_hosted_service_url_for_every_call () =
  reset ();
  Self_hosted_bluesky.invalidate_session ~account_id:"acct";
  let posted = ref None in
  Self_hosted_bluesky.post_single ~account_id:"acct" ~text:"hello"
    ~media_urls:[] ~skip_enrichments:true (fun outcome -> posted := Some outcome);
  ignore (expect_outcome_success "self-hosted post" !posted);
  let read = ref None in
  Self_hosted_bluesky.get_post_thread ~account_id:"acct"
    ~post_uri:"at://did:plc:alice/app.bsky.feed.post/x" (fun r -> read := Some r);
  let urls = List.map (fun r -> r.url) (sent ()) in
  assert (List.length urls = 3);
  List.iter
    (fun url ->
      if not (String.starts_with ~prefix:"https://pds.example.org/xrpc/" url) then
        fail "self-hosted: request went to %s" url)
    urls;
  assert (Self_hosted_bluesky.service_url () = "https://pds.example.org")

(* -- atproto error mapping ------------------------------------------------- *)

let thread_error_for ~status ~body =
  reset ();
  Default_bluesky.invalidate_session ~account_id:"acct";
  (responder :=
     fun request ->
       if String.ends_with ~suffix:"com.atproto.server.refreshSession" request.url
       then json_response session_body
       else json_response ~status body);
  let result = ref None in
  Default_bluesky.get_post_thread ~account_id:"acct"
    ~post_uri:"at://did:plc:alice/app.bsky.feed.post/x" (fun r -> result := Some r);
  match !result with
  | Some (Error err) -> err
  | Some (Ok _) -> fail "expected an error for %d %s" status body
  | None -> fail "get_post_thread never completed"

let test_expired_token_is_auth_error () =
  match
    thread_error_for ~status:400
      ~body:{|{"error":"ExpiredToken","message":"Token has expired"}|}
  with
  | Error_types.Auth_error Error_types.Token_expired -> ()
  | err -> fail "ExpiredToken: got %s" (Error_types.error_to_string err)

let test_invalid_token_is_auth_error () =
  match
    thread_error_for ~status:400
      ~body:{|{"error":"InvalidToken","message":"Token could not be verified"}|}
  with
  | Error_types.Auth_error Error_types.Token_invalid -> ()
  | err -> fail "InvalidToken: got %s" (Error_types.error_to_string err)

let test_not_found_is_resource_not_found () =
  match
    thread_error_for ~status:400
      ~body:{|{"error":"NotFound","message":"Post not found: at://x"}|}
  with
  | Error_types.Resource_not_found message ->
      assert (message = "Post not found: at://x")
  | err -> fail "NotFound: got %s" (Error_types.error_to_string err)

let test_record_not_found_is_resource_not_found () =
  match
    thread_error_for ~status:400
      ~body:{|{"error":"RecordNotFound","message":"Could not locate record"}|}
  with
  | Error_types.Resource_not_found _ -> ()
  | err -> fail "RecordNotFound: got %s" (Error_types.error_to_string err)

let test_other_400_stays_api_error () =
  match
    thread_error_for ~status:400
      ~body:{|{"error":"InvalidRequest","message":"bad uri"}|}
  with
  | Error_types.Api_error { status_code = 400; message = "bad uri"; _ } -> ()
  | err -> fail "InvalidRequest: got %s" (Error_types.error_to_string err)

let test_401_without_envelope_is_auth_error () =
  match thread_error_for ~status:401 ~body:"Unauthorized" with
  | Error_types.Auth_error Error_types.Token_invalid -> ()
  | err -> fail "401: got %s" (Error_types.error_to_string err)

let test_create_record_expired_token_fails_with_auth_error () =
  reset ();
  Default_bluesky.invalidate_session ~account_id:"acct";
  (responder :=
     fun request ->
       if String.ends_with ~suffix:"com.atproto.repo.createRecord" request.url then
         json_response ~status:400 {|{"error":"ExpiredToken","message":"expired"}|}
       else default_responder request);
  let result = ref None in
  Default_bluesky.post_single ~account_id:"acct" ~text:"hello" ~media_urls:[]
    ~skip_enrichments:true (fun outcome -> result := Some outcome);
  match !result with
  | Some (Error_types.Failure (Error_types.Auth_error Error_types.Token_expired)) -> ()
  | Some (Error_types.Failure err) ->
      fail "createRecord ExpiredToken: got %s" (Error_types.error_to_string err)
  | Some (Error_types.Success _ | Error_types.Partial_success _) ->
      fail "createRecord ExpiredToken: expected failure"
  | None -> fail "post_single never completed"

(* -- Stored credentials ------------------------------------------------------ *)

let expect_refresh_row name (creds : Social_core.credentials) =
  if creds.access_token <> "did:plc:alice" then
    fail "%s: access_token %s, expected the DID" name creds.access_token;
  if creds.refresh_token <> Some "eyJrefresh2" then
    fail "%s: refresh_token is not the session refresh JWT" name;
  if creds.auth_type <> Social_core.Bearer then fail "%s: auth_type not Bearer" name;
  if creds.expires_at <> None then fail "%s: expires_at set" name

let session =
  {
    Social_bluesky_v1.Auth.did = "did:plc:alice";
    handle = "alice.test";
    access_jwt = "eyJaccess";
    refresh_jwt = "eyJrefresh2";
  }

let test_session_to_refresh_credentials_never_stores_password () =
  expect_refresh_row "session_to_refresh_credentials"
    (Social_bluesky_v1.Auth.session_to_refresh_credentials session);
  let module Auth_http = Social_bluesky_v1.Auth.Make (Mock_http) in
  expect_refresh_row "Auth.Make alias" (Auth_http.session_to_refresh_credentials session)

let test_legacy_app_password_row_is_rotated () =
  reset ();
  stored_credentials :=
    {
      Social_core.access_token = "alice@example.com";
      refresh_token = Some "abcd-efgh-ijkl-mnop";
      expires_at = None;
      auth_type = Social_core.App_password;
      scope = None;
    };
  Default_bluesky.invalidate_session ~account_id:"acct";
  let result = ref None in
  Default_bluesky.ensure_valid_token ~account_id:"acct"
    (fun jwt -> result := Some (Ok jwt))
    (fun err -> result := Some (Error err));
  (match !result with
  | Some (Ok "eyJaccess") -> ()
  | Some (Ok jwt) -> fail "rotation: unexpected access JWT %s" jwt
  | Some (Error err) -> fail "rotation: %s" (Error_types.error_to_string err)
  | None -> fail "rotation: ensure_valid_token never completed");
  (match sent () with
  | [ create ] ->
      assert (String.ends_with ~suffix:"com.atproto.server.createSession" create.url);
      let body = Yojson.Basic.from_string (Option.get create.body) in
      assert (
        Yojson.Basic.Util.(member "identifier" body |> to_string) = "alice@example.com")
  | _ -> fail "rotation: expected exactly one createSession call");
  expect_refresh_row "rotated row" !stored_credentials

let test_refresh_rewrites_row_with_did () =
  reset ();
  Default_bluesky.invalidate_session ~account_id:"acct";
  let result = ref None in
  Default_bluesky.ensure_valid_token ~account_id:"acct"
    (fun jwt -> result := Some (Ok jwt))
    (fun err -> result := Some (Error err));
  (match !result with
  | Some (Ok _) -> ()
  | Some (Error err) -> fail "refresh: %s" (Error_types.error_to_string err)
  | None -> fail "refresh: ensure_valid_token never completed");
  expect_refresh_row "refreshed row" !stored_credentials

(* -- Replies and threads ------------------------------------------------------ *)

let creates () =
  List.filter
    (fun r -> String.ends_with ~suffix:"com.atproto.repo.createRecord" r.url)
    (sent ())

let body_json request =
  match request.body with
  | Some body -> Yojson.Basic.from_string body
  | None -> fail "%s had no body" request.url

let reply_field request side field =
  Yojson.Basic.Util.(
    body_json request |> member "record" |> member "reply" |> member side
    |> member field |> to_string)

(** createRecord answers with post n for the n-th call; [fail_at] answers 500. *)
let numbered_creates ?fail_at () =
  responder :=
    fun request ->
      if String.ends_with ~suffix:"com.atproto.repo.createRecord" request.url then
        let n = List.length (creates ()) in
        if Some n = fail_at then
          json_response ~status:500 {|{"error":"InternalServerError","message":"boom"}|}
        else
          json_response
            (Printf.sprintf
               {|{"uri":"at://did:plc:alice/app.bsky.feed.post/p%d","cid":"cid%d"}|} n n)
      else default_responder request

let root_ref = { Social_bluesky_v1.uri = "at://did:plc:bob/app.bsky.feed.post/root"; cid = "rootcid" }

let parent_ref =
  { Social_bluesky_v1.uri = "at://did:plc:carol/app.bsky.feed.post/parent"; cid = "parentcid" }

let test_reply_to_post_request_shape () =
  reset ();
  Default_bluesky.invalidate_session ~account_id:"acct";
  numbered_creates ();
  let result = ref None in
  Default_bluesky.reply_to_post ~account_id:"acct"
    ~reply_to:{ Social_bluesky_v1.root = root_ref; parent = parent_ref }
    ~text:"thanks!" ~media_urls:[] ~skip_enrichments:true
    (fun outcome -> result := Some outcome);
  let post_id = expect_outcome_success "reply_to_post" !result in
  assert (post_id = "at://did:plc:alice/app.bsky.feed.post/p1|cid1");
  match creates () with
  | [ create ] ->
      assert (reply_field create "root" "uri" = root_ref.uri);
      assert (reply_field create "root" "cid" = "rootcid");
      assert (reply_field create "parent" "uri" = parent_ref.uri);
      assert (reply_field create "parent" "cid" = "parentcid");
      let json = body_json create in
      (* The repo is the session DID, not the stored login identifier. *)
      assert (Yojson.Basic.Util.(member "repo" json |> to_string) = "did:plc:alice");
      assert (
        Yojson.Basic.Util.(member "record" json |> member "text" |> to_string)
        = "thanks!")
  | _ -> fail "reply_to_post: expected exactly one createRecord"

let test_post_thread_under_reply_to () =
  reset ();
  Default_bluesky.invalidate_session ~account_id:"acct";
  numbered_creates ();
  let result = ref None in
  Default_bluesky.post_thread ~account_id:"acct" ~texts:[ "two"; "three" ]
    ~media_urls_per_post:[ []; [] ] ~skip_enrichments:true
    ~reply_to:{ Social_bluesky_v1.root = root_ref; parent = parent_ref }
    (fun outcome -> result := Some outcome);
  let thread = expect_outcome_success "post_thread ~reply_to" !result in
  assert (thread.Error_types.failed_at_index = None);
  assert (
    thread.Error_types.posted_ids
    = [ "at://did:plc:alice/app.bsky.feed.post/p1|cid1";
        "at://did:plc:alice/app.bsky.feed.post/p2|cid2" ]);
  match creates () with
  | [ first; second ] ->
      assert (reply_field first "root" "uri" = root_ref.uri);
      assert (reply_field first "parent" "uri" = parent_ref.uri);
      assert (reply_field first "parent" "cid" = "parentcid");
      assert (reply_field second "root" "uri" = root_ref.uri);
      assert (reply_field second "root" "cid" = "rootcid");
      assert (reply_field second "parent" "uri" = "at://did:plc:alice/app.bsky.feed.post/p1");
      assert (reply_field second "parent" "cid" = "cid1")
  | _ -> fail "post_thread ~reply_to: expected two createRecord calls"

let test_post_thread_without_reply_to_starts_a_thread () =
  reset ();
  Default_bluesky.invalidate_session ~account_id:"acct";
  numbered_creates ();
  let result = ref None in
  Default_bluesky.post_thread ~account_id:"acct" ~texts:[ "one"; "two" ]
    ~media_urls_per_post:[ []; [] ] ~skip_enrichments:true
    (fun outcome -> result := Some outcome);
  ignore (expect_outcome_success "post_thread" !result);
  match creates () with
  | [ first; second ] ->
      assert (Yojson.Basic.Util.(body_json first |> member "record" |> member "reply") = `Null);
      assert (reply_field second "root" "uri" = "at://did:plc:alice/app.bsky.feed.post/p1");
      assert (reply_field second "parent" "uri" = "at://did:plc:alice/app.bsky.feed.post/p1")
  | _ -> fail "post_thread: expected two createRecord calls"

let test_post_thread_partial_failure_reports_index () =
  reset ();
  Default_bluesky.invalidate_session ~account_id:"acct";
  numbered_creates ~fail_at:2 ();
  let result = ref None in
  Default_bluesky.post_thread ~account_id:"acct" ~texts:[ "one"; "two"; "three" ]
    ~media_urls_per_post:[ []; []; [] ] ~skip_enrichments:true
    (fun outcome -> result := Some outcome);
  match !result with
  | Some (Error_types.Partial_success { result = thread; _ }) ->
      assert (thread.Error_types.failed_at_index = Some 1);
      assert (thread.Error_types.posted_ids = [ "at://did:plc:alice/app.bsky.feed.post/p1|cid1" ])
  | Some (Error_types.Success _) -> fail "partial thread: expected Partial_success"
  | Some (Error_types.Failure err) ->
      fail "partial thread: got failure %s" (Error_types.error_to_string err)
  | None -> fail "partial thread: never completed"

let test_strong_ref_post_id_round_trip () =
  let id = "at://did:plc:alice/app.bsky.feed.post/p1|cid1" in
  (match Social_bluesky_v1.strong_ref_of_post_id id with
  | Some ref_ ->
      assert (ref_.Social_bluesky_v1.uri = "at://did:plc:alice/app.bsky.feed.post/p1");
      assert (ref_.Social_bluesky_v1.cid = "cid1");
      assert (Social_bluesky_v1.post_id_of_strong_ref ref_ = id)
  | None -> fail "strong_ref_of_post_id rejected a valid id");
  assert (Social_bluesky_v1.strong_ref_of_post_id "at://no-cid" = None);
  assert (Social_bluesky_v1.strong_ref_of_post_id "at://x|" = None);
  assert (Social_bluesky_v1.strong_ref_of_post_id "a|b|c" = None)

(* -- Video upload -------------------------------------------------------------- *)

let sleeps : float list ref = ref []

module Sleeping_options = struct
  include Social_bluesky_v1.Default_options

  let sleep =
    Some
      (fun ~seconds on_continue _on_error ->
        sleeps := seconds :: !sleeps;
        on_continue ())
end

module Video_bluesky =
  Social_bluesky_v1.Make_with_options (Mock_config) (Sleeping_options)

let video_url = "https://cdn.example.com/clip.mp4"
let shard = "https://morel.us-east.host.bsky.network"
let video_blob_json = {|{"$type":"blob","ref":{"$link":"bafkreivideo"},"mimeType":"video/mp4","size":4}|}

let session_with_did_doc endpoint =
  Printf.sprintf
    {|{"did":"did:plc:alice","handle":"alice.test","didDoc":{"id":"did:plc:alice","service":[{"id":"#bsky_notif","type":"Other","serviceEndpoint":"https://notif.example"},{"id":"#atproto_pds","type":"AtprotoPersonalDataServer","serviceEndpoint":"%s"}]}}|}
    endpoint

let has_suffix suffix request =
  let path = List.hd (String.split_on_char '?' request.url) in
  String.ends_with ~suffix path

(** A video service that answers each step as given. *)
let video_responder ?(session = session_with_did_doc shard)
    ?(service_auth = json_response {|{"token":"svc-token"}|})
    ?(upload = json_response {|{"jobStatus":{"jobId":"job-1","did":"did:plc:alice","state":"JOB_STATE_CREATED"}}|})
    ?(statuses = [])
    () =
  let remaining = ref statuses in
  responder :=
    fun request ->
      if request.url = video_url then
        Ok { Social_core.status = 200; headers = [ ("content-type", "video/mp4") ]; body = "\000\000\000\024ftypmp42" }
      else if has_suffix "com.atproto.server.getSession" request then json_response session
      else if has_suffix "com.atproto.server.getServiceAuth" request then service_auth
      else if has_suffix "app.bsky.video.uploadVideo" request then upload
      else if has_suffix "app.bsky.video.getJobStatus" request then
        match !remaining with
        | [ last ] -> last
        | next :: rest ->
            remaining := rest;
            next
        | [] -> json_response {|{"jobStatus":{"jobId":"job-1","did":"did:plc:alice","state":"JOB_STATE_ENCODING"}}|}
      else if has_suffix "com.atproto.repo.uploadBlob" request then
        json_response (Printf.sprintf {|{"blob":%s}|} video_blob_json)
      else default_responder request

module type POSTER = sig
  val post_single :
    account_id:string -> text:string -> media_urls:string list ->
    ?alt_texts:string option list -> ?widths:int option list ->
    ?heights:int option list -> ?skip_enrichments:bool ->
    ?validate_media_before_upload:bool ->
    (string Error_types.outcome -> unit) -> unit

  val invalidate_session : account_id:string -> unit
end

let post_video (module B : POSTER) =
  B.invalidate_session ~account_id:"acct";
  sleeps := [];
  let result = ref None in
  B.post_single ~account_id:"acct" ~text:"clip" ~media_urls:[ video_url ]
    (fun outcome -> result := Some outcome);
  !result

let find_request suffix =
  match List.filter (has_suffix suffix) (sent ()) with
  | request :: _ -> request
  | [] -> fail "no %s request" suffix

let count_requests suffix = List.length (List.filter (has_suffix suffix) (sent ()))

let embedded_video () =
  Yojson.Basic.Util.(
    body_json (find_request "com.atproto.repo.createRecord")
    |> member "record" |> member "embed")

let query_param request key = Uri.get_query_param (Uri.of_string request.url) key

let test_video_without_sleep_uses_upload_blob () =
  reset ();
  video_responder ();
  ignore (expect_outcome_success "video without sleep" (post_video (module Default_bluesky)));
  assert (count_requests "com.atproto.repo.uploadBlob" = 1);
  assert (
    (find_request "com.atproto.repo.uploadBlob").url
    = "https://bsky.social/xrpc/com.atproto.repo.uploadBlob");
  assert (count_requests "app.bsky.video.uploadVideo" = 0);
  assert (count_requests "com.atproto.server.getServiceAuth" = 0);
  let embed = embedded_video () in
  assert (Yojson.Basic.Util.(member "$type" embed |> to_string) = "app.bsky.embed.video")

let test_video_service_request_shapes () =
  reset ();
  video_responder
    ~statuses:
      [ json_response {|{"jobStatus":{"jobId":"job-1","did":"did:plc:alice","state":"JOB_STATE_ENCODING","progress":40}}|};
        json_response
          (Printf.sprintf
             {|{"jobStatus":{"jobId":"job-1","did":"did:plc:alice","state":"JOB_STATE_COMPLETED","blob":%s}}|}
             video_blob_json) ]
    ();
  ignore (expect_outcome_success "video service" (post_video (module Video_bluesky)));
  (* Service auth is minted by the account's PDS shard, for that shard. *)
  let auth = find_request "com.atproto.server.getServiceAuth" in
  assert (String.starts_with ~prefix:(shard ^ "/xrpc/") auth.url);
  assert (query_param auth "aud" = Some "did:web:morel.us-east.host.bsky.network");
  assert (query_param auth "lxm" = Some "com.atproto.repo.uploadBlob");
  (match query_param auth "exp" with
  | Some exp -> assert (int_of_string exp > int_of_float (Unix.gettimeofday ()) + 1700)
  | None -> fail "getServiceAuth without exp");
  assert (List.assoc_opt "Authorization" auth.headers = Some "Bearer eyJaccess");
  let upload = find_request "app.bsky.video.uploadVideo" in
  assert (upload.meth = "POST");
  assert (String.starts_with ~prefix:"https://video.bsky.app/xrpc/" upload.url);
  assert (query_param upload "did" = Some "did:plc:alice");
  assert (query_param upload "name" <> None);
  assert (List.assoc_opt "Authorization" upload.headers = Some "Bearer svc-token");
  assert (List.assoc_opt "Content-Type" upload.headers = Some "video/mp4");
  (* Two polls, each after a wait, and no credentials sent with them. *)
  assert (count_requests "app.bsky.video.getJobStatus" = 2);
  assert (!sleeps = [ 2.0; 2.0 ]);
  List.iter
    (fun request ->
      if has_suffix "app.bsky.video.getJobStatus" request then begin
        assert (query_param request "jobId" = Some "job-1");
        assert (List.assoc_opt "Authorization" request.headers = None)
      end)
    (sent ());
  assert (count_requests "com.atproto.repo.uploadBlob" = 0);
  let embed = embedded_video () in
  assert (Yojson.Basic.Util.(member "$type" embed |> to_string) = "app.bsky.embed.video");
  assert (
    Yojson.Basic.Util.(member "video" embed |> member "ref" |> member "$link" |> to_string)
    = "bafkreivideo")

let test_video_already_exists_uses_earlier_blob () =
  reset ();
  video_responder
    ~upload:(json_response ~status:409 {|{"error":"already_exists","message":"Video already processed","jobId":"job-0"}|})
    ~statuses:
      [ json_response ~status:400
          (Printf.sprintf
             {|{"error":"already_exists","jobStatus":{"jobId":"job-0","did":"did:plc:alice","state":"JOB_STATE_COMPLETED","blob":%s}}|}
             video_blob_json) ]
    ();
  ignore (expect_outcome_success "already_exists" (post_video (module Video_bluesky)));
  assert (query_param (find_request "app.bsky.video.getJobStatus") "jobId" = Some "job-0");
  assert (count_requests "com.atproto.repo.uploadBlob" = 0)

let test_video_job_failed_is_an_error () =
  reset ();
  video_responder
    ~statuses:
      [ json_response {|{"jobStatus":{"jobId":"job-1","did":"did:plc:alice","state":"JOB_STATE_FAILED","failureCode":"validation_failure","error":"Video too long"}}|} ]
    ();
  (match post_video (module Video_bluesky) with
  | Some (Error_types.Failure err) ->
      let message = Error_types.error_to_string err in
      if not (Re.execp (Re.compile (Re.str "validation_failure")) message) then
        fail "failed job: reason missing from %s" message
  | Some _ -> fail "failed job: expected failure"
  | None -> fail "failed job: never completed");
  assert (count_requests "com.atproto.repo.createRecord" = 0);
  assert (count_requests "com.atproto.repo.uploadBlob" = 0)

let test_video_processing_timeout () =
  reset ();
  video_responder ();
  (match post_video (module Video_bluesky) with
  | Some (Error_types.Failure _) -> ()
  | Some _ -> fail "timeout: expected failure"
  | None -> fail "timeout: never completed");
  assert (count_requests "app.bsky.video.getJobStatus" = Video_bluesky.video_job_max_polls);
  assert (count_requests "com.atproto.repo.createRecord" = 0)

let test_video_service_auth_failure_falls_back_to_upload_blob () =
  reset ();
  video_responder ~service_auth:(json_response ~status:500 {|{"error":"InternalServerError"}|}) ();
  ignore (expect_outcome_success "service auth failure" (post_video (module Video_bluesky)));
  assert (count_requests "app.bsky.video.uploadVideo" = 0);
  assert (count_requests "com.atproto.repo.uploadBlob" = 1);
  List.iter
    (fun request ->
      if String.starts_with ~prefix:"https://video.bsky.app" request.url then
        fail "a request went to the video service: %s" request.url)
    (sent ())

let test_video_token_rejected_falls_back_to_upload_blob () =
  reset ();
  video_responder ~upload:(json_response ~status:401 {|{"error":"AuthMissing","message":"bad audience"}|}) ();
  ignore (expect_outcome_success "token rejected" (post_video (module Video_bluesky)));
  assert (count_requests "app.bsky.video.uploadVideo" = 1);
  assert (count_requests "com.atproto.repo.uploadBlob" = 1)

let test_unsafe_did_doc_endpoint_is_ignored () =
  reset ();
  video_responder ~session:(session_with_did_doc "http://169.254.169.254/latest")
    ~upload:(json_response (Printf.sprintf {|{"jobStatus":{"jobId":"j","did":"did:plc:alice","state":"JOB_STATE_COMPLETED","blob":%s}}|} video_blob_json))
    ();
  ignore (expect_outcome_success "unsafe endpoint" (post_video (module Video_bluesky)));
  let auth = find_request "com.atproto.server.getServiceAuth" in
  assert (String.starts_with ~prefix:"https://bsky.social/xrpc/" auth.url);
  assert (query_param auth "aud" = Some "did:web:bsky.social");
  List.iter
    (fun request ->
      if String.starts_with ~prefix:"http://169.254" request.url then
        fail "request sent to the DID document endpoint %s" request.url)
    (sent ())

let test_pds_endpoint_of_did_doc () =
  let doc json = Yojson.Basic.from_string json in
  assert (
    Social_bluesky_v1.pds_endpoint_of_did_doc
      (Yojson.Basic.Util.member "didDoc" (doc (session_with_did_doc shard)))
    = Some shard);
  assert (Social_bluesky_v1.pds_endpoint_of_did_doc (doc {|{"service":[]}|}) = None);
  assert (Social_bluesky_v1.pds_endpoint_of_did_doc (doc {|{"id":"did:plc:x"}|}) = None);
  assert (Social_bluesky_v1.pds_endpoint_of_did_doc (doc {|"not an object"|}) = None)

let test_record_operations_use_session_did () =
  let module Config = struct
    include Mock_config
    let update_credentials ~account_id:_ ~credentials:_ _on_success on_error =
      on_error "store unavailable"
  end in
  let module B = Social_bluesky_v1.Make (Config) in
  let result = function
    | Social_core.Ok _ -> ()
    | Social_core.Error err -> failwith (Error_types.error_to_string err)
  in
  let uri = "at://did:plc:alice/app.bsky.feed.post/r" in
  List.iter (fun operation ->
    reset ();
    stored_credentials := { jwt_credentials with access_token = "alice@example.org";
      refresh_token = Some "app-password" };
    B.invalidate_session ~account_id:"acct";
    responder := (fun request ->
      if String.ends_with ~suffix:"com.atproto.repo.deleteRecord" request.url then
        json_response "{}"
      else default_responder request);
    operation ();
    assert ((!stored_credentials).access_token = "alice@example.org");
    let records = List.filter (fun request ->
      String.ends_with ~suffix:"com.atproto.repo.createRecord" request.url ||
      String.ends_with ~suffix:"com.atproto.repo.deleteRecord" request.url) (sent ()) in
    match records with
    | [ request ] ->
        let body = Yojson.Basic.from_string (Option.get request.body) in
        assert (Yojson.Basic.Util.member "repo" body = `String "did:plc:alice")
    | [] | _ :: _ -> failwith "expected one record request")
    [ (fun () -> B.delete_post ~account_id:"acct" ~post_uri:uri (function
          | Error_types.Success () | Error_types.Partial_success { result = (); _ } -> ()
          | Error_types.Failure err -> failwith (Error_types.error_to_string err)));
      (fun () -> B.like_post ~account_id:"acct" ~post_uri:uri ~post_cid:"cid" result);
      (fun () -> B.unlike_post ~account_id:"acct" ~like_uri:uri result);
      (fun () -> B.repost ~account_id:"acct" ~post_uri:uri ~post_cid:"cid" result);
      (fun () -> B.unrepost ~account_id:"acct" ~repost_uri:uri result);
      (fun () -> B.follow ~account_id:"acct" ~did:"did:plc:other" result);
      (fun () -> B.unfollow ~account_id:"acct" ~follow_uri:uri result);
      (fun () -> B.block_actor ~account_id:"acct" ~actor:"did:plc:other" result);
      (fun () -> B.unblock_actor ~account_id:"acct" ~block_uri:uri result) ]

let () =
  test_record_operations_use_session_did ();
  test_video_without_sleep_uses_upload_blob ();
  test_video_service_request_shapes ();
  test_video_already_exists_uses_earlier_blob ();
  test_video_job_failed_is_an_error ();
  test_video_processing_timeout ();
  test_video_service_auth_failure_falls_back_to_upload_blob ();
  test_video_token_rejected_falls_back_to_upload_blob ();
  test_unsafe_did_doc_endpoint_is_ignored ();
  test_pds_endpoint_of_did_doc ();
  test_reply_to_post_request_shape ();
  test_post_thread_under_reply_to ();
  test_post_thread_without_reply_to_starts_a_thread ();
  test_post_thread_partial_failure_reports_index ();
  test_strong_ref_post_id_round_trip ();
  test_session_to_refresh_credentials_never_stores_password ();
  test_legacy_app_password_row_is_rotated ();
  test_refresh_rewrites_row_with_did ();
  test_default_service_url ();
  test_self_hosted_service_url_for_every_call ();
  test_expired_token_is_auth_error ();
  test_invalid_token_is_auth_error ();
  test_not_found_is_resource_not_found ();
  test_record_not_found_is_resource_not_found ();
  test_other_400_stays_api_error ();
  test_401_without_envelope_is_auth_error ();
  test_create_record_expired_token_fails_with_auth_error ();
  print_endline "Bluesky service contract tests passed"
