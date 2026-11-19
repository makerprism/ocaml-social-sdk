(** Contract tests for {!Social_meta_graph_error}: one realistic Graph error body
    per documented code, and the SDK error each one becomes.

    The Facebook, Instagram and Threads packages all delegate to this table,
    so a change here changes account health for every Meta consumer. Each
    row cites the reason it is classified the way it is. *)

let failures = ref 0

let check name condition =
  if condition then Printf.printf "  ✓ %s\n" name
  else begin
    incr failures;
    Printf.printf "  ✗ %s\n" name
  end

let graph_body ?subcode ?error_type ?(is_transient = false) ~message code =
  let fields =
    [ Some (Printf.sprintf {|"message":%S|} message);
      Option.map (Printf.sprintf {|"type":%S|}) error_type;
      (if is_transient then Some {|"is_transient":true|} else None);
      Option.map (Printf.sprintf {|"code":%d|}) code;
      Option.map (Printf.sprintf {|"error_subcode":%d|}) subcode;
      Some {|"fbtrace_id":"AvX3kq0Lm2P"|} ]
    |> List.filter_map Fun.id
  in
  Printf.sprintf {|{"error":{%s}}|} (String.concat "," fields)

let classification body =
  Option.map Social_meta_graph_error.classify (Social_meta_graph_error.of_body body)

let show = function
  | None -> "not a Graph error"
  | Some c ->
      (match (c : Social_meta_graph_error.classification) with
       | Resource_not_found -> "Resource_not_found"
       | Invalid_request -> "Invalid_request"
       | Transient -> "Transient"
       | Rate_limited -> "Rate_limited"
       | Token_invalid _ -> "Token_invalid"
       | Permission_denied -> "Permission_denied"
       | Duplicate_content -> "Duplicate_content"
       | Unclassified -> "Unclassified")

let expect name body expected =
  let actual = classification body in
  check
    (Printf.sprintf "%s -> %s (got %s)" name (show (Some expected)) (show actual))
    (actual = Some expected)

let oauth = "OAuthException"

let test_classification_per_code () =
  Printf.printf "Test: classification per documented code\n";
  let open Social_meta_graph_error in
  (* Object ids that no longer resolve (deleted post, comment, container). *)
  expect "100/33 object missing"
    (graph_body ~subcode:33 ~error_type:"GraphMethodException"
       ~message:"Unsupported get request. Object with ID '17841400000000000' does not exist, cannot be loaded due to missing permissions, or does not support this operation. Please read the Graph API documentation at https://developers.facebook.com/docs/graph-api"
       (Some 100))
    Resource_not_found;
  expect "100 without subcode"
    (graph_body ~error_type:oauth ~message:"(#100) Param message must be a string" (Some 100))
    Invalid_request;
  expect "100/2207028 Instagram carousel size"
    (graph_body ~subcode:2207028 ~error_type:oauth
       ~message:"Your post won't work as a carousel." (Some 100))
    Invalid_request;
  expect "1 unknown"
    (graph_body ~error_type:oauth ~message:"An unknown error has occurred." (Some 1))
    Transient;
  expect "2 service"
    (graph_body ~error_type:oauth ~is_transient:true
       ~message:"An unexpected error has occurred. Please retry your request later." (Some 2))
    Transient;
  (* Instagram reuses code 1 for a validation failure, pinned by subcode. *)
  expect "1/2207057 Instagram thumbnail offset"
    (graph_body ~subcode:2207057 ~error_type:oauth
       ~message:"Thumbnail offset must be greater than or equal to 0 and less than video duration" (Some 1))
    Unclassified;
  List.iter
    (fun (code, subcode, message) ->
      expect (Printf.sprintf "%d rate limit" code)
        (graph_body ?subcode ~error_type:oauth ~message (Some code))
        Rate_limited)
    [ (4, None, "(#4) Application request limit reached");
      (4, Some 2207051, "We restrict certain activity to protect our community.");
      (9, Some 2207042, "You reached maximum number of posts that is allowed to be published by Content Publishing API.");
      (17, None, "(#17) User request limit reached");
      (32, None, "(#32) Page request limit reached");
      (341, None, "(#341) Application limit reached");
      (368, Some 1390008, "It looks like you were misusing this feature by going too fast. You've been temporarily blocked from using it.");
      (613, None, "(#613) Calls to this api have exceeded the rate limit.");
      (80000, Some 2446079, "(#80000) There have been too many calls from this ad-account.");
      (80001, None, "(#80001) There have been too many calls to this Page account. Wait a bit and try again.");
      (80002, None, "(#80002) There have been too many calls to this Instagram account. Wait a bit and try again.");
      (80014, None, "(#80014) There have been too many calls for the batch uploads to this catalog account.") ];
  expect "80015 is past the Business Use Case range"
    (graph_body ~error_type:oauth ~message:"(#80015) Unknown" (Some 80015))
    Unclassified;
  List.iter
    (fun (code, subcode, message, reason) ->
      expect (Printf.sprintf "%d/%s token" code
                (match subcode with Some s -> string_of_int s | None -> "-"))
        (graph_body ?subcode ~error_type:oauth ~message (Some code))
        (Token_invalid reason))
    [ (102, None, "Session key invalid. This could be because the session key has an incorrect format, or because the user has revoked this session", Unspecified);
      (190, None, "Invalid OAuth access token - Cannot parse access token", Unspecified);
      (190, Some 458, "Error validating access token: The user has not authorized application 1234567890.", App_not_installed);
      (190, Some 459, "Error validating access token: You cannot access the app till you log in to www.facebook.com and follow the instructions given.", User_checkpointed);
      (190, Some 460, "Error validating access token: The session has been invalidated because the user changed their password or Facebook has changed the session for security reasons.", Password_changed);
      (190, Some 463, "Error validating access token: Session has expired on Monday, 16-Nov-26 10:00:00 PST. The current time is Tuesday, 17-Nov-26 01:00:00 PST.", Expired);
      (190, Some 464, "Error validating access token: Sessions for the user are not allowed because the user is not a confirmed user.", Unconfirmed_user);
      (190, Some 467, "Error validating access token: The session is invalid because the user logged out.", Invalid_access_token);
      (190, Some 492, "Error validating access token: The user associated with this Page access token has no role on the Page.", Invalid_session);
      (190, Some 1234, "Error validating access token: unexpected subcode", Unspecified) ];
  expect "OAuthException without a code"
    (graph_body ~error_type:oauth ~message:"Error validating access token" None)
    (Token_invalid Unspecified);
  List.iter
    (fun (code, message) ->
      expect (Printf.sprintf "%d permission" code)
        (graph_body ~error_type:oauth ~message (Some code))
        Permission_denied)
    [ (3, "(#3) Application does not have the capability to make this API call.");
      (10, "(#10) Application does not have permission for this action");
      (200, "(#200) The user hasn't authorized the application to perform this action");
      (230, "(#230) Requires pages_messaging permission to manage the object");
      (299, "(#299) Requires extended permission") ];
  expect "300 is past the permission range"
    (graph_body ~error_type:oauth ~message:"(#300) Edit failure" (Some 300))
    Unclassified;
  expect "506 duplicate post"
    (graph_body ~subcode:1455006 ~error_type:oauth ~message:"Duplicate status message" (Some 506))
    Duplicate_content;
  (* Instagram labels media errors OAuthException; they are not token errors. *)
  expect "9004/2207052 OAuthException media download"
    (graph_body ~subcode:2207052 ~error_type:oauth
       ~message:"The media could not be fetched from this uri: https://example.com/a.jpg" (Some 9004))
    Unclassified;
  expect "36003/2207009 OAuthException aspect ratio"
    (graph_body ~subcode:2207009 ~error_type:oauth
       ~message:"The submitted image with aspect ratio 4:1 cannot be published." (Some 36003))
    Unclassified;
  expect "24/4279009 Threads container not ready"
    (graph_body ~subcode:4279009 ~error_type:"THApiException"
       ~message:"The requested resource does not exist" (Some 24))
    Unclassified

let test_of_body () =
  Printf.printf "Test: of_body reads the envelope and rejects others\n";
  let body =
    {|{"error":{"message":"(#4) Application request limit reached","type":"OAuthException","is_transient":true,"code":4,"error_subcode":1349210,"error_user_title":"Slow down","error_user_msg":"Try again later","fbtrace_id":"AvX3kq0Lm2P"}}|}
  in
  check "every field parsed"
    (Social_meta_graph_error.of_body body
     = Some
         { Social_meta_graph_error.code = Some 4; subcode = Some 1349210;
           error_type = Some "OAuthException";
           message = Some "(#4) Application request limit reached";
           user_title = Some "Slow down"; user_message = Some "Try again later";
           fbtrace_id = Some "AvX3kq0Lm2P"; is_transient = Some true });
  check "HTML is not a Graph error"
    (Social_meta_graph_error.of_body "<html><body>502 Bad Gateway</body></html>" = None);
  check "atproto envelope is not a Graph error"
    (Social_meta_graph_error.of_body {|{"error":"ExpiredToken","message":"Token has expired"}|} = None);
  check "success body is not a Graph error"
    (Social_meta_graph_error.of_body {|{"id":"123"}|} = None)

let to_error ?(status_code = 400) ?retry_after_seconds ?request_id ?redact_body
    ?redact_message body =
  Social_meta_graph_error.to_error ~platform:Platform_types.FacebookPage ~status_code
    ~required_permissions:[ "pages_manage_posts" ] ?retry_after_seconds
    ?request_id ?redact_body ?redact_message body

let test_to_error () =
  Printf.printf "Test: to_error builds the SDK error per class\n";
  let token subcode =
    to_error (graph_body ?subcode ~error_type:oauth ~message:"Error validating access token" (Some 190))
  in
  check "463 -> Token_expired"
    (token (Some 463) = Error_types.Auth_error Error_types.Token_expired);
  check "467 -> Token_expired"
    (token (Some 467) = Error_types.Auth_error Error_types.Token_expired);
  check "458 -> Token_revoked"
    (token (Some 458) = Error_types.Auth_error Error_types.Token_revoked);
  check "460 -> Token_revoked"
    (token (Some 460) = Error_types.Auth_error Error_types.Token_revoked);
  check "no subcode -> Token_invalid"
    (token None = Error_types.Auth_error Error_types.Token_invalid);
  check "492 -> Token_invalid"
    (token (Some 492) = Error_types.Auth_error Error_types.Token_invalid);
  check "200 -> Insufficient_permissions with the caller's list"
    (to_error (graph_body ~error_type:oauth ~message:"(#200) Permissions error" (Some 200))
     = Error_types.Auth_error
         (Error_types.Insufficient_permissions
            { required = [ "pages_manage_posts" ];
              platform_message = Some "(#200) Permissions error" }));
  check "368 -> Rate_limited with the caller's retry hint"
    (to_error ~retry_after_seconds:300
       (graph_body ~subcode:1390008 ~error_type:oauth ~message:"temporarily blocked" (Some 368))
     = Error_types.Rate_limited
         { retry_after_seconds = Some 300; limit = None; remaining = Some 0; reset_at = None });
  check "HTTP 429 with a non-Graph body -> Rate_limited"
    (to_error ~status_code:429 ~retry_after_seconds:7 "Too Many Requests"
     = Error_types.Rate_limited
         { retry_after_seconds = Some 7; limit = None; remaining = Some 0; reset_at = None });
  check "100/33 -> Resource_not_found"
    (to_error (graph_body ~subcode:33 ~error_type:"GraphMethodException" ~message:"Object does not exist" (Some 100))
     = Error_types.Resource_not_found "Object does not exist");
  check "506 -> Duplicate_content"
    (to_error (graph_body ~error_type:oauth ~message:"Duplicate status message" (Some 506))
     = Error_types.Duplicate_content);
  let invalid = graph_body ~error_type:oauth ~message:"(#100) Invalid parameter" (Some 100) in
  check "100 -> Api_error carrying body and fbtrace_id"
    (to_error invalid
     = Error_types.Api_error
         { status_code = 400; message = "(#100) Invalid parameter";
           platform = Platform_types.FacebookPage; raw_response = Some invalid;
           request_id = Some "AvX3kq0Lm2P" });
  check "explicit request_id wins over fbtrace_id"
    (match to_error ~request_id:"hdr-1" invalid with
     | Error_types.Api_error { request_id = Some "hdr-1"; _ } -> true
     | _ -> false);
  let transient =
    to_error ~status_code:500
      (graph_body ~error_type:oauth ~is_transient:true ~message:"An unexpected error has occurred." (Some 2))
  in
  check "2 with HTTP 500 -> Api_error the core classifier calls transient"
    (Error_types.classify_error transient = Error_types.Transient_failure);
  (* Status-only core helpers cannot see a transient body under a 4xx;
     of_error + classify can. The status must not be rewritten. *)
  let transient_400 =
    to_error ~status_code:400
      (graph_body ~error_type:oauth ~is_transient:true ~message:"Please retry your request later." (Some 2))
  in
  check "2 with HTTP 400 keeps its status"
    (match transient_400 with
     | Error_types.Api_error { status_code = 400; _ } -> true
     | _ -> false);
  check "2 with HTTP 400 classifies as Transient via of_error"
    (Option.map Social_meta_graph_error.classify (Social_meta_graph_error.of_error transient_400)
     = Some Social_meta_graph_error.Transient);
  check "of_error is None for non-Api_error"
    (Social_meta_graph_error.of_error (Error_types.Auth_error Error_types.Token_invalid) = None);
  let html = "<html><body>502 Bad Gateway</body></html>" in
  check "non-Graph body -> Api_error with the body as message"
    (to_error ~status_code:502 html
     = Error_types.Api_error
         { status_code = 502; message = html; platform = Platform_types.FacebookPage;
           raw_response = Some html; request_id = None });
  check "redactors apply to stored body and message"
    (match
       to_error ~redact_body:(fun _ -> "BODY") ~redact_message:(fun _ -> "MSG") invalid
     with
     | Error_types.Api_error { message = "MSG"; raw_response = Some "BODY"; _ } -> true
     | _ -> false);
  check "transport_error -> Network_error"
    (Social_meta_graph_error.transport_error "connection refused"
     = Error_types.Network_error (Error_types.Connection_failed "connection refused"))

(* The point of the taxonomy for consumers: which failures say something
   about the stored credentials. *)
let test_health_evidence () =
  Printf.printf "Test: only token and permission classes are credential evidence\n";
  let health body = Health_status.of_error (to_error body) in
  check "190/463 -> token_expired"
    (health (graph_body ~subcode:463 ~error_type:oauth ~message:"expired" (Some 190))
     = Some Health_status.Token_expired);
  check "9004 OAuthException media error -> no health write"
    (health (graph_body ~subcode:2207052 ~error_type:oauth ~message:"The media could not be fetched" (Some 9004))
     = None);
  check "368 temporarily blocked -> no health write"
    (health (graph_body ~error_type:oauth ~message:"temporarily blocked" (Some 368)) = None);
  check "transport failure -> no health write"
    (Health_status.of_error (Social_meta_graph_error.transport_error "timeout") = None)

(* The additive failure envelope keeps the classification reachable while the
   original status, codes and trace survive, and hands every prose field to
   the caller's redactor before retaining it. *)
let test_response_error () =
  Printf.printf "Test: response_error keeps detail and redacts prose\n";
  let error =
    Social_meta_graph_error.response_error ~platform:Platform_types.FacebookPage
      ~status_code:400 ~required_permissions:[] ~redact:(fun _ -> "[redacted]")
      {|{"error":{"message":"secret","code":190,"error_subcode":463,"fbtrace_id":"trace"}}|}
  in
  check "190/463 detail keeps status, code, subcode and trace"
    (error.detail.http_status = 400 && error.detail.code = Some "190"
     && error.detail.subcode = Some 463 && error.detail.trace_id = Some "trace");
  check "kind classifies as token_expired"
    (error.kind = Error_types.Auth_error Error_types.Token_expired);
  check "graph classification survives with the message redacted"
    (match error.graph with
     | Some graph ->
         graph.message = Some "[redacted]"
         && Social_meta_graph_error.classify graph
            = Social_meta_graph_error.Token_invalid Social_meta_graph_error.Expired
     | None -> false);
  check "message and body excerpt pass through the redactor"
    (error.detail.message = Some "[redacted]"
     && error.detail.body_excerpt = Some "[redacted]")

let () =
  Printf.printf "Running Social_meta_graph_error contract tests\n\n";
  test_classification_per_code ();
  test_of_body ();
  test_to_error ();
  test_health_evidence ();
  test_response_error ();
  if !failures > 0 then begin
    Printf.printf "\n%d check(s) failed\n" !failures;
    exit 1
  end
  else Printf.printf "\nAll Social_meta_graph_error checks passed\n"
