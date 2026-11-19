(** Contract tests for {!Social_provider_error_detail}: one realistic error body per
    platform envelope the SDK talks to, and the fields read back out of it. *)

let failures = ref 0

let check name condition =
  if condition then Printf.printf "  ✓ %s\n" name
  else begin
    incr failures;
    Printf.printf "  ✗ %s\n" name
  end

let detail ?(status_code = 400) ?request_id body =
  Social_provider_error_detail.of_response ~status_code ?request_id body

let expect name ?status_code ?request_id body ~code ~subcode ~error_type
    ~message ~trace_id =
  let d = detail ?status_code ?request_id body in
  check (name ^ ": code") (d.code = code);
  check (name ^ ": subcode") (d.subcode = subcode);
  check (name ^ ": type") (d.error_type = error_type);
  check (name ^ ": message") (d.message = message);
  check (name ^ ": trace id") (d.trace_id = trace_id);
  check (name ^ ": excerpt is the body") (d.body_excerpt = Some body)

let test_envelopes () =
  Printf.printf "Test: every recognised envelope\n";
  expect "Meta Graph"
    {|{"error":{"message":"Error validating access token: Session has expired","type":"OAuthException","code":190,"error_subcode":463,"fbtrace_id":"AvX3kq0Lm2P"}}|}
    ~code:(Some "190") ~subcode:(Some 463) ~error_type:(Some "OAuthException")
    ~message:(Some "Error validating access token: Session has expired")
    ~trace_id:(Some "AvX3kq0Lm2P");
  expect "TikTok"
    {|{"data":{},"error":{"code":"url_ownership_unverified","message":"Please review our URL ownership verification rules.","log_id":"20261117010203AB"}}|}
    ~code:(Some "url_ownership_unverified") ~subcode:None ~error_type:None
    ~message:(Some "Please review our URL ownership verification rules.")
    ~trace_id:(Some "20261117010203AB");
  expect "Google (YouTube quota)"
    {|{"error":{"code":403,"message":"The request cannot be completed because you have exceeded your quota.","errors":[{"message":"The request cannot be completed because you have exceeded your quota.","domain":"youtube.quota","reason":"quotaExceeded"}],"status":"PERMISSION_DENIED"}}|}
    ~code:(Some "quotaExceeded") ~subcode:None ~error_type:(Some "PERMISSION_DENIED")
    ~message:(Some "The request cannot be completed because you have exceeded your quota.")
    ~trace_id:None;
  expect "AT Protocol" ~status_code:400
    {|{"error":"ExpiredToken","message":"Token has expired"}|}
    ~code:(Some "ExpiredToken") ~subcode:None ~error_type:None
    ~message:(Some "Token has expired") ~trace_id:None;
  expect "OAuth 2.0"
    {|{"error":"invalid_grant","error_description":"The provided authorization grant is invalid, expired, revoked."}|}
    ~code:(Some "invalid_grant") ~subcode:None ~error_type:None
    ~message:(Some "The provided authorization grant is invalid, expired, revoked.")
    ~trace_id:None;
  expect "OAuth 2.0 bare code" {|{"error":"invalid_client"}|}
    ~code:(Some "invalid_client") ~subcode:None ~error_type:None ~message:None
    ~trace_id:None;
  expect "Mastodon prose" ~status_code:401
    {|{"error":"The access token is invalid"}|}
    ~code:None ~subcode:None ~error_type:None
    ~message:(Some "The access token is invalid") ~trace_id:None;
  expect "X/Twitter v2 problem" ~status_code:429
    {|{"title":"Too Many Requests","detail":"Too Many Requests","type":"about:blank","status":429}|}
    ~code:(Some "Too Many Requests") ~subcode:None ~error_type:(Some "about:blank")
    ~message:(Some "Too Many Requests") ~trace_id:None;
  expect "X/Twitter errors list" ~status_code:403
    {|{"errors":[{"code":187,"message":"Status is a duplicate."}]}|}
    ~code:(Some "187") ~subcode:None ~error_type:None
    ~message:(Some "Status is a duplicate.") ~trace_id:None;
  expect "LinkedIn" ~status_code:403
    {|{"status":403,"serviceErrorCode":100,"code":"ACCESS_DENIED","message":"Not enough permissions to access: ugcPosts.CREATE.NO_VERSION"}|}
    ~code:(Some "ACCESS_DENIED") ~subcode:(Some 100) ~error_type:None
    ~message:(Some "Not enough permissions to access: ugcPosts.CREATE.NO_VERSION")
    ~trace_id:None;
  expect "LinkedIn legacy" ~status_code:401
    {|{"serviceErrorCode":65600,"message":"Invalid access token","status":401}|}
    ~code:(Some "65600") ~subcode:None ~error_type:None
    ~message:(Some "Invalid access token") ~trace_id:None;
  expect "Pinterest" ~status_code:401
    {|{"code":2,"message":"Authentication failed."}|}
    ~code:(Some "2") ~subcode:None ~error_type:None
    ~message:(Some "Authentication failed.") ~trace_id:None;
  expect "TikTok flat" ~status_code:400
    {|{"error_code":"url_ownership_unverified","error_message":"Verify URL ownership"}|}
    ~code:(Some "url_ownership_unverified") ~subcode:None ~error_type:None
    ~message:(Some "Verify URL ownership") ~trace_id:None;
  expect "Reddit status" ~status_code:403 {|{"message":"Forbidden","error":403}|}
    ~code:(Some "403") ~subcode:None ~error_type:None ~message:(Some "Forbidden")
    ~trace_id:None;
  expect "Reddit json.errors" ~status_code:200
    {|{"json":{"errors":[["SUBREDDIT_NOEXIST","that subreddit doesn't exist","sr"]]}}|}
    ~code:(Some "SUBREDDIT_NOEXIST") ~subcode:None ~error_type:None
    ~message:(Some "that subreddit doesn't exist") ~trace_id:None

let test_non_json_and_trace () =
  Printf.printf "Test: non-JSON bodies, trace id precedence, excerpt\n";
  let html = "<html><body><h1>502 Bad Gateway</h1></body></html>" in
  let d = detail ~status_code:502 html in
  check "HTML keeps status" (d.http_status = 502);
  check "HTML has no code" (d.code = None && d.message = None);
  check "HTML excerpt" (d.body_excerpt = Some html);
  check "empty body has no excerpt" ((detail "").body_excerpt = None);
  let graph = {|{"error":{"message":"x","code":1,"fbtrace_id":"BODY"}}|} in
  check "recorded request_id wins over body trace"
    ((detail ~request_id:"HEADER" graph).trace_id = Some "HEADER");
  check "body trace used without request_id"
    ((detail graph).trace_id = Some "BODY");
  (* 1023 ASCII bytes, then a 3-byte character straddling the limit. *)
  let long = String.make 1023 'a' ^ "\xe2\x82\xac" ^ String.make 50 'b' in
  check "excerpt never splits a UTF-8 sequence"
    ((detail long).body_excerpt = Some (String.make 1023 'a'));
  let exact = String.make 2000 'c' in
  check "excerpt is capped at excerpt_max_bytes"
    ((detail exact).body_excerpt
     = Some (String.make Social_provider_error_detail.excerpt_max_bytes 'c'))

let test_of_error () =
  Printf.printf "Test: of_error reads Api_error and only Api_error\n";
  let body =
    {|{"error":{"message":"(#100) Invalid parameter","type":"OAuthException","code":100,"error_subcode":2207028,"fbtrace_id":"AvX3"}}|}
  in
  let api_error =
    Error_types.make_api_error ~platform:Platform_types.Instagram ~status_code:400
      ~message:"Instagram API error" ~raw_response:body ()
  in
  (match Social_provider_error_detail.of_error api_error with
   | Some d ->
       check "Api_error code" (d.code = Some "100");
       check "Api_error subcode" (d.subcode = Some 2207028);
       check "Api_error provider message, not the SDK message"
         (d.message = Some "(#100) Invalid parameter");
       check "Api_error trace" (d.trace_id = Some "AvX3")
   | None -> check "Api_error has detail" false);
  let without_body =
    Error_types.make_api_error ~platform:Platform_types.Reddit ~status_code:400
      ~message:"Gallery posts require at least 2 images" ~request_id:"r-1" ()
  in
  check "Api_error without body keeps status and request id"
    (Social_provider_error_detail.of_error without_body
     = Some
         { Social_provider_error_detail.http_status = 400; code = None; subcode = None;
           error_type = None; message = None; trace_id = Some "r-1";
           body_excerpt = None });
  check "Auth_error has no detail"
    (Social_provider_error_detail.of_error (Error_types.Auth_error Error_types.Token_invalid) = None);
  check "Network_error has no detail"
    (Social_provider_error_detail.of_error
       (Error_types.Network_error Error_types.Timeout) = None)

(* The redactor must see the complete body before the excerpt is cut: a
   secret crossing the cut, or one appearing only after it, would otherwise
   survive in the excerpt. The redactor here replaces the whole text when it
   sees the secret, like the Facebook keyword redactor. *)
let test_redact_before_excerpt_cut () =
  Printf.printf "Test: redaction runs before the excerpt cut\n";
  let contains ~needle haystack =
    let hay_len = String.length haystack and needle_len = String.length needle in
    let rec loop i =
      i + needle_len > hay_len
      || (String.sub haystack i needle_len = needle || loop (i + 1))
    in
    loop 0
  in
  let secret = "secret-token-1234567890" in
  let redact text = if contains ~needle:secret text then "[redacted]" else text in
  let excerpt body =
    Option.value ~default:""
      (Social_provider_error_detail.of_response ~status_code:400 ~redact body).body_excerpt
  in
  (* The secret starts before byte 1024 and crosses the cut. *)
  let crossing =
    {|{"error":{"message":"|} ^ String.make 1015 'x' ^ secret ^ {|","code":190}}|}
  in
  check "crossing secret redacts the whole excerpt"
    (excerpt crossing = "[redacted]");
  (* The secret appears only after byte 1024: only a whole-body pass sees it. *)
  let after = String.make 1100 'y' ^ secret in
  check "secret after the cut redacts the whole excerpt"
    (excerpt after = "[redacted]");
  check "structured fields still parse from the unredacted body"
    ((Social_provider_error_detail.of_response ~status_code:400 ~redact crossing).code
     = Some "190")

let () =
  Printf.printf "Running Social_provider_error_detail contract tests\n\n";
  test_envelopes ();
  test_non_json_and_trace ();
  test_of_error ();
  test_redact_before_excerpt_cut ();
  if !failures > 0 then begin
    Printf.printf "\n%d check(s) failed\n" !failures;
    exit 1
  end
  else Printf.printf "\nAll Social_provider_error_detail checks passed\n"
