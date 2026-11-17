(* Contract tests for the shared Google OAuth module. *)

module Mock_http = struct
  let requests : (string * string * (string * string) list * string) list ref = ref []
  let responses : Social_core.response list ref = ref []

  let reset () =
    requests := [];
    responses := []

  let next on_success on_error =
    match !responses with
    | response :: rest ->
        responses := rest;
        on_success response
    | [] -> on_error "no mock response"

  include (struct
    let get ?(headers = []) url on_success on_error =
      requests := ("GET", url, headers, "") :: !requests;
      next on_success on_error

    let post ?(headers = []) ?(body = "") url on_success on_error =
      requests := ("POST", url, headers, body) :: !requests;
      next on_success on_error

    let put ?(headers = []) ?(body = "") url on_success on_error =
      requests := ("PUT", url, headers, body) :: !requests;
      next on_success on_error

    let delete ?(headers = []) url on_success on_error =
      requests := ("DELETE", url, headers, "") :: !requests;
      next on_success on_error

    let post_multipart ?(headers = []) ~parts:_ url on_success on_error =
      requests := ("POST_MULTIPART", url, headers, "") :: !requests;
      next on_success on_error
  end : Social_core.HTTP_CLIENT)
end

module Google = Social_google_oauth.Make (Mock_http)

let platform = Platform_types.YouTubeShorts

let test_authorization_url () =
  let url =
    Social_google_oauth.get_authorization_url ~client_id:"cid" ~redirect_uri:"https://e.com/cb"
      ~state:"st" ~code_verifier:"dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
      ~scopes:[ "openid"; "https://www.googleapis.com/auth/youtube.readonly" ]
  in
  let uri = Uri.of_string url in
  let param key = Uri.get_query_param uri key in
  assert (String.starts_with ~prefix:"https://accounts.google.com/o/oauth2/v2/auth?" url);
  assert (param "scope" = Some "openid https://www.googleapis.com/auth/youtube.readonly");
  assert (param "code_challenge" = Some "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM");
  assert (param "code_challenge_method" = Some "S256");
  assert (param "access_type" = Some "offline");
  print_endline "ok: authorization URL carries scopes and the S256 challenge"

let test_get_userinfo () =
  Mock_http.reset ();
  Mock_http.responses :=
    [ { Social_core.status = 200;
        body =
          {|{"id":"1093","email":"a@example.com","verified_email":true,"name":"A B",
             "given_name":"A","family_name":"B","picture":"https://p","locale":"en","hd":""}|};
        headers = [] } ];
  Google.get_userinfo ~platform ~access_token:"tok" (function
    | Ok info ->
        assert (info.Social_google_oauth.id = "1093");
        assert (info.email = Some "a@example.com");
        assert (info.verified_email = Some true);
        assert (info.hd = None);
        (match !Mock_http.requests with
         | [ ("GET", url, headers, _) ] ->
             assert (url = "https://www.googleapis.com/oauth2/v2/userinfo");
             assert (List.assoc_opt "Authorization" headers = Some "Bearer tok")
         | _ -> failwith "expected one GET")
    | Error err -> failwith (Error_types.error_to_string err));
  (match Social_google_oauth.parse_userinfo {|{"sub":"s1","email_verified":false}|} with
   | Ok info -> assert (info.id = "s1" && info.verified_email = Some false)
   | Error message -> failwith message);
  assert (Result.is_error (Social_google_oauth.parse_userinfo {|{"email":"x"}|}));
  print_endline "ok: get_userinfo returns the typed account"

let test_api_error () =
  let error status body =
    Social_google_oauth.api_error ~platform ~required_scopes:[ "s" ] ~status_code:status body
  in
  (match error 401 {|{"error":{"code":401,"message":"Invalid Credentials","status":"UNAUTHENTICATED"}}|} with
   | Error_types.Auth_error Error_types.Token_invalid -> ()
   | _ -> failwith "401");
  (match error 403 {|{"error":{"code":403,"message":"quota","errors":[{"reason":"quotaExceeded"}]}}|} with
   | Error_types.Rate_limited _ -> ()
   | _ -> failwith "403 quota");
  (match error 403 {|{"error":{"code":403,"message":"Request had insufficient authentication scopes.","status":"PERMISSION_DENIED"}}|} with
   | Error_types.Auth_error (Error_types.Insufficient_permissions { required = [ "s" ]; platform_message = Some _ }) -> ()
   | _ -> failwith "403 scopes");
  (match error 404 {|{"error":{"code":404,"message":"gone"}}|} with
   | Error_types.Resource_not_found "gone" -> ()
   | _ -> failwith "404");
  (match error 500 "oops" with
   | Error_types.Api_error { status_code = 500; raw_response = Some "oops"; _ } -> ()
   | _ -> failwith "500");
  Mock_http.reset ();
  Google.get_userinfo ~platform ~access_token:"tok" (function
    | Error (Error_types.Network_error _) -> ()
    | Error _ | Ok _ -> failwith "expected Network_error with no response");
  print_endline "ok: Google error envelope mapping"

let () =
  test_authorization_url ();
  test_get_userinfo ();
  test_api_error ()
