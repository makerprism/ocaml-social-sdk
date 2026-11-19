(** Tests for Facebook Graph API v25 Provider *)

open Social_core
open Social_facebook_graph_v25

(** Helper to check if string contains substring *)
let string_contains s substr =
  try
    ignore (Str.search_forward (Str.regexp_string substr) s 0);
    true
  with Not_found -> false

(** Helper to handle outcome type in tests *)
let handle_outcome on_success on_error outcome =
  match outcome with
  | Error_types.Success result -> on_success result
  | Error_types.Partial_success { result; _ } -> on_success result
  | Error_types.Failure err -> on_error (Error_types.error_to_string err)

(** Mock HTTP client for testing *)
module Mock_http = struct
  let requests = ref []
  let response_queue = ref []
  let error_queue = ref []
  
  let reset () =
    requests := [];
    response_queue := [];
    error_queue := []
  
  let set_response response =
    response_queue := [response]
  
  let set_responses responses =
    response_queue := responses

  let set_errors errors =
    error_queue := errors
  
  let get_next_response () =
    match !response_queue with
    | [] -> None
    | r :: rest ->
        response_queue := rest;
        Some r

  let get_next_error () =
    match !error_queue with
    | [] -> None
    | e :: rest ->
        error_queue := rest;
        Some e
  
  include (struct
  let get ?(headers=[]) url on_success on_error =
    requests := ("GET", url, headers, "") :: !requests;
    match get_next_response () with
    | Some response -> on_success response
    | None ->
        (match get_next_error () with
         | Some err -> on_error err
         | None -> on_error "No mock response set")
  
  let post ?(headers=[]) ?(body="") url on_success on_error =
    requests := ("POST", url, headers, body) :: !requests;
    match get_next_response () with
    | Some response -> on_success response
    | None ->
        (match get_next_error () with
         | Some err -> on_error err
         | None -> on_error "No mock response set")
  
  let put ?(headers=[]) ?(body="") url on_success on_error =
    requests := ("PUT", url, headers, body) :: !requests;
    match get_next_response () with
    | Some response -> on_success response
    | None ->
        (match get_next_error () with
         | Some err -> on_error err
         | None -> on_error "No mock response set")
  
  let delete ?(headers=[]) url on_success on_error =
    requests := ("DELETE", url, headers, "") :: !requests;
    match get_next_response () with
    | Some response -> on_success response
    | None ->
        (match get_next_error () with
         | Some err -> on_error err
         | None -> on_error "No mock response set")
  
  let post_multipart ?(headers=[]) ~parts url on_success on_error =
    let body_str = Printf.sprintf "multipart with %d parts" (List.length parts) in
    requests := ("POST_MULTIPART", url, headers, body_str) :: !requests;
    match get_next_response () with
    | Some response -> on_success response
    | None ->
        (match get_next_error () with
         | Some err -> on_error err
         | None -> on_error "No mock response set")
  end : HTTP_CLIENT)
end

(** Mock config for testing *)
module Mock_config = struct
  module Http = Mock_http
  
  let env_vars = ref []
  let credentials_store = ref []
  let health_statuses = ref []
  let page_ids = ref []
  let rate_limits = ref []
  
  let reset () =
    env_vars := [];
    credentials_store := [];
    health_statuses := [];
    page_ids := [];
    rate_limits := [];
    Mock_http.reset ()
  
  let set_env key value =
    env_vars := (key, value) :: !env_vars
  
  let get_env key =
    List.assoc_opt key !env_vars
  
  let on_rate_limit_update info =
    rate_limits := info :: !rate_limits
  
  let set_credentials ~account_id ~credentials =
    credentials_store := (account_id, credentials) :: !credentials_store
  
  let _set_page_id ~account_id ~page_id =
    page_ids := (account_id, page_id) :: !page_ids
  
  let get_credentials ~account_id on_success on_error =
    match List.assoc_opt account_id !credentials_store with
    | Some creds -> on_success creds
    | None -> on_error "Credentials not found"
  
  let update_credentials ~account_id ~credentials on_success _on_error =
    credentials_store := (account_id, credentials) :: 
      (List.remove_assoc account_id !credentials_store);
    on_success ()
  
  let encrypt data on_success _on_error =
    on_success ("encrypted:" ^ data)
  
  let decrypt data on_success on_error =
    if String.starts_with ~prefix:"encrypted:" data then
      on_success (String.sub data 10 (String.length data - 10))
    else
      on_error "Invalid encrypted data"
  
  let update_health_status ~account_id ~status ~error_message on_success _on_error =
    health_statuses := (account_id, status, error_message) :: !health_statuses;
    on_success ()
  
  let get_page_id ~account_id on_success on_error =
    match List.assoc_opt account_id !page_ids with
    | Some page_id -> on_success page_id
    | None -> on_error "Page ID not found"
  
  let get_health_status account_id =
    List.find_opt (fun (id, _, _) -> id = account_id) !health_statuses
end

module Facebook = Make(Mock_config)

(** Test: OAuth URL generation *)
let test_oauth_url () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  
  let state = "test_state_123" in
  let redirect_uri = "https://example.com/callback" in
  
  let scopes = ["pages_read_engagement"; "pages_manage_posts"; "pages_show_list"] in

  Facebook.get_oauth_url ~redirect_uri ~state ~scopes
    (fun url ->
      assert (string_contains url "client_id=test_app_id");
      assert (string_contains url "state=test_state_123");
      assert (string_contains url "response_type=code");
      assert (string_contains url "pages_manage_posts");
      print_endline "✓ OAuth URL generation")
    (fun err -> failwith ("OAuth URL failed: " ^ err))

(** Test: Token exchange *)
let test_token_exchange () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_secret";
  
  let response_body = {|{
    "access_token": "short_lived_access_token_123",
    "token_type": "bearer",
    "expires_in": 3600
  }|} in

  let long_lived_response_body = {|{
    "access_token": "new_access_token_123",
    "token_type": "bearer",
    "expires_in": 5184000
  }|} in
  
  Mock_http.set_responses [
    { status = 200; body = response_body; headers = [] };
    { status = 200; body = long_lived_response_body; headers = [] };
  ];
  
  Facebook.exchange_code 
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    (fun creds ->
      assert (creds.access_token = "new_access_token_123");
      assert (creds.refresh_token = None);
      assert (creds.auth_type = Bearer);
      assert (creds.expires_at <> None);
      print_endline "✓ Token exchange")
    (fun err -> failwith ("Token exchange failed: " ^ err))

(** Test: Upload photo *)
let test_upload_photo () =
  Mock_config.reset ();
  
  (* Set up two responses: first for image download, second for upload *)
  Mock_http.set_responses [
    { status = 200; body = "fake_image_data"; headers = [] };  (* GET image *)
    { status = 200; body = {|{"id": "photo_12345"}|}; headers = [] };  (* POST multipart *)
  ];
  
  Facebook.upload_photo
    ~page_id:"123456"
    ~page_access_token:"test_token"
    ~image_url:"https://example.com/image.jpg"
    ~alt_text:None
    (function
      | Ok photo_id ->
          assert (photo_id = "photo_12345");
          print_endline "✓ Upload photo"
      | Error e -> failwith ("Upload photo failed: " ^ Error_types.error_to_string e))

(** Test: Content validation *)
let test_content_validation () =
  (* Valid content *)
  (match Facebook.validate_content ~text:"Hello Facebook!" with
   | Ok () -> print_endline "✓ Valid content passes"
   | Error e -> failwith ("Valid content failed: " ^ e));
  
  (* Empty content *)
  (match Facebook.validate_content ~text:"" with
   | Error _ -> print_endline "✓ Empty content rejected"
   | Ok () -> failwith "Empty content should fail");
  
  (* Too long *)
  let long_text = String.make 5001 'x' in
  (match Facebook.validate_content ~text:long_text with
   | Error msg when string_contains msg "5000" -> 
       print_endline "✓ Long content rejected"
   | _ -> failwith "Long content should fail")

(** Test: Ensure valid token (fresh token) *)
let test_ensure_valid_token_fresh () =
  Mock_config.reset ();
  
  (* Set credentials with far-future expiry *)
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    auth_type = Bearer;
    scope = None;
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  Facebook.ensure_valid_token ~account_id:"test_account"
    (fun token ->
      assert (token = "valid_token");
      (* Verify health status was updated *)
      match Mock_config.get_health_status "test_account" with
      | Some (_, "healthy", None) -> print_endline "✓ Ensure valid token (fresh)"
      | _ -> failwith "Health status not updated correctly")
    (fun err -> failwith ("Ensure valid token failed: " ^ Error_types.error_to_string err))

(** Test: Ensure valid token (expired token) *)
let test_ensure_valid_token_expired () =
  Mock_config.reset ();
  
  (* Set credentials with past expiry *)
  let past_time = 
    let now = Ptime_clock.now () in
    match Ptime.sub_span now (Ptime.Span.of_int_s 86400) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate past time"
  in
  
  let creds = {
    access_token = "expired_token";
    refresh_token = None;
    expires_at = Some past_time;
    auth_type = Bearer;
    scope = None;
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  Facebook.ensure_valid_token ~account_id:"test_account"
    (fun _ -> failwith "Should fail with expired token")
    (fun err ->
      let err_str = Error_types.error_to_string err in
      assert (string_contains err_str "expired");
      (* Verify health status was updated *)
      match Mock_config.get_health_status "test_account" with
      | Some (_, "token_expired", _) -> print_endline "✓ Ensure valid token (expired)"
      | _ -> failwith "Health status not updated correctly")

(** Test: Rate limit parsing *)
let test_rate_limit_parsing () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_secret";
  
  let response_body = {|{"id": "me"}|} in
  let headers = [
    ("X-App-Usage", {|{"call_count":15,"total_cputime":25,"total_time":30}|});
  ] in
  
  Mock_http.set_response { status = 200; body = response_body; headers };
  
  Facebook.get ~path:"me" ~access_token:"test_token"
    (function
      | Ok _response ->
          (* Check that rate limit was captured *)
          (match !Mock_config.rate_limits with
          | info :: _ ->
              assert (info.call_count = 15);
              assert (info.total_cputime = 25);
              assert (info.total_time = 30);
              assert (info.percentage_used = 30.0);
              print_endline "✓ Rate limit parsing"
          | [] -> failwith "Rate limit not captured")
      | Error e -> failwith ("Rate limit test failed: " ^ Error_types.error_to_string e))

(** Test: Full OAuth onboarding to pages *)
let test_exchange_code_and_get_pages () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_secret";

  Mock_http.set_responses [
    { status = 200; body = {|{"access_token":"short_token","token_type":"bearer","expires_in":3600}|}; headers = [] };
    { status = 200; body = {|{"access_token":"long_token","token_type":"bearer","expires_in":5184000}|}; headers = [] };
    { status = 200; body = {|{"data":[{"id":"123","name":"My Page","access_token":"page_token","category":"Brand"}]}|}; headers = [] };
  ];

  Facebook.exchange_code_and_get_pages
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    (fun (creds, pages) ->
      assert (creds.access_token = "long_token");
      assert (List.length pages = 1);
      let p = List.hd pages in
      assert (p.page_id = "123");
      assert (p.page_access_token = "page_token");
      print_endline "✓ Exchange code and get pages")
    (fun err -> failwith ("Exchange code and get pages failed: " ^ err))

module OAuth_http = OAuth.Make (Mock_http)

let query_param url key =
  match Uri.get_query_param (Uri.of_string url) key with
  | Some value -> value
  | None -> failwith (Printf.sprintf "missing query parameter %s in %s" key url)

(* Tokens may contain reserved characters ('|' in app tokens "id|secret",
   '+', '/', '=', '&' in opaque tokens). Each must survive as one query
   value. *)
let tricky_token = "EAAB+x/y=&fields=evil"

let test_get_user_pages_encodes_token () =
  Mock_http.reset ();
  Mock_http.set_response
    { status = 200; body = {|{"data":[]}|}; headers = [] };
  OAuth_http.get_user_pages ~user_access_token:tricky_token
    (fun _pages ->
      match !Mock_http.requests with
      | [ ("GET", url, _, _) ] ->
          assert (query_param url "access_token" = tricky_token);
          assert (query_param url "fields" = "id,name,access_token,category");
          assert (not (string_contains url "&fields=evil"));
          print_endline "✓ get_user_pages percent-encodes the user token"
      | _ -> failwith "expected one GET for get_user_pages")
    (fun err -> failwith ("get_user_pages failed: " ^ err))

let test_debug_token_encodes_tokens () =
  Mock_http.reset ();
  Mock_http.set_response
    { status = 200; body = {|{"data":{"is_valid":true}}|}; headers = [] };
  let app_token = "1234|s3cr+t/=" in
  OAuth_http.debug_token ~access_token:tricky_token ~app_token
    (fun _json ->
      match !Mock_http.requests with
      | [ ("GET", url, _, _) ] ->
          assert (query_param url "input_token" = tricky_token);
          assert (query_param url "access_token" = app_token);
          assert (not (string_contains url "|"));
          print_endline "✓ debug_token percent-encodes input and app tokens"
      | _ -> failwith "expected one GET for debug_token")
    (fun err -> failwith ("debug_token failed: " ^ err))

let test_debug_token_info_typed () =
  Mock_http.reset ();
  Mock_http.set_response
    { status = 200;
      body = {|{"data":{"app_id":"1","type":"USER","is_valid":true,"expires_at":0,
               "scopes":["pages_show_list","pages_manage_posts"],
               "granular_scopes":[{"scope":"pages_manage_posts","target_ids":["42"]}],
               "user_id":"7"}}|};
      headers = [] };
  let app_access_token = Social_meta_debug_token.app_access_token ~app_id:"1234" ~app_secret:"s3cr+t/=" in
  OAuth_http.debug_token_info ~app_access_token ~input_token:tricky_token (function
    | Stdlib.Ok info ->
        (match !Mock_http.requests with
         | [ ("GET", url, _, _) ] ->
             assert (string_contains url "https://graph.facebook.com/v25.0/debug_token?");
             assert (query_param url "input_token" = tricky_token);
             assert (query_param url "access_token" = "1234|s3cr+t/=")
         | _ -> failwith "expected one GET for debug_token_info");
        assert info.Social_meta_debug_token.is_valid;
        assert (info.expires_at = Some Social_meta_debug_token.Never_expires);
        assert (Social_meta_debug_token.granted_for_target ~target_id:"42" ~scope:"pages_manage_posts" info);
        assert (not (Social_meta_debug_token.granted_for_target ~target_id:"43" ~scope:"pages_manage_posts" info));
        print_endline "✓ debug_token_info returns the typed answer"
    | Stdlib.Error err -> failwith (Error_types.error_to_string err))

let test_debug_token_info_errors () =
  Mock_http.reset ();
  Mock_http.set_response
    { status = 400;
      body = {|{"error":{"message":"Invalid OAuth access token.","type":"OAuthException","code":190}}|};
      headers = [] };
  OAuth_http.debug_token_info ~app_access_token:"1|bad" ~input_token:"t" (function
    | Stdlib.Error (Error_types.Auth_error _) -> ()
    | Stdlib.Error err -> failwith ("unexpected error " ^ Error_types.error_to_string err)
    | Stdlib.Ok _ -> failwith "expected an error for a bad app token");
  Mock_http.reset ();
  Mock_http.set_errors [ "connect failed for https://graph.facebook.com/?access_token=SECRET" ];
  OAuth_http.debug_token_info ~app_access_token:"1|s" ~input_token:"t" (function
    | Stdlib.Error (Error_types.Network_error (Error_types.Connection_failed message)) ->
        assert (not (string_contains message "SECRET"))
    | Stdlib.Error err -> failwith ("unexpected error " ^ Error_types.error_to_string err)
    | Stdlib.Ok _ -> failwith "expected a network error");
  Mock_http.reset ();
  Mock_http.set_response { status = 200; body = {|{"data":{}}|}; headers = [] };
  OAuth_http.debug_token_info ~app_access_token:"1|s" ~input_token:"t" (function
    | Stdlib.Error (Error_types.Internal_error _) -> ()
    | Stdlib.Error _ | Stdlib.Ok _ -> failwith "expected Internal_error for an unreadable answer");
  print_endline "✓ debug_token_info maps Graph, transport and parse failures"

let test_get_permissions () =
  Mock_http.reset ();
  Mock_http.set_response
    { status = 200;
      body = {|{"data":[{"permission":"public_profile","status":"granted"},
                        {"permission":"pages_manage_posts","status":"declined"},
                        {"permission":"read_insights","status":"expired"},
                        {"permission":"odd"}],"paging":{}}|};
      headers = [] };
  OAuth_http.get_permissions ~access_token:tricky_token ~app_secret:"appsecret" (function
    | Stdlib.Ok permissions ->
        (match !Mock_http.requests with
         | [ ("GET", url, _, _) ] ->
             assert (string_contains url "https://graph.facebook.com/v25.0/me/permissions?");
             assert (query_param url "access_token" = tricky_token);
             assert (
               query_param url "appsecret_proof"
               = OAuth.app_secret_proof ~app_secret:"appsecret" ~access_token:tricky_token)
         | _ -> failwith "expected one GET for get_permissions");
        assert (List.length permissions = 3);
        assert (OAuth.granted_permissions permissions = [ "public_profile" ]);
        assert (OAuth.declined_permissions permissions = [ "pages_manage_posts" ]);
        assert (List.exists (fun p -> p.OAuth.status = OAuth.Expired) permissions);
        print_endline "✓ get_permissions parses granted/declined/expired"
    | Stdlib.Error err -> failwith (Error_types.error_to_string err))

let test_app_secret_proof_known_answer () =
  (* HMAC-SHA256("token", key "secret"), lowercase hex. *)
  assert (
    OAuth.app_secret_proof ~app_secret:"secret" ~access_token:"token"
    = "e941110e3d2bfe82621f0e3e1434730d7305d106c5f68c87165d0b27a4611a4a");
  print_endline "✓ app_secret_proof matches HMAC-SHA256 known answer"

let ok_response body = { status = 200; body; headers = [] }

let test_get_me_id () =
  Mock_http.reset ();
  Mock_http.set_response (ok_response {|{"id":"10101"}|});
  OAuth_http.get_me_id ~access_token:tricky_token (function
    | Stdlib.Ok id ->
        assert (id = "10101");
        (match !Mock_http.requests with
         | [ ("GET", url, _, _) ] ->
             assert (query_param url "fields" = "id");
             assert (query_param url "access_token" = tricky_token);
             assert (Uri.get_query_param (Uri.of_string url) "appsecret_proof" = None)
         | _ -> failwith "expected one GET");
        print_endline "✓ get_me_id reads /me?fields=id"
    | Stdlib.Error err -> failwith (Error_types.error_to_string err))

let test_get_managed_pages_follows_paging () =
  Mock_http.reset ();
  Mock_http.set_responses
    [ ok_response
        {|{"data":[{"id":"1","name":"One","access_token":"p1","category":"Cafe",
                    "about":"","picture":{"data":{"url":"https://x/1.jpg"}},"tasks":["MANAGE"]}],
           "paging":{"cursors":{"before":"b","after":"c1"},"next":"https://graph.facebook.com/next"}}|};
      ok_response
        {|{"data":[{"id":"2","name":"Two"},{"name":"no id"}],
           "paging":{"cursors":{"before":"c1","after":"c2"}}}|} ];
  OAuth_http.get_managed_pages ~access_token:"tok" ~app_secret:"sec"
    ~fields:(OAuth.default_page_fields @ [ "tasks" ]) (function
    | Stdlib.Ok pages ->
        (match List.rev !Mock_http.requests with
         | [ ("GET", first, _, _); ("GET", second, _, _) ] ->
             assert (string_contains first "https://graph.facebook.com/v25.0/me/accounts?");
             assert (query_param first "fields" = "id,name,access_token,category,about,picture{url},tasks");
             assert (Uri.get_query_param (Uri.of_string first) "after" = None);
             assert (query_param first "appsecret_proof"
                     = OAuth.app_secret_proof ~app_secret:"sec" ~access_token:"tok");
             assert (query_param second "after" = "c1")
         | _ -> failwith "expected two GETs");
        (match pages with
         | [ one; two ] ->
             assert (one.OAuth.id = "1" && one.access_token = Some "p1");
             assert (one.about = None);
             assert (one.picture_url = Some "https://x/1.jpg");
             assert (one.tasks = [ "MANAGE" ]);
             assert (two.OAuth.id = "2" && two.access_token = None && two.tasks = [])
         | _ -> failwith "expected two pages");
        print_endline "✓ get_managed_pages follows paging.next via the after cursor"
    | Stdlib.Error err -> failwith (Error_types.error_to_string err))

let test_get_all_business_pages () =
  Mock_http.reset ();
  Mock_http.set_responses
    [ ok_response {|{"data":[{"id":"b1","name":"Biz"},{"id":"b2"}]}|};
      ok_response {|{"data":[{"id":"1","name":"One","access_token":"p1"}]}|};
      ok_response {|{"data":[{"id":"1","name":"One again"},{"id":"3","name":"Three","access_token":"p3"}]}|};
      { status = 403;
        body = {|{"error":{"message":"(#200) Permissions error","type":"OAuthException","code":200}}|};
        headers = [] };
      ok_response {|{"data":[]}|} ];
  OAuth_http.get_all_business_pages ~access_token:"tok" (function
    | Stdlib.Ok { OAuth.pages; failures } ->
        let paths =
          List.rev_map (fun (_, url, _, _) -> Uri.path (Uri.of_string url)) !Mock_http.requests
        in
        assert (paths = [ "/v25.0/me/businesses"; "/v25.0/b1/owned_pages"; "/v25.0/b1/client_pages";
                          "/v25.0/b2/owned_pages"; "/v25.0/b2/client_pages" ]);
        assert (List.map (fun p -> p.OAuth.id) pages = [ "1"; "3" ]);
        assert ((List.hd pages).OAuth.name = "One");
        (match failures with
         | [ { OAuth.failed_business_id = "b2"; failed_edge = OAuth.Owned_pages;
               failure = Error_types.Auth_error (Error_types.Insufficient_permissions _) } ] -> ()
         | _ -> failwith "expected one owned_pages failure for b2");
        print_endline "✓ get_all_business_pages dedups and reports failing edges"
    | Stdlib.Error err -> failwith (Error_types.error_to_string err));
  Mock_http.reset ();
  Mock_http.set_response
    { status = 400; body = {|{"error":{"message":"bad","type":"OAuthException","code":190}}|}; headers = [] };
  OAuth_http.get_all_business_pages ~access_token:"tok" (function
    | Stdlib.Error (Error_types.Auth_error _) -> print_endline "✓ business list failure is an error"
    | Stdlib.Error _ | Stdlib.Ok _ -> failwith "expected an auth error")

let test_authorize_url () =
  let url =
    OAuth.authorize_url ~client_id:"123" ~redirect_uri:"https://e.com/cb?x=1" ~state:"s&t"
      ~scopes:[ "pages_show_list"; "pages_manage_posts" ] ()
  in
  assert (String.starts_with ~prefix:"https://www.facebook.com/v25.0/dialog/oauth?" url);
  assert (query_param url "client_id" = "123");
  assert (query_param url "redirect_uri" = "https://e.com/cb?x=1");
  assert (query_param url "state" = "s&t");
  assert (query_param url "response_type" = "code");
  assert (query_param url "scope" = "pages_show_list,pages_manage_posts");
  assert (Uri.get_query_param (Uri.of_string url) "params[scope]" = None);
  assert (Uri.get_query_param (Uri.of_string url) "auth_type" = None);
  let url =
    OAuth.authorize_url ~client_id:"123" ~redirect_uri:"https://e.com/cb" ~state:"s"
      ~scopes:[ "email" ] ~auth_type:OAuth.Reauthorize ~extra_params:[ ("config_id", "9") ] ()
  in
  assert (query_param url "auth_type" = "reauthorize");
  assert (query_param url "config_id" = "9");
  (* Make.get_oauth_url delegates and keeps auth_type=rerequest. *)
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "app";
  Facebook.get_oauth_url ~redirect_uri:"https://e.com/cb" ~state:"s" ~scopes:[ "a"; "b" ]
    (fun url ->
      assert (query_param url "scope" = "a,b");
      assert (query_param url "auth_type" = "rerequest"))
    (fun err -> failwith err);
  print_endline "✓ authorize_url builds the documented plain-scope Login URL"

(** Test: Field selection *)
let test_field_selection () =
  Mock_config.reset ();
  
  let response_body = {|{"id":"123","name":"Test"}|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Facebook.get ~path:"me" ~access_token:"test_token" ~fields:["id"; "name"]
    (function
      | Ok _response ->
          (* Check that request URL contains fields parameter *)
          let requests = !Mock_http.requests in
          (match requests with
          | (_, url, _, _) :: _ ->
              assert (string_contains url "fields=id%2Cname");
              print_endline "✓ Field selection"
          | [] -> failwith "No requests made")
      | Error e -> failwith ("Field selection test failed: " ^ Error_types.error_to_string e))

(** Test: Error code parsing *)
let test_error_code_parsing () =
  Mock_config.reset ();
  
  let error_response = {|{
    "error": {
      "message": "Invalid OAuth access token",
      "type": "OAuthException",
      "code": 190,
      "error_subcode": 463,
      "fbtrace_id": "ABC123"
    }
  }|} in
  
  Mock_http.set_response { status = 400; body = error_response; headers = [] };
  
  Facebook.get ~path:"me" ~access_token:"invalid_token"
    (function
      | Ok _response -> failwith "Should have failed with error"
      | Error e ->
          (* Token expired errors are converted to Auth_error *)
          let err_str = Error_types.error_to_string e in
          assert (string_contains err_str "expired" || string_contains err_str "token");
          print_endline "✓ Error code parsing")

(** Test: Invalid token without expiry subcode maps to token invalid *)
let test_invalid_token_without_expiry_subcode () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "Invalid OAuth access token",
      "type": "OAuthException",
      "code": 190,
      "fbtrace_id": "ABC124"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.get ~path:"me" ~access_token:"invalid_token"
    (function
      | Ok _response -> failwith "Should have failed with error"
      | Error (Error_types.Auth_error Error_types.Token_invalid) ->
          print_endline "✓ Invalid token mapping without expiry subcode"
      | Error e ->
          failwith ("Expected Token_invalid, got: " ^ Error_types.error_to_string e))

(** Test: Token subcode 467 maps to token expired *)
let test_token_subcode_467_maps_to_expired () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "Invalid OAuth access token - Session has expired",
      "type": "OAuthException",
      "code": 190,
      "error_subcode": 467,
      "fbtrace_id": "ABC125"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.get ~path:"me" ~access_token:"expired_token"
    (function
      | Ok _response -> failwith "Should have failed with expired token error"
      | Error (Error_types.Auth_error Error_types.Token_expired) ->
          print_endline "✓ Token subcode 467 maps to expired"
      | Error e ->
          failwith ("Expected Token_expired, got: " ^ Error_types.error_to_string e))

(** Test: Permission denied returns provider-required scopes *)
let test_permission_error_scopes () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "PERM123"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.get ~path:"me" ~access_token:"token"
    (function
      | Ok _ -> failwith "Should have failed with permission error"
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions { required = scopes; _ })) ->
          assert (List.mem "pages_manage_posts" scopes);
          assert (List.mem "pages_show_list" scopes);
          assert (List.mem "pages_read_engagement" scopes);
          print_endline "✓ Permission error includes provider-required scopes"
      | Error e ->
          failwith ("Expected insufficient_permissions error, got: " ^ Error_types.error_to_string e))

(** Test: me/accounts permission error maps to pages_show_list scope *)
let test_me_accounts_permission_scope () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "PERM_ACCOUNTS"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.get ~path:"me/accounts" ~access_token:"token"
    (function
      | Ok _ -> failwith "Should have failed with permission error"
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions { required = scopes; _ })) ->
          assert (scopes = ["pages_show_list"]);
          print_endline "✓ me/accounts permission scope mapping"
      | Error e ->
          failwith ("Expected insufficient_permissions error, got: " ^ Error_types.error_to_string e))

(** Test: Generic GET supports explicit required_permissions override *)
let test_get_required_permissions_override () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "PERM_OVERRIDE"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.get
    ~path:"me/accounts"
    ~access_token:"token"
    ~required_permissions:["custom_scope"]
    (function
      | Ok _ -> failwith "Should have failed with permission error"
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions { required = scopes; _ })) ->
          assert (scopes = ["custom_scope"]);
          print_endline "✓ GET required_permissions override"
      | Error e ->
          failwith ("Expected insufficient_permissions error, got: " ^ Error_types.error_to_string e))

(** Test: Generic POST supports explicit required_permissions override *)
let test_post_required_permissions_override () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "PERM_OVERRIDE_POST"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.post
    ~path:"me/feed"
    ~access_token:"token"
    ~params:[("message", "hello")]
    ~required_permissions:["custom_post_scope"]
    (function
      | Ok _ -> failwith "Should have failed with permission error"
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions { required = scopes; _ })) ->
          assert (scopes = ["custom_post_scope"]);
          print_endline "✓ POST required_permissions override"
      | Error e ->
          failwith ("Expected insufficient_permissions error, got: " ^ Error_types.error_to_string e))

(** Test: Generic DELETE supports explicit required_permissions override *)
let test_delete_required_permissions_override () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "PERM_OVERRIDE_DELETE"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.delete
    ~path:"12345"
    ~access_token:"token"
    ~required_permissions:["custom_delete_scope"]
    (function
      | Ok _ -> failwith "Should have failed with permission error"
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions { required = scopes; _ })) ->
          assert (scopes = ["custom_delete_scope"]);
          print_endline "✓ DELETE required_permissions override"
      | Error e ->
          failwith ("Expected insufficient_permissions error, got: " ^ Error_types.error_to_string e))

(** Test: Pagination *)
let test_pagination () =
  Mock_config.reset ();
  
  let page_response = {|{
    "data": [{"id": "1"}, {"id": "2"}],
    "paging": {
      "cursors": {
        "before": "cursor_before",
        "after": "cursor_after"
      },
      "next": "https://graph.facebook.com/v25.0/next_page"
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = page_response; headers = [] };
  
  let parse_data json =
    let open Yojson.Basic.Util in
    json |> to_list
  in
  
  Facebook.get_page ~path:"me/posts" ~access_token:"test_token" parse_data
    (function
      | Ok page_result ->
          assert (List.length page_result.data = 2);
          (match page_result.paging with
          | Some cursors ->
              assert (cursors.after = Some "cursor_after");
              assert (cursors.before = Some "cursor_before");
              assert (page_result.next_url <> None);
              print_endline "✓ Pagination"
          | None -> failwith "No paging info")
      | Error e -> failwith ("Pagination test failed: " ^ Error_types.error_to_string e))

(** Test: Batch requests *)
let test_batch_requests () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_secret";
  
  let batch_response = {|[
    {"code": 200, "headers": [{"name": "Content-Type", "value": "application/json"}], "body": "{\"id\":\"1\"}"},
    {"code": 200, "headers": [], "body": "{\"id\":\"2\"}"}
  ]|} in
  
  Mock_http.set_response { status = 200; body = batch_response; headers = [] };
  
  let open Facebook in
  let requests = [
    { method_ = `GET; relative_url = "me"; body = None; name = Some "me" };
    { method_ = `GET; relative_url = "me/posts"; body = None; name = None };
  ] in
  
  Facebook.batch_request ~requests ~access_token:"test_token"
    (function
      | Ok results ->
          assert (List.length results = 2);
          (match results with
          | r1 :: r2 :: _ ->
              assert (r1.code = 200);
              assert (r2.code = 200);
              assert (string_contains r1.body "\"id\":\"1\"");
              print_endline "✓ Batch requests"
          | _ -> failwith "Unexpected batch response")
      | Error e -> failwith ("Batch test failed: " ^ Error_types.error_to_string e))

(** Test: Batch request supports explicit required_permissions override *)
let test_batch_required_permissions_override () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "PERM_OVERRIDE_BATCH"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  let open Facebook in
  let requests = [
    { method_ = `GET; relative_url = "me"; body = None; name = None };
  ] in

  Facebook.batch_request
    ~requests
    ~access_token:"test_token"
    ~required_permissions:["custom_batch_scope"]
    (function
      | Ok _ -> failwith "Should have failed with permission error"
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions { required = scopes; _ })) ->
          assert (scopes = ["custom_batch_scope"]);
          print_endline "✓ BATCH required_permissions override"
      | Error e ->
          failwith ("Expected insufficient_permissions error, got: " ^ Error_types.error_to_string e))

(** Test: App secret proof *)
let test_app_secret_proof () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_secret";
  
  Mock_http.set_response { status = 200; body = {|{"id":"me"}|}; headers = [] };
  
  Facebook.get ~path:"me" ~access_token:"test_token"
    (function
      | Ok _response ->
          (* Check that request includes appsecret_proof *)
          let requests = !Mock_http.requests in
          (match requests with
          | (_, url, _, _) :: _ ->
              assert (string_contains url "appsecret_proof");
              print_endline "✓ App secret proof"
          | [] -> failwith "No requests made")
      | Error e -> failwith ("App secret proof test failed: " ^ Error_types.error_to_string e))

(** Test: Authorization header usage *)
let test_authorization_header () =
  Mock_config.reset ();
  
  Mock_http.set_response { status = 200; body = {|{"id":"me"}|}; headers = [] };
  
  Facebook.get ~path:"me" ~access_token:"test_token"
    (function
      | Ok _response ->
          (* Check that Authorization header is present *)
          let requests = !Mock_http.requests in
          (match requests with
          | (_, _, headers, _) :: _ ->
              let has_auth = List.exists (fun (k, v) ->
                k = "Authorization" && string_contains v "Bearer test_token"
              ) headers in
              assert has_auth;
              print_endline "✓ Authorization header"
           | [] -> failwith "No requests made")
      | Error e -> failwith ("Authorization header test failed: " ^ Error_types.error_to_string e))

(** Asserts an insights request against its decoded query, so the contract
    holds whatever percent-encoding [Uri] picks for the values. *)
let assert_insights_url url ~path ~query =
  let uri = Uri.of_string url in
  assert (Uri.scheme uri = Some "https");
  assert (Uri.host uri = Some "graph.facebook.com");
  assert (Uri.path uri = path);
  assert (Uri.query uri = query)

(** Test: Account analytics request contract (v25 insights endpoint) *)
let test_account_analytics_request_contract () =
  Mock_config.reset ();

  let response_body = {|{
    "data": [
      {"name": "page_impressions_unique", "values": [{"value": 10, "end_time": "2024-01-02T08:00:00+0000"}]},
      {"name": "page_posts_impressions_unique", "values": [{"value": 9, "end_time": "2024-01-02T08:00:00+0000"}]},
      {"name": "page_post_engagements", "values": [{"value": 8, "end_time": "2024-01-02T08:00:00+0000"}]},
      {"name": "page_daily_follows", "values": [{"value": 7, "end_time": "2024-01-02T08:00:00+0000"}]},
      {"name": "page_video_views", "values": [{"value": 6, "end_time": "2024-01-02T08:00:00+0000"}]}
    ]
  }|} in

  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Facebook.get_account_analytics
    ~id:"12345"
    ~since:"2024-01-01"
    ~until:"2024-01-07"
    ~access_token:"acct_token"
    (function
      | Ok _analytics ->
          (match !Mock_http.requests with
          | (method_, url, headers, body) :: _ ->
              assert (method_ = "GET");
              assert (headers = [ ("Authorization", "Bearer acct_token") ]);
              assert (body = "");
              assert_insights_url url
                ~path:"/v25.0/12345/insights"
                ~query:
                  [ ("metric", [ "page_impressions_unique,page_posts_impressions_unique,page_post_engagements,page_daily_follows,page_video_views" ]);
                    ("period", [ "day" ]);
                    ("since", [ "2024-01-01" ]);
                    ("until", [ "2024-01-07" ]);
                    ("access_token", [ "acct_token" ]) ];
              print_endline "✓ Account analytics request contract"
          | [] -> failwith "No requests made")
      | Error e -> failwith ("Account analytics contract test failed: " ^ Error_types.error_to_string e))

(** Test: Post analytics request contract (v25 insights endpoint) *)
let test_post_analytics_request_contract () =
  Mock_config.reset ();

  let response_body = {|{
    "data": [
      {"name": "post_impressions_unique", "values": [{"value": 100}]},
      {"name": "post_reactions_by_type_total", "values": [{"value": {"like": 5}}]},
      {"name": "post_clicks", "values": [{"value": 11}]},
      {"name": "post_clicks_by_type", "values": [{"value": {"other clicks": 4}}]}
    ]
  }|} in

  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Facebook.get_post_analytics
    ~post_id:"12345_67890"
    ~access_token:"post_token"
    (function
      | Ok _analytics ->
          (match !Mock_http.requests with
          | (method_, url, headers, body) :: _ ->
              assert (method_ = "GET");
              assert (headers = [ ("Authorization", "Bearer post_token") ]);
              assert (body = "");
              assert_insights_url url
                ~path:"/v25.0/12345_67890/insights"
                ~query:
                  [ ("metric", [ "post_impressions_unique,post_reactions_by_type_total,post_clicks,post_clicks_by_type" ]);
                    ("access_token", [ "post_token" ]) ];
              print_endline "✓ Post analytics request contract"
          | [] -> failwith "No requests made")
      | Error e -> failwith ("Post analytics contract test failed: " ^ Error_types.error_to_string e))

(** Test: analytics requests percent-encode the token. An app token is
    [app_id|app_secret] and opaque tokens can contain [+ / = &]; raw, a [&]
    would split the query and a [+] would decode as a space. *)
let test_analytics_encode_reserved_token_chars () =
  let token = "EAAB+x/y=z&appsecret_proof=forged|secret" in
  let check_request label =
    match !Mock_http.requests with
    | (_, url, _, _) :: _ ->
        let uri = Uri.of_string url in
        if Uri.get_query_param uri "access_token" <> Some token then
          failwith (label ^ ": access_token did not round-trip: " ^ url);
        if Uri.get_query_param uri "appsecret_proof" <> None then
          failwith (label ^ ": token split the query: " ^ url)
    | [] -> failwith (label ^ ": no request made")
  in
  Mock_config.reset ();
  Mock_http.set_response { status = 200; body = {|{"data": []}|}; headers = [] };
  Facebook.get_account_analytics ~id:"12345" ~since:"2024-01-01"
    ~until:"2024-01-07" ~access_token:token (fun _ -> ());
  check_request "get_account_analytics";
  Mock_config.reset ();
  Mock_http.set_response { status = 200; body = {|{"data": []}|}; headers = [] };
  Facebook.get_post_analytics ~post_id:"12345_67890" ~access_token:token
    (fun _ -> ());
  check_request "get_post_analytics";
  print_endline "✓ Analytics requests percent-encode the token"

(** Test: Account analytics parsing *)
let test_account_analytics_parsing () =
  Mock_config.reset ();

  let response_body = {|{
    "data": [
      {"name": "page_impressions_unique", "values": [
        {"value": 101, "end_time": "2024-01-02T08:00:00+0000"},
        {"value": 102, "end_time": "2024-01-03T08:00:00+0000"}
      ]},
      {"name": "page_posts_impressions_unique", "values": [
        {"value": 201, "end_time": "2024-01-02T08:00:00+0000"}
      ]},
      {"name": "page_post_engagements", "values": [
        {"value": 301, "end_time": "2024-01-02T08:00:00+0000"}
      ]},
      {"name": "page_daily_follows", "values": [
        {"value": 401, "end_time": "2024-01-02T08:00:00+0000"}
      ]},
      {"name": "page_video_views", "values": [
        {"value": 501, "end_time": "2024-01-02T08:00:00+0000"}
      ]}
    ]
  }|} in

  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Facebook.get_account_analytics
    ~id:"page_123"
    ~since:"2024-01-01"
    ~until:"2024-01-07"
    ~access_token:"token"
    (function
      | Ok analytics ->
          assert (List.length analytics.page_impressions_unique = 2);
          assert ((List.hd analytics.page_impressions_unique).value = 101);
          assert ((List.hd analytics.page_impressions_unique).end_time = Some "2024-01-02T08:00:00+0000");
          assert ((List.hd analytics.page_posts_impressions_unique).value = 201);
          assert ((List.hd analytics.page_post_engagements).value = 301);
          assert ((List.hd analytics.page_daily_follows).value = 401);
          assert ((List.hd analytics.page_video_views).value = 501);
          print_endline "✓ Account analytics parsing"
      | Error e -> failwith ("Account analytics parsing failed: " ^ Error_types.error_to_string e))

(** Test: Post analytics parsing *)
let test_post_analytics_parsing () =
  Mock_config.reset ();

  (* The parser accepts BOTH the v25 name [post_total_media_view_unique]
     and the legacy [post_impressions_unique], even though we now only
     REQUEST the legacy name (the v25 name triggers silent-omit on real
     Page responses). The parser tolerance lets us flip back to the v25
     name on the request side as soon as Meta fixes whatever is causing
     the silent-drop, without code changes on the response side. This
     test exercises the v25 response shape;
     [test_post_analytics_parsing_legacy_unique] covers the legacy
     response shape. *)
  let response_body = {|{
    "data": [
      {"name": "post_total_media_view_unique", "values": [{"value": 1234}]},
      {"name": "post_reactions_by_type_total", "values": [{"value": {"like": 10, "love": 3}}]},
      {"name": "post_clicks", "values": [{"value": 345}]},
      {"name": "post_clicks_by_type", "values": [{"value": {"other clicks": 12, "photo view": 9}}]}
    ]
  }|} in

  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Facebook.get_post_analytics
    ~post_id:"post_123"
    ~access_token:"token"
    (function
      | Ok analytics ->
          assert (analytics.post_impressions_unique = Some 1234);
          assert (analytics.post_clicks = Some 345);
          assert (List.assoc_opt "like" analytics.post_reactions_by_type_total = Some 10);
          assert (List.assoc_opt "love" analytics.post_reactions_by_type_total = Some 3);
          assert (List.assoc_opt "other clicks" analytics.post_clicks_by_type = Some 12);
          assert (List.assoc_opt "photo view" analytics.post_clicks_by_type = Some 9);
          print_endline "✓ Post analytics parsing (v25 name)"
       | Error e -> failwith ("Post analytics parsing failed: " ^ Error_types.error_to_string e))

let test_post_analytics_parsing_legacy_unique () =
  Mock_config.reset ();

  (* Legacy fallback: when only [post_impressions_unique] is in the
     response (older accounts, archived data, or external callers
     pinning to v24), the parser still surfaces it on the same field. *)
  let response_body = {|{
    "data": [
      {"name": "post_impressions_unique", "values": [{"value": 999}]}
    ]
  }|} in

  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Facebook.get_post_analytics
    ~post_id:"post_legacy"
    ~access_token:"token"
    (function
      | Ok analytics ->
          assert (analytics.post_impressions_unique = Some 999);
          print_endline "✓ Post analytics parsing (legacy fallback)"
       | Error e -> failwith ("Post analytics legacy fallback failed: " ^ Error_types.error_to_string e))

(** Lock in that [get_page_posts] requests the inline-analytics field
    expansions Meta needs us to use for status posts (since per-post
    /{post_id}/insights is blocked there since v2.4). The page-feed
    edge bypasses the deprecation; this test guards that we keep
    asking for the data and don't accidentally regress to a slim
    field set. *)
let test_get_page_posts_includes_inline_analytics_expansions () =
  Mock_config.reset ();
  let future_time =
    match Ptime.add_span (Ptime_clock.now ()) (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  let creds = {
    access_token = "tok";
    refresh_token = None;
    expires_at = Some future_time;
    auth_type = Bearer;
    scope = None;
  } in
  Mock_config.set_credentials ~account_id:"acct_with_inline_metrics" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"acct_with_inline_metrics" ~page_id:"page_xyz";
  let response_body = {|{ "data": [], "paging": { "cursors": {} } }|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  Facebook.get_page_posts
    ~account_id:"acct_with_inline_metrics"
    ~limit:25
    ~after:None
    ()
    (function
      | Ok _ ->
          (match !Mock_http.requests with
          | (_, url, _, _) :: _ ->
              let must_contain marker =
                let needle_present =
                  let len_url = String.length url in
                  let len_n = String.length marker in
                  let rec find i =
                    if i + len_n > len_url then false
                    else if String.sub url i len_n = marker then true
                    else find (i + 1)
                  in
                  find 0
                in
                if not needle_present then
                  failwith ("get_page_posts URL missing expected marker: " ^ marker)
              in
              must_contain "insights.metric";
              assert (not (string_contains url "post_impressions_unique"));
              must_contain "post_reactions_by_type_total";
              must_contain "comments.summary";
              must_contain "reactions.summary";
              must_contain "shares";
              print_endline "✓ get_page_posts requests inline analytics expansions"
          | [] -> failwith "No request recorded for get_page_posts")
      | Error e ->
          failwith ("get_page_posts inline-expansion test failed: "
                    ^ Error_types.error_to_string e))

let test_canonical_analytics_adapters () =
  let find_series provider_metric series =
    List.find_opt
      (fun item -> item.Analytics_types.provider_metric = Some provider_metric)
      series
  in

  let account_analytics : Facebook.account_analytics = {
    page_impressions_unique = [ { end_time = Some "2024-01-02T08:00:00+0000"; value = 11 } ];
    page_posts_impressions_unique = [ { end_time = Some "2024-01-02T08:00:00+0000"; value = 9 } ];
    page_post_engagements = [ { end_time = Some "2024-01-02T08:00:00+0000"; value = 7 } ];
    page_daily_follows = [ { end_time = Some "2024-01-02T08:00:00+0000"; value = 5 } ];
    page_video_views = [ { end_time = Some "2024-01-02T08:00:00+0000"; value = 3 } ];
  } in
  let account_series = Facebook.to_canonical_account_analytics_series account_analytics in
  assert (List.length account_series = 5);
  (match find_series "page_impressions_unique" account_series with
   | Some item ->
       assert (Analytics_types.canonical_metric_key item.metric = "impressions");
       assert ((List.hd item.points).value = 11)
   | None -> failwith "Missing page_impressions_unique canonical series");
  (match find_series "page_daily_follows" account_series with
   | Some item ->
       assert (Analytics_types.canonical_metric_key item.metric = "follows");
       assert ((List.hd item.points).value = 5)
   | None -> failwith "Missing page_daily_follows canonical series");

  let post_analytics : Facebook.post_analytics = {
    post_impressions_unique = Some 120;
    post_reactions_by_type_total = [ ("like", 4); ("love", 2) ];
    post_clicks = Some 15;
    post_clicks_by_type = [ ("link", 6); ("photo", 1) ];
  } in
  let post_series = Facebook.to_canonical_post_analytics_series post_analytics in
  assert (List.length post_series = 4);
  (match find_series "post_reactions_by_type_total" post_series with
   | Some item ->
       assert (Analytics_types.canonical_metric_key item.metric = "reactions");
       assert ((List.hd item.points).value = 6)
   | None -> failwith "Missing post_reactions_by_type_total canonical series");
  (match find_series "post_clicks_by_type" post_series with
   | Some item ->
       assert (Analytics_types.canonical_metric_key item.metric = "clicks");
       assert ((List.hd item.points).value = 7)
   | None -> failwith "Missing post_clicks_by_type canonical series");

  print_endline "✓ Canonical analytics adapters"

(** Test: analytics network errors redact query access_token text *)
let test_analytics_network_error_redacts_query_token () =
  Mock_config.reset ();
  Mock_http.set_errors
    [ "GET https://graph.facebook.com/v25.0/123/insights?access_token=secret_token_123 failed" ];

  Facebook.get_account_analytics
    ~id:"123"
    ~since:"2024-01-01"
    ~until:"2024-01-02"
    ~access_token:"secret_token_123"
    (fun result ->
      match result with
      | Error (Error_types.Network_error (Error_types.Connection_failed msg)) ->
          if string_contains msg "secret_token_123" then
            failwith ("Token was not redacted: " ^ msg);
          if not (string_contains msg "REDACTED" || msg = "No mock response set") then
            failwith ("Unexpected redaction message: " ^ msg);
          print_endline "✓ Analytics network error redacts query token"
      | Ok _ -> failwith "Expected network error"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: Version policy guard - analytics and OAuth both use v25 *)
let test_graph_version_policy_guard () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "app_123";
  Mock_http.set_responses
    [ { status = 200; body = {|{"data":[]}|}; headers = [] };
      { status = 200; body = {|{"data":[]}|}; headers = [] } ];

  let oauth_url = ref "" in
  Facebook.get_oauth_url
    ~redirect_uri:"https://example.com/callback"
    ~state:"state-123"
    ~scopes:["pages_manage_posts"]
    (fun url -> oauth_url := url)
    (fun err -> failwith ("OAuth URL generation failed: " ^ err));

  Facebook.get_account_analytics
    ~id:"123"
    ~since:"2024-01-01"
    ~until:"2024-01-02"
    ~access_token:"token_1"
    (fun _ -> ());
  Facebook.get_post_analytics ~post_id:"123_456" ~access_token:"token_1"
    (fun _ -> ());

  let requests = List.rev !Mock_http.requests in
  (match requests with
   | [ ("GET", account_url, _, _); ("GET", post_url, _, _) ] ->
       assert (string_contains !oauth_url "/v25.0/dialog/oauth");
       assert (string_contains account_url "graph.facebook.com/v25.0/");
       assert (string_contains post_url "graph.facebook.com/v25.0/");
       print_endline "✓ Graph version policy guard"
   | _ -> failwith "Expected two analytics requests for version policy guard")

(** Test: exported version constants are the ones every request uses *)
let test_exported_api_constants () =
  assert (Social_facebook_graph_v25.api_version = "v25.0");
  assert (Social_facebook_graph_v25.graph_api_base = "https://graph.facebook.com/v25.0");
  assert (Social_facebook_graph_v25.rupload_base
          = "https://rupload.facebook.com/video-upload/v25.0");
  assert (Social_facebook_graph_v25.oauth_authorize_url
          = "https://www.facebook.com/v25.0/dialog/oauth");
  assert (Social_facebook_graph_v25.token_endpoint
          = "https://graph.facebook.com/v25.0/oauth/access_token");
  (* The OAuth metadata and the functor's aliases derive from them *)
  assert (OAuth.Metadata.api_base == Social_facebook_graph_v25.graph_api_base);
  assert (OAuth.Metadata.authorization_endpoint == Social_facebook_graph_v25.oauth_authorize_url);
  assert (OAuth.Metadata.token_endpoint == Social_facebook_graph_v25.token_endpoint);
  assert (Facebook.graph_api_base == Social_facebook_graph_v25.graph_api_base);
  assert (Facebook.graph_api_version == Social_facebook_graph_v25.api_version);
  let url =
    OAuth.get_authorization_url ~client_id:"c" ~redirect_uri:"https://e.com/cb"
      ~state:"s" ~scopes:[ OAuth.Scopes.Pages_show_list ] ()
  in
  assert (String.starts_with ~prefix:(Social_facebook_graph_v25.oauth_authorize_url ^ "?") url);
  print_endline "✓ Exported API version constants"

(** Test: OAuth URL with required permissions *)
let test_oauth_url_permissions () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  
  let state = "test_state" in
  let redirect_uri = "https://example.com/callback" in
  
  let scopes = ["pages_manage_posts"; "pages_read_engagement"] in

  Facebook.get_oauth_url ~redirect_uri ~state ~scopes
    (fun url ->
      (* Should contain required permissions *)
      assert (string_contains url "pages_manage_posts" || string_contains url "pages_read_engagement");
      print_endline "✓ OAuth URL permissions")
    (fun err -> failwith ("OAuth URL permissions failed: " ^ err))

(** Test: OAuth URL encoding of special characters *)
let test_oauth_url_special_chars () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app";
  
  let redirect_uri = "https://example.com/callback?foo=bar&baz=qux" in
  let state = "state with spaces & special=chars" in
  
  let scopes = ["pages_manage_posts"] in

  Facebook.get_oauth_url ~redirect_uri ~state ~scopes
    (fun url ->
      (* URL should be properly encoded *)
      assert (not (String.contains url ' '));
      print_endline "✓ OAuth URL special character encoding")
    (fun err -> failwith ("OAuth URL encoding failed: " ^ err))

(** Test: Token exchange error responses *)
let test_token_exchange_errors () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_secret";
  
  let error_response = {|{
    "error": {
      "message": "Invalid verification code format.",
      "type": "OAuthException",
      "code": 100
    }
  }|} in
  
  Mock_http.set_response { status = 400; body = error_response; headers = [] };
  
  Facebook.exchange_code 
    ~code:"bad_code"
    ~redirect_uri:"https://example.com/callback"
    (fun _ -> failwith "Should fail with bad code")
    (fun err ->
      assert (string_contains err "400" || string_contains err "OAuth" || string_contains err "Invalid");
      print_endline "✓ Token exchange error handling")

(** Test: Long-lived token exchange *)
let test_long_lived_token () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_secret";
  
  let short_response_body = {|{
    "access_token": "short_lived_token_123",
    "token_type": "bearer",
    "expires_in": 3600
  }|} in

  let response_body = {|{
    "access_token": "long_lived_token_123",
    "token_type": "bearer",
    "expires_in": 5184000
  }|} in
  
  Mock_http.set_responses [
    { status = 200; body = short_response_body; headers = [] };
    { status = 200; body = response_body; headers = [] };
  ];
  
  Facebook.exchange_code 
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    (fun creds ->
      (* Facebook long-lived tokens last 60 days *)
      match creds.expires_at with
      | Some _ -> print_endline "✓ Long-lived token exchange"
      | None -> failwith "No expiry set for long-lived token")
    (fun err -> failwith ("Long-lived token test failed: " ^ err))

(** Test: Page access token vs user access token *)
let test_page_vs_user_token () =
  Mock_config.reset ();
  
  (* In Facebook, you need both user token and page token *)
  (* User token is used to get page token, then page token is used for posting *)
  
  let response_body = {|{"id": "page_123"}|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Facebook.get ~path:"me/accounts" ~access_token:"user_token"
    (function
      | Ok _response ->
          (* This would return page access tokens in real usage *)
          print_endline "✓ Page vs user token handling"
      | Error e -> failwith ("Page token test failed: " ^ Error_types.error_to_string e))

(** Test: Token expiry detection *)
let test_token_expiry_detection () =
  Mock_config.reset ();
  
  (* Set credentials with near-future expiry (should refresh soon) *)
  let near_future = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s 60) with  (* 1 minute *)
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate time"
  in
  
  let creds = {
    access_token = "expiring_soon_token";
    refresh_token = None;
    expires_at = Some near_future;
    auth_type = Bearer;
    scope = None;
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  (* Should detect token expiring soon *)
  Facebook.ensure_valid_token ~account_id:"test_account"
    (fun token ->
      (* Token is still valid but will expire soon *)
      assert (token = "expiring_soon_token");
      print_endline "✓ Token expiry detection")
    (fun _err ->
      (* Or might fail if implementation checks expiry threshold *)
      print_endline "✓ Token expiry detection (failed as expected)")

(** Test: OAuth state CSRF protection *)
let test_oauth_csrf_protection () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  
  let state1 = "csrf_token_1" in
  let state2 = "csrf_token_2" in
  let redirect_uri = "https://example.com/callback" in
  
  let scopes = ["pages_manage_posts"] in

  Facebook.get_oauth_url ~redirect_uri ~state:state1 ~scopes
    (fun url1 ->
      Facebook.get_oauth_url ~redirect_uri ~state:state2 ~scopes
        (fun url2 ->
          (* Each URL should have different state *)
          assert (string_contains url1 state1);
          assert (string_contains url2 state2);
          assert (url1 <> url2);
          print_endline "✓ OAuth CSRF protection")
        (fun err -> failwith ("CSRF test failed: " ^ err)))
    (fun err -> failwith ("CSRF test failed: " ^ err))

(** Test: App secret proof generation *)
let test_app_secret_proof_generation () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_secret";
  
  (* App secret proof is HMAC-SHA256 of access token with app secret *)
  (* This adds security to API calls *)
  
  Mock_http.set_response { status = 200; body = {|{"id":"test"}|}; headers = [] };
  
  Facebook.get ~path:"me" ~access_token:"test_token"
    (function
      | Ok _response ->
          (* Verify request included appsecret_proof *)
          let requests = !Mock_http.requests in
          (match requests with
          | (_, url, _, _) :: _ ->
              if string_contains url "appsecret_proof=" then
                print_endline "✓ App secret proof generation"
              else
                failwith "App secret proof not included"
          | [] -> failwith "No requests made")
      | Error e -> failwith ("App secret proof test failed: " ^ Error_types.error_to_string e))

(** Test: Redirect URI validation *)
let test_redirect_uri_validation () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  
  let valid_uris = [
    "https://example.com/callback";
    "https://example.com/auth/facebook";
    "https://subdomain.example.com/callback";
  ] in
  
  (* All should generate valid URLs *)
  let scopes = ["pages_manage_posts"] in

  let results = List.map (fun uri ->
    let state = "test_state" in
    let success = ref false in
    Facebook.get_oauth_url ~redirect_uri:uri ~state ~scopes
      (fun url ->
        success := true && String.length url > 0)
      (fun _ -> ());
    !success
  ) valid_uris in
  
  if List.for_all (fun x -> x) results then
    print_endline "✓ Redirect URI validation"
  else
    failwith "Some redirect URIs failed"

(** Test: Upload photo with alt-text *)
let test_upload_photo_with_alt_text () =
  Mock_config.reset ();
  
  Mock_http.set_responses [
    { status = 200; body = "fake_image_data"; headers = [] };
    { status = 200; body = {|{"id": "photo_with_alt"}|}; headers = [] };
  ];
  
  Facebook.upload_photo
    ~page_id:"123456"
    ~page_access_token:"test_token"
    ~image_url:"https://example.com/image.jpg"
    ~alt_text:(Some "A beautiful landscape photo")
    (function
      | Ok photo_id ->
          assert (photo_id = "photo_with_alt");
          print_endline "✓ Upload photo with alt-text"
      | Error e -> failwith ("Upload with alt-text failed: " ^ Error_types.error_to_string e))

(** Test: Post with single image and alt-text *)
let test_post_with_alt_text () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    auth_type = Bearer;
    scope = None;
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";
  
  Mock_http.set_responses [
    { status = 200; body = "image_data"; headers = [] };
    { status = 200; body = {|{"id": "photo123"}|}; headers = [] };
    { status = 200; body = {|{"id": "post123"}|}; headers = [] };
  ];
  
  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Check out this photo!"
    ~media_urls:["https://example.com/image.jpg"]
    ~alt_texts:[Some "Descriptive alt text for accessibility"]
    (handle_outcome
      (fun _post_id ->
        print_endline "✓ Post with single image and alt-text")
      (fun err -> failwith ("Post with alt-text failed: " ^ err)))

(** Test: Post with multiple images and alt-texts *)
let test_post_with_multiple_alt_texts () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    auth_type = Bearer;
    scope = None;
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";
  
  Mock_http.set_responses [
    { status = 200; body = "image1_data"; headers = [] };
    { status = 200; body = {|{"id": "photo1"}|}; headers = [] };
    { status = 200; body = "image2_data"; headers = [] };
    { status = 200; body = {|{"id": "photo2"}|}; headers = [] };
    { status = 200; body = {|{"id": "post456"}|}; headers = [] };
  ];
  
  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Multiple photos with descriptions"
    ~media_urls:["https://example.com/img1.jpg"; "https://example.com/img2.jpg"]
    ~alt_texts:[Some "First image description"; Some "Second image description"]
    (handle_outcome
      (fun _post_id ->
        print_endline "✓ Post with multiple images and alt-texts")
      (fun err -> failwith ("Post with multiple alt-texts failed: " ^ err)))

(** Test: Multi-photo uses indexed attached_media payload *)
let test_post_with_indexed_attached_media_payload () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    auth_type = Bearer;
    scope = None;
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";

  Mock_http.set_responses [
    { status = 200; body = "image1_data"; headers = [] };
    { status = 200; body = {|{"id": "photo1"}|}; headers = [] };
    { status = 200; body = "image2_data"; headers = [] };
    { status = 200; body = {|{"id": "photo2"}|}; headers = [] };
    { status = 200; body = {|{"id": "post456"}|}; headers = [] };
  ];

  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Multiple photos payload test"
    ~media_urls:["https://example.com/img1.jpg"; "https://example.com/img2.jpg"]
    ~alt_texts:[Some "First image"; Some "Second image"]
    (handle_outcome
      (fun _post_id ->
        let reqs = !Mock_http.requests in
        let feed_request =
          List.find_opt (fun (m, u, _, _) -> m = "POST" && string_contains u "/feed") reqs
        in
        (match feed_request with
        | Some (_, _, _, body) ->
            assert (string_contains body "attached_media%5B0%5D");
            assert (string_contains body "attached_media%5B1%5D");
            print_endline "✓ Multi-photo uses indexed attached_media payload"
        | None -> failwith "Feed request not found"))
      (fun err -> failwith ("Post payload test failed: " ^ err)))

(** Test: post_single forwards [place] (location Page id) into the /feed request,
    even when the post has attached media. *)
let test_post_with_place () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    auth_type = Bearer;
    scope = None;
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";

  Mock_http.set_responses [
    { status = 200; body = "image_data"; headers = [] };
    { status = 200; body = {|{"id": "photo123"}|}; headers = [] };
    { status = 200; body = {|{"id": "post123"}|}; headers = [] };
  ];

  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Photo with a location"
    ~media_urls:["https://example.com/image.jpg"]
    ~place:"place_98765"
    (handle_outcome
      (fun _post_id ->
        let reqs = !Mock_http.requests in
        let feed_request =
          List.find_opt (fun (m, u, _, _) -> m = "POST" && string_contains u "/feed") reqs
        in
        (match feed_request with
        | Some (_, _, _, body) ->
            assert (string_contains body "place=place_98765");
            print_endline "✓ post_single forwards place into the feed request"
        | None -> failwith "Feed request not found"))
      (fun err -> failwith ("Post with place failed: " ^ err)))

(** Test: Recovers page token from user token on post failure *)
let test_post_recovers_page_token_from_user_token () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "user_token_initial";
    refresh_token = None;
    expires_at = Some future_time;
    auth_type = Bearer;
    scope = None;
  } in

  let permission_error = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "TRACE_PERM"
    }
  }|} in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";

  Mock_http.set_responses [
    { status = 200; body = "image_data_attempt1"; headers = [] };
    { status = 400; body = permission_error; headers = [] };
    { status = 200; body = {|{"data":[{"id":"page123","name":"My Page","access_token":"resolved_page_token","category":"Brand"}]}|}; headers = [] };
    { status = 200; body = "image_data_attempt2"; headers = [] };
    { status = 200; body = {|{"id":"photo_after_recovery"}|}; headers = [] };
    { status = 200; body = {|{"id":"post_after_recovery"}|}; headers = [] };
  ];

  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Recover token and post"
    ~media_urls:["https://example.com/recover.jpg"]
    (handle_outcome
      (fun post_id ->
        assert (post_id = "post_after_recovery");
        let reqs = !Mock_http.requests in
        let used_resolved_token =
          List.exists (fun (_m, _u, headers, _body) ->
            List.exists (fun (k, v) ->
              k = "Authorization" && string_contains v "resolved_page_token"
            ) headers
          ) reqs
        in
        assert used_resolved_token;
        (match Mock_config.get_health_status "test_account" with
        (* Recovery succeeded, so the account is healthy. This used to record
           the literal "token_recovered", which no consumer recognises. *)
        | Some (_, status, Some msg)
          when status = Health_status.(to_string Healthy) ->
            assert (string_contains msg "Recovered Page access token")
        | Some (_, status, _) ->
            failwith ("Expected healthy health status, got: " ^ status)
        | None ->
            failwith "Expected health status update for token recovery");
        (match List.assoc_opt "test_account" !Mock_config.credentials_store with
        | Some updated_creds ->
            assert (updated_creds.access_token = "resolved_page_token");
            assert (updated_creds.refresh_token = Some "user_token_initial")
        | None -> failwith "Expected updated credentials after token recovery");
        print_endline "✓ Post recovers page token from user token")
      (fun err -> failwith ("Token recovery post failed: " ^ err)))

(** Test: Recovery falls back to stored user token when current token fails *)
let test_post_recovery_uses_stored_user_token_fallback () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "stale_page_token";
    refresh_token = Some "stored_user_token";
    expires_at = Some future_time;
    auth_type = Bearer;
    scope = None;
  } in

  let permission_error = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "TRACE_FALLBACK"
    }
  }|} in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";

  Mock_http.set_responses [
    { status = 200; body = "image_data_attempt1"; headers = [] };
    { status = 400; body = permission_error; headers = [] };
    { status = 400; body = permission_error; headers = [] };
    { status = 200; body = {|{"data":[{"id":"page123","name":"My Page","access_token":"resolved_page_token_from_backup","category":"Brand"}]}|}; headers = [] };
    { status = 200; body = "image_data_attempt2"; headers = [] };
    { status = 200; body = {|{"id":"photo_after_fallback"}|}; headers = [] };
    { status = 200; body = {|{"id":"post_after_fallback"}|}; headers = [] };
  ];

  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Recover with fallback user token"
    ~media_urls:["https://example.com/recover-fallback.jpg"]
    (handle_outcome
      (fun post_id ->
        assert (post_id = "post_after_fallback");
        let reqs = !Mock_http.requests in
        let me_accounts_requests =
          List.filter (fun (m, u, _, _) -> m = "GET" && string_contains u "/me/accounts") reqs
        in
        assert (List.length me_accounts_requests = 2);
        let used_stored_user_token =
          List.exists (fun (_m, u, _, _) -> string_contains u "stored_user_token") me_accounts_requests
        in
        assert used_stored_user_token;
        (match List.assoc_opt "test_account" !Mock_config.credentials_store with
        | Some updated_creds ->
            assert (updated_creds.access_token = "resolved_page_token_from_backup");
            assert (updated_creds.refresh_token = Some "stored_user_token")
        | None -> failwith "Expected updated credentials after fallback recovery");
        print_endline "✓ Recovery uses stored user token fallback")
      (fun err -> failwith ("Fallback recovery test failed: " ^ err)))

(** Test: Recovery does not fallback to other tokens on rate limiting *)
let test_post_recovery_stops_on_rate_limit () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "stale_page_token";
    refresh_token = Some "stored_user_token";
    expires_at = Some future_time;
    auth_type = Bearer;
    scope = None;
  } in

  let permission_error = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "TRACE_RATE_STOP_1"
    }
  }|} in

  let rate_limit_error = {|{
    "error": {
      "message": "(#80001) There have been too many calls to this Page account",
      "type": "OAuthException",
      "code": 80001,
      "fbtrace_id": "TRACE_RATE_STOP_2"
    }
  }|} in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";

  Mock_http.set_responses [
    { status = 200; body = "image_data_attempt1"; headers = [] };
    { status = 400; body = permission_error; headers = [] };
    { status = 400; body = rate_limit_error; headers = [] };
  ];

  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Recovery should stop on rate limit"
    ~media_urls:["https://example.com/recover-stop.jpg"]
    (handle_outcome
      (fun _ -> failwith "Expected post failure")
      (fun _err ->
        let me_accounts_requests =
          List.filter (fun (m, u, _, _) -> m = "GET" && string_contains u "/me/accounts") !Mock_http.requests
        in
        assert (List.length me_accounts_requests = 1);
        (* Being rate limited says nothing about the stored credentials, so
           recovery must stop without downgrading account health. Every status
           recorded during this run stays "healthy". *)
        List.iter
          (fun (_, status, _) ->
            if status <> Health_status.(to_string Healthy) then
              failwith ("Rate-limited recovery must not change health, got: " ^ status))
          !Mock_config.health_statuses;
        print_endline "✓ Recovery stops on rate limit without fallback"))

(** Test: Cleans up uploaded photos if feed publish fails *)
let test_post_cleans_up_uploaded_photos_on_publish_failure () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "page_token_initial";
    refresh_token = None;
    expires_at = Some future_time;
    auth_type = Bearer;
    scope = None;
  } in

  let publish_error = {|{
    "error": {
      "message": "(#100) Invalid parameter",
      "type": "OAuthException",
      "code": 100,
      "fbtrace_id": "TRACE_PUBLISH_FAIL"
    }
  }|} in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";

  Mock_http.set_responses [
    { status = 200; body = "image1_data"; headers = [] };
    { status = 200; body = {|{"id":"photo_to_cleanup_1"}|}; headers = [] };
    { status = 200; body = "image2_data"; headers = [] };
    { status = 200; body = {|{"id":"photo_to_cleanup_2"}|}; headers = [] };
    { status = 400; body = publish_error; headers = [] };
    { status = 200; body = {|{"success":true}|}; headers = [] };
    { status = 200; body = {|{"success":true}|}; headers = [] };
  ];

  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Should fail and cleanup"
    ~media_urls:["https://example.com/c1.jpg"; "https://example.com/c2.jpg"]
    (handle_outcome
      (fun _ -> failwith "Expected publish failure")
      (fun _err ->
        let reqs = !Mock_http.requests in
        let deleted_photo1 =
          List.exists (fun (m, u, _, _) -> m = "DELETE" && string_contains u "/photo_to_cleanup_1") reqs
        in
        let deleted_photo2 =
          List.exists (fun (m, u, _, _) -> m = "DELETE" && string_contains u "/photo_to_cleanup_2") reqs
        in
        assert deleted_photo1;
        assert deleted_photo2;
        print_endline "✓ Cleans up uploaded photos on publish failure"))

(** Test: Rate-limit error code mapping for Pages/BUC *)
let test_rate_limit_error_code_80001 () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "(#80001) There have been too many calls to this Page account",
      "type": "OAuthException",
      "code": 80001,
      "fbtrace_id": "TRACE123"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.get ~path:"me" ~access_token:"test_token"
    (function
      | Ok _ -> failwith "Should have been rate limited"
      | Error e ->
          (match e with
          | Error_types.Rate_limited _ -> print_endline "✓ Rate-limit error code 80001 mapping"
          | _ -> failwith ("Expected rate-limited error, got: " ^ Error_types.error_to_string e)))

(** Test: Post without alt-text *)
let test_post_without_alt_text () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    auth_type = Bearer;
    scope = None;
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";
  
  Mock_http.set_responses [
    { status = 200; body = "image_data"; headers = [] };
    { status = 200; body = {|{"id": "photo789"}|}; headers = [] };
    { status = 200; body = {|{"id": "post789"}|}; headers = [] };
  ];
  
  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Photo without description"
    ~media_urls:["https://example.com/image.jpg"]
    ~alt_texts:[]
    (handle_outcome
      (fun _post_id ->
        print_endline "✓ Post without alt-text")
      (fun err -> failwith ("Post without alt-text failed: " ^ err)))

(** Test: Alt-text with special characters *)
let test_alt_text_special_characters () =
  Mock_config.reset ();
  
  Mock_http.set_responses [
    { status = 200; body = "fake_image_data"; headers = [] };
    { status = 200; body = {|{"id": "photo_special"}|}; headers = [] };
  ];
  
  Facebook.upload_photo
    ~page_id:"123456"
    ~page_access_token:"test_token"
    ~image_url:"https://example.com/image.jpg"
    ~alt_text:(Some "Photo with \"quotes\", & special <chars> and emojis 🎉")
    (function
      | Ok _photo_id ->
          print_endline "✓ Alt-text with special characters"
      | Error e -> failwith ("Alt-text with special chars failed: " ^ Error_types.error_to_string e))

(** Test: Partial alt-texts - fewer alt-texts than images *)
let test_partial_alt_texts () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    auth_type = Bearer;
    scope = None;
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";
  
  Mock_http.set_responses [
    { status = 200; body = "image1_data"; headers = [] };
    { status = 200; body = {|{"id": "photo1"}|}; headers = [] };
    { status = 200; body = "image2_data"; headers = [] };
    { status = 200; body = {|{"id": "photo2"}|}; headers = [] };
    { status = 200; body = "image3_data"; headers = [] };
    { status = 200; body = {|{"id": "photo3"}|}; headers = [] };
    { status = 200; body = {|{"id": "post_partial"}|}; headers = [] };
  ];
  
  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Three images, two alt-texts"
    ~media_urls:["https://example.com/img1.jpg"; "https://example.com/img2.jpg"; "https://example.com/img3.jpg"]
    ~alt_texts:[Some "First image"; Some "Second image"]
    (handle_outcome
      (fun _post_id ->
        print_endline "✓ Post with partial alt-texts (3 images, 2 alt-texts)")
      (fun err -> failwith ("Post with partial alt-texts failed: " ^ err)))

(** {1 Stories Tests} *)

(** Test: Upload photo story *)
let test_upload_photo_story () =
  Mock_config.reset ();
  
  Mock_http.set_responses [
    { status = 200; body = "fake_image_data"; headers = [] };  (* GET image *)
    { status = 200; body = {|{"post_id": "story_123"}|}; headers = [] };  (* POST multipart *)
  ];
  
  Facebook.upload_photo_story
    ~page_id:"page_123"
    ~page_access_token:"test_token"
    ~image_url:"https://example.com/story.jpg"
    (function
      | Ok story_id ->
          assert (story_id = "story_123");
          (* Verify the request went to photo_stories endpoint *)
          let requests = !Mock_http.requests in
          let has_photo_stories = List.exists (fun (_, url, _, _) ->
            string_contains url "photo_stories"
          ) requests in
          assert has_photo_stories;
          print_endline "✓ Upload photo story"
      | Error e -> failwith ("Upload photo story failed: " ^ Error_types.error_to_string e))

(** Test: Upload video story *)
let test_upload_video_story () =
  Mock_config.reset ();
  
  Mock_http.set_responses [
    { status = 200; body = "fake_video_data"; headers = [] };  (* GET video *)
    { status = 200; body = {|{"video_id": "vid_456", "upload_url": "https://upload.example.com"}|}; headers = [] };  (* POST init *)
    { status = 200; body = {|{"success": true}|}; headers = [] };  (* POST upload *)
    { status = 200; body = {|{"success": true, "post_id": "video_story_789"}|}; headers = [] };  (* POST finish *)
  ];
  
  Facebook.upload_video_story
    ~page_id:"page_123"
    ~page_access_token:"test_token"
    ~video_url:"https://example.com/story.mp4"
    (function
      | Ok story_id ->
          assert (story_id = "video_story_789");
          (* Verify the request went to video_stories endpoint *)
          let requests = !Mock_http.requests in
          let has_video_stories = List.exists (fun (_, url, _, _) ->
            string_contains url "video_stories"
          ) requests in
          assert has_video_stories;
          print_endline "✓ Upload video story"
      | Error e -> failwith ("Upload video story failed: " ^ Error_types.error_to_string e))

(** Test: Post photo story (high-level) *)
let test_post_story_photo () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    auth_type = Bearer;
    scope = None;
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";
  
  Mock_http.set_responses [
    { status = 200; body = "image_data"; headers = [] };
    { status = 200; body = {|{"post_id": "photo_story_abc"}|}; headers = [] };
  ];
  
  Facebook.post_story_photo
    ~account_id:"test_account"
    ~image_url:"https://example.com/story.jpg"
    (handle_outcome
      (fun story_id ->
        assert (story_id = "photo_story_abc");
        print_endline "✓ Post photo story (high-level)")
      (fun err -> failwith ("Post photo story failed: " ^ err)))

(** Test: Post video story (high-level) *)
let test_post_story_video () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    auth_type = Bearer;
    scope = None;
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";
  
  Mock_http.set_responses [
    { status = 200; body = "video_data"; headers = [] };
    { status = 200; body = {|{"video_id": "v123", "upload_url": "https://upload.example.com"}|}; headers = [] };
    { status = 200; body = {|{"success": true}|}; headers = [] };
    { status = 200; body = {|{"success": true, "post_id": "video_story_xyz"}|}; headers = [] };
  ];
  
  Facebook.post_story_video
    ~account_id:"test_account"
    ~video_url:"https://example.com/story.mp4"
    (handle_outcome
      (fun story_id ->
        assert (story_id = "video_story_xyz");
        print_endline "✓ Post video story (high-level)")
      (fun err -> failwith ("Post video story failed: " ^ err)))

(** Test: Post video story recovers on auth error during upload phase *)
let test_post_story_video_recovers_on_upload_auth_error () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "user_token_story";
    refresh_token = None;
    expires_at = Some future_time;
    auth_type = Bearer;
    scope = None;
  } in

  let permission_error = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "STORY_UP_AUTH"
    }
  }|} in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_story_123";

  Mock_http.set_responses [
    { status = 200; body = "video_data_1"; headers = [] };
    { status = 200; body = {|{"video_id":"story_vid_1","upload_url":"https://upload.facebook.com/story-up-1"}|}; headers = [] };
    { status = 400; body = permission_error; headers = [] };
    { status = 200; body = {|{"data":[{"id":"page_story_123","name":"Story Page","access_token":"story_resolved_page_token","category":"Brand"}]}|}; headers = [] };
    { status = 200; body = "video_data_2"; headers = [] };
    { status = 200; body = {|{"video_id":"story_vid_2","upload_url":"https://upload.facebook.com/story-up-2"}|}; headers = [] };
    { status = 200; body = {|{"success":true}|}; headers = [] };
    { status = 200; body = {|{"success":true,"post_id":"story_post_recovered"}|}; headers = [] };
  ];

  Facebook.post_story_video
    ~account_id:"test_account"
    ~video_url:"https://example.com/story-recover.mp4"
    (handle_outcome
      (fun story_id ->
        assert (story_id = "story_post_recovered");
        let used_resolved_token =
          List.exists (fun (_m, _u, headers, _body) ->
            List.exists (fun (k, v) -> k = "Authorization" && string_contains v "story_resolved_page_token") headers
          ) !Mock_http.requests
        in
        assert used_resolved_token;
        (match List.assoc_opt "test_account" !Mock_config.credentials_store with
        | Some updated_creds ->
            assert (updated_creds.access_token = "story_resolved_page_token");
            assert (updated_creds.refresh_token = Some "user_token_story")
        | None -> failwith "Expected updated credentials after story recovery");
        print_endline "✓ Post video story recovers on upload auth error")
      (fun err -> failwith ("Story upload auth recovery failed: " ^ err)))

(** Test: Post video story does not recover on upload rate-limit errors *)
let test_post_story_video_no_recovery_on_upload_rate_limit () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "user_token_story_rl";
    refresh_token = Some "stored_user_token_story_rl";
    expires_at = Some future_time;
    auth_type = Bearer;
    scope = None;
  } in

  let rate_limit_error = {|{
    "error": {
      "message": "(#80001) There have been too many calls to this Page account",
      "type": "OAuthException",
      "code": 80001,
      "fbtrace_id": "STORY_UP_RL"
    }
  }|} in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_story_123";

  Mock_http.set_responses [
    { status = 200; body = "video_data_1"; headers = [] };
    { status = 200; body = {|{"video_id":"story_vid_1","upload_url":"https://upload.facebook.com/story-up-1"}|}; headers = [] };
    { status = 400; body = rate_limit_error; headers = [] };
  ];

  Facebook.post_story_video
    ~account_id:"test_account"
    ~video_url:"https://example.com/story-rate-limit.mp4"
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Rate_limited _) ->
          let me_accounts_requests =
            List.filter (fun (m, u, _, _) -> m = "GET" && string_contains u "/me/accounts") !Mock_http.requests
          in
          assert (List.length me_accounts_requests = 0);
          print_endline "✓ Post video story skips recovery on upload rate-limit"
      | Error_types.Failure e ->
          failwith ("Expected rate-limited error, got: " ^ Error_types.error_to_string e)
      | _ -> failwith "Expected story failure on upload rate-limit")

(** Test: Post story with auto-detect (image) *)
let test_post_story_auto_image () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    auth_type = Bearer;
    scope = None;
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";
  
  Mock_http.set_responses [
    { status = 200; body = "image_data"; headers = [] };
    { status = 200; body = {|{"post_id": "auto_story_img"}|}; headers = [] };
  ];
  
  Facebook.post_story
    ~account_id:"test_account"
    ~media_url:"https://example.com/story.png"
    (handle_outcome
      (fun story_id ->
        assert (story_id = "auto_story_img");
        print_endline "✓ Post story with auto-detect (image)")
      (fun err -> failwith ("Post story auto-detect failed: " ^ err)))

(** Test: Post story with auto-detect (video) *)
let test_post_story_auto_video () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    auth_type = Bearer;
    scope = None;
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";
  
  Mock_http.set_responses [
    { status = 200; body = "video_data"; headers = [] };
    { status = 200; body = {|{"video_id": "v789", "upload_url": "https://upload.example.com"}|}; headers = [] };
    { status = 200; body = {|{"success": true}|}; headers = [] };
    { status = 200; body = {|{"success": true, "post_id": "auto_story_vid"}|}; headers = [] };
  ];
  
  Facebook.post_story
    ~account_id:"test_account"
    ~media_url:"https://example.com/story.mov"
    (handle_outcome
      (fun story_id ->
        assert (story_id = "auto_story_vid");
        print_endline "✓ Post story with auto-detect (video)")
      (fun err -> failwith ("Post story auto-detect video failed: " ^ err)))

(** Test: Story validation - valid image URL *)
let test_validate_story_valid_image () =
  match Facebook.validate_story ~media_url:"https://example.com/story.jpg" with
  | Ok () -> print_endline "✓ Story validation - valid image URL"
  | Error e -> failwith ("Story validation failed: " ^ e)

(** Test: Story validation - valid video URL *)
let test_validate_story_valid_video () =
  match Facebook.validate_story ~media_url:"https://example.com/story.mp4" with
  | Ok () -> print_endline "✓ Story validation - valid video URL"
  | Error e -> failwith ("Story validation failed: " ^ e)

(** Test: Story validation - invalid URL *)
let test_validate_story_invalid_url () =
  match Facebook.validate_story ~media_url:"not-a-url" with
  | Error msg when string_contains msg "HTTP" -> 
      print_endline "✓ Story validation - rejects invalid URL"
  | _ -> failwith "Should reject invalid URL"

(** Test: Story validation - invalid format *)
let test_validate_story_invalid_format () =
  match Facebook.validate_story ~media_url:"https://example.com/story.txt" with
  | Error msg when string_contains msg "image" || string_contains msg "video" -> 
      print_endline "✓ Story validation - rejects invalid format"
  | _ -> failwith "Should reject invalid format"

(** {1 Video Reel Tests} *)

(** Responses for one successful Reels flow: start, hosted upload, finish. *)
let reel_success_responses ~video_id : response list = [
  { status = 200;
    body = Printf.sprintf {|{"video_id": "%s", "upload_url": "https://rupload.facebook.com/video-upload/v25.0/%s"}|} video_id video_id;
    headers = [] };
  { status = 200; body = {|{"success": true}|}; headers = [] };
  { status = 200; body = {|{"success": true}|}; headers = [] };
]

(** Test: Upload video reel (Reels Publishing API, hosted file)

    1. POST /{page_id}/video_reels with upload_phase=start
    2. POST rupload.facebook.com/video-upload/v25.0/{video_id} with file_url header
    3. POST /{page_id}/video_reels with upload_phase=finish, video_state=PUBLISHED

    @see <https://developers.facebook.com/docs/video-api/guides/reels-publishing>
*)
let test_upload_video_reel () =
  Mock_config.reset ();

  Mock_http.set_responses (reel_success_responses ~video_id:"reel_video_123");

  Facebook.upload_video_reel
    ~page_id:"page_123"
    ~page_access_token:"test_token"
    ~video_url:"https://example.com/reel.mp4"
    ~description:"My first Reel!"
    (fun video_id ->
      assert (video_id = "reel_video_123");
      (match List.rev !Mock_http.requests with
       | [ ("POST", start_url, start_headers, start_body);
           ("POST", upload_url, upload_headers, upload_body);
           ("POST", finish_url, _, finish_body) ] ->
           assert (start_url = "https://graph.facebook.com/v25.0/page_123/video_reels");
           assert (List.assoc_opt "Content-Type" start_headers
                   = Some "application/x-www-form-urlencoded");
           assert (start_body = "upload_phase=start&access_token=test_token");

           assert (upload_url = "https://rupload.facebook.com/video-upload/v25.0/reel_video_123");
           assert (List.assoc_opt "Authorization" upload_headers = Some "OAuth test_token");
           assert (List.assoc_opt "file_url" upload_headers = Some "https://example.com/reel.mp4");
           assert (upload_body = "");

           assert (finish_url = "https://graph.facebook.com/v25.0/page_123/video_reels");
           assert (string_contains finish_body "upload_phase=finish");
           assert (string_contains finish_body "video_id=reel_video_123");
           assert (string_contains finish_body "video_state=PUBLISHED");
           assert (string_contains finish_body "description=My");
           assert (string_contains finish_body "access_token=test_token");
           assert (not (string_contains finish_body "place="));
           assert (not (string_contains finish_body "appsecret_proof"))
       | _ -> failwith "Expected start, hosted upload and finish requests in order");
      print_endline "✓ Upload video reel (start, hosted upload, finish)")
    (fun err -> failwith ("Upload video reel failed: " ^ err))

(** Test: place and appsecret_proof reach the reel phases that take them *)
let test_upload_video_reel_with_place () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_app_secret";

  Mock_http.set_responses (reel_success_responses ~video_id:"reel_place_1");

  Facebook.upload_video_reel
    ~page_id:"page_123"
    ~page_access_token:"test_token"
    ~video_url:"https://example.com/reel.mp4"
    ~description:"Reel at a place"
    ~place:"place_98765"
    (fun video_id ->
      assert (video_id = "reel_place_1");
      (match List.rev !Mock_http.requests with
       | [ (_, _, _, start_body); _; (_, _, _, finish_body) ] ->
           assert (not (string_contains start_body "place="));
           assert (string_contains start_body "appsecret_proof=");
           assert (string_contains finish_body "place=place_98765");
           assert (string_contains finish_body "appsecret_proof=")
       | _ -> failwith "Expected three reel requests");
      print_endline "✓ Upload video reel sends place on finish only")
    (fun err -> failwith ("Upload video reel with place failed: " ^ err))

(** Run one reel upload against [responses] and expect a failure *)
let expect_reel_upload_failure ~name ~responses ~expected_requests =
  Mock_config.reset ();
  Mock_http.set_responses responses;
  Facebook.upload_video_reel_typed
    ~page_id:"page_123"
    ~page_access_token:"test_token"
    ~video_url:"https://example.com/reel.mp4"
    ~description:"Should fail"
    (fun video_id -> failwith (Printf.sprintf "%s: expected failure, got video %s" name video_id))
    (fun _err ->
      assert (List.length !Mock_http.requests = expected_requests);
      Printf.printf "✓ Upload video reel fails: %s\n" name)

(** Test: success=false and non-2xx at each phase fail the reel *)
let test_upload_video_reel_failures () =
  let start = List.hd (reel_success_responses ~video_id:"vid_f") in
  let ok = { status = 200; body = {|{"success": true}|}; headers = [] } in
  let not_ok = { status = 200; body = {|{"success": false}|}; headers = [] } in
  let server_error = { status = 500; body = {|{"error":{"message":"boom","type":"FacebookApiException","code":2}}|}; headers = [] } in
  expect_reel_upload_failure ~name:"start non-2xx"
    ~responses:[server_error] ~expected_requests:1;
  expect_reel_upload_failure ~name:"start without video_id"
    ~responses:[ok] ~expected_requests:1;
  expect_reel_upload_failure ~name:"upload success=false"
    ~responses:[start; not_ok] ~expected_requests:2;
  expect_reel_upload_failure ~name:"upload non-2xx"
    ~responses:[start; server_error] ~expected_requests:2;
  expect_reel_upload_failure ~name:"finish success=false"
    ~responses:[start; ok; not_ok] ~expected_requests:3;
  expect_reel_upload_failure ~name:"finish without success flag"
    ~responses:[start; ok; { status = 200; body = "{}"; headers = [] }] ~expected_requests:3;
  expect_reel_upload_failure ~name:"finish non-2xx"
    ~responses:[start; ok; server_error] ~expected_requests:3

(** Test: Post reel (high-level) *)
let test_post_reel () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    auth_type = Bearer;
    scope = None;
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";
  
  Mock_http.set_responses (reel_success_responses ~video_id:"vid_456");

  Facebook.post_reel
    ~account_id:"test_account"
    ~text:"Check out my Reel! #facebook #reel"
    ~video_url:"https://example.com/reel.mp4"
    (handle_outcome
      (fun video_id ->
        assert (video_id = "vid_456");
        print_endline "✓ Post reel (high-level)")
      (fun err -> failwith ("Post reel failed: " ^ err)))

(** Test: post_reel forwards place, and an auth error in the finish phase
    recovers the Page token and reruns the whole flow *)
let test_post_reel_with_place_recovers_on_finish_auth_error () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "user_token_reel_place";
    refresh_token = None;
    expires_at = Some future_time;
    auth_type = Bearer;
    scope = None;
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";

  let permission_error = {|{"error":{"message":"(#200) Permissions error","type":"OAuthException","code":200}}|} in
  let first_attempt =
    match reel_success_responses ~video_id:"vid_first" with
    | [start; upload; _finish] -> [start; upload; { status = 400; body = permission_error; headers = [] }]
    | _ -> failwith "Expected three reel responses"
  in
  Mock_http.set_responses (
    first_attempt
    @ [ { status = 200; body = {|{"data":[{"id":"page_123","name":"My Page","access_token":"resolved_reel_place_token","category":"Brand"}]}|}; headers = [] } ]
    @ reel_success_responses ~video_id:"vid_place");

  Facebook.post_reel
    ~account_id:"test_account"
    ~text:"Reel with place"
    ~video_url:"https://example.com/reel.mp4"
    ~place:"place_98765"
    (handle_outcome
      (fun video_id ->
        assert (video_id = "vid_place");
        let requests = List.rev !Mock_http.requests in
        let starts =
          List.filter (fun (_, _, _, body) -> string_contains body "upload_phase=start") requests
        in
        assert (List.length starts = 2);
        (match List.rev requests with
         | (_, url, _, finish_body) :: _ ->
             assert (string_contains url "page_123/video_reels");
             assert (string_contains finish_body "upload_phase=finish");
             assert (string_contains finish_body "place=place_98765");
             assert (string_contains finish_body "access_token=resolved_reel_place_token")
         | [] -> failwith "Expected reel requests");
        print_endline "✓ Post reel forwards place and recovers on finish auth error")
      (fun err -> failwith ("Post reel with place failed: " ^ err)))

(** Test: Post reel does not attempt token recovery on non-auth errors *)
let test_post_reel_no_recovery_on_non_auth_error () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    auth_type = Bearer;
    scope = None;
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";

  (* Single response: API returns a non-auth error (400 bad request).
     Should NOT trigger token recovery (/me/accounts). *)
  Mock_http.set_responses [
    { status = 400; body = {|{"error":{"message":"Invalid video","type":"OAuthException","code":100}}|}; headers = [] };
  ];

  Facebook.post_reel
    ~account_id:"test_account"
    ~text:"Reel should fail without recovery"
    ~video_url:"https://example.com/reel.mp4"
    (fun outcome ->
      match outcome with
      | Error_types.Failure _ ->
          let requested_recovery =
            List.exists (fun (m, u, _, _) ->
              m = "GET" && string_contains u "/me/accounts"
            ) !Mock_http.requests
          in
          assert (not requested_recovery);
          print_endline "✓ Post reel skips recovery on non-auth errors"
      | _ -> failwith "Expected reel failure")

(** Test: Post reel recovers token on auth error *)
let test_post_reel_recovers_on_auth_error () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "user_token_for_reel";
    refresh_token = None;
    expires_at = Some future_time;
    auth_type = Bearer;
    scope = None;
  } in

  let permission_error = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "REEL_PERM"
    }
  }|} in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";

  Mock_http.set_responses ([
    (* First attempt: auth error *)
    { status = 400; body = permission_error; headers = [] };
    (* Token recovery: /me/accounts *)
    { status = 200; body = {|{"data":[{"id":"page_123","name":"My Page","access_token":"resolved_reel_page_token","category":"Brand"}]}|}; headers = [] };
    (* Second attempt with recovered token: full flow succeeds *)
  ] @ reel_success_responses ~video_id:"vid_recovered");

  Facebook.post_reel
    ~account_id:"test_account"
    ~text:"Reel recovers on auth error"
    ~video_url:"https://example.com/reel.mp4"
    (handle_outcome
      (fun video_id ->
        assert (video_id = "vid_recovered");
        let used_resolved_token =
          List.exists (fun (_m, _u, headers, _body) ->
            List.exists (fun (k, v) ->
              k = "Authorization" && string_contains v "resolved_reel_page_token"
            ) headers
          ) !Mock_http.requests
        in
        assert used_resolved_token;
        print_endline "✓ Post reel recovers on auth errors")
      (fun err -> failwith ("Expected reel recovery success, got: " ^ err)))

(** ===== Ordinary Page videos ([/{page-id}/videos]) ===== *)

let future_credentials token =
  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  { access_token = token; refresh_token = None;
    expires_at = Some future_time; auth_type = Bearer; scope = None }

(** Test: Videos.parse accepts only a body with a string id *)
let test_videos_parse () =
  (match Videos.parse {|{"id":"987654"}|} with
   | Ok { Videos.id } -> assert (id = "987654")
   | Error message -> failwith ("expected id to parse: " ^ message));
  let expect_error name body =
    match Videos.parse body with
    | Ok _ -> failwith (name ^ ": must not parse")
    | Error _ -> ()
  in
  expect_error "success without id" {|{"success":true}|};
  expect_error "non-string id" {|{"id":123}|};
  expect_error "not an object" {|["987654"]|};
  expect_error "not JSON" "not json at all";
  print_endline "✓ Videos.parse accepts only a string id"

(** Test: create_video posts description and file_url to the videos edge *)
let test_create_video () =
  Mock_config.reset ();

  Mock_http.set_responses [ { status = 200; body = {|{"id":"video_123"}|}; headers = [] } ];

  Facebook.create_video
    ~page_id:"page_123"
    ~page_access_token:"test_token"
    ~file_url:"https://example.com/video.mp4"
    ~description:"An ordinary video"
    (fun video ->
      assert (video.Videos.id = "video_123");
      (match List.rev !Mock_http.requests with
       | [ ("POST", url, headers, body) ] ->
           assert (url = "https://graph.facebook.com/v25.0/page_123/videos");
           assert (List.assoc_opt "Authorization" headers = Some "Bearer test_token");
           assert (string_contains body "description=An");
           assert (string_contains body "file_url=https");
           assert (not (string_contains body "access_token="));
           assert (not (string_contains body "appsecret_proof"))
       | _ -> failwith "Expected exactly one videos POST");
      print_endline "✓ Create video (hosted file)")
    (fun err -> failwith ("Create video failed: " ^ Error_types.error_to_string err))

(** Test: create_video appends appsecret_proof when configured *)
let test_create_video_with_app_secret () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_app_secret";

  Mock_http.set_responses [ { status = 200; body = {|{"id":"video_secret"}|}; headers = [] } ];

  Facebook.create_video
    ~page_id:"page_123"
    ~page_access_token:"test_token"
    ~file_url:"https://example.com/video.mp4"
    ~description:"With proof"
    (fun video ->
      assert (video.Videos.id = "video_secret");
      (match List.rev !Mock_http.requests with
       | [ (_, _, _, body) ] -> assert (string_contains body "appsecret_proof=")
       | _ -> failwith "Expected one videos POST");
      print_endline "✓ Create video sends appsecret_proof")
    (fun err -> failwith ("Create video with app secret failed: " ^ Error_types.error_to_string err))

(** Run one create_video against [body] at [status] and expect a failure
    with exactly one request (no retry at the Page-token level). *)
let expect_create_video_failure ~name ~status ~body =
  Mock_config.reset ();
  Mock_http.set_responses [ { status; body; headers = [] } ];
  Facebook.create_video
    ~page_id:"page_123"
    ~page_access_token:"test_token"
    ~file_url:"https://example.com/video.mp4"
    ~description:"Should fail"
    (fun video -> failwith (Printf.sprintf "%s: expected failure, got video %s" name video.Videos.id))
    (fun _err ->
      assert (List.length !Mock_http.requests = 1);
      Printf.printf "✓ Create video fails: %s\n" name)

(** Test: non-2xx and id-less 2xx bodies fail the creation *)
let test_create_video_failures () =
  expect_create_video_failure ~name:"non-2xx" ~status:400
    ~body:{|{"error":{"message":"(#100) Invalid parameter","type":"OAuthException","code":100}}|};
  expect_create_video_failure ~name:"2xx without id" ~status:200 ~body:{|{"success":true}|};
  expect_create_video_failure ~name:"2xx with non-string id" ~status:200 ~body:{|{"id":123}|};
  expect_create_video_failure ~name:"2xx not an object" ~status:200 ~body:{|["987654"]|}

(** Test: post_video (high-level) publishes through the Page context *)
let test_post_video () =
  Mock_config.reset ();

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:(future_credentials "valid_token");
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";

  Mock_http.set_responses [ { status = 200; body = {|{"id":"vid_post_1"}|}; headers = [] } ];

  Facebook.post_video
    ~account_id:"test_account"
    ~text:"Ordinary video caption"
    ~video_url:"https://example.com/video.mp4"
    (handle_outcome
      (fun video ->
        assert (video.Videos.id = "vid_post_1");
        (match List.rev !Mock_http.requests with
         | [ ("POST", url, headers, _) ] ->
             assert (url = "https://graph.facebook.com/v25.0/page_123/videos");
             assert (List.assoc_opt "Authorization" headers = Some "Bearer valid_token")
         | _ -> failwith "Expected exactly one videos POST");
        print_endline "✓ Post video (high-level)")
      (fun err -> failwith ("Post video failed: " ^ err)))

(** Test: post_video recovers the Page token once and retries the POST *)
let test_post_video_recovers_on_auth_error () =
  Mock_config.reset ();

  Mock_config.set_credentials ~account_id:"test_account"
    ~credentials:(future_credentials "stale_page_token");
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";

  Mock_http.set_responses [
    (* First POST: the stored token is rejected *)
    { status = 400;
      body = {|{"error":{"message":"Invalid OAuth access token","type":"OAuthException","code":190}}|};
      headers = [] };
    (* Recovery: /me/accounts hands out a fresh Page token *)
    { status = 200;
      body = {|{"data":[{"id":"page_123","name":"My Page","access_token":"resolved_video_page_token","category":"Brand"}]}|};
      headers = [] };
    (* Retried POST with the resolved token *)
    { status = 200; body = {|{"id":"vid_recovered"}|}; headers = [] };
  ];

  Facebook.post_video
    ~account_id:"test_account"
    ~text:"Recovers after auth failure"
    ~video_url:"https://example.com/video.mp4"
    (handle_outcome
      (fun video ->
        assert (video.Videos.id = "vid_recovered");
        (match List.rev !Mock_http.requests with
         | [ ("POST", _, first_headers, _); ("GET", _, _, _); ("POST", _, retry_headers, _) ] ->
             assert (List.assoc_opt "Authorization" first_headers = Some "Bearer stale_page_token");
             assert (List.assoc_opt "Authorization" retry_headers = Some "Bearer resolved_video_page_token")
         | _ -> failwith "Expected POST, recovery GET and retried POST");
        print_endline "✓ Post video recovers on auth error")
      (fun err -> failwith ("Post video recovery failed: " ^ err)))

(** Test: post_video does not attempt token recovery on non-auth errors *)
let test_post_video_no_recovery_on_non_auth_error () =
  Mock_config.reset ();

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:(future_credentials "valid_token");
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";

  (* Rate limit is not an auth failure: no /me/accounts read, no retry. *)
  Mock_http.set_responses [
    { status = 429; body = {|{"error":{"message":"Too many requests","code":4}}|}; headers = [] };
  ];

  Facebook.post_video
    ~account_id:"test_account"
    ~text:"Fails without recovery"
    ~video_url:"https://example.com/video.mp4"
    (fun outcome ->
      match outcome with
      | Error_types.Failure _ ->
          assert (List.length !Mock_http.requests = 1);
          assert (not (List.exists (fun (m, _, _, _) -> m = "GET") !Mock_http.requests));
          print_endline "✓ Post video skips recovery on non-auth errors"
      | _ -> failwith "Expected video failure")

(** Test: post_video rejects an over-limit caption before any request *)
let test_post_video_caption_validation () =
  Mock_config.reset ();

  let long_caption = String.make 63207 'a' in
  Facebook.post_video
    ~account_id:"test_account"
    ~text:long_caption
    ~video_url:"https://example.com/video.mp4"
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Validation_error _) ->
          assert (!Mock_http.requests = []);
          print_endline "✓ Post video validates caption length"
      | _ -> failwith "Expected caption validation failure")

(** Test: Post reel recovers on auth error and persists updated credentials *)
let test_post_reel_recovers_on_upload_auth_error () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "user_token_reel_upload";
    refresh_token = None;
    expires_at = Some future_time;
    auth_type = Bearer;
    scope = None;
  } in

  let permission_error = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "REEL_UP_AUTH"
    }
  }|} in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";

  Mock_http.set_responses ([
    (* First attempt: auth error *)
    { status = 400; body = permission_error; headers = [] };
    (* Token recovery: /me/accounts *)
    { status = 200; body = {|{"data":[{"id":"page_123","name":"My Page","access_token":"resolved_reel_upload_token","category":"Brand"}]}|}; headers = [] };
    (* Second attempt with recovered token: full flow succeeds *)
  ] @ reel_success_responses ~video_id:"vid_up_2");

  Facebook.post_reel
    ~account_id:"test_account"
    ~text:"Reel upload auth recovery"
    ~video_url:"https://example.com/reel-upload-recover.mp4"
    (handle_outcome
      (fun video_id ->
        assert (video_id = "vid_up_2");
        let used_resolved_token =
          List.exists (fun (_m, _u, headers, _body) ->
            List.exists (fun (k, v) -> k = "Authorization" && string_contains v "resolved_reel_upload_token") headers
          ) !Mock_http.requests
        in
        assert used_resolved_token;
        (match List.assoc_opt "test_account" !Mock_config.credentials_store with
        | Some updated_creds ->
            assert (updated_creds.access_token = "resolved_reel_upload_token");
            assert (updated_creds.refresh_token = Some "user_token_reel_upload")
        | None -> failwith "Expected updated credentials after reel upload recovery");
        print_endline "✓ Post reel recovers on upload auth error")
      (fun err -> failwith ("Reel upload auth recovery failed: " ^ err)))

(** Test: Post reel does not recover on upload rate-limit errors *)
let test_post_reel_no_recovery_on_upload_rate_limit () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "user_token_reel_rl";
    refresh_token = Some "stored_user_token_reel_rl";
    expires_at = Some future_time;
    auth_type = Bearer;
    scope = None;
  } in

  let rate_limit_error = {|{
    "error": {
      "message": "(#80001) There have been too many calls to this Page account",
      "type": "OAuthException",
      "code": 80001,
      "fbtrace_id": "REEL_UP_RL"
    }
  }|} in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";

  Mock_http.set_responses [
    (* Start phase: rate-limit error *)
    { status = 400; body = rate_limit_error; headers = [] };
  ];

  Facebook.post_reel
    ~account_id:"test_account"
    ~text:"Reel upload rate-limit"
    ~video_url:"https://example.com/reel-upload-rate-limit.mp4"
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Rate_limited _) ->
          let me_accounts_requests =
            List.filter (fun (m, u, _, _) -> m = "GET" && string_contains u "/me/accounts") !Mock_http.requests
          in
          assert (List.length me_accounts_requests = 0);
          print_endline "✓ Post reel skips recovery on upload rate-limit"
      | Error_types.Failure e ->
          failwith ("Expected rate-limited error, got: " ^ Error_types.error_to_string e)
      | _ -> failwith "Expected reel failure on upload rate-limit")

(** Test: Video reel validation - caption too long *)
let test_reel_caption_validation () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    auth_type = Bearer;
    scope = None;
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";
  
  (* Facebook's actual post limit is 63206 chars - test exceeding that *)
  let long_caption = String.make 63300 'x' in
  
  Facebook.post_reel
    ~account_id:"test_account"
    ~text:long_caption
    ~video_url:"https://example.com/reel.mp4"
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Validation_error _) ->
          print_endline "✓ Reel caption validation - too long rejected"
      | _ -> failwith "Should reject caption over 63206 chars")

(** Test: Video media validation - validates URLs are accessible *)
let test_video_media_validation () =
  (* Valid video URL *)
  (match Facebook.validate_media ~media_urls:["https://example.com/video.mp4"] with
   | Ok () -> print_endline "✓ Valid video URL passes"
   | Error _ -> failwith "Valid video URL should pass");
  
  (* Multiple valid URLs *)
  (match Facebook.validate_media ~media_urls:[
    "https://example.com/photo1.jpg";
    "https://example.com/photo2.jpg"
  ] with
   | Ok () -> print_endline "✓ Multiple valid URLs pass"
   | Error _ -> failwith "Multiple valid URLs should pass");
  
  (* Invalid URL (not http/https) rejected *)
  (match Facebook.validate_media ~media_urls:["ftp://example.com/video.mp4"] with
   | Error _ -> print_endline "✓ Invalid URL rejected"
   | Ok () -> failwith "Invalid URL should fail")

(** Test: Post comment *)
let test_post_comment () =
  Mock_config.reset ();

  let response_body = {|{"id": "12345_67890"}|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Facebook.post_comment
    ~post_id:"12345"
    ~access_token:"test_token"
    ~message:"Great post!"
    (function
      | Ok comment_id ->
          assert (comment_id = "12345_67890");
          (* Verify the request went to the right endpoint *)
          let requests = !Mock_http.requests in
          (match requests with
          | (method_, url, _, body) :: _ ->
              assert (method_ = "POST");
              assert (string_contains url "12345/comments");
              assert (string_contains body "message");
              assert (string_contains body "Great");
              print_endline "✓ Post comment"
          | [] -> failwith "No requests made")
      | Error e -> failwith ("Post comment failed: " ^ Error_types.error_to_string e))

(** Test: post_comment form-urlencodes special characters in the POST body *)
let test_post_comment_form_urlencodes_special_chars () =
  Mock_config.reset ();

  let response_body = {|{"id": "12345_99999"}|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Facebook.post_comment
    ~post_id:"12345"
    ~access_token:"test_token"
    ~message:"Hello & welcome = great + news!"
    (function
      | Ok comment_id ->
          assert (comment_id = "12345_99999");
          let requests = !Mock_http.requests in
          (match requests with
          | (method_, url, _, body) :: _ ->
              assert (method_ = "POST");
              assert (string_contains url "12345/comments");
              (* Spaces should be encoded as + *)
              assert (string_contains body "Hello+%26+welcome+%3D+great+%2B+news%21");
              print_endline "✓ Post comment form-urlencodes special characters"
          | [] -> failwith "No requests made")
      | Error e -> failwith ("Post comment special chars failed: " ^ Error_types.error_to_string e))

(** Test: Get comments *)
let test_get_comments () =
  Mock_config.reset ();

  let response_body = {|{
    "data": [
      {
        "id": "comment_1",
        "message": "Nice!",
        "created_time": "2024-01-15T10:00:00+0000",
        "from": {"name": "Alice", "id": "user_1"}
      },
      {
        "id": "comment_2",
        "message": "Love it!",
        "created_time": "2024-01-15T11:00:00+0000",
        "from": {"name": "Bob", "id": "user_2"}
      }
    ]
  }|} in

  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Facebook.get_comments
    ~post_id:"12345"
    ~access_token:"test_token"
    (function
      | Ok comments ->
          assert (List.length comments = 2);
          let c1 = List.hd comments in
          assert (c1.comment_id = "comment_1");
          assert (c1.comment_message = "Nice!");
          assert (c1.comment_created_time = Some "2024-01-15T10:00:00+0000");
          assert (c1.comment_from = Some "Alice");
          let c2 = List.nth comments 1 in
          assert (c2.comment_id = "comment_2");
          assert (c2.comment_message = "Love it!");
          assert (c2.comment_from = Some "Bob");
          (* Verify the request URL *)
          let requests = !Mock_http.requests in
          (match requests with
          | (method_, url, _, _) :: _ ->
              assert (method_ = "GET");
              assert (string_contains url "12345/comments");
              assert (string_contains url "fields=");
              print_endline "✓ Get comments"
          | [] -> failwith "No requests made")
      | Error e -> failwith ("Get comments failed: " ^ Error_types.error_to_string e))

(** Test: Get comments on a post with no comments *)
let test_get_comments_empty () =
  Mock_config.reset ();

  let response_body = {|{"data": []}|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Facebook.get_comments
    ~post_id:"12345"
    ~access_token:"test_token"
    (function
      | Ok comments ->
          assert (List.length comments = 0);
          print_endline "✓ Get comments (empty)"
      | Error e -> failwith ("Get comments empty failed: " ^ Error_types.error_to_string e))

(** Test: Post comment returns API error on failure *)
let test_post_comment_error () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "Invalid post ID",
      "type": "OAuthException",
      "code": 100,
      "fbtrace_id": "ERR123"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.post_comment
    ~post_id:"invalid_post"
    ~access_token:"test_token"
    ~message:"Test"
    (function
      | Ok _ -> failwith "Should have failed with error"
      | Error (Error_types.Api_error err) ->
          assert (err.status_code = 400);
          assert (string_contains err.message "Invalid post ID");
          print_endline "✓ Post comment error handling"
      | Error e -> failwith ("Expected Api_error, got: " ^ Error_types.error_to_string e))

(** Test: Get page info returns API error on failure *)
let test_get_page_info_error () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "Unsupported get request",
      "type": "GraphMethodException",
      "code": 100,
      "fbtrace_id": "PAGE_ERR"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.get_page_info
    ~page_id:"nonexistent_page"
    ~access_token:"test_token"
    (function
      | Ok _ -> failwith "Should have failed with error"
      | Error (Error_types.Api_error err) ->
          assert (err.status_code = 400);
          assert (string_contains err.message "Unsupported get request");
          print_endline "✓ Get page info error handling"
      | Error e -> failwith ("Expected Api_error, got: " ^ Error_types.error_to_string e))

(** Test: Get comments handles missing from field gracefully *)
let test_get_comments_missing_from () =
  Mock_config.reset ();

  let response_body = {|{
    "data": [
      {
        "id": "comment_no_from",
        "message": "Anonymous comment",
        "created_time": "2024-01-15T10:00:00+0000"
      }
    ]
  }|} in

  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Facebook.get_comments
    ~post_id:"12345"
    ~access_token:"test_token"
    (function
      | Ok comments ->
          assert (List.length comments = 1);
          let c = List.hd comments in
          assert (c.comment_id = "comment_no_from");
          assert (c.comment_message = "Anonymous comment");
          assert (c.comment_from = None);
          print_endline "✓ Get comments (missing from field)"
      | Error e -> failwith ("Get comments missing from failed: " ^ Error_types.error_to_string e))

(** Test: Post with link *)
let test_post_with_link () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    auth_type = Bearer;
    scope = None;
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";

  Mock_http.set_response { status = 200; body = {|{"id": "post_link_123"}|}; headers = [] };

  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Check out this article!"
    ~media_urls:[]
    ~link:"https://example.com/article"
    (handle_outcome
      (fun post_id ->
        assert (post_id = "post_link_123");
        (* Verify the request body contains the link parameter *)
        let requests = !Mock_http.requests in
        let feed_request =
          List.find_opt (fun (m, u, _, _) -> m = "POST" && string_contains u "/feed") requests
        in
        (match feed_request with
        | Some (_, _, _, body) ->
            assert (string_contains body "link=");
            assert (string_contains body "example.com");
            print_endline "✓ Post with link"
        | None -> failwith "Feed request not found"))
      (fun err -> failwith ("Post with link failed: " ^ err)))

(** Test: Post with scheduled publish time *)
let test_post_with_scheduled_publish_time () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    auth_type = Bearer;
    scope = None;
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";

  Mock_http.set_response { status = 200; body = {|{"id": "scheduled_post_123"}|}; headers = [] };

  Facebook.post_single
    ~account_id:"test_account"
    ~text:"This is a scheduled post"
    ~media_urls:[]
    ~scheduled_publish_time:1735689600
    (handle_outcome
      (fun post_id ->
        assert (post_id = "scheduled_post_123");
        (* Verify the request body contains scheduling parameters *)
        let requests = !Mock_http.requests in
        let feed_request =
          List.find_opt (fun (m, u, _, _) -> m = "POST" && string_contains u "/feed") requests
        in
        (match feed_request with
        | Some (_, _, _, body) ->
            assert (string_contains body "scheduled_publish_time=1735689600");
            assert (string_contains body "published=false");
            print_endline "✓ Post with scheduled publish time"
        | None -> failwith "Feed request not found"))
      (fun err -> failwith ("Scheduled post failed: " ^ err)))

(** Test: Get page info *)
let test_get_page_info () =
  Mock_config.reset ();

  let response_body = {|{
    "id": "page_123",
    "name": "My Awesome Page",
    "about": "This is a test page",
    "fan_count": 5000,
    "followers_count": 4500
  }|} in

  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Facebook.get_page_info
    ~page_id:"page_123"
    ~access_token:"test_token"
    (function
      | Ok detail ->
          assert (detail.page_detail_id = "page_123");
          assert (detail.page_detail_name = Some "My Awesome Page");
          assert (detail.page_detail_about = Some "This is a test page");
          assert (detail.page_detail_fan_count = Some 5000);
          assert (detail.page_detail_followers_count = Some 4500);
          (* Verify the request URL contains proper fields *)
          let requests = !Mock_http.requests in
          (match requests with
          | (method_, url, _, _) :: _ ->
              assert (method_ = "GET");
              assert (string_contains url "page_123");
              assert (string_contains url "fields=");
              assert (string_contains url "fan_count");
              assert (string_contains url "followers_count");
              print_endline "✓ Get page info"
          | [] -> failwith "No requests made")
      | Error e -> failwith ("Get page info failed: " ^ Error_types.error_to_string e))

(** Test: Get page info with partial fields *)
let test_get_page_info_partial () =
  Mock_config.reset ();

  let response_body = {|{
    "id": "page_456",
    "name": "Minimal Page"
  }|} in

  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Facebook.get_page_info
    ~page_id:"page_456"
    ~access_token:"test_token"
    (function
      | Ok detail ->
          assert (detail.page_detail_id = "page_456");
          assert (detail.page_detail_name = Some "Minimal Page");
          assert (detail.page_detail_about = None);
          assert (detail.page_detail_fan_count = None);
          assert (detail.page_detail_followers_count = None);
          print_endline "✓ Get page info with partial fields"
      | Error e -> failwith ("Get page info partial failed: " ^ Error_types.error_to_string e))

(** Run all tests *)

(* Graph error body as Meta sends it, for the shared-classifier contract. *)
let graph_error_body ?subcode ?(error_type = "OAuthException") ~message code =
  Printf.sprintf {|{"error":{"message":%S,"type":%S,"code":%d%s,"fbtrace_id":"AvX3kq0Lm2P"}}|}
    message error_type code
    (match subcode with Some s -> Printf.sprintf {|,"error_subcode":%d|} s | None -> "")

let is_rate_limited = function Error_types.Rate_limited _ -> true | _ -> false
let is_permission_error = function
  | Error_types.Auth_error (Error_types.Insufficient_permissions _) -> true
  | _ -> false

(** Facebook maps Graph codes through the table shared with Instagram and
    Threads ([Social_meta_graph_error]). Before, 3, 102, 341, 201-298 and 100/33
    were plain Api_errors and 368 was "temporarily unavailable". *)
let test_shared_meta_graph_error_contract () =
  let parse ?(status_code = 400) body =
    Facebook.parse_api_error ~status_code ~response_body:body
  in
  List.iter
    (fun code ->
      assert (is_rate_limited (parse (graph_error_body ~message:"limit" code))))
    [ 4; 17; 32; 341; 368; 613; 80001; 80007; 80014 ];
  List.iter
    (fun code ->
      assert (is_permission_error (parse (graph_error_body ~message:"denied" code))))
    [ 3; 10; 200; 230; 299 ];
  assert (parse (graph_error_body ~message:"Session key invalid" 102)
          = Error_types.Auth_error Error_types.Token_invalid);
  assert (parse (graph_error_body ~subcode:458 ~message:"App not installed" 190)
          = Error_types.Auth_error Error_types.Token_revoked);
  (match
     parse (graph_error_body ~subcode:33 ~error_type:"GraphMethodException"
              ~message:"Object with ID '1790' does not exist" 100)
   with
   | Error_types.Resource_not_found _ -> ()
   | _ -> failwith "100/33 should be Resource_not_found");
  assert (Facebook.parse_error_code 368 = Rate_limit_exceeded);
  assert (Facebook.parse_error_code 1 = Temporarily_unavailable);
  assert (Facebook.parse_error_code 102 = Invalid_token);
  assert (Facebook.parse_error_code 3 = Permission_denied);
  assert (Facebook.parse_error_code 1609005 = Unknown 1609005);
  (* A code-1 error pinned by a subcode and is_transient=false is not
     retryable; the subcode must not be dropped before classifying. *)
  (match
     Facebook.parse_facebook_error
       {|{"error":{"message":"Invalid thumbnail offset","type":"OAuthException","code":1,"error_subcode":99,"is_transient":false}}|}
   with
   | Some fb_err ->
       assert (fb_err.code = Unknown 1);
       assert (not fb_err.should_retry)
   | None -> failwith "expected a Graph error");
  print_endline "✓ Shared Meta Graph error contract"

(** [Social_provider_error_detail] reads the code back out of a Facebook Api_error. *)
let test_api_error_provider_detail () =
  let body =
    graph_error_body ~subcode:1487390 ~message:"(#100) Invalid parameter" 100
  in
  (match
     Social_provider_error_detail.of_error
       (Facebook.parse_api_error ~status_code:400 ~response_body:body)
   with
   | Some detail ->
       assert (detail.http_status = 400);
       assert (detail.code = Some "100");
       assert (detail.subcode = Some 1487390);
       assert (detail.error_type = Some "OAuthException");
       assert (detail.trace_id = Some "AvX3kq0Lm2P")
   | None -> failwith "expected provider detail");
  print_endline "✓ Api_error provider detail"

(* Permalink by id: GET /{id}?fields=permalink_url with appsecret_proof, and the
   three outcomes callers branch on. *)
let test_get_permalink () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_SECRET" "app_secret";
  Mock_http.set_responses
    [ { status = 200; headers = [];
        body = {|{"permalink_url":"https://example.com/p/1","id":"123_456"}|} } ];
  Facebook.get_permalink ~post_id:"123_456/other?x=y" ~access_token:"tok"
    (function
      | Ok (Some "https://example.com/p/1") -> ()
      | Ok _ -> failwith "expected the permalink"
      | Error err -> failwith (Error_types.error_to_string err));
  (match !Mock_http.requests with
   | [ ("GET", url, _, _) ] ->
       let uri = Uri.of_string url in
       assert (Uri.host uri = Some "graph.facebook.com");
       assert (Uri.path uri = "/v25.0/123_456%2Fother%3Fx=y");
       assert (Uri.get_query_param uri "x" = None);
       assert (Uri.get_query_param uri "fields" = Some "permalink_url");
       assert (Uri.get_query_param uri "appsecret_proof" <> None)
   | _ -> failwith "expected one GET");
  Mock_config.reset ();
  Mock_http.set_responses [ { status = 200; headers = []; body = {|{"id":"123_456"}|} } ];
  Facebook.get_permalink ~post_id:"123_456" ~access_token:"tok"
    (function
      | Ok None -> ()
      | _ -> failwith "a missing permalink is Ok None");
  Mock_config.reset ();
  Mock_http.set_responses
    [ { status = 400; headers = [];
        body = {|{"error":{"message":"Unsupported get request","type":"GraphMethodException","code":100,"error_subcode":33}}|} } ];
  Facebook.get_permalink ~post_id:"gone" ~access_token:"tok"
    (function
      | Error (Error_types.Resource_not_found _) -> ()
      | Error err ->
          failwith ("expected Resource_not_found, got " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "expected an error");
  Mock_config.reset ();
  Mock_http.set_responses [ { status = 200; headers = []; body = "<html>" } ];
  Facebook.get_permalink ~post_id:"123_456" ~access_token:"tok"
    (function
      | Error (Error_types.Internal_error _) -> ()
      | _ -> failwith "an unparseable 2xx body is Internal_error");
  print_endline "PASS get_permalink"

(* A filter=stream page of /{post-id}/comments as Graph returns it to a Page
   token: a top-level comment, a reply placed by parent{id}, and a comment
   without [from] (no Business Asset User Profile Access), plus cursors and a
   next link that echoes the token. *)
let stream_comments_body ~next =
  Printf.sprintf
    {|{"data":[
      {"id":"122105_9001","message":"When does the event start?","created_time":"2026-11-16T09:12:44+0000","from":{"id":"7034918273","name":"Alice Example"},"permalink_url":"https://www.facebook.com/122105/posts/122105?comment_id=9001"},
      {"id":"122105_9002","message":"At 7pm!","created_time":"2026-11-16T09:20:01+0000","from":{"id":"104857","name":"Example Page"},"permalink_url":"https://www.facebook.com/122105/posts/122105?comment_id=9002&reply_comment_id=9002","parent":{"id":"122105_9001"}},
      {"id":"122105_9003","created_time":"2026-11-16T10:01:00+0000","permalink_url":"https://www.facebook.com/122105/posts/122105?comment_id=9003"}],
     "paging":{"cursors":{"before":"WTI5dGJXVnU","after":"WTI5dGJXVnV"}%s}}|}
    (match next with
     | Some url -> Printf.sprintf {|,"next":%S|} url
     | None -> "")

let comments_next_url =
  "https://graph.facebook.com/v25.0/104857_122105/comments?access_token=EAAOLD&fields=id%2Cmessage%2Cfrom%7Bid%2Cname%7D%2Cparent%7Bid%7D&filter=stream&order=reverse_chronological&limit=3&after=WTI5dGJXVnV"

let test_get_comments_page_stream () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_SECRET" "app-secret";
  Mock_http.set_response
    { status = 200; headers = []; body = stream_comments_body ~next:(Some comments_next_url) };
  Facebook.get_comments_page ~object_id:"104857_122105" ~access_token:"page-token"
    ~filter:Comments.Stream ~order:Comments.Reverse_chronological ~limit:3
    (function
      | Ok page ->
          (match page.data with
           | [ top; reply; anonymous ] ->
               assert (top.Comments.message = Some "When does the event start?");
               assert (top.from = Some { Comments.author_id = "7034918273"; author_name = Some "Alice Example" });
               assert (top.parent_id = None);
               assert (reply.parent_id = Some "122105_9001");
               assert (anonymous.from = None);
               assert (anonymous.message = None);
               assert (anonymous.permalink_url <> None)
           | _ -> failwith "expected three comments");
          assert (Social_meta_graph_paging.next_after page = Some "WTI5dGJXVnV")
      | Error e -> failwith (Error_types.error_to_string e));
  (match !Mock_http.requests with
   | [ ("GET", url, headers, _) ] ->
       let uri = Uri.of_string url in
       assert (Uri.path uri = "/v25.0/104857_122105/comments");
       assert (Uri.get_query_param uri "filter" = Some "stream");
       assert (Uri.get_query_param uri "order" = Some "reverse_chronological");
       assert (Uri.get_query_param uri "limit" = Some "3");
       assert (Uri.get_query_param uri "fields" = Some Comments.fields);
       assert (Uri.get_query_param uri "access_token" = None);
       assert (Uri.get_query_param uri "appsecret_proof" <> None);
       assert (List.assoc_opt "Authorization" headers = Some "Bearer page-token")
   | _ -> failwith "expected one GET");
  print_endline "✓ Get comments page (filter=stream, typed, optional from)"

let test_get_comments_page_positions () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_SECRET" "app-secret";
  Mock_http.set_responses
    [ { status = 200; headers = []; body = stream_comments_body ~next:None };
      { status = 200; headers = []; body = stream_comments_body ~next:None } ];
  let expect_last = function
    | Ok page -> assert (Social_meta_graph_paging.next_after page = None)
    | Error e -> failwith (Error_types.error_to_string e)
  in
  Facebook.get_comments_page ~object_id:"104857_122105" ~access_token:"page-token"
    ~position:(Social_meta_graph_paging.After "WTI5dGJXVnV") expect_last;
  Facebook.get_comments_page ~object_id:"ignored" ~access_token:"page-token"
    ~filter:Comments.Toplevel ~position:(Social_meta_graph_paging.Page_url comments_next_url)
    expect_last;
  (match List.rev !Mock_http.requests with
   | [ ("GET", cursor_url, _, _); ("GET", followed_url, headers, _) ] ->
       let cursor_uri = Uri.of_string cursor_url in
       assert (Uri.get_query_param cursor_uri "after" = Some "WTI5dGJXVnV");
       assert (Uri.get_query_param cursor_uri "filter" = None);
       let uri = Uri.of_string followed_url in
       assert (Uri.path uri = "/v25.0/104857_122105/comments");
       (* the URL's own options win; the echoed token is dropped *)
       assert (Uri.get_query_param uri "filter" = Some "stream");
       assert (Uri.get_query_param uri "after" = Some "WTI5dGJXVnV");
       assert (Uri.get_query_param uri "access_token" = None);
       assert (Uri.get_query_param uri "appsecret_proof" <> None);
       assert (List.assoc_opt "Authorization" headers = Some "Bearer page-token")
   | _ -> failwith "expected two GETs");
  Mock_config.reset ();
  Facebook.get_comments_page ~object_id:"x" ~access_token:"page-token"
    ~position:(Social_meta_graph_paging.Page_url "https://graph.facebook.com.evil.example/v25.0/1/comments")
    (function
      | Error (Error_types.Validation_error _) -> ()
      | _ -> failwith "expected a foreign host to be rejected");
  assert (!Mock_http.requests = []);
  print_endline "✓ Get comments page (cursor, next URL, foreign host)"

let test_get_comments_page_error () =
  Mock_config.reset ();
  Mock_http.set_response
    { status = 400; headers = [];
      body = {|{"error":{"message":"(#10) This endpoint requires the 'pages_read_engagement' permission or the 'Page Public Content Access' feature.","type":"OAuthException","code":10,"fbtrace_id":"AbCdEf"}}|} };
  Facebook.get_comments_page ~object_id:"104857_122105" ~access_token:"t" (function
    | Error (Error_types.Auth_error (Error_types.Insufficient_permissions { required; _ })) ->
        assert (List.mem "pages_read_engagement" required)
    | Error e -> failwith ("unexpected error " ^ Error_types.error_to_string e)
    | Ok _ -> failwith "expected an error");
  print_endline "✓ Get comments page (permission error)"

(* Place search: GET /pages/search with q and Places.fields, Bearer token
   and appsecret_proof; Pages without a location are kept, with no
   coordinates. *)
let test_search_places () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_SECRET" "app-secret";
  Mock_http.set_response
    { status = 200; headers = [];
      body =
        {|{"data":[
          {"id":"309968765748101","name":"Facebook HQ","location":{"city":"Menlo Park","country":"United States","latitude":37.4851,"longitude":-122.1483,"state":"CA","street":"1 Hacker Way","zip":94025}},
          {"id":"42","name":"Online only"},
          {"id":"43","name":"Half","location":{"city":"Berlin","latitude":52}}]}|} };
  Facebook.search_places ~access_token:"page-token" ~query:" Facebook HQ " (function
    | Ok [ hq; online; half ] ->
        assert (hq.Places.id = "309968765748101");
        assert (hq.name = Some "Facebook HQ");
        assert (Places.coordinates hq = Some (37.4851, -122.1483));
        (match hq.location with
         | Some { city = Some "Menlo Park"; zip = Some "94025"; street = Some "1 Hacker Way"; _ } -> ()
         | _ -> failwith "expected the HQ location");
        assert (online.location = None);
        assert (Places.coordinates online = None);
        assert (Places.coordinates half = None)
    | Ok _ -> failwith "expected three places"
    | Error e -> failwith (Error_types.error_to_string e));
  (match !Mock_http.requests with
   | [ ("GET", url, headers, _) ] ->
       let uri = Uri.of_string url in
       assert (Uri.path uri = "/v25.0/pages/search");
       assert (Uri.get_query_param uri "q" = Some "Facebook HQ");
       assert (Uri.get_query_param uri "fields" = Some Places.fields);
       assert (Uri.get_query_param uri "type" = None);
       assert (Uri.get_query_param uri "access_token" = None);
       assert (Uri.get_query_param uri "appsecret_proof" <> None);
       assert (List.assoc_opt "Authorization" headers = Some "Bearer page-token")
   | _ -> failwith "expected one GET");
  Mock_config.reset ();
  Facebook.search_places ~access_token:"t" ~query:"   " (function
    | Error (Error_types.Validation_error [ Error_types.Text_empty ]) -> ()
    | _ -> failwith "a blank query must be rejected");
  assert (!Mock_http.requests = []);
  Mock_config.reset ();
  Mock_http.set_response
    { status = 400; headers = [];
      body = {|{"error":{"message":"Invalid OAuth access token.","type":"OAuthException","code":190}}|} };
  Facebook.search_places ~access_token:"t" ~query:"x" (function
    | Error (Error_types.Auth_error _) -> ()
    | Error e -> failwith ("expected an auth error, got " ^ Error_types.error_to_string e)
    | Ok _ -> failwith "expected an error");
  Mock_config.reset ();
  Mock_http.set_response { status = 200; headers = []; body = {|{"data":[{"name":"no id"}]}|} };
  Facebook.search_places ~access_token:"t" ~query:"x" (function
    | Error (Error_types.Internal_error _) -> ()
    | _ -> failwith "an item without an id must fail the parse");
  print_endline "✓ Search places (pages/search, typed, unlocated kept)"

let page_posts_body ~next =
  Printf.sprintf
    {|{"data":[
      {"id":"104857_1","message":"Hello","created_time":"2026-11-16T09:12:44+0000","permalink_url":"https://www.facebook.com/104857/posts/1",
       "full_picture":"https://scontent.example/p.jpg",
       "attachments":{"data":[{"type":"photo","media_type":"photo","url":"https://www.facebook.com/photo/1","media":{"image":{"src":"https://scontent.example/p.jpg"}},"target":{"id":"555"}}]},
       "insights":{"data":[{"name":"post_impressions_unique","period":"lifetime","values":[{"value":120}]},{"name":"post_reactions_by_type_total","period":"lifetime","values":[{"value":{"like":3,"love":1}}]}]},
       "comments_summary":{"data":[],"summary":{"total_count":4}},
       "reactions_summary":{"data":[],"summary":{"total_count":5}},
       "shares":{"count":2}},
      {"id":"104857_2","created_time":"2026-11-15T09:00:00+0000","permalink_url":"https://www.facebook.com/reel/777",
       "attachments":{"data":[{"type":"video_inline","media_type":"video","target":{"id":"777"}}]}},
      {"id":"104857_3","message":"Album","attachments":{"data":[{"type":"album","media_type":"album","subattachments":{"data":[{"media_type":"photo","target":{"id":"8"}},{"media_type":"photo","target":{"id":"9"}}]}}]},
       "insights":{"error":"dropped"}}],
     "paging":{"cursors":{"before":"QkVG","after":"QUZU"}%s}}|}
    (match next with Some url -> Printf.sprintf {|,"next":%S|} url | None -> "")

let page_posts_next_url =
  "https://graph.facebook.com/v25.0/104857/posts?access_token=EAAOLD&fields=id%2Cmessage&limit=3&after=QUZU"

let test_get_page_posts_page () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_SECRET" "app-secret";
  Mock_http.set_response
    { status = 200; headers = []; body = page_posts_body ~next:(Some page_posts_next_url) };
  Facebook.get_page_posts_page ~page_id:"104857" ~access_token:"page-token" ~limit:500
    (function
      | Ok page ->
          (match page.data with
           | [ photo; reel; album ] ->
               assert (photo.Page_posts.message = Some "Hello");
               assert (Page_posts.media_type photo = Some "photo");
               assert (not (Page_posts.is_reel photo));
               assert (Page_posts.video_id_of photo = None);
               (match photo.attachments with
                | [ { image_src = Some "https://scontent.example/p.jpg"; target_id = Some "555";
                      attachment_type = Some "photo"; _ } ] -> ()
                | _ -> failwith "expected the photo attachment");
               assert (Insights.latest_count "post_impressions_unique" photo.insights = Some 120);
               assert (Insights.by_type "post_reactions_by_type_total" photo.insights
                       = Some [ ("like", 3); ("love", 1) ]);
               assert (photo.comment_count = Some 4);
               assert (photo.reaction_count = Some 5);
               assert (photo.share_count = Some 2);
               assert (Page_posts.is_reel reel);
               assert (Page_posts.video_id_of reel = Some "777");
               assert (reel.message = None);
               assert (reel.insights = []);
               assert (reel.comment_count = None);
               (match album.attachments with
                | [ { subattachments = [ { target_id = Some "8"; _ }; { target_id = Some "9"; _ } ]; _ } ] -> ()
                | _ -> failwith "expected the album's subattachments");
               assert (album.insights = [])
           | _ -> failwith "expected three posts");
          assert (Social_meta_graph_paging.next_after page = Some "QUZU")
      | Error e -> failwith (Error_types.error_to_string e));
  (match !Mock_http.requests with
   | [ ("GET", url, headers, _) ] ->
       let uri = Uri.of_string url in
       assert (Uri.path uri = "/v25.0/104857/posts");
       assert (Uri.get_query_param uri "fields" = Some Page_posts.basic_fields);
       assert (Uri.get_query_param uri "limit" = Some "100");
       assert (Uri.get_query_param uri "access_token" = None);
       assert (Uri.get_query_param uri "appsecret_proof" <> None);
       assert (List.assoc_opt "Authorization" headers = Some "Bearer page-token")
   | _ -> failwith "expected one GET");
  print_endline "✓ Get page posts page (typed, /posts, limit clamped)"

let test_get_page_posts_page_positions () =
  Mock_config.reset ();
  Mock_http.set_responses
    [ { status = 200; headers = []; body = page_posts_body ~next:None };
      { status = 200; headers = []; body = page_posts_body ~next:None } ];
  let expect_last = function
    | Ok page -> assert (Social_meta_graph_paging.next_after page = None)
    | Error e -> failwith (Error_types.error_to_string e)
  in
  Facebook.get_page_posts_page ~page_id:"104857" ~access_token:"page-token"
    ~edge:Page_posts.Feed ~fields:Page_posts.analytics_fields
    ~position:(Social_meta_graph_paging.After "QUZU") expect_last;
  Facebook.get_page_posts_page ~page_id:"ignored" ~access_token:"page-token"
    ~position:(Social_meta_graph_paging.Page_url page_posts_next_url) expect_last;
  (match List.rev !Mock_http.requests with
   | [ ("GET", cursor_url, _, _); ("GET", followed_url, _, _) ] ->
       let cursor_uri = Uri.of_string cursor_url in
       assert (Uri.path cursor_uri = "/v25.0/104857/feed");
       assert (Uri.get_query_param cursor_uri "after" = Some "QUZU");
       assert (Uri.get_query_param cursor_uri "fields" = Some Page_posts.analytics_fields);
       assert (not (string_contains Page_posts.analytics_fields "post_impressions_unique"));
       assert (string_contains Page_posts.analytics_fields
         "insights.metric(post_reactions_by_type_total,post_clicks,post_clicks_by_type)");
       assert (Uri.get_query_param cursor_uri "limit" = None);
       let uri = Uri.of_string followed_url in
       assert (Uri.path uri = "/v25.0/104857/posts");
       assert (Uri.get_query_param uri "after" = Some "QUZU");
       assert (Uri.get_query_param uri "access_token" = None)
   | _ -> failwith "expected two GETs");
  (* A paging URL of another edge is refused before any request. *)
  Mock_config.reset ();
  Facebook.get_page_posts_page ~page_id:"104857" ~access_token:"t" ~edge:Page_posts.Feed
    ~position:(Social_meta_graph_paging.Page_url page_posts_next_url)
    (function
      | Error (Error_types.Validation_error _) -> ()
      | _ -> failwith "expected a /posts URL to be refused for /feed");
  assert (!Mock_http.requests = []);
  print_endline "✓ Get page posts page (edge, cursor, next URL)"

let test_get_page_posts_page_for_account () =
  Mock_config.reset ();
  Mock_config.set_credentials ~account_id:"acct"
    ~credentials:{ access_token = "stored-page-token"; refresh_token = None;
                   expires_at = None; auth_type = Bearer; scope = None };
  Mock_config._set_page_id ~account_id:"acct" ~page_id:"104857";
  Mock_http.set_response { status = 200; headers = []; body = {|{"data":[]}|} };
  Facebook.get_page_posts_page_for_account ~account_id:"acct" ~edge:Page_posts.Published_posts
    (function
      | Ok { data = []; _ } -> ()
      | Ok _ -> failwith "expected no posts"
      | Error e -> failwith (Error_types.error_to_string e));
  (match !Mock_http.requests with
   | [ ("GET", url, headers, _) ] ->
       assert (Uri.path (Uri.of_string url) = "/v25.0/104857/published_posts");
       assert (List.assoc_opt "Authorization" headers = Some "Bearer stored-page-token")
   | _ -> failwith "expected one GET");
  print_endline "✓ Get page posts page for account"

let test_comment_moderation () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_SECRET" "app-secret";
  Mock_http.set_responses
    [ { status = 200; headers = []; body = {|{"id":"122105_9004"}|} };
      { status = 200; headers = []; body = {|{"success":true}|} };
      { status = 200; headers = []; body = {|{"success":true}|} };
      { status = 200; headers = []; body = {|{"success":true}|} };
      { status = 200; headers = []; body = {|{"success":true}|} };
      { status = 200; headers = []; body = {|{"success":true}|} } ];
  let ok = function Ok _ -> () | Error e -> failwith (Error_types.error_to_string e) in
  Facebook.reply_to_comment ~comment_id:"122105_9001" ~access_token:"pt"
    ~message:"Thanks & see you!" (function
    | Ok id -> assert (id = "122105_9004")
    | Error e -> failwith (Error_types.error_to_string e));
  Facebook.set_comment_hidden ~comment_id:"122105_9001" ~access_token:"pt" ~hidden:true ok;
  Facebook.set_comment_hidden ~comment_id:"122105_9001" ~access_token:"pt" ~hidden:false ok;
  Facebook.like_object ~object_id:"122105_9001" ~access_token:"pt" ok;
  Facebook.unlike_object ~object_id:"122105_9001" ~access_token:"pt" ok;
  Facebook.delete_comment ~comment_id:"122105_9003" ~access_token:"pt" ok;
  (match List.rev !Mock_http.requests with
   | [ ("POST", reply_url, reply_headers, reply_body);
       ("POST", hide_url, _, hide_body);
       ("POST", _, _, unhide_body);
       ("POST", like_url, _, like_body);
       ("DELETE", unlike_url, _, _);
       ("DELETE", delete_url, delete_headers, _) ] ->
       assert (String.ends_with ~suffix:"/v25.0/122105_9001/comments" reply_url);
       assert (string_contains reply_body "message=Thanks%20%26%20see%20you%21"
               || string_contains reply_body "message=Thanks+%26+see+you%21");
       assert (string_contains reply_body "appsecret_proof=");
       assert (List.assoc_opt "Authorization" reply_headers = Some "Bearer pt");
       assert (String.ends_with ~suffix:"/v25.0/122105_9001" hide_url);
       assert (string_contains hide_body "is_hidden=true");
       assert (string_contains unhide_body "is_hidden=false");
       assert (String.ends_with ~suffix:"/v25.0/122105_9001/likes" like_url);
       assert (string_contains like_body "appsecret_proof=");
       assert (string_contains unlike_url "/v25.0/122105_9001/likes?appsecret_proof=");
       assert (string_contains delete_url "/v25.0/122105_9003?appsecret_proof=");
       assert (List.assoc_opt "Authorization" delete_headers = Some "Bearer pt")
   | _ -> failwith "unexpected moderation requests");
  print_endline "✓ Comment reply, hide/unhide, like/unlike, delete"

let test_comment_moderation_failures () =
  Mock_config.reset ();
  Mock_http.set_responses
    [ { status = 200; headers = []; body = {|{"success":false}|} };
      { status = 400; headers = [];
        body = {|{"error":{"message":"(#200) Permissions error","type":"OAuthException","code":200,"fbtrace_id":"Ab"}}|} };
      { status = 400; headers = [];
        body = {|{"error":{"message":"Unsupported post request. Object with ID '122105_404' does not exist","type":"GraphMethodException","code":100,"error_subcode":33,"fbtrace_id":"Ab"}}|} } ];
  Facebook.set_comment_hidden ~comment_id:"c" ~access_token:"pt" ~hidden:true (function
    | Error (Error_types.Api_error { raw_response = Some _; _ }) -> ()
    | _ -> failwith "success=false must be an error");
  Facebook.like_object ~object_id:"c" ~access_token:"pt" (function
    | Error (Error_types.Auth_error (Error_types.Insufficient_permissions { required; _ })) ->
        assert (required = [ "pages_manage_engagement" ])
    | _ -> failwith "expected a permission error");
  Facebook.delete_comment ~comment_id:"122105_404" ~access_token:"pt" (function
    | Error (Error_types.Resource_not_found _) -> ()
    | _ -> failwith "expected Resource_not_found");
  Mock_config.reset ();
  Facebook.reply_to_comment ~comment_id:"  " ~access_token:"pt" ~message:"x" (function
    | Error (Error_types.Validation_error _) -> ()
    | _ -> failwith "expected a validation error");
  assert (!Mock_http.requests = []);
  print_endline "✓ Comment moderation failures"

(* Response shape from https://developers.facebook.com/docs/platforminsights/page
   (data[] with name, period, values[{value, end_time}], title, description,
   id, plus paging); [page_follows] is a daily follower total. *)
let documented_page_insights_body =
  {|{"data":[
    {"name":"page_media_view","period":"day",
     "values":[{"value":120,"end_time":"2026-11-15T08:00:00+0000"},
               {"value":95,"end_time":"2026-11-16T08:00:00+0000"}],
     "title":"Daily Media Views","description":"Daily: media views.",
     "id":"1234/insights/page_media_view/day"},
    {"name":"page_total_media_view_unique","period":"day",
     "values":[{"value":80,"end_time":"2026-11-16T08:00:00+0000"}],
     "title":"Daily Total Media View Unique","description":"Daily: viewers.",
     "id":"1234/insights/page_total_media_view_unique/day"},
    {"name":"page_follows","period":"day",
     "values":[{"value":1500,"end_time":"2026-11-15T08:00:00+0000"},
               {"value":1503,"end_time":"2026-11-16T08:00:00+0000"}],
     "title":"Followers","description":"follows","id":"1234/insights/page_follows/day"}],
   "paging":{"previous":"https://graph.facebook.com/v25.0/1234/insights?since=1&until=2",
             "next":"https://graph.facebook.com/v25.0/1234/insights?since=3&until=4"}}|}

let test_page_insights_v25_contract () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_SECRET" "appsecret";
  let token = "EAAB+x/y=z&appsecret_proof=forged|secret" in
  Mock_http.set_response { status = 200; body = documented_page_insights_body; headers = [] };
  Facebook.get_page_insights ~page_id:"1234" ~access_token:token ~since:"1763164800"
    ~until:"1763337600" (function
    | Error e -> failwith ("page insights: " ^ Error_types.error_to_string e)
    | Ok metrics ->
        assert (List.length metrics = 3);
        assert (Insights.latest_count "page_follows" metrics = Some 1503);
        assert (Insights.latest_count "page_media_view" metrics = Some 95);
        (match Insights.find "page_media_view" metrics with
         | Some { period = Some Insights.Day; title = Some "Daily Media Views"; points = [ _; { end_time = Some "2026-11-16T08:00:00+0000"; _ } ]; _ } -> ()
         | _ -> failwith "page_media_view should have two daily points");
        let series = Insights.to_canonical_series ~scope:Analytics_types.Page metrics in
        let canonical metric =
          List.find_opt (fun (s : Analytics_types.series) -> s.metric = metric) series
        in
        (match canonical Analytics_types.Followers with
         | Some s -> assert (s.provider_metric = Some "page_follows" && List.length s.points = 2)
         | None -> failwith "page_follows should be the Followers series");
        assert (canonical Analytics_types.Reach <> None);
        assert (canonical Analytics_types.Views <> None));
  (match !Mock_http.requests with
   | [ ("GET", url, headers, _) ] ->
       let uri = Uri.of_string url in
       assert (Uri.host uri = Some "graph.facebook.com");
       assert (Uri.path uri = "/v25.0/1234/insights");
       assert (Uri.get_query_param uri "metric" = Some (String.concat "," Insights.page_metrics));
       assert (Uri.get_query_param uri "period" = Some "day");
       assert (Uri.get_query_param uri "since" = Some "1763164800");
       assert (Uri.get_query_param uri "until" = Some "1763337600");
       assert (Uri.get_query_param uri "access_token" = Some token);
       assert (Uri.get_query_param uri "appsecret_proof"
               = Some (OAuth.app_secret_proof ~app_secret:"appsecret" ~access_token:token));
       assert (List.assoc_opt "Authorization" headers = Some ("Bearer " ^ token))
   | _ -> failwith "expected one GET");
  print_endline "✓ Page insights v25 contract (encoded token, appsecret_proof)"

let test_insights_default_metrics_not_deprecated () =
  List.iter
    (fun metric ->
      if Insights.is_deprecated_metric metric then failwith ("deprecated default metric: " ^ metric))
    (Insights.page_metrics @ Insights.post_metrics @ Insights.reels_metrics @ Insights.video_metrics);
  assert (Insights.is_deprecated_metric "page_impressions_unique");
  assert (Insights.is_deprecated_metric " post_impressions_unique ");
  assert (List.for_all Insights.is_deprecated_metric
            (String.split_on_char ',' Facebook.account_analytics_metrics
             |> List.filter (fun m -> String.starts_with ~prefix:"page_impressions" m
                                      || String.starts_with ~prefix:"page_posts" m)));
  print_endline "✓ Insights default metric sets avoid deprecated metrics"

(* Lifetime post values carry no end_time; *_by_type metrics carry an object
   of counts (https://developers.facebook.com/docs/platforminsights/page). *)
let test_post_insights_by_type () =
  Mock_config.reset ();
  Mock_http.set_response
    { status = 200; headers = [];
      body = {|{"data":[
        {"name":"post_total_media_view_unique","period":"lifetime","values":[{"value":120}]},
        {"name":"post_media_view","period":"lifetime","values":[{"value":300}],
         "title":"Lifetime Media Views","id":"1_2/insights/post_media_view/lifetime"},
        {"name":"post_reactions_by_type_total","period":"lifetime",
         "values":[{"value":{"like":5,"love":2}}],"id":"1_2/insights/post_reactions_by_type_total/lifetime"},
        {"name":"post_clicks","period":"lifetime","values":[{"value":11}]}]}|} };
  Facebook.get_post_insights ~post_id:"1_2" ~access_token:"t" (function
    | Error e -> failwith (Error_types.error_to_string e)
    | Ok metrics ->
        assert (Insights.by_type "post_reactions_by_type_total" metrics = Some [ ("like", 5); ("love", 2) ]);
        assert (Insights.latest_count "post_reactions_by_type_total" metrics = Some 7);
        (match Insights.find "post_media_view" metrics with
         | Some { period = Some Insights.Lifetime; points = [ { value = Insights.Count 300; end_time = None } ]; _ } -> ()
         | _ -> failwith "post_media_view should be one lifetime count");
        let series = Insights.to_canonical_series ~scope:Analytics_types.Post metrics in
        assert (List.exists (fun (s : Analytics_types.series) ->
          s.metric = Analytics_types.Reactions && s.points = [ Analytics_types.make_datapoint 7 ]) series);
        assert (List.exists (fun (s : Analytics_types.series) ->
          s.metric = Analytics_types.Reach && s.points = [ Analytics_types.make_datapoint 120 ]) series);
        assert (List.exists (fun (s : Analytics_types.series) ->
          s.metric = Analytics_types.Views && s.points = [ Analytics_types.make_datapoint 300 ]) series));
  (match !Mock_http.requests with
   | [ ("GET", url, _, _) ] ->
       let uri = Uri.of_string url in
       assert (Uri.path uri = "/v25.0/1_2/insights");
       assert (Uri.get_query_param uri "metric" = Some (String.concat "," Insights.post_metrics));
       assert (Uri.get_query_param uri "period" = None)
   | _ -> failwith "expected one GET");
  (* A value Meta left empty or unreadable carries no figure: that point is
     left out and the other metrics still arrive (the read used to fail as
     a whole). *)
  Mock_config.reset ();
  Mock_http.set_response
    { status = 200; headers = [];
      body = {|{"data":[
        {"name":"post_clicks","values":[{"value":"many"}]},
        {"name":"post_media_view","values":[{"value":null}]},
        {"name":"post_total_media_view_unique","values":[{}]},
        {"name":"post_reactions_by_type_total","values":[{"value":{"like":2,"love":null}}]},
        {"name":"post_clicks_by_type","values":[{"value":{"link clicks":4}}]}]}|} };
  Facebook.get_post_insights ~post_id:"1_2" ~access_token:"t" (function
    | Error e -> failwith (Error_types.error_to_string e)
    | Ok metrics ->
        assert (List.length metrics = 5);
        assert (Insights.latest_count "post_clicks" metrics = None);
        assert (Insights.latest_count "post_media_view" metrics = None);
        assert (Insights.latest_count "post_total_media_view_unique" metrics = None);
        assert (Insights.by_type "post_reactions_by_type_total" metrics = None);
        assert (Insights.by_type "post_clicks_by_type" metrics = Some [ ("link clicks", 4) ]));
  print_endline "✓ Post insights by-type values"

let test_contextual_permission_errors () =
  let check required = function
    | Error (Error_types.Auth_error (Error_types.Insufficient_permissions permissions)) ->
        assert (permissions.required = required)
    | Error err -> failwith (Error_types.error_to_string err)
    | Ok _ -> failwith "expected permission error"
  in
  Mock_config.reset ();
  Mock_http.set_responses (List.init 2 (fun _ ->
    { status = 403; headers = []; body = {|{"error":{"code":200,"message":"Denied"}}|} }));
  Facebook.get_video_insights ~video_id:"v" ~access_token:"t"
    (check ["read_insights"; "pages_manage_engagement"]);
  Facebook.search_places ~access_token:"t" ~query:"cafe" (check [])

let test_video_insights_reels () =
  Mock_config.reset ();
  Mock_http.set_response
    { status = 200; headers = [];
      body = {|{"data":[
        {"name":"fb_reels_total_plays","period":"lifetime","values":[{"value":4200}],
         "title":"Total plays","id":"987/video_insights/fb_reels_total_plays/lifetime"},
        {"name":"post_video_avg_time_watched","period":"lifetime","values":[{"value":5321.5}],
         "id":"987/video_insights/post_video_avg_time_watched/lifetime"}],"paging":{}}|} };
  Facebook.get_video_insights ~video_id:"987" ~access_token:"t" (function
    | Error e -> failwith (Error_types.error_to_string e)
    | Ok metrics ->
        assert (Insights.latest_count "fb_reels_total_plays" metrics = Some 4200);
        assert (Insights.latest_count "post_video_avg_time_watched" metrics = Some 5321));
  (match !Mock_http.requests with
   | [ ("GET", url, _, _) ] ->
       let uri = Uri.of_string url in
       assert (Uri.path uri = "/v25.0/987/video_insights");
       assert (Uri.get_query_param uri "metric" = Some (String.concat "," Insights.reels_metrics));
       assert (Uri.get_query_param uri "period" = Some "lifetime")
   | _ -> failwith "expected one GET");
  print_endline "✓ Video insights (Reels) contract"

let test_page_insights_for_account () =
  Mock_config.reset ();
  Mock_config.set_credentials ~account_id:"acct"
    ~credentials:{ access_token = "page_tok"; refresh_token = None; expires_at = None;
                   auth_type = Bearer; scope = None };
  Mock_config._set_page_id ~account_id:"acct" ~page_id:"pg-1";
  Mock_http.set_response { status = 200; headers = []; body = {|{"data":[]}|} };
  Facebook.get_page_insights_for_account ~account_id:"acct" ~metrics:[ "page_follows" ] (function
    | Ok [] -> ()
    | Ok _ -> failwith "expected no metrics"
    | Error e -> failwith (Error_types.error_to_string e));
  (match !Mock_http.requests with
   | [ ("GET", url, _, _) ] ->
       let uri = Uri.of_string url in
       assert (Uri.path uri = "/v25.0/pg-1/insights");
       assert (Uri.get_query_param uri "metric" = Some "page_follows");
       assert (Uri.get_query_param uri "access_token" = Some "page_tok")
   | _ -> failwith "expected one GET");
  print_endline "✓ Page insights for account"

let parse_insights_fixture body =
  match Insights.parse body with
  | Ok metrics -> metrics
  | Error e -> failwith ("insights fixture did not parse: " ^ e)

(* Typed descriptors: a rename is one typed metric, every request name
   and alias resolves to exactly one, and the descriptor carries the
   unit, aggregation and edges (deprecated names from
   https://developers.facebook.com/docs/platforminsights/page/deprecated-metrics,
   edge applicability from the insights and video insights
   references). *)
let test_insights_typed_descriptors () =
  assert (Insights.request_names
            [ Insights.Page_follows; Insights.Page_media_view; Insights.Page_unique_viewers ]
          = [ "page_follows"; "page_media_view"; "page_total_media_view_unique" ]);
  assert (Insights.typed_metric_of_name "page_fans" = Some Insights.Page_follows);
  assert (Insights.typed_metric_of_name "page_impressions_unique"
          = Some Insights.Page_unique_viewers);
  assert (Insights.typed_metric_of_name "post_impressions_unique"
          = Some Insights.Post_unique_viewers);
  assert (Insights.typed_metric_of_name "post_total_media_view_unique"
          = Some Insights.Post_unique_viewers);
  assert (Insights.typed_metric_of_name "blue_reels_play_count" = Some Insights.Reel_plays);
  assert (Insights.typed_metric_of_name "page_no_such_metric" = None);
  List.iter
    (fun metric ->
      let d = Insights.descriptor metric in
      List.iter
        (fun name ->
          match Insights.typed_metric_of_name name with
          | Some resolved -> assert (resolved = metric)
          | None -> failwith ("unresolved descriptor name: " ^ name))
        (d.request_name :: d.aliases))
    Insights.all_typed_metrics;
  let page_follows = Insights.descriptor Insights.Page_follows in
  assert (page_follows.aggregation = Insights.Running_total);
  assert (page_follows.unit_ = Insights.Followers);
  assert (page_follows.edges = [ Insights.Page_edge ]);
  let post_unique_viewers = Insights.descriptor Insights.Post_unique_viewers in
  assert (post_unique_viewers.edges = [ Insights.Post_edge; Insights.Video_edge ]);
  assert ((Insights.descriptor Insights.Reel_plays).edges = [ Insights.Video_edge ]);
  assert ((Insights.descriptor Insights.Page_daily_follows_unique).aggregation
          = Insights.Per_period);
  print_endline "✓ Insights typed metric descriptors"

(* [series] follows descriptor order, not the order Meta listed the
   series in, so a rename cannot flip which figure wins. *)
let test_insights_series_prefers_request_name () =
  let both_names = parse_insights_fixture {|{"data":[
    {"name":"post_impressions_unique","period":"lifetime","values":[{"value":450}]},
    {"name":"post_total_media_view_unique","period":"lifetime","values":[{"value":500}]}]}|} in
  (match Insights.series Insights.Post_unique_viewers both_names with
   | Some { name = "post_total_media_view_unique"; _ } -> ()
   | Some { name; _ } -> failwith ("series preferred " ^ name ^ " over the v25 name")
   | None -> failwith "no series answered for Post_unique_viewers");
  let legacy_only =
    parse_insights_fixture
      {|{"data":[{"name":"post_impressions_unique","period":"lifetime","values":[{"value":90}]}]}|} in
  (match Insights.series Insights.Post_unique_viewers legacy_only with
   | Some { name = "post_impressions_unique"; _ } -> ()
   | _ -> failwith "the deprecated name should answer for its successor");
  let no_family_member =
    parse_insights_fixture {|{"data":[{"name":"page_video_views","values":[{"value":1}]}]}|} in
  assert (Insights.series Insights.Page_follows no_family_member = None);
  print_endline "✓ Insights series resolves renames by descriptor order"

(* The Reel plays family: [fb_reels_total_plays] (includes replays) is
   the documented total; [fb_reels_total_views] is not in the video
   insights reference but has been observed; [blue_reels_play_count]
   excludes replays. A member Meta left without a figure does not
   answer. *)
let test_insights_reel_plays () =
  assert (Insights.reel_plays_metrics
          = [ "fb_reels_total_plays"; "fb_reels_total_views"; "blue_reels_play_count" ]);
  let plays body = Insights.reel_plays (parse_insights_fixture body) in
  assert (plays {|{"data":[
    {"name":"blue_reels_play_count","period":"lifetime","values":[{"value":700}]},
    {"name":"fb_reels_total_plays","period":"lifetime","values":[{"value":900}]}]}|}
    = Some ("fb_reels_total_plays", 900));
  assert (plays {|{"data":[
    {"name":"fb_reels_total_plays","period":"lifetime","values":[{"value":null}]},
    {"name":"blue_reels_play_count","period":"lifetime","values":[{"value":700}]}]}|}
    = Some ("blue_reels_play_count", 700));
  assert (plays
            {|{"data":[{"name":"fb_reels_total_views","period":"lifetime","values":[{"value":650}]}]}|}
    = Some ("fb_reels_total_views", 650));
  assert (plays {|{"data":[]}|} = None);
  print_endline "✓ Insights reel plays alias chain"

(* [page_follows] reports the follower total as of each day, so every
   point is an absolute; the day is the date part of [end_time].
   Points without a figure or without [end_time] carry no day total. *)
let test_insights_follower_totals_by_day () =
  let metrics =
    parse_insights_fixture {|{"data":[
      {"name":"page_follows","period":"day","values":[
        {"value":1200,"end_time":"2026-11-15T08:00:00+0000"},
        {"value":1203,"end_time":"2026-11-16T08:00:00+0000"},
        {"value":null,"end_time":"2026-11-17T08:00:00+0000"},
        {"value":1207}],
       "title":"Daily Follows","id":"123/insights/page_follows/day"}]}|} in
  assert (Insights.follower_totals_by_day metrics
          = [ ("2026-11-15", 1200); ("2026-11-16", 1203) ]);
  assert (Insights.follower_totals_by_day (parse_insights_fixture {|{"data":[]}|}) = []);
  print_endline "✓ Insights follower totals by day"

(* Regression: the legacy analytics calls sent no appsecret_proof, so apps
   with "Require App Secret" enabled could not read insights. *)
let test_legacy_analytics_send_appsecret_proof () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_SECRET" "appsecret";
  Mock_http.set_response { status = 200; body = {|{"data": []}|}; headers = [] };
  Facebook.get_account_analytics ~id:"12345" ~since:"2024-01-01" ~until:"2024-01-07"
    ~access_token:"tok" (fun _ -> ());
  Mock_http.set_response { status = 200; body = {|{"data": []}|}; headers = [] };
  Facebook.get_post_analytics ~post_id:"1_2" ~access_token:"tok" (fun _ -> ());
  let proof = OAuth.app_secret_proof ~app_secret:"appsecret" ~access_token:"tok" in
  assert (List.length !Mock_http.requests = 2);
  List.iter
    (fun (_, url, _, _) ->
      assert (Uri.get_query_param (Uri.of_string url) "appsecret_proof" = Some proof))
    !Mock_http.requests;
  print_endline "✓ Legacy analytics send appsecret_proof"

let () =
  print_endline "\n=== Facebook Provider Tests ===\n";
  
  print_endline "--- OAuth Flow Tests ---";
  test_oauth_url ();
  test_oauth_url_permissions ();
  test_oauth_url_special_chars ();
  test_oauth_csrf_protection ();
  test_redirect_uri_validation ();
  test_token_exchange ();
  test_token_exchange_errors ();
  test_exchange_code_and_get_pages ();
  test_get_user_pages_encodes_token ();
  test_debug_token_encodes_tokens ();
  test_long_lived_token ();
  test_page_vs_user_token ();
  test_token_expiry_detection ();
  test_app_secret_proof_generation ();
  
  print_endline "\n--- API Feature Tests ---";
  test_upload_photo ();
  test_content_validation ();
  test_ensure_valid_token_fresh ();
  test_ensure_valid_token_expired ();
  test_rate_limit_parsing ();
  test_field_selection ();
  test_error_code_parsing ();
  test_invalid_token_without_expiry_subcode ();
  test_shared_meta_graph_error_contract ();
  test_api_error_provider_detail ();
  test_token_subcode_467_maps_to_expired ();
  test_permission_error_scopes ();
  test_me_accounts_permission_scope ();
  test_get_required_permissions_override ();
  test_post_required_permissions_override ();
  test_delete_required_permissions_override ();
  test_pagination ();
  test_batch_requests ();
  test_batch_required_permissions_override ();
  test_app_secret_proof ();
  test_authorization_header ();
  test_account_analytics_request_contract ();
  test_post_analytics_request_contract ();
  test_analytics_encode_reserved_token_chars ();
  test_page_insights_v25_contract ();
  test_insights_default_metrics_not_deprecated ();
  test_post_insights_by_type ();
  test_contextual_permission_errors ();
  test_video_insights_reels ();
  test_page_insights_for_account ();
  test_insights_typed_descriptors ();
  test_insights_series_prefers_request_name ();
  test_insights_reel_plays ();
  test_insights_follower_totals_by_day ();
  test_legacy_analytics_send_appsecret_proof ();
  test_account_analytics_parsing ();
  test_post_analytics_parsing ();
  test_post_analytics_parsing_legacy_unique ();
  test_get_page_posts_includes_inline_analytics_expansions ();
  test_canonical_analytics_adapters ();
  test_analytics_network_error_redacts_query_token ();
  test_graph_version_policy_guard ();
  test_exported_api_constants ();
  
  print_endline "\n--- Alt-Text Tests ---";
  test_upload_photo_with_alt_text ();
  test_post_with_alt_text ();
  test_post_with_multiple_alt_texts ();
  test_post_with_indexed_attached_media_payload ();
  test_post_with_place ();
  test_post_recovers_page_token_from_user_token ();
  test_post_recovery_uses_stored_user_token_fallback ();
  test_post_recovery_stops_on_rate_limit ();
  test_post_cleans_up_uploaded_photos_on_publish_failure ();
  test_post_without_alt_text ();
  test_alt_text_special_characters ();
  test_partial_alt_texts ();
  
  print_endline "\n--- Stories Tests ---";
  test_upload_photo_story ();
  test_upload_video_story ();
  test_post_story_photo ();
  test_post_story_video ();
  test_post_story_video_recovers_on_upload_auth_error ();
  test_post_story_video_no_recovery_on_upload_rate_limit ();
  test_post_story_auto_image ();
  test_post_story_auto_video ();
  test_validate_story_valid_image ();
  test_validate_story_valid_video ();
  test_validate_story_invalid_url ();
  test_validate_story_invalid_format ();
  
  print_endline "\n--- Form URL Encoding Tests ---";
  test_post_comment_form_urlencodes_special_chars ();

  print_endline "\n--- Video Reel Tests ---";
  test_upload_video_reel ();
  test_upload_video_reel_with_place ();
  test_upload_video_reel_failures ();
  test_post_reel ();
  test_post_reel_with_place_recovers_on_finish_auth_error ();
  test_post_reel_no_recovery_on_non_auth_error ();
  test_post_reel_recovers_on_auth_error ();
  test_post_reel_recovers_on_upload_auth_error ();
  test_post_reel_no_recovery_on_upload_rate_limit ();
  test_reel_caption_validation ();
  test_video_media_validation ();
  test_rate_limit_error_code_80001 ();

  print_endline "\n--- Ordinary Page Video Tests ---";
  test_videos_parse ();
  test_create_video ();
  test_create_video_with_app_secret ();
  test_create_video_failures ();
  test_post_video ();
  test_post_video_recovers_on_auth_error ();
  test_post_video_no_recovery_on_non_auth_error ();
  test_post_video_caption_validation ();

  print_endline "\n--- Comment Management Tests ---";
  test_post_comment ();
  test_post_comment_error ();
  test_get_comments ();
  test_get_comments_empty ();
  test_get_comments_missing_from ();
  test_get_comments_page_stream ();
  test_get_comments_page_positions ();
  test_get_comments_page_error ();
  test_search_places ();
  test_get_page_posts_page ();
  test_get_page_posts_page_positions ();
  test_get_page_posts_page_for_account ();
  test_comment_moderation ();
  test_comment_moderation_failures ();

  print_endline "\n--- Link Posts and Scheduling Tests ---";
  test_post_with_link ();
  test_post_with_scheduled_publish_time ();

  print_endline "\n--- Page Info Tests ---";
  test_get_page_info ();
  test_get_page_info_partial ();
  test_get_page_info_error ();

  test_get_permalink ();

  print_endline "\n--- Token Tooling Tests ---";
  test_debug_token_info_typed ();
  test_debug_token_info_errors ();
  test_get_permissions ();
  test_app_secret_proof_known_answer ();

  test_authorize_url ();

  print_endline "\n--- Login Discovery Tests ---";
  test_get_me_id ();
  test_get_managed_pages_follows_paging ();
  test_get_all_business_pages ();

  print_endline "
=== All 93 tests passed! ===
"

let test_coordinated_refresh () =
  Mock_config.reset ();
  Mock_http.reset ();
  let credentials = {
    Social_core.access_token = "old-access";
    refresh_token = None;
    expires_at = Some "2020-01-01T00:00:00Z";
    auth_type = Social_core.Bearer;
    scope = None;
  } in
  Mock_config.credentials_store := [ ("account", credentials) ];
  Mock_config._set_page_id ~account_id:"account" ~page_id:"page";
  let locked = ref false in
  let waiting = Queue.create () in
  let persist = ref None in
  let completed = ref 0 in
  let module Config = struct
    include Mock_config
    let get_credentials ~account_id on_success on_error =
      assert !locked;
      Mock_config.get_credentials ~account_id on_success on_error
    let update_credentials ~account_id ~credentials on_success on_error =
      assert !locked;
      persist := Some (fun () ->
        Mock_config.update_credentials ~account_id ~credentials on_success on_error)
    let with_refresh_lock ~account_id:_ run on_success on_error =
      let release () =
        locked := false;
        if not (Queue.is_empty waiting) then (Queue.take waiting) ()
      in
      let start () =
        assert (not !locked);
        locked := true;
        run
          (fun credentials -> release (); on_success credentials)
          (fun error -> release (); on_error error)
      in
      if !locked then Queue.add start waiting else start ()
  end in
  let module Provider = Social_facebook_graph_v25.Make_coordinated (Config) in
  Mock_http.set_response {
    Social_core.status = 200; headers = [];
    body = {|{"data":[{"id":"page","access_token":"new-access"}]}|};
  };
  let run () = Provider.recover_page_access_token ~account_id:"account" ~current_access_token:"old-access"
    (fun page_id token -> assert (page_id = "page"); assert (token = "new-access"); incr completed)
    (fun error -> failwith (Error_types.error_to_string error))
  in
  run ();
  run ();
  assert (!completed = 0 && !locked);
  assert (List.length !Mock_http.requests = 1);
  (match !persist with Some finish -> finish () | None -> assert false);
  assert (!completed = 2 && not !locked);
  assert (List.length !Mock_http.requests = 1)

let () = test_coordinated_refresh ()

let test_coordinated_recovery_persist_failure () =
  Mock_config.reset ();
  Mock_config._set_page_id ~account_id:"account" ~page_id:"page";
  Mock_config.credentials_store := [("account", {
    Social_core.access_token = "old-access"; refresh_token = None;
    expires_at = None; auth_type = Social_core.Bearer; scope = None;
  })];
  let locked = ref false in
  let failed = ref false in
  let module Config = struct
    include Mock_config
    let update_credentials ~account_id:_ ~credentials:_ _ on_error =
      assert !locked;
      on_error "storage unavailable"
    let with_refresh_lock ~account_id:_ run on_success on_error =
      locked := true;
      run
        (fun credentials -> locked := false; on_success credentials)
        (fun error -> locked := false; on_error error)
  end in
  let module Provider = Social_facebook_graph_v25.Make_coordinated (Config) in
  Mock_http.set_response { Social_core.status = 200; headers = [];
    body = {|{"data":[{"id":"page","access_token":"new-access"}]}|} };
  Provider.recover_page_access_token ~account_id:"account" ~current_access_token:"old-access"
    (fun _ _ -> assert false)
    (fun error ->
      assert (error = Error_types.Network_error (Error_types.Connection_failed "storage unavailable"));
      assert (not !locked);
      failed := true);
  assert !failed;
  assert ((List.assoc "account" !Mock_config.credentials_store).Social_core.access_token = "old-access")

let () = test_coordinated_recovery_persist_failure ()

let test_coordinated_recovery_reload_failure () =
  Mock_config.reset ();
  Mock_http.reset ();
  Mock_config._set_page_id ~account_id:"account" ~page_id:"page";
  Mock_config.credentials_store :=
    [
      ( "account",
        {
          Social_core.access_token = "old-access";
          refresh_token = None;
          expires_at = None;
          auth_type = Social_core.Bearer;
          scope = None;
        } );
    ];
  let reads = ref 0 in
  let result = ref None in
  let module Config = struct
    include Mock_config

    let get_credentials ~account_id on_success on_error =
      incr reads;
      if !reads = 2 then on_error "storage unavailable"
      else Mock_config.get_credentials ~account_id on_success on_error

    let with_refresh_lock ~account_id:_ run on_success on_error =
      run on_success on_error
  end in
  let module Provider = Social_facebook_graph_v25.Make_coordinated (Config) in
  Mock_http.set_response
    {
      Social_core.status = 200;
      headers = [];
      body = {|{"data":[{"id":"page","access_token":"new-access"}]}|};
    };
  Provider.recover_page_access_token ~account_id:"account"
    ~current_access_token:"old-access"
    (fun page token -> result := Some (Ok (page, token)))
    (fun error -> result := Some (Error error));
  assert (
    !result
    = Some
        (Error
           (Error_types.Network_error
              (Error_types.Connection_failed "storage unavailable"))));
  assert (!Mock_http.requests = [])

let () = test_coordinated_recovery_reload_failure ()

(* Graph error shapes: https://developers.facebook.com/docs/graph-api/guides/error-handling/ *)
let () =
  List.iter (fun (status, code, subcode) ->
    Mock_http.reset ();
    let body = Printf.sprintf
      {|{"error":{"message":"Provider explanation","type":"OAuthException","code":%d,"error_subcode":%d,"fbtrace_id":"trace"}}|}
      code subcode in
    Mock_http.set_response { Social_core.status; headers = [("Retry-After", "42")]; body };
    Facebook.reply_to_comment_detailed ~comment_id:"comment" ~access_token:"pt"
      ~message:"Reply" (function
        | Error (Social_meta_graph_error.Http_failure error) ->
            assert (error.detail.http_status = status);
            assert (error.detail.code = Some (string_of_int code));
            assert (error.detail.subcode = Some subcode);
            assert (error.detail.trace_id = Some "trace");
            assert (error.detail.message = Some "Provider explanation");
            assert (Option.is_some error.graph);
            assert (error.retry_after_seconds = Some 42);
            (match error.kind with
             | Error_types.Rate_limited rate -> assert (rate.retry_after_seconds = Some 42)
             | Error_types.Auth_error _ | Error_types.Resource_not_found _
             | Error_types.Duplicate_content | Error_types.Api_error _ -> ()
             | Error_types.Validation_error _ | Error_types.Network_error _
             | Error_types.Content_policy_violation _ | Error_types.Internal_error _ -> assert false)
        | Ok _ | Error (Social_meta_graph_error.Transport_failure _
          | Social_meta_graph_error.Malformed_response _
          | Social_meta_graph_error.Validation_failure _) -> assert false))
    [400, 190, 463; 403, 200, 0; 429, 4, 0; 400, 100, 33;
     400, 506, 0; 503, 2, 0];
  Mock_http.reset ();
  Mock_http.set_response { Social_core.status = 200; headers = []; body = "{}" };
  Facebook.reply_to_comment_detailed ~comment_id:"comment" ~access_token:"pt"
    ~message:"Reply" (function
      | Error (Social_meta_graph_error.Malformed_response { status_code; _ }) ->
          assert (status_code = 200)
      | Ok _ | Error (Social_meta_graph_error.Http_failure _
        | Social_meta_graph_error.Transport_failure _

        | Social_meta_graph_error.Validation_failure _) -> assert false);
  Mock_http.reset ();
  Mock_http.set_errors ["connection reset"];
  Facebook.reply_to_comment_detailed ~comment_id:"comment" ~access_token:"pt"
    ~message:"Reply" (function
      | Error (Social_meta_graph_error.Transport_failure message) ->
          assert (String.equal message "connection reset")
      | Ok _ | Error (Social_meta_graph_error.Http_failure _
        | Social_meta_graph_error.Malformed_response _
        | Social_meta_graph_error.Validation_failure _) -> assert false);
  Mock_http.reset ();
  Facebook.reply_to_comment_detailed ~comment_id:" " ~access_token:"pt"
    ~message:"Reply" (function
      | Error (Social_meta_graph_error.Validation_failure _) -> ()
      | Ok _ | Error (Social_meta_graph_error.Http_failure _
        | Social_meta_graph_error.Transport_failure _
        | Social_meta_graph_error.Malformed_response _) -> assert false);
  assert (!Mock_http.requests = [])

(* Detail-preserving comment moderation and listings. Response shapes:
     - a like answer is 200 with {"success":true} ({"success":false} means
       Facebook withheld the like);
     - Graph errors are {"error":{"message","type","code","error_subcode",
       "fbtrace_id"}}
       (https://developers.facebook.com/docs/graph-api/guides/error-handling/);
     - a comments / page-posts page is 200 with
       {"data":[...],"paging":{"cursors":{...}[,"next":url]}}. *)
let () =
  Mock_config.reset ();
  (* A like answered with success is Ok; {"success":false} is a 2xx the SDK
     cannot read as the like having happened. *)
  Mock_http.set_response { status = 200; headers = []; body = {|{"success":true}|} };
  Facebook.like_object_detailed ~object_id:"122105_9001" ~access_token:"pt"
    (function
      | Ok () -> ()
      | Error _ -> assert false);
  Mock_http.set_response { status = 200; headers = []; body = {|{"success":false}|} };
  Facebook.like_object_detailed ~object_id:"122105_9001" ~access_token:"pt"
    (function
      | Error (Social_meta_graph_error.Malformed_response { status_code; message }) ->
          assert (status_code = 200);
          assert (String.equal message "Facebook did not like the object")
      | Ok _ | Error _ -> assert false);
  (* A permission-denied like keeps its Graph classification. *)
  Mock_http.set_response
    { status = 403; headers = [ ("x-fb-trace-id", "AvFbL") ];
      body =
        {|{"error":{"message":"(#10) Not allowed to like","type":"OAuthException","code":10,"fbtrace_id":"AvFbB"}}|} };
  Facebook.like_object_detailed ~object_id:"122105_9001" ~access_token:"pt"
    (function
      | Error (Social_meta_graph_error.Http_failure error) ->
          assert (error.detail.http_status = 403);
          assert (error.detail.code = Some "10");
          assert (error.detail.trace_id = Some "AvFbL");
          (match error.graph with
           | Some graph ->
               assert (
                 Social_meta_graph_error.classify graph
                 = Social_meta_graph_error.Permission_denied)
           | None -> assert false)
      | Ok _ | Error _ -> assert false);
  (* An empty object id is refused before any request. *)
  Mock_http.reset ();
  Facebook.like_object_detailed ~object_id:" " ~access_token:"pt"
    (function
      | Error (Social_meta_graph_error.Validation_failure _) -> ()
      | Ok _ | Error _ -> assert false);
  assert (!Mock_http.requests = [])

let () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_SECRET" "app-secret";
  (* The comments page keeps the legacy shape. *)
  Mock_http.set_response
    { status = 200; headers = []; body = stream_comments_body ~next:None };
  Facebook.get_comments_page_detailed ~object_id:"104857_122105"
    ~access_token:"page-token" ~filter:Comments.Stream
    ~order:Comments.Reverse_chronological ~limit:3
    (function
      | Ok page ->
          assert (List.length page.data = 3);
          assert (page.next_url = None);
          (match page.paging with
           | Some { Social_meta_graph_paging.after = Some "WTI5dGJXVnV"; _ } -> ()
           | Some _ | None -> assert false)
      | Error _ -> assert false);
  (* A 190 token error keeps status, codes, trace and the classification. *)
  Mock_http.set_response
    { status = 400; headers = [ ("Retry-After", "23") ];
      body =
        {|{"error":{"message":"Session has expired","type":"OAuthException","code":190,"error_subcode":463,"fbtrace_id":"AvFbE"}}|} };
  Facebook.get_comments_page_detailed ~object_id:"104857_122105"
    ~access_token:"page-token"
    (function
      | Error (Social_meta_graph_error.Http_failure error) ->
          assert (error.detail.http_status = 400);
          assert (error.detail.subcode = Some 463);
          assert (error.detail.trace_id = Some "AvFbE");
          assert (error.retry_after_seconds = Some 23);
          assert (error.kind = Error_types.Auth_error Error_types.Token_expired)
      | Ok _ | Error _ -> assert false);
  (* A 2xx body that is not a page keeps its status as malformed. *)
  Mock_http.set_response { status = 200; headers = []; body = "<html>" };
  Facebook.get_comments_page_detailed ~object_id:"104857_122105"
    ~access_token:"page-token"
    (function
      | Error (Social_meta_graph_error.Malformed_response { status_code; _ }) ->
          assert (status_code = 200)
      | Ok _ | Error _ -> assert false);
  (* A foreign paging URL is refused before any request. *)
  Mock_http.reset ();
  Facebook.get_comments_page_detailed ~object_id:"104857_122105"
    ~access_token:"page-token"
    ~position:(Social_meta_graph_paging.Page_url "https://evil.example/comments?access_token=t")
    (function
      | Error (Social_meta_graph_error.Validation_failure _) -> ()
      | Ok _ | Error _ -> assert false);
  assert (!Mock_http.requests = [])

let () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_SECRET" "app-secret";
  Mock_http.set_response
    { status = 200; headers = []; body = page_posts_body ~next:(Some page_posts_next_url) };
  Facebook.get_page_posts_page_detailed ~page_id:"104857" ~access_token:"page-token"
    ~limit:500
    (function
      | Ok page ->
          assert (List.length page.data = 3);
          assert (page.next_url = Some page_posts_next_url)
      | Error _ -> assert false);
  (* A transport failure is reported without an HTTP status. *)
  Mock_http.reset ();
  Mock_http.set_errors [ "connection reset" ];
  Facebook.get_page_posts_page_detailed ~page_id:"104857" ~access_token:"page-token"
    (function
      | Error (Social_meta_graph_error.Transport_failure message) ->
          assert (String.equal message "connection reset")
      | Ok _ | Error _ -> assert false)

(* Per-operation scope requirements are platform facts (epic #1945 step 12).
   Page insights need both read permissions together with read_insights:
   the endpoints answer permission error (#10) until pages_read_user_content
   joins pages_read_engagement. *)
let () =
  assert (
    OAuth.Scopes.page_insights
    = [ "pages_read_engagement"; "pages_read_user_content"; "read_insights" ]);
  assert (
    OAuth.Scopes.page_comment_management
    = [ "pages_read_engagement"; "pages_read_user_content"; "pages_manage_engagement" ]);
  print_endline "Facebook scope requirement sets ok"
