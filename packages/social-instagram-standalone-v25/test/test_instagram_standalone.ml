(** Tests for Instagram Standalone (Business Login) API v25 Provider *)

open Social_core
open Social_instagram_standalone_v25

(** Helper to check if string contains substring *)
let string_contains s substr =
  try
    ignore (Str.search_forward (Str.regexp_string substr) s 0);
    true
  with Not_found -> false

let query_param url key =
  let params = Uri.query (Uri.of_string url) in
  match List.assoc_opt key params with
  | Some (v :: _) -> Some v
  | _ -> None

(** Helper to handle outcome type in tests *)
let handle_outcome on_success on_error outcome =
  match outcome with
  | Error_types.Success result -> on_success result
  | Error_types.Partial_success { result; _ } -> on_success result
  | Error_types.Failure err -> on_error (Error_types.error_to_string err)

(** Helper to handle api_result type in tests *)
let handle_result on_success on_error result =
  match result with
  | Ok value -> on_success value
  | Error err -> on_error (Error_types.error_to_string err)

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
  let ig_user_ids = ref []
  let sleep_calls = ref []

  let reset () =
    env_vars := [];
    credentials_store := [];
    health_statuses := [];
    ig_user_ids := [];
    sleep_calls := [];
    Mock_http.reset ()

  let set_env key value =
    env_vars := (key, value) :: !env_vars

  let get_env key =
    List.assoc_opt key !env_vars

  let set_credentials ~account_id ~credentials =
    credentials_store := (account_id, credentials) :: !credentials_store

  let set_ig_user_id ~account_id ~ig_user_id =
    ig_user_ids := (account_id, ig_user_id) :: !ig_user_ids

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

  let get_ig_user_id ~account_id on_success on_error =
    match List.assoc_opt account_id !ig_user_ids with
    | Some ig_user_id -> on_success ig_user_id
    | None -> on_error "IG User ID not found"

  let sleep duration on_continue =
    sleep_calls := duration :: !sleep_calls;
    on_continue ()

  let on_rate_limit_update _info =
    () (* No-op for tests *)
end

module Instagram = Make(Mock_config)
module OAuth_standalone_client = OAuth.Make(Mock_http)

(** {1 OAuth Standalone Tests} *)

(** Test: Standalone OAuth authorization URL starts with correct endpoint *)
let test_standalone_auth_url_endpoint () =
  let url = OAuth.get_authorization_url
    ~client_id:"test_app_id"
    ~redirect_uri:"https://example.com/callback"
    ~state:"state_abc"
    ~scopes:OAuth.Scopes.write
    ()
  in
  assert (String.length url > 0);
  assert (string_contains url "https://api.instagram.com/oauth/authorize");
  print_endline "PASS Standalone auth URL starts with correct endpoint"

let test_standalone_authorize_url () =
  let url =
    OAuth.authorize_url ~client_id:"app" ~redirect_uri:"https://e.com/cb" ~state:"st"
      ~scopes:[ "instagram_business_basic"; "instagram_business_content_publish" ]
      ~enable_fb_login:false ()
  in
  assert (String.starts_with ~prefix:"https://api.instagram.com/oauth/authorize?" url);
  assert (query_param url "scope" = Some "instagram_business_basic,instagram_business_content_publish");
  assert (query_param url "params[scope]" = None);
  assert (query_param url "response_type" = Some "code");
  assert (query_param url "state" = Some "st");
  assert (query_param url "enable_fb_login" = Some "0");
  assert (query_param url "force_reauth" = None);
  let url =
    OAuth.authorize_url ~client_id:"app" ~redirect_uri:"https://e.com/cb" ~state:"st"
      ~scopes:[ "instagram_business_basic" ] ~force_reauth:true ()
  in
  assert (query_param url "enable_fb_login" = None);
  assert (query_param url "force_reauth" = Some "true");
  print_endline "PASS authorize_url sends plain comma-separated scope and optional flags"

let expect_ok = function
  | Stdlib.Ok value -> value
  | Stdlib.Error err -> failwith (Error_types.error_to_string err)

let test_exchange_code_full_shapes () =
  (* Documented shape: wrapped in data, permissions comma-joined. *)
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"data":[{"access_token":"tok","user_id":"1020","permissions":"instagram_business_basic,instagram_business_manage_comments"}]}|};
    headers = [];
  };
  OAuth_standalone_client.exchange_code_full ~client_id:"app" ~client_secret:"sec"
    ~redirect_uri:"https://e.com/cb" ~code:"c" (fun result ->
      let exchange = expect_ok result in
      assert (exchange.OAuth.user_id = "1020");
      assert (exchange.credentials.access_token = "tok");
      assert (exchange.permissions = Some [ "instagram_business_basic"; "instagram_business_manage_comments" ]);
      assert (exchange.credentials.scope = Some "instagram_business_basic,instagram_business_manage_comments");
      assert (exchange.expires_in = None);
      (match !Mock_http.requests with
       | [ ("POST", url, _, body) ] ->
           assert (url = "https://api.instagram.com/oauth/access_token");
           assert (string_contains body "grant_type=authorization_code")
       | _ -> failwith "expected one POST"));
  (* Live shape: flat, numeric user_id, permissions as an array. *)
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"access_token":"tok2","user_id":9876543210,"permissions":["instagram_business_basic"],"expires_in":3600}|};
    headers = [];
  };
  OAuth_standalone_client.exchange_code_full ~client_id:"app" ~client_secret:"sec"
    ~redirect_uri:"https://e.com/cb" ~code:"c" (fun result ->
      let exchange = expect_ok result in
      assert (exchange.OAuth.user_id = "9876543210");
      assert (exchange.permissions = Some [ "instagram_business_basic" ]);
      assert (exchange.expires_in = Some 3600));
  (* No permissions member: None, scope None. *)
  Mock_http.reset ();
  Mock_http.set_response { status = 200; body = {|{"access_token":"t","user_id":"1"}|}; headers = [] };
  OAuth_standalone_client.exchange_code_full ~client_id:"app" ~client_secret:"sec"
    ~redirect_uri:"https://e.com/cb" ~code:"c" (fun result ->
      let exchange = expect_ok result in
      assert (exchange.OAuth.permissions = None && exchange.credentials.scope = None));
  Mock_http.reset ();
  Mock_http.set_response { status = 200; body = {|{"access_token":"t"}|}; headers = [] };
  OAuth_standalone_client.exchange_code_full ~client_id:"app" ~client_secret:"sec"
    ~redirect_uri:"https://e.com/cb" ~code:"c" (function
      | Stdlib.Error (Error_types.Internal_error _) -> ()
      | Stdlib.Error _ | Stdlib.Ok _ -> failwith "expected Internal_error without user_id");
  print_endline "PASS exchange_code_full keeps permissions in both response shapes"

let test_get_me_profile () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"id":"app-scoped","user_id":"1784","username":"maker","name":"Maker","account_type":"Business",
             "profile_picture_url":"https://p/x.jpg","followers_count":12,"follows_count":3,"media_count":40,
             "biography":"","website":"https://maker.example"}|};
    headers = [];
  };
  OAuth_standalone_client.get_me ~access_token:"tok"
    ~fields:(OAuth.profile_fields @ [ "biography"; "website" ]) ~app_secret:"sec" (fun result ->
      let profile = expect_ok result in
      (match !Mock_http.requests with
       | [ ("GET", url, _, _) ] ->
           assert (string_contains url "https://graph.instagram.com/v25.0/me?");
           assert (query_param url "fields"
                   = Some "id,user_id,username,name,account_type,profile_picture_url,followers_count,follows_count,media_count,biography,website");
           assert (query_param url "appsecret_proof" <> None)
       | _ -> failwith "expected one GET");
      assert (profile.OAuth.user_id = Some "1784");
      assert (profile.followers_count = Some 12 && profile.media_count = Some 40);
      assert (profile.biography = None);
      assert (profile.website = Some "https://maker.example"));
  (* The get-started guide shows a data-wrapped answer. *)
  Mock_http.reset ();
  Mock_http.set_response
    { status = 200; body = {|{"data":[{"user_id":"1784","username":"maker"}]}|}; headers = [] };
  OAuth_standalone_client.get_me ~access_token:"tok" (fun result ->
      let profile = expect_ok result in
      assert (profile.OAuth.id = None && profile.user_id = Some "1784" && profile.username = Some "maker"));
  Mock_http.reset ();
  Mock_http.set_response { status = 200; body = {|{"username":"x"}|}; headers = [] };
  OAuth_standalone_client.get_me ~access_token:"tok" (function
    | Stdlib.Error (Error_types.Internal_error _) -> ()
    | Stdlib.Error _ | Stdlib.Ok _ -> failwith "expected Internal_error without ids");
  print_endline "PASS get_me reads the typed profile (flat and data-wrapped)"

(** Test: Standalone OAuth authorization URL includes enable_fb_login=0 *)
let test_standalone_auth_url_enable_fb_login () =
  let url = OAuth.get_authorization_url
    ~client_id:"test_app_id"
    ~redirect_uri:"https://example.com/callback"
    ~state:"state_abc"
    ~scopes:OAuth.Scopes.write
    ()
  in
  assert (string_contains url "enable_fb_login=0");
  print_endline "PASS Standalone auth URL includes enable_fb_login=0"

(** Test: Standalone OAuth authorization URL has comma-separated scopes *)
let test_standalone_auth_url_scopes_comma_separated () =
  let url = OAuth.get_authorization_url
    ~client_id:"test_app_id"
    ~redirect_uri:"https://example.com/callback"
    ~state:"state_abc"
    ~scopes:OAuth.Scopes.write
    ()
  in
  let scope_val = query_param url "params[scope]" in
  (match scope_val with
   | Some s ->
       assert (string_contains s ",");
       assert (string_contains s "instagram_business_basic");
       assert (string_contains s "instagram_business_content_publish")
   | None -> failwith "Missing params[scope] parameter");
  print_endline "PASS Standalone auth URL has comma-separated scopes"

(** Test: Standalone OAuth authorization URL has response_type=code *)
let test_standalone_auth_url_response_type () =
  let url = OAuth.get_authorization_url
    ~client_id:"test_app_id"
    ~redirect_uri:"https://example.com/callback"
    ~state:"state_abc"
    ~scopes:OAuth.Scopes.write
    ()
  in
  assert (query_param url "response_type" = Some "code");
  print_endline "PASS Standalone auth URL has response_type=code"

(** Test: Standalone OAuth authorization URL supports custom scopes *)
let test_standalone_auth_url_custom_scopes () =
  let url = OAuth.get_authorization_url
    ~client_id:"test_app_id"
    ~redirect_uri:"https://example.com/callback"
    ~state:"state_abc"
    ~scopes:[OAuth.Instagram_business_basic; OAuth.Instagram_business_manage_comments]
    ()
  in
  let scope_val = query_param url "params[scope]" in
  (match scope_val with
   | Some s ->
       assert (string_contains s "instagram_business_basic");
       assert (string_contains s "instagram_business_manage_comments");
       assert (not (string_contains s "instagram_business_content_publish"))
   | None -> failwith "Missing params[scope] parameter");
  print_endline "PASS Standalone auth URL supports custom scopes"

(** Test: Standalone exchange_code happy path with user_id as int *)
let test_standalone_exchange_code_happy_path_int_user_id () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"access_token":"short_tok_123","user_id":9876543210,"permissions":"instagram_business_basic,instagram_business_content_publish"}|};
    headers = [];
  };

  OAuth_standalone_client.exchange_code
    ~client_id:"app_id"
    ~client_secret:"app_secret"
    ~redirect_uri:"https://example.com/cb"
    ~code:"auth_code_xyz"
    (handle_result
      (fun (creds, user_id) ->
        assert (creds.access_token = "short_tok_123");
        assert (creds.auth_type = Bearer);
        assert (user_id = "9876543210");
        (* Verify it was a POST request *)
        let requests = !Mock_http.requests in
        let has_post = List.exists (fun (meth, _, _, _) -> meth = "POST") requests in
        assert has_post;
        (* Verify Content-Type header *)
        let has_form_header = List.exists (fun (_, _, headers, _) ->
          List.exists (fun (k, v) -> k = "Content-Type" && v = "application/x-www-form-urlencoded") headers
        ) requests in
        assert has_form_header;
        print_endline "PASS Standalone exchange_code happy path (int user_id)")
      (fun err -> failwith ("Standalone exchange_code failed: " ^ err)))

(** Test: Standalone exchange_code happy path with user_id as string *)
let test_standalone_exchange_code_happy_path_string_user_id () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"access_token":"short_tok_456","user_id":"1234567890","token_type":"bearer"}|};
    headers = [];
  };

  OAuth_standalone_client.exchange_code
    ~client_id:"app_id"
    ~client_secret:"app_secret"
    ~redirect_uri:"https://example.com/cb"
    ~code:"auth_code_abc"
    (handle_result
      (fun (creds, user_id) ->
        assert (creds.access_token = "short_tok_456");
        assert (user_id = "1234567890");
        print_endline "PASS Standalone exchange_code happy path (string user_id)")
      (fun err -> failwith ("Standalone exchange_code string user_id failed: " ^ err)))

(** Test: Standalone exchange_code normalizes auth_type to Bearer *)
let test_standalone_exchange_code_normalizes_auth_type () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"access_token":"tok","user_id":"123","token_type":"bearer"}|};
    headers = [];
  };

  OAuth_standalone_client.exchange_code
    ~client_id:"app_id"
    ~client_secret:"app_secret"
    ~redirect_uri:"https://example.com/cb"
    ~code:"code"
    (handle_result
      (fun (creds, _user_id) ->
        assert (creds.auth_type = Bearer);
        print_endline "PASS Standalone exchange_code normalizes auth_type to Bearer")
      (fun err -> failwith ("Standalone exchange_code auth_type failed: " ^ err)))

(** Test: Standalone exchange_code error when user_id is missing *)
let test_standalone_exchange_code_missing_user_id () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"access_token":"tok_no_uid","token_type":"bearer"}|};
    headers = [];
  };

  OAuth_standalone_client.exchange_code
    ~client_id:"app_id"
    ~client_secret:"app_secret"
    ~redirect_uri:"https://example.com/cb"
    ~code:"code"
    (function
      | Ok _ -> failwith "Expected error for missing user_id"
      | Error (Error_types.Internal_error msg) when string_contains msg "user_id" ->
          print_endline "PASS Standalone exchange_code error on missing user_id"
      | Error err -> failwith ("Unexpected error type: " ^ Error_types.error_to_string err))

(** Test: Standalone exchange_code handles HTTP error response *)
let test_standalone_exchange_code_http_error () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 400;
    body = {|{"error":{"code":190,"message":"Invalid OAuth 2.0 Access Token"}}|};
    headers = [];
  };

  OAuth_standalone_client.exchange_code
    ~client_id:"app_id"
    ~client_secret:"app_secret"
    ~redirect_uri:"https://example.com/cb"
    ~code:"bad_code"
    (function
      | Ok _ -> failwith "Expected error for HTTP 400"
      | Error (Error_types.Auth_error Error_types.Token_invalid) ->
          print_endline "PASS Standalone exchange_code handles HTTP error"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** Test: Standalone exchange_for_long_lived_token happy path *)
let test_standalone_exchange_long_lived_happy_path () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"access_token":"long_lived_tok_abc","token_type":"bearer","expires_in":5184000}|};
    headers = [];
  };

  OAuth_standalone_client.exchange_for_long_lived_token
    ~client_secret:"app_secret"
    ~short_lived_token:"short_tok"
    (handle_result
      (fun creds ->
        assert (creds.access_token = "long_lived_tok_abc");
        assert (creds.auth_type = Bearer);
        assert (creds.expires_at <> None);
        (* Verify it was a GET request to the correct endpoint with query params *)
        let requests = !Mock_http.requests in
        let has_correct_request = List.exists (fun (meth, url, _, _) ->
          meth = "GET"
          && string_contains url "graph.instagram.com/access_token"
          && string_contains url "grant_type=ig_exchange_token"
        ) requests in
        assert has_correct_request;
        print_endline "PASS Standalone exchange_for_long_lived_token happy path")
      (fun err -> failwith ("Standalone exchange_for_long_lived_token failed: " ^ err)))

(** Test: Standalone exchange_for_long_lived_token normalizes auth_type *)
let test_standalone_exchange_long_lived_normalizes_auth_type () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"access_token":"ll_tok","token_type":"bearer","expires_in":5184000}|};
    headers = [];
  };

  OAuth_standalone_client.exchange_for_long_lived_token
    ~client_secret:"secret"
    ~short_lived_token:"sl_tok"
    (handle_result
      (fun creds ->
        assert (creds.auth_type = Bearer);
        print_endline "PASS Standalone exchange_for_long_lived_token normalizes auth_type")
      (fun err -> failwith ("Standalone long-lived auth_type failed: " ^ err)))

(** Test: Standalone exchange_for_long_lived_token with appsecret_proof *)
let test_standalone_exchange_long_lived_appsecret_proof () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"access_token":"ll_tok","token_type":"bearer","expires_in":5184000}|};
    headers = [];
  };

  OAuth_standalone_client.exchange_for_long_lived_token
    ~client_secret:"secret"
    ~short_lived_token:"sl_tok"
    ~app_secret:"my_app_secret"
    (handle_result
      (fun _creds ->
        let requests = !Mock_http.requests in
        let has_proof = List.exists (fun (_, url, _, _) ->
          string_contains url "appsecret_proof="
        ) requests in
        assert has_proof;
        print_endline "PASS Standalone exchange_for_long_lived_token with appsecret_proof")
      (fun err -> failwith ("Standalone long-lived appsecret_proof failed: " ^ err)))

(** Test: Standalone get_profile happy path *)
let test_standalone_get_profile_happy_path () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"id":"12345","username":"testuser","name":"Test User","profile_picture_url":"https://example.com/pic.jpg"}|};
    headers = [];
  };

  OAuth_standalone_client.get_profile
    ~access_token:"valid_tok"
    (handle_result
      (fun (profile : standalone_profile) ->
        assert (profile.user_id = "12345");
        assert (profile.username = "testuser");
        assert (profile.name = "Test User");
        assert (profile.profile_picture_url = Some "https://example.com/pic.jpg");
        print_endline "PASS Standalone get_profile happy path")
      (fun err -> failwith ("Standalone get_profile failed: " ^ err)))

(** Test: Standalone get_profile with missing profile_picture_url *)
let test_standalone_get_profile_no_picture () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"id":"67890","username":"nopic_user","name":"No Pic"}|};
    headers = [];
  };

  OAuth_standalone_client.get_profile
    ~access_token:"valid_tok"
    (handle_result
      (fun (profile : standalone_profile) ->
        assert (profile.user_id = "67890");
        assert (profile.username = "nopic_user");
        assert (profile.profile_picture_url = None);
        print_endline "PASS Standalone get_profile with missing profile_picture_url")
      (fun err -> failwith ("Standalone get_profile no picture failed: " ^ err)))

(** Test: Standalone get_profile requests 'id' field (not 'user_id') *)
let test_standalone_get_profile_fields_parameter () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"id":"99999","username":"u","name":"N"}|};
    headers = [];
  };

  OAuth_standalone_client.get_profile
    ~access_token:"valid_tok"
    (handle_result
      (fun _profile ->
        let requests = !Mock_http.requests in
        let has_id_field = List.exists (fun (_, url, _, _) ->
          match query_param url "fields" with
          | Some f -> string_contains f "id" && not (string_contains f "user_id")
          | None -> false
        ) requests in
        assert has_id_field;
        print_endline "PASS Standalone get_profile requests 'id' field (not 'user_id')")
      (fun err -> failwith ("Standalone get_profile fields failed: " ^ err)))

(** Test: Standalone get_profile error on missing identifier *)
let test_standalone_get_profile_missing_identifier () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"username":"orphan_user","name":"No ID"}|};
    headers = [];
  };

  OAuth_standalone_client.get_profile
    ~access_token:"valid_tok"
    (function
      | Ok _ -> failwith "Expected error for missing identifier"
      | Error (Error_types.Internal_error msg) when string_contains msg "identifier" ->
          print_endline "PASS Standalone get_profile error on missing identifier"
      | Error err -> failwith ("Unexpected error: " ^ Error_types.error_to_string err))

(** {1 Content Publishing Tests} *)

(** Test: OAuth URL uses Instagram standalone endpoint *)
let test_oauth_url () =
  Mock_config.reset ();
  Mock_config.set_env "INSTAGRAM_APP_ID" "test_app_id";

  Instagram.get_oauth_url ~redirect_uri:"https://example.com/callback" ~state:"test_state_123"
    ~scopes:OAuth.Scopes.write
    (fun url ->
      assert (string_contains url "client_id=test_app_id");
      assert (string_contains url "state=test_state_123");
      assert (string_contains url "instagram_business");
      print_endline "PASS OAuth URL generation (standalone)")
    (fun err -> failwith ("OAuth URL failed: " ^ err))

(** Test: API base URL uses graph.instagram.com *)
let test_api_base_url_uses_instagram () =
  Mock_config.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"data": []}|};
    headers = [];
  };

  Instagram.get_account_audience_insights
    ~id:"ig_user_123"
    ~access_token:"tok"
    ~since:"2025-01-01"
    ~until:"2025-01-07"
    (handle_result
      (fun _insights ->
        let requests = !Mock_http.requests in
        let uses_instagram_graph = List.exists (fun (_, url, _, _) ->
          string_contains url "graph.instagram.com/v25.0"
        ) requests in
        assert uses_instagram_graph;
        let uses_facebook_graph = List.exists (fun (_, url, _, _) ->
          string_contains url "graph.facebook.com"
        ) requests in
        assert (not uses_facebook_graph);
        print_endline "PASS API base URL uses graph.instagram.com (not graph.facebook.com)")
      (fun err -> failwith ("API base URL check failed: " ^ err)))

(** Test: Rate-limit parser tolerates numeric formats and uses max component *)
let test_rate_limit_parsing () =
  let headers = [
    ("X-App-Usage", "{\"call_count\":5,\"total_cputime\":24.9,\"total_time\":12}");
  ] in
  match Instagram.parse_rate_limit_header headers with
  | Some info ->
      assert (info.call_count = 5);
      assert (info.total_cputime = 24);
      assert (info.total_time = 12);
      assert (info.percentage_used = 24.0);
      print_endline "PASS Rate limit parsing"
  | None -> failwith "Rate limit parsing should succeed"

(** Test: Create container *)
let test_create_container () =
  Mock_config.reset ();

  let response_body = {|{"id": "container_12345"}|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Instagram.create_image_container
    ~ig_user_id:"ig_123"
    ~access_token:"test_token"
    ~image_url:"https://example.com/image.jpg"
    ~caption:"Test caption"
    ~alt_text:None
    ~is_carousel_item:false
    (handle_result
      (fun container_id ->
        assert (container_id = "container_12345");
        print_endline "PASS Create image container")
      (fun err -> failwith ("Create image container failed: " ^ err)))

(** Test: Publish container *)
let test_publish_container () =
  Mock_config.reset ();

  let response_body = {|{"id": "media_67890"}|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Instagram.publish_container
    ~ig_user_id:"ig_123"
    ~access_token:"test_token"
    ~container_id:"container_12345"
    (handle_result
      (fun media_id ->
        assert (media_id = "media_67890");
        print_endline "PASS Publish container")
      (fun err -> failwith ("Publish container failed: " ^ err)))

(** Test: Check container status *)
let test_check_status () =
  Mock_config.reset ();

  let response_body = {|{
    "status_code": "FINISHED",
    "status": "Processing complete"
  }|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Instagram.check_container_status
    ~container_id:"container_12345"
    ~access_token:"test_token"
    (handle_result
      (fun (status_code, status) ->
        assert (status_code = "FINISHED");
        assert (status = "Processing complete");
        print_endline "PASS Check container status")
      (fun err -> failwith ("Check status failed: " ^ err)))

(** Test: Content validation *)
let test_content_validation () =
  (* Valid content *)
  (match Instagram.validate_content ~text:"Hello Instagram! #test" with
   | Ok () -> print_endline "PASS Valid content passes"
   | Error e -> failwith ("Valid content failed: " ^ e));

  (* Too long *)
  let long_text = String.make 2201 'x' in
  (match Instagram.validate_content ~text:long_text with
   | Error msg when string_contains msg "2,200" ->
       print_endline "PASS Long content rejected"
   | _ -> failwith "Long content should fail");

  (* Too many hashtags *)
  let many_hashtags = String.concat " " (List.init 31 (fun i -> Printf.sprintf "#tag%d" i)) in
  (match Instagram.validate_content ~text:many_hashtags with
   | Error msg when string_contains msg "30 hashtags" ->
       print_endline "PASS Too many hashtags rejected"
   | _ -> failwith "Too many hashtags should fail")

(** Test: Post single (full flow) *)
let test_post_single () =
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
  Mock_config.set_ig_user_id ~account_id:"test_account" ~ig_user_id:"ig_123";

  Mock_http.set_responses [
    { status = 200; body = {|{"id": "container_12345"}|}; headers = [] };
    { status = 200; body = {|{"status_code": "FINISHED", "status": "OK"}|}; headers = [] };
    { status = 200; body = {|{"id": "media_67890"}|}; headers = [] };
  ];

  Instagram.post_single
    ~account_id:"test_account"
    ~text:"Test post"
    ~media_urls:["https://example.com/image.jpg"]
    (handle_outcome
      (fun media_id ->
        assert (media_id = "media_67890");
        assert (List.length !Mock_config.sleep_calls > 0);
        print_endline "PASS Post single (full flow)")
      (fun err -> failwith ("Post single failed: " ^ err)))


(** Mock config for [Make_nonblocking]: [sleep] only records the timer,
    the way a cooperative runtime schedules it, and the test fires it. *)
module Nonblocking_mock_config = struct
  include Mock_config

  let pending_sleeps : (float * (unit -> unit) * (string -> unit)) list ref =
    ref []

  let sleep ~seconds on_continue on_error =
    pending_sleeps := (seconds, on_continue, on_error) :: !pending_sleeps
end

module Instagram_nonblocking = Make_nonblocking (Nonblocking_mock_config)

let take_single_pending_sleep () =
  match !Nonblocking_mock_config.pending_sleeps with
  | [ timer ] ->
      Nonblocking_mock_config.pending_sleeps := [];
      timer
  | timers ->
      failwith
        (Printf.sprintf "expected one pending sleep, got %d"
           (List.length timers))

let setup_nonblocking_post () =
  Mock_config.reset ();
  Nonblocking_mock_config.pending_sleeps := [];
  let expires_at =
    match
      Ptime.add_span (Ptime_clock.now ()) (Ptime.Span.of_int_s (30 * 86400))
    with
    | Some t -> Some (Ptime.to_rfc3339 t)
    | None -> failwith "Failed to calculate future time"
  in
  Mock_config.set_credentials ~account_id:"test_account"
    ~credentials:
      { access_token = "valid_token"; refresh_token = None; expires_at;
        auth_type = Bearer; scope = None };
  Mock_config.set_ig_user_id ~account_id:"test_account" ~ig_user_id:"ig_123";
  Mock_http.set_responses [
    { status = 200; body = {|{"id": "container_12345"}|}; headers = [] };
    { status = 200; body = {|{"status_code": "FINISHED", "status": "OK"}|}; headers = [] };
    { status = 200; body = {|{"id": "media_67890"}|}; headers = [] };
  ]

(** Test: container polling waits through the runtime's sleep and returns
    to the caller instead of blocking until the timer fires *)
let test_post_single_nonblocking_sleep () =
  setup_nonblocking_post ();
  let result = ref None in
  Instagram_nonblocking.post_single ~account_id:"test_account"
    ~text:"Test post" ~media_urls:["https://example.com/image.jpg"]
    (fun outcome -> result := Some outcome);
  assert (!result = None);
  assert (List.length !Mock_http.requests = 1);
  let seconds, on_continue, _on_error = take_single_pending_sleep () in
  assert (seconds = 2.0);
  on_continue ();
  (match !result with
   | Some (Error_types.Success "media_67890") -> ()
   | _ -> failwith "post_single should publish once the timer fires");
  assert (List.length !Mock_http.requests = 3);
  print_endline "PASS Container polling uses the non-blocking sleep"

(** Test: a timer the runtime cannot schedule fails the post instead of
    publishing or hanging *)
let test_post_single_nonblocking_sleep_error () =
  setup_nonblocking_post ();
  let result = ref None in
  Instagram_nonblocking.post_single ~account_id:"test_account"
    ~text:"Test post" ~media_urls:["https://example.com/image.jpg"]
    (fun outcome -> result := Some outcome);
  let _seconds, _on_continue, on_error = take_single_pending_sleep () in
  on_error "timer cancelled";
  (match !result with
   | Some (Error_types.Failure _) -> ()
   | _ -> failwith "a failed sleep should fail the post");
  assert (List.length !Mock_http.requests = 1);
  print_endline "PASS Failed sleep fails the post without publishing"

(** Test: post_single threads collaborators into the container request *)
let test_post_single_with_collaborators () =
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
  Mock_config.set_ig_user_id ~account_id:"test_account" ~ig_user_id:"ig_123";

  Mock_http.set_responses [
    { status = 200; body = {|{"id": "container_12345"}|}; headers = [] };
    { status = 200; body = {|{"status_code": "FINISHED", "status": "OK"}|}; headers = [] };
    { status = 200; body = {|{"id": "media_67890"}|}; headers = [] };
  ];

  Instagram.post_single
    ~account_id:"test_account"
    ~text:"Test post"
    ~media_urls:["https://example.com/image.jpg"]
    ~collaborators:["collab_user1"; "collab_user2"]
    (handle_outcome
      (fun media_id ->
        assert (media_id = "media_67890");
        let requests = !Mock_http.requests in
        let has_collabs = List.exists (fun (_, _, _, body) ->
          string_contains body "collaborators"
          && string_contains body "collab_user1"
          && string_contains body "collab_user2"
        ) requests in
        assert has_collabs;
        print_endline "PASS Post single with collaborators")
      (fun err -> failwith ("Post single with collaborators failed: " ^ err)))

(** Test: post_single threads location_id into the container request *)
let test_post_single_with_location () =
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
  Mock_config.set_ig_user_id ~account_id:"test_account" ~ig_user_id:"ig_123";

  Mock_http.set_responses [
    { status = 200; body = {|{"id": "container_12345"}|}; headers = [] };
    { status = 200; body = {|{"status_code": "FINISHED", "status": "OK"}|}; headers = [] };
    { status = 200; body = {|{"id": "media_67890"}|}; headers = [] };
  ];

  Instagram.post_single
    ~account_id:"test_account"
    ~text:"Test post"
    ~media_urls:["https://example.com/image.jpg"]
    ~location_id:"place_98765"
    (handle_outcome
      (fun media_id ->
        assert (media_id = "media_67890");
        let requests = !Mock_http.requests in
        let has_location = List.exists (fun (_, _, _, body) ->
          string_contains body "location_id"
          && string_contains body "place_98765"
        ) requests in
        assert has_location;
        print_endline "PASS Post single with location")
      (fun err -> failwith ("Post single with location failed: " ^ err)))

(** Test: Post requires media *)
let test_post_requires_media () =
  Mock_config.reset ();

  Instagram.post_single
    ~account_id:"test_account"
    ~text:"Test"
    ~media_urls:[]
    (fun outcome ->
      match outcome with
      | Error_types.Failure _ -> print_endline "PASS Post requires media"
      | _ -> failwith "Should fail without media")

(** Test: post_single rejects unsupported media extension *)
let test_post_single_rejects_unsupported_media () =
  Mock_config.reset ();
  Instagram.post_single
    ~account_id:"test_account"
    ~text:"Unsupported media"
    ~media_urls:["https://example.com/file.avi"]
    (function
      | Error_types.Failure (Error_types.Validation_error errs) ->
          let has_format_error = List.exists (function
            | Error_types.Media_unsupported_format _ -> true
            | _ -> false
          ) errs in
          assert has_format_error;
          let requests = !Mock_http.requests in
          assert (requests = []);
          print_endline "PASS post_single rejects unsupported media"
      | _ -> failwith "Expected media format validation error")

(** Test: Polling does not publish when status checks keep failing *)
let test_polling_no_publish_on_status_error () =
  Mock_config.reset ();

  Mock_http.set_response { status = 500; body = {|{"error":{"message":"boom"}}|}; headers = [] };

  Instagram.poll_container_status
    ~container_id:"container_123"
    ~access_token:"token_123"
    ~ig_user_id:"ig_123"
    ~attempt:1
    ~max_attempts:1
    (fun _ -> failwith "Polling should not publish on status-check failure")
    (fun _err ->
      let requests = !Mock_http.requests in
      let attempted_publish = List.exists (fun (_, url, _, _) -> string_contains url "media_publish") requests in
      assert (not attempted_publish);
      print_endline "PASS Polling does not publish on status-check errors")

(** Test: OAuth API error mapping for token expiration (190 / subcode 463) *)
let test_parse_api_error_token_expired () =
  let err =
    Instagram.parse_api_error
      ~status_code:400
      ~response_body:{|{"error":{"message":"Error validating access token: Session has expired on Monday, 16-Nov-26 10:00:00 PST. The current time is Tuesday, 17-Nov-26 01:00:00 PST.","type":"OAuthException","code":190,"error_subcode":463,"fbtrace_id":"AbCdEf"}}|}
  in
  match err with
  | Error_types.Auth_error Error_types.Token_expired ->
      print_endline "PASS OAuth API error mapping (token expired)"
  | _ -> failwith "Expected Auth_error Token_expired"

(** Test: 190 without a subcode is an invalid token, not an expired one.
    Before the shared Meta classifier every 190 read as Token_expired. *)
let test_parse_api_error_token_invalid_without_subcode () =
  let err =
    Instagram.parse_api_error
      ~status_code:400
      ~response_body:{|{"error":{"code":190,"message":"Invalid OAuth 2.0 Access Token"}}|}
  in
  match err with
  | Error_types.Auth_error Error_types.Token_invalid ->
      print_endline "PASS OAuth API error mapping (190 without subcode)"
  | _ -> failwith "Expected Auth_error Token_invalid"

(** Test: User-friendly error response mapping for token expiration *)
let test_parse_error_response_token_expired () =
  let msg =
    Instagram.parse_error_response
      {|{"error":{"code":190,"message":"Invalid OAuth 2.0 Access Token"}}|}
      400
  in
  if string_contains msg "reconnect" then
    print_endline "PASS User-friendly OAuth error mapping"
  else
    failwith "Expected reconnect guidance for token expiration"

(** Test: container creation returns structured auth error on API auth failure *)
let test_create_container_structured_auth_error () =
  Mock_config.reset ();
  Mock_http.set_response {
    status = 400;
    body = {|{"error":{"code":190,"message":"Invalid OAuth 2.0 Access Token"}}|};
    headers = [];
  };

  Instagram.create_image_container
    ~ig_user_id:"ig_user_123"
    ~access_token:"bad_token"
    ~image_url:"https://example.com/image.jpg"
    ~caption:"caption"
    ~alt_text:None
    ~is_carousel_item:false
    (function
      | Ok _ -> failwith "Expected auth error"
      | Error (Error_types.Auth_error Error_types.Token_invalid) ->
          print_endline "PASS Structured auth error from container creation"
      | Error _ -> failwith "Expected Token_invalid auth error")

(** Test: Standalone refresh_token in OAuth.Make *)
let test_standalone_oauth_refresh_token () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"access_token":"refreshed_tok","token_type":"bearer","expires_in":5184000}|};
    headers = [];
  };

  OAuth_standalone_client.refresh_token
    ~app_secret:"secret_abc"
    ~access_token:"old_tok"
    (handle_result
      (fun creds ->
        assert (creds.access_token = "refreshed_tok");
        assert (creds.auth_type = Bearer);
        let requests = !Mock_http.requests in
        let has_refresh_endpoint = List.exists (fun (_, url, _, _) ->
          string_contains url "graph.instagram.com/refresh_access_token"
        ) requests in
        assert has_refresh_endpoint;
        let has_proof = List.exists (fun (_, url, _, _) ->
          match query_param url "appsecret_proof" with
          | Some p -> String.length p = 64
          | None -> false
        ) requests in
        assert has_proof;
        print_endline "PASS Standalone OAuth refresh_token (with auth_type normalization)")
      (fun err -> failwith ("Standalone OAuth refresh_token failed: " ^ err)))

(** Test: Standalone exchange_code form-urlencodes special characters in client_secret *)
let test_standalone_exchange_code_form_urlencodes_special_chars () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"access_token":"tok","user_id":"123","token_type":"bearer"}|};
    headers = [];
  };

  OAuth_standalone_client.exchange_code
    ~client_id:"app_id"
    ~client_secret:"sec/ret=with+special@chars"
    ~redirect_uri:"https://example.com/cb"
    ~code:"code"
    (handle_result
      (fun (_creds, _user_id) ->
        let requests = !Mock_http.requests in
        let post_req = List.find (fun (meth, _, _, _) -> meth = "POST") requests in
        let (_, _, _, body) = post_req in
        assert (string_contains body "client_secret=sec%2Fret%3Dwith%2Bspecial%40chars");
        print_endline "PASS Standalone exchange_code form-urlencodes special chars in client_secret")
      (fun err -> failwith ("Standalone exchange_code form-urlencodes failed: " ^ err)))

(** Test: OAuth.Make.exchange_code calls on_response callback *)
let test_standalone_exchange_code_on_response () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"access_token":"tok","user_id":"123","token_type":"bearer"}|};
    headers = [("X-App-Usage", {|{"call_count":5,"total_cputime":10,"total_time":3}|})];
  };

  let callback_called = ref false in
  let on_response _response = callback_called := true in

  OAuth_standalone_client.exchange_code
    ~client_id:"app_id"
    ~client_secret:"app_secret"
    ~redirect_uri:"https://example.com/cb"
    ~code:"code"
    ~on_response
    (handle_result
      (fun (_creds, _user_id) ->
        assert !callback_called;
        print_endline "PASS Standalone exchange_code calls on_response callback")
      (fun err -> failwith ("Standalone exchange_code on_response failed: " ^ err)))

(** Test: OAuth.Make.refresh_token calls on_response callback *)
let test_standalone_refresh_token_on_response () =
  Mock_http.reset ();
  Mock_http.set_response {
    status = 200;
    body = {|{"access_token":"refreshed_tok","token_type":"bearer","expires_in":5184000}|};
    headers = [("X-App-Usage", {|{"call_count":12,"total_cputime":8,"total_time":4}|})];
  };

  let callback_called = ref false in
  let on_response _response = callback_called := true in

  OAuth_standalone_client.refresh_token
    ~access_token:"old_tok"
    ~on_response
    (handle_result
      (fun _creds ->
        assert !callback_called;
        print_endline "PASS Standalone refresh_token calls on_response callback")
      (fun err -> failwith ("Standalone refresh_token on_response failed: " ^ err)))

(** {1 Comment Posting Tests (H1)} *)

(** Test: Post comment request contract *)
let test_post_comment () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 200;
    body = {|{"id": "comment_12345"}|};
    headers = [];
  };

  Instagram.post_comment
    ~media_id:"media_99"
    ~access_token:"test_token"
    ~message:"Great post! #firstcomment"
    (handle_result
      (fun comment_id ->
        assert (comment_id = "comment_12345");
        let requests = !Mock_http.requests in
        let has_contract = List.exists (fun (meth, url, _, body) ->
          meth = "POST"
          && string_contains url "/media_99/comments"
          && string_contains body "message"
          && string_contains body "Great"
        ) requests in
        assert has_contract;
        print_endline "PASS Post comment")
      (fun err -> failwith ("Post comment failed: " ^ err)))

(** Test: Post comment returns structured error on auth failure *)
let test_post_comment_auth_error () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 400;
    body = {|{"error":{"code":190,"message":"Invalid OAuth 2.0 Access Token"}}|};
    headers = [];
  };

  Instagram.post_comment
    ~media_id:"media_99"
    ~access_token:"bad_token"
    ~message:"This will fail"
    (function
      | Ok _ -> failwith "Expected auth error"
      | Error (Error_types.Auth_error Error_types.Token_invalid) ->
          print_endline "PASS Post comment auth error"
      | Error _ -> failwith "Expected Token_invalid")

(** {1 Media Listing Tests (H2)} *)

(** Test: Get user media request contract *)
let test_get_user_media () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 200;
    body = {|{
      "data": [
        {
          "id": "media_1",
          "caption": "First post",
          "timestamp": "2025-01-01T12:00:00+0000",
          "media_type": "IMAGE",
          "media_url": "https://example.com/img1.jpg",
          "thumbnail_url": null,
          "permalink": "https://www.instagram.com/p/abc123/",
          "media_product_type": "FEED"
        },
        {
          "id": "media_2",
          "caption": null,
          "timestamp": "2025-01-02T12:00:00+0000",
          "media_type": "VIDEO",
          "media_url": "https://example.com/vid1.mp4",
          "thumbnail_url": "https://example.com/thumb1.jpg",
          "permalink": "https://www.instagram.com/p/def456/",
          "media_product_type": "REELS"
        }
      ],
      "paging": {
        "cursors": {
          "before": "before_cursor",
          "after": "next_page_cursor"
        }
      }
    }|};
    headers = [];
  };

  Instagram.get_user_media
    ~ig_user_id:"ig_user_123"
    ~access_token:"tok+en/=="
    (handle_result
      (fun ((items, next_cursor) : Instagram.media_item list * string option) ->
        assert (List.length items = 2);
        let first = List.nth items 0 in
        assert (first.id = "media_1");
        assert (first.caption = Some "First post");
        assert (first.media_type = Some "IMAGE");
        assert (first.permalink = Some "https://www.instagram.com/p/abc123/");
        assert (first.media_product_type = Some "FEED");
        let second = List.nth items 1 in
        assert (second.id = "media_2");
        assert (second.caption = None);
        assert (second.media_type = Some "VIDEO");
        assert (second.thumbnail_url = Some "https://example.com/thumb1.jpg");
        assert (second.media_product_type = Some "REELS");
        (* Verify pagination cursor is returned *)
        assert (next_cursor = Some "next_page_cursor");
        (* Verify request uses graph.instagram.com *)
        let requests = !Mock_http.requests in
        let has_contract = List.exists (fun (meth, url, _, _) ->
          meth = "GET"
          && string_contains url "graph.instagram.com"
          && string_contains url "/ig_user_123/media"
          && string_contains url "media_product_type"
        ) requests in
        assert has_contract;
        print_endline "PASS Get user media")
      (fun err -> failwith ("Get user media failed: " ^ err)))

(** Test: Get user media with limit parameter *)
let test_get_user_media_with_limit () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 200;
    body = {|{"data": [{"id": "media_1"}]}|};
    headers = [];
  };

  Instagram.get_user_media
    ~ig_user_id:"ig_user_123"
    ~access_token:"test_token"
    ~limit:5
    (handle_result
      (fun (_items, next_cursor) ->
        assert (next_cursor = None);
        let requests = !Mock_http.requests in
        let has_limit = List.exists (fun (_, url, _, _) ->
          query_param url "limit" = Some "5"
        ) requests in
        assert has_limit;
        print_endline "PASS Get user media with limit")
      (fun err -> failwith ("Get user media with limit failed: " ^ err)))

(** Test: Get user media with pagination cursor *)
let test_get_user_media_with_pagination () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 200;
    body = {|{"data": [{"id": "media_3"}]}|};
    headers = [];
  };

  Instagram.get_user_media
    ~ig_user_id:"ig_user_123"
    ~access_token:"test_token"
    ~after:"cursor_abc"
    (handle_result
      (fun (_items, _next_cursor) ->
        let requests = !Mock_http.requests in
        let has_after = List.exists (fun (_, url, _, _) ->
          query_param url "after" = Some "cursor_abc"
        ) requests in
        assert has_after;
        print_endline "PASS Get user media with pagination")
      (fun err -> failwith ("Get user media with pagination failed: " ^ err)))

(** {1 Media Deletion Tests (H3)} *)

(** Test: Delete media request contract *)
let test_delete_media () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 200;
    body = {|{"success": true}|};
    headers = [];
  };

  Instagram.delete_media
    ~media_id:"media_to_delete"
    ~access_token:"test_token"
    (handle_result
      (fun success ->
        assert success;
        let requests = !Mock_http.requests in
        let has_contract = List.exists (fun (meth, url, _, _) ->
          meth = "DELETE"
          && string_contains url "/media_to_delete"
        ) requests in
        assert has_contract;
        print_endline "PASS Delete media")
      (fun err -> failwith ("Delete media failed: " ^ err)))

(** Test: Delete media returns structured error on auth failure *)
let test_delete_media_auth_error () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 400;
    body = {|{"error":{"code":190,"message":"Invalid OAuth 2.0 Access Token"}}|};
    headers = [];
  };

  Instagram.delete_media
    ~media_id:"media_to_delete"
    ~access_token:"bad_token"
    (function
      | Ok _ -> failwith "Expected auth error"
      | Error (Error_types.Auth_error Error_types.Token_invalid) ->
          print_endline "PASS Delete media auth error"
      | Error _ -> failwith "Expected Token_invalid")

(** {1 Tagging Tests (H4)} *)

(** Test: Image container with user tags, location, and collaborators *)
let test_image_container_with_tagging () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 200;
    body = {|{"id": "container_tagged"}|};
    headers = [];
  };

  Instagram.create_image_container
    ~ig_user_id:"ig_123"
    ~access_token:"test_token"
    ~image_url:"https://example.com/image.jpg"
    ~caption:"Tagged post"
    ~alt_text:None
    ~is_carousel_item:false
    ~user_tags:[("user1", 0.5, 0.5); ("user2", 0.2, 0.8)]
    ~location_id:"location_456"
    ~collaborators:["collab_user1"; "collab_user2"]
    (handle_result
      (fun container_id ->
        assert (container_id = "container_tagged");
        let requests = !Mock_http.requests in
        let has_tags = List.exists (fun (_, _, _, body) ->
          string_contains body "user_tags"
          && string_contains body "user1"
          && string_contains body "user2"
        ) requests in
        assert has_tags;
        let has_location = List.exists (fun (_, _, _, body) ->
          string_contains body "location_id"
          && string_contains body "location_456"
        ) requests in
        assert has_location;
        let has_collabs = List.exists (fun (_, _, _, body) ->
          string_contains body "collaborators"
          && string_contains body "collab_user1"
        ) requests in
        assert has_collabs;
        print_endline "PASS Image container with tagging")
      (fun err -> failwith ("Image container with tagging failed: " ^ err)))

(** Test: Video container with location *)
let test_video_container_with_location () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 200;
    body = {|{"id": "container_loc"}|};
    headers = [];
  };

  Instagram.create_video_container
    ~ig_user_id:"ig_123"
    ~access_token:"test_token"
    ~video_url:"https://example.com/video.mp4"
    ~caption:"Video with location"
    ~alt_text:None
    ~media_type:"VIDEO"
    ~is_carousel_item:false
    ~location_id:"loc_789"
    (handle_result
      (fun container_id ->
        assert (container_id = "container_loc");
        let requests = !Mock_http.requests in
        let has_location = List.exists (fun (_, _, _, body) ->
          string_contains body "location_id"
          && string_contains body "loc_789"
        ) requests in
        assert has_location;
        print_endline "PASS Video container with location")
      (fun err -> failwith ("Video container with location failed: " ^ err)))

(** Test: Story image container with user_tags (mentions are username-only,
    never coordinates) *)
let test_story_container_with_user_tags () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 200;
    body = {|{"id": "story_container_tagged"}|};
    headers = [];
  };

  Instagram.create_story_image_container
    ~ig_user_id:"ig_123"
    ~access_token:"test_token"
    ~image_url:"https://example.com/story.jpg"
    ~user_tags:["mentioned_one"; "mentioned_two"]
    (handle_result
      (fun container_id ->
        assert (container_id = "story_container_tagged");
        let requests = !Mock_http.requests in
        let has_tags = List.exists (fun (_, _, _, body) ->
          string_contains body "user_tags"
          && string_contains body "mentioned_one"
          && string_contains body "mentioned_two"
          && string_contains body "STORIES"
        ) requests in
        assert has_tags;
        (* Story mentions must not carry coordinates. The form-urlencoded
           body percent-encodes the JSON quotes, so a coordinate key would
           appear as %22x%22. *)
        let has_coords = List.exists (fun (_, _, _, body) ->
          string_contains body "%22x%22"
        ) requests in
        assert (not has_coords);
        print_endline "PASS Story container with user_tags")
      (fun err -> failwith ("Story container with user_tags failed: " ^ err)))

(** Test: Story container without user_tags omits the parameter entirely *)
let test_story_container_without_user_tags () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 200;
    body = {|{"id": "story_container_plain"}|};
    headers = [];
  };

  Instagram.create_story_video_container
    ~ig_user_id:"ig_123"
    ~access_token:"test_token"
    ~video_url:"https://example.com/story.mp4"
    (handle_result
      (fun container_id ->
        assert (container_id = "story_container_plain");
        let requests = !Mock_http.requests in
        let has_tags = List.exists (fun (_, _, _, body) ->
          string_contains body "user_tags"
        ) requests in
        assert (not has_tags);
        print_endline "PASS Story container without user_tags")
      (fun err -> failwith ("Story container without user_tags failed: " ^ err)))

(** Test: JSON-special characters in usernames are escaped, not spliced raw
    into the user_tags / collaborators payloads *)
let test_tagging_params_escape_json_special_chars () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 200;
    body = {|{"id": "container_escaped"}|};
    headers = [];
  };

  Instagram.create_image_container
    ~ig_user_id:"ig_123"
    ~access_token:"test_token"
    ~image_url:"https://example.com/image.jpg"
    ~caption:"Escaping test"
    ~alt_text:None
    ~is_carousel_item:false
    ~user_tags:[({|quo"te|}, 0.5, 0.5)]
    ~collaborators:[{|back\slash|}]
    (handle_result
      (fun container_id ->
        assert (container_id = "container_escaped");
        let requests = !Mock_http.requests in
        (* The JSON escape for '"' is '\"'; form-urlencoded that is %5C%22.
           An unescaped quote would terminate the JSON string early and the
           body would carry quo%22te with no %5C. *)
        let has_escaped_quote = List.exists (fun (_, _, _, body) ->
          string_contains body "quo%5C%22te"
        ) requests in
        assert has_escaped_quote;
        let has_escaped_backslash = List.exists (fun (_, _, _, body) ->
          string_contains body "back%5C%5Cslash"
        ) requests in
        assert has_escaped_backslash;
        print_endline "PASS Tagging params escape JSON-special characters")
      (fun err -> failwith ("Tagging params escaping failed: " ^ err)))

(** {1 Reel Parameters Tests (H5)} *)

(** Test: Reel with share_to_feed, cover_url, thumb_offset, audio_name *)
let test_reel_with_extra_params () =
  Mock_config.reset ();

  Mock_http.set_response {
    status = 200;
    body = {|{"id": "reel_container_params"}|};
    headers = [];
  };

  Instagram.create_video_container
    ~ig_user_id:"ig_123"
    ~access_token:"test_token"
    ~video_url:"https://example.com/reel.mp4"
    ~caption:"Reel with params"
    ~alt_text:None
    ~media_type:"REELS"
    ~is_carousel_item:false
    ~share_to_feed:true
    ~cover_url:"https://example.com/cover.jpg"
    ~thumb_offset:5000
    ~audio_name:"My Audio Track"
    (handle_result
      (fun container_id ->
        assert (container_id = "reel_container_params");
        let requests = !Mock_http.requests in
        let has_share_to_feed = List.exists (fun (_, _, _, body) ->
          string_contains body "share_to_feed" && string_contains body "true"
        ) requests in
        assert has_share_to_feed;
        let has_cover = List.exists (fun (_, _, _, body) ->
          string_contains body "cover_url"
        ) requests in
        assert has_cover;
        let has_thumb = List.exists (fun (_, _, _, body) ->
          string_contains body "thumb_offset" && string_contains body "5000"
        ) requests in
        assert has_thumb;
        let has_audio = List.exists (fun (_, _, _, body) ->
          string_contains body "audio_name"
        ) requests in
        assert has_audio;
        print_endline "PASS Reel with extra params (share_to_feed, cover_url, thumb_offset, audio_name)")
      (fun err -> failwith ("Reel with extra params failed: " ^ err)))

(** Test: post_reel passes reel-specific parameters via create_video_container *)
let test_post_reel_with_reel_params () =
  Mock_config.reset ();

  (* Use create_video_container directly to verify reel params are sent *)
  Mock_http.set_response {
    status = 200;
    body = {|{"id": "reel_container_direct"}|};
    headers = [];
  };

  Instagram.create_video_container
    ~ig_user_id:"ig_123"
    ~access_token:"test_token"
    ~video_url:"https://example.com/reel.mp4"
    ~caption:"Reel caption"
    ~alt_text:None
    ~media_type:"REELS"
    ~is_carousel_item:false
    ~share_to_feed:true
    ~cover_url:"https://example.com/cover.jpg"
    ~thumb_offset:3000
    ~audio_name:"Cool Audio"
    (handle_result
      (fun container_id ->
        assert (container_id = "reel_container_direct");
        let requests = !Mock_http.requests in
        let create_req = List.find (fun (meth, _, _, _) -> meth = "POST") requests in
        let (_, _, _, body) = create_req in
        assert (string_contains body "share_to_feed");
        assert (string_contains body "cover_url");
        assert (string_contains body "thumb_offset");
        assert (string_contains body "3000");
        assert (string_contains body "audio_name");
        print_endline "PASS post_reel with reel-specific params (via create_video_container)")
      (fun err -> failwith ("post_reel with reel params failed: " ^ err)))

(** Test: post_reel end-to-end flow with reel-specific params *)
let test_post_reel_e2e_with_params () =
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
  Mock_config.set_ig_user_id ~account_id:"test_account" ~ig_user_id:"ig_user_123";

  Mock_http.set_responses [
    { status = 200; body = {|{"id": "reel_container"}|}; headers = [] };
    { status = 200; body = {|{"status_code": "FINISHED"}|}; headers = [] };
    { status = 200; body = {|{"id": "reel_post_e2e"}|}; headers = [] };
  ];

  Instagram.post_reel
    ~account_id:"test_account"
    ~text:"Reel e2e"
    ~video_url:"https://example.com/reel.mp4"
    ~share_to_feed:(Some true)
    ~cover_url:(Some "https://example.com/cover.jpg")
    ~thumb_offset:(Some 3000)
    ~audio_name:(Some "Cool Audio")
    (handle_outcome
      (fun post_id ->
        assert (post_id = "reel_post_e2e");
        print_endline "PASS post_reel e2e with reel-specific params")
      (fun err -> failwith ("post_reel e2e with params failed: " ^ err)))

(** Run all tests *)
let test_exported_api_constants () =
  assert (Social_instagram_standalone_v25.api_version = "v25.0");
  assert (Social_instagram_standalone_v25.graph_api_base = "https://graph.instagram.com/v25.0");
  assert (Social_instagram_standalone_v25.refresh_endpoint
          = "https://graph.instagram.com/refresh_access_token");
  assert (Social_instagram_standalone_v25.long_lived_token_endpoint
          = "https://graph.instagram.com/access_token");
  assert (OAuth.Metadata.api_base == Social_instagram_standalone_v25.graph_api_base);
  assert (OAuth.Metadata.authorization_endpoint
          == Social_instagram_standalone_v25.oauth_authorize_url);
  assert (OAuth.Metadata.token_endpoint == Social_instagram_standalone_v25.token_endpoint);
  print_endline "✓ Exported API version constants"


(* Graph error body as Meta sends it, for the shared-classifier contract. *)
let graph_error_body ?subcode ?(error_type = "OAuthException") ~message code =
  Printf.sprintf {|{"error":{"message":%S,"type":%S,"code":%d%s,"fbtrace_id":"AvX3kq0Lm2P"}}|}
    message error_type code
    (match subcode with Some s -> Printf.sprintf {|,"error_subcode":%d|} s | None -> "")

let is_rate_limited = function Error_types.Rate_limited _ -> true | _ -> false
let is_permission_error = function
  | Error_types.Auth_error (Error_types.Insufficient_permissions _) -> true
  | _ -> false

(** Instagram maps Graph codes through the table shared with Facebook and
    Threads ([Social_meta_graph_error]). Before, 17/341/368/80002, 3, 100/33 and 506
    were plain Api_errors. Instagram's OAuthException-typed media errors must
    not read as token errors. *)
let test_shared_meta_graph_error_contract () =
  let parse ?(status_code = 400) body =
    Instagram.parse_api_error ~status_code ~response_body:body
  in
  List.iter
    (fun code ->
      assert (is_rate_limited (parse (graph_error_body ~message:"limit" code))))
    [ 4; 9; 17; 32; 341; 368; 613; 80002 ];
  List.iter
    (fun code ->
      assert (is_permission_error (parse (graph_error_body ~message:"denied" code))))
    [ 3; 10; 200; 230; 299 ];
  (match
     parse (graph_error_body ~subcode:33 ~error_type:"GraphMethodException"
              ~message:"Object with ID '1790' does not exist" 100)
   with
   | Error_types.Resource_not_found _ -> ()
   | _ -> failwith "100/33 should be Resource_not_found");
  assert (parse (graph_error_body ~subcode:1455006 ~message:"Duplicate" 506)
          = Error_types.Duplicate_content);
  (match
     parse (graph_error_body ~subcode:2207052
              ~message:"The media could not be fetched from this uri: https://example.com/a.jpg" 9004)
   with
   | Error_types.Api_error { status_code = 400; _ } -> ()
   | _ -> failwith "9004 media error should stay an Api_error");
  print_endline "PASS Shared Meta Graph error contract"

(* Permalink by id: GET /{id}?fields=permalink with appsecret_proof, and the
   three outcomes callers branch on. *)
let test_get_permalink () =
  Mock_config.reset ();
  Mock_config.set_env "INSTAGRAM_APP_SECRET" "app_secret";
  Mock_http.set_responses
    [ { status = 200; headers = [];
        body = {|{"permalink":"https://example.com/p/1","id":"178"}|} } ];
  Instagram.get_permalink ~media_id:"178" ~access_token:"tok"
    (function
      | Ok (Some "https://example.com/p/1") -> ()
      | Ok _ -> failwith "expected the permalink"
      | Error err -> failwith (Error_types.error_to_string err));
  (match !Mock_http.requests with
   | [ ("GET", url, _, _) ] ->
       let uri = Uri.of_string url in
       assert (Uri.host uri = Some "graph.instagram.com");
       assert (Uri.path uri = "/v25.0/178");
       assert (Uri.get_query_param uri "fields" = Some "permalink");
       assert (Uri.get_query_param uri "appsecret_proof" <> None)
   | _ -> failwith "expected one GET");
  Mock_config.reset ();
  Mock_http.set_responses [ { status = 200; headers = []; body = {|{"id":"178"}|} } ];
  Instagram.get_permalink ~media_id:"178" ~access_token:"tok"
    (function
      | Ok None -> ()
      | _ -> failwith "a missing permalink is Ok None");
  Mock_config.reset ();
  Mock_http.set_responses
    [ { status = 400; headers = [];
        body = {|{"error":{"message":"Unsupported get request","type":"GraphMethodException","code":100,"error_subcode":33}}|} } ];
  Instagram.get_permalink ~media_id:"gone" ~access_token:"tok"
    (function
      | Error (Error_types.Resource_not_found _) -> ()
      | Error err ->
          failwith ("expected Resource_not_found, got " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "expected an error");
  Mock_config.reset ();
  Mock_http.set_responses [ { status = 200; headers = []; body = "<html>" } ];
  Instagram.get_permalink ~media_id:"178" ~access_token:"tok"
    (function
      | Error (Error_types.Internal_error _) -> ()
      | _ -> failwith "an unparseable 2xx body is Internal_error");
  print_endline "PASS get_permalink"

(* /{media-id}/comments as graph.instagram.com returns it with the replies
   expansion: newest first, one comment with a first page of replies that
   has its own paging.next, one without replies, and a top-level next link
   that echoes the token. *)
let media_comments_body =
  {|{"data":[
     {"id":"17858893269000001","text":"Love this!","username":"alice.example","timestamp":"2026-11-16T10:05:00+0000","hidden":false,"like_count":3,
      "replies":{"data":[
        {"id":"17858893269000101","text":"@alice.example thank you!","username":"example.brand","timestamp":"2026-11-16T10:07:00+0000","hidden":false,"parent_id":"17858893269000001","like_count":0}],
       "paging":{"cursors":{"before":"QVFIUnJ","after":"QVFIUnK"},
                 "next":"https://graph.instagram.com/v25.0/17858893269000001/replies?access_token=IGOLD&fields=id%2Ctext%2Cparent_id&limit=1&after=QVFIUnK"}}},
     {"id":"17858893269000002","text":"Where can I buy it?","username":"bob.example","timestamp":"2026-11-16T09:00:00+0000","hidden":true,"like_count":0}],
   "paging":{"cursors":{"before":"QVFIUmF","after":"QVFIUmG"},
             "next":"https://graph.instagram.com/v25.0/17900000000000001/comments?access_token=IGOLD&fields=id&limit=2&after=QVFIUmG"}}|}

let test_get_comments_page_with_replies () =
  Mock_config.reset ();
  Mock_config.set_env "INSTAGRAM_APP_SECRET" "app_secret";
  Mock_http.set_responses [ { status = 200; headers = []; body = media_comments_body } ];
  Instagram.get_comments_page ~media_id:"17900000000000001" ~access_token:"ig-token"
    ~replies_limit:1 ~limit:2
    (function
      | Ok page ->
          (match page.data with
           | [ first; second ] ->
               assert (first.Comments.username = Some "alice.example");
               assert (first.hidden = Some false);
               assert (first.like_count = Some 3);
               assert (first.parent_id = None);
               (match first.replies with
                | Some replies ->
                    (match replies.data with
                     | [ reply ] ->
                         assert (reply.parent_id = Some "17858893269000001");
                         assert (reply.replies = None)
                     | _ -> failwith "expected one reply");
                    assert (Social_meta_graph_paging.next_after replies = Some "QVFIUnK")
                | None -> failwith "expected a replies page");
               assert (second.hidden = Some true);
               assert (second.replies = None)
           | _ -> failwith "expected two comments");
          assert (Social_meta_graph_paging.next_after page = Some "QVFIUmG")
      | Error err -> failwith (Error_types.error_to_string err));
  (match !Mock_http.requests with
   | [ ("GET", url, headers, _) ] ->
       let uri = Uri.of_string url in
       assert (Uri.host uri = Some "graph.instagram.com");
       assert (Uri.path uri = "/v25.0/17900000000000001/comments");
       assert (Uri.get_query_param uri "fields"
               = Some "id,text,username,timestamp,hidden,parent_id,like_count,replies.limit(1){id,text,username,timestamp,hidden,parent_id,like_count}");
       assert (Uri.get_query_param uri "limit" = Some "2");
       assert (Uri.get_query_param uri "access_token" = None);
       assert (Uri.get_query_param uri "appsecret_proof" <> None);
       assert (List.assoc_opt "Authorization" headers = Some "Bearer ig-token")
   | _ -> failwith "expected one GET");
  print_endline "✓ get_comments_page with nested replies"

let test_get_comment_replies_page_follow_ups () =
  Mock_config.reset ();
  let replies_body =
    {|{"data":[{"id":"17858893269000102","text":"+1","username":"carol.example","timestamp":"2026-11-16T10:09:00+0000","hidden":false,"parent_id":"17858893269000001","like_count":0}],
       "paging":{"cursors":{"before":"QVFIUnL","after":"QVFIUnL"}}}|}
  in
  Mock_http.set_responses
    [ { status = 200; headers = []; body = replies_body };
      { status = 200; headers = []; body = replies_body } ];
  let expect_last = function
    | Ok page ->
        assert (List.length page.Social_meta_graph_paging.data = 1);
        assert (Social_meta_graph_paging.next_after page = None)
    | Error err -> failwith (Error_types.error_to_string err)
  in
  Instagram.get_comment_replies_page ~comment_id:"17858893269000001" ~access_token:"ig-token"
    ~position:(Social_meta_graph_paging.After "QVFIUnK") ~limit:500 expect_last;
  Instagram.get_comment_replies_page ~comment_id:"ignored" ~access_token:"ig-token"
    ~position:
      (Social_meta_graph_paging.Page_url
         "https://graph.instagram.com/v25.0/17858893269000001/replies?access_token=IGOLD&fields=id%2Ctext%2Cparent_id&limit=1&after=QVFIUnK")
    expect_last;
  (match List.rev !Mock_http.requests with
   | [ ("GET", cursor_url, _, _); ("GET", followed_url, headers, _) ] ->
       let cursor_uri = Uri.of_string cursor_url in
       assert (Uri.path cursor_uri = "/v25.0/17858893269000001/replies");
       assert (Uri.get_query_param cursor_uri "after" = Some "QVFIUnK");
       assert (Uri.get_query_param cursor_uri "limit" = Some "50");
       assert (Uri.get_query_param cursor_uri "fields" = Some Comments.reply_fields);
       let uri = Uri.of_string followed_url in
       assert (Uri.path uri = "/v25.0/17858893269000001/replies");
       assert (Uri.get_query_param uri "fields" = Some "id,text,parent_id");
       assert (Uri.get_query_param uri "access_token" = None);
       assert (List.assoc_opt "Authorization" headers = Some "Bearer ig-token")
   | _ -> failwith "expected two GETs");
  List.iter
    (fun url ->
      Mock_config.reset ();
      Instagram.get_comment_replies_page ~comment_id:"c" ~access_token:"ig-token"
        ~position:(Social_meta_graph_paging.Page_url url)
        (function
          | Error (Error_types.Validation_error _) -> ()
          | _ -> failwith ("expected a rejected paging URL: " ^ url));
      assert (!Mock_http.requests = []))
    [ "https://graph.facebook.com/v25.0/c/replies?after=x";
      "https://graph.instagram.com/v25.0/c/comments?after=x" ];
  print_endline "✓ get_comment_replies_page (cursor, replies.paging.next, rejected URLs)"

let test_comment_reply_hide_delete () =
  Mock_config.reset ();
  Mock_config.set_env "INSTAGRAM_APP_SECRET" "app_secret";
  Mock_http.set_responses
    [ { status = 200; headers = []; body = {|{"id":"17873440459141029"}|} };
      { status = 200; headers = []; body = {|{"success":true}|} };
      { status = 200; headers = []; body = {|{"success":true}|} } ];
  Instagram.reply_to_comment ~comment_id:"17858893269000001" ~access_token:"ig-token"
    ~message:"Thanks for sharing!" (function
    | Ok id -> assert (id = "17873440459141029")
    | Error err -> failwith (Error_types.error_to_string err));
  Instagram.set_comment_hidden ~comment_id:"17858893269000002" ~access_token:"ig-token"
    ~hidden:true (function
    | Ok () -> ()
    | Error err -> failwith (Error_types.error_to_string err));
  Instagram.delete_comment ~comment_id:"17858893269000002" ~access_token:"ig-token"
    (function
      | Ok () -> ()
      | Error err -> failwith (Error_types.error_to_string err));
  (match List.rev !Mock_http.requests with
   | [ ("POST", reply_url, reply_headers, reply_body);
       ("POST", hide_url, _, hide_body);
       ("DELETE", delete_url, delete_headers, _) ] ->
       assert (reply_url = "https://graph.instagram.com/v25.0/17858893269000001/replies");
       assert (string_contains reply_body "message=Thanks");
       assert (string_contains reply_body "appsecret_proof=");
       assert (not (string_contains reply_body "access_token"));
       assert (List.assoc_opt "Authorization" reply_headers = Some "Bearer ig-token");
       assert (hide_url = "https://graph.instagram.com/v25.0/17858893269000002");
       assert (string_contains hide_body "hide=true");
       let delete_uri = Uri.of_string delete_url in
       assert (Uri.path delete_uri = "/v25.0/17858893269000002");
       assert (Uri.get_query_param delete_uri "appsecret_proof" <> None);
       assert (List.assoc_opt "Authorization" delete_headers = Some "Bearer ig-token")
   | _ -> failwith "unexpected moderation requests");
  print_endline "✓ reply_to_comment, set_comment_hidden, delete_comment"

let test_comment_moderation_errors () =
  Mock_config.reset ();
  Mock_http.set_responses
    [ { status = 200; headers = []; body = {|{"success":false}|} };
      { status = 400; headers = [];
        body = {|{"error":{"message":"Unsupported post request. Object with ID '17858893269000404' does not exist, cannot be loaded due to missing permissions, or does not support this operation","type":"GraphMethodException","code":100,"error_subcode":33,"fbtrace_id":"AbC"}}|} } ];
  Instagram.set_comment_hidden ~comment_id:"c" ~access_token:"t" ~hidden:false (function
    | Error (Error_types.Api_error { raw_response = Some _; _ }) -> ()
    | _ -> failwith "success=false must be an error");
  Instagram.reply_to_comment ~comment_id:"17858893269000404" ~access_token:"t" ~message:"hi"
    (function
      | Error (Error_types.Resource_not_found _) -> ()
      | Error err -> failwith ("expected Resource_not_found, got " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "expected an error");
  Mock_config.reset ();
  Instagram.delete_comment ~comment_id:" " ~access_token:"t" (function
    | Error (Error_types.Validation_error _) -> ()
    | _ -> failwith "expected a validation error");
  Instagram.get_comments_page ~media_id:"" ~access_token:"t" (function
    | Error (Error_types.Validation_error _) -> ()
    | _ -> failwith "expected a validation error");
  assert (!Mock_http.requests = []);
  print_endline "✓ comment moderation errors"

let test_comments_page_for_account () =
  Mock_config.reset ();
  Mock_config.set_credentials ~account_id:"acct"
    ~credentials:
      { access_token = "stored-token"; refresh_token = None;
        expires_at = Some "2099-01-01T00:00:00Z"; auth_type = Bearer; scope = None };
  Mock_http.set_responses [ { status = 200; headers = []; body = {|{"data":[]}|} } ];
  Instagram.get_comments_page_for_account ~account_id:"acct" ~media_id:"m" (function
    | Ok { data = []; next_url = None; _ } -> ()
    | _ -> failwith "expected an empty page");
  (match List.filter (fun (m, _, _, _) -> m = "GET") !Mock_http.requests with
   | [ ("GET", _, headers, _) ] ->
       assert (List.assoc_opt "Authorization" headers = Some "Bearer stored-token")
   | _ -> failwith "expected one GET");
  print_endline "✓ get_comments_page_for_account"
let ig_insights_account () =
  Mock_config.reset ();
  Mock_config.set_credentials ~account_id:"acct"
    ~credentials:{ access_token = "ig_tok"; refresh_token = None; expires_at = None;
                   auth_type = Bearer; scope = None };
  Mock_config.set_ig_user_id ~account_id:"acct" ~ig_user_id:"17841400000000000"

let single_get_uri () =
  match !Mock_http.requests with
  | [ ("GET", url, _, _) ] -> Uri.of_string url
  | _ -> failwith "expected one GET"

(* Per-product metric sets from
   https://developers.facebook.com/docs/instagram-platform/reference/instagram-media/insights:
   stories have no likes/comments/saved, reels no follows/profile_visits.
   Response: values[{value}] (lifetime), some metrics with total_value. *)
let test_media_metrics_per_product () =
  let module I = Insights in
  List.iter
    (fun product ->
      assert (I.unsupported_media_metrics product (I.default_media_metrics product) = []))
    [ I.Feed; I.Reels; I.Story ];
  assert (not (List.mem I.Follows (I.supported_media_metrics I.Reels)));
  assert (not (List.mem I.Likes (I.supported_media_metrics I.Story)));
  assert (I.media_product_of_string "reels" = Some I.Reels);
  assert (I.media_product_of_string "AD" = None);
  ig_insights_account ();
  Mock_http.set_response
    { status = 200; headers = [];
      body = {|{"data":[
        {"name":"views","period":"lifetime","values":[{"value":812}],"title":"Views",
         "description":"Total number of times IG Media has been played.","id":"m1/insights/views/lifetime"},
        {"name":"replies","period":"lifetime","values":[{"value":3}],"id":"m1/insights/replies/lifetime"},
        {"name":"profile_visits","period":"lifetime","total_value":{"value":9},"id":"m1/insights/profile_visits/lifetime"}]}|} };
  Instagram.get_media_metrics_for_account ~account_id:"acct" ~post_id:"m1" ~media_product:I.Story
    (function
    | Ok insights ->
        assert (List.mem { I.metric = I.Views; value = 812 } insights);
        assert (List.mem { I.metric = I.Replies; value = 3 } insights);
        assert (List.mem { I.metric = I.Profile_visits; value = 9 } insights)
    | Error e -> failwith (Error_types.error_to_string e));
  let uri = single_get_uri () in
  assert (Uri.path uri = "/v25.0/m1/insights");
  assert (Uri.get_query_param uri "metric"
          = Some "views,reach,shares,total_interactions,follows,profile_visits,replies");
  assert (Uri.get_query_param uri "access_token" = Some "ig_tok");
  (* An unsupported metric fails before any request. *)
  ig_insights_account ();
  Instagram.get_media_metrics ~post_id:"m2" ~access_token:"t" ~media_product:I.Reels
    ~metrics:[ I.Views; I.Follows ] (function
    | Error (Error_types.Validation_error [ Error_types.Option_conflict msg ]) ->
        assert (string_contains msg "follows")
    | _ -> failwith "follows on a reel must be a validation error");
  assert (!Mock_http.requests = []);
  print_endline "✓ Media metrics per product (story, reels validation)"

(* Regression: get_media_insights asked stories for saved/likes/comments,
   which Meta rejects for STORY media. *)
let test_media_insights_story_subset () =
  ig_insights_account ();
  Mock_http.set_response
    { status = 200; headers = [];
      body = {|{"data":[{"name":"views","period":"lifetime","values":[{"value":40}]}]}|} };
  Instagram.get_media_insights ~media_product:Insights.Story ~post_id:"s1" ~access_token:"t"
    (function
    | Ok insights -> assert (insights.views = Some 40 && insights.likes = None)
    | Error e -> failwith (Error_types.error_to_string e));
  assert (Uri.get_query_param (single_get_uri ()) "metric" = Some "views,reach,shares");
  ig_insights_account ();
  Mock_http.set_response { status = 200; headers = []; body = {|{"data":[]}|} };
  Instagram.get_media_insights ~post_id:"p1" ~access_token:"t" (fun _ -> ());
  assert (Uri.get_query_param (single_get_uri ()) "metric"
          = Some "views,reach,saved,likes,comments,shares");
  print_endline "✓ get_media_insights ~media_product:Story asks only story metrics"

(* User insights with metric_type=total_value, shape from
   https://developers.facebook.com/docs/instagram-platform/api-reference/instagram-user/insights *)
let test_account_totals_total_value () =
  ig_insights_account ();
  Mock_http.set_response
    { status = 200; headers = [];
      body = {|{"data":[
        {"name":"views","period":"day","title":"Views","description":"views",
         "total_value":{"value":5120},"id":"17841400000000000/insights/views/day"},
        {"name":"reach","period":"day","title":"Accounts reached","description":"reach",
         "total_value":{"value":224,"breakdowns":[{"dimension_keys":["media_product_type"],
           "results":[{"dimension_values":["POST"],"value":200},{"dimension_values":["CAROUSEL_CONTAINER"],"value":24}]}]},
         "id":"17841400000000000/insights/reach/day"}],
        "paging":{"previous":"https://graph.instagram.com/v25.0/x","next":"https://graph.instagram.com/v25.0/y"}}|} };
  Instagram.get_account_totals_for_account ~account_id:"acct" ~since:"1763164800"
    ~until:"1763337600" (function
    | Ok totals ->
        assert (totals = [ { Insights.metric = Insights.Account_views; total = 5120 };
                           { Insights.metric = Insights.Account_reach; total = 224 } ])
    | Error e -> failwith (Error_types.error_to_string e));
  let uri = single_get_uri () in
  assert (Uri.path uri = "/v25.0/17841400000000000/insights");
  assert (Uri.get_query_param uri "metric_type" = Some "total_value");
  assert (Uri.get_query_param uri "period" = Some "day");
  assert (Uri.get_query_param uri "metric"
          = Some "views,reach,accounts_engaged,total_interactions,likes,comments,shares,saves,replies");
  assert (Uri.get_query_param uri "since" = Some "1763164800");
  print_endline "✓ Account totals use metric_type=total_value"

(* The follower_count insight is a per-day change; followers_count on the
   profile is the total. The two must come out typed apart. *)
let test_follower_change_vs_followers_count () =
  ig_insights_account ();
  Mock_http.set_response
    { status = 200; headers = [];
      body = {|{"data":[{"name":"follower_count","period":"day","values":[
        {"value":4,"end_time":"2026-11-15T08:00:00+0000"},
        {"value":-1,"end_time":"2026-11-16T08:00:00+0000"}],
        "title":"Follower Count","id":"17841400000000000/insights/follower_count/day"}]}|} };
  Instagram.get_daily_follower_changes_for_account ~account_id:"acct" ~since:"1" ~until:"2"
    (function
    | Ok [ { Insights.change = 4; end_time = Some _ }; { Insights.change = -1; _ } ] -> ()
    | Ok _ -> failwith "expected two daily changes"
    | Error e -> failwith (Error_types.error_to_string e));
  let uri = single_get_uri () in
  assert (Uri.get_query_param uri "metric" = Some "follower_count");
  assert (Uri.get_query_param uri "period" = Some "day");
  ig_insights_account ();
  Mock_http.set_response
    { status = 200; headers = [];
      body = {|{"id":"17841400000000000","followers_count":1503,"follows_count":120,"media_count":87}|} };
  Instagram.get_account_counts_for_account ~account_id:"acct" (function
    | Ok { Insights.followers_count = Some 1503; follows_count = Some 120; media_count = Some 87 } -> ()
    | Ok _ -> failwith "unexpected counts"
    | Error e -> failwith (Error_types.error_to_string e));
  let uri = single_get_uri () in
  assert (Uri.path uri = "/v25.0/17841400000000000");
  assert (Uri.get_query_param uri "fields" = Some "id,followers_count,follows_count,media_count");
  assert (Analytics_normalization.instagram_metric_to_canonical "follower_count"
          = Some Analytics_types.Follows);
  print_endline "✓ follower_count changes vs followers_count total"

(* A metric or day Meta left without a value carries no figure. It is left
   out and the rest of the read still arrives (it used to fail as a whole). *)
let test_insights_skip_missing_values () =
  let module I = Insights in
  (match
     I.parse_media_insights
       {|{"data":[
         {"name":"views","period":"lifetime","values":[{"value":812}]},
         {"name":"reach","period":"lifetime","values":[{"value":null}]},
         {"name":"likes","period":"lifetime"},
         {"name":"saved","period":"lifetime","values":[],"total_value":{"value":5}}]}|}
   with
   | Ok insights ->
       assert (insights = [ { I.metric = I.Views; value = 812 }; { I.metric = I.Saved; value = 5 } ])
   | Error msg -> failwith msg);
  (match
     I.parse_account_totals
       {|{"data":[
         {"name":"views","period":"day","total_value":{"value":5120}},
         {"name":"reach","period":"day","total_value":{}},
         {"name":"likes","period":"day"}]}|}
   with
   | Ok totals -> assert (totals = [ { I.metric = I.Account_views; total = 5120 } ])
   | Error msg -> failwith msg);
  (match
     I.parse_follower_changes
       {|{"data":[{"name":"follower_count","period":"day","values":[
         {"value":4,"end_time":"2026-11-15T08:00:00+0000"},
         {"value":null,"end_time":"2026-11-16T08:00:00+0000"},
         {"end_time":"2026-11-17T08:00:00+0000"}]}]}|}
   with
   | Ok [ { I.change = 4; end_time = Some "2026-11-15T08:00:00+0000" } ] -> ()
   | Ok _ -> failwith "expected the one day with a value"
   | Error msg -> failwith msg);
  assert (Result.is_error (I.parse_media_insights {|{"error":{"message":"x"}}|}));
  print_endline "✓ Insights parsers skip values Meta left empty"

let () =
  print_endline "\n=== Instagram Standalone Provider Tests ===\n";

  print_endline "--- OAuth Standalone Tests ---";
  test_exported_api_constants ();
  test_standalone_auth_url_endpoint ();
  test_standalone_authorize_url ();
  test_exchange_code_full_shapes ();
  test_get_me_profile ();
  test_media_metrics_per_product ();
  test_media_insights_story_subset ();
  test_account_totals_total_value ();
  test_follower_change_vs_followers_count ();
  test_insights_skip_missing_values ();
  test_standalone_auth_url_enable_fb_login ();
  test_standalone_auth_url_scopes_comma_separated ();
  test_standalone_auth_url_response_type ();
  test_standalone_auth_url_custom_scopes ();
  test_standalone_exchange_code_happy_path_int_user_id ();
  test_standalone_exchange_code_happy_path_string_user_id ();
  test_standalone_exchange_code_normalizes_auth_type ();
  test_standalone_exchange_code_missing_user_id ();
  test_standalone_exchange_code_http_error ();
  test_standalone_exchange_code_form_urlencodes_special_chars ();
  test_standalone_exchange_long_lived_happy_path ();
  test_standalone_exchange_long_lived_normalizes_auth_type ();
  test_standalone_exchange_long_lived_appsecret_proof ();
  test_standalone_get_profile_happy_path ();
  test_standalone_get_profile_no_picture ();
  test_standalone_get_profile_fields_parameter ();
  test_standalone_get_profile_missing_identifier ();
  test_standalone_oauth_refresh_token ();
  test_standalone_exchange_code_on_response ();
  test_standalone_refresh_token_on_response ();

  print_endline "\n--- Content Publishing Tests ---";
  test_oauth_url ();
  test_api_base_url_uses_instagram ();
  test_rate_limit_parsing ();
  test_create_container ();
  test_publish_container ();
  test_check_status ();
  test_content_validation ();
  test_post_single ();
  test_post_single_nonblocking_sleep ();
  test_post_single_nonblocking_sleep_error ();
  test_post_single_with_collaborators ();
  test_post_single_with_location ();
  test_post_requires_media ();
  test_post_single_rejects_unsupported_media ();
  test_polling_no_publish_on_status_error ();
  test_parse_api_error_token_expired ();
  test_parse_api_error_token_invalid_without_subcode ();
  test_shared_meta_graph_error_contract ();
  test_parse_error_response_token_expired ();
  test_create_container_structured_auth_error ();

  print_endline "\n--- Comment Posting Tests (H1) ---";
  test_post_comment ();
  test_post_comment_auth_error ();

  print_endline "\n--- Media Listing Tests (H2) ---";
  test_get_user_media ();
  test_get_user_media_with_limit ();
  test_get_user_media_with_pagination ();

  print_endline "\n--- Media Deletion Tests (H3) ---";
  test_delete_media ();
  test_delete_media_auth_error ();

  print_endline "\n--- Tagging Tests (H4) ---";
  test_image_container_with_tagging ();
  test_video_container_with_location ();
  test_story_container_with_user_tags ();
  test_story_container_without_user_tags ();
  test_tagging_params_escape_json_special_chars ();

  print_endline "\n--- Reel Parameters Tests (H5) ---";
  test_reel_with_extra_params ();
  test_post_reel_with_reel_params ();
  test_post_reel_e2e_with_params ();

  test_get_permalink ();

  print_endline "\n--- Comment Read and Moderation Tests (I-1) ---";
  test_get_comments_page_with_replies ();
  test_get_comment_replies_page_follow_ups ();
  test_comment_reply_hide_delete ();
  test_comment_moderation_errors ();
  test_comments_page_for_account ();

  print_endline "\n=== All standalone tests passed! ===\n"
