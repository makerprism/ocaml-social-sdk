(** Tests for Reddit API v1 Provider 
    
    Test patterns adapted from go-reddit (https://github.com/vartanbeno/go-reddit)
    and PRAW (https://github.com/praw-dev/praw)
*)

open Social_core
open Social_reddit_v1

(** {1 Test Helpers} *)

(** Helper to check if string contains substring *)
let string_contains s substr =
  try
    ignore (Str.search_forward (Str.regexp_string substr) s 0);
    true
  with Not_found -> false

(** Helper to handle outcome results in tests *)
let handle_outcome on_success on_error outcome =
  match outcome with
  | Error_types.Success result -> on_success result
  | Error_types.Partial_success { result; _ } -> on_success result
  | Error_types.Failure err -> on_error (Error_types.error_to_string err)

(** Helper to handle api_result type for tests *)
let _handle_api_result on_success on_error result =
  match result with
  | Ok value -> on_success value
  | Error err -> on_error (Error_types.error_to_string err)

(** Helper to create future expiration time *)
let future_expiry () =
  match Ptime.add_span (Ptime_clock.now ()) (Ptime.Span.of_int_s 3600) with
  | Some t -> Ptime.to_rfc3339 t
  | None -> ""

(** Helper to create past expiration time (expired) *)
let past_expiry () =
  match Ptime.sub_span (Ptime_clock.now ()) (Ptime.Span.of_int_s 3600) with
  | Some t -> Ptime.to_rfc3339 t
  | None -> ""



(** {1 Mock HTTP Client} *)

module Mock_http = struct
  let requests = ref []
  let response_queue = ref []
  
  let reset () =
    requests := [];
    response_queue := []
  
  (** Set a single response (for backward compatibility) *)
  let set_response response =
    response_queue := [response]
  
  (** Set multiple responses to be returned in order *)
  let set_responses responses =
    response_queue := responses
  
  (** Get next response from queue *)
  let get_next_response () =
    match !response_queue with
    | [] -> None
    | r :: rest ->
        response_queue := rest;
        Some r
  
  include (struct
    let get ?(headers=[]) url on_success on_error =
      requests := ("GET", url, headers, "") :: !requests;
      match get_next_response () with
      | Some response -> on_success response
      | None -> on_error "No mock response set"
    
    let post ?(headers=[]) ?(body="") url on_success on_error =
      requests := ("POST", url, headers, body) :: !requests;
      match get_next_response () with
      | Some response -> on_success response
      | None -> on_error "No mock response set"
    
    let put ?(headers=[]) ?(body="") url on_success on_error =
      requests := ("PUT", url, headers, body) :: !requests;
      match get_next_response () with
      | Some response -> on_success response
      | None -> on_error "No mock response set"
    
    let delete ?(headers=[]) url on_success on_error =
      requests := ("DELETE", url, headers, "") :: !requests;
      match get_next_response () with
      | Some response -> on_success response
      | None -> on_error "No mock response set"
    
    let post_multipart ?(headers=[]) ~parts url on_success on_error =
      let _ = parts in
      requests := ("POST_MULTIPART", url, headers, "") :: !requests;
      match get_next_response () with
      | Some response -> on_success response
      | None -> on_error "No mock response set"
  end : HTTP_CLIENT)
end

(** {1 Mock Config} *)

module Mock_config = struct
  module Http = Mock_http
  
  let env_vars = ref []
  let credentials_store = ref []
  let health_statuses = ref []
  
  let reset () =
    env_vars := [];
    credentials_store := [];
    health_statuses := [];
    Mock_http.reset ()
  
  let set_env key value =
    env_vars := (key, value) :: !env_vars
  
  let get_env key =
    List.assoc_opt key !env_vars
  
  let set_credentials ~account_id ~credentials =
    credentials_store := (account_id, credentials) :: !credentials_store
  
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
    if String.length data > 10 && String.sub data 0 10 = "encrypted:" then
      on_success (String.sub data 10 (String.length data - 10))
    else
      on_error "Invalid encrypted data"
  
  let update_health_status ~account_id ~status ~error_message on_success _on_error =
    health_statuses := (account_id, status, error_message) :: !health_statuses;
    on_success ()
  
  (** Helper: Setup standard test environment with valid credentials *)
  let setup_valid_credentials ?(account_id="test_account") () =
    reset ();
    set_env "REDDIT_CLIENT_ID" "test_client";
    set_env "REDDIT_CLIENT_SECRET" "test_secret";
    set_credentials ~account_id ~credentials:{
      access_token = "test_token";
      refresh_token = Some "refresh_tok";
      expires_at = Some (future_expiry ());
      token_type = "Bearer";
    }
  
  (** Helper: Setup test environment with expired credentials *)
  let setup_expired_credentials ?(account_id="test_account") () =
    reset ();
    set_env "REDDIT_CLIENT_ID" "test_client";
    set_env "REDDIT_CLIENT_SECRET" "test_secret";
    set_credentials ~account_id ~credentials:{
      access_token = "old_token";
      refresh_token = Some "refresh_tok";
      expires_at = Some (past_expiry ());
      token_type = "Bearer";
    }
end

module Reddit = Make(Mock_config)

(** {1 OAuth Tests} *)

let test_oauth_url_generation () =
  let client_id = "test_client_id" in
  let redirect_uri = "https://example.com/callback" in
  let state = "test_state_123" in
  
  let url = OAuth.get_authorization_url ~client_id ~redirect_uri ~state () in
  
  assert (string_contains url "client_id=test_client_id");
  assert (string_contains url "redirect_uri=");
  assert (string_contains url "state=test_state_123");
  assert (string_contains url "response_type=code");
  assert (string_contains url "duration=permanent");
  assert (string_contains url "scope=");
  assert (string_contains url "submit");
  print_endline "✓ OAuth URL generation"

let test_oauth_url_custom_scopes () =
  let client_id = "test_client" in
  let redirect_uri = "https://example.com/cb" in
  let state = "state123" in
  let scopes = ["identity"; "submit"; "read"] in
  
  let url = OAuth.get_authorization_url ~client_id ~redirect_uri ~state ~scopes () in
  
  assert (string_contains url "identity");
  assert (string_contains url "submit");
  assert (string_contains url "read");
  print_endline "✓ OAuth URL with custom scopes"

let test_token_exchange () =
  Mock_config.reset ();
  Mock_config.set_env "REDDIT_CLIENT_ID" "test_client";
  Mock_config.set_env "REDDIT_CLIENT_SECRET" "test_secret";
  
  (* Mock response from Reddit - based on go-reddit testdata *)
  let response_body = {|{
    "access_token": "new_access_token_123",
    "refresh_token": "refresh_token_456",
    "token_type": "bearer",
    "expires_in": 3600,
    "scope": "submit read mysubreddits flair modposts"
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.exchange_code 
    ~code:"test_auth_code"
    ~redirect_uri:"https://example.com/callback"
    (fun creds ->
      assert (creds.access_token = "new_access_token_123");
      assert (creds.refresh_token = Some "refresh_token_456");
      assert (creds.token_type = "bearer");
      assert (creds.expires_at <> None);
      print_endline "✓ Token exchange")
    (fun err -> failwith ("Token exchange failed: " ^ err))

let test_token_exchange_basic_auth () =
  Mock_config.reset ();
  Mock_config.set_env "REDDIT_CLIENT_ID" "my_client";
  Mock_config.set_env "REDDIT_CLIENT_SECRET" "my_secret";
  
  let response_body = {|{"access_token": "tok", "token_type": "bearer", "expires_in": 3600, "scope": "submit read mysubreddits flair modposts"}|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.exchange_code 
    ~code:"code123"
    ~redirect_uri:"https://example.com/cb"
    (fun _ ->
      (* Check that Basic Auth header was used *)
      let (_, _, headers, _) = List.hd !Mock_http.requests in
      let auth_header = List.assoc_opt "Authorization" headers in
      assert (auth_header <> None);
      let auth = Option.get auth_header in
      assert (String.sub auth 0 6 = "Basic ");
      print_endline "✓ Token exchange uses Basic Auth")
    (fun err -> failwith ("Token exchange failed: " ^ err))

let test_token_exchange_rejects_missing_scopes () =
  Mock_config.reset ();
  Mock_config.set_env "REDDIT_CLIENT_ID" "test_client";
  Mock_config.set_env "REDDIT_CLIENT_SECRET" "test_secret";

  let response_body = {|{
    "access_token": "new_access_token_123",
    "refresh_token": "refresh_token_456",
    "token_type": "bearer",
    "expires_in": 3600,
    "scope": "submit read mysubreddits flair"
  }|} in

  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Reddit.exchange_code
    ~code:"test_auth_code"
    ~redirect_uri:"https://example.com/callback"
    (fun _ -> failwith "Token exchange should fail when required scopes are missing")
    (fun err ->
      assert (string_contains err "Missing required Reddit OAuth scopes");
      assert (string_contains err "modposts");
      print_endline "✓ Token exchange rejects missing required scopes")

(** {1 Validation Tests} *)

let test_validate_title_empty () =
  match Reddit.validate_content ~title:"" () with
  | Error msg when string_contains msg "empty" -> 
      print_endline "✓ Title validation - empty rejected"
  | _ -> failwith "Empty title should fail validation"

let test_validate_title_too_long () =
  let long_title = String.make 301 'x' in
  match Reddit.validate_content ~title:long_title () with
  | Error msg when string_contains msg "300" -> 
      print_endline "✓ Title validation - too long rejected"
  | _ -> failwith "Long title should fail validation"

let test_validate_title_valid () =
  match Reddit.validate_content ~title:"Test Post Title" () with
  | Ok () -> print_endline "✓ Title validation - valid passes"
  | Error e -> failwith ("Valid title should pass: " ^ e)

let test_validate_body_too_long () =
  let long_body = String.make 40001 'x' in
  match Reddit.validate_content ~title:"Title" ~body:long_body () with
  | Error msg when string_contains msg "40000" -> 
      print_endline "✓ Body validation - too long rejected"
  | _ -> failwith "Long body should fail validation"

let test_validate_body_valid () =
  let body = "This is a test post body with some content." in
  match Reddit.validate_content ~title:"Title" ~body () with
  | Ok () -> print_endline "✓ Body validation - valid passes"
  | Error e -> failwith ("Valid body should pass: " ^ e)

let test_validate_media_video_too_large () =
  let media : Platform_types.post_media = {
    media_type = Platform_types.Video;
    mime_type = "video/mp4";
    file_size_bytes = (1024 * 1024 * 1024) + 1;
    width = Some 1920;
    height = Some 1080;
    duration_seconds = Some 30.0;
    alt_text = None;
  } in
  match Reddit.validate_media ~media with
  | Error [Error_types.Media_too_large _] ->
      print_endline "✓ Video validation - oversized video rejected"
  | Error errs ->
      failwith ("Unexpected video size validation result: " ^ String.concat ", " (List.map Error_types.validation_error_to_string errs))
  | Ok () ->
      failwith "Oversized video should fail validation"

let test_validate_media_video_too_long () =
  let media : Platform_types.post_media = {
    media_type = Platform_types.Video;
    mime_type = "video/mp4";
    file_size_bytes = 1024 * 1024;
    width = Some 1920;
    height = Some 1080;
    duration_seconds = Some 901.0;
    alt_text = None;
  } in
  match Reddit.validate_media ~media with
  | Error [Error_types.Video_too_long _] ->
      print_endline "✓ Video validation - overlong video rejected"
  | Error errs ->
      failwith ("Unexpected video duration validation result: " ^ String.concat ", " (List.map Error_types.validation_error_to_string errs))
  | Ok () ->
      failwith "Overlong video should fail validation"

(** {1 Post Submission Tests} *)

let test_submit_self_post () =
  Mock_config.setup_valid_credentials ();
  
  (* Mock response - based on go-reddit testdata/post/submit.json *)
  let response_body = {|{
    "json": {
      "errors": [],
      "data": {
        "url": "https://www.reddit.com/r/test/comments/hw6l6a/test_title/",
        "drafts_count": 0,
        "id": "hw6l6a",
        "name": "t3_hw6l6a"
      }
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Test Title"
    ~body:"Test Text"
    ~spoiler:true
    ()
    (handle_outcome
      (fun post_id ->
        assert (post_id = "hw6l6a");
        
        (* Verify request parameters - based on go-reddit test assertions *)
        let (method_, url, _, body) = List.hd !Mock_http.requests in
        assert (method_ = "POST");
        assert (string_contains url "/api/submit");
        assert (string_contains body "api_type=json");
        assert (string_contains body "kind=self");
        assert (string_contains body "sr=test");
        assert (string_contains body "title=Test%20Title");
        assert (string_contains body "text=Test%20Text");
        assert (string_contains body "spoiler=true");
        print_endline "✓ Submit self post")
      (fun err -> failwith ("Submit failed: " ^ err)))

let test_submit_link_post () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|{
    "json": {
      "errors": [],
      "data": {
        "url": "https://www.reddit.com/r/test/comments/hw6l6a/test_title/",
        "id": "hw6l6a",
        "name": "t3_hw6l6a"
      }
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_link_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Test Title"
    ~url:"https://www.example.com"
    ~resubmit:true
    ~nsfw:true
    ()
    (handle_outcome
      (fun post_id ->
        assert (post_id = "hw6l6a");
        
        let (_, url, _, body) = List.hd !Mock_http.requests in
        assert (string_contains url "/api/submit");
        assert (string_contains body "kind=link");
        assert (string_contains body "url=https");
        assert (string_contains body "resubmit=true");
        assert (string_contains body "nsfw=true");
        print_endline "✓ Submit link post")
      (fun err -> failwith ("Submit link failed: " ^ err)))

let test_submit_post_with_flair () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|{
    "json": {
      "errors": [],
      "data": {
        "id": "flair123",
        "name": "t3_flair123",
        "url": "https://www.reddit.com/r/test/comments/flair123/test/"
      }
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Test Post"
    ~flair_id:"flair_template_123"
    ~flair_text:"Discussion"
    ()
    (handle_outcome
      (fun _ ->
        let (_, _, _, body) = List.hd !Mock_http.requests in
        assert (string_contains body "flair_id=flair_template_123");
        assert (string_contains body "flair_text=Discussion");
        print_endline "✓ Submit post with flair")
      (fun err -> failwith ("Submit with flair failed: " ^ err)))

let test_delete_post () =
  Mock_config.setup_valid_credentials ();
  
  Mock_http.set_response { status = 200; body = "{}"; headers = [] };
  
  Reddit.delete_post
    ~account_id:"test_account"
    ~post_id:"t3_test"
    (fun outcome ->
      match outcome with
      | Error_types.Success () ->
          let (method_, url, _, body) = List.hd !Mock_http.requests in
          assert (method_ = "POST");
          assert (string_contains url "/api/del");
          assert (string_contains body "id=t3_test");
          print_endline "✓ Delete post"
      | Error_types.Failure err ->
          failwith ("Delete failed: " ^ Error_types.error_to_string err)
      | _ -> failwith "Unexpected outcome")

(** {1 Subreddit Tests} *)

let test_get_moderated_subreddits () =
  Mock_config.setup_valid_credentials ();
  
  (* Mock response - based on go-reddit testdata/subreddit/list.json *)
  let response_body = {|{
    "kind": "Listing",
    "data": {
      "children": [
        {
          "kind": "t5",
          "data": {
            "id": "2qs0k",
            "display_name": "Home",
            "subscribers": 15336,
            "over18": false,
            "link_flair_enabled": true,
            "submission_type": "any"
          }
        },
        {
          "kind": "t5",
          "data": {
            "id": "2qh1i",
            "display_name": "AskReddit",
            "subscribers": 28449174,
            "over18": false,
            "link_flair_enabled": true,
            "submission_type": "self"
          }
        }
      ],
      "after": "t5_2qh0u"
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.get_moderated_subreddits
    ~account_id:"test_account"
    (handle_outcome
      (fun (subreddits : Social_reddit_v1.subreddit list) ->
        assert (List.length subreddits = 2);
        let first = List.hd subreddits in
        assert (first.name = "Home");
        assert (first.subscribers = 15336);
        assert (first.flair_enabled = true);
        print_endline "✓ Get moderated subreddits")
      (fun err -> failwith ("Get moderated failed: " ^ err)))

let test_get_subreddit_flairs () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|[
    {
      "id": "flair_1",
      "text": "Discussion",
      "text_editable": false,
      "background_color": "#0055ff",
      "text_color": "light"
    },
    {
      "id": "flair_2",
      "text": "Question",
      "text_editable": true,
      "background_color": "#ff5500",
      "text_color": "dark"
    }
  ]|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.get_subreddit_flairs
    ~account_id:"test_account"
    ~subreddit:"test"
    (handle_outcome
      (fun (flairs : Social_reddit_v1.flair list) ->
        assert (List.length flairs = 2);
        let first = List.hd flairs in
        assert (first.flair_id = "flair_1");
        assert (first.flair_text = "Discussion");
        assert (first.flair_text_editable = false);
        print_endline "✓ Get subreddit flairs")
      (fun err -> failwith ("Get flairs failed: " ^ err)))

let test_check_flair_required () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|{
    "is_flair_required": true,
    "title_text_min_length": 0,
    "title_text_max_length": 300
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.check_flair_required
    ~account_id:"test_account"
    ~subreddit:"AskReddit"
    (handle_outcome
      (fun is_required ->
        assert (is_required = true);
        print_endline "✓ Check flair required")
      (fun err -> failwith ("Check flair required failed: " ^ err)))

let test_get_user_info () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|{
    "id": "abc123",
    "name": "testuser",
    "icon_img": "https://www.redditstatic.com/avatars/avatar.png",
    "total_karma": 1234,
    "created_utc": 1234567890.0
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.get_user_info
    ~account_id:"test_account"
    (handle_outcome
      (fun (user : Social_reddit_v1.user_info) ->
        assert (user.name = "testuser");
        assert (user.total_karma = 1234);
        print_endline "✓ Get user info")
      (fun err -> failwith ("Get user info failed: " ^ err)))

(** {1 Error Handling Tests} *)

let test_rate_limit_error () =
  Mock_config.setup_valid_credentials ();
  
  (* Rate limit error response - based on PRAW test cases *)
  let response_body = {|{
    "json": {
      "errors": [["RATELIMIT", "You are doing that too much. Try again in 5 minutes.", "ratelimit"]]
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Test"
    ()
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Rate_limited info) ->
          assert (info.retry_after_seconds = Some 300);
          print_endline "✓ Rate limit error handling"
      | Error_types.Success _ ->
          failwith "Should have failed with rate limit"
      | Error_types.Failure err ->
          failwith ("Wrong error type: " ^ Error_types.error_to_string err)
      | _ -> failwith "Unexpected outcome")

let test_rate_limit_seconds () =
  Mock_config.setup_valid_credentials ();
  
  (* Rate limit with seconds - based on PRAW test case *)
  let response_body = {|{
    "json": {
      "errors": [["RATELIMIT", "You are doing that too much. Try again in 6 seconds.", "ratelimit"]]
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Test"
    ()
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Rate_limited info) ->
          assert (info.retry_after_seconds = Some 6);
          print_endline "✓ Rate limit with seconds"
      | _ -> failwith "Should have failed with rate limit")

let test_rate_limit_singular_minute () =
  Mock_config.setup_valid_credentials ();
  
  (* Rate limit with singular minute - based on PRAW test case *)
  let response_body = {|{
    "json": {
      "errors": [["RATELIMIT", "You are doing that too much. Try again in 1 minute.", "ratelimit"]]
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Test"
    ()
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Rate_limited info) ->
          assert (info.retry_after_seconds = Some 60);
          print_endline "✓ Rate limit with 1 minute"
      | _ -> failwith "Should have failed with rate limit")

let test_auth_error () =
  Mock_config.setup_valid_credentials ();
  
  Mock_http.set_response { status = 401; body = {|{"message": "Unauthorized", "error": 401}|}; headers = [] };
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Test"
    ()
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Auth_error Error_types.Token_invalid) ->
          print_endline "✓ Auth error handling"
      | Error_types.Success _ ->
          failwith "Should have failed with auth error"
      | Error_types.Failure err ->
          failwith ("Wrong error type: " ^ Error_types.error_to_string err)
      | _ -> failwith "Unexpected outcome")

let test_subreddit_not_allowed () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|{
    "json": {
      "errors": [["SUBREDDIT_NOTALLOWED", "you aren't allowed to post there.", "sr"]]
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"private_sub"
    ~title:"Test"
    ()
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Auth_error (Error_types.Insufficient_permissions _)) ->
          print_endline "✓ Subreddit not allowed error"
      | Error_types.Success _ ->
          failwith "Should have failed"
      | Error_types.Failure err ->
          failwith ("Wrong error type: " ^ Error_types.error_to_string err)
      | _ -> failwith "Unexpected outcome")

let test_no_selfs_error () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|{
    "json": {
      "errors": [["NO_SELFS", "This subreddit doesn't allow text posts", "kind"]]
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"pics"
    ~title:"Test"
    ()
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Api_error info) ->
          assert (string_contains info.message "text posts");
          print_endline "✓ NO_SELFS error handling"
      | _ -> failwith "Should have failed with API error")

let test_no_links_error () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|{
    "json": {
      "errors": [["NO_LINKS", "This subreddit only allows text posts", "kind"]]
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_link_post
    ~account_id:"test_account"
    ~subreddit:"askreddit"
    ~title:"Test"
    ~url:"https://example.com"
    ()
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Api_error info) ->
          assert (string_contains info.message "link posts");
          print_endline "✓ NO_LINKS error handling"
      | _ -> failwith "Should have failed with API error")

let test_bad_subreddit_name () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|{
    "json": {
      "errors": [["BAD_SR_NAME", "that subreddit doesn't exist", "sr"]]
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"nonexistent_sub_12345"
    ~title:"Test"
    ()
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Resource_not_found _) ->
          print_endline "✓ BAD_SR_NAME error handling"
      | _ -> failwith "Should have failed with resource not found")

let test_duplicate_post () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|{
    "json": {
      "errors": [["ALREADY_SUB", "that link has already been submitted", "url"]]
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_link_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Test"
    ~url:"https://already-submitted.com"
    ()
    (fun outcome ->
      match outcome with
      | Error_types.Failure Error_types.Duplicate_content ->
          print_endline "✓ ALREADY_SUB error handling"
      | _ -> failwith "Should have failed with duplicate content")

(** {1 Crosspost Tests} *)

let test_submit_crosspost () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|{
    "json": {
      "errors": [],
      "data": {
        "id": "crosspost123",
        "name": "t3_crosspost123",
        "url": "https://www.reddit.com/r/target/comments/crosspost123/test/"
      }
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_crosspost
    ~account_id:"test_account"
    ~subreddit:"target_subreddit"
    ~title:"Crosspost Title"
    ~original_post_id:"t3_original123"
    ()
    (handle_outcome
      (fun post_id ->
        assert (post_id = "crosspost123");
        
        let (_, _, _, body) = List.hd !Mock_http.requests in
        assert (string_contains body "kind=crosspost");
        assert (string_contains body "crosspost_fullname=t3_original123");
        print_endline "✓ Submit crosspost")
      (fun err -> failwith ("Crosspost failed: " ^ err)))

(** {1 post_single Tests} *)

let test_post_single_self () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|{
    "json": {
      "errors": [],
      "data": {"id": "self123", "name": "t3_self123", "url": "..."}
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.post_single
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Self Post"
    ~body:"Body text"
    ()
    (handle_outcome
      (fun _ ->
        let (_, _, _, body) = List.hd !Mock_http.requests in
        assert (string_contains body "kind=self");
        print_endline "✓ post_single as self post")
      (fun err -> failwith ("post_single failed: " ^ err)))

let test_post_single_link () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|{
    "json": {
      "errors": [],
      "data": {"id": "link123", "name": "t3_link123", "url": "..."}
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.post_single
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Link Post"
    ~url:"https://example.com"
    ()
    (handle_outcome
      (fun _ ->
        let (_, _, _, body) = List.hd !Mock_http.requests in
        assert (string_contains body "kind=link");
        print_endline "✓ post_single as link post")
      (fun err -> failwith ("post_single failed: " ^ err)))

let test_post_single_video_uses_native_video_flow () =
  Mock_config.setup_valid_credentials ();

  let video_download_response = {
    status = 200;
    body = "fake_video_binary";
    headers = [ ("content-type", "video/mp4") ];
  } in
  let upload_lease_response = {
    status = 200;
    body = {|{
      "args": {
        "action": "https://reddit-upload.s3.amazonaws.com",
        "fields": [
          {"name": "key", "value": "uploads/video_key.mp4"},
          {"name": "x-amz-algorithm", "value": "AWS4-HMAC-SHA256"}
        ],
        "asset": {
          "asset_id": "asset_video_123",
          "websocket_url": "wss://reddit.example/ws/asset_video_123"
        }
      }
    }|};
    headers = [];
  } in
  let s3_upload_response = { status = 204; body = ""; headers = [] } in
  let submit_response = {
    status = 200;
    body = {|{
      "json": {
        "errors": [],
        "data": {"id": "vid123", "name": "t3_vid123", "url": "..."}
      }
    }|};
    headers = [];
  } in

  Mock_http.set_responses [
    video_download_response;
    upload_lease_response;
    s3_upload_response;
    submit_response;
  ];

  Reddit.post_single
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Video Post"
    ~media_urls:["https://cdn.example.com/video.mp4"]
    ()
    (handle_outcome
      (fun post_id ->
        assert (post_id = "vid123");
        let chronological = List.rev !Mock_http.requests in
        assert (List.length chronological = 4);

        let (m1, u1, _, _) = List.nth chronological 0 in
        assert (m1 = "GET");
        assert (u1 = "https://cdn.example.com/video.mp4");

        let (m2, u2, _, b2) = List.nth chronological 1 in
        assert (m2 = "POST");
        assert (string_contains u2 "/api/media/asset.json");
        assert (string_contains b2 "mimetype=video%2Fmp4");

        let (m3, u3, _, _) = List.nth chronological 2 in
        assert (m3 = "POST_MULTIPART");
        assert (u3 = "https://reddit-upload.s3.amazonaws.com");

        let (m4, u4, _, b4) = List.nth chronological 3 in
        assert (m4 = "POST");
        assert (string_contains u4 "/api/submit");
        assert (string_contains b4 "kind=video");
        assert (string_contains b4 "url=https");
        assert (string_contains b4 "video_key.mp4");
        print_endline "✓ post_single video uses native upload + submit flow")
      (fun err -> failwith ("Native video flow failed: " ^ err)))

let test_post_single_unknown_extension_video_uses_native_flow () =
  Mock_config.setup_valid_credentials ();

  let video_download_response = {
    status = 200;
    body = "fake_video_binary";
    headers = [ ("content-type", "video/mp4") ];
  } in
  let upload_lease_response = {
    status = 200;
    body = {|{
      "args": {
        "action": "https://reddit-upload.s3.amazonaws.com",
        "fields": [
          {"name": "key", "value": "uploads/video_key_noext.mp4"}
        ],
        "asset": {
          "asset_id": "asset_video_456"
        }
      }
    }|};
    headers = [];
  } in
  let s3_upload_response = { status = 204; body = ""; headers = [] } in
  let submit_response = {
    status = 200;
    body = {|{
      "json": {
        "errors": [],
        "data": {"id": "vid456", "name": "t3_vid456", "url": "..."}
      }
    }|};
    headers = [];
  } in

  Mock_http.set_responses [
    video_download_response;
    upload_lease_response;
    s3_upload_response;
    submit_response;
  ];

  Reddit.post_single
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Video without extension"
    ~media_urls:["https://cdn.example.com/asset?id=123"]
    ()
    (handle_outcome
      (fun post_id ->
        assert (post_id = "vid456");
        let chronological = List.rev !Mock_http.requests in
        assert (List.length chronological = 4);
        let (_, _, _, submit_body) = List.nth chronological 3 in
        assert (string_contains submit_body "kind=video");
        print_endline "✓ post_single routes unknown extension media via native video flow")
      (fun err -> failwith ("Unknown-extension video flow failed: " ^ err)))

let test_post_single_video_content_type_with_charset_is_supported () =
  Mock_config.setup_valid_credentials ();

  let video_download_response = {
    status = 200;
    body = "fake_video_binary";
    headers = [ ("content-type", "video/mp4; charset=binary") ];
  } in
  let upload_lease_response = {
    status = 200;
    body = {|{
      "args": {
        "action": "https://reddit-upload.s3.amazonaws.com",
        "fields": [
          {"name": "key", "value": "uploads/video_charset.mp4"}
        ],
        "asset": {
          "asset_id": "asset_video_charset"
        }
      }
    }|};
    headers = [];
  } in
  let s3_upload_response = { status = 204; body = ""; headers = [] } in
  let submit_response = {
    status = 200;
    body = {|{
      "json": {
        "errors": [],
        "data": {"id": "vidcharset", "name": "t3_vidcharset", "url": "..."}
      }
    }|};
    headers = [];
  } in

  Mock_http.set_responses [
    video_download_response;
    upload_lease_response;
    s3_upload_response;
    submit_response;
  ];

  Reddit.post_single
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Video with content-type params"
    ~media_urls:["https://cdn.example.com/video-param"]
    ()
    (handle_outcome
      (fun post_id ->
        assert (post_id = "vidcharset");
        let chronological = List.rev !Mock_http.requests in
        let (_, _, _, lease_body) = List.nth chronological 1 in
        assert (string_contains lease_body "mimetype=video%2Fmp4");
        print_endline "✓ post_single supports video content-type with parameters")
      (fun err -> failwith ("Video content-type normalization failed: " ^ err)))

let test_post_single_unknown_extension_image_falls_back_to_image_flow () =
  Mock_config.setup_valid_credentials ();

  let image_download_response = {
    status = 200;
    body = "fake_image_binary";
    headers = [ ("content-type", "image/jpeg") ];
  } in
  let image_submit_response = {
    status = 200;
    body = {|{
      "json": {
        "errors": [],
        "data": {"id": "img456", "name": "t3_img456", "url": "..."}
      }
    }|};
    headers = [];
  } in

  Mock_http.set_responses [
    image_download_response;
    image_submit_response;
  ];

  Reddit.post_single
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Image without extension"
    ~media_urls:["https://cdn.example.com/asset?id=img"]
    ()
    (handle_outcome
      (fun post_id ->
        assert (post_id = "img456");
        let chronological = List.rev !Mock_http.requests in
        assert (List.length chronological = 2);
        let (m1, _, _, _) = List.nth chronological 0 in
        assert (m1 = "GET");
        let (m2, u2, _, b2) = List.nth chronological 1 in
        assert (m2 = "POST");
        assert (string_contains u2 "/api/submit");
        assert (string_contains b2 "kind=image");
        print_endline "✓ post_single falls back to image flow for unknown extension image media")
      (fun err -> failwith ("Unknown-extension image fallback failed: " ^ err)))

let test_post_single_unknown_extension_non_image_fails_without_fallback () =
  Mock_config.setup_valid_credentials ();

  let non_media_download_response = {
    status = 200;
    body = "fake_pdf_binary";
    headers = [ ("content-type", "application/pdf") ];
  } in

  Mock_http.set_responses [ non_media_download_response ];

  Reddit.post_single
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Unsupported media type"
    ~media_urls:["https://cdn.example.com/asset?id=pdf"]
    ()
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Validation_error [Error_types.Media_unsupported_format fmt]) ->
          assert (fmt = "application/pdf");
          let chronological = List.rev !Mock_http.requests in
          assert (List.length chronological = 1);
          print_endline "✓ post_single unknown extension non-image fails without image fallback"
      | Error_types.Failure err ->
          failwith ("Unexpected error for unsupported unknown-extension media: " ^ Error_types.error_to_string err)
      | Error_types.Success _ ->
          failwith "Unsupported unknown-extension media should not succeed"
      | Error_types.Partial_success _ ->
          failwith "Unsupported unknown-extension media should not partially succeed")

let test_post_single_video_with_thumbnail_uses_poster_flow () =
  Mock_config.setup_valid_credentials ();

  let video_download_response = {
    status = 200;
    body = "fake_video_binary";
    headers = [ ("content-type", "video/mp4") ];
  } in
  let video_upload_lease_response = {
    status = 200;
    body = {|{
      "args": {
        "action": "https://reddit-upload.s3.amazonaws.com",
        "fields": [
          {"name": "key", "value": "uploads/video_with_thumb.mp4"}
        ],
        "asset": {
          "asset_id": "asset_video_789"
        }
      }
    }|};
    headers = [];
  } in
  let video_s3_upload_response = { status = 204; body = ""; headers = [] } in
  let poster_download_response = {
    status = 200;
    body = "fake_poster_binary";
    headers = [ ("content-type", "image/jpeg") ];
  } in
  let poster_upload_lease_response = {
    status = 200;
    body = {|{
      "args": {
        "action": "https://reddit-upload.s3.amazonaws.com",
        "fields": [
          {"name": "key", "value": "uploads/poster_for_video.jpg"}
        ],
        "asset": {
          "asset_id": "asset_poster_789"
        }
      }
    }|};
    headers = [];
  } in
  let poster_s3_upload_response = { status = 204; body = ""; headers = [] } in
  let submit_response = {
    status = 200;
    body = {|{
      "json": {
        "errors": [],
        "data": {"id": "vid789", "name": "t3_vid789", "url": "..."}
      }
    }|};
    headers = [];
  } in

  Mock_http.set_responses [
    video_download_response;
    video_upload_lease_response;
    video_s3_upload_response;
    poster_download_response;
    poster_upload_lease_response;
    poster_s3_upload_response;
    submit_response;
  ];

  Reddit.post_single
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Video with thumbnail"
    ~media_urls:["https://cdn.example.com/video.mp4"; "https://cdn.example.com/poster.jpg"]
    ()
    (handle_outcome
      (fun post_id ->
        assert (post_id = "vid789");
        let chronological = List.rev !Mock_http.requests in
        assert (List.length chronological = 7);
        let (m4, u4, _, _) = List.nth chronological 3 in
        assert (m4 = "GET");
        assert (u4 = "https://cdn.example.com/poster.jpg");
        let (_, submit_url, _, submit_body) = List.nth chronological 6 in
        assert (string_contains submit_url "/api/submit");
        assert (string_contains submit_body "kind=video");
        assert (string_contains submit_body "video_poster_url=https");
        assert (string_contains submit_body "poster_for_video.jpg");
        print_endline "✓ post_single video with thumbnail uploads and submits poster URL")
      (fun err -> failwith ("Video thumbnail flow failed: " ^ err)))

let test_post_single_video_with_unknown_extension_thumbnail_uses_poster_flow () =
  Mock_config.setup_valid_credentials ();

  let video_download_response = {
    status = 200;
    body = "fake_video_binary";
    headers = [ ("content-type", "video/mp4") ];
  } in
  let video_upload_lease_response = {
    status = 200;
    body = {|{
      "args": {
        "action": "https://reddit-upload.s3.amazonaws.com",
        "fields": [
          {"name": "key", "value": "uploads/video_unknown_thumb.mp4"}
        ],
        "asset": {
          "asset_id": "asset_video_100"
        }
      }
    }|};
    headers = [];
  } in
  let video_s3_upload_response = { status = 204; body = ""; headers = [] } in
  let poster_download_response = {
    status = 200;
    body = "fake_poster_binary";
    headers = [ ("content-type", "image/png; charset=binary") ];
  } in
  let poster_upload_lease_response = {
    status = 200;
    body = {|{
      "args": {
        "action": "https://reddit-upload.s3.amazonaws.com",
        "fields": [
          {"name": "key", "value": "uploads/poster_unknown_ext.png"}
        ],
        "asset": {
          "asset_id": "asset_poster_100"
        }
      }
    }|};
    headers = [];
  } in
  let poster_s3_upload_response = { status = 204; body = ""; headers = [] } in
  let submit_response = {
    status = 200;
    body = {|{
      "json": {
        "errors": [],
        "data": {"id": "vid100", "name": "t3_vid100", "url": "..."}
      }
    }|};
    headers = [];
  } in

  Mock_http.set_responses [
    video_download_response;
    video_upload_lease_response;
    video_s3_upload_response;
    poster_download_response;
    poster_upload_lease_response;
    poster_s3_upload_response;
    submit_response;
  ];

  Reddit.post_single
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Video with unknown-extension thumbnail"
    ~media_urls:["https://cdn.example.com/video.mp4"; "https://cdn.example.com/asset?id=poster"]
    ()
    (handle_outcome
      (fun post_id ->
        assert (post_id = "vid100");
        let chronological = List.rev !Mock_http.requests in
        assert (List.length chronological = 7);
        let (m4, u4, _, _) = List.nth chronological 3 in
        assert (m4 = "GET");
        assert (u4 = "https://cdn.example.com/asset?id=poster");
        let (_, submit_url, _, submit_body) = List.nth chronological 6 in
        assert (string_contains submit_url "/api/submit");
        assert (string_contains submit_body "video_poster_url=https");
        assert (string_contains submit_body "poster_unknown_ext.png");
        print_endline "✓ post_single accepts unknown-extension thumbnail and validates by content-type")
      (fun err -> failwith ("Unknown-extension thumbnail flow failed: " ^ err)))

let test_post_single_video_with_invalid_thumbnail_still_posts () =
  Mock_config.setup_valid_credentials ();

  let video_download_response = {
    status = 200;
    body = "fake_video_binary";
    headers = [ ("content-type", "video/mp4") ];
  } in
  let video_upload_lease_response = {
    status = 200;
    body = {|{
      "args": {
        "action": "https://reddit-upload.s3.amazonaws.com",
        "fields": [
          {"name": "key", "value": "uploads/video_no_thumb.mp4"}
        ],
        "asset": {
          "asset_id": "asset_video_200"
        }
      }
    }|};
    headers = [];
  } in
  let video_s3_upload_response = { status = 204; body = ""; headers = [] } in
  let invalid_poster_download_response = {
    status = 200;
    body = "fake_pdf_binary";
    headers = [ ("content-type", "application/pdf") ];
  } in
  let submit_response = {
    status = 200;
    body = {|{
      "json": {
        "errors": [],
        "data": {"id": "vid200", "name": "t3_vid200", "url": "..."}
      }
    }|};
    headers = [];
  } in

  Mock_http.set_responses [
    video_download_response;
    video_upload_lease_response;
    video_s3_upload_response;
    invalid_poster_download_response;
    submit_response;
  ];

  Reddit.post_single
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Video should post without invalid thumbnail"
    ~media_urls:["https://cdn.example.com/video.mp4"; "https://cdn.example.com/not-an-image"]
    ()
    (handle_outcome
      (fun post_id ->
        assert (post_id = "vid200");
        let chronological = List.rev !Mock_http.requests in
        assert (List.length chronological = 5);
        let (_, submit_url, _, submit_body) = List.nth chronological 4 in
        assert (string_contains submit_url "/api/submit");
        assert (string_contains submit_body "kind=video");
        assert (not (string_contains submit_body "video_poster_url="));
        print_endline "✓ invalid thumbnail does not block video post")
      (fun err -> failwith ("Invalid thumbnail should not block post: " ^ err)))

let test_post_single_media_takes_precedence_over_url () =
  Mock_config.setup_valid_credentials ();

  let response_body = {|{
    "json": {
      "errors": [],
      "data": {"id": "img999", "name": "t3_img999", "url": "..."}
    }
  }|} in

  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Reddit.post_single
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Media Wins"
    ~url:"https://example.com/article"
    ~media_urls:["https://cdn.example.com/photo.jpg"]
    ()
    (handle_outcome
      (fun _ ->
        let (_, _, _, body) = List.hd !Mock_http.requests in
        assert (string_contains body "kind=image");
        assert (string_contains body "url=https");
        assert (string_contains body "photo.jpg");
        assert (not (string_contains body "kind=link"));
        print_endline "✓ post_single prefers media over link URL")
      (fun err -> failwith ("post_single precedence failed: " ^ err)))

(** {1 Token Refresh Tests} *)

let test_token_refresh_expired () =
  Mock_config.setup_expired_credentials ();
  
  (* First response: token refresh, second: API call *)
  let refresh_response = {|{
    "access_token": "new_access_token",
    "refresh_token": "new_refresh_token",
    "token_type": "bearer",
    "expires_in": 3600,
    "scope": "submit"
  }|} in
  let submit_response = {|{
    "json": {
      "errors": [],
      "data": {"id": "refreshed123", "name": "t3_refreshed123", "url": "..."}
    }
  }|} in
  
  Mock_http.set_responses [
    { status = 200; body = refresh_response; headers = [] };
    { status = 200; body = submit_response; headers = [] };
  ];
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Test Post After Refresh"
    ()
    (handle_outcome
      (fun post_id ->
        assert (post_id = "refreshed123");
        print_endline "✓ Token refresh when expired")
      (fun err -> failwith ("Token refresh failed: " ^ err)))

let test_token_refresh_no_refresh_token () =
  Mock_config.reset ();
  Mock_config.set_env "REDDIT_CLIENT_ID" "test_client";
  Mock_config.set_env "REDDIT_CLIENT_SECRET" "test_secret";
  
  (* Set up credentials that are expired but have no refresh token *)
  Mock_config.set_credentials 
    ~account_id:"test_account"
    ~credentials:{
      access_token = "old_token";
      refresh_token = None;
      expires_at = Some (past_expiry ());
      token_type = "Bearer";
    };
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Test"
    ()
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Auth_error Error_types.Token_expired) ->
          print_endline "✓ Token refresh fails without refresh token"
      | Error_types.Success _ ->
          failwith "Should have failed with expired token"
      | Error_types.Failure err ->
          failwith ("Wrong error type: " ^ Error_types.error_to_string err)
      | _ -> failwith "Unexpected outcome")

(** {1 Header Verification Tests} *)

let test_user_agent_header () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|{
    "json": {
      "errors": [],
      "data": {"id": "ua123", "name": "t3_ua123", "url": "..."}
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Test"
    ()
    (handle_outcome
      (fun _ ->
        let (_, _, headers, _) = List.hd !Mock_http.requests in
        let ua_header = List.assoc_opt "User-Agent" headers in
        assert (ua_header <> None);
        let ua = Option.get ua_header in
        assert (string_contains ua "ocaml");
        assert (string_contains ua "social-reddit");
        print_endline "✓ User-Agent header verification")
      (fun err -> failwith ("Request failed: " ^ err)))

let test_authorization_header () =
  Mock_config.reset ();
  Mock_config.set_env "REDDIT_CLIENT_ID" "test_client";
  Mock_config.set_env "REDDIT_CLIENT_SECRET" "test_secret";
  Mock_config.set_credentials 
    ~account_id:"test_account"
    ~credentials:{
      access_token = "my_access_token_123";
      refresh_token = Some "refresh_tok";
      expires_at = Some (future_expiry ());
      token_type = "Bearer";
    };
  
  let response_body = {|{
    "json": {
      "errors": [],
      "data": {"id": "auth123", "name": "t3_auth123", "url": "..."}
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Test"
    ()
    (handle_outcome
      (fun _ ->
        let (_, _, headers, _) = List.hd !Mock_http.requests in
        let auth_header = List.assoc_opt "Authorization" headers in
        assert (auth_header <> None);
        let auth = Option.get auth_header in
        assert (auth = "Bearer my_access_token_123");
        print_endline "✓ Authorization Bearer header verification")
      (fun err -> failwith ("Request failed: " ^ err)))

(** {1 Credentials Tests} *)

let test_missing_credentials () =
  Mock_config.reset ();
  (* Don't set REDDIT_CLIENT_ID or REDDIT_CLIENT_SECRET *)
  
  Mock_config.set_credentials 
    ~account_id:"test_account"
    ~credentials:{
      access_token = "old_token";
      refresh_token = Some "refresh_tok";
      expires_at = Some (past_expiry ());  (* expired, so refresh will be attempted *)
      token_type = "Bearer";
    };
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Test"
    ()
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Auth_error Error_types.Missing_credentials) ->
          print_endline "✓ Missing credentials error"
      | Error_types.Success _ ->
          failwith "Should have failed with missing credentials"
      | Error_types.Failure err ->
          failwith ("Wrong error type: " ^ Error_types.error_to_string err)
      | _ -> failwith "Unexpected outcome")

let test_account_not_found () =
  Mock_config.reset ();
  Mock_config.set_env "REDDIT_CLIENT_ID" "test_client";
  Mock_config.set_env "REDDIT_CLIENT_SECRET" "test_secret";
  (* Don't set any credentials for the account *)
  
  Reddit.submit_self_post
    ~account_id:"nonexistent_account"
    ~subreddit:"test"
    ~title:"Test"
    ()
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Auth_error _) ->
          print_endline "✓ Account not found error"
      | Error_types.Success _ ->
          failwith "Should have failed with account not found"
      | Error_types.Failure err ->
          failwith ("Wrong error type: " ^ Error_types.error_to_string err)
      | _ -> failwith "Unexpected outcome")

(** {1 Rate Limit Header Parsing Tests} *)

let test_parse_rate_limit_headers_all_present () =
  let headers = [
    ("x-ratelimit-used", "5");
    ("x-ratelimit-remaining", "95.0");
    ("x-ratelimit-reset", "300");
  ] in
  let info = Reddit.parse_rate_limit_headers headers in
  assert (info.used = Some 5.0);
  assert (info.remaining = Some 95.0);
  assert (info.reset = Some 300);
  print_endline "  ok: parse_rate_limit_headers with all headers present"

let test_parse_rate_limit_headers_missing () =
  let headers = [("content-type", "application/json")] in
  let info = Reddit.parse_rate_limit_headers headers in
  assert (info.used = None);
  assert (info.remaining = None);
  assert (info.reset = None);
  print_endline "  ok: parse_rate_limit_headers with no rate limit headers"

let test_parse_rate_limit_headers_case_insensitive () =
  let headers = [
    ("X-Ratelimit-Used", "10");
    ("X-Ratelimit-Remaining", "90");
    ("X-Ratelimit-Reset", "600");
  ] in
  let info = Reddit.parse_rate_limit_headers headers in
  assert (info.used = Some 10.0);
  assert (info.remaining = Some 90.0);
  assert (info.reset = Some 600);
  print_endline "  ok: parse_rate_limit_headers is case-insensitive"

let test_parse_rate_limit_headers_float_reset () =
  let headers = [
    ("x-ratelimit-used", "1");
    ("x-ratelimit-remaining", "99.5");
    ("x-ratelimit-reset", "123.7");
  ] in
  let info = Reddit.parse_rate_limit_headers headers in
  assert (info.used = Some 1.0);
  assert (info.remaining = Some 99.5);
  assert (info.reset = Some 123);
  print_endline "  ok: parse_rate_limit_headers handles float reset value"

(** {1 Subreddit Search Tests} *)

let test_search_subreddits () =
  Mock_config.setup_valid_credentials ();

  let response_body = {|{
    "kind": "Listing",
    "data": {
      "children": [
        {
          "kind": "t5",
          "data": {
            "id": "abc123",
            "display_name": "ocaml",
            "subscribers": 15000,
            "over18": false,
            "user_is_moderator": false,
            "link_flair_enabled": false,
            "submission_type": "any"
          }
        },
        {
          "kind": "t5",
          "data": {
            "id": "def456",
            "display_name": "ocaml_beginners",
            "subscribers": 3000,
            "over18": false,
            "user_is_moderator": true,
            "link_flair_enabled": true,
            "submission_type": "self"
          }
        }
      ]
    }
  }|} in

  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Reddit.search_subreddits
    ~account_id:"test_account"
    ~query:"ocaml"
    (handle_outcome
      (fun (subreddits : Social_reddit_v1.subreddit list) ->
        assert (List.length subreddits = 2);
        let first = List.hd subreddits in
        assert (first.name = "ocaml");
        assert (first.subscribers = 15000);
        assert (first.user_is_moderator = false);
        let second = List.nth subreddits 1 in
        assert (second.name = "ocaml_beginners");
        assert (second.user_is_moderator = true);

        (* Verify request URL *)
        let (method_, url, _, _) = List.hd !Mock_http.requests in
        assert (method_ = "GET");
        assert (string_contains url "/subreddits/search");
        assert (string_contains url "q=ocaml");
        print_endline "  ok: search_subreddits returns results")
      (fun err -> failwith ("Search subreddits failed: " ^ err)))

let test_search_subreddits_custom_limit () =
  Mock_config.setup_valid_credentials ();

  let response_body = {|{
    "kind": "Listing",
    "data": {
      "children": []
    }
  }|} in

  Mock_http.set_response { status = 200; body = response_body; headers = [] };

  Reddit.search_subreddits
    ~account_id:"test_account"
    ~query:"niche_topic"
    ~limit:5
    (handle_outcome
      (fun (subreddits : Social_reddit_v1.subreddit list) ->
        assert (List.length subreddits = 0);
        let (_, url, _, _) = List.hd !Mock_http.requests in
        assert (string_contains url "limit=5");
        print_endline "  ok: search_subreddits passes custom limit")
      (fun err -> failwith ("Search subreddits with limit failed: " ^ err)))

(** {1 Gallery Post Tests} *)

let test_submit_gallery_post () =
  Mock_config.setup_valid_credentials ();

  let img1_download = { status = 200; body = "img1_bytes"; headers = [("content-type", "image/jpeg")] } in
  let img1_lease = {
    status = 200;
    body = {|{
      "args": {
        "action": "https://reddit-upload.s3.amazonaws.com",
        "fields": [{"name": "key", "value": "uploads/img1.jpg"}],
        "asset": {"asset_id": "asset_img1"}
      }
    }|};
    headers = [];
  } in
  let img1_s3 = { status = 204; body = ""; headers = [] } in
  let img2_download = { status = 200; body = "img2_bytes"; headers = [("content-type", "image/png")] } in
  let img2_lease = {
    status = 200;
    body = {|{
      "args": {
        "action": "https://reddit-upload.s3.amazonaws.com",
        "fields": [{"name": "key", "value": "uploads/img2.png"}],
        "asset": {"asset_id": "asset_img2"}
      }
    }|};
    headers = [];
  } in
  let img2_s3 = { status = 204; body = ""; headers = [] } in
  let submit_response = {
    status = 200;
    body = {|{
      "json": {
        "errors": [],
        "data": {"id": "gallery123", "name": "t3_gallery123", "url": "https://reddit.com/r/test/gallery123"}
      }
    }|};
    headers = [];
  } in

  Mock_http.set_responses [
    img1_download; img1_lease; img1_s3;
    img2_download; img2_lease; img2_s3;
    submit_response;
  ];

  let items : Social_reddit_v1.gallery_item list = [
    { media_url = "https://cdn.example.com/img1.jpg"; caption = Some "First image"; outbound_url = None };
    { media_url = "https://cdn.example.com/img2.png"; caption = None; outbound_url = Some "https://example.com" };
  ] in

  Reddit.submit_gallery_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Gallery Post"
    ~items
    ()
    (handle_outcome
      (fun post_id ->
        assert (post_id = "gallery123");
        let chronological = List.rev !Mock_http.requests in
        assert (List.length chronological = 7);

        (* Verify the final submit request *)
        let (method_, url, _, body) = List.nth chronological 6 in
        assert (method_ = "POST");
        assert (string_contains url "/api/submit_gallery_post");
        assert (string_contains body "asset_img1");
        assert (string_contains body "asset_img2");
        assert (string_contains body "First image");
        assert (string_contains body "Gallery Post");
        (* Gallery endpoint should NOT include "kind" field *)
        assert (not (string_contains body "\"kind\""));
        (* Should include validate_on_submit for proper error reporting *)
        assert (string_contains body "validate_on_submit");
        (* Each item should have caption and outbound_url fields *)
        assert (string_contains body "\"caption\"");
        assert (string_contains body "\"outbound_url\"");
        print_endline "  ok: submit_gallery_post uploads images and submits")
      (fun err -> failwith ("Gallery post failed: " ^ err)))

let test_submit_gallery_post_too_few_items () =
  Mock_config.setup_valid_credentials ();

  let items : Social_reddit_v1.gallery_item list = [
    { media_url = "https://cdn.example.com/img1.jpg"; caption = None; outbound_url = None };
  ] in

  Reddit.submit_gallery_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Too Few Images"
    ~items
    ()
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Api_error { message; _ }) when string_contains message "at least 2" ->
          print_endline "  ok: submit_gallery_post rejects fewer than 2 items"
      | Error_types.Success _ ->
          failwith "Gallery with 1 image should fail validation"
      | Error_types.Failure err ->
          failwith ("Unexpected error type for single-image gallery: " ^ Error_types.error_to_string err)
      | _ -> failwith "Unexpected outcome for single-image gallery")

(** {1 Multi-Subreddit Posting Tests} *)

let test_post_to_subreddits () =
  Mock_config.setup_valid_credentials ();

  let response1 = {
    status = 200;
    body = {|{"json":{"errors":[],"data":{"id":"post1","name":"t3_post1","url":"..."}}}|};
    headers = [];
  } in
  let response2 = {
    status = 200;
    body = {|{"json":{"errors":[],"data":{"id":"post2","name":"t3_post2","url":"..."}}}|};
    headers = [];
  } in

  Mock_http.set_responses [response1; response2];

  Reddit.post_to_subreddits
    ~account_id:"test_account"
    ~subreddits:["sub1"; "sub2"]
    ~title:"Multi Post"
    ~body:"Body text"
    ()
    (handle_outcome
      (fun (results : Social_reddit_v1.multi_post_result list) ->
        assert (List.length results = 2);
        let r1 = List.hd results in
        assert (r1.subreddit = "sub1");
        assert (r1.post_id = Some "post1");
        assert (r1.error = None);
        let r2 = List.nth results 1 in
        assert (r2.subreddit = "sub2");
        assert (r2.post_id = Some "post2");
        assert (r2.error = None);
        print_endline "  ok: post_to_subreddits posts to multiple subreddits")
      (fun err -> failwith ("Multi-subreddit post failed: " ^ err)))

let test_post_to_subreddits_partial_failure () =
  Mock_config.setup_valid_credentials ();

  let response1 = {
    status = 200;
    body = {|{"json":{"errors":[],"data":{"id":"post1","name":"t3_post1","url":"..."}}}|};
    headers = [];
  } in
  let response2 = {
    status = 403;
    body = {|{"message": "Forbidden", "error": 403}|};
    headers = [];
  } in
  let response3 = {
    status = 200;
    body = {|{"json":{"errors":[],"data":{"id":"post3","name":"t3_post3","url":"..."}}}|};
    headers = [];
  } in

  Mock_http.set_responses [response1; response2; response3];

  Reddit.post_to_subreddits
    ~account_id:"test_account"
    ~subreddits:["sub1"; "sub2"; "sub3"]
    ~title:"Partial Fail"
    ~body:"Body"
    ()
    (handle_outcome
      (fun (results : Social_reddit_v1.multi_post_result list) ->
        assert (List.length results = 3);
        let r1 = List.hd results in
        assert (r1.subreddit = "sub1");
        assert (r1.post_id = Some "post1");
        assert (r1.error = None);
        let r2 = List.nth results 1 in
        assert (r2.subreddit = "sub2");
        assert (r2.post_id = None);
        assert (r2.error <> None);
        let r3 = List.nth results 2 in
        assert (r3.subreddit = "sub3");
        assert (r3.post_id = Some "post3");
        assert (r3.error = None);
        print_endline "  ok: post_to_subreddits continues after partial failure")
      (fun err -> failwith ("Multi-subreddit partial failure test failed: " ^ err)))

let test_post_to_subreddits_empty_list () =
  Mock_config.setup_valid_credentials ();

  Reddit.post_to_subreddits
    ~account_id:"test_account"
    ~subreddits:[]
    ~title:"Empty List"
    ()
    (handle_outcome
      (fun (results : Social_reddit_v1.multi_post_result list) ->
        assert (List.length results = 0);
        print_endline "  ok: post_to_subreddits with empty list returns empty results")
      (fun err -> failwith ("Empty subreddit list test failed: " ^ err)))

(** {1 Run All Tests} *)

let () =
  print_endline "\n=== Reddit Provider Tests ===\n";
  
  print_endline "--- OAuth Tests ---";
  test_oauth_url_generation ();
  test_oauth_url_custom_scopes ();
  test_token_exchange ();
  test_token_exchange_basic_auth ();
  test_token_exchange_rejects_missing_scopes ();
  
  print_endline "\n--- Validation Tests ---";
  test_validate_title_empty ();
  test_validate_title_too_long ();
  test_validate_title_valid ();
  test_validate_body_too_long ();
  test_validate_body_valid ();
  test_validate_media_video_too_large ();
  test_validate_media_video_too_long ();
  
  print_endline "\n--- Post Submission Tests ---";
  test_submit_self_post ();
  test_submit_link_post ();
  test_submit_post_with_flair ();
  test_delete_post ();
  
  print_endline "\n--- Subreddit Tests ---";
  test_get_moderated_subreddits ();
  test_get_subreddit_flairs ();
  test_check_flair_required ();
  
  print_endline "\n--- User Tests ---";
  test_get_user_info ();
  
  print_endline "\n--- Error Handling Tests ---";
  test_rate_limit_error ();
  test_rate_limit_seconds ();
  test_rate_limit_singular_minute ();
  test_auth_error ();
  test_subreddit_not_allowed ();
  test_no_selfs_error ();
  test_no_links_error ();
  test_bad_subreddit_name ();
  test_duplicate_post ();
  
  print_endline "\n--- Crosspost Tests ---";
  test_submit_crosspost ();
  
  print_endline "\n--- post_single Tests ---";
  test_post_single_self ();
  test_post_single_link ();
  test_post_single_video_uses_native_video_flow ();
  test_post_single_unknown_extension_video_uses_native_flow ();
  test_post_single_video_content_type_with_charset_is_supported ();
  test_post_single_unknown_extension_image_falls_back_to_image_flow ();
  test_post_single_unknown_extension_non_image_fails_without_fallback ();
  test_post_single_video_with_thumbnail_uses_poster_flow ();
  test_post_single_video_with_unknown_extension_thumbnail_uses_poster_flow ();
  test_post_single_video_with_invalid_thumbnail_still_posts ();
  test_post_single_media_takes_precedence_over_url ();
  
  print_endline "\n--- Token Refresh Tests ---";
  test_token_refresh_expired ();
  test_token_refresh_no_refresh_token ();
  
  print_endline "\n--- Header Verification Tests ---";
  test_user_agent_header ();
  test_authorization_header ();
  
  print_endline "\n--- Credentials Tests ---";
  test_missing_credentials ();
  test_account_not_found ();

  print_endline "\n--- Rate Limit Header Parsing Tests ---";
  test_parse_rate_limit_headers_all_present ();
  test_parse_rate_limit_headers_missing ();
  test_parse_rate_limit_headers_case_insensitive ();
  test_parse_rate_limit_headers_float_reset ();

  print_endline "\n--- Subreddit Search Tests ---";
  test_search_subreddits ();
  test_search_subreddits_custom_limit ();

  print_endline "\n--- Gallery Post Tests ---";
  test_submit_gallery_post ();
  test_submit_gallery_post_too_few_items ();

  print_endline "\n--- Multi-Subreddit Posting Tests ---";
  test_post_to_subreddits ();
  test_post_to_subreddits_partial_failure ();
  test_post_to_subreddits_empty_list ();

  print_endline "\n=== All tests passed! ===";
  print_endline "\nTest Coverage Summary:";
  print_endline "  - OAuth 2.0 with Basic Auth (5 tests)";
  print_endline "  - Content validation (7 tests)";
  print_endline "  - Post submission (4 tests)";
  print_endline "  - Subreddit operations (3 tests)";
  print_endline "  - User operations (1 test)";
  print_endline "  - Error handling (9 tests)";
  print_endline "  - Crosspost (1 test)";
  print_endline "  - post_single interface (11 tests)";
  print_endline "  - Token refresh (2 tests)";
  print_endline "  - Header verification (2 tests)";
  print_endline "  - Credentials handling (2 tests)";
  print_endline "  - Rate limit header parsing (4 tests)";
  print_endline "  - Subreddit search (2 tests)";
  print_endline "  - Gallery posts (2 tests)";
  print_endline "  - Multi-subreddit posting (3 tests)";
  print_endline "";
  print_endline "Total: 58 test functions\n"
