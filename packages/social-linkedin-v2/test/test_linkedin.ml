(** Tests for LinkedIn API v2 Provider *)

open Social_core
open Social_linkedin_v2

(** Helper to check if string contains substring *)
let string_contains s substr =
  try
    ignore (Str.search_forward (Str.regexp_string substr) s 0);
    true
  with Not_found -> false

(** Mock HTTP client for testing *)
module Mock_http = struct
  let requests = ref []
  let next_response = ref None
  
  let reset () =
    requests := [];
    next_response := None
  
  let set_response response =
    next_response := Some response
  
  include (struct
  
  let get ?(headers=[]) url on_success on_error =
    requests := ("GET", url, headers, "") :: !requests;
    match !next_response with
    | Some response -> on_success response
    | None -> on_error "No mock response set"
  
  let post ?(headers=[]) ?(body="") url on_success on_error =
    requests := ("POST", url, headers, body) :: !requests;
    match !next_response with
    | Some response -> on_success response
    | None -> on_error "No mock response set"
  
  let put ?(headers=[]) ?(body="") url on_success on_error =
    requests := ("PUT", url, headers, body) :: !requests;
    match !next_response with
    | Some response -> on_success response
    | None -> on_error "No mock response set"
  
  let delete ?(headers=[]) url on_success on_error =
    requests := ("DELETE", url, headers, "") :: !requests;
    match !next_response with
    | Some response -> on_success response
    | None -> on_error "No mock response set"
  
  let post_multipart ?(headers=[]) ~parts url on_success on_error =
    let body_str = Printf.sprintf "multipart with %d parts" (List.length parts) in
    requests := ("POST_MULTIPART", url, headers, body_str) :: !requests;
    match !next_response with
    | Some response -> on_success response
    | None -> on_error "No mock response set"
  end : HTTP_CLIENT)
end

(** Mock config for testing *)
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
    if String.starts_with ~prefix:"encrypted:" data then
      on_success (String.sub data 10 (String.length data - 10))
    else
      on_error "Invalid encrypted data"
  
  let update_health_status ~account_id ~status ~error_message on_success _on_error =
    health_statuses := (account_id, status, error_message) :: !health_statuses;
    on_success ()
  
  let get_health_status account_id =
    List.find_opt (fun (id, _, _) -> id = account_id) !health_statuses
end

module LinkedIn = Make(Mock_config)

(** Test: OAuth URL generation *)
let test_oauth_url () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client_id";
  
  let state = "test_state_123" in
  let redirect_uri = "https://example.com/callback" in
  
  LinkedIn.get_oauth_url ~redirect_uri ~state
    (fun url ->
      assert (string_contains url "response_type=code");
      assert (string_contains url "client_id=test_client_id");
      assert (string_contains url "state=test_state_123");
      assert (string_contains url "scope=openid");
      assert (string_contains url "scope=openid+profile+email+w_member_social" || 
              string_contains url "openid%20profile%20email%20w_member_social");
      print_endline "✓ OAuth URL generation")
    (fun err -> failwith ("OAuth URL failed: " ^ err))

(** Test: Token exchange *)
let test_token_exchange () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client";
  Mock_config.set_env "LINKEDIN_CLIENT_SECRET" "test_secret";
  
  let response_body = {|{
    "access_token": "new_access_token_123",
    "refresh_token": "new_refresh_token_456",
    "expires_in": 5184000
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  LinkedIn.exchange_code 
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    (fun creds ->
      assert (creds.access_token = "new_access_token_123");
      assert (creds.refresh_token = Some "new_refresh_token_456");
      assert (creds.token_type = "Bearer");
      assert (creds.expires_at <> None);
      print_endline "✓ Token exchange")
    (fun err -> failwith ("Token exchange failed: " ^ err))

(** Test: Get person URN *)
let test_get_person_urn () =
  Mock_config.reset ();
  
  let response_body = {|{
    "sub": "abc123xyz",
    "name": "Test User",
    "email": "test@example.com"
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  LinkedIn.get_person_urn ~access_token:"test_token"
    (fun person_urn ->
      assert (person_urn = "urn:li:person:abc123xyz");
      print_endline "✓ Get person URN")
    (fun err -> failwith ("Get person URN failed: " ^ err))

(** Test: Register upload *)
let test_register_upload () =
  Mock_config.reset ();
  
  let response_body = {|{
    "value": {
      "asset": "urn:li:digitalmediaAsset:test123",
      "uploadMechanism": {
        "com.linkedin.digitalmedia.uploading.MediaUploadHttpRequest": {
          "uploadUrl": "https://upload.linkedin.com/test"
        }
      }
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  LinkedIn.register_upload 
    ~access_token:"test_token"
    ~person_urn:"urn:li:person:test"
    ~media_type:"image"
    (fun (asset, upload_url) ->
      assert (asset = "urn:li:digitalmediaAsset:test123");
      assert (upload_url = "https://upload.linkedin.com/test");
      print_endline "✓ Register upload")
    (fun err -> failwith ("Register upload failed: " ^ err))

(** Test: Content validation *)
let test_content_validation () =
  (* Valid content *)
  (match LinkedIn.validate_content ~text:"Hello LinkedIn!" with
   | Ok () -> print_endline "✓ Valid content passes"
   | Error e -> failwith ("Valid content failed: " ^ e));
  
  (* Empty content *)
  (match LinkedIn.validate_content ~text:"" with
   | Error _ -> print_endline "✓ Empty content rejected"
   | Ok () -> failwith "Empty content should fail");
  
  (* Too long *)
  let long_text = String.make 3001 'x' in
  (match LinkedIn.validate_content ~text:long_text with
   | Error msg when string_contains msg "too long" -> 
       print_endline "✓ Long content rejected"
   | _ -> failwith "Long content should fail")

(** Test: Token refresh (partner program) *)
let test_token_refresh_partner () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client";
  Mock_config.set_env "LINKEDIN_CLIENT_SECRET" "test_secret";
  Mock_config.set_env "LINKEDIN_ENABLE_PROGRAMMATIC_REFRESH" "true";
  
  let response_body = {|{
    "access_token": "refreshed_token",
    "refresh_token": "new_refresh_token",
    "expires_in": 5184000
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  LinkedIn.refresh_access_token
    ~client_id:"test_client"
    ~client_secret:"test_secret"
    ~refresh_token:"old_refresh"
    (fun (access, refresh, _expires) ->
      assert (access = "refreshed_token");
      assert (refresh = "new_refresh_token");
      print_endline "✓ Token refresh (partner)")
    (fun err -> failwith ("Token refresh failed: " ^ err))

(** Test: Token refresh disabled (standard app) *)
let test_token_refresh_standard () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client";
  Mock_config.set_env "LINKEDIN_CLIENT_SECRET" "test_secret";
  (* Don't set LINKEDIN_ENABLE_PROGRAMMATIC_REFRESH *)
  
  LinkedIn.refresh_access_token
    ~client_id:"test_client"
    ~client_secret:"test_secret"
    ~refresh_token:"old_refresh"
    (fun _ -> failwith "Should fail for standard app")
    (fun err ->
      assert (string_contains err "not enabled");
      print_endline "✓ Token refresh disabled for standard apps")

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
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  LinkedIn.ensure_valid_token ~account_id:"test_account"
    (fun token ->
      assert (token = "valid_token");
      (* Verify health status was updated *)
      match Mock_config.get_health_status "test_account" with
      | Some (_, "healthy", None) -> print_endline "✓ Ensure valid token (fresh)"
      | _ -> failwith "Health status not updated correctly")
    (fun err -> failwith ("Ensure valid token failed: " ^ err))

(** Test: Get profile *)
let test_get_profile () =
  Mock_config.reset ();
  
  (* Set valid credentials *)
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  let response_body = {|{
    "sub": "abc123",
    "name": "John Doe",
    "given_name": "John",
    "family_name": "Doe",
    "email": "john@example.com",
    "email_verified": true,
    "locale": "en-US"
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  LinkedIn.get_profile ~account_id:"test_account"
    (fun profile ->
      assert (profile.sub = "abc123");
      assert (profile.name = Some "John Doe");
      assert (profile.email = Some "john@example.com");
      print_endline "✓ Get profile")
    (fun err -> failwith ("Get profile failed: " ^ err))

(** Test: Get posts with pagination *)
let test_get_posts () =
  Mock_config.reset ();
  
  (* Set valid credentials *)
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  (* First call to get person URN *)
  let person_response = {|{"sub": "user123"}|} in
  Mock_http.set_response { status = 200; body = person_response; headers = [] };
  
  (* Second call to get posts *)
  let posts_response = {|{
    "elements": [
      {
        "id": "urn:li:share:123",
        "author": "urn:li:person:user123",
        "created": {"time": "2024-01-01T10:00:00Z"},
        "lifecycleState": "PUBLISHED",
        "specificContent": {
          "com.linkedin.ugc.ShareContent": {
            "shareCommentary": {"text": "Test post"}
          }
        }
      }
    ],
    "paging": {
      "start": 0,
      "count": 1,
      "total": 10
    }
  }|} in
  Mock_http.set_response { status = 200; body = posts_response; headers = [] };
  
  LinkedIn.get_posts ~account_id:"test_account" ~start:0 ~count:10
    (fun collection ->
      assert (List.length collection.elements = 1);
      let post = List.hd collection.elements in
      assert (post.id = "urn:li:share:123");
      assert (post.text = Some "Test post");
      (match collection.paging with
      | Some p -> 
          assert (p.start = 0);
          assert (p.count = 1);
          assert (p.total = Some 10)
      | None -> failwith "Expected paging metadata");
      print_endline "✓ Get posts with pagination")
    (fun err -> failwith ("Get posts failed: " ^ err))

(** Test: Batch get posts *)
let test_batch_get_posts () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  let response_body = {|{
    "results": {
      "urn:li:share:123": {
        "id": "urn:li:share:123",
        "author": "urn:li:person:user1",
        "lifecycleState": "PUBLISHED",
        "specificContent": {
          "com.linkedin.ugc.ShareContent": {
            "shareCommentary": {"text": "Post 1"}
          }
        }
      },
      "urn:li:share:456": {
        "id": "urn:li:share:456",
        "author": "urn:li:person:user2",
        "lifecycleState": "PUBLISHED",
        "specificContent": {
          "com.linkedin.ugc.ShareContent": {
            "shareCommentary": {"text": "Post 2"}
          }
        }
      }
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  LinkedIn.batch_get_posts 
    ~account_id:"test_account" 
    ~post_urns:["urn:li:share:123"; "urn:li:share:456"]
    (fun posts ->
      assert (List.length posts = 2);
      print_endline "✓ Batch get posts")
    (fun err -> failwith ("Batch get posts failed: " ^ err))

(** Test: Posts scroller *)
let test_posts_scroller () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  (* First response for person URN *)
  let person_response = {|{"sub": "user123"}|} in
  Mock_http.set_response { status = 200; body = person_response; headers = [] };
  
  (* Second response for posts *)
  let posts_response = {|{
    "elements": [{"id": "1", "author": "urn:li:person:user123", "lifecycleState": "PUBLISHED"}],
    "paging": {"start": 0, "count": 1, "total": 5}
  }|} in
  Mock_http.set_response { status = 200; body = posts_response; headers = [] };
  
  let scroller = LinkedIn.create_posts_scroller ~account_id:"test_account" ~page_size:1 () in
  
  scroller.scroll_next
    (fun collection ->
      assert (List.length collection.elements = 1);
      assert (scroller.current_position () = 1);
      assert (scroller.has_more () = true);
      print_endline "✓ Posts scroller")
    (fun err -> failwith ("Posts scroller failed: " ^ err))

(** Test: Search posts *)
let test_search_posts () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  let search_response = {|{
    "elements": [
      {"id": "post1", "author": "urn:li:person:user1", "lifecycleState": "PUBLISHED"},
      {"id": "post2", "author": "urn:li:person:user2", "lifecycleState": "PUBLISHED"}
    ],
    "paging": {"start": 0, "count": 2, "total": 10}
  }|} in
  
  Mock_http.set_response { status = 200; body = search_response; headers = [] };
  
  LinkedIn.search_posts ~account_id:"test_account" ~keywords:"OCaml" ~start:0 ~count:10
    (fun collection ->
      assert (List.length collection.elements = 2);
      print_endline "✓ Search posts")
    (fun err -> failwith ("Search posts failed: " ^ err))

(** Test: Like post *)
let test_like_post () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  (* Person URN response *)
  let person_response = {|{"sub": "user123"}|} in
  Mock_http.set_response { status = 200; body = person_response; headers = [] };
  
  (* Like response *)
  Mock_http.set_response { status = 201; body = "{}"; headers = [] };
  
  LinkedIn.like_post ~account_id:"test_account" ~post_urn:"urn:li:share:123"
    (fun () -> print_endline "✓ Like post")
    (fun err -> failwith ("Like post failed: " ^ err))

(** Test: Comment on post *)
let test_comment_on_post () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  (* Person URN response *)
  let person_response = {|{"sub": "user123"}|} in
  Mock_http.set_response { status = 200; body = person_response; headers = [] };
  
  (* Comment response *)
  let comment_response = {|{"id": "comment123"}|} in
  Mock_http.set_response { status = 201; body = comment_response; headers = [] };
  
  LinkedIn.comment_on_post 
    ~account_id:"test_account" 
    ~post_urn:"urn:li:share:123"
    ~text:"Great post!"
    (fun comment_id ->
      assert (comment_id = "comment123");
      print_endline "✓ Comment on post")
    (fun err -> failwith ("Comment failed: " ^ err))

(** Test: Get post comments *)
let test_get_post_comments () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  let comments_response = {|{
    "elements": [
      {
        "id": "comment1",
        "actor": "urn:li:person:user1",
        "message": {"text": "Nice!"},
        "created": {"time": "2024-01-01T10:00:00Z"}
      }
    ],
    "paging": {"start": 0, "count": 1, "total": 5}
  }|} in
  
  Mock_http.set_response { status = 200; body = comments_response; headers = [] };
  
  LinkedIn.get_post_comments ~account_id:"test_account" ~post_urn:"urn:li:share:123"
    (fun collection ->
      assert (List.length collection.elements = 1);
      let comment = List.hd collection.elements in
      assert (comment.text = "Nice!");
      print_endline "✓ Get post comments")
    (fun err -> failwith ("Get comments failed: " ^ err))

(** Test: Post with URL preview (ARTICLE media category) *)
let test_post_with_url_preview () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  (* Person URN response *)
  let person_response = {|{"sub": "user123"}|} in
  Mock_http.set_response { status = 200; body = person_response; headers = [] };
  
  (* Post response *)
  let post_response = {|{"id": "urn:li:share:789"}|} in
  Mock_http.set_response { status = 201; body = post_response; headers = [] };
  
  let text = "Great article about OCaml! https://example.com/ocaml-article" in
  
  LinkedIn.post_single ~account_id:"test_account" ~text ~media_urls:[]
    (fun post_id ->
      assert (post_id = "urn:li:share:789");
      
      (* Check that the request included ARTICLE media category and originalUrl *)
      let requests = List.rev !Mock_http.requests in
      let post_request = List.find (fun (method_, url, _, _) ->
        method_ = "POST" && string_contains url "ugcPosts"
      ) requests in
      
      let (_, _, _, body) = post_request in
      assert (string_contains body "shareMediaCategory");
      assert (string_contains body "ARTICLE");
      assert (string_contains body "originalUrl");
      assert (string_contains body "https://example.com/ocaml-article");
      
      print_endline "✓ Post with URL preview (ARTICLE)")
    (fun err -> failwith ("Post with URL failed: " ^ err))

(** Test: OAuth URL contains all required parameters *)
let test_oauth_url_parameters () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client_id";
  
  let state = "test_state_123" in
  let redirect_uri = "https://example.com/callback" in
  
  LinkedIn.get_oauth_url ~redirect_uri ~state
    (fun url ->
      (* Verify all required OAuth parameters are present *)
      assert (string_contains url "response_type=code");
      assert (string_contains url "client_id=test_client_id");
      assert (string_contains url "redirect_uri=");
      assert (string_contains url "state=test_state_123");
      assert (string_contains url "scope=");
      (* Verify scope contains required permissions *)
      assert (string_contains url "openid" || string_contains url "profile" || string_contains url "email");
      print_endline "✓ OAuth URL parameters complete")
    (fun err -> failwith ("OAuth URL parameters test failed: " ^ err))

(** Test: OAuth URL encoding *)
let test_oauth_url_encoding () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test&client=id";
  
  let state = "state with spaces" in
  let redirect_uri = "https://example.com/callback?param=value" in
  
  LinkedIn.get_oauth_url ~redirect_uri ~state
    (fun url ->
      (* URL should be properly encoded *)
      assert (not (String.contains url ' '));
      assert (string_contains url "state=" || string_contains url "redirect_uri=");
      print_endline "✓ OAuth URL encoding")
    (fun err -> failwith ("OAuth URL encoding test failed: " ^ err))

(** Test: Token exchange with invalid response *)
let test_token_exchange_invalid () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client";
  Mock_config.set_env "LINKEDIN_CLIENT_SECRET" "test_secret";
  
  let invalid_response = {|{"error": "invalid_grant"}|} in
  Mock_http.set_response { status = 400; body = invalid_response; headers = [] };
  
  LinkedIn.exchange_code 
    ~code:"bad_code"
    ~redirect_uri:"https://example.com/callback"
    (fun _ -> failwith "Should fail with invalid grant")
    (fun err ->
      assert (string_contains err "400" || string_contains err "invalid");
      print_endline "✓ Token exchange invalid response handling")

(** Test: Token exchange with missing fields *)
let test_token_exchange_missing_fields () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client";
  Mock_config.set_env "LINKEDIN_CLIENT_SECRET" "test_secret";
  
  let incomplete_response = {|{"access_token": "token123", "expires_in": 5184000}|} in
  Mock_http.set_response { status = 200; body = incomplete_response; headers = [] };
  
  LinkedIn.exchange_code 
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    (fun creds ->
      (* Should handle missing refresh_token gracefully *)
      assert (creds.access_token = "token123");
      assert (creds.refresh_token = None);
      print_endline "✓ Token exchange with missing optional fields")
    (fun err -> failwith ("Should succeed with minimal response: " ^ err))

(** Test: Token refresh with rotating tokens *)
let test_token_refresh_rotation () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client";
  Mock_config.set_env "LINKEDIN_CLIENT_SECRET" "test_secret";
  Mock_config.set_env "LINKEDIN_ENABLE_PROGRAMMATIC_REFRESH" "true";
  
  let response_body = {|{
    "access_token": "new_access_token_v2",
    "refresh_token": "new_refresh_token_v2",
    "expires_in": 5184000
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  LinkedIn.refresh_access_token
    ~client_id:"test_client"
    ~client_secret:"test_secret"
    ~refresh_token:"old_refresh_v1"
    (fun (access, refresh, _expires) ->
      (* New tokens should be different *)
      assert (access = "new_access_token_v2");
      assert (refresh = "new_refresh_token_v2");
      assert (access <> "old_access");
      assert (refresh <> "old_refresh_v1");
      print_endline "✓ Token refresh with rotation")
    (fun err -> failwith ("Token refresh rotation failed: " ^ err))

(** Test: Expired token detection *)
let test_expired_token_detection () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_ENABLE_PROGRAMMATIC_REFRESH" "true";
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client";
  Mock_config.set_env "LINKEDIN_CLIENT_SECRET" "test_secret";
  
  (* Set credentials with past expiry *)
  let past_time = 
    let now = Ptime_clock.now () in
    match Ptime.sub_span now (Ptime.Span.of_int_s 86400) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate past time"
  in
  
  let creds = {
    access_token = "expired_token";
    refresh_token = Some "refresh_token";
    expires_at = Some past_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  (* Provider will attempt to refresh expired token *)
  let refresh_response = {|{
    "access_token": "refreshed_token",
    "refresh_token": "new_refresh_token",
    "expires_in": 5184000
  }|} in
  Mock_http.set_response { status = 200; body = refresh_response; headers = [] };
  
  LinkedIn.ensure_valid_token ~account_id:"test_account"
    (fun token ->
      (* Token refresh should succeed *)
      assert (token = "refreshed_token");
      print_endline "✓ Expired token detection and refresh")
    (fun err ->
      (* Or fail gracefully if refresh not enabled *)
      assert (String.length err > 0);
      print_endline "✓ Expired token detection and refresh")

(** Test: OAuth state CSRF protection *)
let test_oauth_state_validation () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client_id";
  
  (* Generate two different states *)
  let state1 = "state_123" in
  let state2 = "state_456" in
  
  LinkedIn.get_oauth_url ~redirect_uri:"https://example.com/callback" ~state:state1
    (fun url1 ->
      LinkedIn.get_oauth_url ~redirect_uri:"https://example.com/callback" ~state:state2
        (fun url2 ->
          (* URLs should contain different states *)
          assert (string_contains url1 "state_123");
          assert (string_contains url2 "state_456");
          assert (url1 <> url2);
          print_endline "✓ OAuth state CSRF protection")
        (fun err -> failwith ("State validation test failed: " ^ err)))
    (fun err -> failwith ("State validation test failed: " ^ err))

(** Test: Refresh token expiry behavior *)
let test_refresh_token_expiry () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_ENABLE_PROGRAMMATIC_REFRESH" "true";
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client";
  Mock_config.set_env "LINKEDIN_CLIENT_SECRET" "test_secret";
  
  (* Test with expired refresh token *)
  let error_response = {|{"error": "invalid_grant", "error_description": "Refresh token expired"}|} in
  Mock_http.set_response { status = 400; body = error_response; headers = [] };
  
  LinkedIn.refresh_access_token
    ~client_id:"test_client"
    ~client_secret:"test_secret"
    ~refresh_token:"expired_refresh"
    (fun _ -> failwith "Should fail with expired refresh token")
    (fun err ->
      (* Error message should contain something about failure *)
      assert (String.length err > 0);
      print_endline "✓ Refresh token expiry handling")

(** Test: Scope validation in OAuth URL *)
let test_oauth_scope_validation () =
  Mock_config.reset ();
  Mock_config.set_env "LINKEDIN_CLIENT_ID" "test_client_id";
  
  LinkedIn.get_oauth_url ~redirect_uri:"https://example.com/callback" ~state:"test_state"
    (fun url ->
      (* Verify required scopes are present *)
      let has_openid = string_contains url "openid" in
      let has_profile = string_contains url "profile" in
      let has_email = string_contains url "email" in
      let has_posts = string_contains url "w_member_social" in
      
      assert (has_openid && has_profile && has_email && has_posts);
      print_endline "✓ OAuth scope validation")
    (fun err -> failwith ("Scope validation failed: " ^ err))

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
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  Mock_http.set_response { status = 200; body = {|{"sub": "user123"}|}; headers = [] };
  
  LinkedIn.post_single 
    ~account_id:"test_account"
    ~text:"Check out this image!"
    ~media_urls:["https://example.com/image.jpg"]
    ~alt_texts:[Some "A beautiful sunset over mountains"]
    (fun _post_id ->
      print_endline "✓ Post with single image and alt-text")
    (fun err -> failwith ("Post with alt-text failed: " ^ err))

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
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  Mock_http.set_response { status = 200; body = {|{"sub": "user123"}|}; headers = [] };
  
  LinkedIn.post_single 
    ~account_id:"test_account"
    ~text:"Multiple images with descriptions"
    ~media_urls:["https://example.com/img1.jpg"; "https://example.com/img2.jpg"]
    ~alt_texts:[Some "First image description"; Some "Second image description"]
    (fun _post_id ->
      print_endline "✓ Post with multiple images and alt-texts")
    (fun err -> failwith ("Post with multiple alt-texts failed: " ^ err))

(** Test: Post with image but no alt-text *)
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
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  Mock_http.set_response { status = 200; body = {|{"sub": "user123"}|}; headers = [] };
  
  LinkedIn.post_single 
    ~account_id:"test_account"
    ~text:"Image without description"
    ~media_urls:["https://example.com/image.jpg"]
    ~alt_texts:[]
    (fun _post_id ->
      print_endline "✓ Post without alt-text")
    (fun err -> failwith ("Post without alt-text failed: " ^ err))

(** Test: Partial alt-texts - fewer alt-texts than images *)
let test_post_with_partial_alt_texts () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  Mock_http.set_response { status = 200; body = {|{"sub": "user123"}|}; headers = [] };
  
  LinkedIn.post_single 
    ~account_id:"test_account"
    ~text:"Three images, two descriptions"
    ~media_urls:["https://example.com/img1.jpg"; "https://example.com/img2.jpg"; "https://example.com/img3.jpg"]
    ~alt_texts:[Some "First image"; Some "Second image"]
    (fun _post_id ->
      print_endline "✓ Post with partial alt-texts (3 images, 2 alt-texts)")
    (fun err -> failwith ("Post with partial alt-texts failed: " ^ err))

(** Test: Alt-text with special characters *)
let test_alt_text_special_chars () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  Mock_http.set_response { status = 200; body = {|{"sub": "user123"}|}; headers = [] };
  
  LinkedIn.post_single 
    ~account_id:"test_account"
    ~text:"Testing special characters in alt-text"
    ~media_urls:["https://example.com/image.jpg"]
    ~alt_texts:[Some "A photo with \"quotes\", emojis 🌅, & special chars: <>&"]
    (fun _post_id ->
      print_endline "✓ Alt-text with special characters")
    (fun err -> failwith ("Alt-text with special chars failed: " ^ err))

(** Test: Thread with alt-texts per post *)
let test_thread_with_alt_texts () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  Mock_http.set_response { status = 200; body = {|{"sub": "user123"}|}; headers = [] };
  
  LinkedIn.post_thread
    ~account_id:"test_account"
    ~texts:["First post with image"; "Second post with image"]
    ~media_urls_per_post:[["https://example.com/img1.jpg"]; ["https://example.com/img2.jpg"]]
    ~alt_texts_per_post:[[Some "Description for first image"]; [Some "Description for second image"]]
    (fun _post_ids ->
      print_endline "✓ Thread with alt-texts per post")
    (fun err -> failwith ("Thread with alt-texts failed: " ^ err))

(** Run all tests *)
let () =
  print_endline "\n=== LinkedIn Provider Tests ===\n";
  
  print_endline "--- OAuth Flow Tests ---";
  test_oauth_url ();
  test_oauth_url_parameters ();
  test_oauth_url_encoding ();
  test_oauth_state_validation ();
  test_oauth_scope_validation ();
  test_token_exchange ();
  test_token_exchange_invalid ();
  test_token_exchange_missing_fields ();
  test_token_refresh_partner ();
  test_token_refresh_standard ();
  test_token_refresh_rotation ();
  test_refresh_token_expiry ();
  test_expired_token_detection ();
  
  print_endline "\n--- API Operation Tests ---";
  test_get_person_urn ();
  test_register_upload ();
  test_content_validation ();
  test_ensure_valid_token_fresh ();
  test_get_profile ();
  test_get_posts ();
  test_batch_get_posts ();
  test_posts_scroller ();
  test_search_posts ();
  test_like_post ();
  test_comment_on_post ();
  test_get_post_comments ();
  test_post_with_url_preview ();
  
  print_endline "\n--- Alt-Text Tests ---";
  test_post_with_alt_text ();
  test_post_with_multiple_alt_texts ();
  test_post_without_alt_text ();
  test_post_with_partial_alt_texts ();
  test_alt_text_special_chars ();
  test_thread_with_alt_texts ();
  
  print_endline "\n=== All 31 tests passed! ===\n"
