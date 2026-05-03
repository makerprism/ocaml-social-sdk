(** Focused instance-limits contract tests for Mastodon provider. *)

module Limits_http = struct
  let status_post_calls = ref 0
  let v2_instance_calls = ref 0
  let v1_instance_calls = ref 0
  let fail_v2_instance = ref false
  let v2_max_chars = ref 10
  let v2_max_media = ref 2
  let v2_image_limit = ref 1000
  let v2_video_limit = ref 2000

  let reset () =
    status_post_calls := 0;
    v2_instance_calls := 0;
    v1_instance_calls := 0;
    fail_v2_instance := false;
    v2_max_chars := 10;
    v2_max_media := 2;
    v2_image_limit := 1000;
    v2_video_limit := 2000

  let set_fail_v2_instance v =
    fail_v2_instance := v

  let set_v2_limits ~max_chars ~max_media ~image_limit ~video_limit =
    v2_max_chars := max_chars;
    v2_max_media := max_media;
    v2_image_limit := image_limit;
    v2_video_limit := video_limit

  let get ?headers:_ url on_success _on_error =
    if String.ends_with ~suffix:"/api/v1/accounts/verify_credentials" url then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body = {|{"id":"123","username":"tester"}|};
      }
    else if String.ends_with ~suffix:"/api/v2/instance" url then
      (
        incr v2_instance_calls;
        if !fail_v2_instance then
          on_success { Social_core.status = 500; headers = []; body = "v2 unavailable" }
        else
          on_success {
            Social_core.status = 200;
            headers = [ ("content-type", "application/json") ];
            body =
              Printf.sprintf
                {|{"configuration":{"statuses":{"max_characters":%d,"max_media_attachments":%d},"media_attachments":{"image_size_limit":%d,"video_size_limit":%d}}}|}
                !v2_max_chars
                !v2_max_media
                !v2_image_limit
                !v2_video_limit;
          }
      )
    else if String.ends_with ~suffix:"/api/v1/instance" url then
      (
        incr v1_instance_calls;
        on_success {
          Social_core.status = 200;
          headers = [ ("content-type", "application/json") ];
          body = {|{"max_toot_chars":12}|};
        }
      )
    else if String.starts_with ~prefix:"https://media.example/" url then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "image/jpeg") ];
        body = "IMG";
      }
    else
      on_success { Social_core.status = 404; headers = []; body = "not found" }

  let post ?headers:_ ?body:_ url on_success _on_error =
    if String.ends_with ~suffix:"/api/v1/statuses" url then (
      incr status_post_calls;
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body = {|{"id":"54321","url":"https://mastodon.social/@tester/54321"}|};
      }
    ) else
      on_success { Social_core.status = 200; headers = []; body = "{}" }

  let post_multipart ?headers:_ ~parts:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = {|{"id":"m1"}|} }

  let put ?headers:_ ?body:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }

  let delete ?headers:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }
end

module Limits_config = struct
  module Http = Limits_http

  let get_env _key = None

  let get_credentials ~account_id:_ on_success _on_error =
    on_success {
      Social_core.access_token =
        {|{"access_token":"limits_token","instance_url":"https://mastodon.social"}|};
      refresh_token = None;
      expires_at = None;
      auth_type = Social_core.Bearer;
      scope = None;
    }

  let update_credentials ~account_id:_ ~credentials:_ on_success _on_error = on_success ()
  let encrypt _data on_success _on_error = on_success "encrypted"
  let decrypt _data on_success _on_error = on_success "decrypted"
  let sleep ~seconds:_ on_success _on_error = on_success ()
  let update_health_status ~account_id:_ ~status:_ ~error_message:_ on_success _on_error = on_success ()
end

module Mastodon = Social_mastodon_v1.Make (Limits_config)

let test_instance_max_characters_enforced () =
  Printf.printf "Test: instance max_characters enforced... ";
  Limits_http.reset ();
  let got_failure = ref false in
  Mastodon.post_single
    ~account_id:"acct"
    ~text:"this text is longer than ten"
    ~media_urls:[]
    (function
      | Error_types.Failure (Error_types.Validation_error errs) ->
          got_failure := true;
          assert (List.exists (function Error_types.Text_too_long { max; _ } -> max = 10 | _ -> false) errs)
      | _ -> failwith "Expected validation failure from instance max_characters");
  assert !got_failure;
  assert (!Limits_http.status_post_calls = 0);
  Printf.printf "✓\n"

let test_instance_max_media_enforced () =
  Printf.printf "Test: instance max_media_attachments enforced... ";
  Limits_http.reset ();
  let got_failure = ref false in
  Mastodon.post_single
    ~account_id:"acct"
    ~text:"short"
    ~media_urls:["https://media.example/1.jpg"; "https://media.example/2.jpg"; "https://media.example/3.jpg"]
    (function
      | Error_types.Failure (Error_types.Validation_error errs) ->
          got_failure := true;
          assert (List.exists (function Error_types.Too_many_media { max; _ } -> max = 2 | _ -> false) errs)
      | _ -> failwith "Expected validation failure from instance max_media_attachments");
  assert !got_failure;
  assert (!Limits_http.status_post_calls = 0);
  Printf.printf "✓\n"

let test_instance_limits_v1_fallback_and_cache () =
  Printf.printf "Test: instance limits v1 fallback and cache... ";
  Limits_http.reset ();
  Limits_http.set_fail_v2_instance true;

  let got_limits = ref false in
  Mastodon.get_instance_limits
    ~account_id:"acct"
    ~force_refresh:true
    (function
      | Ok limits ->
          got_limits := true;
          assert (limits.max_status_chars = 12)
      | Error err ->
          failwith (Printf.sprintf "Unexpected get_instance_limits error: %s" (Error_types.error_to_string err)));
  assert !got_limits;
  assert (!Limits_http.v2_instance_calls = 1);
  assert (!Limits_http.v1_instance_calls = 1);

  (* Second call should use cache, no additional HTTP calls *)
  let got_limits_cached = ref false in
  Mastodon.get_instance_limits
    ~account_id:"acct"
    (function
      | Ok limits ->
          got_limits_cached := true;
          assert (limits.max_status_chars = 12)
      | Error err ->
          failwith (Printf.sprintf "Unexpected cached get_instance_limits error: %s" (Error_types.error_to_string err)));
  assert !got_limits_cached;
  assert (!Limits_http.v2_instance_calls = 1);
  assert (!Limits_http.v1_instance_calls = 1);
  Printf.printf "✓\n"

let test_initial_validation_does_not_use_default_limits () =
  Printf.printf "Test: initial validation does not enforce stale defaults... ";
  Limits_http.reset ();
  Limits_http.set_v2_limits ~max_chars:1000 ~max_media:4 ~image_limit:1000000 ~video_limit:1000000;
  let limits_refreshed = ref false in
  Mastodon.get_instance_limits
    ~account_id:"acct"
    ~force_refresh:true
    (function
      | Ok limits ->
          limits_refreshed := true;
          assert (limits.max_status_chars = 1000)
      | Error err ->
          failwith (Printf.sprintf "Unexpected limits refresh error: %s" (Error_types.error_to_string err)));
  assert !limits_refreshed;
  let long_text = String.make 700 'x' in
  let got_success = ref false in
  Mastodon.post_single
    ~account_id:"acct"
    ~text:long_text
    ~media_urls:[]
    (function
      | Error_types.Success _ -> got_success := true
      | Error_types.Partial_success _ -> got_success := true
      | Error_types.Failure err ->
          failwith (Printf.sprintf "Unexpected failure with high instance max_chars: %s" (Error_types.error_to_string err)));
  assert !got_success;
  assert (!Limits_http.status_post_calls = 1);
  Printf.printf "✓\n"

let () =
  Printf.printf "\n=== Mastodon Instance Limits Contract Tests ===\n\n";
  test_instance_max_characters_enforced ();
  test_instance_max_media_enforced ();
  test_instance_limits_v1_fallback_and_cache ();
  test_initial_validation_does_not_use_default_limits ();
  Printf.printf "\n✓ Instance limits contract tests passed\n"
