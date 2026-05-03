(** Focused video processing contract tests for Mastodon provider. *)

module Video_http = struct
  let media_status_checks = ref 0
  let status_post_calls = ref 0
  let media_status_queue : string list ref = ref []

  let reset () =
    media_status_checks := 0;
    status_post_calls := 0;
    media_status_queue := []

  let enqueue_media_statuses statuses =
    media_status_queue := statuses

  let pop_media_status_body () =
    match !media_status_queue with
    | [] -> {|{"id":"media123","url":null,"meta":{"processing":{"state":"processing"}}}|}
    | body :: rest ->
        media_status_queue := rest;
        body

  let get ?headers:_ url on_success _on_error =
    if String.ends_with ~suffix:"/api/v1/accounts/verify_credentials" url then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body = {|{"id":"123","username":"tester"}|};
      }
    else if String.starts_with ~prefix:"https://media.example/" url then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "video/mp4") ];
        body = "FAKE_VIDEO_DATA";
      }
    else if String.contains url '/' && String.starts_with ~prefix:"https://mastodon.social/api/v1/media/" url then (
      incr media_status_checks;
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body = pop_media_status_body ();
      }
    ) else
      on_success { Social_core.status = 404; headers = []; body = "not found" }

  let post ?headers:_ ?body:_ url on_success _on_error =
    if String.ends_with ~suffix:"/api/v1/statuses" url then (
      incr status_post_calls;
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body =
          {|{"id":"54321","url":"https://mastodon.social/@tester/54321","account":{"acct":"tester"}}|};
      }
    ) else
      on_success { Social_core.status = 200; headers = []; body = "{}" }

  let post_multipart ?headers:_ ~parts:_ url on_success _on_error =
    if String.ends_with ~suffix:"/api/v2/media" url then
      on_success {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body = {|{"id":"media123","type":"video","url":null}|};
      }
    else
      on_success { Social_core.status = 404; headers = []; body = "not found" }

  let put ?headers:_ ?body:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }

  let delete ?headers:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }
end

module Video_config = struct
  module Http = Video_http

  let get_env _key = None

  let get_credentials ~account_id:_ on_success _on_error =
    on_success {
      Social_core.access_token =
        {|{"access_token":"video_token","instance_url":"https://mastodon.social"}|};
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

module Mastodon = Social_mastodon_v1.Make (Video_config)

let test_video_waits_for_processing_success () =
  Printf.printf "Test: Video waits for processing before post... ";
  Video_http.reset ();
  Video_http.enqueue_media_statuses [
    {|{"id":"media123","url":null,"meta":{"processing":{"state":"processing"}}}|};
    {|{"id":"media123","url":"https://mastodon.social/media/media123.mp4","meta":{"processing":{"state":"succeeded"}}}|};
  ];
  let got_success = ref false in
  Mastodon.post_single
    ~account_id:"acct"
    ~text:"video post"
    ~media_urls:[ "https://media.example/video.mp4" ]
    (function
      | Error_types.Success _ -> got_success := true
      | Error_types.Partial_success _ -> got_success := true
      | Error_types.Failure err ->
          failwith (Printf.sprintf "Unexpected failure: %s" (Error_types.error_to_string err)));
  assert !got_success;
  assert (!Video_http.media_status_checks >= 2);
  assert (!Video_http.status_post_calls = 1);
  Printf.printf "✓\n"

let test_video_failed_processing_blocks_post () =
  Printf.printf "Test: Video failed processing blocks post... ";
  Video_http.reset ();
  Video_http.enqueue_media_statuses [
    {|{"id":"media123","url":null,"meta":{"processing":{"state":"failed","error":"Transcode failed"}}}|};
  ];
  let got_failure = ref false in
  Mastodon.post_single
    ~account_id:"acct"
    ~text:"video post"
    ~media_urls:[ "https://media.example/video.mp4" ]
    (function
      | Error_types.Failure _ -> got_failure := true
      | Error_types.Success _ -> failwith "Expected failure for failed video processing"
      | Error_types.Partial_success _ -> failwith "Expected full failure for failed video processing");
  assert !got_failure;
  assert (!Video_http.status_post_calls = 0);
  Printf.printf "✓\n"

let () =
  Printf.printf "\n=== Mastodon Video Contract Tests ===\n\n";
  test_video_waits_for_processing_success ();
  test_video_failed_processing_blocks_post ();
  Printf.printf "\n✓ Video contract tests passed\n"
