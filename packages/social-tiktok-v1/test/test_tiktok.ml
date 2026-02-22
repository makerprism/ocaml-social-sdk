(** Basic tests for TikTok API types and validation *)

open Social_core

(** Mock HTTP client for testing *)
module Mock_http = struct
  type get_call = {
    url : string;
    headers : (string * string) list;
  }

  type post_call = {
    url : string;
    headers : (string * string) list;
    body : string option;
  }

  type put_call = {
    url : string;
    headers : (string * string) list;
    body : string option;
  }

  let get_calls : get_call list ref = ref []
  let post_calls : post_call list ref = ref []
  let put_calls : put_call list ref = ref []
  let custom_get_handler : (string -> Social_core.response) option ref = ref None
  let custom_get_error_handler : (string -> string) option ref = ref None
  let custom_post_handler : (string -> ((string * string) list) -> string option -> Social_core.response) option ref = ref None
  let custom_put_handler : (string -> ((string * string) list) -> string option -> Social_core.response) option ref = ref None

  let reset () =
    get_calls := [];
    post_calls := [];
    put_calls := [];
    custom_get_handler := None;
    custom_get_error_handler := None;
    custom_post_handler := None;
    custom_put_handler := None

  let set_custom_get_handler f =
    custom_get_handler := Some f

  let set_custom_get_error_handler f =
    custom_get_error_handler := Some f

  let set_custom_post_handler f =
    custom_post_handler := Some f

  let set_custom_put_handler f =
    custom_put_handler := Some f

  let get ?(headers=[]) url on_success _on_error =
    get_calls := { url; headers } :: !get_calls;
    match !custom_get_error_handler with
    | Some f -> _on_error (f url)
    | None ->
        (match !custom_get_handler with
         | Some f -> on_success (f url)
         | None ->
              (* Return mock video content *)
              on_success {
               Social_core.status = 200;
               headers = [("content-type", "video/mp4")];
               body = "mock_video_content";
             })
  
  let post ?(headers=[]) ?body url on_success _on_error =
    post_calls := { url; headers; body } :: !post_calls;
    match !custom_post_handler with
    | Some f -> on_success (f url headers body)
    | None ->
    if String.ends_with ~suffix:"video/init/" url then
      on_success {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"data":{"publish_id":"pub_123","upload_url":"https://upload.tiktok.com/video"}}|};
      }
    else if String.ends_with ~suffix:"creator_info/query/" url then
      on_success {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"data":{"creator_avatar_url":"https://example.com/avatar.jpg","creator_username":"testuser","creator_nickname":"Test User","privacy_level_options":["PUBLIC_TO_EVERYONE","SELF_ONLY"],"comment_disabled":false,"duet_disabled":false,"stitch_disabled":false,"max_video_post_duration_sec":600}}|};
      }
    else if String.ends_with ~suffix:"status/fetch/" url then
      on_success {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"data":{"status":"PUBLISH_COMPLETE","publicaly_available_post_id":[{"id":"video_123"}]}}|};
      }
    else
      on_success {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"test_token","refresh_token":"test_refresh","expires_in":86400}|};
      }
  
  let post_multipart ?headers:_ ~parts:_ _url on_success _on_error =
    on_success {
      Social_core.status = 200;
      headers = [];
      body = "{}";
    }
  
  let put ?(headers=[]) ?body url on_success _on_error =
    put_calls := { url; headers; body } :: !put_calls;
    match !custom_put_handler with
    | Some f -> on_success (f url headers body)
    | None ->
        on_success {
          Social_core.status = 200;
          headers = [];
          body = "{}";
        }
  
  let delete ?headers:_ _url on_success _on_error =
    on_success {
      Social_core.status = 200;
      headers = [];
      body = "{}";
    }
end

(** Mock config for testing *)
module Mock_config = struct
  module Http = Mock_http

  let env_vars : (string * string) list ref = ref [
    ("TIKTOK_CLIENT_KEY", "test_client_key");
    ("TIKTOK_CLIENT_SECRET", "test_client_secret");
  ]

  let current_credentials : Social_core.credentials ref = ref {
    Social_core.access_token = "test_access_token";
    refresh_token = Some "test_refresh_token";
    expires_at = Some "2099-12-31T23:59:59Z";
    token_type = "Bearer";
  }

  let set_credentials creds =
    current_credentials := creds

  let set_env key value_opt =
    env_vars := List.remove_assoc key !env_vars;
    match value_opt with
    | Some v -> env_vars := (key, v) :: !env_vars
    | None -> ()
  
  let get_env key =
    List.assoc_opt key !env_vars
  
  let get_credentials ~account_id:_ on_success _on_error =
    on_success !current_credentials

  let update_credentials ~account_id:_ ~credentials on_success _on_error =
    current_credentials := credentials;
    on_success ()
  
  let encrypt _data on_success _on_error =
    on_success "encrypted_data"
  
  let decrypt _data on_success _on_error =
    on_success "decrypted_data"
  
  let update_health_status ~account_id:_ ~status:_ ~error_message:_ on_success _on_error =
    on_success ()
end

module TikTok = Social_tiktok_v1.Make(Mock_config)

let reset_mock_state () =
  Mock_http.reset ();
  Mock_config.set_env "TIKTOK_CLIENT_KEY" (Some "test_client_key");
  Mock_config.set_env "TIKTOK_CLIENT_SECRET" (Some "test_client_secret");
  Mock_config.set_credentials {
    Social_core.access_token = "test_access_token";
    refresh_token = Some "test_refresh_token";
    expires_at = Some "2099-12-31T23:59:59Z";
    token_type = "Bearer";
  }

(** Helper to handle outcome type for tests *)
let handle_outcome on_success on_error outcome =
  match outcome with
  | Error_types.Success result -> on_success result
  | Error_types.Partial_success { result; _ } -> on_success result
  | Error_types.Failure err -> on_error (Error_types.error_to_string err)

(** Helper to handle thread_result outcome *)
let handle_thread_outcome on_success on_error outcome =
  match outcome with
  | Error_types.Success result -> on_success result.Error_types.posted_ids
  | Error_types.Partial_success { result; _ } -> on_success result.Error_types.posted_ids
  | Error_types.Failure err -> on_error (Error_types.error_to_string err)

let test_validate_video_ok () =
  print_string "Test: validate_video OK... ";
  match Social_tiktok_v1.validate_video ~duration_sec:30 ~file_size_bytes:10_000_000 ~width:1080 ~height:1920 with
  | Ok () -> print_endline "PASSED"
  | Error msg -> failwith ("Expected Ok, got Error: " ^ msg)

let test_validate_video_too_short () =
  print_string "Test: validate_video too short... ";
  match Social_tiktok_v1.validate_video ~duration_sec:1 ~file_size_bytes:10_000_000 ~width:1080 ~height:1920 with
  | Error _ -> print_endline "PASSED"
  | Ok () -> failwith "Expected Error for too short video"

let test_validate_video_too_large () =
  print_string "Test: validate_video too large... ";
  match Social_tiktok_v1.validate_video ~duration_sec:30 ~file_size_bytes:5_000_000_000 ~width:1080 ~height:1920 with
  | Error _ -> print_endline "PASSED"
  | Ok () -> failwith "Expected Error for too large video"

let test_privacy_level_roundtrip () =
  print_string "Test: privacy_level roundtrip... ";
  let levels = [Social_tiktok_v1.PublicToEveryone; MutualFollowFriends; SelfOnly] in
  List.iter (fun level ->
    let s = Social_tiktok_v1.string_of_privacy_level level in
    let level2 = Social_tiktok_v1.privacy_level_of_string s in
    assert (level = level2)
  ) levels;
  print_endline "PASSED"

let test_authorization_url () =
  print_string "Test: authorization_url... ";
  let url = Social_tiktok_v1.get_authorization_url 
    ~client_id:"test_client"
    ~redirect_uri:"https://example.com/callback"
    ~scope:"user.info.basic,video.publish"
    ~state:"random_state"
  in
  assert (String.length url > 0);
  assert (String.sub url 0 5 = "https");
  print_endline "PASSED"

let test_post_single_no_media () =
  print_string "Test: post_single returns validation error without media... ";
  let error_received = ref false in
  TikTok.post_single
    ~account_id:"test_account"
    ~text:"Test caption"
    ~media_urls:[]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Validation_error errs) ->
          (* Should contain Media_required since TikTok requires video *)
          let has_media_required = List.exists (function Error_types.Media_required -> true | _ -> false) errs in
          if has_media_required then begin
            error_received := true;
            print_endline "PASSED"
          end else
            failwith "Expected Media_required validation error"
      | _ ->
          failwith "Expected validation error");
  assert !error_received

let test_post_single_caption_too_long () =
  print_string "Test: post_single returns validation error for long caption... ";
  let error_received = ref false in
  let long_caption = String.make 2300 'a' in
  TikTok.post_single
    ~account_id:"test_account"
    ~text:long_caption
    ~media_urls:["https://example.com/video.mp4"]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Validation_error _) ->
          error_received := true;
          print_endline "PASSED"
      | _ ->
          failwith "Expected validation error");
  assert !error_received

let test_post_single_invalid_media_url () =
  print_string "Test: post_single invalid media URL... ";
  reset_mock_state ();
  let error_received = ref false in
  TikTok.post_single
    ~account_id:"test_account"
    ~text:"Test caption"
    ~media_urls:["not-a-valid-url"]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Validation_error errs) ->
          let has_invalid_url =
            List.exists (function Error_types.Invalid_url "not-a-valid-url" -> true | _ -> false) errs
          in
          if has_invalid_url then (
            error_received := true;
            print_endline "PASSED"
          ) else
            failwith "Expected Invalid_url validation error"
      | _ -> failwith "Expected validation error");
  assert !error_received

let test_post_single_uppercase_scheme_url_valid () =
  print_string "Test: post_single uppercase scheme URL valid... ";
  reset_mock_state ();
  let success_called = ref false in
  TikTok.post_single
    ~account_id:"test_account"
    ~text:"Test caption"
    ~media_urls:["HTTPS://example.com/video.mp4"]
    (handle_outcome
      (fun _publish_id ->
        success_called := true;
        print_endline "PASSED")
      (fun err ->
        failwith ("Unexpected error: " ^ err)));
  assert !success_called

let test_post_single_success () =
  print_string "Test: post_single success... ";
  let success_called = ref false in
  TikTok.post_single
    ~account_id:"test_account"
    ~text:"Test caption"
    ~media_urls:["https://example.com/video.mp4"]
    (handle_outcome
      (fun _publish_id ->
        success_called := true;
        print_endline "PASSED")
      (fun err ->
        failwith ("Unexpected error: " ^ err)));
  assert !success_called

let test_post_thread_success () =
  print_string "Test: post_thread success... ";
  let success_called = ref false in
  TikTok.post_thread
    ~account_id:"test_account"
    ~texts:["First video"; "Second video"]
    ~media_urls_per_post:[["https://example.com/video1.mp4"]; ["https://example.com/video2.mp4"]]
    (handle_thread_outcome
      (fun post_ids ->
        assert (List.length post_ids = 2);
        success_called := true;
        print_endline "PASSED")
      (fun err ->
        failwith ("Unexpected error: " ^ err)));
  assert !success_called

let test_post_thread_empty () =
  print_string "Test: post_thread returns validation error for empty thread... ";
  let error_received = ref false in
  TikTok.post_thread
    ~account_id:"test_account"
    ~texts:[]
    ~media_urls_per_post:[]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Validation_error _) ->
          error_received := true;
          print_endline "PASSED"
      | _ ->
          failwith "Expected validation error");
  assert !error_received

let test_post_thread_mismatched_lengths_extra_media () =
  print_string "Test: post_thread mismatched lengths (extra media)... ";
  reset_mock_state ();
  let error_received = ref false in
  TikTok.post_thread
    ~account_id:"test_account"
    ~texts:["First video"]
    ~media_urls_per_post:[
      ["https://example.com/video1.mp4"];
      ["https://example.com/video2.mp4"]
    ]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Validation_error errs) ->
          let has_mismatch =
            List.exists
              (function
                | Error_types.Thread_post_invalid { index = 1; errors } ->
                    List.exists (function Error_types.Text_empty -> true | _ -> false) errors
                | _ -> false)
              errs
          in
          if has_mismatch then (
            error_received := true;
            print_endline "PASSED"
          ) else
            failwith "Expected Thread_post_invalid mismatch error"
      | _ -> failwith "Expected validation error");
  assert !error_received

let test_post_thread_mismatched_lengths_missing_media_entry () =
  print_string "Test: post_thread mismatched lengths (missing media)... ";
  reset_mock_state ();
  let error_received = ref false in
  TikTok.post_thread
    ~account_id:"test_account"
    ~texts:["First video"; "Second video"]
    ~media_urls_per_post:[
      ["https://example.com/video1.mp4"]
    ]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Validation_error errs) ->
          let has_mismatch =
            List.exists
              (function
                | Error_types.Thread_post_invalid { index = 1; errors } ->
                    List.exists (function Error_types.Media_required -> true | _ -> false) errors
                | _ -> false)
              errs
          in
          if has_mismatch then (
            error_received := true;
            print_endline "PASSED"
          ) else
            failwith "Expected Thread_post_invalid mismatch error"
      | _ -> failwith "Expected validation error");
  assert !error_received

(** Helper to handle api_result for tests *)
let handle_api_result on_success on_error result =
  match result with
  | Ok value -> on_success value
  | Error err -> on_error (Error_types.error_to_string err)

let test_get_creator_info () =
  print_string "Test: get_creator_info success... ";
  let success_called = ref false in
  TikTok.get_creator_info
    ~account_id:"test_account"
    (handle_api_result
      (fun info ->
        assert (info.Social_tiktok_v1.creator_username = "testuser");
        assert (info.creator_nickname = "Test User");
        assert (info.max_video_post_duration_sec = 600);
        success_called := true;
        print_endline "PASSED")
      (fun err ->
        failwith ("Unexpected error: " ^ err)));
  assert !success_called

let test_parse_creator_info_missing_optional_fields () =
  print_string "Test: parse_creator_info missing optional fields... ";
  let json = Yojson.Basic.from_string
    {|{"data":{"creator_username":"user_only"}}|}
  in
  match Social_tiktok_v1.parse_creator_info json with
  | Ok info ->
      assert (info.creator_username = "user_only");
      assert (List.length info.privacy_level_options >= 1);
      assert (info.comment_disabled = false);
      assert (info.duet_disabled = false);
      assert (info.stitch_disabled = false);
      assert (info.max_video_post_duration_sec = 600);
      print_endline "PASSED"
  | Error err -> failwith ("Unexpected parse error: " ^ err)

let test_parse_creator_info_unknown_privacy_values_fallback () =
  print_string "Test: parse_creator_info unknown privacy fallback... ";
  let json = Yojson.Basic.from_string
    {|{"data":{"creator_username":"user","privacy_level_options":["UNKNOWN_POLICY"]}}|}
  in
  match Social_tiktok_v1.parse_creator_info json with
  | Ok info ->
      assert (info.privacy_level_options = [Social_tiktok_v1.SelfOnly]);
      print_endline "PASSED"
  | Error err -> failwith ("Unexpected parse error: " ^ err)

let test_parse_creator_info_missing_privacy_defaults_self_only () =
  print_string "Test: parse_creator_info missing privacy defaults self_only... ";
  let json = Yojson.Basic.from_string
    {|{"data":{"creator_username":"user_no_privacy"}}|}
  in
  match Social_tiktok_v1.parse_creator_info json with
  | Ok info ->
      assert (info.privacy_level_options = [Social_tiktok_v1.SelfOnly]);
       print_endline "PASSED"
  | Error err -> failwith ("Unexpected parse error: " ^ err)

let test_parse_account_stats_response () =
  print_string "Test: parse_account_stats_response... ";
  let json = Yojson.Basic.from_string
    {|{"data":{"user":{"follower_count":101,"following_count":"12","likes_count":5000,"video_count":34}}}|}
  in
  match Social_tiktok_v1.parse_account_stats_response json with
  | Ok (stats : Social_tiktok_v1.account_stats) ->
      assert (stats.Social_tiktok_v1.follower_count = 101);
      assert (stats.Social_tiktok_v1.following_count = 12);
      assert (stats.Social_tiktok_v1.likes_count = 5000);
      assert (stats.Social_tiktok_v1.video_count = 34);
      print_endline "PASSED"
  | Error err -> failwith ("Unexpected parse error: " ^ err)

let test_parse_video_ids_response () =
  print_string "Test: parse_video_ids_response... ";
  let json = Yojson.Basic.from_string
    {|{"data":{"videos":[{"id":"video_1"},{"id":"video_2"},{"bad":"skip"}]}}|}
  in
  match Social_tiktok_v1.parse_video_ids_response json with
  | Ok ids ->
      assert (ids = ["video_1"; "video_2"]);
      print_endline "PASSED"
  | Error err -> failwith ("Unexpected parse error: " ^ err)

let test_parse_video_analytics_response () =
  print_string "Test: parse_video_analytics_response... ";
  let json = Yojson.Basic.from_string
    {|{"data":{"videos":[{"id":"video_1","like_count":10,"comment_count":"2","share_count":1,"view_count":300}]}}|}
  in
  match Social_tiktok_v1.parse_video_analytics_response json with
  | Ok [ (video : Social_tiktok_v1.video_analytics) ] ->
      assert (video.Social_tiktok_v1.id = "video_1");
      assert (video.Social_tiktok_v1.like_count = 10);
      assert (video.Social_tiktok_v1.comment_count = 2);
      assert (video.Social_tiktok_v1.share_count = 1);
      assert (video.Social_tiktok_v1.view_count = 300);
      print_endline "PASSED"
  | Ok _ -> failwith "Expected exactly one parsed video analytics record"
  | Error err -> failwith ("Unexpected parse error: " ^ err)

let test_canonical_analytics_adapters () =
  print_string "Test: canonical analytics adapters... ";
  let find_series provider_metric series =
    List.find_opt
      (fun item -> item.Analytics_types.provider_metric = Some provider_metric)
      series
  in
  let analytics : Social_tiktok_v1.account_analytics = {
    account = {
      follower_count = 1000;
      following_count = 250;
      likes_count = 9000;
      video_count = 40;
    };
    recent_video_analytics =
      [ { id = "video_1"; like_count = 20; comment_count = 3; share_count = 2; view_count = 120 };
        { id = "video_2"; like_count = 30; comment_count = 4; share_count = 1; view_count = 180 } ];
  } in
  let account_series = Social_tiktok_v1.to_canonical_account_analytics_series analytics in
  assert (List.length account_series = 8);
  (match find_series "follower_count" account_series with
   | Some item ->
       assert (Analytics_types.canonical_metric_key item.metric = "followers");
       assert ((List.hd item.points).value = 1000)
   | None -> failwith "Missing follower_count canonical series");
  (match find_series "view_count" account_series with
   | Some item ->
       assert (Analytics_types.canonical_metric_key item.metric = "views");
       assert ((List.hd item.points).value = 300)
   | None -> failwith "Missing view_count canonical series");

  let video_analytics : Social_tiktok_v1.video_analytics = {
    id = "video_99";
    like_count = 7;
    comment_count = 3;
    share_count = 2;
    view_count = 400;
  } in
  let video_series = Social_tiktok_v1.to_canonical_video_analytics_series video_analytics in
  assert (List.length video_series = 4);
  (match find_series "comment_count" video_series with
   | Some item ->
       assert (Analytics_types.canonical_metric_key item.metric = "comments");
       assert ((List.hd item.points).value = 3)
   | None -> failwith "Missing comment_count canonical series");
  print_endline "PASSED"

let test_get_account_analytics_request_contract () =
  print_string "Test: get_account_analytics request contract... ";
  reset_mock_state ();
  Mock_http.set_custom_get_handler (fun url ->
    if url = "https://open.tiktokapis.com/v2/user/info/?fields=follower_count,following_count,likes_count,video_count" then
      {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body = {|{"data":{"user":{"follower_count":1000,"following_count":250,"likes_count":9999,"video_count":77}}}|};
      }
    else
      failwith ("Unexpected GET URL: " ^ url));
  Mock_http.set_custom_post_handler (fun url _headers body ->
    if url = "https://open.tiktokapis.com/v2/video/list/?fields=id" then
      {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body = {|{"data":{"videos":[{"id":"video_1"},{"id":"video_2"}]}}|};
      }
    else if url = "https://open.tiktokapis.com/v2/video/query/?fields=id,like_count,comment_count,share_count,view_count" then
      {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body = {|{"data":{"videos":[{"id":"video_1","like_count":11,"comment_count":2,"share_count":1,"view_count":100},{"id":"video_2","like_count":22,"comment_count":4,"share_count":2,"view_count":200}]}}|};
      }
    else
      failwith ("Unexpected POST URL: " ^ url ^ " body=" ^ Option.value ~default:"" body));
  let success_called = ref false in
  TikTok.get_account_analytics
    ~account_id:"test_account"
    (handle_api_result
      (fun (analytics : Social_tiktok_v1.account_analytics) ->
        assert (analytics.Social_tiktok_v1.account.Social_tiktok_v1.follower_count = 1000);
        assert (List.length analytics.Social_tiktok_v1.recent_video_analytics = 2);
        success_called := true)
      (fun err -> failwith ("Unexpected error: " ^ err)));
  assert !success_called;

  let user_info_get_call =
    match List.find_opt (fun (c : Mock_http.get_call) -> c.url = "https://open.tiktokapis.com/v2/user/info/?fields=follower_count,following_count,likes_count,video_count") !(Mock_http.get_calls) with
    | Some c -> c
    | None -> failwith "Expected user info GET call"
  in
  let auth_header_present =
    List.exists (fun (k, v) -> k = "Authorization" && v = "Bearer test_access_token") user_info_get_call.headers
  in
  assert auth_header_present;

  let video_list_call =
    match List.find_opt (fun (c : Mock_http.post_call) -> c.url = "https://open.tiktokapis.com/v2/video/list/?fields=id") !(Mock_http.post_calls) with
    | Some c -> c
    | None -> failwith "Expected video list POST call"
  in
  let video_list_body = Option.value ~default:"" video_list_call.body in
  let list_json = Yojson.Basic.from_string video_list_body in
  let open Yojson.Basic.Util in
  assert ((list_json |> member "max_count" |> to_int) = 20);

  let video_query_call =
    match List.find_opt (fun (c : Mock_http.post_call) -> c.url = "https://open.tiktokapis.com/v2/video/query/?fields=id,like_count,comment_count,share_count,view_count") !(Mock_http.post_calls) with
    | Some c -> c
    | None -> failwith "Expected video query POST call"
  in
  let video_query_body = Option.value ~default:"" video_query_call.body in
  let query_json = Yojson.Basic.from_string video_query_body in
  let query_video_ids =
    query_json
    |> member "filters"
    |> member "video_ids"
    |> to_list
    |> List.map to_string
  in
  assert (query_video_ids = ["video_1"; "video_2"]);
  print_endline "PASSED"

let test_get_post_analytics_request_contract () =
  print_string "Test: get_post_analytics request contract... ";
  reset_mock_state ();
  Mock_http.set_custom_post_handler (fun url _headers body ->
    if url = "https://open.tiktokapis.com/v2/video/query/?fields=id,like_count,comment_count,share_count,view_count" then
      {
        Social_core.status = 200;
        headers = [ ("content-type", "application/json") ];
        body = {|{"data":{"videos":[{"id":"video_99","like_count":7,"comment_count":3,"share_count":2,"view_count":400}]}}|};
      }
    else
      failwith ("Unexpected POST URL: " ^ url ^ " body=" ^ Option.value ~default:"" body));
  let success_called = ref false in
  TikTok.get_post_analytics
    ~account_id:"test_account"
    ~video_id:"video_99"
    (handle_api_result
      (fun (video : Social_tiktok_v1.video_analytics) ->
        assert (video.Social_tiktok_v1.id = "video_99");
        assert (video.Social_tiktok_v1.view_count = 400);
        success_called := true)
      (fun err -> failwith ("Unexpected error: " ^ err)));
  assert !success_called;

  let query_call =
    match List.find_opt (fun (c : Mock_http.post_call) -> c.url = "https://open.tiktokapis.com/v2/video/query/?fields=id,like_count,comment_count,share_count,view_count") !(Mock_http.post_calls) with
    | Some c -> c
    | None -> failwith "Expected video query POST call"
  in
  let query_body = Option.value ~default:"" query_call.body in
  let query_json = Yojson.Basic.from_string query_body in
  let open Yojson.Basic.Util in
  let video_ids =
    query_json
    |> member "filters"
    |> member "video_ids"
    |> to_list
    |> List.map to_string
  in
  assert (video_ids = ["video_99"]);
  print_endline "PASSED"

let test_check_publish_status () =
  print_string "Test: check_publish_status success... ";
  let success_called = ref false in
  TikTok.check_publish_status
    ~account_id:"test_account"
    ~publish_id:"pub_123"
    (handle_api_result
      (fun status ->
        (match status with
        | Social_tiktok_v1.Published video_id ->
            assert (video_id = "video_123");
            success_called := true;
            print_endline "PASSED"
        | _ ->
            failwith "Expected Published status"))
      (fun err ->
        failwith ("Unexpected error: " ^ err)));
  assert !success_called

let test_exchange_code () =
  print_string "Test: exchange_code success... ";
  reset_mock_state ();
  let success_called = ref false in
  TikTok.exchange_code
    ~code:"auth_code_123"
    ~redirect_uri:"https://example.com/callback"
    (handle_api_result
      (fun creds ->
        assert (creds.Social_core.access_token = "test_token");
        assert (creds.refresh_token = Some "test_refresh");
        success_called := true;
        print_endline "PASSED")
      (fun err ->
        failwith ("Unexpected error: " ^ err)));
  assert !success_called

let test_get_oauth_url () =
  print_string "Test: get_oauth_url success... ";
  reset_mock_state ();
  let success_called = ref false in
  TikTok.get_oauth_url
    ~redirect_uri:"https://example.com/callback"
    ~state:"random_state"
    ~code_verifier:""
    (handle_api_result
      (fun url ->
        assert (String.length url > 0);
        assert (String.sub url 0 5 = "https");
        success_called := true;
        print_endline "PASSED")
      (fun err ->
        failwith ("Unexpected error: " ^ err)));
  assert !success_called

let test_get_oauth_url_includes_pkce_when_code_verifier_present () =
  print_string "Test: get_oauth_url includes PKCE... ";
  reset_mock_state ();
  let success_called = ref false in
  TikTok.get_oauth_url
    ~redirect_uri:"https://example.com/callback"
    ~state:"random_state"
    ~code_verifier:"pkce_verifier_123"
    (handle_api_result
      (fun url ->
        let uri = Uri.of_string url in
        assert (Uri.get_query_param uri "code_challenge" = Some "pkce_verifier_123");
        assert (Uri.get_query_param uri "code_challenge_method" = Some "plain");
        success_called := true;
        print_endline "PASSED")
      (fun err ->
        failwith ("Unexpected error: " ^ err)));
  assert !success_called

let get_query_param_exn uri key =
  match Uri.get_query_param uri key with
  | Some v -> v
  | None -> failwith ("Missing query param: " ^ key)

let test_oauth_auth_url_required_params () =
  print_string "Test: oauth_auth_url_required_params... ";
  let url = Social_tiktok_v1.get_authorization_url
    ~client_id:"test_client"
    ~redirect_uri:"https://example.com/callback"
    ~scope:"user.info.basic,video.publish"
    ~state:"random_state"
  in
  let uri = Uri.of_string url in
  assert (get_query_param_exn uri "client_key" = "test_client");
  assert (Uri.get_query_param uri "client_id" = None);
  assert (get_query_param_exn uri "response_type" = "code");
  assert (get_query_param_exn uri "scope" = "user.info.basic,video.publish");
  assert (get_query_param_exn uri "state" = "random_state");
  print_endline "PASSED"

let test_oauth_exchange_request_contract () =
  print_string "Test: oauth_exchange_request_contract... ";
  reset_mock_state ();
  let success_called = ref false in
  TikTok.exchange_code
    ~code:"auth_code_123"
    ~redirect_uri:"https://example.com/callback"
    (handle_api_result
      (fun _ -> success_called := true)
      (fun err -> failwith ("Unexpected error: " ^ err)));
  assert !success_called;
  let call =
    match !(Mock_http.post_calls) with
    | last :: _ -> last
    | [] -> failwith "Expected at least one POST call"
  in
  assert (call.url = "https://open.tiktokapis.com/v2/oauth/token/");
  let content_type_ok =
    List.exists (fun (k, v) -> k = "Content-Type" && v = "application/x-www-form-urlencoded") call.headers
  in
  assert content_type_ok;
  let body = Option.value ~default:"" call.body in
  let params = Uri.query_of_encoded body in
  let get_param key =
    match List.assoc_opt key params with
    | Some (v :: _) -> v
    | _ -> failwith ("Missing body param: " ^ key)
  in
  assert (get_param "client_key" = "test_client_key");
  assert (get_param "client_secret" = "test_client_secret");
  assert (get_param "code" = "auth_code_123");
  assert (get_param "grant_type" = "authorization_code");
  assert (get_param "redirect_uri" = "https://example.com/callback");
  print_endline "PASSED"

let test_oauth_exchange_request_includes_code_verifier_when_present () =
  print_string "Test: oauth_exchange includes code_verifier... ";
  reset_mock_state ();
  let success_called = ref false in
  TikTok.exchange_code
    ~code_verifier:"pkce_verifier_123"
    ~code:"auth_code_123"
    ~redirect_uri:"https://example.com/callback"
    (handle_api_result
      (fun _ -> success_called := true)
      (fun err -> failwith ("Unexpected error: " ^ err)));
  assert !success_called;
  let call =
    match !(Mock_http.post_calls) with
    | last :: _ -> last
    | [] -> failwith "Expected at least one POST call"
  in
  let body = Option.value ~default:"" call.body in
  let params = Uri.query_of_encoded body in
  let code_verifier =
    match List.assoc_opt "code_verifier" params with
    | Some (v :: _) -> v
    | _ -> failwith "Expected code_verifier body param"
  in
  assert (code_verifier = "pkce_verifier_123");
  print_endline "PASSED"

let test_oauth_refresh_triggered_within_buffer () =
  print_string "Test: oauth_refresh_triggered_within_buffer... ";
  reset_mock_state ();
  let now = Ptime_clock.now () in
  let expiring_soon =
    match Ptime.add_span now (Ptime.Span.of_int_s 10) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> Ptime.to_rfc3339 now
  in
  Mock_config.set_credentials {
    Social_core.access_token = "stale_access";
    refresh_token = Some "old_refresh";
    expires_at = Some expiring_soon;
    token_type = "Bearer";
  };
  Mock_http.set_custom_post_handler (fun url _headers body ->
    if String.ends_with ~suffix:"oauth/token/" url then {
      Social_core.status = 200;
      headers = [("content-type", "application/json")];
      body = {|{"access_token":"new_access","refresh_token":"new_refresh","expires_in":86400}|};
    } else if String.ends_with ~suffix:"creator_info/query/" url then {
      Social_core.status = 200;
      headers = [("content-type", "application/json")];
      body = {|{"data":{"creator_avatar_url":"https://example.com/avatar.jpg","creator_username":"testuser","creator_nickname":"Test User","privacy_level_options":["PUBLIC_TO_EVERYONE","SELF_ONLY"],"comment_disabled":false,"duet_disabled":false,"stitch_disabled":false,"max_video_post_duration_sec":600}}|};
    } else (
      let body_text = Option.value ~default:"" body in
      failwith ("Unexpected POST URL: " ^ url ^ " body=" ^ body_text)
    ));
  let success_called = ref false in
  TikTok.get_creator_info ~account_id:"test_account"
    (handle_api_result
      (fun _ -> success_called := true)
      (fun err -> failwith ("Unexpected error: " ^ err)));
  assert !success_called;
  let refresh_call_opt =
    List.find_opt
      (fun (c : Mock_http.post_call) -> String.ends_with ~suffix:"oauth/token/" c.url)
      !(Mock_http.post_calls)
  in
  let refresh_call =
    match refresh_call_opt with
    | Some c -> c
    | None -> failwith "Expected refresh token call"
  in
  let refresh_body = Option.value ~default:"" refresh_call.body in
  let refresh_params = Uri.query_of_encoded refresh_body in
  let get_param key =
    match List.assoc_opt key refresh_params with
    | Some (v :: _) -> v
    | _ -> failwith ("Missing refresh param: " ^ key)
  in
  assert (get_param "grant_type" = "refresh_token");
  assert (get_param "refresh_token" = "old_refresh");
  assert (!Mock_config.current_credentials.Social_core.access_token = "new_access");
  assert (!Mock_config.current_credentials.refresh_token = Some "new_refresh");
  print_endline "PASSED"

let test_get_oauth_url_missing_client_key () =
  print_string "Test: get_oauth_url missing client key... ";
  reset_mock_state ();
  Mock_config.set_env "TIKTOK_CLIENT_KEY" None;
  let error_called = ref false in
  TikTok.get_oauth_url
    ~redirect_uri:"https://example.com/callback"
    ~state:"random_state"
    ~code_verifier:""
    (function
      | Ok _ -> failwith "Expected error when client key is missing"
      | Error _ ->
          error_called := true;
          print_endline "PASSED");
  assert !error_called;
  Mock_config.set_env "TIKTOK_CLIENT_KEY" (Some "test_client_key")

let test_parse_publish_status_with_publicly_field () =
  print_string "Test: parse_publish_status publicly field... ";
  let json = Yojson.Basic.from_string
    {|{"data":{"status":"PUBLISH_COMPLETE","publicly_available_post_id":[{"id":"video_publicly_1"}]}}|}
  in
  match Social_tiktok_v1.parse_publish_status json with
  | Social_tiktok_v1.Published id when id = "video_publicly_1" -> print_endline "PASSED"
  | _ -> failwith "Expected Published status with fallback field name"

let test_parse_publish_status_failed_without_reason () =
  print_string "Test: parse_publish_status failed fallback reason... ";
  let json = Yojson.Basic.from_string {|{"data":{"status":"FAILED"}}|} in
  match Social_tiktok_v1.parse_publish_status json with
  | Social_tiktok_v1.Failed { error_code; error_message } ->
      assert (error_code = "UPLOAD_FAILED");
      assert (String.length error_message > 0);
      print_endline "PASSED"
  | _ -> failwith "Expected Failed status"

let test_init_video_upload_request_contract () =
  print_string "Test: init_video_upload request contract... ";
  reset_mock_state ();
  Mock_http.set_custom_post_handler (fun url _headers body ->
    if String.ends_with ~suffix:"video/init/" url then {
      Social_core.status = 200;
      headers = [("content-type", "application/json")];
      body = {|{"data":{"publish_id":"pub_contract","upload_url":"https://upload.tiktok.com/video"}}|};
    } else if String.ends_with ~suffix:"oauth/token/" url then {
      Social_core.status = 200;
      headers = [("content-type", "application/json")];
      body = {|{"access_token":"new_access","refresh_token":"new_refresh","expires_in":86400}|};
    } else (
      let body_text = Option.value ~default:"" body in
      failwith ("Unexpected POST URL: " ^ url ^ " body=" ^ body_text)
    ));
  let post_info = Social_tiktok_v1.make_post_info ~title:"Contract test" ~privacy_level:Social_tiktok_v1.SelfOnly () in
  let success_called = ref false in
  TikTok.init_video_upload
    ~account_id:"test_account"
    ~post_info
    ~video_size:12345
    (handle_api_result
      (fun (publish_id, _upload_url) ->
        assert (publish_id = "pub_contract");
        success_called := true)
      (fun err -> failwith ("Unexpected error: " ^ err)));
  assert !success_called;
  let init_call =
    match List.find_opt (fun (c : Mock_http.post_call) -> String.ends_with ~suffix:"video/init/" c.url) !(Mock_http.post_calls) with
    | Some c -> c
    | None -> failwith "Expected init video upload POST call"
  in
  let body = Option.value ~default:"" init_call.body in
  let json = Yojson.Basic.from_string body in
  let open Yojson.Basic.Util in
  let post_json = json |> member "post_info" in
  let source_json = json |> member "source_info" in
  assert ((post_json |> member "title" |> to_string) = "Contract test");
  assert ((post_json |> member "privacy_level" |> to_string) = "SELF_ONLY");
  assert ((source_json |> member "source" |> to_string) = "FILE_UPLOAD");
  assert ((source_json |> member "video_size" |> to_int) = 12345);
  assert ((source_json |> member "chunk_size" |> to_int) = 12345);
  assert ((source_json |> member "total_chunk_count" |> to_int) = 1);
  print_endline "PASSED"

let test_check_publish_status_request_contract () =
  print_string "Test: check_publish_status request contract... ";
  reset_mock_state ();
  let seen_publish_id = ref None in
  Mock_http.set_custom_post_handler (fun url _headers body ->
    if String.ends_with ~suffix:"status/fetch/" url then (
      let body_text = Option.value ~default:"" body in
      let json = Yojson.Basic.from_string body_text in
      let open Yojson.Basic.Util in
      seen_publish_id := Some (json |> member "publish_id" |> to_string);
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"data":{"status":"PROCESSING_UPLOAD"}}|};
      }
    ) else (
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"data":{"status":"PROCESSING_UPLOAD"}}|};
      }
    ));
  let success_called = ref false in
  TikTok.check_publish_status
    ~account_id:"test_account"
    ~publish_id:"pub_req_42"
    (handle_api_result
      (fun _ -> success_called := true)
      (fun err -> failwith ("Unexpected error: " ^ err)));
  assert !success_called;
  assert (!seen_publish_id = Some "pub_req_42");
  print_endline "PASSED"

let test_upload_video_chunk_headers_contract () =
  print_string "Test: upload_video_chunk headers contract... ";
  reset_mock_state ();
  let success_called = ref false in
  let content = "video-bytes" in
  TikTok.upload_video_chunk
    ~upload_url:"https://upload.tiktok.com/video"
    ~video_content:content
    (handle_api_result
      (fun () -> success_called := true)
      (fun err -> failwith ("Unexpected error: " ^ err)));
  assert !success_called;
  let call =
    match !(Mock_http.put_calls) with
    | c :: _ -> c
    | [] -> failwith "Expected PUT upload call"
  in
  let video_size = String.length content in
  let get_header name =
    match List.assoc_opt name call.headers with
    | Some v -> v
    | None -> failwith ("Missing header: " ^ name)
  in
  assert (get_header "Content-Type" = "video/mp4");
  assert (get_header "Content-Length" = string_of_int video_size);
  assert (get_header "Content-Range" = Printf.sprintf "bytes 0-%d/%d" (video_size - 1) video_size);
  print_endline "PASSED"

let test_upload_video_chunk_empty_content_rejected () =
  print_string "Test: upload_video_chunk empty content rejected... ";
  reset_mock_state ();
  let done_ = ref false in
  TikTok.upload_video_chunk
    ~upload_url:"https://upload.tiktok.com/video"
    ~video_content:""
    (function
      | Error (Error_types.Validation_error [Error_types.Media_required]) ->
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Media_required, got: " ^ Error_types.error_to_string err)
      | Ok () -> failwith "Expected validation error");
  assert !done_

let test_upload_video_chunk_invalid_range_rejected () =
  print_string "Test: upload_video_chunk invalid range rejected... ";
  reset_mock_state ();
  let done_ = ref false in
  TikTok.upload_video_chunk
    ~upload_url:"https://upload.tiktok.com/video"
    ~video_content:"abc"
    ~start_byte:5
    ~total_size:5
    (function
      | Error (Error_types.Internal_error msg) ->
          assert (String.length msg > 0);
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Internal_error, got: " ^ Error_types.error_to_string err)
      | Ok () -> failwith "Expected range error");
  assert !done_

let test_upload_video_chunk_end_before_start_rejected () =
  print_string "Test: upload_video_chunk end<start rejected... ";
  reset_mock_state ();
  let done_ = ref false in
  TikTok.upload_video_chunk
    ~upload_url:"https://upload.tiktok.com/video"
    ~video_content:"abc"
    ~start_byte:10
    ~end_byte:5
    ~total_size:20
    (function
      | Error (Error_types.Internal_error msg) ->
          assert (String.length msg > 0);
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Internal_error, got: " ^ Error_types.error_to_string err)
      | Ok () -> failwith "Expected range error");
  assert !done_

let test_upload_video_chunk_length_range_mismatch_rejected () =
  print_string "Test: upload_video_chunk length/range mismatch rejected... ";
  reset_mock_state ();
  let done_ = ref false in
  TikTok.upload_video_chunk
    ~upload_url:"https://upload.tiktok.com/video"
    ~video_content:"abcdef"
    ~start_byte:0
    ~end_byte:2
    ~total_size:6
    (function
      | Error (Error_types.Internal_error msg) ->
          assert (String.length msg > 0);
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Internal_error, got: " ^ Error_types.error_to_string err)
      | Ok () -> failwith "Expected range mismatch error");
  assert !done_

let test_post_video_uses_multi_chunk_upload () =
  print_string "Test: post_video uses multi-chunk upload... ";
  reset_mock_state ();
  let video_content = "abcdefghij" in
  let success_called = ref false in
  TikTok.post_video
    ~account_id:"test_account"
    ~caption:"Chunked upload"
    ~video_content
    ~upload_chunk_size_bytes:4
    (handle_api_result
      (fun _publish_id -> success_called := true)
      (fun err -> failwith ("Unexpected error: " ^ err)));
  assert !success_called;

  let init_call =
    match List.find_opt (fun (c : Mock_http.post_call) -> String.ends_with ~suffix:"video/init/" c.url) !(Mock_http.post_calls) with
    | Some c -> c
    | None -> failwith "Expected init video upload call"
  in
  let init_body = Option.value ~default:"" init_call.body in
  let json = Yojson.Basic.from_string init_body in
  let open Yojson.Basic.Util in
  let source_json = json |> member "source_info" in
  assert ((source_json |> member "chunk_size" |> to_int) = 4);
  assert ((source_json |> member "total_chunk_count" |> to_int) = 3);

  let content_ranges =
    List.map
      (fun c ->
        match List.assoc_opt "Content-Range" c.Mock_http.headers with
        | Some v -> v
        | None -> failwith "Missing Content-Range header")
      (List.rev !(Mock_http.put_calls))
  in
  assert (List.length content_ranges = 3);
  assert (content_ranges = ["bytes 0-3/10"; "bytes 4-7/10"; "bytes 8-9/10"]);
  print_endline "PASSED"

let test_post_video_caption_too_long_validation_error () =
  print_string "Test: post_video caption too long validation error... ";
  reset_mock_state ();
  let long_caption = String.make 2300 'x' in
  let done_ = ref false in
  TikTok.post_video
    ~account_id:"test_account"
    ~caption:long_caption
    ~video_content:"video-bytes"
    (function
      | Error (Error_types.Validation_error [Error_types.Text_too_long { length; max }]) ->
          assert (length = 2300);
          assert (max = 2200);
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Text_too_long validation error, got: " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "Expected validation error");
  assert !done_

let test_post_video_rejects_disallowed_privacy_level () =
  print_string "Test: post_video rejects disallowed privacy level... ";
  reset_mock_state ();
  let init_called = ref false in
  Mock_http.set_custom_post_handler (fun url _headers _body ->
    if String.ends_with ~suffix:"creator_info/query/" url then
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"data":{"creator_avatar_url":"https://example.com/avatar.jpg","creator_username":"testuser","creator_nickname":"Test User","privacy_level_options":["SELF_ONLY"],"comment_disabled":false,"duet_disabled":false,"stitch_disabled":false,"max_video_post_duration_sec":600}}|};
      }
    else if String.ends_with ~suffix:"video/init/" url then (
      init_called := true;
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"data":{"publish_id":"pub_should_not_happen","upload_url":"https://upload.tiktok.com/video"}}|};
      }
    ) else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"new_access","refresh_token":"new_refresh","expires_in":86400}|};
      });
  let done_ = ref false in
  TikTok.post_video
    ~account_id:"test_account"
    ~caption:"Privacy restricted"
    ~privacy_level:Social_tiktok_v1.PublicToEveryone
    ~video_content:"video-bytes"
    (function
      | Error (Error_types.Content_policy_violation msg) ->
          assert (String.length msg > 0);
          assert (not !init_called);
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Content_policy_violation, got: " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "Expected policy error");
  assert !done_

let test_post_video_rejects_comment_enabled_when_creator_disables_comments () =
  print_string "Test: post_video rejects enabled comments policy... ";
  reset_mock_state ();
  let init_called = ref false in
  Mock_http.set_custom_post_handler (fun url _headers _body ->
    if String.ends_with ~suffix:"creator_info/query/" url then
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"data":{"creator_avatar_url":"https://example.com/avatar.jpg","creator_username":"testuser","creator_nickname":"Test User","privacy_level_options":["PUBLIC_TO_EVERYONE","SELF_ONLY"],"comment_disabled":true,"duet_disabled":false,"stitch_disabled":false,"max_video_post_duration_sec":600}}|};
      }
    else if String.ends_with ~suffix:"video/init/" url then (
      init_called := true;
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"data":{"publish_id":"pub_should_not_happen","upload_url":"https://upload.tiktok.com/video"}}|};
      }
    ) else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"new_access","refresh_token":"new_refresh","expires_in":86400}|};
      });
  let done_ = ref false in
  TikTok.post_video
    ~account_id:"test_account"
    ~caption:"Comments policy"
    ~disable_comment:false
    ~video_content:"video-bytes"
    (function
      | Error (Error_types.Content_policy_violation msg) ->
          assert (String.length msg > 0);
          assert (not !init_called);
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Content_policy_violation, got: " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "Expected policy error");
  assert !done_

let test_post_video_allows_when_creator_policies_satisfied () =
  print_string "Test: post_video allows when creator policies satisfied... ";
  reset_mock_state ();
  let init_called = ref false in
  Mock_http.set_custom_post_handler (fun url _headers _body ->
    if String.ends_with ~suffix:"creator_info/query/" url then
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"data":{"creator_avatar_url":"https://example.com/avatar.jpg","creator_username":"testuser","creator_nickname":"Test User","privacy_level_options":["PUBLIC_TO_EVERYONE","SELF_ONLY"],"comment_disabled":true,"duet_disabled":true,"stitch_disabled":true,"max_video_post_duration_sec":600}}|};
      }
    else if String.ends_with ~suffix:"video/init/" url then (
      init_called := true;
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"data":{"publish_id":"pub_policy_ok","upload_url":"https://upload.tiktok.com/video"}}|};
      }
    ) else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"new_access","refresh_token":"new_refresh","expires_in":86400}|};
      });
  let done_ = ref false in
  TikTok.post_video
    ~account_id:"test_account"
    ~caption:"Policy compliant"
    ~privacy_level:Social_tiktok_v1.PublicToEveryone
    ~disable_comment:true
    ~disable_duet:true
    ~disable_stitch:true
    ~video_content:"video-bytes"
    (handle_api_result
      (fun publish_id ->
        assert (publish_id = "pub_policy_ok");
        assert !init_called;
        done_ := true;
        print_endline "PASSED")
      (fun err -> failwith ("Unexpected error: " ^ err)));
  assert !done_

let test_post_video_from_url_propagates_content_type () =
  print_string "Test: post_video_from_url propagates content type... ";
  reset_mock_state ();
  Mock_http.set_custom_get_handler (fun _url ->
    {
      Social_core.status = 200;
      headers = [("content-type", "video/webm")];
      body = "webm-bytes";
    });
  let success_called = ref false in
  TikTok.post_video_from_url
    ~account_id:"test_account"
    ~caption:"WebM upload"
    ~video_url:"https://example.com/video.webm"
    (handle_api_result
      (fun _publish_id -> success_called := true)
      (fun err -> failwith ("Unexpected error: " ^ err)));
  assert !success_called;
  let put_call =
    match !(Mock_http.put_calls) with
    | c :: _ -> c
    | [] -> failwith "Expected upload PUT call"
  in
  let content_type =
    match List.assoc_opt "Content-Type" put_call.headers with
    | Some v -> v
    | None -> failwith "Missing Content-Type header"
  in
  assert (content_type = "video/webm");
  print_endline "PASSED"

let test_post_video_from_url_normalizes_content_type () =
  print_string "Test: post_video_from_url normalizes content type... ";
  reset_mock_state ();
  Mock_http.set_custom_get_handler (fun _url ->
    {
      Social_core.status = 200;
      headers = [("Content-Type", "video/mp4; charset=binary")];
      body = "mp4-bytes";
    });
  let success_called = ref false in
  TikTok.post_video_from_url
    ~account_id:"test_account"
    ~caption:"MP4 upload"
    ~video_url:"https://example.com/video.mp4"
    (handle_api_result
      (fun _publish_id -> success_called := true)
      (fun err -> failwith ("Unexpected error: " ^ err)));
  assert !success_called;
  let put_call =
    match !(Mock_http.put_calls) with
    | c :: _ -> c
    | [] -> failwith "Expected upload PUT call"
  in
  let content_type =
    match List.assoc_opt "Content-Type" put_call.headers with
    | Some v -> v
    | None -> failwith "Missing Content-Type header"
  in
  assert (content_type = "video/mp4");
  print_endline "PASSED"

let test_post_video_from_url_rejects_unsupported_content_type () =
  print_string "Test: post_video_from_url rejects unsupported type... ";
  reset_mock_state ();
  Mock_http.set_custom_get_handler (fun _url ->
    {
      Social_core.status = 200;
      headers = [("content-type", "image/gif")];
      body = "gif-bytes";
    });
  let done_ = ref false in
  TikTok.post_video_from_url
    ~account_id:"test_account"
    ~caption:"GIF upload"
    ~video_url:"https://example.com/video.gif"
    (function
      | Error (Error_types.Validation_error [Error_types.Media_unsupported_format "image/gif"]) ->
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Media_unsupported_format, got: " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "Expected validation error");
  assert !done_

let test_post_video_from_url_empty_content_type_defaults_mp4 () =
  print_string "Test: post_video_from_url empty type defaults mp4... ";
  reset_mock_state ();
  Mock_http.set_custom_get_handler (fun _url ->
    {
      Social_core.status = 200;
      headers = [("content-type", "")];
      body = "bytes";
    });
  let success_called = ref false in
  TikTok.post_video_from_url
    ~account_id:"test_account"
    ~caption:"Default type"
    ~video_url:"https://example.com/video"
    (handle_api_result
      (fun _ -> success_called := true)
      (fun err -> failwith ("Unexpected error: " ^ err)));
  assert !success_called;
  let put_call =
    match !(Mock_http.put_calls) with
    | c :: _ -> c
    | [] -> failwith "Expected upload PUT call"
  in
  let content_type =
    match List.assoc_opt "Content-Type" put_call.headers with
    | Some v -> v
    | None -> failwith "Missing Content-Type header"
  in
  assert (content_type = "video/mp4");
  print_endline "PASSED"

let test_post_video_from_url_accepts_x_quicktime () =
  print_string "Test: post_video_from_url accepts x-quicktime... ";
  reset_mock_state ();
  Mock_http.set_custom_get_handler (fun _url ->
    {
      Social_core.status = 200;
      headers = [("content-type", "video/x-quicktime")];
      body = "mov-bytes";
    });
  let success_called = ref false in
  TikTok.post_video_from_url
    ~account_id:"test_account"
    ~caption:"QuickTime upload"
    ~video_url:"https://example.com/video.mov"
    (handle_api_result
      (fun _ -> success_called := true)
      (fun err -> failwith ("Unexpected error: " ^ err)));
  assert !success_called;
  let put_call =
    match !(Mock_http.put_calls) with
    | c :: _ -> c
    | [] -> failwith "Expected upload PUT call"
  in
  let content_type =
    match List.assoc_opt "Content-Type" put_call.headers with
    | Some v -> v
    | None -> failwith "Missing Content-Type header"
  in
  assert (content_type = "video/x-quicktime");
  print_endline "PASSED"

let test_post_video_from_url_invalid_url_validation () =
  print_string "Test: post_video_from_url invalid URL validation... ";
  reset_mock_state ();
  let done_ = ref false in
  TikTok.post_video_from_url
    ~account_id:"test_account"
    ~caption:"Invalid URL"
    ~video_url:"not-a-url"
    (function
      | Error (Error_types.Validation_error [Error_types.Invalid_url "not-a-url"]) ->
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Invalid_url, got: " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "Expected validation error");
  assert !done_

let test_post_video_from_url_download_404_maps_resource_not_found () =
  print_string "Test: post_video_from_url download 404 maps not_found... ";
  reset_mock_state ();
  Mock_http.set_custom_get_handler (fun _url ->
    {
      Social_core.status = 404;
      headers = [];
      body = "";
    });
  let done_ = ref false in
  TikTok.post_video_from_url
    ~account_id:"test_account"
    ~caption:"Not found"
    ~video_url:"https://example.com/missing.mp4"
    (function
      | Error (Error_types.Resource_not_found url) ->
          assert (url = "https://example.com/missing.mp4");
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Resource_not_found, got: " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "Expected not found error");
  assert !done_

let test_post_video_from_url_download_500_maps_network_error () =
  print_string "Test: post_video_from_url download 500 maps network error... ";
  reset_mock_state ();
  Mock_http.set_custom_get_handler (fun _url ->
    {
      Social_core.status = 500;
      headers = [];
      body = "";
    });
  let done_ = ref false in
  TikTok.post_video_from_url
    ~account_id:"test_account"
    ~caption:"Server error"
    ~video_url:"https://example.com/error.mp4"
    (function
      | Error (Error_types.Network_error (Error_types.Connection_failed _)) ->
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Network_error, got: " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "Expected network error");
  assert !done_

let test_post_video_from_url_download_transport_error_maps_network_error () =
  print_string "Test: post_video_from_url transport error maps network error... ";
  reset_mock_state ();
  Mock_http.set_custom_get_error_handler (fun _url -> "connection reset by peer");
  let done_ = ref false in
  TikTok.post_video_from_url
    ~account_id:"test_account"
    ~caption:"Transport error"
    ~video_url:"https://example.com/error.mp4"
    (function
      | Error (Error_types.Network_error (Error_types.Connection_failed msg)) ->
          assert (String.length msg > 0);
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Network_error, got: " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "Expected network error");
  assert !done_

let test_get_creator_info_429_maps_rate_limited () =
  print_string "Test: get_creator_info 429 maps rate-limited... ";
  reset_mock_state ();
  Mock_http.set_custom_post_handler (fun url _headers _body ->
    if String.ends_with ~suffix:"creator_info/query/" url then
      {
        Social_core.status = 429;
        headers = [("retry-after", "17")];
        body = {|{"error":{"code":"rate_limited","message":"Too many requests"}}|};
      }
    else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"new_access","refresh_token":"new_refresh","expires_in":86400}|};
      });
  let done_ = ref false in
  TikTok.get_creator_info ~account_id:"test_account"
    (function
      | Error (Error_types.Rate_limited info) ->
          assert (info.retry_after_seconds = Some 17);
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Rate_limited, got: " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "Expected error");
  assert !done_

let test_get_creator_info_429_invalid_json_still_rate_limited () =
  print_string "Test: get_creator_info 429 invalid json still rate-limited... ";
  reset_mock_state ();
  Mock_http.set_custom_post_handler (fun url _headers _body ->
    if String.ends_with ~suffix:"creator_info/query/" url then
      {
        Social_core.status = 429;
        headers = [("retry-after", "9")];
        body = "not-json";
      }
    else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"new_access","refresh_token":"new_refresh","expires_in":86400}|};
      });
  let done_ = ref false in
  TikTok.get_creator_info ~account_id:"test_account"
    (function
      | Error (Error_types.Rate_limited info) ->
          assert (info.retry_after_seconds = Some 9);
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Rate_limited, got: " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "Expected error");
  assert !done_

let test_get_creator_info_401_expired_maps_token_expired () =
  print_string "Test: get_creator_info 401 expired maps token_expired... ";
  reset_mock_state ();
  Mock_http.set_custom_post_handler (fun url _headers _body ->
    if String.ends_with ~suffix:"creator_info/query/" url then
      {
        Social_core.status = 401;
        headers = [];
        body = {|{"error":{"code":"access_token_expired","message":"Access token expired"}}|};
      }
    else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"new_access","refresh_token":"new_refresh","expires_in":86400}|};
      });
  let done_ = ref false in
  TikTok.get_creator_info ~account_id:"test_account"
    (function
      | Error (Error_types.Auth_error Error_types.Token_expired) ->
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Token_expired, got: " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "Expected auth error");
  assert !done_

let test_get_creator_info_404_maps_resource_not_found () =
  print_string "Test: get_creator_info 404 maps resource_not_found... ";
  reset_mock_state ();
  Mock_http.set_custom_post_handler (fun url _headers _body ->
    if String.ends_with ~suffix:"creator_info/query/" url then
      {
        Social_core.status = 404;
        headers = [];
        body = {|{"error":{"message":"Not found"}}|};
      }
    else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"new_access","refresh_token":"new_refresh","expires_in":86400}|};
      });
  let done_ = ref false in
  TikTok.get_creator_info ~account_id:"test_account"
    (function
      | Error (Error_types.Resource_not_found msg) ->
          assert (String.length msg > 0);
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Resource_not_found, got: " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "Expected not-found error");
  assert !done_

let test_get_creator_info_500_request_id_and_top_level_error () =
  print_string "Test: get_creator_info 500 captures request id... ";
  reset_mock_state ();
  Mock_http.set_custom_post_handler (fun url _headers _body ->
    if String.ends_with ~suffix:"creator_info/query/" url then
      {
        Social_core.status = 500;
        headers = [("x-tt-logid", "log-abc-123")];
        body = {|{"code":"server_error","message":"Internal failure"}|};
      }
    else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"new_access","refresh_token":"new_refresh","expires_in":86400}|};
      });
  let done_ = ref false in
  TikTok.get_creator_info ~account_id:"test_account"
    (function
      | Error (Error_types.Api_error api_err) ->
          assert (api_err.request_id = Some "log-abc-123");
          assert (api_err.message = "server_error: Internal failure");
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Api_error, got: " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "Expected error");
  assert !done_

let test_get_creator_info_500_numeric_error_code () =
  print_string "Test: get_creator_info 500 numeric error code... ";
  reset_mock_state ();
  Mock_http.set_custom_post_handler (fun url _headers _body ->
    if String.ends_with ~suffix:"creator_info/query/" url then
      {
        Social_core.status = 500;
        headers = [("x-request-id", "req-42")];
        body = {|{"error":{"code":1001,"message":"Numeric code"}}|};
      }
    else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"new_access","refresh_token":"new_refresh","expires_in":86400}|};
      });
  let done_ = ref false in
  TikTok.get_creator_info ~account_id:"test_account"
    (function
      | Error (Error_types.Api_error api_err) ->
          assert (api_err.request_id = Some "req-42");
          assert (api_err.message = "1001: Numeric code");
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Api_error, got: " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "Expected error");
  assert !done_

let test_get_creator_info_403_scope_mapping () =
  print_string "Test: get_creator_info 403 scope mapping... ";
  reset_mock_state ();
  Mock_http.set_custom_post_handler (fun url _headers _body ->
    if String.ends_with ~suffix:"creator_info/query/" url then
      {
        Social_core.status = 403;
        headers = [];
        body = {|{"error":{"code":"forbidden","message":"Missing scope"}}|};
      }
    else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"new_access","refresh_token":"new_refresh","expires_in":86400}|};
      });
  let done_ = ref false in
  TikTok.get_creator_info ~account_id:"test_account"
    (function
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (scopes = ["user.info.basic"]);
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Insufficient_permissions, got: " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "Expected permission error");
  assert !done_

let test_init_video_upload_403_scope_mapping () =
  print_string "Test: init_video_upload 403 scope mapping... ";
  reset_mock_state ();
  let post_info = Social_tiktok_v1.make_post_info ~title:"Scope test" () in
  Mock_http.set_custom_post_handler (fun url _headers _body ->
    if String.ends_with ~suffix:"video/init/" url then
      {
        Social_core.status = 403;
        headers = [];
        body = {|{"error":{"code":"forbidden","message":"Missing publish scope"}}|};
      }
    else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"new_access","refresh_token":"new_refresh","expires_in":86400}|};
      });
  let done_ = ref false in
  TikTok.init_video_upload
    ~account_id:"test_account"
    ~post_info
    ~video_size:123
    (function
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (scopes = ["video.publish"]);
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Insufficient_permissions, got: " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "Expected permission error");
  assert !done_

let test_check_publish_status_403_scope_mapping () =
  print_string "Test: check_publish_status 403 scope mapping... ";
  reset_mock_state ();
  Mock_http.set_custom_post_handler (fun url _headers _body ->
    if String.ends_with ~suffix:"status/fetch/" url then
      {
        Social_core.status = 403;
        headers = [];
        body = {|{"error":{"code":"forbidden","message":"Missing publish scope"}}|};
      }
    else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"new_access","refresh_token":"new_refresh","expires_in":86400}|};
      });
  let done_ = ref false in
  TikTok.check_publish_status
    ~account_id:"test_account"
    ~publish_id:"pub_scope"
    (function
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (scopes = ["video.publish"]);
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Insufficient_permissions, got: " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "Expected permission error");
  assert !done_

let test_upload_video_chunk_403_scope_mapping () =
  print_string "Test: upload_video_chunk 403 scope mapping... ";
  reset_mock_state ();
  Mock_http.set_custom_put_handler (fun _url _headers _body ->
    {
      Social_core.status = 403;
      headers = [];
      body = {|{"error":{"code":"forbidden","message":"Missing upload scope"}}|};
    });
  let done_ = ref false in
  TikTok.upload_video_chunk
    ~upload_url:"https://upload.tiktok.com/video"
    ~video_content:"bytes"
    (function
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (scopes = ["video.upload"]);
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Insufficient_permissions, got: " ^ Error_types.error_to_string err)
      | Ok () -> failwith "Expected permission error");
  assert !done_

let test_get_creator_info_invalid_json_returns_error () =
  print_string "Test: get_creator_info invalid json returns error... ";
  reset_mock_state ();
  Mock_http.set_custom_post_handler (fun url _headers _body ->
    if String.ends_with ~suffix:"creator_info/query/" url then
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = "{not valid json";
      }
    else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"new_access","refresh_token":"new_refresh","expires_in":86400}|};
      });
  let done_ = ref false in
  TikTok.get_creator_info ~account_id:"test_account"
    (function
      | Error (Error_types.Internal_error msg) ->
          assert (String.length msg > 0);
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Internal_error, got: " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "Expected parse error");
  assert !done_

let test_check_publish_status_invalid_json_returns_error () =
  print_string "Test: check_publish_status invalid json returns error... ";
  reset_mock_state ();
  Mock_http.set_custom_post_handler (fun url _headers _body ->
    if String.ends_with ~suffix:"status/fetch/" url then
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = "{bad json";
      }
    else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"new_access","refresh_token":"new_refresh","expires_in":86400}|};
      });
  let done_ = ref false in
  TikTok.check_publish_status
    ~account_id:"test_account"
    ~publish_id:"pub_bad_json"
    (function
      | Error (Error_types.Internal_error msg) ->
          assert (String.length msg > 0);
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Internal_error, got: " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "Expected parse error");
  assert !done_

let test_wait_for_publish_processing_then_published () =
  print_string "Test: wait_for_publish processing then published... ";
  reset_mock_state ();
  let status_calls = ref 0 in
  Mock_http.set_custom_post_handler (fun url _headers _body ->
    if String.ends_with ~suffix:"status/fetch/" url then (
      status_calls := !status_calls + 1;
      if !status_calls < 3 then
        {
          Social_core.status = 200;
          headers = [("content-type", "application/json")];
          body = {|{"data":{"status":"PROCESSING_UPLOAD"}}|};
        }
      else
        {
          Social_core.status = 200;
          headers = [("content-type", "application/json")];
          body = {|{"data":{"status":"PUBLISH_COMPLETE","publicaly_available_post_id":[{"id":"video_done"}]}}|};
        }
    ) else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"new_access","refresh_token":"new_refresh","expires_in":86400}|};
      });
  let done_ = ref false in
  TikTok.wait_for_publish
    ~account_id:"test_account"
    ~publish_id:"pub_wait"
    ~max_attempts:5
    (handle_api_result
      (function
        | Social_tiktok_v1.Published id ->
            assert (id = "video_done");
            assert (!status_calls = 3);
            done_ := true;
            print_endline "PASSED"
        | _ -> failwith "Expected Published status")
      (fun err -> failwith ("Unexpected error: " ^ err)));
  assert !done_

let test_wait_for_publish_timeout () =
  print_string "Test: wait_for_publish timeout... ";
  reset_mock_state ();
  Mock_http.set_custom_post_handler (fun url _headers _body ->
    if String.ends_with ~suffix:"status/fetch/" url then
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"data":{"status":"PROCESSING_UPLOAD"}}|};
      }
    else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"new_access","refresh_token":"new_refresh","expires_in":86400}|};
      });
  let done_ = ref false in
  TikTok.wait_for_publish
    ~account_id:"test_account"
    ~publish_id:"pub_timeout"
    ~max_attempts:2
    (function
      | Error (Error_types.Internal_error msg) ->
          assert (String.length msg > 0);
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected timeout Internal_error, got: " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "Expected timeout error");
  assert !done_

let test_wait_for_publish_on_processing_callback () =
  print_string "Test: wait_for_publish on_processing callback... ";
  reset_mock_state ();
  let status_calls = ref 0 in
  let processing_calls = ref 0 in
  Mock_http.set_custom_post_handler (fun url _headers _body ->
    if String.ends_with ~suffix:"status/fetch/" url then (
      status_calls := !status_calls + 1;
      if !status_calls < 2 then
        {
          Social_core.status = 200;
          headers = [("content-type", "application/json")];
          body = {|{"data":{"status":"PROCESSING_UPLOAD"}}|};
        }
      else
        {
          Social_core.status = 200;
          headers = [("content-type", "application/json")];
          body = {|{"data":{"status":"PUBLISH_COMPLETE","publicaly_available_post_id":[{"id":"video_done_2"}]}}|};
        }
    ) else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"new_access","refresh_token":"new_refresh","expires_in":86400}|};
      });
  let done_ = ref false in
  TikTok.wait_for_publish
    ~account_id:"test_account"
    ~publish_id:"pub_wait_callback"
    ~max_attempts:3
    ~on_processing:(fun ~attempt:_ ~max_attempts:_ ->
      processing_calls := !processing_calls + 1)
    (handle_api_result
      (function
        | Social_tiktok_v1.Published id ->
            assert (id = "video_done_2");
            assert (!processing_calls = 1);
            done_ := true;
            print_endline "PASSED"
        | _ -> failwith "Expected Published status")
      (fun err -> failwith ("Unexpected error: " ^ err)));
  assert !done_

let test_wait_for_publish_invalid_max_attempts () =
  print_string "Test: wait_for_publish invalid max_attempts... ";
  reset_mock_state ();
  let done_ = ref false in
  TikTok.wait_for_publish
    ~account_id:"test_account"
    ~publish_id:"pub_invalid_max"
    ~max_attempts:0
    (function
      | Error (Error_types.Internal_error msg) ->
          assert (String.length msg > 0);
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Internal_error, got: " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "Expected configuration error");
  assert !done_

let test_exchange_code_without_refresh_token () =
  print_string "Test: exchange_code without refresh token... ";
  reset_mock_state ();
  Mock_http.set_custom_post_handler (fun url _headers _body ->
    if String.ends_with ~suffix:"oauth/token/" url then
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"token_only","expires_in":86400}|};
      }
    else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = "{}";
      });
  let done_ = ref false in
  TikTok.exchange_code
    ~code:"auth_code_123"
    ~redirect_uri:"https://example.com/callback"
    (handle_api_result
      (fun creds ->
        assert (creds.Social_core.access_token = "token_only");
        assert (creds.refresh_token = None);
        done_ := true;
        print_endline "PASSED")
      (fun err -> failwith ("Unexpected error: " ^ err)));
  assert !done_

let test_exchange_code_without_expires_in_uses_default () =
  print_string "Test: exchange_code without expires_in uses default... ";
  reset_mock_state ();
  Mock_http.set_custom_post_handler (fun url _headers _body ->
    if String.ends_with ~suffix:"oauth/token/" url then
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"token_default_exp"}|};
      }
    else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = "{}";
      });
  let done_ = ref false in
  TikTok.exchange_code
    ~code:"auth_code_123"
    ~redirect_uri:"https://example.com/callback"
    (handle_api_result
      (fun creds ->
        assert (creds.Social_core.access_token = "token_default_exp");
        assert (creds.expires_at <> None);
        done_ := true;
        print_endline "PASSED")
      (fun err -> failwith ("Unexpected error: " ^ err)));
  assert !done_

let test_exchange_code_403_scope_mapping () =
  print_string "Test: exchange_code 403 scope mapping... ";
  reset_mock_state ();
  Mock_http.set_custom_post_handler (fun url _headers _body ->
    if String.ends_with ~suffix:"oauth/token/" url then
      {
        Social_core.status = 403;
        headers = [];
        body = {|{"error":{"code":"forbidden","message":"scope missing"}}|};
      }
    else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = "{}";
      });
  let done_ = ref false in
  TikTok.exchange_code
    ~code:"auth_code_123"
    ~redirect_uri:"https://example.com/callback"
    (function
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (scopes = ["user.info.basic"; "video.publish"]);
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Insufficient_permissions, got: " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "Expected permission error");
  assert !done_

let test_refresh_without_new_refresh_token_keeps_old () =
  print_string "Test: refresh without new refresh token keeps old... ";
  reset_mock_state ();
  let now = Ptime_clock.now () in
  let expiring_soon =
    match Ptime.add_span now (Ptime.Span.of_int_s 5) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> Ptime.to_rfc3339 now
  in
  Mock_config.set_credentials {
    Social_core.access_token = "old_access";
    refresh_token = Some "persist_refresh";
    expires_at = Some expiring_soon;
    token_type = "Bearer";
  };
  Mock_http.set_custom_post_handler (fun url _headers _body ->
    if String.ends_with ~suffix:"oauth/token/" url then
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"new_access_only","expires_in":86400}|};
      }
    else if String.ends_with ~suffix:"creator_info/query/" url then
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"data":{"creator_avatar_url":"https://example.com/avatar.jpg","creator_username":"testuser","creator_nickname":"Test User","privacy_level_options":["PUBLIC_TO_EVERYONE","SELF_ONLY"],"comment_disabled":false,"duet_disabled":false,"stitch_disabled":false,"max_video_post_duration_sec":600}}|};
      }
    else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = "{}";
      });
  let done_ = ref false in
  TikTok.get_creator_info
    ~account_id:"test_account"
    (handle_api_result
      (fun _ ->
        assert (!Mock_config.current_credentials.Social_core.access_token = "new_access_only");
        assert (!Mock_config.current_credentials.refresh_token = Some "persist_refresh");
        done_ := true;
        print_endline "PASSED")
      (fun err -> failwith ("Unexpected error: " ^ err)));
  assert !done_

let test_refresh_without_expires_in_uses_default () =
  print_string "Test: refresh without expires_in uses default... ";
  reset_mock_state ();
  let now = Ptime_clock.now () in
  let expiring_soon =
    match Ptime.add_span now (Ptime.Span.of_int_s 5) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> Ptime.to_rfc3339 now
  in
  Mock_config.set_credentials {
    Social_core.access_token = "old_access";
    refresh_token = Some "refresh_for_default";
    expires_at = Some expiring_soon;
    token_type = "Bearer";
  };
  Mock_http.set_custom_post_handler (fun url _headers _body ->
    if String.ends_with ~suffix:"oauth/token/" url then
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"new_access_default_exp"}|};
      }
    else if String.ends_with ~suffix:"creator_info/query/" url then
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"data":{"creator_avatar_url":"https://example.com/avatar.jpg","creator_username":"testuser","creator_nickname":"Test User","privacy_level_options":["PUBLIC_TO_EVERYONE","SELF_ONLY"],"comment_disabled":false,"duet_disabled":false,"stitch_disabled":false,"max_video_post_duration_sec":600}}|};
      }
    else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = "{}";
      });
  let done_ = ref false in
  TikTok.get_creator_info
    ~account_id:"test_account"
    (handle_api_result
      (fun _ ->
        assert (!Mock_config.current_credentials.Social_core.access_token = "new_access_default_exp");
        assert (!Mock_config.current_credentials.expires_at <> None);
        done_ := true;
        print_endline "PASSED")
      (fun err -> failwith ("Unexpected error: " ^ err)));
  assert !done_

let test_post_single_multiple_media_rejected () =
  print_string "Test: post_single rejects multiple media... ";
  reset_mock_state ();
  let error_received = ref false in
  TikTok.post_single
    ~account_id:"test_account"
    ~text:"Test caption"
    ~media_urls:["https://example.com/video1.mp4"; "https://example.com/video2.mp4"]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Validation_error errs) ->
          let has_too_many_media =
            List.exists
              (function
                | Error_types.Too_many_media { count; max } -> count = 2 && max = 1
                | _ -> false)
              errs
          in
          if has_too_many_media then (
            error_received := true;
            print_endline "PASSED"
          ) else
            failwith "Expected Too_many_media validation error"
      | _ -> failwith "Expected validation error");
  assert !error_received

(** {1 Video Upload Tests} *)

(** Test: Video validation - resolution too low *)
let test_validate_video_resolution_too_low () =
  print_string "Test: validate_video resolution too low... ";
  match Social_tiktok_v1.validate_video ~duration_sec:30 ~file_size_bytes:10_000_000 ~width:240 ~height:240 with
  | Error msg when String.length msg > 0 -> print_endline "PASSED"
  | Error _ -> print_endline "PASSED"
  | Ok () -> failwith "Expected Error for low resolution video"

(** Test: Video validation - resolution too high *)
let test_validate_video_resolution_too_high () =
  print_string "Test: validate_video resolution too high... ";
  match Social_tiktok_v1.validate_video ~duration_sec:30 ~file_size_bytes:10_000_000 ~width:8000 ~height:8000 with
  | Error msg when String.length msg > 0 -> print_endline "PASSED"
  | Error _ -> print_endline "PASSED"
  | Ok () -> failwith "Expected Error for high resolution video"

(** Test: Video validation - duration too long *)
let test_validate_video_duration_too_long () =
  print_string "Test: validate_video duration too long... ";
  match Social_tiktok_v1.validate_video ~duration_sec:700 ~file_size_bytes:10_000_000 ~width:1080 ~height:1920 with
  | Error msg when String.length msg > 0 -> print_endline "PASSED"
  | Error _ -> print_endline "PASSED"
  | Ok () -> failwith "Expected Error for too long video"

(** Test: Video at exact limits *)
let test_validate_video_at_limits () =
  print_string "Test: validate_video at exact limits... ";
  (* Exactly at max duration (600s), max size (4GB), max resolution (4096) *)
  match Social_tiktok_v1.validate_video
    ~duration_sec:600
    ~file_size_bytes:4_294_967_296
    ~width:4096
    ~height:2160 with
  | Ok () -> print_endline "PASSED"
  | Error msg -> failwith ("Expected Ok at limits, got Error: " ^ msg)

(** Test: Caption validation - valid caption *)
let test_validate_caption_valid () =
  print_string "Test: validate_caption valid... ";
  match Social_tiktok_v1.validate_caption "Check out my video! #tiktok #viral" with
  | Ok () -> print_endline "PASSED"
  | Error msg -> failwith ("Expected Ok, got Error: " ^ msg)

(** Test: Caption validation - too long *)
let test_validate_caption_too_long () =
  print_string "Test: validate_caption too long... ";
  let long_caption = String.make 2300 'a' in
  match Social_tiktok_v1.validate_caption long_caption with
  | Error msg when String.length msg > 0 -> print_endline "PASSED"
  | Error _ -> print_endline "PASSED"
  | Ok () -> failwith "Expected Error for too long caption"

(** Test: Post info creation with defaults *)
let test_make_post_info_defaults () =
  print_string "Test: make_post_info with defaults... ";
  let info = Social_tiktok_v1.make_post_info ~title:"Test video" () in
  assert (info.title = "Test video");
  assert (info.privacy_level = Social_tiktok_v1.SelfOnly);  (* Default is SelfOnly *)
  assert (info.disable_duet = false);
  assert (info.disable_comment = false);
  assert (info.disable_stitch = false);
  assert (info.video_cover_timestamp_ms = None);
  print_endline "PASSED"

(** Test: Post info creation with all options *)
let test_make_post_info_all_options () =
  print_string "Test: make_post_info with all options... ";
  let info = Social_tiktok_v1.make_post_info 
    ~title:"My viral video"
    ~privacy_level:Social_tiktok_v1.PublicToEveryone
    ~disable_duet:true
    ~disable_comment:true
    ~disable_stitch:true
    ~video_cover_timestamp_ms:5000
    () in
  assert (info.title = "My viral video");
  assert (info.privacy_level = Social_tiktok_v1.PublicToEveryone);
  assert (info.disable_duet = true);
  assert (info.disable_comment = true);
  assert (info.disable_stitch = true);
  assert (info.video_cover_timestamp_ms = Some 5000);
  print_endline "PASSED"

(** Test: Post info to JSON serialization *)
let test_post_info_to_json () =
  print_string "Test: post_info_to_json... ";
  let info = Social_tiktok_v1.make_post_info 
    ~title:"Test"
    ~privacy_level:Social_tiktok_v1.MutualFollowFriends
    () in
  let json = Social_tiktok_v1.post_info_to_json info in
  let json_str = Yojson.Basic.to_string json in
  assert (String.length json_str > 0);
  assert (try ignore (String.index json_str '{'); true with Not_found -> false);
  print_endline "PASSED"

(** Test: Privacy level string roundtrip *)
let test_all_privacy_levels () =
  print_string "Test: all privacy levels... ";
  let levels = [
    Social_tiktok_v1.PublicToEveryone;
    Social_tiktok_v1.MutualFollowFriends;
    Social_tiktok_v1.SelfOnly
  ] in
  List.iter (fun level ->
    let s = Social_tiktok_v1.string_of_privacy_level level in
    let level2 = Social_tiktok_v1.privacy_level_of_string s in
    assert (level = level2)
  ) levels;
  print_endline "PASSED"

(** Test: Init video upload request *)
let test_init_video_upload () =
  print_string "Test: init_video_upload... ";
  let success_called = ref false in
  let post_info = Social_tiktok_v1.make_post_info 
    ~title:"Test upload" 
    ~privacy_level:Social_tiktok_v1.SelfOnly
    () in
  TikTok.init_video_upload
    ~account_id:"test_account"
    ~post_info
    ~video_size:1000000
    (handle_api_result
      (fun (publish_id, upload_url) ->
        assert (publish_id = "pub_123");
        assert (String.length upload_url > 0);
        success_called := true;
        print_endline "PASSED")
      (fun err ->
        failwith ("Unexpected error: " ^ err)));
  assert !success_called

(** Test: post_info privacy level options *)
let test_post_info_privacy_options () =
  print_string "Test: post_info with all privacy options... ";
  
  (* Test each privacy level creates valid post_info *)
  let levels = [
    Social_tiktok_v1.PublicToEveryone;
    Social_tiktok_v1.MutualFollowFriends;
    Social_tiktok_v1.SelfOnly;
  ] in
  
  List.iter (fun privacy ->
    let info = Social_tiktok_v1.make_post_info ~title:"Test" ~privacy_level:privacy () in
    assert (info.privacy_level = privacy);
    (* Verify JSON serialization includes correct privacy level *)
    let json = Social_tiktok_v1.post_info_to_json info in
    let json_str = Yojson.Basic.to_string json in
    let expected_str = Social_tiktok_v1.string_of_privacy_level privacy in
    assert (try ignore (Str.search_forward (Str.regexp_string expected_str) json_str 0); true with Not_found -> false)
  ) levels;
  
  print_endline "PASSED"

(** Test: Thread validation - missing media *)
let test_thread_validation_missing_media () =
  print_string "Test: thread validation - missing media... ";
  let error_received = ref false in
  TikTok.post_thread
    ~account_id:"test_account"
    ~texts:["First post"; "Second post"]
    ~media_urls_per_post:[["https://example.com/video1.mp4"]; []]  (* Second post missing media *)
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Validation_error _) ->
          error_received := true;
          print_endline "PASSED"
      | Error_types.Partial_success _ ->
          (* Partial success could also happen if first post succeeded but second validation failed *)
          error_received := true;
          print_endline "PASSED"
      | _ ->
          failwith "Expected validation error for missing media");
  assert !error_received

let test_thread_validation_invalid_media_url () =
  print_string "Test: thread validation - invalid media URL... ";
  reset_mock_state ();
  let error_received = ref false in
  TikTok.post_thread
    ~account_id:"test_account"
    ~texts:["First post"; "Second post"]
    ~media_urls_per_post:[[","]; ["https://example.com/video2.mp4"]]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Validation_error errs) ->
          let has_invalid_url =
            List.exists
              (function
                | Error_types.Thread_post_invalid { index = 0; errors } ->
                    List.exists (function Error_types.Invalid_url _ -> true | _ -> false) errors
                | _ -> false)
              errs
          in
          if has_invalid_url then (
            error_received := true;
            print_endline "PASSED"
          ) else
            failwith "Expected Thread_post_invalid with Invalid_url"
      | _ -> failwith "Expected validation error");
  assert !error_received

(** Test: Content validation function *)
let test_validate_content () =
  print_string "Test: validate_content... ";
  (* Valid content *)
  (match TikTok.validate_content ~text:"Valid caption #tiktok" with
   | Ok () -> ()
   | Error msg -> failwith ("Valid content should pass: " ^ msg));
  
  (* Too long content *)
  let long_text = String.make 2500 'x' in
  (match TikTok.validate_content ~text:long_text with
   | Error _ -> ()  (* Expected *)
   | Ok () -> failwith "Long content should fail");
  
  print_endline "PASSED"

(** {1 Photo Post Tests} *)

let test_make_photo_post_info_defaults () =
  print_string "Test: make_photo_post_info with defaults... ";
  let info = Social_tiktok_v1.make_photo_post_info
    ~title:"Photo post"
    ~photo_images:["https://example.com/photo1.jpg"]
    () in
  assert (info.title = "Photo post");
  assert (info.privacy_level = Social_tiktok_v1.SelfOnly);
  assert (info.disable_comment = false);
  assert (info.is_aigc = false);
  assert (info.photo_images = ["https://example.com/photo1.jpg"]);
  assert (info.photo_cover_index = 0);
  assert (info.post_mode = Social_tiktok_v1.Direct_post);
  print_endline "PASSED"

let test_make_photo_post_info_all_options () =
  print_string "Test: make_photo_post_info with all options... ";
  let info = Social_tiktok_v1.make_photo_post_info
    ~title:"My photo carousel"
    ~photo_images:["https://example.com/photo1.jpg"; "https://example.com/photo2.jpg"]
    ~privacy_level:Social_tiktok_v1.PublicToEveryone
    ~disable_comment:true
    ~is_aigc:true
    ~photo_cover_index:1
    ~post_mode:Social_tiktok_v1.Media_upload
    () in
  assert (info.title = "My photo carousel");
  assert (info.privacy_level = Social_tiktok_v1.PublicToEveryone);
  assert (info.disable_comment = true);
  assert (info.is_aigc = true);
  assert (List.length info.photo_images = 2);
  assert (info.photo_cover_index = 1);
  assert (info.post_mode = Social_tiktok_v1.Media_upload);
  print_endline "PASSED"

let test_photo_post_info_to_json () =
  print_string "Test: photo_post_info_to_json... ";
  let info = Social_tiktok_v1.make_photo_post_info
    ~title:"Photo test"
    ~photo_images:["https://example.com/img1.jpg"; "https://example.com/img2.jpg"]
    ~privacy_level:Social_tiktok_v1.SelfOnly
    () in
  let json = Social_tiktok_v1.photo_post_info_to_json info in
  let json_str = Yojson.Basic.to_string json in
  let open Yojson.Basic.Util in
  let parsed = Yojson.Basic.from_string json_str in
  assert ((parsed |> member "media_type" |> to_string) = "PHOTO");
  assert ((parsed |> member "post_mode" |> to_string) = "DIRECT_POST");
  assert ((parsed |> member "source_info" |> member "source" |> to_string) = "PULL_FROM_URL");
  let images = parsed |> member "source_info" |> member "photo_images" |> to_list |> List.map to_string in
  assert (images = ["https://example.com/img1.jpg"; "https://example.com/img2.jpg"]);
  assert ((parsed |> member "source_info" |> member "photo_cover_index" |> to_int) = 0);
  assert ((parsed |> member "post_info" |> member "title" |> to_string) = "Photo test");
  assert ((parsed |> member "post_info" |> member "privacy_level" |> to_string) = "SELF_ONLY");
  print_endline "PASSED"

let test_init_photo_post_request_contract () =
  print_string "Test: init_photo_post request contract... ";
  reset_mock_state ();
  Mock_http.set_custom_post_handler (fun url _headers body ->
    if String.ends_with ~suffix:"content/init/" url then (
      let body_text = Option.value ~default:"" body in
      let json = Yojson.Basic.from_string body_text in
      let open Yojson.Basic.Util in
      assert ((json |> member "media_type" |> to_string) = "PHOTO");
      assert ((json |> member "post_mode" |> to_string) = "DIRECT_POST");
      assert ((json |> member "source_info" |> member "source" |> to_string) = "PULL_FROM_URL");
      assert ((json |> member "source_info" |> member "photo_cover_index" |> to_int) = 0);
      let images = json |> member "source_info" |> member "photo_images" |> to_list |> List.map to_string in
      assert (images = ["https://example.com/photo1.jpg"]);
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"data":{"publish_id":"photo_pub_123"}}|};
      }
    ) else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"new_access","refresh_token":"new_refresh","expires_in":86400}|};
      });
  let photo_info = Social_tiktok_v1.make_photo_post_info
    ~title:"Photo post"
    ~photo_images:["https://example.com/photo1.jpg"]
    () in
  let success_called = ref false in
  TikTok.init_photo_post
    ~account_id:"test_account"
    ~photo_post_info:photo_info
    (handle_api_result
      (fun publish_id ->
        assert (publish_id = "photo_pub_123");
        success_called := true;
        print_endline "PASSED")
      (fun err -> failwith ("Unexpected error: " ^ err)));
  assert !success_called

let test_init_photo_post_auth_header () =
  print_string "Test: init_photo_post includes auth header... ";
  reset_mock_state ();
  Mock_http.set_custom_post_handler (fun url headers _body ->
    if String.ends_with ~suffix:"content/init/" url then (
      let auth_present =
        List.exists (fun (k, v) -> k = "Authorization" && v = "Bearer test_access_token") headers
      in
      assert auth_present;
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"data":{"publish_id":"photo_auth_ok"}}|};
      }
    ) else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"new_access","refresh_token":"new_refresh","expires_in":86400}|};
      });
  let photo_info = Social_tiktok_v1.make_photo_post_info
    ~title:"Auth check"
    ~photo_images:["https://example.com/photo1.jpg"]
    () in
  let success_called = ref false in
  TikTok.init_photo_post
    ~account_id:"test_account"
    ~photo_post_info:photo_info
    (handle_api_result
      (fun publish_id ->
        assert (publish_id = "photo_auth_ok");
        success_called := true;
        print_endline "PASSED")
      (fun err -> failwith ("Unexpected error: " ^ err)));
  assert !success_called

let test_init_photo_post_media_upload_mode () =
  print_string "Test: init_photo_post media_upload mode... ";
  reset_mock_state ();
  Mock_http.set_custom_post_handler (fun url _headers body ->
    if String.ends_with ~suffix:"content/init/" url then (
      let body_text = Option.value ~default:"" body in
      let json = Yojson.Basic.from_string body_text in
      let open Yojson.Basic.Util in
      assert ((json |> member "post_mode" |> to_string) = "MEDIA_UPLOAD");
      assert ((json |> member "media_type" |> to_string) = "PHOTO");
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"data":{"publish_id":"photo_draft_123"}}|};
      }
    ) else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"new_access","refresh_token":"new_refresh","expires_in":86400}|};
      });
  let photo_info = Social_tiktok_v1.make_photo_post_info
    ~title:"Draft photo"
    ~photo_images:["https://example.com/photo1.jpg"]
    ~post_mode:Social_tiktok_v1.Media_upload
    () in
  let success_called = ref false in
  TikTok.init_photo_post
    ~account_id:"test_account"
    ~photo_post_info:photo_info
    (handle_api_result
      (fun publish_id ->
        assert (publish_id = "photo_draft_123");
        success_called := true;
        print_endline "PASSED")
      (fun err -> failwith ("Unexpected error: " ^ err)));
  assert !success_called

let test_init_photo_post_empty_images_rejected () =
  print_string "Test: init_photo_post empty images rejected... ";
  reset_mock_state ();
  let photo_info = Social_tiktok_v1.make_photo_post_info
    ~title:"No images"
    ~photo_images:[]
    () in
  let done_ = ref false in
  TikTok.init_photo_post
    ~account_id:"test_account"
    ~photo_post_info:photo_info
    (function
      | Error (Error_types.Validation_error [Error_types.Media_required]) ->
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Media_required, got: " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "Expected validation error");
  assert !done_

let test_init_photo_post_cover_index_out_of_bounds () =
  print_string "Test: init_photo_post cover index out of bounds... ";
  reset_mock_state ();
  let photo_info = Social_tiktok_v1.make_photo_post_info
    ~title:"Bad index"
    ~photo_images:["https://example.com/photo1.jpg"]
    ~photo_cover_index:5
    () in
  let done_ = ref false in
  TikTok.init_photo_post
    ~account_id:"test_account"
    ~photo_post_info:photo_info
    (function
      | Error (Error_types.Internal_error msg) ->
          assert (String.length msg > 0);
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Internal_error, got: " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "Expected validation error");
  assert !done_

let test_init_photo_post_403_scope_mapping () =
  print_string "Test: init_photo_post 403 scope mapping... ";
  reset_mock_state ();
  Mock_http.set_custom_post_handler (fun url _headers _body ->
    if String.ends_with ~suffix:"content/init/" url then
      {
        Social_core.status = 403;
        headers = [];
        body = {|{"error":{"code":"forbidden","message":"Missing publish scope"}}|};
      }
    else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"new_access","refresh_token":"new_refresh","expires_in":86400}|};
      });
  let photo_info = Social_tiktok_v1.make_photo_post_info
    ~title:"Scope test"
    ~photo_images:["https://example.com/photo1.jpg"]
    () in
  let done_ = ref false in
  TikTok.init_photo_post
    ~account_id:"test_account"
    ~photo_post_info:photo_info
    (function
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (scopes = ["video.publish"]);
          done_ := true;
          print_endline "PASSED"
      | Error err -> failwith ("Expected Insufficient_permissions, got: " ^ Error_types.error_to_string err)
      | Ok _ -> failwith "Expected permission error");
  assert !done_

let test_upload_video_chunk_tracks_upload_url () =
  print_string "Test: upload_video_chunk tracks upload URL... ";
  reset_mock_state ();
  let success_called = ref false in
  let content = "video-bytes" in
  TikTok.upload_video_chunk
    ~upload_url:"https://upload.tiktok.com/video?upload_id=123"
    ~video_content:content
    (handle_api_result
      (fun () -> success_called := true)
      (fun err -> failwith ("Unexpected error: " ^ err)));
  assert !success_called;
  let call =
    match !(Mock_http.put_calls) with
    | c :: _ -> c
    | [] -> failwith "Expected PUT upload call"
  in
  assert (call.url = "https://upload.tiktok.com/video?upload_id=123");
  assert (call.body = Some content);
  print_endline "PASSED"

(** {1 PULL_FROM_URL Tests} *)

let test_init_video_pull_from_url_request_contract () =
  print_string "Test: init_video_pull_from_url request contract... ";
  reset_mock_state ();
  Mock_http.set_custom_post_handler (fun url _headers body ->
    if String.ends_with ~suffix:"video/init/" url then (
      let body_text = Option.value ~default:"" body in
      let json = Yojson.Basic.from_string body_text in
      let open Yojson.Basic.Util in
      let source_json = json |> member "source_info" in
      assert ((source_json |> member "source" |> to_string) = "PULL_FROM_URL");
      assert ((source_json |> member "video_url" |> to_string) = "https://example.com/video.mp4");
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"data":{"publish_id":"pull_pub_456"}}|};
      }
    ) else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"new_access","refresh_token":"new_refresh","expires_in":86400}|};
      });
  let post_info = Social_tiktok_v1.make_post_info ~title:"Pull from URL" ~privacy_level:Social_tiktok_v1.SelfOnly () in
  let success_called = ref false in
  TikTok.init_video_pull_from_url
    ~account_id:"test_account"
    ~post_info
    ~video_url:"https://example.com/video.mp4"
    (handle_api_result
      (fun publish_id ->
        assert (publish_id = "pull_pub_456");
        success_called := true;
        print_endline "PASSED")
      (fun err -> failwith ("Unexpected error: " ^ err)));
  assert !success_called

(** {1 Inbox/Draft Upload Tests} *)

let test_init_inbox_video_upload_request_contract () =
  print_string "Test: init_inbox_video_upload request contract... ";
  reset_mock_state ();
  Mock_http.set_custom_post_handler (fun url _headers body ->
    if String.ends_with ~suffix:"inbox/video/init/" url then (
      let body_text = Option.value ~default:"" body in
      let json = Yojson.Basic.from_string body_text in
      let open Yojson.Basic.Util in
      let source_json = json |> member "source_info" in
      assert ((source_json |> member "source" |> to_string) = "FILE_UPLOAD");
      assert ((source_json |> member "video_size" |> to_int) = 5000);
      assert ((json |> member "post_info" |> member "title" |> to_string) = "Draft upload");
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"data":{"publish_id":"inbox_pub_789","upload_url":"https://upload.tiktok.com/inbox"}}|};
      }
    ) else
      {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"new_access","refresh_token":"new_refresh","expires_in":86400}|};
      });
  let post_info = Social_tiktok_v1.make_post_info ~title:"Draft upload" ~privacy_level:Social_tiktok_v1.SelfOnly () in
  let success_called = ref false in
  TikTok.init_inbox_video_upload
    ~account_id:"test_account"
    ~post_info
    ~video_size:5000
    (handle_api_result
      (fun (publish_id, upload_url) ->
        assert (publish_id = "inbox_pub_789");
        assert (upload_url = "https://upload.tiktok.com/inbox");
        success_called := true;
        print_endline "PASSED")
      (fun err -> failwith ("Unexpected error: " ^ err)));
  assert !success_called

(** {1 Video Source / Media Type / Post Mode Type Tests} *)

let test_video_source_string_roundtrip () =
  print_string "Test: video_source string values... ";
  assert (Social_tiktok_v1.string_of_video_source Social_tiktok_v1.File_upload = "FILE_UPLOAD");
  assert (Social_tiktok_v1.string_of_video_source Social_tiktok_v1.Pull_from_url = "PULL_FROM_URL");
  print_endline "PASSED"

let test_post_mode_string_values () =
  print_string "Test: post_mode string values... ";
  assert (Social_tiktok_v1.string_of_post_mode Social_tiktok_v1.Direct_post = "DIRECT_POST");
  assert (Social_tiktok_v1.string_of_post_mode Social_tiktok_v1.Media_upload = "MEDIA_UPLOAD");
  print_endline "PASSED"

let test_media_type_string_values () =
  print_string "Test: media_type string values... ";
  assert (Social_tiktok_v1.string_of_media_type Social_tiktok_v1.Video = "VIDEO");
  assert (Social_tiktok_v1.string_of_media_type Social_tiktok_v1.Photo = "PHOTO");
  print_endline "PASSED"

let test_max_video_size_is_4gb () =
  print_string "Test: max_video_size_bytes is 4GB... ";
  assert (Social_tiktok_v1.max_video_size_bytes = 4_294_967_296);
  print_endline "PASSED"

let test_validate_video_accepts_large_file_under_4gb () =
  print_string "Test: validate_video accepts large file under 4GB... ";
  match Social_tiktok_v1.validate_video ~duration_sec:30 ~file_size_bytes:3_000_000_000 ~width:1080 ~height:1920 with
  | Ok () -> print_endline "PASSED"
  | Error msg -> failwith ("Expected Ok for 3GB file, got Error: " ^ msg)

let () =
  print_endline "\n=== TikTok Provider Tests ===\n";
  
  print_endline "--- Video Validation ---";
  test_validate_video_ok ();
  test_validate_video_too_short ();
  test_validate_video_too_large ();
  test_validate_video_resolution_too_low ();
  test_validate_video_resolution_too_high ();
  test_validate_video_duration_too_long ();
  test_validate_video_at_limits ();
  
  print_endline "\n--- Caption Validation ---";
  test_validate_caption_valid ();
  test_validate_caption_too_long ();
  test_validate_content ();
  
  print_endline "\n--- Privacy & Post Info ---";
  test_privacy_level_roundtrip ();
  test_all_privacy_levels ();
  test_make_post_info_defaults ();
  test_make_post_info_all_options ();
  test_post_info_to_json ();
  test_authorization_url ();
  test_oauth_auth_url_required_params ();
  
  print_endline "\n--- Post Operations ---";
  test_post_single_no_media ();
  test_post_single_caption_too_long ();
  test_post_single_invalid_media_url ();
  test_post_single_uppercase_scheme_url_valid ();
  test_post_single_success ();
  test_post_info_privacy_options ();
  test_post_thread_success ();
  test_post_thread_empty ();
  test_post_thread_mismatched_lengths_extra_media ();
  test_post_thread_mismatched_lengths_missing_media_entry ();
  test_thread_validation_missing_media ();
  test_thread_validation_invalid_media_url ();
  
  print_endline "\n--- API Operations ---";
  test_get_creator_info ();
  test_parse_creator_info_missing_optional_fields ();
  test_parse_creator_info_unknown_privacy_values_fallback ();
  test_parse_creator_info_missing_privacy_defaults_self_only ();
  test_parse_account_stats_response ();
  test_parse_video_ids_response ();
  test_parse_video_analytics_response ();
  test_canonical_analytics_adapters ();
  test_check_publish_status ();
  test_init_video_upload ();
  test_init_video_upload_request_contract ();
  test_get_account_analytics_request_contract ();
  test_get_post_analytics_request_contract ();
  test_parse_publish_status_with_publicly_field ();
  test_parse_publish_status_failed_without_reason ();
  test_check_publish_status_request_contract ();
  test_upload_video_chunk_headers_contract ();
  test_upload_video_chunk_empty_content_rejected ();
  test_upload_video_chunk_invalid_range_rejected ();
  test_upload_video_chunk_end_before_start_rejected ();
  test_upload_video_chunk_length_range_mismatch_rejected ();
  test_post_video_uses_multi_chunk_upload ();
  test_post_video_caption_too_long_validation_error ();
  test_post_video_rejects_disallowed_privacy_level ();
  test_post_video_rejects_comment_enabled_when_creator_disables_comments ();
  test_post_video_allows_when_creator_policies_satisfied ();
  test_post_video_from_url_propagates_content_type ();
  test_post_video_from_url_normalizes_content_type ();
  test_post_video_from_url_rejects_unsupported_content_type ();
  test_post_video_from_url_empty_content_type_defaults_mp4 ();
  test_post_video_from_url_accepts_x_quicktime ();
  test_post_video_from_url_invalid_url_validation ();
  test_post_video_from_url_download_404_maps_resource_not_found ();
  test_post_video_from_url_download_500_maps_network_error ();
  test_post_video_from_url_download_transport_error_maps_network_error ();
  test_get_creator_info_429_maps_rate_limited ();
  test_get_creator_info_429_invalid_json_still_rate_limited ();
  test_get_creator_info_401_expired_maps_token_expired ();
  test_get_creator_info_403_scope_mapping ();
  test_init_video_upload_403_scope_mapping ();
  test_check_publish_status_403_scope_mapping ();
  test_get_creator_info_404_maps_resource_not_found ();
  test_get_creator_info_500_request_id_and_top_level_error ();
  test_get_creator_info_500_numeric_error_code ();
  test_upload_video_chunk_403_scope_mapping ();
  test_get_creator_info_invalid_json_returns_error ();
  test_check_publish_status_invalid_json_returns_error ();
  test_wait_for_publish_processing_then_published ();
  test_wait_for_publish_timeout ();
  test_wait_for_publish_on_processing_callback ();
  test_wait_for_publish_invalid_max_attempts ();
  test_exchange_code ();
  test_exchange_code_without_refresh_token ();
  test_exchange_code_without_expires_in_uses_default ();
  test_exchange_code_403_scope_mapping ();
  test_get_oauth_url ();
  test_get_oauth_url_includes_pkce_when_code_verifier_present ();
  test_oauth_exchange_request_contract ();
  test_oauth_exchange_request_includes_code_verifier_when_present ();
  test_oauth_refresh_triggered_within_buffer ();
  test_refresh_without_new_refresh_token_keeps_old ();
  test_refresh_without_expires_in_uses_default ();
  test_get_oauth_url_missing_client_key ();
  test_post_single_multiple_media_rejected ();

  print_endline "\n--- Photo Post Support ---";
  test_make_photo_post_info_defaults ();
  test_make_photo_post_info_all_options ();
  test_photo_post_info_to_json ();
  test_init_photo_post_request_contract ();
  test_init_photo_post_auth_header ();
  test_init_photo_post_media_upload_mode ();
  test_init_photo_post_empty_images_rejected ();
  test_init_photo_post_cover_index_out_of_bounds ();
  test_init_photo_post_403_scope_mapping ();

  print_endline "\n--- Upload URL Tracking ---";
  test_upload_video_chunk_tracks_upload_url ();

  print_endline "\n--- PULL_FROM_URL Support ---";
  test_init_video_pull_from_url_request_contract ();

  print_endline "\n--- Inbox/Draft Upload ---";
  test_init_inbox_video_upload_request_contract ();

  print_endline "\n--- Type String Values ---";
  test_video_source_string_roundtrip ();
  test_post_mode_string_values ();
  test_media_type_string_values ();

  print_endline "\n--- Video Size Limit Fix ---";
  test_max_video_size_is_4gb ();
  test_validate_video_accepts_large_file_under_4gb ();

  print_endline "\n=== All tests passed! ==="
