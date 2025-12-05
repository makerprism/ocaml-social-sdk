(** Basic tests for TikTok API types and validation *)

let test_validate_video_ok () =
  match Social_tiktok_v1.validate_video ~duration_sec:30 ~file_size_bytes:10_000_000 ~width:1080 ~height:1920 with
  | Ok () -> ()
  | Error msg -> failwith ("Expected Ok, got Error: " ^ msg)

let test_validate_video_too_short () =
  match Social_tiktok_v1.validate_video ~duration_sec:1 ~file_size_bytes:10_000_000 ~width:1080 ~height:1920 with
  | Error _ -> ()
  | Ok () -> failwith "Expected Error for too short video"

let test_validate_video_too_large () =
  match Social_tiktok_v1.validate_video ~duration_sec:30 ~file_size_bytes:100_000_000 ~width:1080 ~height:1920 with
  | Error _ -> ()
  | Ok () -> failwith "Expected Error for too large video"

let test_privacy_level_roundtrip () =
  let levels = [Social_tiktok_v1.PublicToEveryone; MutualFollowFriends; SelfOnly] in
  List.iter (fun level ->
    let s = Social_tiktok_v1.string_of_privacy_level level in
    let level2 = Social_tiktok_v1.privacy_level_of_string s in
    assert (level = level2)
  ) levels

let test_authorization_url () =
  let url = Social_tiktok_v1.get_authorization_url 
    ~client_id:"test_client"
    ~redirect_uri:"https://example.com/callback"
    ~scope:"user.info.basic,video.publish"
    ~state:"random_state"
  in
  assert (String.length url > 0);
  assert (String.sub url 0 5 = "https")

let () =
  print_endline "Running TikTok API tests...";
  test_validate_video_ok ();
  test_validate_video_too_short ();
  test_validate_video_too_large ();
  test_privacy_level_roundtrip ();
  test_authorization_url ();
  print_endline "All tests passed!"
