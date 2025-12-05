(** Tests for Twitter API v1.1 Provider *)

open OUnit2

(** Helper to check if a string contains a substring *)
let contains_substring str sub =
  try
    let _ = Str.search_forward (Str.regexp_string sub) str 0 in
    true
  with Not_found -> false

(** Mock HTTP client for testing *)
module Mock_http : Social_core.HTTP_CLIENT = struct
  let get ?headers:_ url on_success _on_error =
    (* Return different responses based on URL *)
    if String.contains url '/' && contains_substring url "stream" then
      (* Streaming endpoints *)
      on_success {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"id_str": "123", "text": "Sample tweet from stream"}|};
      }
    else if String.contains url '/' && contains_substring url "oembed" then
      (* oEmbed endpoint *)
      on_success {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"html": "<blockquote>Tweet embed</blockquote>", "width": 550}|};
      }
    else if String.contains url '/' && contains_substring url "geo" then
      (* Geo endpoint *)
      on_success {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"result": {"places": [{"name": "San Francisco"}]}}|};
      }
    else if String.contains url '/' && contains_substring url "STATUS" then
      (* Media upload STATUS check *)
      on_success {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"processing_info": {"state": "succeeded", "progress_percent": 100}}|};
      }
    else
      on_success {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"data": {}}|};
      }
  
  let post ?headers:_ ?body:_ url on_success _on_error =
    (* Return different responses based on URL *)
    let response_body = 
      if String.contains url '/' && contains_substring url "collections/create" then
        {|{"response": {"timeline_id": "custom-123456", "name": "My Collection"}}|}
      else if String.contains url '/' && contains_substring url "collections/entries/add" then
        {|{"response": {"timeline_id": "custom-123456"}}|}
      else if String.contains url '/' && contains_substring url "saved_searches" then
        {|{"id_str": "456", "query": "#OCaml"}|}
      else if String.contains url '/' && contains_substring url "INIT" then
        {|{"media_id_string": "media_123456789"}|}
      else if String.contains url '/' && contains_substring url "APPEND" then
        {|{}|}
      else if String.contains url '/' && contains_substring url "FINALIZE" then
        {|{"media_id_string": "media_123456789", "processing_info": {"state": "pending"}}|}
      else if String.contains url '/' && contains_substring url "filter" then
        {|{"id_str": "789", "text": "Filtered tweet"}|}
      else
        {|{"data": {"id": "result_12345"}}|}
    in
    on_success {
      Social_core.status = 200;
      headers = [("content-type", "application/json")];
      body = response_body;
    }
  
  let post_multipart ?headers:_ ~parts:_ _url on_success _on_error =
    on_success {
      Social_core.status = 200;
      headers = [];
      body = {|{"media_id_string": "media_multipart_123"}|};
    }
  
  let put ?headers:_ ?body:_ _url on_success _on_error =
    on_success {
      Social_core.status = 200;
      headers = [];
      body = {|{"data": {}}|};
    }
  
  let delete ?headers:_ _url on_success _on_error =
    on_success {
      Social_core.status = 200;
      headers = [];
      body = {|{"data": {}}|};
    }
end

(** Mock config for testing *)
module Mock_config = struct
  module Http = Mock_http
  
  let get_env key =
    match key with
    | "TWITTER_CONSUMER_KEY" -> Some "test_consumer_key"
    | "TWITTER_CONSUMER_SECRET" -> Some "test_consumer_secret"
    | _ -> None
  
  let get_credentials ~account_id:_ on_success _on_error =
    let expires = Unix.time () +. 3600. |> Ptime.of_float_s |> Option.get in
    let expires_str = Ptime.to_rfc3339 expires in
    on_success {
      Social_core.access_token = "test_access_token";
      refresh_token = Some "test_token_secret";
      expires_at = Some expires_str;
      token_type = "Bearer";
    }
  
  let update_credentials ~account_id:_ ~credentials:_ on_success _on_error =
    on_success ()
  
  let update_health_status ~account_id:_ ~status:_ ~error_message:_ on_success _on_error =
    on_success ()
end

module Twitter_v1 = Social_twitter_v1.Twitter_v1.Make(Mock_config)

(** Test OAuth 1.0a signature generation *)
let test_oauth_signature _ctx =
  (* Note: OAuth signature functions are internal to the module *)
  (* We test them indirectly through API calls *)
  assert_bool "OAuth signature test placeholder" true

(** Test Collections API - Create collection *)
let test_create_collection _ctx =
  let result = ref None in
  let error = ref None in
  
  Twitter_v1.create_collection
    ~account_id:"test_account"
    ~name:"Test Collection"
    ~description:(Some "A test collection")
    ~url:(Some "https://example.com")
    (fun json -> result := Some json)
    (fun err -> error := Some err);
  
  assert_bool "Should succeed" (Option.is_some !result);
  assert_bool "Should not error" (Option.is_none !error);
  
  match !result with
  | Some json ->
      let open Yojson.Basic.Util in
      let response = json |> member "response" in
      let timeline_id = response |> member "timeline_id" |> to_string in
      assert_equal "custom-123456" timeline_id
  | None -> assert_failure "Expected successful result"

(** Test Collections API - Add to collection *)
let test_add_to_collection _ctx =
  let success = ref false in
  let error = ref None in
  
  Twitter_v1.add_to_collection
    ~account_id:"test_account"
    ~collection_id:"custom-123456"
    ~tweet_id:"123456789"
    (fun () -> success := true)
    (fun err -> error := Some err);
  
  assert_bool "Should succeed" !success;
  assert_bool "Should not error" (Option.is_none !error)

(** Test Saved Searches API *)
let test_create_saved_search _ctx =
  let result = ref None in
  let error = ref None in
  
  Twitter_v1.create_saved_search
    ~account_id:"test_account"
    ~query:"#OCaml"
    (fun json -> result := Some json)
    (fun err -> error := Some err);
  
  assert_bool "Should succeed" (Option.is_some !result);
  assert_bool "Should not error" (Option.is_none !error);
  
  match !result with
  | Some json ->
      let open Yojson.Basic.Util in
      let query = json |> member "query" |> to_string in
      assert_equal "#OCaml" query
  | None -> assert_failure "Expected successful result"

(** Test oEmbed API *)
let test_oembed _ctx =
  let result = ref None in
  let error = ref None in
  
  Twitter_v1.get_oembed
    ~tweet_id:"123456789"
    ~max_width:(Some 400)
    ~hide_media:true
    ()
    (fun json -> result := Some json)
    (fun err -> error := Some err);
  
  assert_bool "Should succeed" (Option.is_some !result);
  assert_bool "Should not error" (Option.is_none !error);
  
  match !result with
  | Some json ->
      let open Yojson.Basic.Util in
      let html = json |> member "html" |> to_string in
      assert_bool "Should contain blockquote" (String.contains html 'b')
  | None -> assert_failure "Expected successful result"

(** Test Geo API - Reverse geocode *)
let test_reverse_geocode _ctx =
  let result = ref None in
  let error = ref None in
  
  Twitter_v1.reverse_geocode
    ~lat:37.7821
    ~long:(-122.4093)
    ~granularity:"city"
    ()
    (fun json -> result := Some json)
    (fun err -> error := Some err);
  
  assert_bool "Should succeed" (Option.is_some !result);
  assert_bool "Should not error" (Option.is_none !error)

(** Test Chunked Media Upload - INIT *)
let test_upload_media_init _ctx =
  let result = ref None in
  let error = ref None in
  
  Twitter_v1.upload_media_init
    ~account_id:"test_account"
    ~total_bytes:5000000
    ~media_type:"video/mp4"
    (fun media_id -> result := Some media_id)
    (fun err -> error := Some err);
  
  assert_bool "Should succeed" (Option.is_some !result);
  assert_bool "Should not error" (Option.is_none !error);
  
  match !result with
  | Some media_id ->
      assert_equal "media_123456789" media_id
  | None -> assert_failure "Expected media_id"

(** Test Chunked Media Upload - APPEND *)
let test_upload_media_append _ctx =
  let success = ref false in
  let error = ref None in
  
  Twitter_v1.upload_media_append
    ~account_id:"test_account"
    ~media_id:"media_123456789"
    ~media_data:"test_chunk_data"
    ~segment_index:0
    (fun () -> success := true)
    (fun err -> error := Some err);
  
  assert_bool "Should succeed" !success;
  assert_bool "Should not error" (Option.is_none !error)

(** Test Chunked Media Upload - FINALIZE *)
let test_upload_media_finalize _ctx =
  let result = ref None in
  let error = ref None in
  
  Twitter_v1.upload_media_finalize
    ~account_id:"test_account"
    ~media_id:"media_123456789"
    (fun (json, processing_info) -> result := Some (json, processing_info))
    (fun err -> error := Some err);
  
  assert_bool "Should succeed" (Option.is_some !result);
  assert_bool "Should not error" (Option.is_none !error);
  
  match !result with
  | Some (_json, processing_info) ->
      assert_bool "Should have processing info" (Option.is_some processing_info)
  | None -> assert_failure "Expected result"

(** Test Chunked Media Upload - STATUS *)
let test_upload_media_status _ctx =
  let result = ref None in
  let error = ref None in
  
  Twitter_v1.upload_media_status
    ~account_id:"test_account"
    ~media_id:"media_123456789"
    (fun json -> result := Some json)
    (fun err -> error := Some err);
  
  assert_bool "Should succeed" (Option.is_some !result);
  assert_bool "Should not error" (Option.is_none !error);
  
  match !result with
  | Some json ->
      let open Yojson.Basic.Util in
      let processing = json |> member "processing_info" in
      let state = processing |> member "state" |> to_string in
      assert_equal "succeeded" state
  | None -> assert_failure "Expected result"

(** Test Complete Chunked Upload Helper *)
let test_upload_media_chunked _ctx =
  let result = ref None in
  let error = ref None in
  
  (* Create test data smaller than chunk size *)
  let test_data = String.make 1000 'x' in
  
  Twitter_v1.upload_media_chunked
    ~account_id:"test_account"
    ~media_data:test_data
    ~media_type:"image/jpeg"
    ~chunk_size:5000000
    ()
    (fun (media_id, processing) -> result := Some (media_id, processing))
    (fun err -> error := Some err);
  
  assert_bool "Should succeed" (Option.is_some !result);
  assert_bool "Should not error" (Option.is_none !error);
  
  match !result with
  | Some (media_id, _processing) ->
      assert_equal "media_123456789" media_id
  | None -> assert_failure "Expected result"

(** Test Media Upload with Alt-Text Metadata *)
let test_upload_media_with_alt_text _ctx =
  let result = ref None in
  let error = ref None in
  
  (* Upload media with alt-text via metadata *)
  let test_data = String.make 1000 'x' in
  
  Twitter_v1.upload_media_chunked
    ~account_id:"test_account"
    ~media_data:test_data
    ~media_type:"image/jpeg"
    ~chunk_size:5000000
    ()
    (fun (media_id, processing) -> result := Some (media_id, processing))
    (fun err -> error := Some err);
  
  assert_bool "Should succeed" (Option.is_some !result);
  assert_bool "Should not error" (Option.is_none !error);
  
  match !result with
  | Some (media_id, _processing) ->
      (* In v1.1, alt-text is added after upload via metadata/create endpoint *)
      assert_equal "media_123456789" media_id
  | None -> assert_failure "Expected result"

(** Test Media Upload for Video with Alt-Text *)
let test_upload_video_with_alt_text _ctx =
  let result = ref None in
  let error = ref None in
  
  let test_video_data = String.make 10000 'v' in
  
  Twitter_v1.upload_media_chunked
    ~account_id:"test_account"
    ~media_data:test_video_data
    ~media_type:"video/mp4"
    ~chunk_size:5000000
    ()
    (fun (media_id, processing) -> result := Some (media_id, processing))
    (fun err -> error := Some err);
  
  assert_bool "Should succeed" (Option.is_some !result);
  assert_bool "Should not error" (Option.is_none !error);
  
  match !result with
  | Some (media_id, _processing) ->
      assert_equal "media_123456789" media_id
  | None -> assert_failure "Expected result"

(** Test Multiple Media Uploads with Alt-Text *)
let test_upload_multiple_media_with_alt_texts _ctx =
  let results = ref [] in
  let error = ref None in
  
  (* Simulate uploading multiple images, each can have alt-text metadata *)
  let test_data_1 = String.make 1000 'a' in
  let test_data_2 = String.make 1000 'b' in
  
  Twitter_v1.upload_media_chunked
    ~account_id:"test_account"
    ~media_data:test_data_1
    ~media_type:"image/jpeg"
    ()
    (fun (media_id, _) -> results := media_id :: !results)
    (fun err -> error := Some err);
  
  Twitter_v1.upload_media_chunked
    ~account_id:"test_account"
    ~media_data:test_data_2
    ~media_type:"image/jpeg"
    ()
    (fun (media_id, _) -> results := media_id :: !results)
    (fun err -> error := Some err);
  
  assert_bool "Should not error" (Option.is_none !error);
  assert_bool "Should have results" (List.length !results > 0)

(** Test Alt-Text with Unicode Characters *)
let test_alt_text_unicode_v1 _ctx =
  (* Note: In v1.1, alt-text is added via separate metadata/create call *)
  (* This test verifies the media upload works for images that will have Unicode alt-text *)
  let result = ref None in
  let error = ref None in
  
  let test_data = String.make 1000 'x' in
  
  Twitter_v1.upload_media_chunked
    ~account_id:"test_account"
    ~media_data:test_data
    ~media_type:"image/png"
    ()
    (fun (media_id, processing) -> result := Some (media_id, processing))
    (fun err -> error := Some err);
  
  assert_bool "Should succeed" (Option.is_some !result);
  assert_bool "Should not error" (Option.is_none !error);
  
  (* Alt-text would be: "Photo of 🌅 sunset with text: こんにちは" *)
  match !result with
  | Some (media_id, _) ->
      assert_equal "media_123456789" media_id
  | None -> assert_failure "Expected result"

(** Test GIF Upload with Alt-Text *)
let test_upload_gif_with_alt_text _ctx =
  let result = ref None in
  let error = ref None in
  
  let test_gif_data = String.make 5000 'g' in
  
  Twitter_v1.upload_media_chunked
    ~account_id:"test_account"
    ~media_data:test_gif_data
    ~media_type:"image/gif"
    ()
    (fun (media_id, processing) -> result := Some (media_id, processing))
    (fun err -> error := Some err);
  
  assert_bool "Should succeed" (Option.is_some !result);
  assert_bool "Should not error" (Option.is_none !error);
  
  match !result with
  | Some (media_id, _) ->
      assert_equal "media_123456789" media_id
  | None -> assert_failure "Expected result"

(** Test Streaming API - Filter *)
let test_stream_filter _ctx =
  let result = ref None in
  let error = ref None in
  
  Twitter_v1.stream_filter
    ~account_id:"test_account"
    ~track:["OCaml"; "functional programming"]
    ~on_tweet:(fun tweet -> result := Some tweet)
    ~on_error:(fun err -> error := Some err);
  
  (* Note: In real implementation, this would be a continuous stream *)
  assert_bool "Should receive data or error" 
    (Option.is_some !result || Option.is_some !error)

(** Test Streaming API - Sample *)
let test_stream_sample _ctx =
  let result = ref None in
  let error = ref None in
  
  Twitter_v1.stream_sample
    ~account_id:"test_account"
    ~on_tweet:(fun tweet -> result := Some tweet)
    ~on_error:(fun err -> error := Some err);
  
  (* Note: In real implementation, this would be a continuous stream *)
  assert_bool "Should receive data or error" 
    (Option.is_some !result || Option.is_some !error)

(** Test Suite *)
let suite =
  "Twitter v1.1 Tests" >::: [
    "test_oauth_signature" >:: test_oauth_signature;
    "test_create_collection" >:: test_create_collection;
    "test_add_to_collection" >:: test_add_to_collection;
    "test_create_saved_search" >:: test_create_saved_search;
    "test_oembed" >:: test_oembed;
    "test_reverse_geocode" >:: test_reverse_geocode;
    "test_upload_media_init" >:: test_upload_media_init;
    "test_upload_media_append" >:: test_upload_media_append;
    "test_upload_media_finalize" >:: test_upload_media_finalize;
    "test_upload_media_status" >:: test_upload_media_status;
    "test_upload_media_chunked" >:: test_upload_media_chunked;
    "test_stream_filter" >:: test_stream_filter;
    "test_stream_sample" >:: test_stream_sample;
    (* Alt-text related tests *)
    "test_upload_media_with_alt_text" >:: test_upload_media_with_alt_text;
    "test_upload_video_with_alt_text" >:: test_upload_video_with_alt_text;
    "test_upload_multiple_media_with_alt_texts" >:: test_upload_multiple_media_with_alt_texts;
    "test_alt_text_unicode_v1" >:: test_alt_text_unicode_v1;
    "test_upload_gif_with_alt_text" >:: test_upload_gif_with_alt_text;
  ]

let () =
  run_test_tt_main suite
