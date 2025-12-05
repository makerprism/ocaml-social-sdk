(** Quick test for link card functionality *)

open Social_bluesky_v1

(** Mock HTTP client *)
module Mock_http : Social_core.HTTP_CLIENT = struct
  let get ?headers:_ url on_success _on_error =
    if String.length url >= 24 && String.sub url 0 24 = "https://www.youtube.com/" then
      let html = {|<html><head>
<meta property="og:title" content="Test Video">
<meta property="og:description" content="Test Description">
<meta property="og:image" content="https://i.ytimg.com/vi/test.jpg">
</head></html>|} in
      on_success {
        Social_core.status = 200;
        headers = [("content-type", "text/html")];
        body = html;
      }
    else if String.length url >= 20 && String.sub url 0 20 = "https://i.ytimg.com/" then
      on_success {
        Social_core.status = 200;
        headers = [("content-type", "image/jpeg")];
        body = "mock_image_bytes";
      }
    else
      on_success {
        Social_core.status = 200;
        headers = [("content-type", "text/plain")];
        body = "ok";
      }
  
  let post ?headers:_ ?body:_ url on_success _on_error =
    if String.contains url 'u' && String.contains url 'p' then
      on_success {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"blob": {"$type": "blob", "ref": {"$link": "bafytest"}, "mimeType": "image/jpeg", "size": 1000}}|};
      }
    else
      on_success {
        Social_core.status = 200;
        headers = [];
        body = "{}";
      }
  
  let post_multipart ?headers:_ ~parts:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }
  
  let put ?headers:_ ?body:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }
  
  let delete ?headers:_ _url on_success _on_error =
    on_success { Social_core.status = 200; headers = []; body = "{}" }
end

module Mock_config = struct
  module Http = Mock_http
  let get_env _key = Some "test"
  let get_credentials ~account_id:_ on_success _on_error =
    on_success {
      Social_core.access_token = "test";
      refresh_token = Some "test";
      expires_at = None;
      token_type = "Bearer";
    }
  let update_credentials ~account_id:_ ~credentials:_ on_success _on_error = on_success ()
  let encrypt _data on_success _on_error = on_success "encrypted"
  let decrypt _data on_success _on_error = on_success "{}"
  let update_health_status ~account_id:_ ~status:_ ~error_message:_ on_success _on_error = on_success ()
end

module Bluesky = Make(Mock_config)

let () =
  print_endline "Testing link card fetching...";
  
  let result = ref None in
  Bluesky.fetch_link_card
    ~access_jwt:"test"
    ~url:"https://www.youtube.com/watch?v=test"
    (fun card_opt -> result := Some card_opt)
    (fun _ -> result := Some None);
  
  match !result with
  | Some (Some card) ->
      let open Yojson.Basic.Util in
      let embed_type = card |> member "$type" |> to_string in
      print_endline ("  Embed type: " ^ embed_type);
      
      let external_obj = card |> member "external" in
      let title = external_obj |> member "title" |> to_string in
      let uri = external_obj |> member "uri" |> to_string in
      
      print_endline ("  Title: " ^ title);
      print_endline ("  URI: " ^ uri);
      
      let has_thumb = try
        let thumb = external_obj |> member "thumb" in
        let _ = thumb |> member "$type" |> to_string in
        true
      with _ -> false in
      
      print_endline ("  Has thumbnail: " ^ string_of_bool has_thumb);
      
      if embed_type = "app.bsky.embed.external" && title = "Test Video" && has_thumb then
        print_endline "\n✅ Link card test PASSED!"
      else
        print_endline "\n❌ Link card test FAILED!"
  | Some None ->
      print_endline "❌ No card returned"
  | None ->
      print_endline "❌ Test didn't complete"
