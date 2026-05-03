(** Simple CPS example showing the core concept
    
    This demonstrates the pure CPS style without async complications.
    For real Lwt/Eio integration, you'd use the appropriate adapters.
*)

(** Synchronous mock HTTP client for demo *)
module Sync_http : Social_core.HTTP_CLIENT = struct
  let get ?headers:_ url on_success _on_error =
    Printf.printf "HTTP GET: %s\n%!" url;
    on_success {
      Social_core.status = 200;
      headers = [("content-type", "application/json")];
      body = {|{"did":"did:plc:test","accessJwt":"test_jwt"}|};
    }
  
  let post ?headers:_ ?body:_ url on_success _on_error =
    Printf.printf "HTTP POST: %s\n%!" url;
    on_success {
      Social_core.status = 200;
      headers = [("content-type", "application/json")];
      body = {|{"uri":"at://test/post/123","cid":"abc"}|};
    }
  
  let post_multipart ?headers:_ ~parts:_ _url on_success _on_error =
    on_success {
      Social_core.status = 200;
      headers = [];
      body = "{}";
    }
  
  let put ?headers:_ ?body:_ _url on_success _on_error =
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

(** Demo configuration *)
module Demo_config = struct
  module Http = Sync_http
  
  let get_env _key = Some "test_value"
  
  let get_credentials ~account_id:_ on_success _on_error =
    on_success {
      Social_core.access_token = "test.handle";
      refresh_token = Some "test_app_password";
      expires_at = None;
      auth_type = Social_core.Bearer;
    scope = None;
    }
  
  let update_credentials ~account_id:_ ~credentials:_ on_success _on_error =
    on_success ()
  
  let encrypt _data on_success _on_error =
    on_success "encrypted"
  
  let decrypt _data on_success _on_error =
    on_success {|{"access_token":"test.handle","refresh_token":"password"}|}
  
  let update_health_status ~account_id:_ ~status ~error_message on_success _on_error =
    Printf.printf "Health: %s%s\n%!" status
      (match error_message with Some m -> " - " ^ m | None -> "");
    on_success ()

  let resize_image ~data:_ ~mime_type:_ ~max_bytes:_ on_result =
    on_result None
end

(** Create provider *)
module Bluesky = Social_bluesky_v1.Make(Demo_config)

(** Example: Post with CPS style *)
let () =
  Printf.printf "=== Bluesky CPS Example ===\n\n";
  
  (* Validate content first *)
  Printf.printf "1. Validating content...\n";
  (match Bluesky.validate_content ~text:"Hello Bluesky!" with
  | Ok () -> Printf.printf "   ✓ Content valid\n"
  | Error e -> Printf.printf "   ✗ Invalid: %s\n" e);
  
  Printf.printf "\n2. Posting to Bluesky...\n";
  Bluesky.post_single
    ~account_id:"demo"
    ~text:"Hello from OCaml CPS!"
    ~media_urls:[]
    (function
      | Error_types.Success post_uri ->
          Printf.printf "   ✓ Success: %s\n%!" post_uri
      | Error_types.Partial_success { result = post_uri; warnings } ->
          Printf.printf "   ✓ Success with warnings: %s\n%!" post_uri;
          List.iter (fun w -> 
            Printf.printf "     Warning: %s\n%!" (Error_types.warning_to_string w)
          ) warnings
      | Error_types.Failure err ->
          Printf.printf "   ✗ Failed: %s\n%!" (Error_types.error_to_string err));
  
  Printf.printf "\n=== Complete ===\n"
