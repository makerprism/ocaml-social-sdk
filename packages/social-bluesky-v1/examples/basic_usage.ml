(** Basic usage example for Bluesky provider with Lwt adapter
    
    This example shows how to use the Bluesky provider with:
    - Lwt for async operations
    - Cohttp for HTTP client
    - In-memory credential storage (for demo purposes)
*)

(** Simple in-memory credential storage for demo *)
let credentials_store = Hashtbl.create 10

(** Direct CPS configuration *)
module Cps_config = struct
  
  module Http : Social_core.HTTP_CLIENT = struct
    (* Mock HTTP - in real app, use cohttp or similar *)
    let get ?headers:_ _url _on_success on_error = on_error "HTTP not implemented"
    let post ?headers:_ ?body:_ _url _on_success on_error = on_error "HTTP not implemented"
    let post_multipart ?headers:_ ~parts:_ _url _on_success on_error = on_error "HTTP not implemented"
    let put ?headers:_ ?body:_ _url _on_success on_error = on_error "HTTP not implemented"
    let delete ?headers:_ _url _on_success on_error = on_error "HTTP not implemented"
  end
  
  let get_env = Sys.getenv_opt
  
  let get_credentials ~account_id on_success on_error =
    match Hashtbl.find_opt credentials_store account_id with
    | Some creds -> on_success creds
    | None -> on_error "Account not found"
  
  let update_credentials ~account_id ~credentials on_success _on_error =
    Hashtbl.replace credentials_store account_id credentials;
    on_success ()
  
  let encrypt data on_success _on_error =
    (* In real app, use proper encryption *)
    on_success (Base64.encode_exn data)
  
  let decrypt data on_success on_error =
    try
      on_success (Base64.decode_exn data)
    with e ->
      on_error (Printexc.to_string e)
  
  let update_health_status ~account_id:_ ~status ~error_message on_success _on_error =
    (match error_message with
    | Some msg -> Printf.printf "Health status: %s - %s\n%!" status msg
    | None -> Printf.printf "Health status: %s\n%!" status);
    on_success ()
end

(** Create Bluesky provider instance *)
module Bluesky = Social_bluesky_v1.Make(Cps_config)

(** Example: Post a simple message *)
let example_post_simple () =
  Printf.printf "Example: Posting a simple message\n%!";
  
  (* Set up mock credentials *)
  Hashtbl.replace credentials_store "demo_account" {
    Social_core.access_token = "your.bsky.handle";
    refresh_token = Some "your-app-password";
    expires_at = None;
    auth_type = Social_core.Bearer;
  };
  
  (* Post using CPS style with outcome type *)
  Bluesky.post_single
    ~account_id:"demo_account"
    ~text:"Hello from OCaml!"
    ~media_urls:[]
    (function
      | Error_types.Success post_uri ->
          Printf.printf "Posted successfully: %s\n%!" post_uri
      | Error_types.Partial_success { result = post_uri; warnings } ->
          Printf.printf "Posted with warnings: %s\n%!" post_uri;
          List.iter (fun w -> Printf.printf "  Warning: %s\n%!" 
            (Error_types.warning_to_string w)) warnings
      | Error_types.Failure err ->
          Printf.printf "Post failed: %s\n%!" (Error_types.error_to_string err))

(** Example: Validate content before posting *)
let example_validate () =
  Printf.printf "\nExample: Validating content\n%!";
  
  let short_text = "This is a short post" in
  let long_text = String.make 400 'a' in
  
  (match Bluesky.validate_content ~text:short_text with
  | Ok () -> Printf.printf "Short text is valid\n%!"
  | Error e -> Printf.printf "Short text invalid: %s\n%!" e);
  
  (match Bluesky.validate_content ~text:long_text with
  | Ok () -> Printf.printf "Long text is valid\n%!"
  | Error e -> Printf.printf "Long text invalid: %s\n%!" e)

(** Example: Validate media *)
let example_validate_media () =
  Printf.printf "\nExample: Validating media\n%!";
  
  let valid_image = {
    Platform_types.media_type = Platform_types.Image;
    mime_type = "image/png";
    file_size_bytes = 500_000;
    width = Some 1024;
    height = Some 768;
    duration_seconds = None;
    alt_text = Some "A beautiful sunset";
  } in
  
  match Bluesky.validate_media ~media:valid_image with
  | Ok () -> Printf.printf "Image is valid\n%!"
  | Error _ -> Printf.printf "Image invalid\n%!"

(** Example: Pre-validate a thread *)
let example_validate_thread () =
  Printf.printf "\nExample: Validating thread\n%!";
  
  let valid_thread = ["First post"; "Second post"; "Third post"] in
  let invalid_thread = ["Short"; String.make 400 'x'; "OK"] in
  
  (match Bluesky.validate_thread ~texts:valid_thread () with
  | Ok () -> Printf.printf "Thread is valid\n%!"
  | Error errs -> 
      Printf.printf "Thread invalid:\n%!";
      List.iter (fun e -> Printf.printf "  - %s\n%!" 
        (Error_types.validation_error_to_string e)) errs);
  
  (match Bluesky.validate_thread ~texts:invalid_thread () with
  | Ok () -> Printf.printf "Thread is valid\n%!"
  | Error errs -> 
      Printf.printf "Thread invalid:\n%!";
      List.iter (fun e -> Printf.printf "  - %s\n%!" 
        (Error_types.validation_error_to_string e)) errs)

(** Run all examples *)
let () =
  Printf.printf "=== Bluesky Provider Usage Examples ===\n\n";
  
  example_validate ();
  example_validate_media ();
  example_validate_thread ();
  example_post_simple ();
  
  Printf.printf "\n=== Examples complete ===\n"
