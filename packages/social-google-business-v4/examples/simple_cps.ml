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
      body = {|{"accounts":[{"name":"accounts/123","accountName":"My Business"}]}|};
    }

  let post ?headers:_ ?body:_ url on_success _on_error =
    Printf.printf "HTTP POST: %s\n%!" url;
    on_success {
      Social_core.status = 200;
      headers = [("content-type", "application/json")];
      body = {|{"name":"locations/456/localPosts/789"}|};
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

  let get_env = function
    | "GOOGLE_BUSINESS_CLIENT_ID" -> Some "demo_client_id"
    | "GOOGLE_BUSINESS_CLIENT_SECRET" -> Some "demo_client_secret"
    | "GOOGLE_BUSINESS_LOCATION_NAME" -> Some "locations/456"
    | _ -> None

  let get_credentials ~account_id:_ on_success _on_error =
    on_success {
      Social_core.access_token = "demo_access_token";
      refresh_token = Some "demo_refresh_token";
      expires_at = None;
      auth_type = Social_core.Bearer;
      scope = None;
    }

  let update_credentials ~account_id:_ ~credentials:_ on_success _on_error =
    on_success ()

  let encrypt _data on_success _on_error =
    on_success "encrypted"

  let decrypt _data on_success _on_error =
    on_success {|{"access_token":"demo","refresh_token":"demo_refresh"}|}

  let update_health_status ~account_id:_ ~status ~error_message on_success _on_error =
    Printf.printf "Health: %s%s\n%!" status
      (match error_message with Some m -> " - " ^ m | None -> "");
    on_success ()
end

(** Create provider *)
module GoogleBusiness = Social_google_business_v4.Make(Demo_config)

(** Example: Post with CPS style *)
let () =
  Printf.printf "=== Google Business Profile CPS Example ===\n\n";

  (* Validate content first *)
  Printf.printf "1. Validating content...\n";
  (match GoogleBusiness.validate_content ~text:"Check out our new spring menu!" with
  | Ok () -> Printf.printf "   Content valid\n"
  | Error e -> Printf.printf "   Invalid: %s\n" e);

  Printf.printf "\n2. Posting a standard update...\n";
  GoogleBusiness.post_single
    ~account_id:"demo"
    ~location_name:"locations/demo-location"
    ~text:"Check out our new spring menu! Fresh seasonal ingredients, locally sourced."
    ~media_urls:[]
    (function
      | Error_types.Success post_name ->
          Printf.printf "   Success: %s\n%!" post_name
      | Error_types.Partial_success { result = post_name; warnings } ->
          Printf.printf "   Success with warnings: %s\n%!" post_name;
          List.iter (fun w ->
            Printf.printf "     Warning: %s\n%!" (Error_types.warning_to_string w)
          ) warnings
      | Error_types.Failure err ->
          Printf.printf "   Failed: %s\n%!" (Error_types.error_to_string err));

  Printf.printf "\n=== Complete ===\n"
