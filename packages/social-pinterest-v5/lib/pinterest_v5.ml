(** Pinterest API v5 Provider
    
    This implementation supports Pinterest pin creation.
    
    - OAuth 2.0 with Basic Auth
    - Long-lived access tokens (no defined expiration)
    - Requires boards for all pins
    - Multipart image upload
*)

open Social_core

(** Configuration module type for Pinterest provider *)
module type CONFIG = sig
  module Http : HTTP_CLIENT
  
  val get_env : string -> string option
  val get_credentials : account_id:string -> (credentials -> unit) -> (string -> unit) -> unit
  val update_credentials : account_id:string -> credentials:credentials -> (unit -> unit) -> (string -> unit) -> unit
  val encrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val decrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val update_health_status : account_id:string -> status:string -> error_message:string option -> (unit -> unit) -> (string -> unit) -> unit
end

(** Make functor to create Pinterest provider with given configuration *)
module Make (Config : CONFIG) = struct
  let pinterest_api_base = "https://api.pinterest.com/v5"
  let pinterest_auth_url = "https://www.pinterest.com/oauth"
  let pinterest_token_url = "https://api.pinterest.com/v5/oauth/token"
  
  (** Ensure valid access token (Pinterest tokens are long-lived) *)
  let ensure_valid_token ~account_id on_success on_error =
    Config.get_credentials ~account_id
      (fun creds ->
        Config.update_health_status ~account_id ~status:"healthy" ~error_message:None
          (fun () -> on_success creds.access_token)
          on_error)
      on_error
  
  (** Get user's default board *)
  let get_default_board ~access_token on_success on_error =
    let url = pinterest_api_base ^ "/boards" in
    let headers = [
      ("Authorization", "Bearer " ^ access_token);
    ] in
    
    Config.Http.get ~headers url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let open Yojson.Basic.Util in
            let json = Yojson.Basic.from_string response.body in
            let boards = json |> member "items" |> to_list in
            match boards with
            | [] -> on_error "No Pinterest boards found - please create a board first"
            | first_board :: _ ->
                let board_id = first_board |> member "id" |> to_string in
                on_success board_id
          with e ->
            on_error (Printf.sprintf "Failed to parse boards: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "Failed to get boards (%d): %s" response.status response.body))
      on_error
  
  (** Upload image to Pinterest with optional alt text *)
  let upload_image ~access_token ~image_url ~alt_text on_success on_error =
    (* Download image first *)
    Config.Http.get ~headers:[] image_url
      (fun image_response ->
        if image_response.status >= 200 && image_response.status < 300 then
          let url = pinterest_api_base ^ "/media" in
          
          (* Create multipart form data *)
          let parts = [{
            name = "file";
            filename = Some "image.jpg";
            content_type = Some "image/jpeg";
            content = image_response.body;
          }] in
          
          let headers = [
            ("Authorization", "Bearer " ^ access_token);
          ] in
          
          Config.Http.post_multipart ~headers ~parts url
            (fun response ->
              if response.status >= 200 && response.status < 300 then
                try
                  let open Yojson.Basic.Util in
                  let json = Yojson.Basic.from_string response.body in
                  let media_id = json |> member "media_id" |> to_string in
                  on_success (media_id, alt_text)
                with e ->
                  on_error (Printf.sprintf "Failed to parse media response: %s" (Printexc.to_string e))
              else
                on_error (Printf.sprintf "Media upload error (%d): %s" response.status response.body))
            on_error
        else
          on_error (Printf.sprintf "Failed to download image (%d)" image_response.status))
      on_error
  
  (** Post to Pinterest *)
  let post_single ~account_id ~text ~media_urls ?(alt_texts=[]) on_success on_error =
    if List.length media_urls = 0 then
      on_error "Pinterest requires at least one image to create a pin"
    else
      ensure_valid_token ~account_id
        (fun access_token ->
          get_default_board ~access_token
            (fun board_id ->
              let image_url = List.hd media_urls in
              let alt_text = try List.nth alt_texts 0 with _ -> None in
              
              upload_image ~access_token ~image_url ~alt_text
                (fun (media_id, alt_text_opt) ->
                  (* Create pin with uploaded media *)
                  let url = pinterest_api_base ^ "/pins" in
                  
                  let base_fields = [
                    ("board_id", `String board_id);
                    ("title", `String (String.sub text 0 (min (String.length text) 100)));
                    ("description", `String text);
                    ("media_source", `Assoc [
                      ("source_type", `String "image_base64");
                      ("media_id", `String media_id);
                    ]);
                  ] in
                  
                  (* Add alt text if provided *)
                  let pin_fields = match alt_text_opt with
                    | Some alt when String.length alt > 0 ->
                        ("alt_text", `String alt) :: base_fields
                    | _ -> base_fields
                  in
                  
                  let pin_json = `Assoc pin_fields in
                  
                  let headers = [
                    ("Authorization", "Bearer " ^ access_token);
                    ("Content-Type", "application/json");
                  ] in
                  
                  let body = Yojson.Basic.to_string pin_json in
                  
                  Config.Http.post ~headers ~body url
                    (fun response ->
                      if response.status >= 200 && response.status < 300 then
                        try
                          let open Yojson.Basic.Util in
                          let json = Yojson.Basic.from_string response.body in
                          let pin_id = json |> member "id" |> to_string in
                          on_success pin_id
                        with e ->
                          on_error (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e))
                      else
                        on_error (Printf.sprintf "Pin creation failed (%d): %s" response.status response.body))
                    on_error)
                on_error)
            on_error)
        on_error
  
  (** Post thread (Pinterest doesn't support threads, posts only first item) *)
  let post_thread ~account_id ~texts ~media_urls_per_post ?(alt_texts_per_post=[]) on_success on_error =
    if List.length texts = 0 then
      on_error "No content to post"
    else
      let first_text = List.hd texts in
      let first_media = if List.length media_urls_per_post > 0 then List.hd media_urls_per_post else [] in
      let first_alt_texts = if List.length alt_texts_per_post > 0 then List.hd alt_texts_per_post else [] in
      post_single ~account_id ~text:first_text ~media_urls:first_media ~alt_texts:first_alt_texts
        (fun pin_id -> on_success [pin_id])
        on_error
  
  (** OAuth authorization URL *)
  let get_oauth_url ~redirect_uri ~state on_success on_error =
    let client_id = Config.get_env "PINTEREST_CLIENT_ID" |> Option.value ~default:"" in
    
    if client_id = "" then
      on_error "Pinterest client ID not configured"
    else (
      let scopes = "boards:read,pins:read,pins:write,user_accounts:read" in
      let params = [
        ("client_id", client_id);
        ("redirect_uri", redirect_uri);
        ("response_type", "code");
        ("scope", scopes);
        ("state", state);
      ] in
      
      let query = List.map (fun (k, v) -> 
        Printf.sprintf "%s=%s" k (Uri.pct_encode v)
      ) params |> String.concat "&" in
      
      let url = pinterest_auth_url ^ "?" ^ query in
      on_success url
    )
  
  (** Exchange OAuth code for access token *)
  let exchange_code ~code ~redirect_uri on_success on_error =
    let client_id = Config.get_env "PINTEREST_CLIENT_ID" |> Option.value ~default:"" in
    let client_secret = Config.get_env "PINTEREST_CLIENT_SECRET" |> Option.value ~default:"" in
    
    if client_id = "" || client_secret = "" then
      on_error "Pinterest OAuth credentials not configured"
    else (
      let url = pinterest_token_url in
      
      (* Pinterest requires Basic Auth: credentials in header *)
      let auth_string = String.trim client_id ^ ":" ^ String.trim client_secret in
      let auth_b64 = Base64.encode_exn auth_string in
      
      (* Body contains grant_type, code, and redirect_uri *)
      let body = Printf.sprintf
        "grant_type=authorization_code&code=%s&redirect_uri=%s"
        (Uri.pct_encode code)
        (Uri.pct_encode redirect_uri)
      in
      
      let headers = [
        ("Content-Type", "application/x-www-form-urlencoded");
        ("Authorization", "Basic " ^ auth_b64);
      ] in
      
      Config.Http.post ~headers ~body url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let access_token = json |> member "access_token" |> to_string in
              let refresh_token = 
                try Some (json |> member "refresh_token" |> to_string)
                with _ -> None
              in
              (* Pinterest tokens are long-lived (no expiry) *)
              let credentials = {
                access_token;
                refresh_token;
                expires_at = None;
                token_type = "Bearer";
              } in
              on_success credentials
            with e ->
              on_error (Printf.sprintf "Failed to parse OAuth response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "OAuth exchange failed (%d): %s" response.status response.body))
        on_error
    )
  
  (** Validate content length *)
  let validate_content ~text =
    let len = String.length text in
    if len = 0 then
      Error "Text cannot be empty"
    else if len > 500 then
      Error (Printf.sprintf "Pinterest description should be under 500 characters (current: %d)" len)
    else
      Ok ()
end
