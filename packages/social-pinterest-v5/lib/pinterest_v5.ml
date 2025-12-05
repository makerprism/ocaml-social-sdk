(** Pinterest API v5 Provider
    
    This implementation supports Pinterest pin creation.
    
    - OAuth 2.0 with Basic Auth
    - Long-lived access tokens (no defined expiration)
    - Requires boards for all pins
    - Multipart image upload
*)

open Social_core

(** OAuth 2.0 module for Pinterest API v5
    
    Pinterest uses OAuth 2.0 with Basic Authentication for token exchange.
    
    Key characteristics:
    - No PKCE support
    - Token exchange requires Basic Auth (client_id:client_secret in header)
    - Access tokens last 30 days by default
    - Refresh tokens last 365 days (can refresh indefinitely)
    - Scopes use colon-separated format (e.g., boards:read, pins:write)
    
    Required environment variables (or pass directly to functions):
    - PINTEREST_CLIENT_ID: OAuth 2.0 App ID from Pinterest Developer Portal
    - PINTEREST_CLIENT_SECRET: OAuth 2.0 App Secret
    - PINTEREST_REDIRECT_URI: Registered callback URL
*)
module OAuth = struct
  (** Scope definitions for Pinterest API v5 *)
  module Scopes = struct
    (** Scopes for reading Pinterest data *)
    let read = [
      "user_accounts:read";
      "boards:read";
      "pins:read";
    ]
    
    (** Scopes required for creating pins *)
    let write = [
      "user_accounts:read";
      "boards:read";
      "pins:read";
      "pins:write";
    ]
    
    (** All commonly used scopes for Pinterest management *)
    let all = [
      "user_accounts:read";
      "boards:read";
      "boards:write";
      "pins:read";
      "pins:write";
      "ads:read";
      "ads:write";
    ]
    
    (** Operations that can be performed with Pinterest API *)
    type operation = 
      | Create_pin
      | Read_pins
      | Read_boards
      | Write_boards
      | Manage_ads
    
    (** Get scopes required for specific operations *)
    let for_operations ops =
      let base = ["user_accounts:read"] in
      let needs_pins_write = List.exists (fun o -> o = Create_pin) ops in
      let needs_pins_read = List.exists (fun o -> o = Read_pins) ops in
      let needs_boards_read = List.exists (fun o -> o = Read_boards || o = Create_pin) ops in
      let needs_boards_write = List.exists (fun o -> o = Write_boards) ops in
      let needs_ads = List.exists (fun o -> o = Manage_ads) ops in
      base @
      (if needs_boards_read then ["boards:read"] else []) @
      (if needs_boards_write then ["boards:write"] else []) @
      (if needs_pins_read || needs_pins_write then ["pins:read"] else []) @
      (if needs_pins_write then ["pins:write"] else []) @
      (if needs_ads then ["ads:read"; "ads:write"] else [])
  end
  
  (** Platform metadata for Pinterest OAuth *)
  module Metadata = struct
    (** Pinterest does NOT support PKCE *)
    let supports_pkce = false
    
    (** Pinterest supports token refresh *)
    let supports_refresh = true
    
    (** Access tokens last 30 days *)
    let access_token_seconds = Some 2592000
    
    (** Refresh tokens last 365 days *)
    let refresh_token_seconds = Some 31536000
    
    (** Recommended buffer before expiry (7 days) *)
    let refresh_buffer_seconds = 604800
    
    (** Maximum retry attempts for token operations *)
    let max_refresh_attempts = 5
    
    (** Pinterest OAuth authorization endpoint *)
    let authorization_endpoint = "https://www.pinterest.com/oauth"
    
    (** Pinterest OAuth token endpoint *)
    let token_endpoint = "https://api.pinterest.com/v5/oauth/token"
    
    (** Pinterest API base URL *)
    let api_base = "https://api.pinterest.com/v5"
  end
  
  (** Generate authorization URL for Pinterest OAuth 2.0 flow
      
      @param client_id Pinterest App ID
      @param redirect_uri Registered callback URL
      @param state CSRF protection state parameter
      @param scopes OAuth scopes to request (defaults to Scopes.write)
      @return Full authorization URL to redirect user to
  *)
  let get_authorization_url ~client_id ~redirect_uri ~state ?(scopes=Scopes.write) () =
    let scope_str = String.concat "," scopes in
    let params = [
      ("client_id", client_id);
      ("redirect_uri", redirect_uri);
      ("state", state);
      ("scope", scope_str);
      ("response_type", "code");
    ] in
    let query = List.map (fun (k, v) -> 
      Printf.sprintf "%s=%s" k (Uri.pct_encode v)
    ) params |> String.concat "&" in
    Printf.sprintf "%s?%s" Metadata.authorization_endpoint query
  
  (** Make functor for OAuth operations that need HTTP client *)
  module Make (Http : HTTP_CLIENT) = struct
    (** Helper to create Basic Auth header
        
        Pinterest requires credentials in Basic Auth format for token operations.
    *)
    let make_basic_auth ~client_id ~client_secret =
      let auth_string = String.trim client_id ^ ":" ^ String.trim client_secret in
      "Basic " ^ Base64.encode_exn auth_string
    
    (** Exchange authorization code for access token
        
        Note: Pinterest uses Basic Authentication - client credentials go
        in the Authorization header, not in the request body.
        
        @param client_id Pinterest App ID
        @param client_secret Pinterest App Secret
        @param redirect_uri Registered callback URL
        @param code Authorization code from callback
        @param on_success Continuation receiving credentials
        @param on_error Continuation receiving error message
    *)
    let exchange_code ~client_id ~client_secret ~redirect_uri ~code on_success on_error =
      let body = Printf.sprintf
        "grant_type=authorization_code&code=%s&redirect_uri=%s"
        (Uri.pct_encode code)
        (Uri.pct_encode redirect_uri)
      in
      let headers = [
        ("Content-Type", "application/x-www-form-urlencoded");
        ("Authorization", make_basic_auth ~client_id ~client_secret);
      ] in
      
      Http.post ~headers ~body Metadata.token_endpoint
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let access_token = json |> member "access_token" |> to_string in
              let refresh_token = 
                try Some (json |> member "refresh_token" |> to_string)
                with _ -> None in
              let expires_in = 
                try json |> member "expires_in" |> to_int
                with _ -> 2592000 (* Default to 30 days *)
              in
              let expires_at = 
                let now = Ptime_clock.now () in
                match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
                | Some exp -> Some (Ptime.to_rfc3339 exp)
                | None -> None in
              let token_type = 
                try json |> member "token_type" |> to_string
                with _ -> "Bearer" in
              let creds : credentials = {
                access_token;
                refresh_token;
                expires_at;
                token_type;
              } in
              on_success creds
            with e ->
              on_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "Token exchange failed (%d): %s" response.status response.body))
        on_error
    
    (** Refresh access token
        
        Pinterest access tokens last 30 days, refresh tokens 365 days.
        You can refresh indefinitely as long as the refresh token hasn't expired.
        
        @param client_id Pinterest App ID
        @param client_secret Pinterest App Secret
        @param refresh_token The refresh token from initial auth
        @param on_success Continuation receiving refreshed credentials
        @param on_error Continuation receiving error message
    *)
    let refresh_token ~client_id ~client_secret ~refresh_token on_success on_error =
      let body = Printf.sprintf
        "grant_type=refresh_token&refresh_token=%s"
        (Uri.pct_encode refresh_token)
      in
      let headers = [
        ("Content-Type", "application/x-www-form-urlencoded");
        ("Authorization", make_basic_auth ~client_id ~client_secret);
      ] in
      
      Http.post ~headers ~body Metadata.token_endpoint
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let access_token = json |> member "access_token" |> to_string in
              let new_refresh_token = 
                try Some (json |> member "refresh_token" |> to_string)
                with _ -> Some refresh_token in  (* Keep old if not returned *)
              let expires_in = 
                try json |> member "expires_in" |> to_int
                with _ -> 2592000
              in
              let expires_at = 
                let now = Ptime_clock.now () in
                match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
                | Some exp -> Some (Ptime.to_rfc3339 exp)
                | None -> None in
              let token_type = 
                try json |> member "token_type" |> to_string
                with _ -> "Bearer" in
              let creds : credentials = {
                access_token;
                refresh_token = new_refresh_token;
                expires_at;
                token_type;
              } in
              on_success creds
            with e ->
              on_error (Printf.sprintf "Failed to parse refresh response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "Token refresh failed (%d): %s" response.status response.body))
        on_error
    
    (** Get user account info using access token
        
        @param access_token Valid access token
        @param on_success Continuation receiving user info as JSON
        @param on_error Continuation receiving error message
    *)
    let get_user_info ~access_token on_success on_error =
      let url = Metadata.api_base ^ "/user_account" in
      let headers = [
        ("Authorization", "Bearer " ^ access_token);
      ] in
      
      Http.get ~headers url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              on_success json
            with e ->
              on_error (Printf.sprintf "Failed to parse user info: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "Get user info failed (%d): %s" response.status response.body))
        on_error
    
    (** Get user's boards
        
        @param access_token Valid access token
        @param on_success Continuation receiving boards list as JSON
        @param on_error Continuation receiving error message
    *)
    let get_boards ~access_token on_success on_error =
      let url = Metadata.api_base ^ "/boards" in
      let headers = [
        ("Authorization", "Bearer " ^ access_token);
      ] in
      
      Http.get ~headers url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              on_success json
            with e ->
              on_error (Printf.sprintf "Failed to parse boards: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "Get boards failed (%d): %s" response.status response.body))
        on_error
  end
end

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
