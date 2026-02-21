(** Reddit API v1 Provider
    
    This implementation supports posting to Reddit subreddits.
    
    Key characteristics:
    - OAuth 2.0 with Basic Auth (similar to Pinterest)
    - Access tokens expire in 1 hour
    - Refresh tokens are long-lived (use duration=permanent)
    - Posts go to subreddits, not user timelines
    - All posts require a title (max 300 chars)
    - Supports self-posts (text), link posts, images, videos, and galleries
    
    Required scopes: submit, read, mysubreddits, flair, modposts
*)

open Social_core

(** {1 Platform Constants} *)

(** Maximum title length for Reddit posts *)
let max_title_length = 300

(** Maximum body length for self-posts *)
let max_body_length = 40_000

(** Maximum image size in bytes (20MB) *)
let max_image_size_bytes = 20 * 1024 * 1024

(** Maximum video size in bytes (1GB) *)
let max_video_size_bytes = 1024 * 1024 * 1024

(** Maximum video duration in seconds (15 minutes) *)
let max_video_duration_seconds = 900

(** Maximum images in a gallery *)
let max_gallery_images = 20

(** {1 Types} *)

(** Reddit post types *)
type post_kind =
  | Self      (** Text post with title + body *)
  | Link      (** Link post with title + URL *)
  | Image     (** Image post with title + uploaded image *)
  | Video     (** Video post with title + uploaded video *)
  | Gallery   (** Gallery post with title + multiple images *)
  | Crosspost (** Crosspost from another subreddit *)

(** Subreddit information *)
type subreddit = {
  id: string;                     (** Subreddit ID (without t5_ prefix) *)
  name: string;                   (** Subreddit name (without r/ prefix) *)
  display_name: string;           (** Display name *)
  subscribers: int;               (** Number of subscribers *)
  over18: bool;                   (** Whether the subreddit is NSFW *)
  user_is_moderator: bool;        (** Whether authenticated user is a moderator *)
  submission_type: string option; (** Allowed submission type: any, link, self *)
  flair_enabled: bool;            (** Whether link flair is enabled *)
}

(** Flair information *)
type flair = {
  flair_id: string;               (** Flair template ID *)
  flair_text: string;             (** Flair text *)
  flair_text_editable: bool;      (** Whether user can edit the text *)
  flair_css_class: string option; (** CSS class for styling *)
  background_color: string option;(** Background color *)
  text_color: string option;      (** Text color: light or dark *)
}

(** Reddit user info *)
type user_info = {
  id: string;                     (** User ID (without t2_ prefix) *)
  name: string;                   (** Username *)
  icon_img: string option;        (** Profile icon URL *)
  total_karma: int;               (** Total karma *)
  created_utc: float;             (** Account creation timestamp *)
}

(** Submitted post result *)
type submitted_post = {
  id: string;                     (** Post ID (without t3_ prefix) *)
  full_id: string;                (** Full ID (t3_xxx) *)
  url: string;                    (** Reddit URL to the post *)
  subreddit: string;              (** Subreddit name *)
}

(** Gallery item for gallery posts *)
type gallery_item = {
  media_url: string;              (** URL of the image to upload *)
  caption: string option;         (** Optional caption for this image *)
  outbound_url: string option;    (** Optional outbound link for this image *)
}

(** Rate limit info parsed from Reddit response headers *)
type rate_limit_headers = {
  used: float option;             (** X-Ratelimit-Used: requests used in this period *)
  remaining: float option;        (** X-Ratelimit-Remaining: requests remaining *)
  reset: int option;              (** X-Ratelimit-Reset: seconds until period reset *)
}

(** Multi-subreddit post result *)
type multi_post_result = {
  subreddit: string;              (** Subreddit name *)
  post_id: string option;         (** Post ID if successful *)
  error: string option;           (** Error message if failed *)
}

(** Media upload result *)
type media_upload = {
  asset_id: string;               (** Asset ID from Reddit *)
  websocket_url: string option;   (** WebSocket URL for upload status (videos) *)
  upload_url: string;             (** S3 presigned URL for upload *)
  upload_fields: (string * string) list; (** S3 form fields returned by Reddit *)
}

(** {1 OAuth Module} *)

module OAuth = struct
  (** Scope definitions for Reddit API *)
  module Scopes = struct
    (** Scopes required for posting to subreddits *)
    let posting = [
      "submit";        (* Submit posts *)
      "read";          (* Read content *)
      "mysubreddits";  (* Access moderated/subscribed subreddits *)
      "flair";         (* Set flair on posts *)
    ]
    
    (** Additional scopes for moderation *)
    let moderation = posting @ [
      "modposts";      (* Moderate posts *)
    ]
    
    (** All commonly used scopes *)
    let all = [
      "identity";
      "submit";
      "read";
      "mysubreddits";
      "flair";
      "modposts";
      "edit";
      "history";
    ]
    
    (** Operations that can be performed *)
    type operation =
      | Post_text
      | Post_link
      | Post_media
      | Post_crosspost
      | Read_subreddits
      | Manage_flair
      | Moderate
    
    (** Get scopes required for specific operations *)
    let for_operations ops =
      let needs_submit = List.exists (fun o -> 
        o = Post_text || o = Post_link || o = Post_media || o = Post_crosspost
      ) ops in
      let needs_read = List.exists (fun o -> 
        o = Read_subreddits || o = Post_crosspost
      ) ops in
      let needs_mysubreddits = List.exists (fun o -> 
        o = Read_subreddits
      ) ops in
      let needs_flair = List.exists (fun o -> o = Manage_flair) ops in
      let needs_modposts = List.exists (fun o -> o = Moderate) ops in
      (if needs_submit then ["submit"] else []) @
      (if needs_read then ["read"] else []) @
      (if needs_mysubreddits then ["mysubreddits"] else []) @
      (if needs_flair then ["flair"] else []) @
      (if needs_modposts then ["modposts"] else [])
  end
  
  (** Platform metadata for Reddit OAuth *)
  module Metadata = struct
    (** Reddit does NOT support PKCE *)
    let supports_pkce = false
    
    (** Reddit supports token refresh *)
    let supports_refresh = true
    
    (** Access tokens last 1 hour (3600 seconds) *)
    let access_token_seconds = Some 3600
    
    (** Refresh tokens are long-lived (no defined expiration) *)
    let refresh_token_seconds = None
    
    (** Recommended buffer before expiry (5 minutes) *)
    let refresh_buffer_seconds = 300
    
    (** Maximum retry attempts for token operations *)
    let max_refresh_attempts = 3
    
    (** Reddit OAuth authorization endpoint *)
    let authorization_endpoint = "https://www.reddit.com/api/v1/authorize"
    
    (** Reddit OAuth token endpoint *)
    let token_endpoint = "https://www.reddit.com/api/v1/access_token"
    
    (** Reddit API base URL (oauth.reddit.com for authenticated requests) *)
    let api_base = "https://oauth.reddit.com"
    
    (** Reddit public API base URL *)
    let public_api_base = "https://www.reddit.com"
  end
  
  (** Generate authorization URL for Reddit OAuth 2.0 flow
      
      @param client_id Reddit App ID
      @param redirect_uri Registered callback URL
      @param state CSRF protection state parameter
      @param scopes OAuth scopes to request (defaults to Scopes.posting)
      @param duration Token duration: "temporary" or "permanent" (for refresh tokens)
      @return Full authorization URL to redirect user to
  *)
  let get_authorization_url ~client_id ~redirect_uri ~state 
      ?(scopes=Scopes.posting) ?(duration="permanent") () =
    let scope_str = String.concat " " scopes in
    let params = [
      ("client_id", client_id);
      ("redirect_uri", redirect_uri);
      ("state", state);
      ("scope", scope_str);
      ("response_type", "code");
      ("duration", duration);
    ] in
    let query = List.map (fun (k, v) -> 
      Printf.sprintf "%s=%s" k (Uri.pct_encode v)
    ) params |> String.concat "&" in
    Printf.sprintf "%s?%s" Metadata.authorization_endpoint query
  
  (** OAuth operations requiring HTTP client *)
  module Make (Http : HTTP_CLIENT) = struct
    (** Helper to create Basic Auth header
        Reddit requires credentials in Basic Auth format for token operations.
    *)
    let make_basic_auth ~client_id ~client_secret =
      let auth_string = String.trim client_id ^ ":" ^ String.trim client_secret in
      "Basic " ^ Base64.encode_exn auth_string
    
    (** Exchange authorization code for access token
        
        Note: Reddit uses Basic Authentication - client credentials go
        in the Authorization header, not in the request body.
        
        @param client_id Reddit App ID
        @param client_secret Reddit App Secret
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
                with _ -> 3600 (* Default to 1 hour *)
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
        
        Reddit access tokens last 1 hour. Refresh tokens last indefinitely
        as long as they're used within 1 year.
        
        @param client_id Reddit App ID
        @param client_secret Reddit App Secret
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
                with _ -> 3600
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
        @param on_success Continuation receiving user info
        @param on_error Continuation receiving error message
    *)
    let get_user_info ~access_token on_success on_error =
      let url = Metadata.api_base ^ "/api/v1/me" in
      let headers = [
        ("Authorization", "Bearer " ^ access_token);
        ("User-Agent", "ocaml:social-reddit-v1:v0.1.0");
      ] in
      
      Http.get ~headers url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let open Yojson.Basic.Util in
              let json = Yojson.Basic.from_string response.body in
              let user : user_info = {
                id = json |> member "id" |> to_string;
                name = json |> member "name" |> to_string;
                icon_img = (try Some (json |> member "icon_img" |> to_string) with _ -> None);
                total_karma = (try json |> member "total_karma" |> to_int with _ -> 0);
                created_utc = (try json |> member "created_utc" |> to_float with _ -> 0.0);
              } in
              on_success user
            with e ->
              on_error (Printf.sprintf "Failed to parse user info: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "Get user info failed (%d): %s" response.status response.body))
        on_error
    
    (** Revoke access token
        
        @param client_id Reddit App ID
        @param client_secret Reddit App Secret
        @param token The token to revoke (access or refresh)
        @param token_type_hint "access_token" or "refresh_token"
        @param on_success Continuation called on successful revocation
        @param on_error Continuation receiving error message
    *)
    let revoke_token ~client_id ~client_secret ~token ?(token_type_hint="access_token") on_success on_error =
      let url = "https://www.reddit.com/api/v1/revoke_token" in
      let body = Printf.sprintf
        "token=%s&token_type_hint=%s"
        (Uri.pct_encode token)
        token_type_hint
      in
      let headers = [
        ("Content-Type", "application/x-www-form-urlencoded");
        ("Authorization", make_basic_auth ~client_id ~client_secret);
      ] in
      
      Http.post ~headers ~body url
        (fun response ->
          (* Reddit returns 204 No Content on success *)
          if response.status >= 200 && response.status < 300 then
            on_success ()
          else
            on_error (Printf.sprintf "Token revocation failed (%d): %s" response.status response.body))
        on_error
  end
end

(** {1 Configuration Interface} *)

module type CONFIG = sig
  module Http : HTTP_CLIENT
  
  val get_env : string -> string option
  val get_credentials : account_id:string -> (credentials -> unit) -> (string -> unit) -> unit
  val update_credentials : account_id:string -> credentials:credentials -> (unit -> unit) -> (string -> unit) -> unit
  val encrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val decrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val update_health_status : account_id:string -> status:string -> error_message:string option -> (unit -> unit) -> (string -> unit) -> unit
end

(** {1 Main Provider Functor} *)

module Make (Config : CONFIG) = struct
  let api_base = "https://oauth.reddit.com"
  
  (** Required User-Agent for Reddit API *)
  let user_agent = "ocaml:social-reddit-v1:v0.1.0"
  
  (** {1 Validation Functions} *)
  
  (** Validate a post title *)
  let validate_title title =
    let errors = ref [] in
    let len = String.length title in
    if len = 0 then
      errors := Error_types.Text_empty :: !errors
    else if len > max_title_length then
      errors := Error_types.Text_too_long { length = len; max = max_title_length } :: !errors;
    if !errors = [] then Ok ()
    else Error (List.rev !errors)
  
  (** Validate a post body (for self-posts) *)
  let validate_body body =
    let errors = ref [] in
    let len = String.length body in
    if len > max_body_length then
      errors := Error_types.Text_too_long { length = len; max = max_body_length } :: !errors;
    if !errors = [] then Ok ()
    else Error (List.rev !errors)
  
  (** Validate a complete post *)
  let validate_post ~title ?body ?url ?(media_count=0) () =
    let errors = ref [] in
    
    (* Title is always required *)
    (match validate_title title with
     | Error errs -> errors := !errors @ errs
     | Ok () -> ());
    
    (* Body validation for self-posts *)
    (match body with
     | Some b -> 
         (match validate_body b with
          | Error errs -> errors := !errors @ errs
          | Ok () -> ())
     | None -> ());
    
    (* URL validation for link posts *)
    (match url with
     | Some u when String.length u > 0 ->
         (* Basic URL validation *)
         if not (String.sub u 0 (min 4 (String.length u)) = "http") then
           errors := Error_types.Invalid_url u :: !errors
     | _ -> ());
    
    (* Gallery validation *)
    if media_count > max_gallery_images then
      errors := Error_types.Too_many_media { count = media_count; max = max_gallery_images } :: !errors;
    
    if !errors = [] then Ok ()
    else Error (List.rev !errors)
  
  (** Validate media file for Reddit *)
  let validate_media ~(media : Platform_types.post_media) =
    match media.Platform_types.media_type with
    | Platform_types.Image ->
        if media.file_size_bytes > max_image_size_bytes then
          Error [Error_types.Media_too_large { 
            size_bytes = media.file_size_bytes; 
            max_bytes = max_image_size_bytes 
          }]
        else
          Ok ()
    | Platform_types.Video ->
        if media.file_size_bytes > max_video_size_bytes then
          Error [Error_types.Media_too_large { 
            size_bytes = media.file_size_bytes; 
            max_bytes = max_video_size_bytes 
          }]
        else
          (match media.duration_seconds with
           | Some d when d > float_of_int max_video_duration_seconds ->
               Error [Error_types.Video_too_long { 
                 duration_seconds = d; 
                 max_seconds = max_video_duration_seconds 
               }]
           | _ -> Ok ())
    | Platform_types.Gif ->
        if media.file_size_bytes > max_image_size_bytes then
          Error [Error_types.Media_too_large { 
            size_bytes = media.file_size_bytes; 
            max_bytes = max_image_size_bytes 
          }]
        else
          Ok ()
  
  (** {1 Error Handling} *)
  
  (** Parse Reddit API error response *)
  let parse_api_error ~status_code ~response_body =
    try
      let json = Yojson.Basic.from_string response_body in
      let open Yojson.Basic.Util in
      
      (* Check for JSON errors format *)
      let json_errors = 
        try json |> member "json" |> member "errors" |> to_list
        with _ -> []
      in
      
      if List.length json_errors > 0 then
        let first_error = List.hd json_errors |> to_list in
        let error_code = List.nth first_error 0 |> to_string in
        let error_msg = List.nth first_error 1 |> to_string in
        
        (* Map Reddit error codes to SDK error types *)
        match error_code with
        | "RATELIMIT" ->
            (* Parse "try again in X minutes/seconds" - handles both singular and plural *)
            let retry_after = 
              if String.length error_msg > 0 then
                try 
                  let regex = Str.regexp {|in \([0-9]+\) \(minute\|second\|millisecond\)|} in
                  let _ = Str.search_forward regex error_msg 0 in
                  let num = int_of_string (Str.matched_group 1 error_msg) in
                  let unit_str = Str.matched_group 2 error_msg in
                  Some (if unit_str = "minute" then num * 60 
                        else if unit_str = "millisecond" then 1  (* round up to 1 second *)
                        else num)
                with Not_found -> None
                   | _ -> None
              else None
            in
            Error_types.Rate_limited { 
              retry_after_seconds = retry_after;
              limit = None;
              remaining = Some 0;
              reset_at = None;
            }
        | "USER_REQUIRED" | "INVALID_TOKEN" ->
            Error_types.Auth_error Error_types.Token_invalid
        | "SUBREDDIT_NOTALLOWED" | "SUBREDDIT_NOEXIST" ->
            Error_types.Auth_error (Error_types.Insufficient_permissions ["subreddit_access"])
        | "NO_SELFS" ->
            Error_types.Api_error {
              status_code;
              message = "This subreddit doesn't allow text posts";
              platform = Platform_types.Reddit;
              raw_response = Some response_body;
              request_id = None;
            }
        | "NO_LINKS" ->
            Error_types.Api_error {
              status_code;
              message = "This subreddit doesn't allow link posts";
              platform = Platform_types.Reddit;
              raw_response = Some response_body;
              request_id = None;
            }
        | "BAD_SR_NAME" ->
            Error_types.Resource_not_found ("subreddit: " ^ error_msg)
        | "ALREADY_SUB" ->
            Error_types.Duplicate_content
        | _ ->
            Error_types.Api_error {
              status_code;
              message = error_msg;
              platform = Platform_types.Reddit;
              raw_response = Some response_body;
              request_id = None;
            }
      else
        (* Check for simple error format *)
        let error_msg = 
          try json |> member "message" |> to_string
          with _ -> 
            try json |> member "error" |> to_string
            with _ -> response_body
        in
        
        if status_code = 401 then
          Error_types.Auth_error Error_types.Token_invalid
        else if status_code = 403 then
          Error_types.Auth_error (Error_types.Insufficient_permissions [])
        else if status_code = 429 then
          Error_types.Rate_limited { 
            retry_after_seconds = Some 60;
            limit = None;
            remaining = Some 0;
            reset_at = None;
          }
        else
          Error_types.Api_error {
            status_code;
            message = error_msg;
            platform = Platform_types.Reddit;
            raw_response = Some response_body;
            request_id = None;
          }
    with _ ->
      Error_types.Api_error {
        status_code;
        message = response_body;
        platform = Platform_types.Reddit;
        raw_response = Some response_body;
        request_id = None;
      }
  
  (** {1 Token Management} *)
  
  (** Check if token needs refresh *)
  let token_needs_refresh creds =
    match creds.expires_at with
    | None -> false  (* No expiry info, assume valid *)
    | Some expires_at_str ->
        try
          match Ptime.of_rfc3339 expires_at_str with
          | Ok (exp_time, _, _) ->
              let now = Ptime_clock.now () in
              let buffer = Ptime.Span.of_int_s OAuth.Metadata.refresh_buffer_seconds in
              (match Ptime.sub_span exp_time buffer with
               | Some threshold -> Ptime.is_earlier now ~than:threshold |> not
               | None -> true)
          | Error _ -> false
        with _ -> false
  
  (** Ensure we have a valid access token, refreshing if necessary *)
  let ensure_valid_token ~account_id on_success on_error =
    Config.get_credentials ~account_id
      (fun creds ->
        if token_needs_refresh creds then
          match creds.refresh_token with
          | Some refresh_tok ->
              let client_id = Config.get_env "REDDIT_CLIENT_ID" |> Option.value ~default:"" in
              let client_secret = Config.get_env "REDDIT_CLIENT_SECRET" |> Option.value ~default:"" in
              if client_id = "" || client_secret = "" then
                on_error (Error_types.Auth_error Error_types.Missing_credentials)
              else
                let module OAuthHttp = OAuth.Make(Config.Http) in
                OAuthHttp.refresh_token ~client_id ~client_secret ~refresh_token:refresh_tok
                  (fun new_creds ->
                    Config.update_credentials ~account_id ~credentials:new_creds
                      (fun () ->
                        Config.update_health_status ~account_id ~status:"healthy" ~error_message:None
                          (fun () -> on_success new_creds.access_token)
                          (fun _ -> on_success new_creds.access_token))
                      (fun err ->
                        Config.update_health_status ~account_id ~status:"refresh_failed" ~error_message:(Some err)
                          (fun () -> on_error (Error_types.Auth_error (Error_types.Refresh_failed err)))
                          (fun _ -> on_error (Error_types.Auth_error (Error_types.Refresh_failed err)))))
                  (fun err ->
                    Config.update_health_status ~account_id ~status:"refresh_failed" ~error_message:(Some err)
                      (fun () -> on_error (Error_types.Auth_error (Error_types.Refresh_failed err)))
                      (fun _ -> on_error (Error_types.Auth_error (Error_types.Refresh_failed err))))
          | None ->
              on_error (Error_types.Auth_error Error_types.Token_expired)
        else
          Config.update_health_status ~account_id ~status:"healthy" ~error_message:None
            (fun () -> on_success creds.access_token)
            (fun _ -> on_success creds.access_token))
      (fun err -> on_error (Error_types.Auth_error (Error_types.Refresh_failed err)))
  
  (** {1 API Functions} *)
  
  (** Get moderated subreddits for the authenticated user *)
  let get_moderated_subreddits ~account_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let url = api_base ^ "/subreddits/mine/moderator?limit=100" in
        let headers = [
          ("Authorization", "Bearer " ^ access_token);
          ("User-Agent", user_agent);
        ] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let open Yojson.Basic.Util in
                let json = Yojson.Basic.from_string response.body in
                let children = json |> member "data" |> member "children" |> to_list in
                let subreddits = List.map (fun child ->
                  let data = child |> member "data" in
                  {
                    id = data |> member "id" |> to_string;
                    name = data |> member "display_name" |> to_string;
                    display_name = data |> member "display_name" |> to_string;
                    subscribers = (try data |> member "subscribers" |> to_int with _ -> 0);
                    over18 = (try data |> member "over18" |> to_bool with _ -> false);
                    user_is_moderator = true;
                    submission_type = (try Some (data |> member "submission_type" |> to_string) with _ -> None);
                    flair_enabled = (try data |> member "link_flair_enabled" |> to_bool with _ -> false);
                  }
                ) children in
                on_result (Error_types.Success subreddits)
              with e ->
                on_result (Error_types.Failure (Error_types.Internal_error (Printf.sprintf "Failed to parse subreddits: %s" (Printexc.to_string e))))
            else
              on_result (Error_types.Failure (parse_api_error ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err))))
      (fun err -> on_result (Error_types.Failure err))
  
  (** Get available flairs for a subreddit *)
  let get_subreddit_flairs ~account_id ~subreddit on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let url = Printf.sprintf "%s/r/%s/api/link_flair_v2" api_base subreddit in
        let headers = [
          ("Authorization", "Bearer " ^ access_token);
          ("User-Agent", user_agent);
        ] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let open Yojson.Basic.Util in
                let json = Yojson.Basic.from_string response.body in
                let flair_list = to_list json in
                let flairs = List.map (fun f ->
                  {
                    flair_id = f |> member "id" |> to_string;
                    flair_text = (try f |> member "text" |> to_string with _ -> "");
                    flair_text_editable = (try f |> member "text_editable" |> to_bool with _ -> false);
                    flair_css_class = (try Some (f |> member "css_class" |> to_string) with _ -> None);
                    background_color = (try Some (f |> member "background_color" |> to_string) with _ -> None);
                    text_color = (try Some (f |> member "text_color" |> to_string) with _ -> None);
                  }
                ) flair_list in
                on_result (Error_types.Success flairs)
              with e ->
                on_result (Error_types.Failure (Error_types.Internal_error (Printf.sprintf "Failed to parse flairs: %s" (Printexc.to_string e))))
            else
              on_result (Error_types.Failure (parse_api_error ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err))))
      (fun err -> on_result (Error_types.Failure err))

  (** Check if a subreddit requires flair for posts *)
  let check_flair_required ~account_id ~subreddit on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let url = Printf.sprintf "%s/api/v1/%s/post_requirements" api_base subreddit in
        let headers = [
          ("Authorization", "Bearer " ^ access_token);
          ("User-Agent", user_agent);
        ] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let open Yojson.Basic.Util in
                let json = Yojson.Basic.from_string response.body in
                let flair_required = 
                  try json |> member "is_flair_required" |> to_bool
                  with _ -> false
                in
                on_result (Error_types.Success flair_required)
              with e ->
                on_result (Error_types.Failure (Error_types.Internal_error (Printf.sprintf "Failed to parse post requirements: %s" (Printexc.to_string e))))
            else if response.status = 404 then
              (* Post requirements endpoint may not exist for all subreddits *)
              on_result (Error_types.Success false)
            else
              on_result (Error_types.Failure (parse_api_error ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err))))
      (fun err -> on_result (Error_types.Failure err))
  
  (** Submit a self-post (text post) to a subreddit *)
  let submit_self_post ~account_id ~subreddit ~title ?body 
      ?flair_id ?flair_text ?(nsfw=false) ?(spoiler=false) () on_result =
    (* Validate *)
    match validate_post ~title ?body () with
    | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        ensure_valid_token ~account_id
          (fun access_token ->
            let url = api_base ^ "/api/submit" in
            
            (* Build form data *)
            let form_params = [
              ("api_type", "json");
              ("kind", "self");
              ("sr", subreddit);
              ("title", title);
              ("text", Option.value ~default:"" body);
              ("nsfw", string_of_bool nsfw);
              ("spoiler", string_of_bool spoiler);
            ] in
            
            (* Add optional flair *)
            let form_params = match flair_id with
              | Some id -> ("flair_id", id) :: form_params
              | None -> form_params
            in
            let form_params = match flair_text with
              | Some text -> ("flair_text", text) :: form_params
              | None -> form_params
            in
            
            let body = List.map (fun (k, v) ->
              Printf.sprintf "%s=%s" k (Uri.pct_encode v)
            ) form_params |> String.concat "&" in
            
            let headers = [
              ("Authorization", "Bearer " ^ access_token);
              ("User-Agent", user_agent);
              ("Content-Type", "application/x-www-form-urlencoded");
            ] in
            
            Config.Http.post ~headers ~body url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  try
                    let open Yojson.Basic.Util in
                    let json = Yojson.Basic.from_string response.body in
                    
                    (* Check for errors in the JSON response *)
                    let errors = 
                      try json |> member "json" |> member "errors" |> to_list
                      with _ -> []
                    in
                    
                    if List.length errors > 0 then
                      on_result (Error_types.Failure (parse_api_error ~status_code:200 ~response_body:response.body))
                    else
                      let data = json |> member "json" |> member "data" in
                      let post : submitted_post = {
                        id = data |> member "id" |> to_string;
                        full_id = data |> member "name" |> to_string;
                        url = data |> member "url" |> to_string;
                        subreddit;
                      } in
                      on_result (Error_types.Success post.id)
                  with e ->
                    on_result (Error_types.Failure (Error_types.Internal_error (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e))))
                else
                  on_result (Error_types.Failure (parse_api_error ~status_code:response.status ~response_body:response.body)))
              (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err))))
          (fun err -> on_result (Error_types.Failure err))
  
  (** Submit a link post to a subreddit *)
  let submit_link_post ~account_id ~subreddit ~title ~url:link_url 
      ?flair_id ?flair_text ?(nsfw=false) ?(spoiler=false) ?(resubmit=false) () on_result =
    (* Validate *)
    match validate_post ~title ~url:link_url () with
    | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        ensure_valid_token ~account_id
          (fun access_token ->
            let api_url = api_base ^ "/api/submit" in
            
            (* Build form data *)
            let form_params = [
              ("api_type", "json");
              ("kind", "link");
              ("sr", subreddit);
              ("title", title);
              ("url", link_url);
              ("nsfw", string_of_bool nsfw);
              ("spoiler", string_of_bool spoiler);
              ("resubmit", string_of_bool resubmit);
            ] in
            
            (* Add optional flair *)
            let form_params = match flair_id with
              | Some id -> ("flair_id", id) :: form_params
              | None -> form_params
            in
            let form_params = match flair_text with
              | Some text -> ("flair_text", text) :: form_params
              | None -> form_params
            in
            
            let body = List.map (fun (k, v) ->
              Printf.sprintf "%s=%s" k (Uri.pct_encode v)
            ) form_params |> String.concat "&" in
            
            let headers = [
              ("Authorization", "Bearer " ^ access_token);
              ("User-Agent", user_agent);
              ("Content-Type", "application/x-www-form-urlencoded");
            ] in
            
            Config.Http.post ~headers ~body api_url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  try
                    let open Yojson.Basic.Util in
                    let json = Yojson.Basic.from_string response.body in
                    
                    let errors = 
                      try json |> member "json" |> member "errors" |> to_list
                      with _ -> []
                    in
                    
                    if List.length errors > 0 then
                      on_result (Error_types.Failure (parse_api_error ~status_code:200 ~response_body:response.body))
                    else
                      let data = json |> member "json" |> member "data" in
                      let post : submitted_post = {
                        id = data |> member "id" |> to_string;
                        full_id = data |> member "name" |> to_string;
                        url = data |> member "url" |> to_string;
                        subreddit;
                      } in
                      on_result (Error_types.Success post.id)
                  with e ->
                    on_result (Error_types.Failure (Error_types.Internal_error (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e))))
                else
                  on_result (Error_types.Failure (parse_api_error ~status_code:response.status ~response_body:response.body)))
              (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err))))
          (fun err -> on_result (Error_types.Failure err))
  
  (** Get media upload lease from Reddit (for image/video posts) *)
  let get_media_upload_lease ~account_id ~filename ~mimetype on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let url = api_base ^ "/api/media/asset.json" in
        
        let form_params = [
          ("filepath", filename);
          ("mimetype", mimetype);
        ] in
        
        let body = List.map (fun (k, v) ->
          Printf.sprintf "%s=%s" k (Uri.pct_encode v)
        ) form_params |> String.concat "&" in
        
        let headers = [
          ("Authorization", "Bearer " ^ access_token);
          ("User-Agent", user_agent);
          ("Content-Type", "application/x-www-form-urlencoded");
        ] in
        
        Config.Http.post ~headers ~body url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let open Yojson.Basic.Util in
                let json = Yojson.Basic.from_string response.body in
                let args = json |> member "args" in
                let action_url = args |> member "action" |> to_string in
                let upload_url =
                  if String.length action_url > 1 && String.sub action_url 0 2 = "//" then
                    "https:" ^ action_url
                  else
                    action_url
                in
                let upload_fields =
                  try
                    args
                    |> member "fields"
                    |> to_list
                    |> List.map (fun f ->
                      (f |> member "name" |> to_string, f |> member "value" |> to_string)
                    )
                  with _ -> []
                in
                let asset_id =
                  try args |> member "asset" |> member "asset_id" |> to_string
                  with _ -> json |> member "asset" |> member "asset_id" |> to_string
                in
                let websocket_url =
                  try Some (args |> member "asset" |> member "websocket_url" |> to_string)
                  with _ ->
                    try Some (json |> member "asset" |> member "websocket_url" |> to_string)
                    with _ -> None
                in
                let upload : media_upload = {
                  asset_id;
                  websocket_url;
                  upload_url;
                  upload_fields;
                } in
                on_result (Error_types.Success upload)
              with e ->
                on_result (Error_types.Failure (Error_types.Internal_error (Printf.sprintf "Failed to parse media lease: %s" (Printexc.to_string e))))
            else
              on_result (Error_types.Failure (parse_api_error ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err))))
      (fun err -> on_result (Error_types.Failure err))
  
  (** Upload media to Reddit's S3 bucket *)
  let upload_media_to_s3 ~upload_url ~upload_fields ~media_content ~mimetype on_result =
    (* Build multipart form with upload fields + file *)
    let field_parts = List.map (fun (name, value) ->
      { name; filename = None; content_type = None; content = value }
    ) upload_fields in
    
    let file_part = {
      name = "file";
      filename = Some "media";
      content_type = Some mimetype;
      content = media_content;
    } in
    
    let parts = field_parts @ [file_part] in
    
    Config.Http.post_multipart ~headers:[] ~parts upload_url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          on_result (Error_types.Success ())
        else
          on_result (Error_types.Failure (Error_types.Internal_error (Printf.sprintf "S3 upload failed (%d): %s" response.status response.body))))
      (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err)))

  let infer_filename_from_url media_url default_name =
    let stop_at ch default =
      try String.index media_url ch with Not_found -> default
    in
    let stop_q = stop_at '?' (String.length media_url) in
    let stop_h = stop_at '#' (String.length media_url) in
    let stop = min stop_q stop_h in
    let trimmed = String.sub media_url 0 stop in
    try
      let idx = String.rindex trimmed '/' in
      let candidate = String.sub trimmed (idx + 1) (String.length trimmed - idx - 1) in
      if String.length candidate = 0 then default_name else candidate
    with Not_found ->
      if String.length trimmed = 0 then default_name else trimmed

  let uploaded_media_url lease =
    match List.assoc_opt "key" lease.upload_fields with
    | Some key when String.length key > 0 ->
        let base =
          if String.length lease.upload_url > 0 && lease.upload_url.[String.length lease.upload_url - 1] = '/' then
            String.sub lease.upload_url 0 (String.length lease.upload_url - 1)
          else
            lease.upload_url
        in
        base ^ "/" ^ key
    | _ -> lease.upload_url

  let normalize_content_type content_type =
    let lowered = String.lowercase_ascii content_type in
    let without_params =
      try
        let idx = String.index lowered ';' in
        String.sub lowered 0 idx
      with Not_found -> lowered
    in
    String.trim without_params

  let download_media_url ~url on_result =
    Config.Http.get ~headers:[] url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          let mimetype =
            List.assoc_opt "content-type" response.headers
            |> Option.value ~default:"application/octet-stream"
            |> normalize_content_type
          in
          on_result (Error_types.Success (response.body, mimetype))
        else
          on_result (Error_types.Failure (Error_types.Api_error {
            status_code = response.status;
            message = Printf.sprintf "Failed to download media URL (%d)" response.status;
            platform = Platform_types.Reddit;
            raw_response = Some response.body;
            request_id = None;
          })))
      (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err)))

  (** Submit a native video post by uploading media then posting kind=video *)
  let submit_video_post ~account_id ~subreddit ~title ~video_url
      ?thumbnail_url ?flair_id ?flair_text ?(nsfw=false) ?(spoiler=false) () on_result =
    let submit_video_request ~access_token ~posted_video_url ?video_poster_url () =
      let api_url = api_base ^ "/api/submit" in
      let base_params = [
        ("api_type", "json");
        ("kind", "video");
        ("sr", subreddit);
        ("title", title);
        ("url", posted_video_url);
        ("nsfw", string_of_bool nsfw);
        ("spoiler", string_of_bool spoiler);
      ] in
      let form_params =
        match video_poster_url with
        | Some poster -> ("video_poster_url", poster) :: base_params
        | None -> base_params
      in
      let form_params = match flair_id with
        | Some id -> ("flair_id", id) :: form_params
        | None -> form_params
      in
      let form_params = match flair_text with
        | Some text -> ("flair_text", text) :: form_params
        | None -> form_params
      in
      let body = List.map (fun (k, v) ->
        Printf.sprintf "%s=%s" k (Uri.pct_encode v)
      ) form_params |> String.concat "&" in
      let headers = [
        ("Authorization", "Bearer " ^ access_token);
        ("User-Agent", user_agent);
        ("Content-Type", "application/x-www-form-urlencoded");
      ] in
      Config.Http.post ~headers ~body api_url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let open Yojson.Basic.Util in
              let json = Yojson.Basic.from_string response.body in
              let errors =
                try json |> member "json" |> member "errors" |> to_list
                with _ -> []
              in
              if List.length errors > 0 then
                on_result (Error_types.Failure (parse_api_error ~status_code:200 ~response_body:response.body))
              else
                let data = json |> member "json" |> member "data" in
                let post_id = data |> member "id" |> to_string in
                on_result (Error_types.Success post_id)
            with e ->
              on_result (Error_types.Failure (Error_types.Internal_error (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e))))
          else
            on_result (Error_types.Failure (parse_api_error ~status_code:response.status ~response_body:response.body)))
        (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err)))
    in

    let upload_thumbnail_if_any on_success =
      match thumbnail_url with
      | None -> on_success None
      | Some poster_url when String.length poster_url = 0 -> on_success None
      | Some poster_url ->
          download_media_url ~url:poster_url
            (fun poster_dl_outcome ->
              match poster_dl_outcome with
              | Error_types.Failure _ -> on_success None
              | Error_types.Partial_success _ ->
                  on_success None
              | Error_types.Success (poster_content, poster_mimetype) ->
                  if not (String.starts_with ~prefix:"image/" poster_mimetype) then
                    on_success None
                  else
                    let poster_media : Platform_types.post_media = {
                      media_type = Platform_types.Image;
                      mime_type = poster_mimetype;
                      file_size_bytes = String.length poster_content;
                      width = None;
                      height = None;
                      duration_seconds = None;
                      alt_text = None;
                    } in
                    match validate_media ~media:poster_media with
                    | Error _ -> on_success None
                    | Ok () ->
                        let poster_filename = infer_filename_from_url poster_url "poster.jpg" in
                        get_media_upload_lease ~account_id ~filename:poster_filename ~mimetype:poster_mimetype
                          (fun poster_lease_outcome ->
                            match poster_lease_outcome with
                            | Error_types.Failure _ -> on_success None
                            | Error_types.Partial_success _ ->
                                on_success None
                            | Error_types.Success poster_lease ->
                                upload_media_to_s3
                                  ~upload_url:poster_lease.upload_url
                                  ~upload_fields:poster_lease.upload_fields
                                  ~media_content:poster_content
                                  ~mimetype:poster_mimetype
                                  (fun poster_upload_outcome ->
                                    match poster_upload_outcome with
                                    | Error_types.Failure _ -> on_success None
                                    | Error_types.Partial_success _ ->
                                        on_success None
                                    | Error_types.Success () ->
                                        on_success (Some (uploaded_media_url poster_lease)))))
    in

    match validate_post ~title ~media_count:1 () with
    | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        ensure_valid_token ~account_id
          (fun access_token ->
            download_media_url ~url:video_url
              (fun video_dl_outcome ->
                match video_dl_outcome with
                | Error_types.Failure err -> on_result (Error_types.Failure err)
                | Error_types.Partial_success _ ->
                    on_result (Error_types.Failure (Error_types.Internal_error "Unexpected partial success while downloading video"))
                | Error_types.Success (video_content, video_mimetype) ->
                    if not (String.starts_with ~prefix:"video/" video_mimetype) then
                      on_result (Error_types.Failure (Error_types.Validation_error [
                        Error_types.Media_unsupported_format video_mimetype
                      ]))
                    else
                      let video_media : Platform_types.post_media = {
                        media_type = Platform_types.Video;
                        mime_type = video_mimetype;
                        file_size_bytes = String.length video_content;
                        width = None;
                        height = None;
                        duration_seconds = None;
                        alt_text = None;
                      } in
                      match validate_media ~media:video_media with
                      | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
                      | Ok () ->
                          let video_filename = infer_filename_from_url video_url "video.mp4" in
                          get_media_upload_lease ~account_id ~filename:video_filename ~mimetype:video_mimetype
                            (fun lease_outcome ->
                              match lease_outcome with
                              | Error_types.Failure err -> on_result (Error_types.Failure err)
                              | Error_types.Partial_success _ ->
                                  on_result (Error_types.Failure (Error_types.Internal_error "Unexpected partial success while requesting video upload lease"))
                              | Error_types.Success video_lease ->
                                  upload_media_to_s3
                                    ~upload_url:video_lease.upload_url
                                    ~upload_fields:video_lease.upload_fields
                                    ~media_content:video_content
                                    ~mimetype:video_mimetype
                                    (fun upload_outcome ->
                                      match upload_outcome with
                                      | Error_types.Failure err -> on_result (Error_types.Failure err)
                                      | Error_types.Partial_success _ ->
                                          on_result (Error_types.Failure (Error_types.Internal_error "Unexpected partial success while uploading video to S3"))
                                      | Error_types.Success () ->
                                          let posted_video_url = uploaded_media_url video_lease in
                                          upload_thumbnail_if_any
                                            (fun video_poster_url ->
                                              submit_video_request ~access_token ~posted_video_url ?video_poster_url ())))))
          (fun err -> on_result (Error_types.Failure err))
  
  (** Submit an image post to a subreddit *)
  let submit_image_post ~account_id ~subreddit ~title ~image_url
      ?flair_id ?flair_text ?(nsfw=false) ?(spoiler=false) () on_result =
    (* Validate *)
    match validate_post ~title ~media_count:1 () with
    | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        ensure_valid_token ~account_id
          (fun access_token ->
            let api_url = api_base ^ "/api/submit" in
            
            (* Build form data - using image URL directly *)
            let form_params = [
              ("api_type", "json");
              ("kind", "image");
              ("sr", subreddit);
              ("title", title);
              ("url", image_url);
              ("nsfw", string_of_bool nsfw);
              ("spoiler", string_of_bool spoiler);
            ] in
            
            (* Add optional flair *)
            let form_params = match flair_id with
              | Some id -> ("flair_id", id) :: form_params
              | None -> form_params
            in
            let form_params = match flair_text with
              | Some text -> ("flair_text", text) :: form_params
              | None -> form_params
            in
            
            let body = List.map (fun (k, v) ->
              Printf.sprintf "%s=%s" k (Uri.pct_encode v)
            ) form_params |> String.concat "&" in
            
            let headers = [
              ("Authorization", "Bearer " ^ access_token);
              ("User-Agent", user_agent);
              ("Content-Type", "application/x-www-form-urlencoded");
            ] in
            
            Config.Http.post ~headers ~body api_url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  try
                    let open Yojson.Basic.Util in
                    let json = Yojson.Basic.from_string response.body in
                    
                    let errors = 
                      try json |> member "json" |> member "errors" |> to_list
                      with _ -> []
                    in
                    
                    if List.length errors > 0 then
                      on_result (Error_types.Failure (parse_api_error ~status_code:200 ~response_body:response.body))
                    else
                      let data = json |> member "json" |> member "data" in
                      let post_id = data |> member "id" |> to_string in
                      on_result (Error_types.Success post_id)
                  with e ->
                    on_result (Error_types.Failure (Error_types.Internal_error (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e))))
                else
                  on_result (Error_types.Failure (parse_api_error ~status_code:response.status ~response_body:response.body)))
              (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err))))
          (fun err -> on_result (Error_types.Failure err))
  
  (** Crosspost from another subreddit *)
  let submit_crosspost ~account_id ~subreddit ~title ~original_post_id
      ?flair_id ?flair_text ?(nsfw=false) ?(spoiler=false) () on_result =
    (* Validate title *)
    match validate_title title with
    | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        ensure_valid_token ~account_id
          (fun access_token ->
            let api_url = api_base ^ "/api/submit" in
            
            (* Ensure original_post_id has t3_ prefix *)
            let crosspost_fullname = 
              if String.length original_post_id > 3 && String.sub original_post_id 0 3 = "t3_" then
                original_post_id
              else
                "t3_" ^ original_post_id
            in
            
            (* Build form data *)
            let form_params = [
              ("api_type", "json");
              ("kind", "crosspost");
              ("sr", subreddit);
              ("title", title);
              ("crosspost_fullname", crosspost_fullname);
              ("nsfw", string_of_bool nsfw);
              ("spoiler", string_of_bool spoiler);
            ] in
            
            (* Add optional flair *)
            let form_params = match flair_id with
              | Some id -> ("flair_id", id) :: form_params
              | None -> form_params
            in
            let form_params = match flair_text with
              | Some text -> ("flair_text", text) :: form_params
              | None -> form_params
            in
            
            let body = List.map (fun (k, v) ->
              Printf.sprintf "%s=%s" k (Uri.pct_encode v)
            ) form_params |> String.concat "&" in
            
            let headers = [
              ("Authorization", "Bearer " ^ access_token);
              ("User-Agent", user_agent);
              ("Content-Type", "application/x-www-form-urlencoded");
            ] in
            
            Config.Http.post ~headers ~body api_url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  try
                    let open Yojson.Basic.Util in
                    let json = Yojson.Basic.from_string response.body in
                    
                    let errors = 
                      try json |> member "json" |> member "errors" |> to_list
                      with _ -> []
                    in
                    
                    if List.length errors > 0 then
                      on_result (Error_types.Failure (parse_api_error ~status_code:200 ~response_body:response.body))
                    else
                      let data = json |> member "json" |> member "data" in
                      let post_id = data |> member "id" |> to_string in
                      on_result (Error_types.Success post_id)
                  with e ->
                    on_result (Error_types.Failure (Error_types.Internal_error (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e))))
                else
                  on_result (Error_types.Failure (parse_api_error ~status_code:response.status ~response_body:response.body)))
              (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err))))
          (fun err -> on_result (Error_types.Failure err))
  
  (** Unified post_single function matching other platform APIs
      
      This function automatically determines the post type based on parameters:
      - If body is provided without url/media: self-post
      - If url is provided without media: link post  
      - If media_urls is provided: image or video post (first media URL)
      
      @param account_id Account identifier
      @param subreddit Subreddit to post to (without r/ prefix)
      @param title Post title (required, max 300 chars)
      @param body Optional post body for self-posts
      @param url Optional URL for link posts
      @param media_urls Optional list of media URLs for image posts
      @param flair_id Optional flair template ID
      @param flair_text Optional flair text
      @param nsfw Mark post as NSFW
      @param spoiler Mark post as spoiler
  *)
  let string_ends_with ~suffix s =
    let s_len = String.length s in
    let suffix_len = String.length suffix in
    s_len >= suffix_len && String.sub s (s_len - suffix_len) suffix_len = suffix

  let strip_url_suffixes url =
    let stop_at ch default =
      try String.index url ch with Not_found -> default
    in
    let stop_q = stop_at '?' (String.length url) in
    let stop_h = stop_at '#' (String.length url) in
    let stop = min stop_q stop_h in
    String.sub url 0 stop

  let is_likely_video_url url =
    let path = strip_url_suffixes url |> String.lowercase_ascii in
    let video_exts = [".mp4"; ".mov"; ".webm"; ".m4v"; ".avi"; ".mkv"] in
    List.exists (fun ext -> string_ends_with ~suffix:ext path) video_exts

  let is_likely_image_url url =
    let path = strip_url_suffixes url |> String.lowercase_ascii in
    let image_exts = [".jpg"; ".jpeg"; ".png"; ".gif"; ".webp"] in
    List.exists (fun ext -> string_ends_with ~suffix:ext path) image_exts

  let post_single ~account_id ~subreddit ~title ?body ?url ?(media_urls=[]) 
      ?flair_id ?flair_text ?(nsfw=false) ?(spoiler=false) () on_result =
    let media_count = List.length media_urls in
    let submit_as_image ~image_url =
      submit_image_post ~account_id ~subreddit ~title ~image_url
        ?flair_id ?flair_text ~nsfw ~spoiler () on_result
    in
    let submit_as_video ~video_url ?thumbnail_url () =
      submit_video_post ~account_id ~subreddit ~title ~video_url ?thumbnail_url
        ?flair_id ?flair_text ~nsfw ~spoiler () on_result
    in
    let has_image_content_type_unsupported errs =
      List.exists (function
        | Error_types.Media_unsupported_format fmt ->
            let lowered = String.lowercase_ascii fmt in
            String.starts_with ~prefix:"image/" lowered
        | _ -> false
      ) errs
    in
    
    (* Determine post type based on parameters *)
    if media_count > 0 then
      (* Media post *)
      let media_url = List.hd media_urls in
      if is_likely_video_url media_url then
        let thumbnail_url =
          if media_count > 1 then
            Some (List.nth media_urls 1)
          else
            None
        in
        submit_as_video ~video_url:media_url ?thumbnail_url ()
      else if is_likely_image_url media_url then
        submit_as_image ~image_url:media_url
      else
        (* Unknown extension: try native video flow first, fallback to image URL post
           only when media type is explicitly non-video. *)
        let thumbnail_url =
          if media_count > 1 then
            Some (List.nth media_urls 1)
          else
            None
        in
        submit_video_post ~account_id ~subreddit ~title ~video_url:media_url
          ?thumbnail_url
          ?flair_id ?flair_text ~nsfw ~spoiler ()
          (fun outcome ->
            match outcome with
            | Error_types.Failure (Error_types.Validation_error errs) when has_image_content_type_unsupported errs ->
                submit_as_image ~image_url:media_url
            | _ -> on_result outcome)
    else match url with
    | Some link_url when String.length link_url > 0 ->
        (* Link post *)
        submit_link_post ~account_id ~subreddit ~title ~url:link_url
          ?flair_id ?flair_text ~nsfw ~spoiler () on_result
    | _ ->
        (* Self-post *)
        submit_self_post ~account_id ~subreddit ~title ?body
          ?flair_id ?flair_text ~nsfw ~spoiler () on_result
  
  (** Delete a post *)
  let delete_post ~account_id ~post_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let url = api_base ^ "/api/del" in
        
        (* Ensure post_id has t3_ prefix *)
        let full_id = 
          if String.length post_id > 3 && String.sub post_id 0 3 = "t3_" then
            post_id
          else
            "t3_" ^ post_id
        in
        
        let body = Printf.sprintf "id=%s" (Uri.pct_encode full_id) in
        
        let headers = [
          ("Authorization", "Bearer " ^ access_token);
          ("User-Agent", user_agent);
          ("Content-Type", "application/x-www-form-urlencoded");
        ] in
        
        Config.Http.post ~headers ~body url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_result (Error_types.Success ())
            else
              on_result (Error_types.Failure (parse_api_error ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err))))
      (fun err -> on_result (Error_types.Failure err))
  
  (** {1 Rate Limit Header Parsing} *)

  (** Parse Reddit rate limit headers from an HTTP response.

      Reddit includes rate limit info in response headers:
      - X-Ratelimit-Used: approximate number of requests used in the current period
      - X-Ratelimit-Remaining: approximate number of requests remaining
      - X-Ratelimit-Reset: approximate seconds until the current period ends
  *)
  let parse_rate_limit_headers (headers : (string * string) list) : rate_limit_headers =
    let find_header name =
      (* Reddit headers are case-insensitive; try lowercase lookup *)
      let lower_name = String.lowercase_ascii name in
      List.find_map (fun (k, v) ->
        if String.lowercase_ascii k = lower_name then Some v
        else None
      ) headers
    in
    let parse_float_opt s =
      try Some (float_of_string s)
      with Failure _ -> None
    in
    let parse_int_opt s =
      try Some (int_of_string s)
      with Failure _ ->
        (* Reset can be a float representing seconds; truncate to int *)
        try Some (int_of_float (float_of_string s))
        with Failure _ -> None
    in
    {
      used = Option.bind (find_header "x-ratelimit-used") parse_float_opt;
      remaining = Option.bind (find_header "x-ratelimit-remaining") parse_float_opt;
      reset = Option.bind (find_header "x-ratelimit-reset") parse_int_opt;
    }

  (** {1 Subreddit Search} *)

  (** Search for subreddits by keyword using GET /subreddits/search *)
  let search_subreddits ~account_id ~query ?(limit=10) on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let url = Printf.sprintf "%s/subreddits/search?q=%s&limit=%d"
          api_base (Uri.pct_encode query) limit in
        let headers = [
          ("Authorization", "Bearer " ^ access_token);
          ("User-Agent", user_agent);
        ] in

        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let open Yojson.Basic.Util in
                let json = Yojson.Basic.from_string response.body in
                let children = json |> member "data" |> member "children" |> to_list in
                let subreddits = List.map (fun child ->
                  let data = child |> member "data" in
                  {
                    id = data |> member "id" |> to_string;
                    name = data |> member "display_name" |> to_string;
                    display_name = data |> member "display_name" |> to_string;
                    subscribers = (try data |> member "subscribers" |> to_int with _ -> 0);
                    over18 = (try data |> member "over18" |> to_bool with _ -> false);
                    user_is_moderator = (try data |> member "user_is_moderator" |> to_bool with _ -> false);
                    submission_type = (try Some (data |> member "submission_type" |> to_string) with _ -> None);
                    flair_enabled = (try data |> member "link_flair_enabled" |> to_bool with _ -> false);
                  }
                ) children in
                on_result (Error_types.Success subreddits)
              with e ->
                on_result (Error_types.Failure (Error_types.Internal_error (Printf.sprintf "Failed to parse subreddit search: %s" (Printexc.to_string e))))
            else
              on_result (Error_types.Failure (parse_api_error ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err))))
      (fun err -> on_result (Error_types.Failure err))

  (** {1 Gallery Posts} *)

  (** Submit a gallery post with multiple images.

      Each image is uploaded via the media asset upload flow, then submitted
      together using the POST /api/submit_gallery_post endpoint.

      @param account_id Account identifier
      @param subreddit Subreddit to post to
      @param title Post title
      @param items List of gallery items (images with optional captions)
      @param flair_id Optional flair template ID
      @param flair_text Optional flair text
      @param nsfw Mark post as NSFW
      @param spoiler Mark post as spoiler
  *)
  let submit_gallery_post ~account_id ~subreddit ~title ~(items : gallery_item list)
      ?flair_id ?flair_text ?(nsfw=false) ?(spoiler=false) () on_result =
    let item_count = List.length items in
    match validate_post ~title ~media_count:item_count () with
    | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        if item_count < 2 then
          on_result (Error_types.Failure (Error_types.Validation_error [
            Error_types.Too_many_media { count = item_count; max = max_gallery_images }
          ]))
        else
          ensure_valid_token ~account_id
            (fun access_token ->
              (* Upload all images and collect their asset IDs *)
              let uploaded = ref [] in
              let failed = ref false in

              let rec upload_items remaining_items =
                if !failed then ()
                else match remaining_items with
                | [] ->
                    (* All uploaded, submit the gallery post *)
                    let gallery_items = List.rev !uploaded in
                    let items_json = List.map (fun (asset_id, caption, outbound_url) ->
                      let fields = [
                        ("media_id", `String asset_id);
                      ] in
                      let fields = match caption with
                        | Some c -> ("caption", `String c) :: fields
                        | None -> fields
                      in
                      let fields = match outbound_url with
                        | Some u -> ("outbound_url", `String u) :: fields
                        | None -> fields
                      in
                      `Assoc fields
                    ) gallery_items in
                    let body_json = `Assoc ([
                      ("api_type", `String "json");
                      ("kind", `String "self");
                      ("sr", `String subreddit);
                      ("title", `String title);
                      ("nsfw", `Bool nsfw);
                      ("spoiler", `Bool spoiler);
                      ("items", `List items_json);
                    ] @ (match flair_id with Some id -> [("flair_id", `String id)] | None -> [])
                      @ (match flair_text with Some t -> [("flair_text", `String t)] | None -> [])
                    ) in
                    let body = Yojson.Basic.to_string body_json in
                    let api_url = api_base ^ "/api/submit_gallery_post" in
                    let headers = [
                      ("Authorization", "Bearer " ^ access_token);
                      ("User-Agent", user_agent);
                      ("Content-Type", "application/json");
                    ] in
                    Config.Http.post ~headers ~body api_url
                      (fun response ->
                        if response.status >= 200 && response.status < 300 then
                          try
                            let open Yojson.Basic.Util in
                            let json = Yojson.Basic.from_string response.body in
                            let json_obj =
                              try json |> member "json"
                              with _ -> json
                            in
                            let errors =
                              try json_obj |> member "errors" |> to_list
                              with _ -> []
                            in
                            if List.length errors > 0 then
                              on_result (Error_types.Failure (parse_api_error ~status_code:200 ~response_body:response.body))
                            else
                              let data =
                                try json_obj |> member "data"
                                with _ -> json
                              in
                              let post_id = data |> member "id" |> to_string in
                              on_result (Error_types.Success post_id)
                          with e ->
                            on_result (Error_types.Failure (Error_types.Internal_error (Printf.sprintf "Failed to parse gallery response: %s" (Printexc.to_string e))))
                        else
                          on_result (Error_types.Failure (parse_api_error ~status_code:response.status ~response_body:response.body)))
                      (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err)))
                | item :: rest ->
                    (* Download this image *)
                    download_media_url ~url:item.media_url
                      (fun dl_outcome ->
                        match dl_outcome with
                        | Error_types.Failure err ->
                            failed := true;
                            on_result (Error_types.Failure err)
                        | Error_types.Partial_success _ ->
                            failed := true;
                            on_result (Error_types.Failure (Error_types.Internal_error "Unexpected partial success downloading gallery image"))
                        | Error_types.Success (content, mimetype) ->
                            let filename = infer_filename_from_url item.media_url "image.jpg" in
                            get_media_upload_lease ~account_id ~filename ~mimetype
                              (fun lease_outcome ->
                                match lease_outcome with
                                | Error_types.Failure err ->
                                    failed := true;
                                    on_result (Error_types.Failure err)
                                | Error_types.Partial_success _ ->
                                    failed := true;
                                    on_result (Error_types.Failure (Error_types.Internal_error "Unexpected partial success requesting upload lease"))
                                | Error_types.Success lease ->
                                    upload_media_to_s3
                                      ~upload_url:lease.upload_url
                                      ~upload_fields:lease.upload_fields
                                      ~media_content:content
                                      ~mimetype
                                      (fun upload_outcome ->
                                        match upload_outcome with
                                        | Error_types.Failure err ->
                                            failed := true;
                                            on_result (Error_types.Failure err)
                                        | Error_types.Partial_success _ ->
                                            failed := true;
                                            on_result (Error_types.Failure (Error_types.Internal_error "Unexpected partial success uploading to S3"))
                                        | Error_types.Success () ->
                                            uploaded := (lease.asset_id, item.caption, item.outbound_url) :: !uploaded;
                                            upload_items rest)))
              in
              upload_items items)
            (fun err -> on_result (Error_types.Failure err))

  (** {1 Multi-Subreddit Posting} *)

  (** Post the same content to multiple subreddits.

      Posts are submitted sequentially. Results are collected for each subreddit.
      A failure for one subreddit does not prevent posting to the others.

      @param account_id Account identifier
      @param subreddits List of subreddit names to post to
      @param title Post title
      @param body Optional post body for self-posts
      @param url Optional URL for link posts
      @param media_urls Optional list of media URLs
      @param flair_id Optional flair template ID
      @param flair_text Optional flair text
      @param nsfw Mark post as NSFW
      @param spoiler Mark post as spoiler
  *)
  let post_to_subreddits ~account_id ~subreddits ~title ?body ?url ?(media_urls=[])
      ?flair_id ?flair_text ?(nsfw=false) ?(spoiler=false) () on_result =
    let results = ref [] in
    let rec post_next remaining =
      match remaining with
      | [] ->
          on_result (Error_types.Success (List.rev !results))
      | sub :: rest ->
          post_single ~account_id ~subreddit:sub ~title ?body ?url ~media_urls
            ?flair_id ?flair_text ~nsfw ~spoiler ()
            (fun outcome ->
              let result = match outcome with
                | Error_types.Success post_id ->
                    { subreddit = sub; post_id = Some post_id; error = None }
                | Error_types.Partial_success { result = post_id; _ } ->
                    { subreddit = sub; post_id = Some post_id; error = None }
                | Error_types.Failure err ->
                    { subreddit = sub; post_id = None; error = Some (Error_types.error_to_string err) }
              in
              results := result :: !results;
              post_next rest)
    in
    post_next subreddits

  (** {1 OAuth Convenience Functions} *)

  (** Generate OAuth authorization URL *)
  let get_oauth_url ~redirect_uri ~state on_success on_error =
    let client_id = Config.get_env "REDDIT_CLIENT_ID" |> Option.value ~default:"" in
    
    if client_id = "" then
      on_error "Reddit client ID not configured"
    else
      let url = OAuth.get_authorization_url ~client_id ~redirect_uri ~state 
        ~scopes:OAuth.Scopes.moderation ~duration:"permanent" () in
      on_success url
  
  (** Exchange OAuth code for access token and fetch initial data *)
  let exchange_code ~code ~redirect_uri on_success on_error =
    let client_id = Config.get_env "REDDIT_CLIENT_ID" |> Option.value ~default:"" in
    let client_secret = Config.get_env "REDDIT_CLIENT_SECRET" |> Option.value ~default:"" in
    
    if client_id = "" || client_secret = "" then
      on_error "Reddit OAuth credentials not configured"
    else
      let expected_scopes = OAuth.Scopes.moderation in
      let body = Printf.sprintf
        "grant_type=authorization_code&code=%s&redirect_uri=%s"
        (Uri.pct_encode code)
        (Uri.pct_encode redirect_uri)
      in
      let auth_string = String.trim client_id ^ ":" ^ String.trim client_secret in
      let headers = [
        ("Content-Type", "application/x-www-form-urlencoded");
        ("Authorization", "Basic " ^ Base64.encode_exn auth_string);
      ] in
      Config.Http.post ~headers ~body OAuth.Metadata.token_endpoint
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
              let expires_in =
                try json |> member "expires_in" |> to_int
                with _ -> 3600
              in
              let token_type =
                try json |> member "token_type" |> to_string
                with _ -> "bearer"
              in
              let granted_scope_raw =
                try Some (json |> member "scope" |> to_string)
                with _ -> None
              in
              let missing_scopes =
                match granted_scope_raw with
                | None -> expected_scopes
                | Some scope_str ->
                    let granted_scopes =
                      scope_str
                      |> String.map (fun c -> if c = ',' then ' ' else c)
                      |> String.split_on_char ' '
                      |> List.map String.trim
                      |> List.filter (fun s -> String.length s > 0)
                    in
                    List.filter (fun required -> not (List.mem required granted_scopes)) expected_scopes
              in
              if missing_scopes <> [] then
                on_error (Printf.sprintf
                  "Missing required Reddit OAuth scopes: %s"
                  (String.concat ", " missing_scopes))
              else
                let expires_at =
                  let now = Ptime_clock.now () in
                  match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
                  | Some exp -> Some (Ptime.to_rfc3339 exp)
                  | None -> None
                in
                let credentials = {
                  access_token;
                  refresh_token;
                  expires_at;
                  token_type;
                } in
                on_success credentials
            with e ->
              on_error (Printf.sprintf "Failed to parse OAuth response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "OAuth exchange failed (%d): %s" response.status response.body))
        on_error
  
  (** Get user info with the current credentials *)
  let get_user_info ~account_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let module OAuthHttp = OAuth.Make(Config.Http) in
        OAuthHttp.get_user_info ~access_token
          (fun user -> on_result (Error_types.Success user))
          (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err))))
      (fun err -> on_result (Error_types.Failure err))
  
  (** {1 Content Validation Helper} *)
  
  (** Validate content (title and optional body) *)
  let validate_content ~title ?body () =
    match validate_post ~title ?body () with
    | Ok () -> Ok ()
    | Error errs -> Error (String.concat "; " (List.map Error_types.validation_error_to_string errs))
end
