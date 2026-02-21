(** Mastodon API v1/v2 Provider
    
    This implementation supports Mastodon instances with OAuth 2.0 authentication.
    Each instance has its own URL and tokens typically don't expire unless revoked.
*)

open Social_core

let redact_sensitive_text text =
  let replace_json_field field input =
    let pattern = Printf.sprintf "\"%s\"[ \t\r\n]*:[ \t\r\n]*\"[^\"]*\"" field in
    let replacement = Printf.sprintf {|"%s":"[REDACTED]"|} field in
    Str.global_replace (Str.regexp pattern) replacement input
  in
  let replace_form_field field input =
    let pattern = Printf.sprintf {|%s=[^&[:space:]]+|} field in
    let replacement = Printf.sprintf "%s=[REDACTED]" field in
    Str.global_replace (Str.regexp pattern) replacement input
  in
  text
  |> replace_json_field "access_token"
  |> replace_json_field "refresh_token"
  |> replace_json_field "client_secret"
  |> replace_json_field "token"
  |> replace_form_field "access_token"
  |> replace_form_field "refresh_token"
  |> replace_form_field "client_secret"
  |> replace_form_field "token"

let header_value_case_insensitive headers key =
  let target = String.lowercase_ascii key in
  headers
  |> List.find_map (fun (k, v) ->
         if String.equal (String.lowercase_ascii k) target then Some v else None)

let parse_positive_int value =
  try
    let parsed = int_of_string value in
    if parsed >= 0 then Some parsed else None
  with _ -> None

(** OAuth 2.0 module for Mastodon
    
    Mastodon uses standard OAuth 2.0 with PKCE support.
    
    IMPORTANT: Unlike other platforms, Mastodon is federated - each instance
    requires separate app registration. The OAuth flow is:
    
    1. Register your app with each instance using register_app
    2. Store the client_id and client_secret per instance
    3. Generate authorization URL for that instance
    4. Exchange code for tokens (tokens never expire unless revoked)
    
    Tokens do NOT expire on Mastodon - they remain valid until explicitly revoked.
*)
module OAuth = struct
  (** Scope definitions for Mastodon API *)
  module Scopes = struct
    (** Scopes required for read-only operations *)
    let read = ["read"]
    
    (** Scopes required for posting content (includes read and follow) *)
    let write = ["read"; "write"; "follow"]
    
    (** All available Mastodon OAuth scopes *)
    let all = ["read"; "write"; "follow"; "push"]
    
    (** Granular read scopes (optional, for fine-grained access) *)
    let read_granular = [
      "read:accounts"; "read:blocks"; "read:bookmarks"; "read:favourites";
      "read:filters"; "read:follows"; "read:lists"; "read:mutes";
      "read:notifications"; "read:search"; "read:statuses"
    ]
    
    (** Granular write scopes (optional, for fine-grained access) *)
    let write_granular = [
      "write:accounts"; "write:blocks"; "write:bookmarks"; "write:conversations";
      "write:favourites"; "write:filters"; "write:follows"; "write:lists";
      "write:media"; "write:mutes"; "write:notifications"; "write:reports";
      "write:statuses"
    ]
    
    (** Operations that can be performed with Mastodon API *)
    type operation = 
      | Post_text
      | Post_media
      | Post_video
      | Read_profile
      | Read_posts
      | Delete_post
      | Manage_pages  (** Not applicable to Mastodon *)
    
    (** Get scopes required for specific operations
        
        Mastodon uses a simple scope model, so most operations just need
        the standard read/write/follow scopes *)
    let for_operations _ops = write
  end
  
  (** Platform metadata for Mastodon OAuth *)
  module Metadata = struct
    (** Mastodon supports PKCE (S256 method) *)
    let supports_pkce = true
    
    (** Mastodon tokens do NOT expire - no refresh needed *)
    let supports_refresh = false
    
    (** Mastodon tokens never expire (None = infinite) *)
    let token_lifetime_seconds = None
    
    (** No refresh buffer needed since tokens don't expire *)
    let refresh_buffer_seconds = 0
    
    (** No refresh attempts since tokens don't expire *)
    let max_refresh_attempts = 0
    
    (** Authorization endpoint template (prepend instance URL) *)
    let authorization_path = "/oauth/authorize"
    
    (** Token endpoint template (prepend instance URL) *)
    let token_path = "/oauth/token"
    
    (** App registration endpoint template (prepend instance URL) *)
    let apps_path = "/api/v1/apps"
    
    (** Token revocation endpoint template (prepend instance URL) *)
    let revoke_path = "/oauth/revoke"
  end
  
  (** PKCE helpers for OAuth 2.0 with Proof Key for Code Exchange *)
  module Pkce = struct
    (** Generate a cryptographically random code verifier (128 chars)
        
        The code verifier uses characters [A-Z] / [a-z] / [0-9] / "-" / "." / "_" / "~"
        per RFC 7636.
        
        @return A 128-character random code verifier string
    *)
    let generate_code_verifier () =
      let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~" in
      let chars_len = String.length chars in
      String.init 128 (fun _ -> String.get chars (Random.int chars_len))
    
    (** Generate code_challenge from code_verifier using SHA256 (S256 method)
        
        code_challenge = BASE64URL(SHA256(ASCII(code_verifier)))
        
        @param verifier The code_verifier string
        @return Base64-URL encoded SHA256 hash without padding
    *)
    let generate_code_challenge verifier =
      let hash = Digestif.SHA256.digest_string verifier in
      let raw_hash = Digestif.SHA256.to_raw_string hash in
      Base64.encode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet raw_hash
  end
  
  (** Generate authorization URL for Mastodon OAuth 2.0 flow
      
      IMPORTANT: instance_url is required because Mastodon is federated.
      
      @param instance_url The Mastodon instance URL (e.g., "https://mastodon.social")
      @param client_id OAuth 2.0 Client ID (from register_app for this instance)
      @param redirect_uri Registered callback URL
      @param scopes OAuth scopes to request (defaults to Scopes.write)
      @param state Optional CSRF protection state parameter
      @param code_challenge Optional PKCE code challenge (generate with Pkce.generate_code_challenge)
      @return Full authorization URL to redirect user to
  *)
  let get_authorization_url ~instance_url ~client_id ~redirect_uri ?(scopes=Scopes.write) ?state ?code_challenge () =
    let scope_str = String.concat " " scopes in
    let base_params = [
      ("client_id", client_id);
      ("redirect_uri", redirect_uri);
      ("response_type", "code");
      ("scope", scope_str);
    ] in
    let with_state = match state with
      | Some s -> ("state", s) :: base_params
      | None -> base_params
    in
    let with_pkce = match code_challenge with
      | Some challenge -> 
          ("code_challenge", challenge) :: ("code_challenge_method", "S256") :: with_state
      | None -> with_state
    in
    let query = Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) with_pkce) in
    Printf.sprintf "%s%s?%s" instance_url Metadata.authorization_path query
  
  (** Make functor for OAuth operations that need HTTP client
      
      This separates the pure functions (URL generation, scope selection) from
      functions that need to make HTTP requests (app registration, token exchange).
  *)
  module Make (Http : HTTP_CLIENT) = struct
    (** Register an application with a Mastodon instance
        
        IMPORTANT: Must be called once per instance before OAuth flow.
        Store the returned client_id and client_secret securely.
        
        @param instance_url The Mastodon instance URL (e.g., "https://mastodon.social")
        @param client_name Your application name
        @param redirect_uris Space-separated list of redirect URIs
        @param scopes Space-separated list of requested scopes (defaults to "read write follow")
        @param website Optional website URL for your application
        @param on_success Continuation receiving (client_id, client_secret)
        @param on_error Continuation receiving error message
    *)
    let register_app ~instance_url ~client_name ~redirect_uris ?(scopes="read write follow") ?website on_success on_error =
      let url = Printf.sprintf "%s%s" instance_url Metadata.apps_path in
      
      let base_fields = [
        ("client_name", `String client_name);
        ("redirect_uris", `String redirect_uris);
        ("scopes", `String scopes);
      ] in
      let fields = match website with
        | Some w -> ("website", `String w) :: base_fields
        | None -> base_fields
      in
      
      let body = Yojson.Basic.to_string (`Assoc fields) in
      let headers = [("Content-Type", "application/json")] in
      
      Http.post ~headers ~body url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let client_id = json |> member "client_id" |> to_string in
              let client_secret = json |> member "client_secret" |> to_string in
              on_success (client_id, client_secret)
            with e ->
              on_error (Printf.sprintf "Failed to parse app registration: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "App registration failed (%d): %s" response.status (redact_sensitive_text response.body)))
        on_error
    
    let exchange_code_impl ~instance_url ~client_id ~client_secret ~redirect_uri ~code ?code_verifier on_success on_error =
      let url = Printf.sprintf "%s%s" instance_url Metadata.token_path in
      
      let base_fields = [
        ("client_id", `String client_id);
        ("client_secret", `String client_secret);
        ("redirect_uri", `String redirect_uri);
        ("grant_type", `String "authorization_code");
        ("code", `String code);
      ] in
      
      let fields = match code_verifier with
        | Some verifier -> ("code_verifier", `String verifier) :: base_fields
        | None -> base_fields
      in
      
      let body = Yojson.Basic.to_string (`Assoc fields) in
      let headers = [("Content-Type", "application/json")] in
      
      Http.post ~headers ~body url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let access_token = json |> member "access_token" |> to_string in
              let token_type = 
                try json |> member "token_type" |> to_string 
                with _ -> "Bearer" in
              let granted_scope =
                try Some (json |> member "scope" |> to_string)
                with _ -> None
              in
              
              (* Mastodon tokens don't expire *)
              let creds : credentials = {
                access_token;
                refresh_token = None;
                expires_at = None;  (* Never expires *)
                token_type;
              } in
              on_success (creds, granted_scope)
            with e ->
              on_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "Token exchange failed (%d): %s" response.status (redact_sensitive_text response.body)))
        on_error

    (** Exchange authorization code for access token
        
        @param instance_url The Mastodon instance URL
        @param client_id OAuth 2.0 Client ID (from register_app)
        @param client_secret OAuth 2.0 Client Secret (from register_app)
        @param redirect_uri Registered callback URL (must match authorization request)
        @param code Authorization code from callback
        @param code_verifier Optional PKCE code verifier (if PKCE was used in authorization)
        @param on_success Continuation receiving credentials
        @param on_error Continuation receiving error message
    *)
    let exchange_code ~instance_url ~client_id ~client_secret ~redirect_uri ~code ?code_verifier on_success on_error =
      exchange_code_impl
        ~instance_url ~client_id ~client_secret ~redirect_uri ~code ?code_verifier
        (fun (creds, _scope) -> on_success creds)
        on_error

    (** Exchange authorization code and include granted scope (if returned) *)
    let exchange_code_with_scope ~instance_url ~client_id ~client_secret ~redirect_uri ~code ?code_verifier on_success on_error =
      exchange_code_impl
        ~instance_url ~client_id ~client_secret ~redirect_uri ~code ?code_verifier
        on_success
        on_error
    
    (** Revoke an access token
        
        @param instance_url The Mastodon instance URL
        @param client_id OAuth 2.0 Client ID
        @param client_secret OAuth 2.0 Client Secret
        @param token The access token to revoke
        @param on_success Continuation called on successful revocation
        @param on_error Continuation receiving error message
    *)
    let revoke_token ~instance_url ~client_id ~client_secret ~token on_success on_error =
      let url = Printf.sprintf "%s%s" instance_url Metadata.revoke_path in
      
      let body = Yojson.Basic.to_string (`Assoc [
        ("client_id", `String client_id);
        ("client_secret", `String client_secret);
        ("token", `String token);
      ]) in
      let headers = [("Content-Type", "application/json")] in
      
      Http.post ~headers ~body url
        (fun response ->
          (* OAuth2 revocation returns 200 for both valid and invalid tokens *)
          if response.status >= 200 && response.status < 300 then
            on_success ()
          else
            on_error (Printf.sprintf "Token revocation failed (%d): %s" response.status (redact_sensitive_text response.body)))
        on_error
  end
end

(** Mastodon-specific credentials with instance URL *)
type mastodon_credentials = {
  access_token: string;
  refresh_token: string option;
  token_type: string;
  instance_url: string;  (** The Mastodon instance URL (e.g., https://mastodon.social) *)
}

(** Visibility levels for statuses *)
type visibility = 
  | Public      (** Visible to everyone, shown in public timelines *)
  | Unlisted    (** Visible to everyone, but not in public timelines *)
  | Private     (** Visible to followers only *)
  | Direct      (** Visible to mentioned users only *)

(** Convert visibility to API string *)
let visibility_to_string = function
  | Public -> "public"
  | Unlisted -> "unlisted"
  | Private -> "private"
  | Direct -> "direct"

(** Poll option for status polls *)
type poll_option = {
  title: string;
}

(** Poll configuration *)
type poll = {
  options: poll_option list;
  expires_in: int;  (** Duration in seconds *)
  multiple: bool;   (** Allow multiple choices *)
  hide_totals: bool; (** Hide vote counts until poll ends *)
}

type timeline_status = {
  id: string;
  content: string;
  url: string option;
  created_at: string option;
  account_acct: string option;
}

type pagination_cursors = {
  next_max_id: string option;
  prev_since_id: string option;
  prev_min_id: string option;
}

type timeline_page = {
  items: timeline_status list;
  cursors: pagination_cursors;
}

type account_summary = {
  id: string;
  acct: string;
  username: string option;
  display_name: string option;
  url: string option;
}

type notification_item = {
  id: string;
  notification_type: string;
  created_at: string option;
  account: account_summary option;
  status: timeline_status option;
}

type trend_tag = {
  name: string;
  url: string option;
  usage_count: int option;
}

type trend_link = {
  url: string;
  title: string option;
  description: string option;
}

type mastodon_list = {
  id: string;
  title: string;
}

type relationship_summary = {
  id: string;
  following: bool;
  followed_by: bool;
  blocking: bool;
  muting: bool;
}

type search_result = {
  statuses: timeline_status list;
  accounts: account_summary list;
  hashtags: trend_tag list;
}

type conversation_item = {
  id: string;
  unread: bool;
  accounts: account_summary list;
  last_status: timeline_status option;
}

type filter_keyword_rule = {
  keyword: string;
  whole_word: bool option;
}

type mastodon_filter = {
  id: string;
  title: string;
  contexts: string list;
  filter_action: string option;
  expires_at: string option;
}

(** Account information returned by verify_credentials *)
type account_info = {
  id: string;
  username: string;
  acct: string;
  display_name: string;
  avatar: string;
  header: string;
  followers_count: int;
  following_count: int;
  statuses_count: int;
  note: string;
  url: string;
  locked: bool;
  bot: bool;
  created_at: string option;
}

(** Configuration module type for Mastodon provider *)
module type CONFIG = sig
  module Http : HTTP_CLIENT
  
  val get_env : string -> string option
  val get_credentials : account_id:string -> (credentials -> unit) -> (string -> unit) -> unit
  val update_credentials : account_id:string -> credentials:credentials -> (unit -> unit) -> (string -> unit) -> unit
  val encrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val decrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val sleep : seconds:float -> (unit -> unit) -> (string -> unit) -> unit
  val update_health_status : account_id:string -> status:string -> error_message:string option -> (unit -> unit) -> (string -> unit) -> unit
end

(** Make functor to create Mastodon provider with given configuration *)
module Make (Config : CONFIG) = struct
  module OAuth_http = OAuth.Make(Config.Http)
  
  (** {1 Platform Constants} *)
  
  let default_max_status_length = 500  (* Many instances have higher limits *)
  let max_media_per_status = 4

  type instance_limits = {
    max_status_chars: int;
    max_media_attachments: int;
    max_image_size_bytes: int;
    max_video_size_bytes: int;
  }

  let default_instance_limits = {
    max_status_chars = default_max_status_length;
    max_media_attachments = max_media_per_status;
    max_image_size_bytes = 10 * 1024 * 1024;
    max_video_size_bytes = 100 * 1024 * 1024;
  }

  let instance_limits_cache : (string, instance_limits) Hashtbl.t = Hashtbl.create 8
  
  (** {1 Validation Functions} *)
  
  (** Validate a single post's content *)
  let validate_post ~text ?(media_count=0) ?(max_length=500) ?(max_media=max_media_per_status) () =
    let errors = ref [] in
    
    (* Check text length *)
    let text_len = String.length text in
    if text_len > max_length then
      errors := Error_types.Text_too_long { length = text_len; max = max_length } :: !errors;
    
    (* Check media count *)
    if media_count > max_media then
      errors := Error_types.Too_many_media { count = media_count; max = max_media } :: !errors;
    
    if !errors = [] then Ok ()
    else Error (List.rev !errors)
  
  (** Validate thread content *)
  let validate_thread ~texts ?(media_counts=[]) ?(max_length=500) ?(max_media=max_media_per_status) () =
    if List.length texts = 0 then
      Error [Error_types.Thread_empty]
    else
      let errors = ref [] in
      List.iteri (fun i text ->
        let media_count = 
          try List.nth media_counts i 
          with _ -> 0 
        in
        match validate_post ~text ~media_count ~max_length ~max_media () with
        | Error post_errors ->
            errors := Error_types.Thread_post_invalid { index = i; errors = post_errors } :: !errors
        | Ok () -> ()
      ) texts;
      if !errors = [] then Ok ()
      else Error (List.rev !errors)

  let resolve_instance_limits ?(force_refresh=false) ~instance_url on_success =
    if (not force_refresh) && Hashtbl.mem instance_limits_cache instance_url then
      on_success (Hashtbl.find instance_limits_cache instance_url)
    else
    let parse_v2 body =
      let open Yojson.Basic.Util in
      let json = Yojson.Basic.from_string body in
      let cfg = json |> member "configuration" in
      {
        max_status_chars =
          (try cfg |> member "statuses" |> member "max_characters" |> to_int
           with _ -> default_instance_limits.max_status_chars);
        max_media_attachments =
          (try cfg |> member "statuses" |> member "max_media_attachments" |> to_int
           with _ -> default_instance_limits.max_media_attachments);
        max_image_size_bytes =
          (try cfg |> member "media_attachments" |> member "image_size_limit" |> to_int
           with _ -> default_instance_limits.max_image_size_bytes);
        max_video_size_bytes =
          (try cfg |> member "media_attachments" |> member "video_size_limit" |> to_int
           with _ -> default_instance_limits.max_video_size_bytes);
      }
    in
    let parse_v1 body =
      let open Yojson.Basic.Util in
      let json = Yojson.Basic.from_string body in
      {
        max_status_chars =
          (try json |> member "max_toot_chars" |> to_int
           with _ -> default_instance_limits.max_status_chars);
        max_media_attachments = default_instance_limits.max_media_attachments;
        max_image_size_bytes = default_instance_limits.max_image_size_bytes;
        max_video_size_bytes = default_instance_limits.max_video_size_bytes;
      }
    in
    let cache_and_return limits =
      Hashtbl.replace instance_limits_cache instance_url limits;
      on_success limits
    in
    let v1_url = Printf.sprintf "%s/api/v1/instance" instance_url in
    let v2_url = Printf.sprintf "%s/api/v2/instance" instance_url in
    let fallback_to_v1 () =
      Config.Http.get v1_url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try cache_and_return (parse_v1 response.body)
            with _ -> cache_and_return default_instance_limits
          else
            cache_and_return default_instance_limits)
        (fun _ -> cache_and_return default_instance_limits)
    in
    Config.Http.get v2_url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try cache_and_return (parse_v2 response.body)
          with _ -> fallback_to_v1 ()
        else
          fallback_to_v1 ())
      (fun _ -> fallback_to_v1 ())

  (** Parse API error response and return structured Error_types.error *)
  let parse_api_error ~headers ~status_code ~response_body =
    let redacted_body = redact_sensitive_text response_body in
    try
      let json = Yojson.Basic.from_string redacted_body in
      let open Yojson.Basic.Util in
      let error_msg = 
        try json |> member "error" |> to_string
        with _ -> redacted_body
      in
      
      (* Map common Mastodon errors *)
      if status_code = 401 then
        Error_types.Auth_error Error_types.Token_invalid
      else if status_code = 403 then
        Error_types.Auth_error (Error_types.Insufficient_permissions ["write:statuses"])
      else if status_code = 429 then
        let retry_after_seconds =
          match header_value_case_insensitive headers "retry-after" with
          | Some value -> parse_positive_int value
          | None -> None
        in
        Error_types.Rate_limited { 
          retry_after_seconds = (match retry_after_seconds with Some _ as v -> v | None -> Some 300);
          limit = None;
          remaining = Some 0;
          reset_at = None;
        }
      else if status_code = 422 && String.lowercase_ascii error_msg |> fun s -> 
          String.length s > 0 && (String.sub s 0 (min 9 (String.length s)) = "duplicate" ||
                                   try ignore (Str.search_forward (Str.regexp_string "duplicate") s 0); true with Not_found -> false) then
        Error_types.Duplicate_content
      else
        Error_types.Api_error {
          status_code;
          message = error_msg;
          platform = Platform_types.Mastodon;
          raw_response = Some redacted_body;
          request_id = None;
        }
    with _ ->
      Error_types.Api_error {
        status_code;
        message = redacted_body;
        platform = Platform_types.Mastodon;
        raw_response = Some redacted_body;
        request_id = None;
      }
  
  (** Parse Mastodon credentials from core credentials type *)
  let parse_mastodon_credentials (credentials : credentials) on_success on_error =
    try
      (* For Mastodon, the access_token field contains a JSON string with both
         access_token and instance_url (provided by Mastodon_config.get_credentials) *)
      let json = Yojson.Basic.from_string credentials.access_token in
      let open Yojson.Basic.Util in
      let instance_url = json |> member "instance_url" |> to_string in
      let actual_token = json |> member "access_token" |> to_string in
      let mastodon_creds = {
        access_token = actual_token;
        refresh_token = credentials.refresh_token;
        token_type = credentials.token_type;
        instance_url;
      } in
      on_success mastodon_creds
    with e ->
      on_error (Printf.sprintf "Failed to parse Mastodon credentials: %s" (Printexc.to_string e))
  
  (** Convert Mastodon credentials back to core credentials type *)
  let to_core_credentials (mastodon_creds : mastodon_credentials) : credentials =
    (* Store both access_token and instance_url as JSON in the access_token field
       This format is expected by parse_mastodon_credentials *)
    let creds_json = `Assoc [
      ("access_token", `String mastodon_creds.access_token);
      ("instance_url", `String mastodon_creds.instance_url);
    ] |> Yojson.Basic.to_string in
    {
      access_token = creds_json;
      refresh_token = mastodon_creds.refresh_token;
      expires_at = None; (* Mastodon tokens don't expire *)
      token_type = mastodon_creds.token_type;
    }
  
  (** Generate a UUID v4 for idempotency keys *)
  let generate_uuid () =
    let random_byte () = Random.int 256 in
    Printf.sprintf "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x"
      (random_byte ()) (random_byte ()) (random_byte ()) (random_byte ())
      (random_byte ()) (random_byte ())
      ((random_byte () land 0x0f) lor 0x40) (random_byte ())
      ((random_byte () land 0x3f) lor 0x80) (random_byte ())
      (random_byte ()) (random_byte ()) (random_byte ()) (random_byte ()) (random_byte ()) (random_byte ())
  
  (** Verify credentials are valid and return account information *)
  let verify_credentials ~mastodon_creds on_success on_error =
    let url = Printf.sprintf "%s/api/v1/accounts/verify_credentials" mastodon_creds.instance_url in
    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
    ] in

    Config.Http.get ~headers url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            let open Yojson.Basic.Util in
            let str field = try json |> member field |> to_string with _ -> "" in
            let int_field field = try json |> member field |> to_int with _ -> 0 in
            let bool_field field = try json |> member field |> to_bool with _ -> false in
            let opt_str field =
              try match json |> member field with
                | `String v when v <> "" -> Some v
                | _ -> None
              with _ -> None
            in
            let info = {
              id = str "id";
              username = str "username";
              acct = str "acct";
              display_name = str "display_name";
              avatar = str "avatar";
              header = str "header";
              followers_count = int_field "followers_count";
              following_count = int_field "following_count";
              statuses_count = int_field "statuses_count";
              note = str "note";
              url = str "url";
              locked = bool_field "locked";
              bot = bool_field "bot";
              created_at = opt_str "created_at";
            } in
            on_success info
          with e ->
            on_error (Printf.sprintf "Failed to parse verify_credentials response: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "Invalid credentials (%d): %s" response.status response.body))
      on_error
  
  (** Upload media to Mastodon *)
  let upload_media ~mastodon_creds ~media_data ~mime_type ~description ~focus on_success on_error =
    let url = Printf.sprintf "%s/api/v2/media" mastodon_creds.instance_url in
    
    (* Determine filename from mime type *)
    let filename = match mime_type with
      | s when String.starts_with ~prefix:"image/" s -> "media.jpg"
      | s when String.starts_with ~prefix:"video/" s -> "media.mp4"
      | s when String.starts_with ~prefix:"image/gif" s -> "media.gif"
      | _ -> "media.bin"
    in
    
    (* Create multipart form data *)
    let base_parts = [
      {
        name = "file";
        filename = Some filename;
        content_type = Some mime_type;
        content = media_data;
      };
    ] in
    
    let parts_with_desc = match description with
      | Some desc when String.length desc > 0 ->
          base_parts @ [{
            name = "description";
            filename = None;
            content_type = None;
            content = desc;
          }]
      | _ -> base_parts
    in
    
    let parts = match focus with
      | Some (x, y) ->
          parts_with_desc @ [{
            name = "focus";
            filename = None;
            content_type = None;
            content = Printf.sprintf "%f,%f" x y;
          }]
      | None -> parts_with_desc
    in
    
    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
    ] in
    
    Config.Http.post_multipart ~headers ~parts url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            let media_id = json |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_string in
            on_success media_id
          with e ->
            on_error (Printf.sprintf "Failed to parse media response: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "Media upload failed (%d): %s" response.status response.body))
      on_error

  type media_processing_state =
    | Media_ready
    | Media_pending of int option
    | Media_failed of string

  let get_media_processing_state ~mastodon_creds ~media_id on_success on_error =
    let url = Printf.sprintf "%s/api/v1/media/%s" mastodon_creds.instance_url media_id in
    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
    ] in
    Config.Http.get ~headers url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            let open Yojson.Basic.Util in
            let url_ready =
              match json |> member "url" with
              | `String s when String.length s > 0 -> true
              | _ -> false
            in
            if url_ready then
              on_success Media_ready
            else
              let processing_state =
                try Some (json |> member "meta" |> member "processing" |> member "state" |> to_string)
                with _ -> None
              in
              let check_after_secs =
                try
                  Some (json |> member "meta" |> member "processing" |> member "check_after_secs" |> to_int)
                with _ -> None
              in
              match processing_state with
              | Some "succeeded" | Some "ready" | Some "processed" -> on_success Media_ready
              | Some "failed" ->
                  let error_msg =
                    try json |> member "meta" |> member "processing" |> member "error" |> to_string
                    with _ -> "Media processing failed"
                  in
                  on_success (Media_failed error_msg)
              | Some _ -> on_success (Media_pending check_after_secs)
              | None -> on_success Media_ready
          with e ->
            on_error (Printf.sprintf "Failed to parse media status: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "Media status check failed (%d): %s" response.status response.body))
      on_error

  let wait_for_media_ready ~mastodon_creds ~media_id on_success on_error =
    let max_attempts = 30 in
    let rec poll attempts_left =
      if attempts_left <= 0 then
        on_error "Media processing timed out"
      else
        get_media_processing_state ~mastodon_creds ~media_id
          (function
            | Media_ready -> on_success ()
            | Media_failed msg -> on_error msg
            | Media_pending check_after_secs ->
                let wait_secs =
                  match check_after_secs with
                  | Some secs when secs > 0 -> secs
                  | _ -> 1
                in
                Config.sleep ~seconds:(float_of_int wait_secs)
                  (fun () -> poll (attempts_left - 1))
                  (fun err -> on_error (Printf.sprintf "Media processing wait failed: %s" err)))
          on_error
    in
    poll max_attempts
  
  (** Update media with alt text and/or focus point *)
  let update_media ~mastodon_creds ~media_id ~alt_text ~focus on_success on_error =
    let url = Printf.sprintf "%s/api/v1/media/%s" mastodon_creds.instance_url media_id in
    
    let fields = [] in
    let fields = match alt_text with
      | Some text -> ("description", `String text) :: fields
      | None -> fields
    in
    let fields = match focus with
      | Some (x, y) -> ("focus", `String (Printf.sprintf "%f,%f" x y)) :: fields
      | None -> fields
    in
    
    let body_json = `Assoc fields in
    let body = Yojson.Basic.to_string body_json in
    
    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
      ("Content-Type", "application/json");
    ] in
    
    Config.Http.put ~headers ~body url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          on_success ()
        else
          on_error (Printf.sprintf "Media update failed (%d): %s" response.status response.body))
      on_error
  
  (** Ensure valid token (Mastodon tokens don't expire unless revoked) *)
  let ensure_valid_token ~account_id on_success on_error =
    Config.get_credentials ~account_id
      (fun creds ->
        parse_mastodon_credentials creds
          (fun mastodon_creds ->
            verify_credentials ~mastodon_creds
              (fun _account_info ->
                Config.update_health_status ~account_id ~status:"healthy" ~error_message:None
                  (fun () -> on_success mastodon_creds)
                  (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err))))
              (fun err ->
                Config.update_health_status ~account_id ~status:"invalid_token"
                  ~error_message:(Some err)
                  (fun () -> on_error (Error_types.Auth_error Error_types.Token_invalid))
                  (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err)))))
          (fun err -> on_error (Error_types.Auth_error (Error_types.Refresh_failed err))))
      (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err)))

  (** Get resolved instance limits for current account instance *)
  let get_instance_limits ~account_id ?(force_refresh=false) on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        resolve_instance_limits ~force_refresh ~instance_url:mastodon_creds.instance_url
          (fun limits -> on_result (Ok limits)))
      (fun err -> on_result (Error err))

  let parse_timeline_status json =
    let open Yojson.Basic.Util in
    let id = json |> member "id" |> to_string in
    let content =
      try json |> member "content" |> to_string
      with _ -> ""
    in
    let url =
      try
        match json |> member "url" with
        | `String v when v <> "" -> Some v
        | _ -> None
      with _ -> None
    in
    let created_at =
      try
        match json |> member "created_at" with
        | `String v when v <> "" -> Some v
        | _ -> None
      with _ -> None
    in
    let account_acct =
      try
        match json |> member "account" |> member "acct" with
        | `String v when v <> "" -> Some v
        | _ -> None
      with _ -> None
    in
    { id; content; url; created_at; account_acct }

  let parse_account_summary json =
    let open Yojson.Basic.Util in
    let id =
      try json |> member "id" |> to_string
      with _ -> ""
    in
    let acct =
      try json |> member "acct" |> to_string
      with _ -> ""
    in
    let username =
      try
        match json |> member "username" with
        | `String v when v <> "" -> Some v
        | _ -> None
      with _ -> None
    in
    let display_name =
      try
        match json |> member "display_name" with
        | `String v when v <> "" -> Some v
        | _ -> None
      with _ -> None
    in
    let url =
      try
        match json |> member "url" with
        | `String v when v <> "" -> Some v
        | _ -> None
      with _ -> None
    in
    { id; acct; username; display_name; url }

  let parse_notification_item json =
    let open Yojson.Basic.Util in
    let id = json |> member "id" |> to_string in
    let notification_type =
      try json |> member "type" |> to_string
      with _ -> "unknown"
    in
    let created_at =
      try
        match json |> member "created_at" with
        | `String v when v <> "" -> Some v
        | _ -> None
      with _ -> None
    in
    let account =
      try
        match json |> member "account" with
        | `Assoc _ as account_json -> Some (parse_account_summary account_json)
        | _ -> None
      with _ -> None
    in
    let status =
      try
        match json |> member "status" with
        | `Assoc _ as status_json -> Some (parse_timeline_status status_json)
        | _ -> None
      with _ -> None
    in
    { id; notification_type; created_at; account; status }

  let parse_pagination_cursors headers =
    let parse_rel_entry entry =
      let trimmed = String.trim entry in
      let re = Str.regexp "<\\([^>]+\\)>;[ ]*rel=\"\\([^\"]+\\)\"" in
      if Str.string_match re trimmed 0 then
        let url = Str.matched_group 1 trimmed in
        let rel = Str.matched_group 2 trimmed in
        Some (rel, Uri.of_string url)
      else
        None
    in
    let extract_from_uri key uri = Uri.get_query_param uri key in
    let update cursors (rel, uri) =
      match rel with
      | "next" ->
          { cursors with next_max_id = (match extract_from_uri "max_id" uri with Some v -> Some v | None -> cursors.next_max_id) }
      | "prev" ->
          {
            cursors with
            prev_since_id = (match extract_from_uri "since_id" uri with Some v -> Some v | None -> cursors.prev_since_id);
            prev_min_id = (match extract_from_uri "min_id" uri with Some v -> Some v | None -> cursors.prev_min_id);
          }
      | _ -> cursors
    in
    let empty = { next_max_id = None; prev_since_id = None; prev_min_id = None } in
    match header_value_case_insensitive headers "link" with
    | None -> empty
    | Some link_header ->
        link_header
        |> String.split_on_char ','
        |> List.filter_map parse_rel_entry
        |> List.fold_left update empty

  let parse_trend_tag json =
    let open Yojson.Basic.Util in
    let name =
      try json |> member "name" |> to_string
      with _ -> ""
    in
    let url =
      try
        match json |> member "url" with
        | `String v when v <> "" -> Some v
        | _ -> None
      with _ -> None
    in
    let usage_count =
      try
        match json |> member "history" |> to_list with
        | [] -> None
        | history_items ->
            let parse_uses item =
              try item |> member "uses" |> to_int
              with _ ->
                try item |> member "uses" |> to_string |> int_of_string
                with _ -> 0
            in
            let total =
              history_items
              |> List.fold_left
                   (fun acc item -> acc + parse_uses item)
                   0
            in
            Some total
      with _ -> None
    in
    { name; url; usage_count }

  let parse_trend_link json =
    let open Yojson.Basic.Util in
    let url =
      try json |> member "url" |> to_string
      with _ -> ""
    in
    let title =
      try
        match json |> member "title" with
        | `String v when v <> "" -> Some v
        | _ -> None
      with _ -> None
    in
    let description =
      try
        match json |> member "description" with
        | `String v when v <> "" -> Some v
        | _ -> None
      with _ -> None
    in
    { url; title; description }

  let parse_mastodon_list json =
    let open Yojson.Basic.Util in
    let id =
      try json |> member "id" |> to_string
      with _ -> ""
    in
    let title =
      try json |> member "title" |> to_string
      with _ -> ""
    in
    { id; title }

  let parse_relationship_summary json =
    let open Yojson.Basic.Util in
    let id = json |> member "id" |> to_string in
    let following =
      try json |> member "following" |> to_bool
      with _ -> false
    in
    let followed_by =
      try json |> member "followed_by" |> to_bool
      with _ -> false
    in
    let blocking =
      try json |> member "blocking" |> to_bool
      with _ -> false
    in
    let muting =
      try json |> member "muting" |> to_bool
      with _ -> false
    in
    { id; following; followed_by; blocking; muting }

  let parse_conversation_item json =
    let open Yojson.Basic.Util in
    let id =
      try json |> member "id" |> to_string
      with _ -> ""
    in
    let unread =
      try json |> member "unread" |> to_bool
      with _ -> false
    in
    let accounts =
      try
        match json |> member "accounts" with
        | `List items -> List.map parse_account_summary items
        | _ -> []
      with _ -> []
    in
    let last_status =
      try
        match json |> member "last_status" with
        | (`Assoc _ as status) -> Some (parse_timeline_status status)
        | _ -> None
      with _ -> None
    in
    { id; unread; accounts; last_status }

  let parse_filter_summary json =
    let open Yojson.Basic.Util in
    let id =
      try json |> member "id" |> to_string
      with _ -> ""
    in
    let title =
      try json |> member "title" |> to_string
      with _ -> ""
    in
    let contexts =
      try
        match json |> member "context" with
        | `List items -> List.map to_string items
        | _ -> []
      with _ -> []
    in
    let filter_action =
      try
        match json |> member "filter_action" with
        | `String v when v <> "" -> Some v
        | _ -> None
      with _ -> None
    in
    let expires_at =
      try
        match json |> member "expires_at" with
        | `String v when v <> "" -> Some v
        | _ -> None
      with _ -> None
    in
    { id; title; contexts; filter_action; expires_at }

  (** Get a single status by id *)
  let get_status ~account_id ~status_id on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/statuses/%s" mastodon_creds.instance_url status_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok (parse_timeline_status json))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse status response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Get home timeline statuses *)
  let get_home_timeline_with_pagination
      ~account_id
      ?(limit=20)
      ?(max_id=None)
      ?(since_id=None)
      ?(min_id=None)
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let base_url = Printf.sprintf "%s/api/v1/timelines/home" mastodon_creds.instance_url in
        let uri = Uri.of_string base_url in
        let query =
          let base = [ ("limit", string_of_int limit) ] in
          let with_max = match max_id with Some v -> ("max_id", v) :: base | None -> base in
          let with_since = match since_id with Some v -> ("since_id", v) :: with_max | None -> with_max in
          match min_id with Some v -> ("min_id", v) :: with_since | None -> with_since
        in
        let url = Uri.with_query' uri query |> Uri.to_string in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let open Yojson.Basic.Util in
                let statuses =
                  json
                  |> to_list
                  |> List.map parse_timeline_status
                in
                let cursors = parse_pagination_cursors response.headers in
                on_result (Ok { items = statuses; cursors })
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse timeline response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Get home timeline statuses *)
  let get_home_timeline
      ~account_id
      ?(limit=20)
      ?max_id
      ?since_id
      ?min_id
      on_result =
    get_home_timeline_with_pagination
      ~account_id ~limit ?max_id ?since_id ?min_id
      (function
        | Ok page -> on_result (Ok page.items)
        | Error err -> on_result (Error err))

  (** Get public timeline statuses *)
  let get_public_timeline
      ~account_id
      ?(limit=20)
      ?(local=false)
      ?(max_id=None)
      ?(since_id=None)
      ?(min_id=None)
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let base_url = Printf.sprintf "%s/api/v1/timelines/public" mastodon_creds.instance_url in
        let uri = Uri.of_string base_url in
        let query =
          let base = [
            ("limit", string_of_int limit);
            ("local", if local then "true" else "false");
          ] in
          let with_max = match max_id with Some v -> ("max_id", v) :: base | None -> base in
          let with_since = match since_id with Some v -> ("since_id", v) :: with_max | None -> with_max in
          match min_id with Some v -> ("min_id", v) :: with_since | None -> with_since
        in
        let url = Uri.with_query' uri query |> Uri.to_string in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let open Yojson.Basic.Util in
                let statuses = json |> to_list |> List.map parse_timeline_status in
                on_result (Ok statuses)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse public timeline response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Get hashtag timeline statuses *)
  let get_tag_timeline
      ~account_id
      ~tag
      ?(limit=20)
      ?(max_id=None)
      ?(since_id=None)
      ?(min_id=None)
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let encoded_tag = Uri.pct_encode tag in
        let base_url = Printf.sprintf "%s/api/v1/timelines/tag/%s" mastodon_creds.instance_url encoded_tag in
        let uri = Uri.of_string base_url in
        let query =
          let base = [ ("limit", string_of_int limit) ] in
          let with_max = match max_id with Some v -> ("max_id", v) :: base | None -> base in
          let with_since = match since_id with Some v -> ("since_id", v) :: with_max | None -> with_max in
          match min_id with Some v -> ("min_id", v) :: with_since | None -> with_since
        in
        let url = Uri.with_query' uri query |> Uri.to_string in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let open Yojson.Basic.Util in
                let statuses = json |> to_list |> List.map parse_timeline_status in
                on_result (Ok statuses)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse tag timeline response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Search statuses by query *)
  let search_statuses
      ~account_id
      ~query
      ?(limit=20)
      ?(resolve=false)
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let base_url = Printf.sprintf "%s/api/v2/search" mastodon_creds.instance_url in
        let uri = Uri.of_string base_url in
        let url =
          Uri.with_query' uri [
            ("q", query);
            ("type", "statuses");
            ("resolve", if resolve then "true" else "false");
            ("limit", string_of_int limit);
          ]
          |> Uri.to_string
        in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let open Yojson.Basic.Util in
                let statuses =
                  match json |> member "statuses" with
                  | `List items -> List.map parse_timeline_status items
                  | `Null -> []
                  | _ -> []
                in
                on_result (Ok statuses)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse search response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Search across statuses, accounts, and hashtags *)
  let search_all
      ~account_id
      ~query
      ?(limit=20)
      ?(resolve=false)
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let base_url = Printf.sprintf "%s/api/v2/search" mastodon_creds.instance_url in
        let uri = Uri.of_string base_url in
        let url =
          Uri.with_query' uri
            [
              ("q", query);
              ("resolve", if resolve then "true" else "false");
              ("limit", string_of_int limit);
            ]
          |> Uri.to_string
        in
        let headers =
          [ ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token) ]
        in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let open Yojson.Basic.Util in
                let statuses =
                  match json |> member "statuses" with
                  | `List items -> List.map parse_timeline_status items
                  | `Null -> []
                  | _ -> []
                in
                let accounts =
                  match json |> member "accounts" with
                  | `List items -> List.map parse_account_summary items
                  | `Null -> []
                  | _ -> []
                in
                let hashtags =
                  match json |> member "hashtags" with
                  | `List items -> List.map parse_trend_tag items
                  | `Null -> []
                  | _ -> []
                in
                on_result (Ok { statuses; accounts; hashtags })
              with e ->
                on_result
                  (Error
                     (Error_types.Internal_error
                        (Printf.sprintf "Failed to parse search-all response: %s"
                           (Printexc.to_string e))))
            else
              on_result
                (Error
                   (parse_api_error ~headers:response.headers
                      ~status_code:response.status ~response_body:response.body)))
          (fun err ->
            on_result
              (Error
                 (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Search accounts by query *)
  let search_accounts
      ~account_id
      ~query
      ?(limit=20)
      ?(resolve=false)
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let base_url = Printf.sprintf "%s/api/v1/accounts/search" mastodon_creds.instance_url in
        let uri = Uri.of_string base_url in
        let url =
          Uri.with_query' uri [
            ("q", query);
            ("resolve", if resolve then "true" else "false");
            ("limit", string_of_int limit);
          ]
          |> Uri.to_string
        in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let accounts =
                  match json with
                  | `List items -> List.map parse_account_summary items
                  | `Null -> []
                  | _ -> []
                in
                on_result (Ok accounts)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse account search response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Search hashtags by query via v2 search endpoint *)
  let search_hashtags
      ~account_id
      ~query
      ?(limit=20)
      ?(resolve=false)
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let base_url = Printf.sprintf "%s/api/v2/search" mastodon_creds.instance_url in
        let uri = Uri.of_string base_url in
        let url =
          Uri.with_query' uri
            [
              ("q", query);
              ("type", "hashtags");
              ("resolve", if resolve then "true" else "false");
              ("limit", string_of_int limit);
            ]
          |> Uri.to_string
        in
        let headers =
          [ ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token) ]
        in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let open Yojson.Basic.Util in
                let tags =
                  match json |> member "hashtags" with
                  | `List items -> List.map parse_trend_tag items
                  | `Null -> []
                  | _ -> []
                in
                on_result (Ok tags)
              with e ->
                on_result
                  (Error
                     (Error_types.Internal_error
                        (Printf.sprintf "Failed to parse hashtag search response: %s"
                           (Printexc.to_string e))))
            else
              on_result
                (Error
                   (parse_api_error ~headers:response.headers
                      ~status_code:response.status ~response_body:response.body)))
          (fun err ->
            on_result
              (Error
                 (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Lookup an account by acct handle (e.g. user@instance) *)
  let lookup_account_by_acct
      ~account_id
      ~acct
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let base_url = Printf.sprintf "%s/api/v1/accounts/lookup" mastodon_creds.instance_url in
        let uri = Uri.of_string base_url in
        let url = Uri.with_query' uri [ ("acct", acct) ] |> Uri.to_string in
        let headers =
          [ ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token) ]
        in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok (parse_account_summary json))
              with e ->
                on_result
                  (Error
                     (Error_types.Internal_error
                        (Printf.sprintf "Failed to parse account lookup response: %s"
                           (Printexc.to_string e))))
            else
              on_result
                (Error
                   (parse_api_error ~headers:response.headers
                      ~status_code:response.status ~response_body:response.body)))
          (fun err ->
            on_result
              (Error
                 (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Get notifications *)
  let get_notifications
      ~account_id
      ?(limit=20)
      ?(max_id=None)
      ?(since_id=None)
      ?(min_id=None)
      ?(exclude_types=[])
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let base_url = Printf.sprintf "%s/api/v1/notifications" mastodon_creds.instance_url in
        let uri = Uri.of_string base_url in
        let query =
          [
            ("limit", [string_of_int limit]);
            ("max_id", (match max_id with Some v -> [v] | None -> []));
            ("since_id", (match since_id with Some v -> [v] | None -> []));
            ("min_id", (match min_id with Some v -> [v] | None -> []));
            ("exclude_types[]", exclude_types);
          ]
        in
        let url = Uri.with_query uri query |> Uri.to_string in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let notifications =
                  match json with
                  | `List items -> List.map parse_notification_item items
                  | `Null -> []
                  | _ -> []
                in
                on_result (Ok notifications)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse notifications response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Get direct-message conversations *)
  let get_conversations
      ~account_id
      ?(limit=20)
      ?(max_id=None)
      ?(since_id=None)
      ?(min_id=None)
      ?(unread_only=false)
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let base_url = Printf.sprintf "%s/api/v1/conversations" mastodon_creds.instance_url in
        let uri = Uri.of_string base_url in
        let query =
          let base =
            [
              ("limit", string_of_int limit);
              ("unread", if unread_only then "true" else "false");
            ]
          in
          let with_max = match max_id with Some v -> ("max_id", v) :: base | None -> base in
          let with_since = match since_id with Some v -> ("since_id", v) :: with_max | None -> with_max in
          match min_id with Some v -> ("min_id", v) :: with_since | None -> with_since
        in
        let url = Uri.with_query' uri query |> Uri.to_string in
        let headers =
          [ ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token) ]
        in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let conversations =
                  match json with
                  | `List items -> List.map parse_conversation_item items
                  | `Null -> []
                  | _ -> []
                in
                on_result (Ok conversations)
              with e ->
                on_result
                  (Error
                     (Error_types.Internal_error
                        (Printf.sprintf "Failed to parse conversations response: %s"
                           (Printexc.to_string e))))
            else
              on_result
                (Error
                   (parse_api_error ~headers:response.headers
                      ~status_code:response.status ~response_body:response.body)))
          (fun err ->
            on_result
              (Error
                 (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Mark a conversation as read *)
  let mark_conversation_read
      ~account_id
      ~conversation_id
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url =
          Printf.sprintf "%s/api/v1/conversations/%s/read" mastodon_creds.instance_url
            conversation_id
        in
        let headers =
          [ ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token) ]
        in
        Config.Http.post ~headers ~body:"" url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_result (Ok ())
            else
              on_result
                (Error
                   (parse_api_error ~headers:response.headers
                      ~status_code:response.status ~response_body:response.body)))
          (fun err ->
            on_result
              (Error
                 (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Remove a conversation *)
  let remove_conversation
      ~account_id
      ~conversation_id
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url =
          Printf.sprintf "%s/api/v1/conversations/%s/remove"
            mastodon_creds.instance_url conversation_id
        in
        let headers =
          [ ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token) ]
        in
        Config.Http.post ~headers ~body:"" url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_result (Ok ())
            else
              on_result
                (Error
                   (parse_api_error ~headers:response.headers
                      ~status_code:response.status ~response_body:response.body)))
          (fun err ->
            on_result
              (Error
                 (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Get bookmarked statuses for the authorized account *)
  let get_bookmarks
      ~account_id
      ?(limit=20)
      ?(max_id=None)
      ?(since_id=None)
      ?(min_id=None)
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let base_url = Printf.sprintf "%s/api/v1/bookmarks" mastodon_creds.instance_url in
        let uri = Uri.of_string base_url in
        let query =
          let base = [ ("limit", string_of_int limit) ] in
          let with_max = match max_id with Some v -> ("max_id", v) :: base | None -> base in
          let with_since = match since_id with Some v -> ("since_id", v) :: with_max | None -> with_max in
          match min_id with Some v -> ("min_id", v) :: with_since | None -> with_since
        in
        let url = Uri.with_query' uri query |> Uri.to_string in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let statuses =
                  match json with
                  | `List items -> List.map parse_timeline_status items
                  | `Null -> []
                  | _ -> []
                in
                on_result (Ok statuses)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse bookmarks response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Get favourited statuses for the authorized account *)
  let get_favourites
      ~account_id
      ?(limit=20)
      ?(max_id=None)
      ?(since_id=None)
      ?(min_id=None)
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let base_url = Printf.sprintf "%s/api/v1/favourites" mastodon_creds.instance_url in
        let uri = Uri.of_string base_url in
        let query =
          let base = [ ("limit", string_of_int limit) ] in
          let with_max = match max_id with Some v -> ("max_id", v) :: base | None -> base in
          let with_since = match since_id with Some v -> ("since_id", v) :: with_max | None -> with_max in
          match min_id with Some v -> ("min_id", v) :: with_since | None -> with_since
        in
        let url = Uri.with_query' uri query |> Uri.to_string in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let statuses =
                  match json with
                  | `List items -> List.map parse_timeline_status items
                  | `Null -> []
                  | _ -> []
                in
                on_result (Ok statuses)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse favourites response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Get lists owned by the authorized account *)
  let get_lists
      ~account_id
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/lists" mastodon_creds.instance_url in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let lists =
                  match json with
                  | `List items -> List.map parse_mastodon_list items
                  | `Null -> []
                  | _ -> []
                in
                on_result (Ok lists)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse lists response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Get timeline for a list *)
  let get_list_timeline
      ~account_id
      ~list_id
      ?(limit=20)
      ?(max_id=None)
      ?(since_id=None)
      ?(min_id=None)
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let base_url = Printf.sprintf "%s/api/v1/timelines/list/%s" mastodon_creds.instance_url list_id in
        let uri = Uri.of_string base_url in
        let query =
          let base = [ ("limit", string_of_int limit) ] in
          let with_max = match max_id with Some v -> ("max_id", v) :: base | None -> base in
          let with_since = match since_id with Some v -> ("since_id", v) :: with_max | None -> with_max in
          match min_id with Some v -> ("min_id", v) :: with_since | None -> with_since
        in
        let url = Uri.with_query' uri query |> Uri.to_string in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let statuses =
                  match json with
                  | `List items -> List.map parse_timeline_status items
                  | `Null -> []
                  | _ -> []
                in
                on_result (Ok statuses)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse list timeline response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Get accounts in a list *)
  let get_list_accounts
      ~account_id
      ~list_id
      ?(limit=40)
      ?(max_id=None)
      ?(since_id=None)
      ?(min_id=None)
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let base_url = Printf.sprintf "%s/api/v1/lists/%s/accounts" mastodon_creds.instance_url list_id in
        let uri = Uri.of_string base_url in
        let query =
          let base = [ ("limit", string_of_int limit) ] in
          let with_max = match max_id with Some v -> ("max_id", v) :: base | None -> base in
          let with_since = match since_id with Some v -> ("since_id", v) :: with_max | None -> with_max in
          match min_id with Some v -> ("min_id", v) :: with_since | None -> with_since
        in
        let url = Uri.with_query' uri query |> Uri.to_string in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let accounts =
                  match json with
                  | `List items -> List.map parse_account_summary items
                  | `Null -> []
                  | _ -> []
                in
                on_result (Ok accounts)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse list accounts response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Get lists that include a specific account *)
  let get_account_lists
      ~account_id
      ~target_account_id
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/accounts/%s/lists" mastodon_creds.instance_url target_account_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let lists =
                  match json with
                  | `List items -> List.map parse_mastodon_list items
                  | `Null -> []
                  | _ -> []
                in
                on_result (Ok lists)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse account lists response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Create a new list *)
  let create_list
      ~account_id
      ~title
      ?(replies_policy=None)
      ?(exclusive=None)
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/lists" mastodon_creds.instance_url in
        let fields = [ ("title", `String title) ] in
        let fields =
          match replies_policy with
          | Some v -> ("replies_policy", `String v) :: fields
          | None -> fields
        in
        let fields =
          match exclusive with
          | Some v -> ("exclusive", `Bool v) :: fields
          | None -> fields
        in
        let body = Yojson.Basic.to_string (`Assoc fields) in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
          ("Content-Type", "application/json");
        ] in
        Config.Http.post ~headers ~body url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok (parse_mastodon_list json))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse create list response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Update an existing list *)
  let update_list
      ~account_id
      ~list_id
      ?(title=None)
      ?(replies_policy=None)
      ?(exclusive=None)
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/lists/%s" mastodon_creds.instance_url list_id in
        let fields = [] in
        let fields =
          match title with
          | Some v -> ("title", `String v) :: fields
          | None -> fields
        in
        let fields =
          match replies_policy with
          | Some v -> ("replies_policy", `String v) :: fields
          | None -> fields
        in
        let fields =
          match exclusive with
          | Some v -> ("exclusive", `Bool v) :: fields
          | None -> fields
        in
        let body = Yojson.Basic.to_string (`Assoc fields) in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
          ("Content-Type", "application/json");
        ] in
        Config.Http.put ~headers ~body url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok (parse_mastodon_list json))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse update list response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Delete a list *)
  let delete_list
      ~account_id
      ~list_id
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/lists/%s" mastodon_creds.instance_url list_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.delete ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_result (Ok ())
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Add accounts to a list *)
  let add_accounts_to_list
      ~account_id
      ~list_id
      ~target_account_ids
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/lists/%s/accounts" mastodon_creds.instance_url list_id in
        let body = Yojson.Basic.to_string (`Assoc [ ("account_ids", `List (List.map (fun id -> `String id) target_account_ids)) ]) in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
          ("Content-Type", "application/json");
        ] in
        Config.Http.post ~headers ~body url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_result (Ok ())
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Remove accounts from a list *)
  let remove_accounts_from_list
      ~account_id
      ~list_id
      ~target_account_ids
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let base_url = Printf.sprintf "%s/api/v1/lists/%s/accounts" mastodon_creds.instance_url list_id in
        let uri = Uri.of_string base_url in
        let query = [ ("account_ids[]", target_account_ids) ] in
        let url = Uri.with_query uri query |> Uri.to_string in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.delete ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_result (Ok ())
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Get filters configured for the authorized account *)
  let get_filters
      ~account_id
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v2/filters" mastodon_creds.instance_url in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let filters =
                  match json with
                  | `List items -> List.map parse_filter_summary items
                  | `Null -> []
                  | _ -> []
                in
                on_result (Ok filters)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse filters response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  let keyword_rules_to_json keywords =
    `List
      (List.map
         (fun rule ->
           let fields = [ ("keyword", `String rule.keyword) ] in
           let fields =
             match rule.whole_word with
             | Some value -> ("whole_word", `Bool value) :: fields
             | None -> fields
           in
           `Assoc fields)
         keywords)

  let status_rules_to_json statuses =
    `List (List.map (fun status_id -> `Assoc [ ("status_id", `String status_id) ]) statuses)

  (** Create a filter for the authorized account *)
  let create_filter
      ~account_id
      ~title
      ~contexts
      ?(filter_action=None)
      ?(expires_in=None)
      ?(keywords=[])
      ?(statuses=[])
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v2/filters" mastodon_creds.instance_url in
        let fields = [
          ("title", `String title);
          ("context", `List (List.map (fun ctx -> `String ctx) contexts));
          ("keywords_attributes", keyword_rules_to_json keywords);
          ("statuses_attributes", status_rules_to_json statuses);
        ] in
        let fields =
          match filter_action with
          | Some value -> ("filter_action", `String value) :: fields
          | None -> fields
        in
        let fields =
          match expires_in with
          | Some value -> ("expires_in", `Int value) :: fields
          | None -> fields
        in
        let body = Yojson.Basic.to_string (`Assoc fields) in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
          ("Content-Type", "application/json");
        ] in
        Config.Http.post ~headers ~body url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok (parse_filter_summary json))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse create filter response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Update an existing filter *)
  let update_filter
      ~account_id
      ~filter_id
      ?(title=None)
      ?(contexts=None)
      ?(filter_action=None)
      ?(expires_in=None)
      ?(keywords=None)
      ?(statuses=None)
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v2/filters/%s" mastodon_creds.instance_url filter_id in
        let fields = [] in
        let fields =
          match title with
          | Some value -> ("title", `String value) :: fields
          | None -> fields
        in
        let fields =
          match contexts with
          | Some value -> ("context", `List (List.map (fun ctx -> `String ctx) value)) :: fields
          | None -> fields
        in
        let fields =
          match filter_action with
          | Some value -> ("filter_action", `String value) :: fields
          | None -> fields
        in
        let fields =
          match expires_in with
          | Some value -> ("expires_in", `Int value) :: fields
          | None -> fields
        in
        let fields =
          match keywords with
          | Some value -> ("keywords_attributes", keyword_rules_to_json value) :: fields
          | None -> fields
        in
        let fields =
          match statuses with
          | Some value -> ("statuses_attributes", status_rules_to_json value) :: fields
          | None -> fields
        in
        let body = Yojson.Basic.to_string (`Assoc fields) in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
          ("Content-Type", "application/json");
        ] in
        Config.Http.put ~headers ~body url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok (parse_filter_summary json))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse update filter response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Delete a filter *)
  let delete_filter
      ~account_id
      ~filter_id
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v2/filters/%s" mastodon_creds.instance_url filter_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.delete ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_result (Ok ())
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Get trending tags *)
  let get_trending_tags
      ~account_id
      ?(limit=10)
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let base_url = Printf.sprintf "%s/api/v1/trends/tags" mastodon_creds.instance_url in
        let url = Uri.of_string base_url |> fun u -> Uri.with_query' u [ ("limit", string_of_int limit) ] |> Uri.to_string in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let tags =
                  match json with
                  | `List items -> List.map parse_trend_tag items
                  | `Null -> []
                  | _ -> []
                in
                on_result (Ok tags)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse trending tags response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Get trending statuses *)
  let get_trending_statuses
      ~account_id
      ?(limit=10)
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let base_url = Printf.sprintf "%s/api/v1/trends/statuses" mastodon_creds.instance_url in
        let url = Uri.of_string base_url |> fun u -> Uri.with_query' u [ ("limit", string_of_int limit) ] |> Uri.to_string in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let statuses =
                  match json with
                  | `List items -> List.map parse_timeline_status items
                  | `Null -> []
                  | _ -> []
                in
                on_result (Ok statuses)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse trending statuses response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Get trending links *)
  let get_trending_links
      ~account_id
      ?(limit=10)
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let base_url = Printf.sprintf "%s/api/v1/trends/links" mastodon_creds.instance_url in
        let url = Uri.of_string base_url |> fun u -> Uri.with_query' u [ ("limit", string_of_int limit) ] |> Uri.to_string in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let links =
                  match json with
                  | `List items -> List.map parse_trend_link items
                  | `Null -> []
                  | _ -> []
                in
                on_result (Ok links)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse trending links response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Get account details by account id *)
  let get_account
      ~account_id
      ~target_account_id
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/accounts/%s" mastodon_creds.instance_url target_account_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok (parse_account_summary json))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse account response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Get featured hashtags for a specific account *)
  let get_account_featured_tags
      ~account_id
      ~target_account_id
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/accounts/%s/featured_tags" mastodon_creds.instance_url target_account_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let tags =
                  match json with
                  | `List items -> List.map parse_trend_tag items
                  | `Null -> []
                  | _ -> []
                in
                on_result (Ok tags)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse featured tags response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Get statuses for a specific account *)
  let get_account_statuses
      ~account_id
      ~target_account_id
      ?(limit=20)
      ?(exclude_replies=false)
      ?(exclude_reblogs=false)
      ?(max_id=None)
      ?(since_id=None)
      ?(min_id=None)
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let base_url = Printf.sprintf "%s/api/v1/accounts/%s/statuses" mastodon_creds.instance_url target_account_id in
        let uri = Uri.of_string base_url in
        let query =
          let base = [
            ("limit", string_of_int limit);
            ("exclude_replies", if exclude_replies then "true" else "false");
            ("exclude_reblogs", if exclude_reblogs then "true" else "false");
          ] in
          let with_max = match max_id with Some v -> ("max_id", v) :: base | None -> base in
          let with_since = match since_id with Some v -> ("since_id", v) :: with_max | None -> with_max in
          match min_id with Some v -> ("min_id", v) :: with_since | None -> with_since
        in
        let url = Uri.with_query' uri query |> Uri.to_string in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let open Yojson.Basic.Util in
                let statuses = json |> to_list |> List.map parse_timeline_status in
                on_result (Ok statuses)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse account statuses response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Get followers for a specific account *)
  let get_account_followers
      ~account_id
      ~target_account_id
      ?(limit=40)
      ?(max_id=None)
      ?(since_id=None)
      ?(min_id=None)
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let base_url = Printf.sprintf "%s/api/v1/accounts/%s/followers" mastodon_creds.instance_url target_account_id in
        let uri = Uri.of_string base_url in
        let query =
          let base = [ ("limit", string_of_int limit) ] in
          let with_max = match max_id with Some v -> ("max_id", v) :: base | None -> base in
          let with_since = match since_id with Some v -> ("since_id", v) :: with_max | None -> with_max in
          match min_id with Some v -> ("min_id", v) :: with_since | None -> with_since
        in
        let url = Uri.with_query' uri query |> Uri.to_string in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let accounts =
                  match json with
                  | `List items -> List.map parse_account_summary items
                  | `Null -> []
                  | _ -> []
                in
                on_result (Ok accounts)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse followers response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Get following accounts for a specific account *)
  let get_account_following
      ~account_id
      ~target_account_id
      ?(limit=40)
      ?(max_id=None)
      ?(since_id=None)
      ?(min_id=None)
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let base_url = Printf.sprintf "%s/api/v1/accounts/%s/following" mastodon_creds.instance_url target_account_id in
        let uri = Uri.of_string base_url in
        let query =
          let base = [ ("limit", string_of_int limit) ] in
          let with_max = match max_id with Some v -> ("max_id", v) :: base | None -> base in
          let with_since = match since_id with Some v -> ("since_id", v) :: with_max | None -> with_max in
          match min_id with Some v -> ("min_id", v) :: with_since | None -> with_since
        in
        let url = Uri.with_query' uri query |> Uri.to_string in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let accounts =
                  match json with
                  | `List items -> List.map parse_account_summary items
                  | `Null -> []
                  | _ -> []
                in
                on_result (Ok accounts)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse following response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** Get relationship status for account ids *)
  let get_account_relationships
      ~account_id
      ~target_account_ids
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let base_url = Printf.sprintf "%s/api/v1/accounts/relationships" mastodon_creds.instance_url in
        let uri = Uri.of_string base_url in
        let query = [ ("id[]", target_account_ids) ] in
        let url = Uri.with_query uri query |> Uri.to_string in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let open Yojson.Basic.Util in
                let relationships = json |> to_list |> List.map parse_relationship_summary in
                on_result (Ok relationships)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse relationships response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Post single status with full options
      
      @param validate_media_before_upload When true, validates media size after download
             but before upload. Mastodon limits: 100MB video, 2hr duration, 10MB images.
             Note: Actual limits depend on instance configuration.
             Default: false
  *)
  let post_single 
      ~account_id 
      ~text 
      ~media_urls 
      ?(alt_texts=[])
      ?(visibility=Public)
      ?(sensitive=false)
      ?(spoiler_text=None)
      ?(in_reply_to_id=None)
      ?(language=None)
      ?(poll=None)
      ?(scheduled_at=None)
      ?(idempotency_key=None)
      ?(validate_media_before_upload=false)
      on_result =
    (* Minimal pre-validation only; enforce instance-specific limits after resolving instance config. *)
    let media_count = List.length media_urls in
    match validate_post ~text ~media_count ~max_length:max_int ~max_media:max_int () with
    | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        resolve_instance_limits ~instance_url:mastodon_creds.instance_url (fun limits ->
        match validate_post ~text ~media_count ~max_length:limits.max_status_chars ~max_media:limits.max_media_attachments () with
        | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
        | Ok () ->
        (* Pair URLs with alt text - use None if alt text list is shorter *)
        let urls_with_alt = List.mapi (fun i url ->
          let alt_text = try List.nth alt_texts i with _ -> None in
          (url, alt_text)
        ) media_urls in
        
        (* Helper to determine media type from MIME type *)
        let media_type_of_mime mime_type =
          if String.starts_with ~prefix:"video/" mime_type then Platform_types.Video
          else if mime_type = "image/gif" then Platform_types.Gif
          else Platform_types.Image
        in
        
        (* Helper to upload media from URLs *)
        let rec upload_media_seq urls_with_alt acc on_complete on_err =
          match urls_with_alt with
          | [] -> on_complete (List.rev acc)
          | (url, alt_text) :: rest ->
              (* Fetch media from URL *)
              Config.Http.get url
                (fun media_resp ->
                  if media_resp.status >= 200 && media_resp.status < 300 then
                    let mime_type = 
                      List.assoc_opt "content-type" media_resp.headers 
                      |> Option.value ~default:"image/jpeg"
                    in
                    let file_size = String.length media_resp.body in
                    
                    (* Validate media if requested - inline validation for Mastodon *)
                    let validation_error =
                      if validate_media_before_upload then
                        let media_type = media_type_of_mime mime_type in
                        match media_type with
                        | Platform_types.Image when file_size > limits.max_image_size_bytes ->
                            Some (Error_types.Media_too_large { 
                              size_bytes = file_size; 
                              max_bytes = limits.max_image_size_bytes
                            })
                        | Platform_types.Video when file_size > limits.max_video_size_bytes ->
                            Some (Error_types.Media_too_large { 
                              size_bytes = file_size; 
                              max_bytes = limits.max_video_size_bytes
                            })
                        | Platform_types.Gif when file_size > limits.max_image_size_bytes ->
                            Some (Error_types.Media_too_large { 
                              size_bytes = file_size; 
                              max_bytes = limits.max_image_size_bytes
                            })
                        | _ -> None
                      else
                        None
                    in
                    
                    (match validation_error with
                    | Some err ->
                        on_result (Error_types.Failure (Error_types.Validation_error [err]))
                    | None ->
                        (* Upload to Mastodon with alt text *)
                        upload_media ~mastodon_creds 
                          ~media_data:media_resp.body ~mime_type ~description:alt_text ~focus:None
                          (fun media_id ->
                            if String.starts_with ~prefix:"video/" mime_type then
                              wait_for_media_ready ~mastodon_creds ~media_id
                                (fun () -> upload_media_seq rest (media_id :: acc) on_complete on_err)
                                on_err
                            else
                              upload_media_seq rest (media_id :: acc) on_complete on_err)
                          on_err)
                  else
                    on_err (Printf.sprintf "Failed to fetch media from %s" url))
                on_err
        in
        
        (* Upload media if provided (instance-specific max) *)
        let media_to_upload = List.filteri (fun i _ -> i < limits.max_media_attachments) urls_with_alt in
        let on_media_error msg = on_result (Error_types.Failure (Error_types.Internal_error (Printf.sprintf "Media upload failed: %s" msg))) in
        upload_media_seq media_to_upload []
          (fun media_ids ->
             let url = Printf.sprintf "%s/api/v1/statuses" mastodon_creds.instance_url in
             let base_fields = [
               ("status", `String text);
               ("visibility", `String (visibility_to_string visibility));
               ("sensitive", `Bool sensitive);
             ] in
             let fields =
               match spoiler_text with
               | Some spoiler when String.length spoiler > 0 -> ("spoiler_text", `String spoiler) :: base_fields
               | _ -> base_fields
             in
             let fields =
               match in_reply_to_id with
               | Some id -> ("in_reply_to_id", `String id) :: fields
               | None -> fields
             in
             let fields =
               match language with
               | Some lang -> ("language", `String lang) :: fields
               | None -> fields
             in
             let fields =
               match scheduled_at with
               | Some datetime -> ("scheduled_at", `String datetime) :: fields
               | None -> fields
             in
             let fields =
               if media_ids <> [] then
                 ("media_ids", `List (List.map (fun id -> `String id) media_ids)) :: fields
               else
                 fields
             in
             let fields =
               match poll with
               | Some (p : poll) ->
                   let poll_json = `Assoc [
                     ("options", `List (List.map (fun (opt : poll_option) -> `String opt.title) p.options));
                     ("expires_in", `Int p.expires_in);
                     ("multiple", `Bool p.multiple);
                     ("hide_totals", `Bool p.hide_totals);
                   ] in
                   ("poll", poll_json) :: fields
               | None -> fields
             in
             let body = Yojson.Basic.to_string (`Assoc fields) in
             let idem_key =
               match idempotency_key with
               | Some key -> key
               | None -> generate_uuid ()
             in
             let headers = [
               ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
               ("Content-Type", "application/json");
               ("Idempotency-Key", idem_key);
             ] in
             Config.Http.post ~headers ~body url
               (fun response ->
                  if response.status >= 200 && response.status < 300 then
                    try
                      let json = Yojson.Basic.from_string response.body in
                      let open Yojson.Basic.Util in
                      let status_url =
                        try
                          match json |> member "url" with
                          | `Null -> raise Not_found
                          | url_json -> url_json |> to_string
                        with _ ->
                          let status_id = json |> member "id" |> to_string in
                          let username =
                            try json |> member "account" |> member "acct" |> to_string
                            with _ -> ""
                          in
                          if username <> "" then
                            Printf.sprintf "%s/@%s/%s" mastodon_creds.instance_url username status_id
                          else
                            Printf.sprintf "%s/statuses/%s" mastodon_creds.instance_url status_id
                      in
                      on_result (Error_types.Success status_url)
                    with e ->
                      on_result
                        (Error_types.Failure
                           (Error_types.Internal_error
                              (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e))))
                  else
                    on_result
                      (Error_types.Failure
                         (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body))
               )
               on_media_error
          )
           on_media_error)
        )
      (fun err -> on_result (Error_types.Failure err))
  
  (** Post thread with full options
      
      @param validate_media_before_upload When true, validates each media file after download.
             Default: false
  *)
  let post_thread 
      ~account_id 
      ~texts 
      ~media_urls_per_post
      ?(alt_texts_per_post=[])
      ?(visibility=Public)
      ?(sensitive=false)
      ?(spoiler_text=None)
      ?(validate_media_before_upload=false)
      on_result =
    (* Minimal pre-validation only; enforce instance-specific limits after resolving instance config. *)
    let media_counts = List.map List.length media_urls_per_post in
    match validate_thread ~texts ~media_counts ~max_length:max_int ~max_media:max_int () with
    | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
          resolve_instance_limits ~instance_url:mastodon_creds.instance_url (fun limits ->
          match validate_thread ~texts ~media_counts ~max_length:limits.max_status_chars ~max_media:limits.max_media_attachments () with
          | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
          | Ok () ->
          (* Helper to determine media type from MIME type *)
          let media_type_of_mime mime_type =
            if String.starts_with ~prefix:"video/" mime_type then Platform_types.Video
            else if mime_type = "image/gif" then Platform_types.Gif
            else Platform_types.Image
          in
          
          (* Helper to upload media from URLs for a single post *)
          let upload_post_media media_urls alt_texts on_complete on_err =
            (* Pair URLs with alt text *)
            let urls_with_alt = List.mapi (fun i url ->
              let alt_text = try List.nth alt_texts i with _ -> None in
              (url, alt_text)
            ) media_urls in
            
            let rec upload_seq urls_with_alt acc =
              match urls_with_alt with
              | [] -> on_complete (List.rev acc)
              | (url, alt_text) :: rest ->
                  Config.Http.get url
                    (fun media_resp ->
                      if media_resp.status >= 200 && media_resp.status < 300 then
                        let mime_type = 
                          List.assoc_opt "content-type" media_resp.headers 
                          |> Option.value ~default:"image/jpeg"
                        in
                        let file_size = String.length media_resp.body in
                        
                        (* Validate media if requested - inline validation for Mastodon *)
                        let validation_error =
                          if validate_media_before_upload then
                            let media_type = media_type_of_mime mime_type in
                            match media_type with
                            | Platform_types.Image when file_size > limits.max_image_size_bytes ->
                                Some (Error_types.Media_too_large { 
                                  size_bytes = file_size; 
                                  max_bytes = limits.max_image_size_bytes
                                })
                            | Platform_types.Video when file_size > limits.max_video_size_bytes ->
                                Some (Error_types.Media_too_large { 
                                  size_bytes = file_size; 
                                  max_bytes = limits.max_video_size_bytes
                                })
                            | Platform_types.Gif when file_size > limits.max_image_size_bytes ->
                                Some (Error_types.Media_too_large { 
                                  size_bytes = file_size; 
                                  max_bytes = limits.max_image_size_bytes
                                })
                            | _ -> None
                          else
                            None
                        in
                        
                        (match validation_error with
                        | Some err ->
                            on_result (Error_types.Failure (Error_types.Validation_error [err]))
                        | None ->
                            upload_media ~mastodon_creds 
                              ~media_data:media_resp.body ~mime_type ~description:alt_text ~focus:None
                              (fun media_id ->
                                if String.starts_with ~prefix:"video/" mime_type then
                                  wait_for_media_ready ~mastodon_creds ~media_id
                                    (fun () -> upload_seq rest (media_id :: acc))
                                    on_err
                                else
                                  upload_seq rest (media_id :: acc))
                              on_err)
                      else
                        on_err (Printf.sprintf "Failed to fetch media from %s" url))
                    on_err
            in
            let media_to_upload = List.filteri (fun i _ -> i < limits.max_media_attachments) urls_with_alt in
            upload_seq media_to_upload []
          in
          
          (* Pair texts with media and alt text - handle mismatched lengths *)
          let num_posts = List.length texts in
          let padded_media_urls = media_urls_per_post @ List.init (max 0 (num_posts - List.length media_urls_per_post)) (fun _ -> []) in
          let padded_alt_texts = alt_texts_per_post @ List.init (max 0 (num_posts - List.length alt_texts_per_post)) (fun _ -> []) in
          
          let posts_with_media_and_alt = List.mapi (fun i text ->
            let media_urls = try List.nth padded_media_urls i with _ -> [] in
            let alt_texts = try List.nth padded_alt_texts i with _ -> [] in
            (text, media_urls, alt_texts)
          ) texts in
          
          let total_requested = List.length texts in
          
          (* Helper to post statuses in sequence with reply references, tracking index *)
          let rec post_statuses_seq posts_with_media_and_alt post_index reply_to_id acc =
            match posts_with_media_and_alt with
            | [] -> 
                let thread_result = {
                  Error_types.posted_ids = List.rev acc;
                  failed_at_index = None;
                  total_requested;
                } in
                on_result (Error_types.Success thread_result)
            | (text, media_urls, alt_texts) :: rest ->
                let on_post_error err_msg =
                  let thread_result = {
                    Error_types.posted_ids = List.rev acc;
                    failed_at_index = Some post_index;
                    total_requested;
                  } in
                  if List.length acc > 0 then
                    (* Partial success - some posts were made *)
                    on_result (Error_types.Partial_success { 
                      result = thread_result; 
                      warnings = [Error_types.Generic_warning { code = "thread_incomplete"; message = err_msg; recoverable = false }]
                    })
                  else
                    (* Complete failure - no posts made *)
                    on_result (Error_types.Failure (Error_types.Api_error {
                      status_code = 0;
                      message = err_msg;
                      platform = Platform_types.Mastodon;
                      raw_response = None;
                      request_id = None;
                    }))
                in
                (* Upload media for this post *)
                upload_post_media media_urls alt_texts
                  (fun media_ids ->
                    let url = Printf.sprintf "%s/api/v1/statuses" mastodon_creds.instance_url in
                    
                    let base_fields = [
                      ("status", `String text);
                      ("visibility", `String (visibility_to_string visibility));
                      ("sensitive", `Bool sensitive);
                    ] in
                    
                    let fields = match spoiler_text with
                      | Some text when String.length text > 0 -> 
                          ("spoiler_text", `String text) :: base_fields
                      | _ -> base_fields
                    in
                    
                    let fields = match reply_to_id with
                      | Some id -> ("in_reply_to_id", `String id) :: fields
                      | None -> fields
                    in
                    
                    let fields = if List.length media_ids > 0 then
                      ("media_ids", `List (List.map (fun id -> `String id) media_ids)) :: fields
                    else
                      fields
                    in
                    
                    let body = Yojson.Basic.to_string (`Assoc fields) in
                    
                    let headers = [
                      ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
                      ("Content-Type", "application/json");
                      ("Idempotency-Key", generate_uuid ());
                    ] in
                    
                    Config.Http.post ~headers ~body url
                      (fun response ->
                        if response.status >= 200 && response.status < 300 then
                          try
                            let json = Yojson.Basic.from_string response.body in
                            let open Yojson.Basic.Util in
                            let status_id = json |> member "id" |> to_string in
                            (* Get the full URL for the status *)
                            let status_url = 
                              try
                                match json |> member "url" with
                                | `Null -> raise Not_found  (* URL is null, use fallback *)
                                | url_json -> url_json |> to_string
                              with _ ->
                                (* Fallback: construct URL from instance, username and status ID *)
                                let username = try
                                  json |> member "account" |> member "acct" |> to_string
                                with _ ->
                                  (* If we can't get username, fall back to /statuses format *)
                                  ""
                                in
                                if username <> "" then
                                  Printf.sprintf "%s/@%s/%s" mastodon_creds.instance_url username status_id
                                else
                                  Printf.sprintf "%s/statuses/%s" mastodon_creds.instance_url status_id
                            in
                            (* Continue with next status in thread, use ID for reply but accumulate URLs *)
                            post_statuses_seq rest (post_index + 1) (Some status_id) (status_url :: acc)
                          with e ->
                            on_post_error (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e))
                        else
                          on_post_error (Printf.sprintf "Mastodon API error (%d): %s" response.status response.body))
                      on_post_error)
                  on_post_error
          in
          
          post_statuses_seq posts_with_media_and_alt 0 None [])
          )
      (fun err -> on_result (Error_types.Failure err))
  
  (** Validate content for Mastodon 
      Note: The actual character limit may vary by instance. 
      Default is 500, but many instances use 1000, 5000, or more.
      Should ideally fetch from /api/v1/instance endpoint. *)
  let validate_content ~text ?(max_length=500) () =
    if String.length text > max_length then
      Error (Printf.sprintf "Status exceeds %d character limit" max_length)
    else
      Ok ()
  
  (** Validate media for Mastodon 
      Note: These are default limits. Actual limits may vary by instance
      and should ideally be fetched from /api/v1/instance endpoint. *)
  let validate_media ~(media : Platform_types.post_media) =
    match media.Platform_types.media_type with
    | Platform_types.Image ->
        if media.file_size_bytes > 10 * 1024 * 1024 then
          Error "Image exceeds 10MB limit (default)"
        else
          Ok ()
    | Platform_types.Video ->
        if media.file_size_bytes > 100 * 1024 * 1024 then
          Error "Video exceeds 100MB limit (default)"
        else if Option.value ~default:0.0 media.duration_seconds > 7200.0 then
          Error "Video exceeds 2 hour duration limit"
        else
          Ok ()
    | Platform_types.Gif ->
        if media.file_size_bytes > 10 * 1024 * 1024 then
          Error "GIF exceeds 10MB limit (default)"
        else
          Ok ()
  
  (** Validate poll options *)
  let validate_poll ~(poll : poll) =
    if List.length poll.options < 2 then
      Error "Poll must have at least 2 options"
    else if List.length poll.options > 4 then
      Error "Poll can have at most 4 options"
    else if List.exists (fun (opt : poll_option) -> String.length opt.title = 0) poll.options then
      Error "Poll options cannot be empty"
    else if List.exists (fun (opt : poll_option) -> String.length opt.title > 50) poll.options then
      Error "Poll option exceeds 50 character limit"
    else if poll.expires_in < 300 then
      Error "Poll must be open for at least 5 minutes"
    else if poll.expires_in > 2592000 then
      Error "Poll cannot be open for more than 30 days"
    else
      Ok ()

  (** Reply to an existing status

      Fetches the original status to determine its author, then posts a reply
      that automatically includes an @mention of the original author and sets
      in_reply_to_id for proper threading.

      @param account_id The account posting the reply
      @param status_id The ID of the status being replied to
      @param text The reply text (the @mention prefix is prepended automatically)
      @param media_urls Optional media URLs to attach
      @param alt_texts Optional alt-text descriptions for each media URL
      @param visibility Visibility level for the reply (default: Public)
      @param sensitive Whether the reply is sensitive (default: false)
      @param spoiler_text Optional content warning text
  *)
  let reply_to_status
      ~account_id
      ~status_id
      ~text
      ?(media_urls=[])
      ?(alt_texts=[])
      ?(visibility=Public)
      ?(sensitive=false)
      ?(spoiler_text=None)
      on_result =
    (* First, fetch the original status to get the author's acct *)
    get_status ~account_id ~status_id
      (fun status_result ->
        match status_result with
        | Error err -> on_result (Error_types.Failure err)
        | Ok original_status ->
          (* Build the reply text with @mention prefix *)
          let reply_text = match original_status.account_acct with
            | Some acct when acct <> "" ->
              let mention = Printf.sprintf "@%s " acct in
              if String.starts_with ~prefix:mention text
                || String.starts_with ~prefix:(Printf.sprintf "@%s" acct) text then
                text  (* Already mentions the author *)
              else
                Printf.sprintf "@%s %s" acct text
            | _ -> text  (* No account info, post as-is *)
          in
          (* Post as a reply using post_single with in_reply_to_id *)
          post_single
            ~account_id
            ~text:reply_text
            ~media_urls
            ~alt_texts
            ~visibility
            ~sensitive
            ~spoiler_text
            ~in_reply_to_id:(Some status_id)
            on_result)

  (** Delete a status *)
  let delete_status ~account_id ~status_id on_success on_error =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/statuses/%s" mastodon_creds.instance_url status_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        
        Config.Http.delete ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_success ()
            else
              on_error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body))
          (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err))))
      (fun err -> on_error err)
  
  (** Edit a status *)
  let edit_status
      ~account_id
      ~status_id
      ~text
      ?(media_ids=None)
      ?(visibility=None)
      ?(sensitive=None)
      ?(spoiler_text=None)
      ?(language=None)
      ?(poll=None)
      on_success
      on_error =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/statuses/%s" mastodon_creds.instance_url status_id in
        
        let base_fields = [("status", `String text)] in
        
        let fields = match visibility with
          | Some v -> ("visibility", `String (visibility_to_string v)) :: base_fields
          | None -> base_fields
        in
        
        let fields = match sensitive with
          | Some s -> ("sensitive", `Bool s) :: fields
          | None -> fields
        in
        
        let fields = match spoiler_text with
          | Some text when String.length text > 0 -> 
              ("spoiler_text", `String text) :: fields
          | _ -> fields
        in
        
        let fields = match language with
          | Some lang -> ("language", `String lang) :: fields
          | None -> fields
        in
        
        let fields = match media_ids with
          | Some ids when List.length ids > 0 ->
              ("media_ids", `List (List.map (fun id -> `String id) ids)) :: fields
          | _ -> fields
        in
        
        let fields = match poll with
          | Some (p : poll) ->
              let poll_json = `Assoc [
                ("options", `List (List.map (fun (opt : poll_option) -> `String opt.title) p.options));
                ("expires_in", `Int p.expires_in);
                ("multiple", `Bool p.multiple);
                ("hide_totals", `Bool p.hide_totals);
              ] in
              ("poll", poll_json) :: fields
          | None -> fields
        in
        
        let body = Yojson.Basic.to_string (`Assoc fields) in
        
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
          ("Content-Type", "application/json");
        ] in
        
        Config.Http.put ~headers ~body url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let edited_id = json |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_string in
                on_success edited_id
              with e ->
                on_error (Error_types.Internal_error (Printf.sprintf "Failed to parse edit response: %s" (Printexc.to_string e)))
            else
              on_error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body))
          (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err))))
      (fun err -> on_error err)
  
  (** Favorite a status *)
  let favorite_status ~account_id ~status_id on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/statuses/%s/favourite" mastodon_creds.instance_url status_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        
        Config.Http.post ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_result (Ok ())
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body))
          )
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))
  
  (** Unfavorite a status *)
  let unfavorite_status ~account_id ~status_id on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/statuses/%s/unfavourite" mastodon_creds.instance_url status_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        
        Config.Http.post ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_result (Ok ())
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body))
          )
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))
  
  (** Boost (reblog) a status *)
  let boost_status ~account_id ~status_id ?(visibility=None) on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/statuses/%s/reblog" mastodon_creds.instance_url status_id in
        
        let body = match visibility with
          | Some v -> 
              Yojson.Basic.to_string (`Assoc [("visibility", `String (visibility_to_string v))])
          | None -> "{}"
        in
        
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
          ("Content-Type", "application/json");
        ] in
        
        Config.Http.post ~headers ~body url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_result (Ok ())
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body))
          )
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))
  
  (** Unboost (unreblog) a status *)
  let unboost_status ~account_id ~status_id on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/statuses/%s/unreblog" mastodon_creds.instance_url status_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        
        Config.Http.post ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_result (Ok ())
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body))
          )
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))
  
  (** Bookmark a status *)
  let bookmark_status ~account_id ~status_id on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/statuses/%s/bookmark" mastodon_creds.instance_url status_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        
        Config.Http.post ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_result (Ok ())
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body))
          )
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))
  
  (** Unbookmark a status *)
  let unbookmark_status ~account_id ~status_id on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/statuses/%s/unbookmark" mastodon_creds.instance_url status_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        
        Config.Http.post ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_result (Ok ())
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body))
          )
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  (** Follow an account *)
  let follow_account
      ~account_id
      ~target_account_id
      ?(reblogs=true)
      ?(notify=false)
      ?(languages=[])
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/accounts/%s/follow" mastodon_creds.instance_url target_account_id in
        let body_json = `Assoc [
          ("reblogs", `Bool reblogs);
          ("notify", `Bool notify);
          ("languages", `List (List.map (fun l -> `String l) languages));
        ] in
        let body = Yojson.Basic.to_string body_json in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
          ("Content-Type", "application/json");
        ] in
        Config.Http.post ~headers ~body url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok (parse_relationship_summary json))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse follow response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  (** Unfollow an account *)
  let unfollow_account
      ~account_id
      ~target_account_id
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/accounts/%s/unfollow" mastodon_creds.instance_url target_account_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.post ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok (parse_relationship_summary json))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse unfollow response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  (** Block an account *)
  let block_account
      ~account_id
      ~target_account_id
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/accounts/%s/block" mastodon_creds.instance_url target_account_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.post ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok (parse_relationship_summary json))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse block response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  (** Unblock an account *)
  let unblock_account
      ~account_id
      ~target_account_id
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/accounts/%s/unblock" mastodon_creds.instance_url target_account_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.post ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok (parse_relationship_summary json))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse unblock response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  (** Mute an account *)
  let mute_account
      ~account_id
      ~target_account_id
      ?(notifications=true)
      ?(duration=None)
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/accounts/%s/mute" mastodon_creds.instance_url target_account_id in
        let body_fields =
          let base = [ ("notifications", `Bool notifications) ] in
          match duration with
          | Some days -> ("duration", `Int days) :: base
          | None -> base
        in
        let body = Yojson.Basic.to_string (`Assoc body_fields) in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
          ("Content-Type", "application/json");
        ] in
        Config.Http.post ~headers ~body url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok (parse_relationship_summary json))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse mute response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  (** Unmute an account *)
  let unmute_account
      ~account_id
      ~target_account_id
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/accounts/%s/unmute" mastodon_creds.instance_url target_account_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.post ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok (parse_relationship_summary json))
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse unmute response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  (** Dismiss a single notification *)
  let dismiss_notification
      ~account_id
      ~notification_id
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/notifications/%s/dismiss" mastodon_creds.instance_url notification_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.post ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_result (Ok ())
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  (** Clear all notifications *)
  let clear_notifications
      ~account_id
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/notifications/clear" mastodon_creds.instance_url in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.post ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_result (Ok ())
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  (** Get pending follow requests *)
  let get_follow_requests
      ~account_id
      ?(limit=40)
      ?(max_id=None)
      ?(since_id=None)
      ?(min_id=None)
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let base_url = Printf.sprintf "%s/api/v1/follow_requests" mastodon_creds.instance_url in
        let uri = Uri.of_string base_url in
        let query =
          let base = [ ("limit", string_of_int limit) ] in
          let with_max = match max_id with Some v -> ("max_id", v) :: base | None -> base in
          let with_since = match since_id with Some v -> ("since_id", v) :: with_max | None -> with_max in
          match min_id with Some v -> ("min_id", v) :: with_since | None -> with_since
        in
        let url = Uri.with_query' uri query |> Uri.to_string in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let accounts =
                  match json with
                  | `List items -> List.map parse_account_summary items
                  | `Null -> []
                  | _ -> []
                in
                on_result (Ok accounts)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse follow requests response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  (** Authorize a follow request *)
  let authorize_follow_request
      ~account_id
      ~target_account_id
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/follow_requests/%s/authorize" mastodon_creds.instance_url target_account_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.post ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_result (Ok ())
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  (** Reject a follow request *)
  let reject_follow_request
      ~account_id
      ~target_account_id
      on_result =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/follow_requests/%s/reject" mastodon_creds.instance_url target_account_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        Config.Http.post ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_result (Ok ())
            else
              on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))
  
  (** Register an application with a Mastodon instance *)
  let register_app ~instance_url ~client_name ~redirect_uris ~scopes ~website on_result =
    let url = Printf.sprintf "%s/api/v1/apps" instance_url in
    
    let body_json = `Assoc [
      ("client_name", `String client_name);
      ("redirect_uris", `String redirect_uris);
      ("scopes", `String scopes);
      ("website", `String website);
    ] in
    let body = Yojson.Basic.to_string body_json in
    
    let headers = [("Content-Type", "application/json")] in
    
    Config.Http.post ~headers ~body url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            let open Yojson.Basic.Util in
            let client_id = json |> member "client_id" |> to_string in
            let client_secret = json |> member "client_secret" |> to_string in
            on_result (Ok (client_id, client_secret))
          with e ->
            on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse app registration: %s" (Printexc.to_string e))))
        else
          on_result (Error (parse_api_error ~headers:response.headers ~status_code:response.status ~response_body:response.body))
      )
      (fun err -> on_result (Error (Error_types.Internal_error err)))
  
  (** Generate PKCE code verifier (43-128 characters)
      
      @deprecated Use OAuth.Pkce.generate_code_verifier instead
  *)
  let generate_code_verifier = OAuth.Pkce.generate_code_verifier
  
  (** Generate PKCE code challenge from verifier using SHA256
      
      @deprecated Use OAuth.Pkce.generate_code_challenge instead
  *)
  let generate_code_challenge = OAuth.Pkce.generate_code_challenge
  
  (** Get OAuth authorization URL with PKCE support *)
  let get_oauth_url ~instance_url ~client_id ~redirect_uri ~scopes ?(state=None) ?(code_challenge=None) () =
    let scope_list =
      scopes
      |> String.split_on_char ' '
      |> List.filter (fun s -> String.length s > 0)
    in
    OAuth.get_authorization_url
      ~instance_url
      ~client_id
      ~redirect_uri
      ~scopes:scope_list
      ?state
      ?code_challenge
      ()

  (** Verify OAuth callback state to prevent CSRF *)
  let verify_oauth_state ~expected_state ~returned_state =
    String.equal expected_state returned_state
  
  (** Exchange authorization code for access token with optional PKCE verifier *)
  let exchange_code ~instance_url ~client_id ~client_secret ~redirect_uri ~code ?(code_verifier=None) on_result =
    OAuth_http.exchange_code_with_scope
      ~instance_url
      ~client_id
      ~client_secret
      ~redirect_uri
      ~code
      ?code_verifier
      (fun (core_creds, granted_scope_opt) ->
        let requested_scopes = ["follow"; "read"; "write"] in
        let granted_scopes_opt =
          match granted_scope_opt with
          | Some granted_scope ->
              let scopes =
                granted_scope
                |> String.split_on_char ' '
                |> List.filter (fun s -> s <> "")
              in
              if scopes = [] then None else Some scopes
          | None -> None
        in
        (match granted_scopes_opt with
         | None -> on_result (Ok core_creds)
         | Some granted_scopes ->
             let missing = List.filter (fun req -> not (List.mem req granted_scopes)) requested_scopes in
             if missing <> [] then
               on_result (Error (Error_types.Auth_error (Error_types.Insufficient_permissions missing)))
             else
               on_result (Ok core_creds)))
      (fun err ->
        if String.starts_with ~prefix:"Token exchange failed (" err then
          try
            let lparen = String.index err '(' in
            let rparen = String.index err ')' in
            let status_str = String.sub err (lparen + 1) (rparen - lparen - 1) in
            let status_code = int_of_string status_str in
            let body_start = rparen + 3 in
            let body_len = max 0 (String.length err - body_start) in
            let response_body = String.sub err body_start body_len in
            on_result (Error (parse_api_error ~headers:[] ~status_code ~response_body))
          with _ ->
            on_result (Error (Error_types.Internal_error err))
        else
          on_result (Error (Error_types.Internal_error err)))
  
  (** Revoke access token on logout/disconnect *)
  let revoke_token ~account_id on_result =
    Config.get_credentials ~account_id
      (fun creds ->
        parse_mastodon_credentials creds
          (fun mastodon_creds ->
            let url = Printf.sprintf "%s/oauth/revoke" mastodon_creds.instance_url in
            let body_json = `Assoc [
              ("token", `String mastodon_creds.access_token);
            ] in
            let body = Yojson.Basic.to_string body_json in
            let headers = [("Content-Type", "application/json")] in
            
            Config.Http.post ~headers ~body url
              (fun response ->
                (* OAuth2 revocation endpoint returns 200 for both valid and invalid tokens
                   This is by design - it's idempotent and safe to call multiple times *)
                if response.status >= 200 && response.status < 300 then
                  on_result (Ok ())
                else
                  (* Don't fail - token might already be revoked or invalid *)
                  on_result (Ok ())  (* Still consider it a success since the goal is achieved *)
              )
              (fun _err ->
                (* Network errors should also not fail the disconnect flow *)
                on_result (Ok ())  (* Continue with disconnect even if revocation failed *)
              ))
          (fun err -> on_result (Error (Error_types.Auth_error (Error_types.Refresh_failed err)))))
      (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err))))
end
