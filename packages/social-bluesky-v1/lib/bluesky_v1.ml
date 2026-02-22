(** Bluesky AT Protocol v1 Provider

    This implementation uses app password authentication and creates sessions
    for each API call.
*)

open Social_core

(** Authentication module for Bluesky

    IMPORTANT: this provider implementation does not use OAuth. It uses app
    passwords with the AT Protocol's session management.
    
    Authentication flow:
    1. User creates an app password at https://bsky.app/settings/app-passwords
    2. Application calls create_session with identifier (handle/DID) and app password
    3. Session returns access_jwt and refresh_jwt
    4. Access JWT expires but can be refreshed with refresh_jwt
    
    App passwords do not expire and can be revoked by the user at any time.
*)
module Auth = struct
  (** Platform metadata for Bluesky authentication *)
  module Metadata = struct
    (** This provider implementation does not use OAuth *)
    let uses_oauth = false
    
    (** Bluesky uses app passwords for authentication *)
    let uses_app_password = true
    
    (** App passwords do not expire (until user revokes them) *)
    let app_password_expires = false
    
    (** Access JWTs expire after a short period (typically minutes) *)
    let access_jwt_lifetime_seconds = Some 300  (* ~5 minutes *)
    
    (** Refresh JWTs can be used to get new access JWTs *)
    let supports_session_refresh = true
    
    (** Default PDS URL (Personal Data Server) *)
    let default_pds_url = "https://bsky.social"
    
    (** Session creation endpoint *)
    let create_session_path = "/xrpc/com.atproto.server.createSession"
    
    (** Session refresh endpoint *)
    let refresh_session_path = "/xrpc/com.atproto.server.refreshSession"
    
    (** Session deletion endpoint *)
    let delete_session_path = "/xrpc/com.atproto.server.deleteSession"
  end
  
  (** Session information returned by Bluesky *)
  type session = {
    did: string;           (** User's DID (decentralized identifier) *)
    handle: string;        (** User's handle (e.g., user.bsky.social) *)
    access_jwt: string;    (** JWT for API authentication *)
    refresh_jwt: string;   (** JWT for session refresh *)
  }
  
  (** Make functor for Auth operations that need HTTP client *)
  module Make (Http : HTTP_CLIENT) = struct
    (** Create a new session using app password
        
        @param pds_url Optional PDS URL (defaults to https://bsky.social)
        @param identifier User's handle (e.g., user.bsky.social) or DID
        @param app_password App password from https://bsky.app/settings/app-passwords
        @param on_result Continuation receiving the outcome
    *)
    let create_session ?(pds_url=Metadata.default_pds_url) ~identifier ~app_password on_result =
      let url = Printf.sprintf "%s%s" pds_url Metadata.create_session_path in
      let body = Yojson.Basic.to_string (`Assoc [
        ("identifier", `String identifier);
        ("password", `String app_password);
      ]) in
      let headers = [("Content-Type", "application/json")] in
      
      Http.post ~headers ~body url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let session = {
                did = json |> member "did" |> to_string;
                handle = json |> member "handle" |> to_string;
                access_jwt = json |> member "accessJwt" |> to_string;
                refresh_jwt = json |> member "refreshJwt" |> to_string;
              } in
              on_result (Error_types.Success session)
            with e ->
              on_result (Error_types.Failure (Error_types.Internal_error 
                (Printf.sprintf "Failed to parse session: %s" (Printexc.to_string e))))
          else if response.status = 401 then
            on_result (Error_types.Failure (Error_types.Auth_error Error_types.Token_invalid))
          else if response.status = 429 then
            on_result (Error_types.Failure (Error_types.make_rate_limited ()))
          else
            on_result (Error_types.Failure (Error_types.make_api_error 
              ~platform:Platform_types.Bluesky
              ~status_code:response.status
              ~message:"Session creation failed"
              ~raw_response:response.body ())))
        (fun err -> on_result (Error_types.Failure (Error_types.Network_error 
          (Error_types.Connection_failed err))))
    
    (** Refresh an existing session
        
        @param pds_url Optional PDS URL (defaults to https://bsky.social)
        @param refresh_jwt The refresh JWT from a previous session
        @param on_result Continuation receiving the outcome
    *)
    let refresh_session ?(pds_url=Metadata.default_pds_url) ~refresh_jwt on_result =
      let url = Printf.sprintf "%s%s" pds_url Metadata.refresh_session_path in
      let headers = [
        ("Authorization", Printf.sprintf "Bearer %s" refresh_jwt);
      ] in
      
      Http.post ~headers url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let session = {
                did = json |> member "did" |> to_string;
                handle = json |> member "handle" |> to_string;
                access_jwt = json |> member "accessJwt" |> to_string;
                refresh_jwt = json |> member "refreshJwt" |> to_string;
              } in
              on_result (Error_types.Success session)
            with e ->
              on_result (Error_types.Failure (Error_types.Internal_error 
                (Printf.sprintf "Failed to parse session: %s" (Printexc.to_string e))))
          else if response.status = 401 then
            on_result (Error_types.Failure (Error_types.Auth_error Error_types.Token_expired))
          else
            on_result (Error_types.Failure (Error_types.make_api_error
              ~platform:Platform_types.Bluesky
              ~status_code:response.status
              ~message:"Session refresh failed"
              ~raw_response:response.body ())))
        (fun err -> on_result (Error_types.Failure (Error_types.Network_error 
          (Error_types.Connection_failed err))))
    
    (** Delete/invalidate a session
        
        @param pds_url Optional PDS URL (defaults to https://bsky.social)
        @param refresh_jwt The refresh JWT of the session to delete
        @param on_result Continuation receiving the outcome
    *)
    let delete_session ?(pds_url=Metadata.default_pds_url) ~refresh_jwt on_result =
      let url = Printf.sprintf "%s%s" pds_url Metadata.delete_session_path in
      let headers = [
        ("Authorization", Printf.sprintf "Bearer %s" refresh_jwt);
      ] in
      
      Http.post ~headers url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            on_result (Error_types.Success ())
          else
            on_result (Error_types.Failure (Error_types.make_api_error
              ~platform:Platform_types.Bluesky
              ~status_code:response.status
              ~message:"Session deletion failed"
              ~raw_response:response.body ())))
        (fun err -> on_result (Error_types.Failure (Error_types.Network_error 
          (Error_types.Connection_failed err))))
    
    (** Convert session to Social_core.credentials for storage
        
        Note: This stores the identifier in access_token and app_password in refresh_token
        for compatibility with the SDK's credential storage model.
    *)
    let session_to_credentials ~identifier ~app_password : credentials =
      {
        access_token = identifier;
        refresh_token = Some app_password;
        expires_at = None;  (* App passwords don't expire *)
        token_type = "AppPassword";
      }
  end
end

(** Configuration module type for Bluesky provider *)
module type CONFIG = sig
  module Http : HTTP_CLIENT
  
  val get_env : string -> string option
  val get_credentials : account_id:string -> (credentials -> unit) -> (string -> unit) -> unit
  val update_credentials : account_id:string -> credentials:credentials -> (unit -> unit) -> (string -> unit) -> unit
  val encrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val decrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val update_health_status : account_id:string -> status:string -> error_message:string option -> (unit -> unit) -> (string -> unit) -> unit
end

(** Make functor to create Bluesky provider with given configuration *)
module Make (Config : CONFIG) = struct
  let pds_url = "https://bsky.social"
  
  (** Platform-specific constants *)
  let max_text_length = 300
  let max_images = 4
  let max_image_size_bytes = 1024 * 1024  (* 1MB *)
  let max_video_size_bytes = 50 * 1024 * 1024  (* 50MB *)
  let max_video_duration_seconds = 60
  
  (** {1 Validation} *)
  
  (** Validate a single post's content *)
  let validate_post ~text ?(media_count=0) () =
    let errors = ref [] in
    let text_len = String.length text in
    if text_len > max_text_length then
      errors := Error_types.Text_too_long { length = text_len; max = max_text_length } :: !errors;
    if media_count > max_images then
      errors := Error_types.Too_many_media { count = media_count; max = max_images } :: !errors;
    if !errors = [] then Ok ()
    else Error (List.rev !errors)
  
  (** Validate a thread before posting *)
  let validate_thread ~texts ?(media_counts=[]) () =
    if texts = [] then
      Error [Error_types.Thread_empty]
    else
      let errors = List.mapi (fun i text ->
        let media_count = try List.nth media_counts i with _ -> 0 in
        match validate_post ~text ~media_count () with
        | Ok () -> None
        | Error errs -> Some (Error_types.Thread_post_invalid { index = i; errors = errs })
      ) texts |> List.filter_map Fun.id in
      if errors = [] then Ok ()
      else Error errors
  
  (** Validate media constraints *)
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
        let errors = ref [] in
        if media.file_size_bytes > max_video_size_bytes then
          errors := Error_types.Media_too_large { 
            size_bytes = media.file_size_bytes; 
            max_bytes = max_video_size_bytes 
          } :: !errors;
        (match media.duration_seconds with
         | Some duration when duration > float_of_int max_video_duration_seconds ->
             errors := Error_types.Video_too_long { 
               duration_seconds = duration; 
               max_seconds = max_video_duration_seconds 
             } :: !errors
         | _ -> ());
        if !errors = [] then Ok ()
        else Error (List.rev !errors)
    | Platform_types.Gif ->
        if media.file_size_bytes > max_image_size_bytes then
          Error [Error_types.Media_too_large { 
            size_bytes = media.file_size_bytes; 
            max_bytes = max_image_size_bytes 
          }]
        else
          Ok ()
  
  (** Legacy validation function for backwards compatibility *)
  let validate_content ~text =
    match validate_post ~text () with
    | Ok () -> Ok ()
    | Error errs -> Error (String.concat "; " (List.map Error_types.validation_error_to_string errs))
  
  (** Session cache: maps account_id to (did, access_jwt, created_at_unix) *)
  let session_cache : (string, string * string * float) Hashtbl.t = Hashtbl.create 16

  (** Session lifetime in seconds (matches Auth.Metadata.access_jwt_lifetime_seconds minus margin) *)
  let session_max_age_seconds = 240.0  (* 4 minutes, conservative vs 5-min expiry *)

  (** Clear cached session for an account *)
  let invalidate_session ~account_id =
    Hashtbl.remove session_cache account_id

  (** {1 Internal Helpers} *)
  
  (** Parse API error from response *)
  let parse_api_error ~status_code ~body =
    let parse_error_message () =
      try
        let json = Yojson.Basic.from_string body in
        let open Yojson.Basic.Util in
        let msg = json |> member "message" in
        if msg <> `Null then to_string msg
        else
          let err = json |> member "error" in
          if err <> `Null then to_string err
          else "API error"
      with _ -> "API error"
    in
    if status_code = 401 then
      Error_types.Auth_error Error_types.Token_invalid
    else if status_code = 429 then
      Error_types.make_rate_limited ()
    else
      Error_types.make_api_error
        ~platform:Platform_types.Bluesky
        ~status_code
        ~message:(parse_error_message ())
        ~raw_response:body ()

  let parse_json_object_or_error ~context ~body =
    try
      let json = Yojson.Basic.from_string body in
      match json with
      | `Assoc _ -> Ok json
      | _ -> Error (Error_types.Internal_error (Printf.sprintf "%s: expected JSON object" context))
    with e ->
      Error (Error_types.Internal_error
        (Printf.sprintf "%s: %s" context (Printexc.to_string e)))

  let ensure_array_field ~json ~field ~context =
    try
      let _ = json |> Yojson.Basic.Util.member field |> Yojson.Basic.Util.to_list in
      Ok ()
    with e ->
      Error (Error_types.Internal_error
        (Printf.sprintf "%s: %s" context (Printexc.to_string e)))
  
  (** Resolve handle to DID *)
  let resolve_handle ~handle on_result =
    let url = Printf.sprintf "%s/xrpc/com.atproto.identity.resolveHandle?handle=%s" 
      pds_url (Uri.pct_encode handle) in
    Config.Http.get url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            let did = json |> Yojson.Basic.Util.member "did" |> Yojson.Basic.Util.to_string in
            on_result (Ok did)
          with e ->
            on_result (Error (Error_types.Internal_error 
              (Printf.sprintf "Failed to parse DID: %s" (Printexc.to_string e))))
        else
          on_result (Error (parse_api_error ~status_code:response.status ~body:response.body)))
      (fun err -> on_result (Error (Error_types.Network_error 
        (Error_types.Connection_failed err))))
  
  (** Extract facets from text (URLs, mentions, hashtags) *)
  let extract_facets text on_success on_error =
    (* URL pattern *)
    let url_pattern = Re.Pcre.regexp 
      "https?://[a-zA-Z0-9][-a-zA-Z0-9@:%._\\+~#=]{0,256}\\.[a-zA-Z0-9()]{1,6}\\b[-a-zA-Z0-9()@:%_\\+.~#?&/=]*"
    in
    
    (* Mention pattern: @handle.bsky.social or @username.com *)
    let mention_pattern = Re.Pcre.regexp "@([a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?\\.)+[a-zA-Z]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?" in
    
    (* Hashtag pattern: #hashtag *)
    let hashtag_pattern = Re.Pcre.regexp "#[a-zA-Z0-9_]+" in
    
    (* Extract all URL facets *)
    let url_matches = Re.all url_pattern text in
    let url_facets = List.map (fun group ->
      let url = Re.Group.get group 0 in
      let start_pos = Re.Group.start group 0 in
      let end_pos = Re.Group.stop group 0 in
      let prefix = String.sub text 0 start_pos in
      let matched = String.sub text start_pos (end_pos - start_pos) in
      let byte_start = String.length prefix in
      let byte_end = byte_start + String.length matched in
      (byte_start, byte_end, `Assoc [
        ("$type", `String "app.bsky.richtext.facet#link");
        ("uri", `String url);
      ])
    ) url_matches in
    
    (* Extract all hashtag facets *)
    let hashtag_matches = Re.all hashtag_pattern text in
    let hashtag_facets = List.map (fun group ->
      let tag = Re.Group.get group 0 in
      let start_pos = Re.Group.start group 0 in
      let end_pos = Re.Group.stop group 0 in
      let prefix = String.sub text 0 start_pos in
      let matched = String.sub text start_pos (end_pos - start_pos) in
      let byte_start = String.length prefix in
      let byte_end = byte_start + String.length matched in
      (* Remove the # prefix for the tag value *)
      let tag_value = String.sub tag 1 (String.length tag - 1) in
      (byte_start, byte_end, `Assoc [
        ("$type", `String "app.bsky.richtext.facet#tag");
        ("tag", `String tag_value);
      ])
    ) hashtag_matches in
    
    (* Extract mention handles *)
    let mention_matches = Re.all mention_pattern text in
    let mention_handles = List.map (fun group ->
      let mention = Re.Group.get group 0 in
      let start_pos = Re.Group.start group 0 in
      let end_pos = Re.Group.stop group 0 in
      let prefix = String.sub text 0 start_pos in
      let matched = String.sub text start_pos (end_pos - start_pos) in
      let byte_start = String.length prefix in
      let byte_end = byte_start + String.length matched in
      (* Remove @ prefix *)
      let handle = String.sub mention 1 (String.length mention - 1) in
      (byte_start, byte_end, handle)
    ) mention_matches in
    
    (* Resolve DIDs for all mentions *)
    let rec resolve_mentions mentions acc on_complete on_err =
      match mentions with
      | [] -> on_complete (List.rev acc)
      | (byte_start, byte_end, handle) :: rest ->
          resolve_handle ~handle (function
            | Ok did ->
                let facet = (byte_start, byte_end, `Assoc [
                  ("$type", `String "app.bsky.richtext.facet#mention");
                  ("did", `String did);
                ]) in
                resolve_mentions rest (facet :: acc) on_complete on_err
            | Error _ ->
                (* Skip mentions that fail to resolve *)
                resolve_mentions rest acc on_complete on_err)
    in
    
    resolve_mentions mention_handles []
      (fun mention_facets ->
        (* Combine all facets and format them *)
        let all_facets = url_facets @ hashtag_facets @ mention_facets in
        let formatted_facets = List.map (fun (byte_start, byte_end, feature) ->
          `Assoc [
            ("index", `Assoc [
              ("byteStart", `Int byte_start);
              ("byteEnd", `Int byte_end);
            ]);
            ("features", `List [feature]);
          ]
        ) all_facets in
        on_success formatted_facets)
      on_error
  
  (** Create session with Bluesky using app password *)
  let create_session ~identifier ~password on_success on_error =
    let url = Printf.sprintf "%s/xrpc/com.atproto.server.createSession" pds_url in
    let body = `Assoc [
      ("identifier", `String identifier);
      ("password", `String password);
    ] in
    let body_str = Yojson.Basic.to_string body in
    let headers = [("Content-Type", "application/json")] in
    
    Config.Http.post ~headers ~body:body_str url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            let open Yojson.Basic.Util in
            let did = json |> member "did" |> to_string in
            let access_jwt = json |> member "accessJwt" |> to_string in
            on_success (did, access_jwt)
          with e ->
            on_error (Printf.sprintf "Failed to parse session: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "Session creation failed (%d): %s" response.status response.body))
      on_error
  
  (** Video upload host *)
  let video_upload_host = "https://video.bsky.app"

  (** Get a service auth token for video upload

      Calls com.atproto.server.getServiceAuth with the user's PDS DID
      as the audience and the uploadBlob lexicon method.

      Per the AT Protocol spec, the aud parameter should be the DID of the
      user's PDS (e.g. did:web:bsky.social), NOT the video service DID.
      The video service uses the resulting token to upload the processed
      blob to the user's PDS on their behalf.

      @param access_jwt Current session JWT for authenticating the request
      @param did The user's DID (unused; the PDS DID is derived from pds_url)
      @param on_success Receives the service auth token
      @param on_error Receives error message
  *)
  let get_service_auth ~access_jwt ~did:_ on_success on_error =
    (* aud = PDS DID derived from the configured PDS URL host *)
    let pds_host =
      match Uri.host (Uri.of_string pds_url) with
      | Some h -> h
      | None -> "bsky.social"
    in
    let aud = "did:web:" ^ pds_host in
    let lxm = "com.atproto.repo.uploadBlob" in
    let exp_seconds = int_of_float (Unix.gettimeofday () +. 1800.0) in (* 30 minutes *)
    let url = Printf.sprintf "%s/xrpc/com.atproto.server.getServiceAuth?aud=%s&lxm=%s&exp=%d"
      pds_url (Uri.pct_encode aud) (Uri.pct_encode lxm) exp_seconds in
    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
    ] in
    Config.Http.get ~headers url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            let token = json |> Yojson.Basic.Util.member "token" |> Yojson.Basic.Util.to_string in
            on_success token
          with e ->
            on_error (Printf.sprintf "Failed to parse service auth response: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "Service auth request failed (%d): %s" response.status response.body))
      on_error

  (** Upload blob to Bluesky with optional alt text

      @param access_jwt Session JWT for authentication
      @param did Optional DID for video uploads (used to get service auth token)
      @param blob_data Raw binary data
      @param mime_type MIME type of the data
      @param alt_text Optional alt text for accessibility
      @param on_success Receives (blob_json, alt_text)
      @param on_error Receives error message
  *)
  let upload_blob ~access_jwt ?did ~blob_data ~mime_type ~alt_text on_success on_error =
    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
      ("Content-Type", mime_type);
    ] in
    let is_video = String.starts_with ~prefix:"video/" mime_type in
    let non_null json key =
      let open Yojson.Basic.Util in
      let v = json |> member key in
      if v = `Null then None else Some v
    in
    let parse_blob_from_json json =
      match non_null json "blob" with
      | Some blob -> Some blob
      | None ->
          (match non_null json "jobStatus" with
           | Some job_status -> non_null job_status "blob"
           | None -> None)
    in
    let parse_string_field json key =
      let open Yojson.Basic.Util in
      match non_null json key with
      | Some v ->
          (try Some (to_string v) with _ -> None)
      | None -> None
    in
    let parse_job_id json =
      match parse_string_field json "jobId" with
      | Some id -> Some id
      | None ->
          (match non_null json "jobStatus" with
           | Some job_status -> parse_string_field job_status "jobId"
           | None -> None)
    in
    let parse_job_state json =
      match parse_string_field json "state" with
      | Some state -> Some state
      | None ->
          (match non_null json "jobStatus" with
           | Some job_status -> parse_string_field job_status "state"
           | None -> None)
    in
    let upload_via_blob () =
      let url = Printf.sprintf "%s/xrpc/com.atproto.repo.uploadBlob" pds_url in
      Config.Http.post ~headers ~body:blob_data url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              match parse_blob_from_json json with
              | Some blob -> on_success (blob, alt_text)
              | None -> on_error "Blob upload response missing blob field"
            with e ->
              on_error (Printf.sprintf "Failed to parse blob response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "Bluesky blob upload error (%d): %s" response.status response.body))
        on_error
    in
    let rec poll_video_job job_id attempts_left =
      if attempts_left <= 0 then
        on_error (Printf.sprintf "Video processing timeout waiting for job %s" job_id)
      else
        (* Poll the video service directly for job status, matching the host
           used for uploadVideo. The PDS can proxy this but direct is preferred. *)
        let status_url = Printf.sprintf "%s/xrpc/app.bsky.video.getJobStatus?jobId=%s"
          video_upload_host (Uri.pct_encode job_id) in
        let should_retry_status status_code =
          status_code = 429 || status_code >= 500
        in
        Config.Http.get ~headers:[("Authorization", Printf.sprintf "Bearer %s" access_jwt)] status_url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                match parse_blob_from_json json with
                | Some blob -> on_success (blob, alt_text)
                | None ->
                    let state = parse_job_state json |> Option.value ~default:"UNKNOWN" in
                    let upper = String.uppercase_ascii state in
                    let rec contains token i =
                      let token_len = String.length token in
                      if i + token_len > String.length upper then false
                      else if String.sub upper i token_len = token then true
                      else contains token (i + 1)
                    in
                    if contains "FAIL" 0 || contains "ERROR" 0 || contains "CANCEL" 0 then
                      on_error (Printf.sprintf "Video processing failed (state=%s)" state)
                    else if contains "COMPLETE" 0 || contains "DONE" 0 then
                      on_error (Printf.sprintf "Video processing completed without blob (state=%s)" state)
                    else
                      poll_video_job job_id (attempts_left - 1)
              with e ->
                on_error (Printf.sprintf "Failed to parse video job status: %s" (Printexc.to_string e))
            else
              if should_retry_status response.status then
                poll_video_job job_id (attempts_left - 1)
              else
                on_error (Printf.sprintf "Video job status failed (%d): %s" response.status response.body))
          (fun _ -> poll_video_job job_id (attempts_left - 1))
    in
    let should_fallback_from_video_endpoint status_code response_body =
      let upper_body = String.uppercase_ascii response_body in
      let contains token =
        let token_len = String.length token in
        let rec loop i =
          if i + token_len > String.length upper_body then false
          else if String.sub upper_body i token_len = token then true
          else loop (i + 1)
        in
        loop 0
      in
      let method_unavailable_error =
        status_code = 400
        && (contains "METHODNOTFOUND"
            || contains "METHOD_NOT_FOUND"
            || contains "METHOD NOT FOUND"
            || contains "UNKNOWNMETHOD"
            || contains "NOTIMPLEMENTED")
      in
      status_code = 404
      || status_code = 405
      || status_code = 501
      || method_unavailable_error
    in
    let do_upload_video ~service_token ~user_did attempts_left =
      let rec upload_via_video attempts_left =
        let video_url = Printf.sprintf "%s/xrpc/app.bsky.video.uploadVideo?did=%s&name=video.mp4"
          video_upload_host (Uri.pct_encode user_did) in
        let video_headers = [
          ("Authorization", Printf.sprintf "Bearer %s" service_token);
          ("Content-Type", mime_type);
        ] in
        Config.Http.post ~headers:video_headers ~body:blob_data video_url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                match parse_blob_from_json json with
                | Some blob -> on_success (blob, alt_text)
                | None ->
                    (match parse_job_id json with
                     | Some job_id -> poll_video_job job_id 5
                     | None -> on_error "Video upload response missing both blob and jobId")
              with e -> on_error (Printf.sprintf "Failed to parse video upload response: %s" (Printexc.to_string e))
            else
              if should_fallback_from_video_endpoint response.status response.body then
                upload_via_blob ()
              else
                on_error (Printf.sprintf "Video upload failed (%d): %s" response.status response.body))
          (fun err ->
            if attempts_left > 1 then
              upload_via_video (attempts_left - 1)
            else
              on_error (Printf.sprintf "Video upload network error: %s" err))
      in
      upload_via_video attempts_left
    in
    if is_video then
      match did with
      | Some user_did ->
          (* Get service auth token for video upload *)
          get_service_auth ~access_jwt ~did:user_did
            (fun service_token ->
              do_upload_video ~service_token ~user_did 2)
            (fun _err ->
              (* Fall back to regular upload if service auth fails *)
              do_upload_video ~service_token:access_jwt ~user_did 2)
      | None ->
          (* No DID available, use regular JWT with video host *)
          do_upload_video ~service_token:access_jwt ~user_did:"unknown" 2
    else
      upload_via_blob ()
  
  (** Extract OpenGraph meta tag content from HTML *)
  let extract_og_tag html property =
    (* Match: <meta property="og:title" content="..."> or <meta content="..." property="og:title"> *)
    let patterns = [
      Printf.sprintf "<meta[^>]*property=['\"]og:%s['\"][^>]*content=['\"]([^'\"]*)['\"]" property;
      Printf.sprintf "<meta[^>]*content=['\"]([^'\"]*)['\"][^>]*property=['\"]og:%s['\"]" property;
    ] in
    let rec try_patterns pats =
      match pats with
      | [] -> None
      | pattern :: rest ->
          try
            let regex = Re.Pcre.regexp ~flags:[`CASELESS] pattern in
            let group = Re.exec regex html in
            Some (Re.Group.get group 1)
          with Not_found -> try_patterns rest
    in
    try_patterns patterns

  (** Fetch link card metadata by scraping OpenGraph tags.
      Returns (card option, warnings list) *)
  let fetch_link_card ~access_jwt ~url on_success =
    let warnings = ref [] in
    
    (* Fetch the HTML content of the URL *)
    Config.Http.get url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let html = response.body in
            
            (* Extract OpenGraph metadata *)
            let title = extract_og_tag html "title" in
            let description = extract_og_tag html "description" in
            let image_url = extract_og_tag html "image" in
            
            (* Only create card if we have at least a title *)
            match title with
            | None -> 
                warnings := Error_types.Link_card_failed 
                  (Printf.sprintf "No og:title found for %s" url) :: !warnings;
                on_success (None, !warnings)
            | Some title_str ->
                let description_str = match description with
                  | Some d -> d
                  | None -> ""
                in
                
                (* Helper to complete card creation *)
                let create_card thumb_blob =
                  let external_fields = [
                    ("uri", `String url);
                    ("title", `String title_str);
                    ("description", `String description_str);
                  ] in
                  let external_with_thumb = match thumb_blob with
                    | Some blob -> external_fields @ [("thumb", blob)]
                    | None -> external_fields
                  in
                  let card = `Assoc [
                    ("$type", `String "app.bsky.embed.external");
                    ("external", `Assoc external_with_thumb);
                  ] in
                  on_success (Some card, !warnings)
                in
                
                (* If there's an image, fetch and upload it *)
                match image_url with
                | None -> 
                    create_card None
                | Some img_url ->
                    (* Handle relative URLs *)
                    let full_img_url = 
                      if String.contains img_url ':' then img_url
                      else
                        (* Parse base URL and append relative path *)
                        let base_uri = Uri.of_string url in
                        let scheme = Uri.scheme base_uri |> Option.value ~default:"https" in
                        let host = Uri.host base_uri |> Option.value ~default:"" in
                        if String.length img_url > 0 && img_url.[0] = '/' then
                          Printf.sprintf "%s://%s%s" scheme host img_url
                        else
                          Printf.sprintf "%s://%s/%s" scheme host img_url
                    in
                    
                    (* Fetch the image *)
                    Config.Http.get full_img_url
                      (fun img_response ->
                        if img_response.status >= 200 && img_response.status < 300 then
                          (* Get mime type from response headers *)
                          let raw_mime_type = 
                            List.assoc_opt "content-type" img_response.headers
                            |> Option.value ~default:"image/jpeg"
                          in
                          (* Extract base MIME type (strip charset and other parameters) *)
                          let mime_type = 
                            match String.split_on_char ';' raw_mime_type with
                            | base :: _ -> String.trim base
                            | [] -> raw_mime_type
                          in
                          
                          (* Validate it's actually an image MIME type *)
                          let is_image_mime = 
                            String.length mime_type >= 6 && 
                            String.lowercase_ascii (String.sub mime_type 0 6) = "image/"
                          in
                          
                          if not is_image_mime then begin
                            warnings := Error_types.Thumbnail_skipped 
                              (Printf.sprintf "Non-image MIME type: %s" mime_type) :: !warnings;
                            create_card None
                          end
                          else
                            (* Check size limit (1MB max for images) *)
                            let img_size = String.length img_response.body in
                            
                            if img_size > 1000000 then begin
                              warnings := Error_types.Thumbnail_skipped 
                                (Printf.sprintf "Image too large: %d bytes" img_size) :: !warnings;
                              create_card None
                            end
                            else
                              (* Upload the image as a blob *)
                              upload_blob ~access_jwt ~blob_data:img_response.body 
                                ~mime_type ~alt_text:None
                                (fun (blob, _) -> create_card (Some blob))
                                (fun err -> 
                                  warnings := Error_types.Thumbnail_skipped 
                                    (Printf.sprintf "Upload failed: %s" err) :: !warnings;
                                  create_card None)
                        else begin
                          warnings := Error_types.Thumbnail_skipped 
                            (Printf.sprintf "HTTP %d fetching thumbnail" img_response.status) :: !warnings;
                          create_card None
                        end)
                      (fun err -> 
                        warnings := Error_types.Thumbnail_skipped 
                          (Printf.sprintf "Network error: %s" err) :: !warnings;
                        create_card None)
          with e ->
            warnings := Error_types.Link_card_failed 
              (Printf.sprintf "Parse error: %s" (Printexc.to_string e)) :: !warnings;
            on_success (None, !warnings)
        else begin
          warnings := Error_types.Link_card_failed 
            (Printf.sprintf "HTTP %d fetching URL" response.status) :: !warnings;
          on_success (None, !warnings)
        end)
      (fun err -> 
        warnings := Error_types.Link_card_failed 
          (Printf.sprintf "Network error: %s" err) :: !warnings;
        on_success (None, !warnings))
  
  (** Ensure valid session token, reusing cached session when available *)
  let ensure_valid_token ~account_id on_success on_error =
    (* Check for a cached session that hasn't expired *)
    let now = Unix.gettimeofday () in
    match Hashtbl.find_opt session_cache account_id with
    | Some (_did, access_jwt, created_at)
      when now -. created_at < session_max_age_seconds ->
        on_success access_jwt
    | _ ->
        (* No valid cached session - create a new one *)
        Config.get_credentials ~account_id
          (fun creds ->
            (* Bluesky uses identifier (handle/email) as access_token and password as refresh_token *)
            match creds.refresh_token with
            | None ->
                Config.update_health_status ~account_id ~status:"refresh_failed"
                  ~error_message:(Some "No app password available")
                  (fun () -> on_error (Error_types.Auth_error Error_types.Missing_credentials))
                  (fun _ -> on_error (Error_types.Auth_error Error_types.Missing_credentials))
            | Some password ->
                (* Create new session *)
                create_session ~identifier:creds.access_token ~password
                  (fun (did, access_jwt) ->
                    (* Cache the session *)
                    Hashtbl.replace session_cache account_id (did, access_jwt, Unix.gettimeofday ());
                    Config.update_health_status ~account_id ~status:"healthy" ~error_message:None
                      (fun () -> on_success access_jwt)
                      (fun _ -> on_success access_jwt))
                  (fun err ->
                    (* Invalidate any stale cache entry *)
                    invalidate_session ~account_id;
                    Config.update_health_status ~account_id ~status:"refresh_failed"
                      ~error_message:(Some ("Session creation failed: " ^ err))
                      (fun () -> on_error (Error_types.Auth_error (Error_types.Refresh_failed err)))
                      (fun _ -> on_error (Error_types.Auth_error (Error_types.Refresh_failed err)))))
          (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err)))
  
  (** Ensure valid session token and also return the DID *)
  let ensure_valid_token_with_did ~account_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        match Hashtbl.find_opt session_cache account_id with
        | Some (did, _, _) -> on_success (did, access_jwt)
        | None ->
            (* ensure_valid_token succeeded but the cache entry is missing;
               this indicates an internal inconsistency *)
            on_error (Error_types.Internal_error "Session cache missing DID after successful token retrieval"))
      on_error

  (** {1 Public API - Posting} *)

  (** Post with optional reply references (internal implementation) *)
  let post_with_reply_impl ~account_id ~text ~media_urls ?(alt_texts=[]) ?(widths=[]) ?(heights=[]) ?(skip_enrichments=false) ?(validate_media_before_upload=false) ~reply_refs on_result =
    (* Validate first *)
    match validate_post ~text ~media_count:(List.length media_urls) () with
    | Error errs ->
        on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        let accumulated_warnings = ref [] in
        
        ensure_valid_token_with_did ~account_id
          (fun (did, access_jwt) ->
            Config.get_credentials ~account_id
              (fun creds ->
                let identifier = creds.access_token in

                (* Pair URLs with alt text and dimensions - use None if lists are shorter *)
                let urls_with_meta = List.mapi (fun i url ->
                  let alt_text = try List.nth alt_texts i with _ -> None in
                  let width = try List.nth widths i with _ -> None in
                  let height = try List.nth heights i with _ -> None in
                  (url, alt_text, width, height)
                ) media_urls in

                (* Helper to determine media type from MIME type *)
                let media_type_of_mime mime_type =
                  if String.starts_with ~prefix:"video/" mime_type then Platform_types.Video
                  else if mime_type = "image/gif" then Platform_types.Gif
                  else Platform_types.Image
                in

                (* Helper to upload multiple blobs in sequence *)
                let rec upload_blobs_seq urls_with_meta acc on_complete on_err =
                  match urls_with_meta with
                  | [] -> on_complete (List.rev acc)
                  | (url, alt_text, w, h) :: rest ->
                      (* Fetch media from URL *)
                      Config.Http.get url
                        (fun media_resp ->
                          if media_resp.status >= 200 && media_resp.status < 300 then
                            let mime_type =
                              List.assoc_opt "content-type" media_resp.headers
                              |> Option.value ~default:"application/octet-stream"
                            in
                            let file_size = String.length media_resp.body in

                            (* Validate media if requested *)
                            let validation_result =
                              if validate_media_before_upload then
                                let media : Platform_types.post_media = {
                                  media_type = media_type_of_mime mime_type;
                                  mime_type = mime_type;
                                  file_size_bytes = file_size;
                                  width = None;
                                  height = None;
                                  duration_seconds = None;
                                  alt_text = alt_text;
                                } in
                                validate_media ~media
                              else
                                Ok ()
                            in

                            (match validation_result with
                            | Error errs ->
                                on_result (Error_types.Failure (Error_types.Validation_error errs))
                            | Ok () ->
                                (* Upload blob with alt text, passing DID for video service auth *)
                                upload_blob ~access_jwt ~did ~blob_data:media_resp.body ~mime_type ~alt_text
                                  (fun (blob, alt) ->
                                    let media_type = media_type_of_mime mime_type in
                                    upload_blobs_seq rest ((blob, alt, media_type, w, h) :: acc) on_complete on_err)
                                  (fun err -> on_err (Error_types.Internal_error err)))
                          else
                            on_err (Error_types.make_api_error
                              ~platform:Platform_types.Bluesky
                              ~status_code:media_resp.status
                              ~message:(Printf.sprintf "Failed to fetch media from %s" url) ()))
                        (fun err -> on_err (Error_types.Network_error (Error_types.Connection_failed err)))
                in

                (* Upload media if provided (max 4 images) *)
                let media_to_upload = List.filteri (fun i _ -> i < 4) urls_with_meta in
                upload_blobs_seq media_to_upload []
                  (fun blobs ->
                    (* Extract facets from text *)
                    extract_facets text
                      (fun facets ->
                        (* Create post record *)
                        let now = Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 (Ptime_clock.now ()) in
                        
                        let base_record = [
                          ("$type", `String "app.bsky.feed.post");
                          ("text", `String text);
                          ("createdAt", `String now);
                        ] in
                        
                        let base_with_facets = 
                          if List.length facets > 0 then
                            base_record @ [("facets", `List facets)]
                          else
                            base_record
                        in
                        
                        (* Add reply references if provided *)
                        let base_with_reply = match reply_refs with
                          | None -> base_with_facets
                          | Some (root_uri, root_cid, parent_uri, parent_cid) ->
                              base_with_facets @ [
                                ("reply", `Assoc [
                                  ("root", `Assoc [
                                    ("uri", `String root_uri);
                                    ("cid", `String root_cid);
                                  ]);
                                  ("parent", `Assoc [
                                    ("uri", `String parent_uri);
                                    ("cid", `String parent_cid);
                                  ]);
                                ])
                              ]
                        in
                    
                    (* Add embed based on content *)
                    let post_record_cont =
                      if List.length blobs > 0 then
                        let video_blobs = List.filter (fun (_, _, mt, _, _) -> mt = Platform_types.Video) blobs in
                        let non_video_blobs = List.filter (fun (_, _, mt, _, _) -> mt <> Platform_types.Video) blobs in
                        if List.length video_blobs > 1 then
                          fun _ ->
                            on_result (Error_types.Failure (Error_types.Validation_error [
                              Error_types.Too_many_media { count = List.length video_blobs; max = 1 }
                            ]))
                        else if List.length video_blobs > 0 && List.length non_video_blobs > 0 then
                          fun _ ->
                            on_result (Error_types.Failure (Error_types.Validation_error [
                              Error_types.Media_unsupported_format
                                "Mixing video and image embeds in one Bluesky post is not supported by this provider"
                            ]))
                        else if List.length video_blobs = 1 then
                          let (video_blob, alt_text_opt, _, _, _) = List.hd video_blobs in
                          let video_fields =
                            match alt_text_opt with
                            | Some alt when String.length alt > 0 ->
                                [
                                  ("$type", `String "app.bsky.embed.video");
                                  ("video", video_blob);
                                  ("alt", `String alt);
                                ]
                            | _ ->
                                [
                                  ("$type", `String "app.bsky.embed.video");
                                  ("video", video_blob);
                                ]
                          in
                          fun on_rec_success ->
                            on_rec_success (`Assoc (base_with_reply @ [
                              ("embed", `Assoc video_fields)
                            ]))
                        else
                          (* Images present - skip link card *)
                          let images_json = `List (List.map (fun (blob, alt_text_opt, _, w, h) ->
                          let alt_text = match alt_text_opt with
                            | Some alt when String.length alt > 0 -> alt
                            | _ -> ""
                          in
                          let base_fields = [
                            ("alt", `String alt_text);
                            ("image", blob);
                          ] in
                          let fields_with_aspect = match w, h with
                            | Some width, Some height ->
                                base_fields @ [
                                  ("aspectRatio", `Assoc [
                                    ("width", `Int width);
                                    ("height", `Int height);
                                  ])
                                ]
                            | _ -> base_fields
                          in
                          `Assoc fields_with_aspect
                          ) non_video_blobs) in
                          fun on_rec_success ->
                            on_rec_success (`Assoc (base_with_reply @ [
                              ("embed", `Assoc [
                                ("$type", `String "app.bsky.embed.images");
                                ("images", images_json);
                              ])
                            ]))
                      else if skip_enrichments then
                        (* Skip link card extraction *)
                        fun on_rec_success -> on_rec_success (`Assoc base_with_reply)
                      else
                        (* Try external link card *)
                        let first_url =
                          try
                            let url_pattern = Re.Pcre.regexp 
                              "https?://[a-zA-Z0-9][-a-zA-Z0-9@:%._\\+~#=]{0,256}\\.[a-zA-Z0-9()]{1,6}\\b[-a-zA-Z0-9()@:%_\\+.~#?&/=]*"
                            in
                            let group = Re.exec url_pattern text in
                            Some (Re.Group.get group 0)
                          with Not_found -> None
                        in
                        match first_url with
                        | None -> 
                            fun on_rec_success -> on_rec_success (`Assoc base_with_reply)
                        | Some url ->
                            fun on_rec_success ->
                              fetch_link_card ~access_jwt ~url
                                (fun (card_opt, card_warnings) ->
                                  accumulated_warnings := !accumulated_warnings @ card_warnings;
                                  match card_opt with
                                  | None -> on_rec_success (`Assoc base_with_reply)
                                  | Some card_json ->
                                      on_rec_success (`Assoc (base_with_reply @ [("embed", card_json)])))
                    in
                    
                    (* Continue with post creation *)
                    post_record_cont
                      (fun post_record ->
                        let url = Printf.sprintf "%s/xrpc/com.atproto.repo.createRecord" pds_url in
                        let body = `Assoc [
                          ("repo", `String identifier);
                          ("collection", `String "app.bsky.feed.post");
                          ("record", post_record);
                        ] in
                        let body_str = Yojson.Basic.to_string body in
                        let headers = [
                          ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
                          ("Content-Type", "application/json");
                        ] in
                        
                        Config.Http.post ~headers ~body:body_str url
                          (fun response ->
                            if response.status >= 200 && response.status < 300 then
                              try
                                let json = Yojson.Basic.from_string response.body in
                                let open Yojson.Basic.Util in
                                let post_uri = json |> member "uri" |> to_string in
                                let post_cid = json |> member "cid" |> to_string in
                                (* Return both URI and CID as "uri|cid" *)
                                let result = Printf.sprintf "%s|%s" post_uri post_cid in
                                if !accumulated_warnings = [] then
                                  on_result (Error_types.Success result)
                                else
                                  on_result (Error_types.Partial_success { 
                                    result; 
                                    warnings = !accumulated_warnings 
                                  })
                              with e ->
                                on_result (Error_types.Failure (Error_types.Internal_error 
                                  (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e))))
                            else
                              on_result (Error_types.Failure (parse_api_error 
                                ~status_code:response.status ~body:response.body)))
                          (fun err -> on_result (Error_types.Failure (Error_types.Network_error 
                            (Error_types.Connection_failed err))))))
                      (fun _ -> on_result (Error_types.Failure (Error_types.Internal_error 
                        "Facet extraction failed"))))
                  (fun err -> on_result (Error_types.Failure err)))
              (fun err -> on_result (Error_types.Failure (Error_types.Network_error 
                (Error_types.Connection_failed err)))))
          (fun err -> on_result (Error_types.Failure err))
  
  (** Post a single post to Bluesky

      @param account_id The account identifier
      @param text The post text (max 300 characters)
      @param media_urls List of media URLs to attach (max 4)
      @param alt_texts Optional alt text for each media item
      @param widths Optional width for each image (for aspectRatio)
      @param heights Optional height for each image (for aspectRatio)
      @param skip_enrichments Skip link card fetching (default: false)
      @param validate_media_before_upload When true, validates media size after download
             but before upload. Bluesky limits: 50MB video, 60s duration, 1MB images.
             Default: false
      @param on_result Callback receiving the outcome
  *)
  let post_single ~account_id ~text ~media_urls ?(alt_texts=[]) ?(widths=[]) ?(heights=[]) ?(skip_enrichments=false) ?(validate_media_before_upload=false) on_result =
    post_with_reply_impl ~account_id ~text ~media_urls ~alt_texts ~widths ~heights ~skip_enrichments ~validate_media_before_upload ~reply_refs:None on_result

  (** Post a thread to Bluesky

      @param account_id The account identifier
      @param texts List of post texts
      @param media_urls_per_post Media URLs for each post
      @param alt_texts_per_post Alt texts for each post's media
      @param widths_per_post Optional widths for each post's images (for aspectRatio)
      @param heights_per_post Optional heights for each post's images (for aspectRatio)
      @param skip_enrichments Skip link card fetching (default: false)
      @param validate_media_before_upload When true, validates each media file after download.
             Default: false
      @param on_result Callback receiving the outcome with thread_result
  *)
  let post_thread ~account_id ~texts ~media_urls_per_post ?(alt_texts_per_post=[]) ?(widths_per_post=[]) ?(heights_per_post=[]) ?(skip_enrichments=false) ?(validate_media_before_upload=false) on_result =
    (* Validate entire thread upfront *)
    let media_counts = List.map List.length media_urls_per_post in
    match validate_thread ~texts ~media_counts () with
    | Error errs ->
        on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        let total_requested = List.length texts in
        let accumulated_warnings = ref [] in

        (* Pair media URLs with alt text and dimensions for each post *)
        let media_with_meta_per_post = List.mapi (fun i media_urls ->
          let alt_texts = try List.nth alt_texts_per_post i with _ -> [] in
          let post_widths = try List.nth widths_per_post i with _ -> [] in
          let post_heights = try List.nth heights_per_post i with _ -> [] in
          (media_urls, alt_texts, post_widths, post_heights)
        ) media_urls_per_post in
        
        (* Helper to post thread items sequentially *)
        let rec post_thread_items remaining_texts remaining_media root_ref parent_ref acc_uris =
          match remaining_texts with
          | [] -> 
              let result = { 
                Error_types.posted_ids = List.rev acc_uris; 
                failed_at_index = None;
                total_requested;
              } in
              if !accumulated_warnings = [] then
                on_result (Error_types.Success result)
              else
                on_result (Error_types.Partial_success { result; warnings = !accumulated_warnings })
          | text :: rest_texts ->
              let (media, alt_texts, post_widths, post_heights) = match remaining_media with
                | [] -> ([], [], [], [])
                | (m, a, w, h) :: _ -> (m, a, w, h)
              in
              let rest_media = match remaining_media with
                | [] -> []
                | _ :: r -> r
              in

              let reply_refs = match root_ref with
                | None -> None  (* First post, no reply *)
                | Some (root_uri, root_cid) ->
                    (* Subsequent posts are replies *)
                    match parent_ref with
                    | Some (parent_uri, parent_cid) ->
                        Some (root_uri, root_cid, parent_uri, parent_cid)
                    | None ->
                        (* Should not happen, but use root as parent *)
                        Some (root_uri, root_cid, root_uri, root_cid)
              in

              post_with_reply_impl ~account_id ~text ~media_urls:media ~alt_texts ~widths:post_widths ~heights:post_heights ~skip_enrichments ~validate_media_before_upload ~reply_refs
                (function
                  | Error_types.Success uri_cid ->
                      process_post_result uri_cid [] rest_texts rest_media root_ref acc_uris
                  | Error_types.Partial_success { result = uri_cid; warnings } ->
                      accumulated_warnings := !accumulated_warnings @ warnings;
                      process_post_result uri_cid warnings rest_texts rest_media root_ref acc_uris
                  | Error_types.Failure err ->
                      (* Thread failed mid-way - return partial result *)
                      let result = { 
                        Error_types.posted_ids = List.rev acc_uris;
                        failed_at_index = Some (total_requested - List.length remaining_texts);
                        total_requested;
                      } in
                      if acc_uris = [] then
                        (* No posts succeeded - this is a full failure *)
                        on_result (Error_types.Failure err)
                      else
                        (* Some posts succeeded - partial success with the error info in warnings *)
                        on_result (Error_types.Partial_success { 
                          result; 
                          warnings = !accumulated_warnings @ [
                            Error_types.Enrichment_skipped (Error_types.error_to_string err)
                          ]
                        }))
        
        and process_post_result uri_cid _warnings rest_texts rest_media root_ref acc_uris =
          (* Parse URI and CID from "uri|cid" format *)
          match String.split_on_char '|' uri_cid with
          | [uri; cid] ->
              let new_root_ref = match root_ref with
                | None -> Some (uri, cid)  (* First post becomes root *)
                | Some r -> Some r  (* Keep existing root *)
              in
              let new_parent_ref = Some (uri, cid) in
              post_thread_items rest_texts rest_media new_root_ref new_parent_ref (uri_cid :: acc_uris)
          | _ ->
              on_result (Error_types.Failure (Error_types.Internal_error 
                "Failed to parse post response (expected uri|cid format)"))
        in
        
        post_thread_items texts media_with_meta_per_post None None []
  
  (** {1 Public API - Post Management} *)
  
  (** Delete a post from Bluesky *)
  let delete_post ~account_id ~post_uri on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        Config.get_credentials ~account_id
          (fun creds ->
            let identifier = creds.access_token in
            let uri_parts = String.split_on_char '/' post_uri in
            let rkey = List.nth uri_parts (List.length uri_parts - 1) in
            
            let url = Printf.sprintf "%s/xrpc/com.atproto.repo.deleteRecord" pds_url in
            let body = `Assoc [
              ("repo", `String identifier);
              ("collection", `String "app.bsky.feed.post");
              ("rkey", `String rkey);
            ] in
            let body_str = Yojson.Basic.to_string body in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
              ("Content-Type", "application/json");
            ] in
            
            Config.Http.post ~headers ~body:body_str url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  on_result (Error_types.Success ())
                else
                  on_result (Error_types.Failure (parse_api_error 
                    ~status_code:response.status ~body:response.body)))
              (fun err -> on_result (Error_types.Failure (Error_types.Network_error 
                (Error_types.Connection_failed err)))))
          (fun err -> on_result (Error_types.Failure (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error_types.Failure err))
  
  (** {1 Public API - Interactions} *)
  
  (** Like a post *)
  let like_post ~account_id ~post_uri ~post_cid on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        Config.get_credentials ~account_id
          (fun creds ->
            let identifier = creds.access_token in
            let now = Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 (Ptime_clock.now ()) in
            
            let url = Printf.sprintf "%s/xrpc/com.atproto.repo.createRecord" pds_url in
            let body = `Assoc [
              ("repo", `String identifier);
              ("collection", `String "app.bsky.feed.like");
              ("record", `Assoc [
                ("$type", `String "app.bsky.feed.like");
                ("subject", `Assoc [
                  ("uri", `String post_uri);
                  ("cid", `String post_cid);
                ]);
                ("createdAt", `String now);
              ]);
            ] in
            let body_str = Yojson.Basic.to_string body in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
              ("Content-Type", "application/json");
            ] in
            
            Config.Http.post ~headers ~body:body_str url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  try
                    let json = Yojson.Basic.from_string response.body in
                    let like_uri = json 
                      |> Yojson.Basic.Util.member "uri" 
                      |> Yojson.Basic.Util.to_string in
                    on_result (Ok like_uri)
                  with e ->
                    on_result (Error (Error_types.Internal_error 
                      (Printf.sprintf "Failed to parse like response: %s" (Printexc.to_string e))))
                else
                  on_result (Error (parse_api_error 
                    ~status_code:response.status ~body:response.body)))
              (fun err -> on_result (Error (Error_types.Network_error 
                (Error_types.Connection_failed err)))))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Unlike a post *)
  let unlike_post ~account_id ~like_uri on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        Config.get_credentials ~account_id
          (fun creds ->
            let identifier = creds.access_token in
            let uri_parts = String.split_on_char '/' like_uri in
            let rkey = List.nth uri_parts (List.length uri_parts - 1) in
            
            let url = Printf.sprintf "%s/xrpc/com.atproto.repo.deleteRecord" pds_url in
            let body = `Assoc [
              ("repo", `String identifier);
              ("collection", `String "app.bsky.feed.like");
              ("rkey", `String rkey);
            ] in
            let body_str = Yojson.Basic.to_string body in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
              ("Content-Type", "application/json");
            ] in
            
            Config.Http.post ~headers ~body:body_str url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  on_result (Ok ())
                else
                  on_result (Error (parse_api_error 
                    ~status_code:response.status ~body:response.body)))
              (fun err -> on_result (Error (Error_types.Network_error 
                (Error_types.Connection_failed err)))))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Repost a post *)
  let repost ~account_id ~post_uri ~post_cid on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        Config.get_credentials ~account_id
          (fun creds ->
            let identifier = creds.access_token in
            let now = Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 (Ptime_clock.now ()) in
            
            let url = Printf.sprintf "%s/xrpc/com.atproto.repo.createRecord" pds_url in
            let body = `Assoc [
              ("repo", `String identifier);
              ("collection", `String "app.bsky.feed.repost");
              ("record", `Assoc [
                ("$type", `String "app.bsky.feed.repost");
                ("subject", `Assoc [
                  ("uri", `String post_uri);
                  ("cid", `String post_cid);
                ]);
                ("createdAt", `String now);
              ]);
            ] in
            let body_str = Yojson.Basic.to_string body in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
              ("Content-Type", "application/json");
            ] in
            
            Config.Http.post ~headers ~body:body_str url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  try
                    let json = Yojson.Basic.from_string response.body in
                    let repost_uri = json 
                      |> Yojson.Basic.Util.member "uri" 
                      |> Yojson.Basic.Util.to_string in
                    on_result (Ok repost_uri)
                  with e ->
                    on_result (Error (Error_types.Internal_error 
                      (Printf.sprintf "Failed to parse repost response: %s" (Printexc.to_string e))))
                else
                  on_result (Error (parse_api_error 
                    ~status_code:response.status ~body:response.body)))
              (fun err -> on_result (Error (Error_types.Network_error 
                (Error_types.Connection_failed err)))))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Unrepost *)
  let unrepost ~account_id ~repost_uri on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        Config.get_credentials ~account_id
          (fun creds ->
            let identifier = creds.access_token in
            let uri_parts = String.split_on_char '/' repost_uri in
            let rkey = List.nth uri_parts (List.length uri_parts - 1) in
            
            let url = Printf.sprintf "%s/xrpc/com.atproto.repo.deleteRecord" pds_url in
            let body = `Assoc [
              ("repo", `String identifier);
              ("collection", `String "app.bsky.feed.repost");
              ("rkey", `String rkey);
            ] in
            let body_str = Yojson.Basic.to_string body in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
              ("Content-Type", "application/json");
            ] in
            
            Config.Http.post ~headers ~body:body_str url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  on_result (Ok ())
                else
                  on_result (Error (parse_api_error 
                    ~status_code:response.status ~body:response.body)))
              (fun err -> on_result (Error (Error_types.Network_error 
                (Error_types.Connection_failed err)))))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** {1 Public API - Social Graph} *)
  
  (** Follow a user *)
  let follow ~account_id ~did on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        Config.get_credentials ~account_id
          (fun creds ->
            let identifier = creds.access_token in
            let now = Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 (Ptime_clock.now ()) in
            
            let url = Printf.sprintf "%s/xrpc/com.atproto.repo.createRecord" pds_url in
            let body = `Assoc [
              ("repo", `String identifier);
              ("collection", `String "app.bsky.graph.follow");
              ("record", `Assoc [
                ("$type", `String "app.bsky.graph.follow");
                ("subject", `String did);
                ("createdAt", `String now);
              ]);
            ] in
            let body_str = Yojson.Basic.to_string body in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
              ("Content-Type", "application/json");
            ] in
            
            Config.Http.post ~headers ~body:body_str url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  try
                    let json = Yojson.Basic.from_string response.body in
                    let follow_uri = json 
                      |> Yojson.Basic.Util.member "uri" 
                      |> Yojson.Basic.Util.to_string in
                    on_result (Ok follow_uri)
                  with e ->
                    on_result (Error (Error_types.Internal_error 
                      (Printf.sprintf "Failed to parse follow response: %s" (Printexc.to_string e))))
                else
                  on_result (Error (parse_api_error 
                    ~status_code:response.status ~body:response.body)))
              (fun err -> on_result (Error (Error_types.Network_error 
                (Error_types.Connection_failed err)))))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Unfollow a user *)
  let unfollow ~account_id ~follow_uri on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        Config.get_credentials ~account_id
          (fun creds ->
            let identifier = creds.access_token in
            let uri_parts = String.split_on_char '/' follow_uri in
            let rkey = List.nth uri_parts (List.length uri_parts - 1) in
            
            let url = Printf.sprintf "%s/xrpc/com.atproto.repo.deleteRecord" pds_url in
            let body = `Assoc [
              ("repo", `String identifier);
              ("collection", `String "app.bsky.graph.follow");
              ("rkey", `String rkey);
            ] in
            let body_str = Yojson.Basic.to_string body in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
              ("Content-Type", "application/json");
            ] in
            
            Config.Http.post ~headers ~body:body_str url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  on_result (Ok ())
                else
                  on_result (Error (parse_api_error 
                    ~status_code:response.status ~body:response.body)))
              (fun err -> on_result (Error (Error_types.Network_error 
                (Error_types.Connection_failed err)))))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** {1 Public API - Read Operations} *)
  
  (** Get a user profile *)
  let get_profile ~account_id ~actor on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let url = Printf.sprintf "%s/xrpc/app.bsky.actor.getProfile?actor=%s" 
          pds_url (Uri.pct_encode actor) in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              match parse_json_object_or_error ~context:"Failed to parse profile" ~body:response.body with
              | Ok json ->
                  (try
                     let _ = json |> Yojson.Basic.Util.member "did" |> Yojson.Basic.Util.to_string in
                     let _ = json |> Yojson.Basic.Util.member "handle" |> Yojson.Basic.Util.to_string in
                     on_result (Ok json)
                   with e ->
                     on_result (Error (Error_types.Internal_error
                       (Printf.sprintf "Failed to parse profile: %s" (Printexc.to_string e)))))
              | Error err -> on_result (Error err)
            else
              on_result (Error (parse_api_error 
                ~status_code:response.status ~body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Get a post thread *)
  let get_post_thread ~account_id ~post_uri on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let url = Printf.sprintf "%s/xrpc/app.bsky.feed.getPostThread?uri=%s" 
          pds_url (Uri.pct_encode post_uri) in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              match parse_json_object_or_error ~context:"Failed to parse thread" ~body:response.body with
              | Ok json ->
                  (try
                     let _ =
                       json
                       |> Yojson.Basic.Util.member "thread"
                       |> Yojson.Basic.Util.member "$type"
                       |> Yojson.Basic.Util.to_string
                     in
                     on_result (Ok json)
                   with e ->
                     on_result (Error (Error_types.Internal_error
                       (Printf.sprintf "Failed to parse thread: %s" (Printexc.to_string e)))))
              | Error err -> on_result (Error err)
            else
              on_result (Error (parse_api_error 
                ~status_code:response.status ~body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Get timeline *)
  let get_timeline ~account_id ?limit ?cursor on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let params = [] in
        let params = match limit with
          | Some l -> ("limit", string_of_int l) :: params
          | None -> params
        in
        let params = match cursor with
          | Some c -> ("cursor", c) :: params
          | None -> params
        in
        let query_string =
          if params = [] then ""
          else
            "?" ^ (String.concat "&" (List.map (fun (k, v) ->
              Printf.sprintf "%s=%s" k (Uri.pct_encode v)) params))
        in
        let url = Printf.sprintf "%s/xrpc/app.bsky.feed.getTimeline%s" pds_url query_string in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              match parse_json_object_or_error ~context:"Failed to parse timeline" ~body:response.body with
              | Ok json ->
                  (match ensure_array_field ~json ~field:"feed" ~context:"Failed to parse timeline" with
                   | Ok () -> on_result (Ok json)
                   | Error err -> on_result (Error err))
              | Error err -> on_result (Error err)
            else
              on_result (Error (parse_api_error 
                ~status_code:response.status ~body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Quote a post with optional text and media *)
  let quote_post ~account_id ~post_uri ~post_cid ~text ~media_urls ?(alt_texts=[]) ?(widths=[]) ?(heights=[]) ?(skip_enrichments=false) on_result =
    (* Validate first *)
    match validate_post ~text ~media_count:(List.length media_urls) () with
    | Error errs ->
        on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        ensure_valid_token_with_did ~account_id
          (fun (did, access_jwt) ->
            Config.get_credentials ~account_id
              (fun creds ->
                let identifier = creds.access_token in
                let _ = skip_enrichments in (* Quote posts don't use link cards *)

                (* Pair URLs with alt text and dimensions *)
                let urls_with_meta = List.mapi (fun i url ->
                  let alt_text = try List.nth alt_texts i with _ -> None in
                  let w = try List.nth widths i with _ -> None in
                  let h = try List.nth heights i with _ -> None in
                  (url, alt_text, w, h)
                ) media_urls in

                (* Helper to upload blobs *)
                let rec upload_blobs_seq urls_with_meta acc on_complete on_err =
                  match urls_with_meta with
                  | [] -> on_complete (List.rev acc)
                  | (url, alt_text, w, h) :: rest ->
                      Config.Http.get url
                        (fun media_resp ->
                          if media_resp.status >= 200 && media_resp.status < 300 then
                            let mime_type =
                              List.assoc_opt "content-type" media_resp.headers
                              |> Option.value ~default:"application/octet-stream"
                            in
                            (* Upload with alt text, passing DID for video service auth *)
                            upload_blob ~access_jwt ~did ~blob_data:media_resp.body ~mime_type ~alt_text
                              (fun (blob, alt) ->
                                let media_type =
                                  if String.starts_with ~prefix:"video/" mime_type then Platform_types.Video
                                  else if mime_type = "image/gif" then Platform_types.Gif
                                  else Platform_types.Image
                                in
                                upload_blobs_seq rest ((blob, alt, media_type, w, h) :: acc) on_complete on_err)
                              (fun err -> on_err (Error_types.Internal_error err))
                          else
                            on_err (Error_types.make_api_error
                              ~platform:Platform_types.Bluesky
                              ~status_code:media_resp.status
                              ~message:(Printf.sprintf "Failed to fetch media from %s" url) ()))
                        (fun err -> on_err (Error_types.Network_error (Error_types.Connection_failed err)))
                in

                let media_to_upload = List.filteri (fun i _ -> i < 4) urls_with_meta in
                upload_blobs_seq media_to_upload []
                  (fun blobs ->
                    let video_count = List.length (List.filter (fun (_, _, mt, _, _) -> mt = Platform_types.Video) blobs) in
                    let non_video_count = List.length (List.filter (fun (_, _, mt, _, _) -> mt <> Platform_types.Video) blobs) in
                    if video_count > 1 then
                      on_result (Error_types.Failure (Error_types.Validation_error [
                        Error_types.Too_many_media { count = video_count; max = 1 }
                      ]))
                    else if video_count > 0 && non_video_count > 0 then
                      on_result (Error_types.Failure (Error_types.Validation_error [
                        Error_types.Media_unsupported_format
                          "Mixing video and image embeds in one quoted Bluesky post is not supported by this provider"
                      ]))
                    else
                    extract_facets text
                      (fun facets ->
                        let now = Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 (Ptime_clock.now ()) in

                        let base_record = [
                          ("$type", `String "app.bsky.feed.post");
                          ("text", `String text);
                          ("createdAt", `String now);
                        ] in

                        let base_with_facets =
                          if List.length facets > 0 then
                            base_record @ [("facets", `List facets)]
                          else
                            base_record
                        in

                        (* Create embed for quote post *)
                        let quote_embed = `Assoc [
                          ("$type", `String "app.bsky.embed.record");
                          ("record", `Assoc [
                            ("uri", `String post_uri);
                            ("cid", `String post_cid);
                          ]);
                        ] in

                        (* If we have media, use recordWithMedia *)
                         let final_record =
                          if List.length blobs > 0 then
                            let video_blobs = List.filter (fun (_, _, mt, _, _) -> mt = Platform_types.Video) blobs in
                            let non_video_blobs = List.filter (fun (_, _, mt, _, _) -> mt <> Platform_types.Video) blobs in
                            let media_embed =
                              if List.length video_blobs = 1 && List.length non_video_blobs = 0 then
                                let (video_blob, alt_text_opt, _, _, _) = List.hd video_blobs in
                                let video_fields =
                                  match alt_text_opt with
                                  | Some alt when String.length alt > 0 ->
                                      [
                                        ("$type", `String "app.bsky.embed.video");
                                        ("video", video_blob);
                                        ("alt", `String alt);
                                      ]
                                  | _ ->
                                      [
                                        ("$type", `String "app.bsky.embed.video");
                                        ("video", video_blob);
                                      ]
                                in
                                `Assoc video_fields
                              else
                                let images_json = `List (List.map (fun (blob, alt_text_opt, _, w, h) ->
                                  let alt_text = match alt_text_opt with
                                    | Some alt when String.length alt > 0 -> alt
                                    | _ -> ""
                                  in
                                  let base_fields = [
                                    ("alt", `String alt_text);
                                    ("image", blob);
                                  ] in
                                  let fields_with_aspect = match w, h with
                                    | Some width, Some height ->
                                        base_fields @ [
                                          ("aspectRatio", `Assoc [
                                            ("width", `Int width);
                                            ("height", `Int height);
                                          ])
                                        ]
                                    | _ -> base_fields
                                  in
                                  `Assoc fields_with_aspect
                                ) non_video_blobs) in
                                `Assoc [
                                  ("$type", `String "app.bsky.embed.images");
                                  ("images", images_json);
                                ]
                            in
                            base_with_facets @ [
                              ("embed", `Assoc [
                                ("$type", `String "app.bsky.embed.recordWithMedia");
                                ("record", `Assoc [
                                  ("$type", `String "app.bsky.embed.record");
                                  ("record", `Assoc [
                                    ("uri", `String post_uri);
                                    ("cid", `String post_cid);
                                  ]);
                                ]);
                                ("media", media_embed);
                              ])
                            ]
                          else
                            base_with_facets @ [("embed", quote_embed)]
                        in
                        
                        let url = Printf.sprintf "%s/xrpc/com.atproto.repo.createRecord" pds_url in
                        let body = `Assoc [
                          ("repo", `String identifier);
                          ("collection", `String "app.bsky.feed.post");
                          ("record", `Assoc final_record);
                        ] in
                        let body_str = Yojson.Basic.to_string body in
                        let headers = [
                          ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
                          ("Content-Type", "application/json");
                        ] in
                        
                        Config.Http.post ~headers ~body:body_str url
                          (fun response ->
                            if response.status >= 200 && response.status < 300 then
                              try
                                let json = Yojson.Basic.from_string response.body in
                                let result_uri = json 
                                  |> Yojson.Basic.Util.member "uri" 
                                  |> Yojson.Basic.Util.to_string in
                                on_result (Error_types.Success result_uri)
                              with e ->
                                on_result (Error_types.Failure (Error_types.Internal_error 
                                  (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e))))
                            else
                              on_result (Error_types.Failure (parse_api_error 
                                ~status_code:response.status ~body:response.body)))
                          (fun err -> on_result (Error_types.Failure (Error_types.Network_error 
                            (Error_types.Connection_failed err)))))
                      (fun _ -> on_result (Error_types.Failure (Error_types.Internal_error 
                        "Facet extraction failed"))))
                  (fun err -> on_result (Error_types.Failure err)))
              (fun err -> on_result (Error_types.Failure (Error_types.Network_error 
                (Error_types.Connection_failed err)))))
          (fun err -> on_result (Error_types.Failure err))
  
  (** {1 Public API - Notifications} *)
  
  (** List notifications *)
  let list_notifications ~account_id ?limit ?cursor on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let params = [] in
        let params = match limit with
          | Some l -> ("limit", string_of_int l) :: params
          | None -> params
        in
        let params = match cursor with
          | Some c -> ("cursor", c) :: params
          | None -> params
        in
        let query_string = match params with
          | [] -> ""
          | _ -> "?" ^ (String.concat "&" (List.map (fun (k, v) -> 
              Printf.sprintf "%s=%s" k (Uri.pct_encode v)) params))
        in
        let url = Printf.sprintf "%s/xrpc/app.bsky.notification.listNotifications%s" 
          pds_url query_string in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              match parse_json_object_or_error ~context:"Failed to parse notifications" ~body:response.body with
              | Ok json ->
                  (match ensure_array_field ~json ~field:"notifications" ~context:"Failed to parse notifications" with
                   | Ok () -> on_result (Ok json)
                   | Error err -> on_result (Error err))
              | Error err -> on_result (Error err)
            else
              on_result (Error (parse_api_error 
                ~status_code:response.status ~body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Count unread notifications *)
  let count_unread_notifications ~account_id on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let url = Printf.sprintf "%s/xrpc/app.bsky.notification.getUnreadCount" pds_url in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let count = json 
                  |> Yojson.Basic.Util.member "count" 
                  |> Yojson.Basic.Util.to_int in
                on_result (Ok count)
              with e ->
                on_result (Error (Error_types.Internal_error 
                  (Printf.sprintf "Failed to parse unread count: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error 
                ~status_code:response.status ~body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Update seen notifications *)
  let update_seen_notifications ~account_id on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let now = Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 (Ptime_clock.now ()) in
        let url = Printf.sprintf "%s/xrpc/app.bsky.notification.updateSeen" pds_url in
        let body = `Assoc [("seenAt", `String now)] in
        let body_str = Yojson.Basic.to_string body in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
          ("Content-Type", "application/json");
        ] in
        
        Config.Http.post ~headers ~body:body_str url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_result (Ok ())
            else
              on_result (Error (parse_api_error 
                ~status_code:response.status ~body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** {1 Public API - Search} *)
  
  (** Search for actors *)
  let search_actors ~account_id ~query ?limit ?cursor on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let params = [("q", query)] in
        let params = match limit with
          | Some l -> ("limit", string_of_int l) :: params
          | None -> params
        in
        let params = match cursor with
          | Some c -> ("cursor", c) :: params
          | None -> params
        in
        let query_string = "?" ^ (String.concat "&" (List.map (fun (k, v) -> 
          Printf.sprintf "%s=%s" k (Uri.pct_encode v)) params)) in
        let url = Printf.sprintf "%s/xrpc/app.bsky.actor.searchActors%s" 
          pds_url query_string in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              match parse_json_object_or_error ~context:"Failed to parse search results" ~body:response.body with
              | Ok json ->
                  (match ensure_array_field ~json ~field:"actors" ~context:"Failed to parse search results" with
                   | Ok () -> on_result (Ok json)
                   | Error err -> on_result (Error err))
              | Error err -> on_result (Error err)
            else
              on_result (Error (parse_api_error 
                ~status_code:response.status ~body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Search for posts *)
  let search_posts ~account_id ~query ?limit ?cursor on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let params = [("q", query)] in
        let params = match limit with
          | Some l -> ("limit", string_of_int l) :: params
          | None -> params
        in
        let params = match cursor with
          | Some c -> ("cursor", c) :: params
          | None -> params
        in
        let query_string = "?" ^ (String.concat "&" (List.map (fun (k, v) -> 
          Printf.sprintf "%s=%s" k (Uri.pct_encode v)) params)) in
        let url = Printf.sprintf "%s/xrpc/app.bsky.feed.searchPosts%s" 
          pds_url query_string in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              match parse_json_object_or_error ~context:"Failed to parse search results" ~body:response.body with
              | Ok json ->
                  (match ensure_array_field ~json ~field:"posts" ~context:"Failed to parse search results" with
                   | Ok () -> on_result (Ok json)
                   | Error err -> on_result (Error err))
              | Error err -> on_result (Error err)
            else
              on_result (Error (parse_api_error 
                ~status_code:response.status ~body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** {1 Public API - Moderation} *)
  
  (** Mute an actor *)
  let mute_actor ~account_id ~actor on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let url = Printf.sprintf "%s/xrpc/app.bsky.graph.muteActor" pds_url in
        let body = `Assoc [("actor", `String actor)] in
        let body_str = Yojson.Basic.to_string body in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
          ("Content-Type", "application/json");
        ] in
        
        Config.Http.post ~headers ~body:body_str url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_result (Ok ())
            else
              on_result (Error (parse_api_error 
                ~status_code:response.status ~body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Unmute an actor *)
  let unmute_actor ~account_id ~actor on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let url = Printf.sprintf "%s/xrpc/app.bsky.graph.unmuteActor" pds_url in
        let body = `Assoc [("actor", `String actor)] in
        let body_str = Yojson.Basic.to_string body in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
          ("Content-Type", "application/json");
        ] in
        
        Config.Http.post ~headers ~body:body_str url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_result (Ok ())
            else
              on_result (Error (parse_api_error 
                ~status_code:response.status ~body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Block an actor *)
  let block_actor ~account_id ~actor on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        Config.get_credentials ~account_id
          (fun creds ->
            let identifier = creds.access_token in
            let now = Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 (Ptime_clock.now ()) in
            
            let url = Printf.sprintf "%s/xrpc/com.atproto.repo.createRecord" pds_url in
            let body = `Assoc [
              ("repo", `String identifier);
              ("collection", `String "app.bsky.graph.block");
              ("record", `Assoc [
                ("$type", `String "app.bsky.graph.block");
                ("subject", `String actor);
                ("createdAt", `String now);
              ]);
            ] in
            let body_str = Yojson.Basic.to_string body in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
              ("Content-Type", "application/json");
            ] in
            
            Config.Http.post ~headers ~body:body_str url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  try
                    let json = Yojson.Basic.from_string response.body in
                    let block_uri = json 
                      |> Yojson.Basic.Util.member "uri" 
                      |> Yojson.Basic.Util.to_string in
                    on_result (Ok block_uri)
                  with e ->
                    on_result (Error (Error_types.Internal_error 
                      (Printf.sprintf "Failed to parse block response: %s" (Printexc.to_string e))))
                else
                  on_result (Error (parse_api_error 
                    ~status_code:response.status ~body:response.body)))
              (fun err -> on_result (Error (Error_types.Network_error 
                (Error_types.Connection_failed err)))))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Unblock an actor *)
  let unblock_actor ~account_id ~block_uri on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        Config.get_credentials ~account_id
          (fun creds ->
            let identifier = creds.access_token in
            let uri_parts = String.split_on_char '/' block_uri in
            let rkey = List.nth uri_parts (List.length uri_parts - 1) in
            
            let url = Printf.sprintf "%s/xrpc/com.atproto.repo.deleteRecord" pds_url in
            let body = `Assoc [
              ("repo", `String identifier);
              ("collection", `String "app.bsky.graph.block");
              ("rkey", `String rkey);
            ] in
            let body_str = Yojson.Basic.to_string body in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
              ("Content-Type", "application/json");
            ] in
            
            Config.Http.post ~headers ~body:body_str url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  on_result (Ok ())
                else
                  on_result (Error (parse_api_error 
                    ~status_code:response.status ~body:response.body)))
              (fun err -> on_result (Error (Error_types.Network_error 
                (Error_types.Connection_failed err)))))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** {1 Public API - Feed Operations} *)
  
  (** Get author feed *)
  let get_author_feed ~account_id ~actor ?limit ?cursor on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let params = [("actor", actor)] in
        let params = match limit with
          | Some l -> ("limit", string_of_int l) :: params
          | None -> params
        in
        let params = match cursor with
          | Some c -> ("cursor", c) :: params
          | None -> params
        in
        let query_string = "?" ^ (String.concat "&" (List.map (fun (k, v) -> 
          Printf.sprintf "%s=%s" k (Uri.pct_encode v)) params)) in
        let url = Printf.sprintf "%s/xrpc/app.bsky.feed.getAuthorFeed%s" 
          pds_url query_string in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              match parse_json_object_or_error ~context:"Failed to parse author feed" ~body:response.body with
              | Ok json ->
                  (match ensure_array_field ~json ~field:"feed" ~context:"Failed to parse author feed" with
                   | Ok () -> on_result (Ok json)
                   | Error err -> on_result (Error err))
              | Error err -> on_result (Error err)
            else
              on_result (Error (parse_api_error 
                ~status_code:response.status ~body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Get likes for a post *)
  let get_likes ~account_id ~post_uri ?limit ?cursor on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let params = [("uri", post_uri)] in
        let params = match limit with
          | Some l -> ("limit", string_of_int l) :: params
          | None -> params
        in
        let params = match cursor with
          | Some c -> ("cursor", c) :: params
          | None -> params
        in
        let query_string = "?" ^ (String.concat "&" (List.map (fun (k, v) -> 
          Printf.sprintf "%s=%s" k (Uri.pct_encode v)) params)) in
        let url = Printf.sprintf "%s/xrpc/app.bsky.feed.getLikes%s" 
          pds_url query_string in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              match parse_json_object_or_error ~context:"Failed to parse likes" ~body:response.body with
              | Ok json ->
                  (match ensure_array_field ~json ~field:"likes" ~context:"Failed to parse likes" with
                   | Ok () -> on_result (Ok json)
                   | Error err -> on_result (Error err))
              | Error err -> on_result (Error err)
            else
              on_result (Error (parse_api_error 
                ~status_code:response.status ~body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Get reposts for a post *)
  let get_reposted_by ~account_id ~post_uri ?limit ?cursor on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let params = [("uri", post_uri)] in
        let params = match limit with
          | Some l -> ("limit", string_of_int l) :: params
          | None -> params
        in
        let params = match cursor with
          | Some c -> ("cursor", c) :: params
          | None -> params
        in
        let query_string = "?" ^ (String.concat "&" (List.map (fun (k, v) -> 
          Printf.sprintf "%s=%s" k (Uri.pct_encode v)) params)) in
        let url = Printf.sprintf "%s/xrpc/app.bsky.feed.getRepostedBy%s" 
          pds_url query_string in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              match parse_json_object_or_error ~context:"Failed to parse reposts" ~body:response.body with
              | Ok json ->
                  (match ensure_array_field ~json ~field:"repostedBy" ~context:"Failed to parse reposts" with
                   | Ok () -> on_result (Ok json)
                   | Error err -> on_result (Error err))
              | Error err -> on_result (Error err)
            else
              on_result (Error (parse_api_error 
                ~status_code:response.status ~body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Get followers *)
  let get_followers ~account_id ~actor ?limit ?cursor on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let params = [("actor", actor)] in
        let params = match limit with
          | Some l -> ("limit", string_of_int l) :: params
          | None -> params
        in
        let params = match cursor with
          | Some c -> ("cursor", c) :: params
          | None -> params
        in
        let query_string = "?" ^ (String.concat "&" (List.map (fun (k, v) -> 
          Printf.sprintf "%s=%s" k (Uri.pct_encode v)) params)) in
        let url = Printf.sprintf "%s/xrpc/app.bsky.graph.getFollowers%s" 
          pds_url query_string in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              match parse_json_object_or_error ~context:"Failed to parse followers" ~body:response.body with
              | Ok json ->
                  (match ensure_array_field ~json ~field:"followers" ~context:"Failed to parse followers" with
                   | Ok () -> on_result (Ok json)
                   | Error err -> on_result (Error err))
              | Error err -> on_result (Error err)
            else
              on_result (Error (parse_api_error 
                ~status_code:response.status ~body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Get follows *)
  let get_follows ~account_id ~actor ?limit ?cursor on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let params = [("actor", actor)] in
        let params = match limit with
          | Some l -> ("limit", string_of_int l) :: params
          | None -> params
        in
        let params = match cursor with
          | Some c -> ("cursor", c) :: params
          | None -> params
        in
        let query_string = "?" ^ (String.concat "&" (List.map (fun (k, v) -> 
          Printf.sprintf "%s=%s" k (Uri.pct_encode v)) params)) in
        let url = Printf.sprintf "%s/xrpc/app.bsky.graph.getFollows%s" 
          pds_url query_string in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              match parse_json_object_or_error ~context:"Failed to parse follows" ~body:response.body with
              | Ok json ->
                  (match ensure_array_field ~json ~field:"follows" ~context:"Failed to parse follows" with
                   | Ok () -> on_result (Ok json)
                   | Error err -> on_result (Error err))
              | Error err -> on_result (Error err)
            else
              on_result (Error (parse_api_error 
                ~status_code:response.status ~body:response.body)))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
end
