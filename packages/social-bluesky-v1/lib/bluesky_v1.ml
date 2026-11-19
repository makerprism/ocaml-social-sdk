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
  
  (** The credentials to store after {!Make.create_session}.

      [access_token] is the account DID: it is public, stable across handle
      changes, and valid wherever atproto takes an identifier (the [repo] of a
      record, a [createSession] login). [refresh_token] is the session's
      refresh JWT, never the app password, and [auth_type] is [Bearer].
      [ensure_valid_token] writes this same shape after every refresh and
      when it rotates a legacy app-password row, so a freshly connected
      account takes the refresh-JWT path on its first call. *)
  let session_to_refresh_credentials (session : session) : credentials =
    {
      access_token = session.did;
      refresh_token = Some session.refresh_jwt;
      expires_at = None;
      auth_type = Bearer;
      scope = None;  (* Bluesky app passwords have no OAuth scope concept *)
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
    
    (** See {!Auth.session_to_refresh_credentials}. *)
    let session_to_refresh_credentials = session_to_refresh_credentials

    (** Deprecated: stores the app password as [refresh_token], so anyone
        with read access to the credential store can log in as the user
        until they revoke the app password. Use
        {!session_to_refresh_credentials}, which stores the refresh JWT. Rows
        written by this function still work: [ensure_valid_token] exchanges
        the app password for a session on first use and rotates the row. *)
    let session_to_credentials ~identifier ~app_password : credentials =
      {
        access_token = identifier;
        refresh_token = Some app_password;
        expires_at = None;  (* App passwords don't expire *)
        auth_type = App_password;
        scope = None;  (* Bluesky app passwords have no OAuth scope concept *)
      }
  end
end

(** Typed [app.bsky] views (post, thread, feed item, notification) and pure
    helpers, as [Social_bluesky_v1.Views]. *)
module Views = Bluesky_views

(** A strong reference to a record: its AT URI and the CID of the exact
    version referenced. Replies must carry both. Defined in
    {!Bluesky_views}. *)
type strong_ref = Bluesky_views.strong_ref = { uri : string; cid : string }

(** Where a reply attaches. [parent] is the post replied to directly; [root]
    is the first post of its thread (the parent's own root, or the parent
    itself when it is a top-level post). *)
type reply_ref = Bluesky_views.reply_ref = { root : strong_ref; parent : strong_ref }

(** Parse the ["uri|cid"] id that {!Make_with_options.post_single} and
    {!Make_with_options.post_thread} return. *)
let strong_ref_of_post_id post_id =
  match String.split_on_char '|' post_id with
  | [ uri; cid ] when uri <> "" && cid <> "" -> Some { uri; cid }
  | _ -> None

(** The ["uri|cid"] id format the posting functions return. *)
let post_id_of_strong_ref { uri; cid } = uri ^ "|" ^ cid

(** Configuration module type for Bluesky provider *)
module type CONFIG = sig
  module Http : HTTP_CLIENT
  
  val get_env : string -> string option
  val get_credentials : account_id:string -> (credentials -> unit) -> (string -> unit) -> unit
  val update_credentials : account_id:string -> credentials:credentials -> (unit -> unit) -> (string -> unit) -> unit
  val encrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val decrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val update_health_status : account_id:string -> status:string -> error_message:string option -> (unit -> unit) -> (string -> unit) -> unit
  val resize_image : data:string -> mime_type:string -> max_bytes:int -> (string option -> unit) -> unit
end

(** The [#atproto_pds] service endpoint of a DID document, as returned in
    the [didDoc] of [createSession] / [getSession]. *)
let pds_endpoint_of_did_doc did_doc =
  let field name = function
    | `Assoc fields -> List.assoc_opt name fields
    | _ -> None
  in
  match field "service" did_doc with
  | Some (`List services) ->
      List.find_map
        (fun service ->
          match field "id" service, field "serviceEndpoint" service with
          | Some (`String id), Some (`String endpoint)
            when String.ends_with ~suffix:"#atproto_pds" id ->
              Some endpoint
          | _ -> None)
        services
  | _ -> None

(** What an [app.bsky.video] job status says. *)
type video_job =
  | Video_blob of Yojson.Basic.t  (** processed; the blob ref to embed *)
  | Video_failed of string  (** [JOB_STATE_FAILED], with the reason *)
  | Video_completed_without_blob
  | Video_pending of string option  (** still processing; the job id if given *)

(** Read a [jobStatus] response, or a bare [{blob}] / error body. A blob
    wins whatever the state or HTTP status: an already-processed video is
    answered with an error ([already_exists]) that still carries the
    earlier blob. *)
let video_job_of_json json =
  let field name = function
    | `Assoc fields ->
        (match List.assoc_opt name fields with
         | Some `Null | None -> None
         | Some v -> Some v)
    | _ -> None
  in
  let string_field name obj =
    match field name obj with
    | Some (`String v) -> Some v
    | _ -> None
  in
  let job = Option.value (field "jobStatus" json) ~default:json in
  match field "blob" json, field "blob" job with
  | Some blob, _ | None, Some blob -> Video_blob blob
  | None, None ->
      let job_id =
        match string_field "jobId" job with
        | Some id -> Some id
        | None -> string_field "jobId" json
      in
      (match string_field "state" job with
       | Some "JOB_STATE_FAILED" ->
           let reason =
             [ string_field "failureCode" job; string_field "error" job;
               string_field "message" job ]
             |> List.filter_map Fun.id |> String.concat ": "
           in
           Video_failed (if reason = "" then "no reason given" else reason)
       | Some "JOB_STATE_COMPLETED" ->
           (match job_id with
            | Some _ -> Video_pending job_id
            | None -> Video_completed_without_blob)
       | Some _ | None -> Video_pending job_id)

(** Check if blob data looks like text/XML/HTML rather than binary media.
    Bluesky sniffs content and rejects non-media regardless of Content-Type header. *)
let blob_looks_like_text blob_data =
  let trimmed =
    let len = String.length blob_data in
    let i = ref 0 in
    while !i < len && (blob_data.[!i] = ' ' || blob_data.[!i] = '\t'
                       || blob_data.[!i] = '\n' || blob_data.[!i] = '\r') do
      incr i
    done;
    if !i < len then String.sub blob_data !i (min 10 (len - !i)) else ""
  in
  String.length trimmed > 0
  && (trimmed.[0] = '<'
      || String.starts_with ~prefix:"{" trimmed
      || String.starts_with ~prefix:"[" trimmed)

(** The atproto error [name] from an XRPC error body, e.g. "ExpiredToken".

    atproto puts the machine-readable name in [error] and the human-readable
    text in [message]. {!parse_xrpc_error} keeps only the text, so callers
    that need to tell one 400 from another read the name with this. A body
    that is not an XRPC error envelope (an HTML page from a proxy, an empty
    body) yields [None]. *)
let atproto_error_name body =
  try
    match Yojson.Basic.from_string body with
    | `Assoc fields ->
        (match List.assoc_opt "error" fields with
         | Some (`String name) -> Some name
         | Some _ | None -> None)
    | _ -> None
  with _ -> None

(** Parse an XRPC error response into an {!Error_types.error}.

    atproto answers most failures with HTTP 400 and puts the reason in the
    error name, so the name decides first:
    - [ExpiredToken] / [InvalidToken]: the access JWT is dead, an
      [Auth_error] (consumers refresh or ask the user to reconnect).
    - [NotFound] / [RecordNotFound]: the post, record or thread is gone,
      [Resource_not_found] carrying the server's message.
    Otherwise 401 is an [Auth_error], 429 [Rate_limited], and anything else
    an [Api_error] with the server's message. *)
let parse_xrpc_error ~status_code ~body =
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
  match atproto_error_name body with
  | Some "ExpiredToken" -> Error_types.Auth_error Error_types.Token_expired
  | Some "InvalidToken" -> Error_types.Auth_error Error_types.Token_invalid
  | Some ("NotFound" | "RecordNotFound") ->
      Error_types.Resource_not_found (parse_error_message ())
  | Some _ | None ->
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

(** HTTP-aware XRPC error mapping; [now] allows deterministic reset delays. *)
let parse_xrpc_response_error ?now (response : response) =
  if response.status = 429 then
    let reset = List.find_map (fun (name, value) ->
      if String.equal (String.lowercase_ascii name) "ratelimit-reset" then
        int_of_string_opt (String.trim value) else None) response.headers in
    let retry_after_seconds = Option.map (fun reset ->
      let now = match now with Some now -> now | None -> Unix.time () in
      max 1 (reset - int_of_float now)) reset in
    Error_types.make_rate_limited ?retry_after_seconds ()
  else parse_xrpc_error ~status_code:response.status ~body:response.body

(** A non-blocking wait in the consumer's runtime, e.g. with Lwt:
    [fun ~seconds k _ -> Lwt.async (fun () -> Lwt.map k (Lwt_unix.sleep seconds))]. *)
type sleep = seconds:float -> (unit -> unit) -> (string -> unit) -> unit

(** Typed XRPC reads that need no account store.

    Each call takes the [service_url] to send it to and, where the lexicon
    allows unauthenticated use, an optional [access_jwt]:
    - with a session, the user's PDS (it proxies [app.bsky.*] to the
      AppView and applies the viewer's blocks and mutes);
    - without one, {!public_api_url}, Bluesky's cached public AppView.
    {{:https://docs.bsky.app/docs/advanced-guides/api-directory}}

    {!Make_with_options} wraps the same calls for a stored account.

    Errors are {!parse_xrpc_error}: [NotFound] is [Resource_not_found],
    [ExpiredToken] / [InvalidToken] are [Auth_error]. A transport failure is
    [Network_error]; a 2xx body that does not match the lexicon is
    [Internal_error]. *)
module Reads = struct
  (** [https://public.api.bsky.app], the public AppView host for
      unauthenticated [app.bsky.*] reads. *)
  let public_api_url = "https://public.api.bsky.app"

  (** Most [uris] one [app.bsky.feed.getPosts] call accepts. *)
  let max_posts_per_request = 25

  let trim_trailing_slash url =
    let len = String.length url in
    if len > 0 && url.[len - 1] = '/' then String.sub url 0 (len - 1) else url

  (* XRPC array params repeat the key ([uris=a&uris=b]). Uri.with_query
     would join a value list with commas instead. *)
  let xrpc_url ~service_url nsid params =
    let query = List.concat_map (fun (key, values) -> List.map (fun v -> (key, [ v ])) values) params in
    Uri.with_query (Uri.of_string (trim_trailing_slash service_url ^ "/xrpc/" ^ nsid)) query
    |> Uri.to_string

  let opt_param key to_string = function Some value -> [ (key, [ to_string value ]) ] | None -> []

  let bearer = function
    | Some token -> [ ("Authorization", "Bearer " ^ token) ]
    | None -> []

  module Make (Http : HTTP_CLIENT) = struct
    (* GET [url]; on 2xx parse the body with [parse]. *)
    let get_view ~what ~headers ~error_of url parse on_result =
      Http.get ~headers url
        (fun (response : response) ->
          if response.status >= 200 && response.status < 300 then
            match Yojson.Basic.from_string response.body with
            | json -> (
                match parse json with
                | Stdlib.Ok value -> on_result (Stdlib.Ok value)
                | Stdlib.Error msg ->
                    on_result
                      (Stdlib.Error
                         (Error_types.Internal_error
                            (Printf.sprintf "Unexpected %s response: %s" what msg))))
            | exception Yojson.Json_error msg ->
                on_result
                  (Stdlib.Error
                     (Error_types.Internal_error
                        (Printf.sprintf "Unexpected %s response: %s" what msg)))
          else on_result (Stdlib.Error (error_of response)))
        (fun err ->
          on_result
            (Stdlib.Error (Error_types.Network_error (Error_types.Connection_failed err))))

    let xrpc_error (response : response) =
      parse_xrpc_response_error response

    (** [app.bsky.feed.getPostThread]: the post, up to [parent_height]
        ancestors and [depth] levels of replies (server defaults 80 and 6,
        both at most 1000). Pass a larger [depth] to crawl a whole
        conversation; the default cuts long threads off. A deleted root is
        [Resource_not_found] ([NotFound]); deleted or blocked posts inside
        the tree are [Not_found_post] / [Blocked_post] nodes.
        {{:https://docs.bsky.app/docs/api/app-bsky-feed-get-post-thread}} *)
    let get_post_thread ~service_url ?access_jwt ~post_uri ?depth ?parent_height on_result =
      let url =
        xrpc_url ~service_url "app.bsky.feed.getPostThread"
          ([ ("uri", [ post_uri ]) ]
          @ opt_param "depth" string_of_int depth
          @ opt_param "parentHeight" string_of_int parent_height)
      in
      get_view ~what:"getPostThread" ~headers:(bearer access_jwt) ~error_of:xrpc_error url
        Bluesky_views.post_thread_of_json on_result

    (** [app.bsky.feed.getAuthorFeed], one page: the actor's posts and
        reposts, newest first. [limit] 1-100 (server default 50); [filter]
        defaults to [Posts_with_replies] on the server; [include_pins] adds
        the pinned post with [Reason_pin]. [BlockedActor] /
        [BlockedByActor] are [Api_error].
        {{:https://docs.bsky.app/docs/api/app-bsky-feed-get-author-feed}} *)
    let get_author_feed ~service_url ?access_jwt ~actor ?limit ?cursor ?filter
        ?include_pins on_result =
      let url =
        xrpc_url ~service_url "app.bsky.feed.getAuthorFeed"
          ([ ("actor", [ actor ]) ]
          @ opt_param "limit" string_of_int limit
          @ opt_param "cursor" Fun.id cursor
          @ opt_param "filter" Bluesky_views.author_feed_filter_to_string filter
          @ opt_param "includePins" string_of_bool include_pins)
      in
      get_view ~what:"getAuthorFeed" ~headers:(bearer access_jwt) ~error_of:xrpc_error url
        Bluesky_views.author_feed_page_of_json on_result

    let rec get_posts_in_chunks ~service_url ~access_jwt acc uris on_result =
      match uris with
      | [] -> on_result (Stdlib.Ok (List.concat (List.rev acc)))
      | _ ->
          let chunk = List.filteri (fun i _ -> i < max_posts_per_request) uris in
          let rest = List.filteri (fun i _ -> i >= max_posts_per_request) uris in
          get_view ~what:"getPosts" ~headers:(bearer access_jwt) ~error_of:xrpc_error
            (xrpc_url ~service_url "app.bsky.feed.getPosts" [ ("uris", chunk) ])
            Bluesky_views.posts_of_json
            (function
              | Stdlib.Ok posts ->
                  get_posts_in_chunks ~service_url ~access_jwt (posts :: acc) rest on_result
              | Stdlib.Error _ as error -> on_result error)

    (** [app.bsky.feed.getPosts]: post views for [post_uris], with counts.
        The lexicon caps one call at 25 URIs, so longer lists are sent as
        consecutive calls of 25 and the results concatenated; the first
        failing call fails the whole read. Posts that no longer exist are
        left out (the result can be shorter than [post_uris]), and order is
        not guaranteed: match on [uri]. An empty list makes no call.
        {{:https://docs.bsky.app/docs/api/app-bsky-feed-get-posts}} *)
    let get_posts ~service_url ?access_jwt ~post_uris on_result =
      get_posts_in_chunks ~service_url ~access_jwt [] post_uris on_result

    (** [app.bsky.actor.getProfile]: the actor's profile with follower,
        follows and post counts. [actor] is a handle or a DID. Needs no
        session ({!public_api_url} answers); with one, the viewer's state
        is included.
        {{:https://docs.bsky.app/docs/api/app-bsky-actor-get-profile}} *)
    let get_profile ~service_url ?access_jwt ~actor on_result =
      get_view ~what:"getProfile" ~headers:(bearer access_jwt) ~error_of:xrpc_error
        (xrpc_url ~service_url "app.bsky.actor.getProfile" [ ("actor", [ actor ]) ])
        Bluesky_views.profile_detailed_of_json on_result

    (** [app.bsky.notification.listNotifications], one page, newest first.
        Requires a session. [reasons] keeps only those reasons; [limit]
        1-100 (server default 50). Pass the returned [cursor] for the next
        (older) page.
        {{:https://docs.bsky.app/docs/api/app-bsky-notification-list-notifications}} *)
    let list_notifications ~service_url ~access_jwt ?limit ?cursor ?(reasons = []) on_result =
      let url =
        xrpc_url ~service_url "app.bsky.notification.listNotifications"
          (opt_param "limit" string_of_int limit
          @ opt_param "cursor" Fun.id cursor
          @ [ ("reasons", List.map Bluesky_views.notification_reason_to_string reasons) ])
      in
      get_view ~what:"listNotifications" ~headers:(bearer (Some access_jwt))
        ~error_of:xrpc_error url Bluesky_views.notifications_page_of_json on_result

    (* The lexicon names [HandleNotFound]; PDS and AppView deployments
       answer an unknown handle with 400 [InvalidRequest] "Unable to resolve
       handle" instead. Both mean the handle does not exist. *)
    let resolve_handle_error (response : response) =
      let not_found =
        match atproto_error_name response.body with
        | Some "HandleNotFound" -> true
        | Some "InvalidRequest" ->
            response.status = 400
            && (match Yojson.Basic.from_string response.body with
               | `Assoc fields -> (
                   match List.assoc_opt "message" fields with
                   | Some (`String message) ->
                       String.equal (String.lowercase_ascii message) "unable to resolve handle"
                   | Some _ | None -> false)
               | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> false
               | exception Yojson.Json_error _ -> false)
        | Some _ | None -> false
      in
      if not_found then Error_types.Resource_not_found "Unable to resolve handle"
      else xrpc_error response

    (** [com.atproto.identity.resolveHandle]: the DID of [handle] (no
        leading [@]). Needs no session; any PDS or {!public_api_url}
        answers. A handle that does not resolve ([HandleNotFound], or the
        400 "Unable to resolve handle" deployed servers send) is
        [Resource_not_found]. The existing
        {!Make_with_options.resolve_handle} keeps reporting that case as
        [Api_error].
        {{:https://docs.bsky.app/docs/api/com-atproto-identity-resolve-handle}} *)
    let resolve_handle ~service_url ~handle on_result =
      get_view ~what:"resolveHandle" ~headers:[] ~error_of:resolve_handle_error
        (xrpc_url ~service_url "com.atproto.identity.resolveHandle" [ ("handle", [ handle ]) ])
        Bluesky_views.did_of_resolve_handle_json on_result
  end
end

(** Options that are not account- or storage-related.

    Kept out of {!CONFIG} so adding one never breaks an existing configuration:
    define yours with [include Default_options] and override what you need.

    [service_url] is the base URL of the PDS (or entryway) every XRPC call goes
    to, e.g. ["https://bsky.social"] or a self-hosted PDS. It is a function so
    a consumer can read it from configuration loaded after the functor is
    applied. A trailing slash is ignored. *)
module type OPTIONS = sig
  val service_url : unit -> string

  (** Wait [seconds], then continue (or report why the wait failed). Polling
      Bluesky's video service needs it; the SDK never blocks a thread, so it
      cannot wait without one. [None] uploads video with plain [uploadBlob]
      (see {!Make_with_options.upload_blob}). *)
  val sleep : sleep option
end

(** Defaults: the Bluesky-hosted entryway, no sleep. *)
module Default_options = struct
  let service_url () = Auth.Metadata.default_pds_url
  let sleep : sleep option = None
end

(** Make functor with explicit {!OPTIONS} (e.g. a self-hosted PDS). *)
module type COORDINATED_CONFIG = sig
  include CONFIG

  val with_refresh_lock :
    account_id:string ->
    ((Social_core.credentials -> unit) -> (Error_types.error -> unit) -> unit) ->
    (Social_core.credentials -> unit) ->
    (Error_types.error -> unit) -> unit
end

module Make_internal (Config : COORDINATED_CONFIG) (Options : OPTIONS)
    (Persistence : sig val required : bool end) = struct
  (** Base URL every XRPC call goes to, without a trailing slash. *)
  let service_url () =
    let url = String.trim (Options.service_url ()) in
    let len = String.length url in
    if len > 0 && url.[len - 1] = '/' then String.sub url 0 (len - 1) else url

  (** Deprecated: the default entryway, kept for source compatibility. It
      never reflects {!OPTIONS}; read {!service_url} instead. *)
  let pds_url = Auth.Metadata.default_pds_url
  
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
  
  type session_commit = Pending | Committed

  (* Each acquisition owns its commit marker. A lock implementation may start
     the next waiter before delivering completion, so a cached pending session
     must remain unusable even after the lock has been released. *)
  let session_cache : (string, string * string * float * session_commit ref) Hashtbl.t = Hashtbl.create 16

  (** Session lifetime in seconds (matches Auth.Metadata.access_jwt_lifetime_seconds minus margin) *)
  let session_max_age_seconds = 240.0  (* 4 minutes, conservative vs 5-min expiry *)

  (** Clear cached session for an account *)
  let invalidate_session ~account_id =
    Hashtbl.remove session_cache account_id

  module Http : HTTP_CLIENT = struct
    let observe headers on_success (response : response) =
      (match atproto_error_name response.body with
       | Some ("ExpiredToken" | "InvalidToken") ->
           let authorization = List.find_map (fun (name, value) ->
             if String.equal (String.lowercase_ascii name) "authorization" then Some value
             else None) headers in
           let accounts = Hashtbl.fold (fun account_id (_, token, _, _) accounts ->
             if authorization = Some ("Bearer " ^ token) then account_id :: accounts
             else accounts) session_cache [] in
           List.iter (fun account_id -> invalidate_session ~account_id) accounts
       | Some _ | None -> ());
      on_success response

    let get ?(headers = []) url on_success on_error =
      Config.Http.get ~headers url (observe headers on_success) on_error
    let post ?(headers = []) ?body url on_success on_error =
      Config.Http.post ~headers ?body url (observe headers on_success) on_error
    let put ?(headers = []) ?body url on_success on_error =
      Config.Http.put ~headers ?body url (observe headers on_success) on_error
    let delete ?(headers = []) url on_success on_error =
      Config.Http.delete ~headers url (observe headers on_success) on_error
    let post_multipart ?(headers = []) ~parts url on_success on_error =
      Config.Http.post_multipart ~headers ~parts url (observe headers on_success) on_error
  end

  (** {1 Internal Helpers} *)

  let parse_ipv4_host host =
    let octets = String.split_on_char '.' host in
    match octets with
    | [a; b; c; d] ->
        (try
           let ai = int_of_string a in
           let bi = int_of_string b in
           let ci = int_of_string c in
           let di = int_of_string d in
           let in_range x = x >= 0 && x <= 255 in
           if in_range ai && in_range bi && in_range ci && in_range di then
             Some (ai, bi, ci, di)
           else
             None
         with _ -> None)
    | _ -> None

  let is_safe_remote_media_url url =
    let uri = Uri.of_string (String.trim url) in
    match Uri.scheme uri, Uri.host uri with
    | Some ("http" | "https"), Some host ->
        Uri.userinfo uri = None && Social_public_host.is_public_host host
    | (Some _ | None), (Some _ | None) -> false

  let atproto_error_name = atproto_error_name

  (** {!parse_xrpc_error}, kept under its original name. *)
  let parse_api_error = parse_xrpc_error

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
      (service_url ()) (Uri.pct_encode handle) in
    Http.get url
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
          on_result (Error (parse_xrpc_response_error response)))
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
    let url = Printf.sprintf "%s/xrpc/com.atproto.server.createSession" (service_url ()) in
    let body = `Assoc [
      ("identifier", `String identifier);
      ("password", `String password);
    ] in
    let body_str = Yojson.Basic.to_string body in
    let headers = [("Content-Type", "application/json")] in
    
    Http.post ~headers ~body:body_str url
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
  
  (** Video service host (uploadVideo, getJobStatus). *)
  let video_upload_host = "https://video.bsky.app"

  (** Seconds between getJobStatus polls. Bluesky's own example polls every
      second; processing a short clip takes several seconds to a minute. *)
  let video_job_poll_interval_seconds = 2.0

  (** Polls before giving up: 150 x 2 s = 5 minutes of processing. *)
  let video_job_max_polls = 150

  (** The PDS that hosts the session's repo, without a trailing slash.

      A Bluesky-hosted account logs in through the [bsky.social] entryway but
      lives on a PDS shard; video service auth must name that shard. Read from
      the [didDoc] of [com.atproto.server.getSession]. Falls back to
      {!service_url} (which already is the PDS for a self-hosted account) when
      the session has no DID document or its endpoint is not a public https
      URL: the endpoint is user-controlled and we send the access JWT there. *)
  let resolve_pds_endpoint ~access_jwt on_done =
    let fallback () = on_done (service_url ()) in
    let url = Printf.sprintf "%s/xrpc/com.atproto.server.getSession" (service_url ()) in
    Http.get ~headers:[("Authorization", Printf.sprintf "Bearer %s" access_jwt)] url
      (fun response ->
        let did_doc =
          if response.status >= 200 && response.status < 300 then
            (try
               match Yojson.Basic.from_string response.body with
               | `Assoc fields -> List.assoc_opt "didDoc" fields
               | _ -> None
             with Yojson.Json_error _ -> None)
          else None
        in
        match Option.bind did_doc pds_endpoint_of_did_doc with
        | Some endpoint
          when String.starts_with ~prefix:"https://" endpoint
               && is_safe_remote_media_url endpoint ->
            let len = String.length endpoint in
            on_done
              (if len > 0 && endpoint.[len - 1] = '/' then String.sub endpoint 0 (len - 1)
               else endpoint)
        | Some _ | None -> fallback ())
      (fun _ -> fallback ())

  (** Get a service auth token for video upload

      Calls com.atproto.server.getServiceAuth with the user's PDS DID
      as the audience and the uploadBlob lexicon method.

      Per the AT Protocol docs, [aud] is the DID of the user's PDS
      ([did:web:<pds host>]), NOT the video service DID: the video service
      uses the token to store the processed blob on that PDS.

      @param pds_endpoint The account's PDS (see {!resolve_pds_endpoint});
             the request goes there and [aud] names its host. Default:
             {!service_url}.
      @param access_jwt Current session JWT for authenticating the request
      @param did The user's DID (unused; the audience comes from the PDS)
      @param on_success Receives the service auth token
      @param on_error Receives error message
  *)
  let get_service_auth ?pds_endpoint ~access_jwt ~did:_ on_success on_error =
    let endpoint = Option.value pds_endpoint ~default:(service_url ()) in
    let pds_host =
      match Uri.host (Uri.of_string endpoint) with
      | Some h -> h
      | None -> "bsky.social"
    in
    let aud = "did:web:" ^ pds_host in
    let lxm = "com.atproto.repo.uploadBlob" in
    let exp_seconds = int_of_float (Unix.gettimeofday () +. 1800.0) in (* 30 minutes *)
    let url = Printf.sprintf "%s/xrpc/com.atproto.server.getServiceAuth?aud=%s&lxm=%s&exp=%d"
      endpoint (Uri.pct_encode aud) (Uri.pct_encode lxm) exp_seconds in
    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
    ] in
    Http.get ~headers url
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

      Images (and video, when {!OPTIONS.sleep} is [None]) go to the PDS via
      [com.atproto.repo.uploadBlob]; Bluesky processes such a video only once
      the post is in the firehose, so it shows as missing for a few seconds.

      With a [sleep], video takes Bluesky's recommended path: a service token
      for the account's PDS ({!resolve_pds_endpoint}, {!get_service_auth}),
      [app.bsky.video.uploadVideo] on {!video_upload_host}, then
      [getJobStatus] every {!video_job_poll_interval_seconds} until the
      processed blob is ready (at most {!video_job_max_polls} polls). It
      falls back to [uploadBlob] when no service token can be had or the
      video service refuses the method or the token (401, 404, 405, 501,
      [MethodNotFound]); a rejected video or a failed job is an error.

      @param access_jwt Session JWT for authentication
      @param did The user's DID; required for the video service path
      @param blob_data Raw binary data
      @param mime_type MIME type of the data
      @param alt_text Optional alt text for accessibility
      @param on_success Receives (blob_json, alt_text)
      @param on_error Receives error message
  *)
  let upload_blob ~access_jwt ?did ~blob_data ~mime_type ~alt_text on_success on_error =
    (* Guard: reject content that is clearly text/XML/HTML/JSON, not binary media.
       CDN error pages and expired-URL responses often return XML/HTML with an
       image/* Content-Type header; Bluesky will sniff the content and reject it. *)
    if blob_looks_like_text blob_data then
      on_error (Printf.sprintf
        "Blob content appears to be text/markup, not binary media (declared MIME: %s). \
         The media URL likely returned an error page instead of an image." mime_type)
    else
    let parse_json body =
      try Some (Yojson.Basic.from_string body) with Yojson.Json_error _ -> None
    in
    let upload_via_blob () =
      let url = Printf.sprintf "%s/xrpc/com.atproto.repo.uploadBlob" (service_url ()) in
      let headers = [
        ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
        ("Content-Type", mime_type);
      ] in
      Http.post ~headers ~body:blob_data url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            match Option.map video_job_of_json (parse_json response.body) with
            | Some (Video_blob blob) -> on_success (blob, alt_text)
            | Some (Video_failed _ | Video_completed_without_blob | Video_pending _) ->
                on_error "Blob upload response missing blob field"
            | None -> on_error "Failed to parse blob response"
          else
            on_error (Printf.sprintf "Bluesky blob upload error (%d): %s" response.status response.body))
        on_error
    in
    (* The job id is the capability here: Bluesky's own client polls
       getJobStatus without credentials, so no token goes to this host. *)
    let rec poll_video_job ~sleep job_id polls_left =
      if polls_left <= 0 then
        on_error (Printf.sprintf "Video processing timeout waiting for job %s" job_id)
      else
        sleep ~seconds:video_job_poll_interval_seconds
          (fun () ->
            let status_url = Printf.sprintf "%s/xrpc/app.bsky.video.getJobStatus?jobId=%s"
              video_upload_host (Uri.pct_encode job_id) in
            Http.get status_url
              (fun response ->
                let ok = response.status >= 200 && response.status < 300 in
                match Option.map video_job_of_json (parse_json response.body) with
                | Some (Video_blob blob) -> on_success (blob, alt_text)
                | Some (Video_failed reason) when ok ->
                    on_error (Printf.sprintf "Video processing failed: %s" reason)
                | Some Video_completed_without_blob when ok ->
                    on_error "Video processing completed without blob"
                | Some (Video_pending _) when ok -> poll_video_job ~sleep job_id (polls_left - 1)
                | None when ok -> on_error "Failed to parse video job status"
                | Some (Video_failed _ | Video_completed_without_blob | Video_pending _) | None ->
                    if response.status = 429 || response.status >= 500 then
                      poll_video_job ~sleep job_id (polls_left - 1)
                    else
                      on_error (Printf.sprintf "Video job status failed (%d): %s"
                        response.status response.body))
              (fun _ -> poll_video_job ~sleep job_id (polls_left - 1)))
          (fun err -> on_error (Printf.sprintf "Video job wait failed: %s" err))
    in
    (* The video service is not there, or does not accept our service token
       (e.g. a PDS whose audience we derived wrongly): uploadBlob still
       publishes the video. *)
    let video_service_unusable status_code response_body =
      let upper_body = String.uppercase_ascii response_body in
      let mentions token = Re.execp (Re.str token |> Re.compile) upper_body in
      status_code = 401
      || status_code = 404
      || status_code = 405
      || status_code = 501
      || (status_code = 400
          && (mentions "METHODNOTFOUND"
              || mentions "METHOD_NOT_FOUND"
              || mentions "METHOD NOT FOUND"
              || mentions "UNKNOWNMETHOD"
              || mentions "NOTIMPLEMENTED"))
    in
    let rec upload_via_video ~sleep ~service_token ~user_did attempts_left =
      let video_url = Printf.sprintf "%s/xrpc/app.bsky.video.uploadVideo?did=%s&name=video.mp4"
        video_upload_host (Uri.pct_encode user_did) in
      let video_headers = [
        ("Authorization", Printf.sprintf "Bearer %s" service_token);
        ("Content-Type", mime_type);
      ] in
      Http.post ~headers:video_headers ~body:blob_data video_url
        (fun response ->
          let ok = response.status >= 200 && response.status < 300 in
          match Option.map video_job_of_json (parse_json response.body) with
          | Some (Video_blob blob) -> on_success (blob, alt_text)
          | Some (Video_pending (Some job_id)) ->
              (* Fresh job, or [already_exists] naming the earlier one. *)
              poll_video_job ~sleep job_id video_job_max_polls
          | Some (Video_failed reason) when ok ->
              on_error (Printf.sprintf "Video processing failed: %s" reason)
          | Some (Video_completed_without_blob | Video_pending None) when ok ->
              on_error "Video upload response missing both blob and jobId"
          | None when ok -> on_error "Failed to parse video upload response"
          | Some (Video_failed _ | Video_completed_without_blob | Video_pending None) | None ->
              if video_service_unusable response.status response.body then
                upload_via_blob ()
              else
                on_error (Printf.sprintf "Video upload failed (%d): %s" response.status response.body))
        (fun err ->
          if attempts_left > 1 then
            upload_via_video ~sleep ~service_token ~user_did (attempts_left - 1)
          else
            on_error (Printf.sprintf "Video upload network error: %s" err))
    in
    let is_video = String.starts_with ~prefix:"video/" mime_type in
    match is_video, Options.sleep, did with
    | true, Some sleep, Some user_did ->
        resolve_pds_endpoint ~access_jwt (fun pds_endpoint ->
          get_service_auth ~pds_endpoint ~access_jwt ~did:user_did
            (fun service_token -> upload_via_video ~sleep ~service_token ~user_did 2)
            (fun _ ->
              (* Never send the user's access JWT to the video service. *)
              upload_via_blob ()))
    | true, Some _, None | true, None, _ | false, _, _ -> upload_via_blob ()

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

  (** Extract HTML <title> tag content as fallback when og:title is missing *)
  let extract_html_title html =
    try
      let regex = Re.Pcre.regexp ~flags:[`CASELESS] "<title[^>]*>([^<]+)</title>" in
      let group = Re.exec regex html in
      let title = String.trim (Re.Group.get group 1) in
      if title = "" then None else Some title
    with Not_found -> None

  (** Fetch link card metadata by scraping OpenGraph tags.
      Returns (card option, warnings list) *)
  let fetch_link_card ~access_jwt ~url on_success =
    let warnings = ref [] in
    
    (* Fetch the HTML content of the URL *)
    Http.get url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let html = response.body in
            
            (* Extract OpenGraph metadata *)
            let title = extract_og_tag html "title" in
            let description = extract_og_tag html "description" in
            let image_url = extract_og_tag html "image" in
            
            (* Use og:title, falling back to <title> tag *)
            let effective_title = match title with
              | Some _ -> title
              | None -> extract_html_title html
            in
            match effective_title with
            | None ->
                warnings := Error_types.Link_card_failed
                  (Printf.sprintf "No og:title or <title> found for %s" url) :: !warnings;
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
                    if not (is_safe_remote_media_url full_img_url) then begin
                      warnings := Error_types.Thumbnail_skipped "Unsafe thumbnail URL" :: !warnings;
                      create_card None
                    end else
                    Http.get full_img_url
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
                            
                            let upload_thumbnail blob_data =
                              upload_blob ~access_jwt ~blob_data
                                ~mime_type ~alt_text:None
                                (fun (blob, _) -> create_card (Some blob))
                                (fun err ->
                                  warnings := Error_types.Thumbnail_skipped
                                    (Printf.sprintf "Upload failed: %s" err) :: !warnings;
                                  create_card None)
                            in
                            if img_size > 1000000 then
                              Config.resize_image ~data:img_response.body ~mime_type ~max_bytes:1000000
                                (fun result ->
                                  match result with
                                  | Some resized -> upload_thumbnail resized
                                  | None ->
                                    warnings := Error_types.Thumbnail_skipped
                                      (Printf.sprintf "Image too large (%d bytes) and resize failed" img_size) :: !warnings;
                                    create_card None)
                            else
                              upload_thumbnail img_response.body
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
  
  (** Does a 400 from [com.atproto.server.refreshSession] mean the stored
      refresh JWT is dead?

      Only for the two atproto error names below. atproto also answers 400 for
      [InvalidRequest] (a malformed call, which is our bug, not the user's),
      and a reverse proxy or WAF in front of a struggling PDS can emit a 400
      of its own with no XRPC envelope at all. Treating those as a revoked
      session hands the user a "reconnect this account" badge for an outage
      they cannot fix. Everything else falls through to [on_refresh_error],
      which consults {!Health_status.of_error} and leaves the stored status
      alone when the error is not credential evidence. *)
  let refresh_session_400_is_terminal raw_response =
    match raw_response with
    | None -> false
    | Some body ->
        (match atproto_error_name body with
         | Some ("ExpiredToken" | "InvalidToken") -> true
         | Some _ | None -> false)

  (** Ensure valid session token, reusing cached session when available.

      Auth model:
      - [creds.access_token] is the user's identifier (handle, DID or the
        login email); public. Every refresh and rotation rewrites the row
        with {!Auth.session_to_refresh_credentials}, so it becomes the DID.
      - [creds.refresh_token] holds either a Bluesky [refresh_jwt] (when
        [creds.auth_type = Bearer]) or a legacy app password (when
        [creds.auth_type = App_password]).
      - On first use with an [App_password]-flagged row, we call
        [Auth.create_session] to obtain a JWT pair and immediately rotate
        storage to [Bearer] with [refresh_jwt]; the app password is then no
        longer persisted. This addresses the H4 finding (see CHANGES.md):
        previously the app password was retained indefinitely and could be
        used by anyone with DB access until the user manually revoked it in
        Bluesky settings.
      - Once in Bearer mode, every refresh rotates [refresh_jwt] via
        [Auth.refresh_session]; an attacker who exfiltrated a refresh_jwt
        becomes useless after our next refresh. *)
  include (struct
  let ensure_valid_token_unlocked ~cache_commit ~require_persistence ~reused ~renewed ~account_id on_success on_error =
    let module AuthMake = Auth.Make (Http) in
    let cache_and_succeed ~did ~access_jwt =
      Hashtbl.replace session_cache account_id (did, access_jwt, Unix.gettimeofday (), cache_commit);
      Config.update_health_status ~account_id
        ~status:(Health_status.to_string Health_status.Healthy) ~error_message:None
        (fun () -> on_success (renewed access_jwt))
        (fun _ -> on_success (renewed access_jwt))
    in
    let on_token_revoked () =
      invalidate_session ~account_id;
      Config.update_health_status ~account_id
        ~status:(Health_status.to_string Health_status.Token_revoked)
        ~error_message:(Some "Bluesky session no longer valid. Please reconnect this account.")
        (fun () -> on_error (Error_types.Auth_error Error_types.Token_expired))
        (fun _ -> on_error (Error_types.Auth_error Error_types.Token_expired))
    in
    (* A refresh that fails for a reason other than authentication tells us
       nothing about the stored credentials. The PDS may be down, rate
       limiting us, or unreachable. Recording that as a credential failure
       turns a connectivity blip into a "reconnect this account" badge that no
       probe can clear, so we leave the stored health status alone and hand
       the caller the underlying error unchanged.

       Passing [err] through rather than rewrapping it as
       [Auth_error (Refresh_failed _)] matters just as much: consumers route
       on [Error_types.classify_error], and the old wrapper made every network
       error classify as [Auth_failure]. *)
    let on_refresh_error ~context err =
      invalidate_session ~account_id;
      match Health_status.of_error err with
      | None -> on_error err
      | Some status ->
          Config.update_health_status ~account_id
            ~status:(Health_status.to_string status)
            ~error_message:(Some (context ^ ": " ^ Error_types.error_to_string err))
            (fun () -> on_error err)
            (fun _ -> on_error err)
    in
    let now = Unix.gettimeofday () in
    match Hashtbl.find_opt session_cache account_id with
    | Some (_did, access_jwt, created_at, commit)
      when !commit = Committed && now -. created_at < session_max_age_seconds ->
        on_success (reused access_jwt)
    | _ ->
        Config.get_credentials ~account_id
          (fun creds ->
            let identifier = creds.access_token in
            match creds.refresh_token with
            | None ->
                (* Genuine credential evidence: the row has nothing to refresh
                   with, so no amount of retrying will help. *)
                Config.update_health_status ~account_id
                  ~status:(Health_status.to_string Health_status.Refresh_failed)
                  ~error_message:(Some "No Bluesky credentials available - reconnect the account")
                  (fun () -> on_error (Error_types.Auth_error Error_types.Missing_credentials))
                  (fun _ -> on_error (Error_types.Auth_error Error_types.Missing_credentials))
            | Some stored_secret ->
                (* Discriminate by format. A JWT is base64url-encoded JSON,
                   so it always starts with eyJ; an app password never does.
                   Detecting by format rather than [auth_type] lets us
                   migrate existing rows without a schema/data migration -
                   [auth_type] defaults to Bearer in many storage backends
                   even when the stored value is actually an app password. *)
                let is_jwt_mode =
                  String.length stored_secret >= 3
                  && String.sub stored_secret 0 3 = "eyJ"
                in
                if is_jwt_mode then
                  AuthMake.refresh_session ~pds_url:(service_url ()) ~refresh_jwt:stored_secret (function
                    | Error_types.Success session
                    | Error_types.Partial_success { result = session; _ } ->
                        let new_creds = Auth.session_to_refresh_credentials session in
                        Config.update_credentials ~account_id ~credentials:new_creds
                          (fun () -> cache_and_succeed ~did:session.did ~access_jwt:session.access_jwt)
                          (fun _ ->
                            if Persistence.required || require_persistence then
                              on_error (Error_types.Network_error
                                (Error_types.Connection_failed "Failed to persist refreshed session"))
                            else cache_and_succeed ~did:session.did ~access_jwt:session.access_jwt)
                    | Error_types.Failure (Error_types.Auth_error _) ->
                        on_token_revoked ()
                    | Error_types.Failure (Error_types.Api_error api_err)
                      when api_err.Error_types.status_code = 400
                           && refresh_session_400_is_terminal
                                api_err.Error_types.raw_response ->
                        on_token_revoked ()
                    | Error_types.Failure err ->
                        on_refresh_error ~context:"Session refresh failed" err)
                else
                  (* Legacy app_password row: exchange for a JWT pair and
                     rotate storage so the app password is forgotten. *)
                  AuthMake.create_session ~pds_url:(service_url ())
                    ~identifier
                    ~app_password:stored_secret
                    (function
                      | Error_types.Success session
                      | Error_types.Partial_success { result = session; _ } ->
                          (* refresh_token becomes the refresh_jwt and
                             auth_type flips to Bearer, so subsequent calls
                             take the refresh path above. *)
                          let rotated = Auth.session_to_refresh_credentials session in
                          Config.update_credentials ~account_id ~credentials:rotated
                            (fun () -> cache_and_succeed ~did:session.did ~access_jwt:session.access_jwt)
                            (fun _ ->
                              if Persistence.required || require_persistence then
                                on_error (Error_types.Network_error
                                  (Error_types.Connection_failed "Failed to persist rotated session"))
                              else cache_and_succeed ~did:session.did ~access_jwt:session.access_jwt)
                      | Error_types.Failure (Error_types.Auth_error _) ->
                          on_token_revoked ()
                      | Error_types.Failure err ->
                          on_refresh_error ~context:"Session creation failed" err))
          (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err)))
  
  let discard_acquisition ~account_id ~cache_commit =
    match Hashtbl.find_opt session_cache account_id with
    | Some (_, _, _, owner) when owner == cache_commit -> invalidate_session ~account_id
    | Some _ | None -> ()

  let ensure_valid_token ~account_id on_success on_error =
    let cache_commit = ref (if Persistence.required then Pending else Committed) in
    if not Persistence.required then
      ensure_valid_token_unlocked ~cache_commit ~require_persistence:false ~reused:Fun.id ~renewed:Fun.id ~account_id on_success on_error
    else
    Config.with_refresh_lock ~account_id
      (fun on_locked_success on_locked_error ->
        ensure_valid_token_unlocked ~cache_commit ~require_persistence:false ~reused:Fun.id ~renewed:Fun.id ~account_id
          (fun access_jwt ->
            Config.get_credentials ~account_id
              (fun credentials ->
                (* Stored access_token is the DID. Only the completion value
                   carries the session JWT; storage keeps its existing codec. *)
                on_locked_success { credentials with Social_core.access_token = access_jwt })
              (fun error -> on_locked_error (Error_types.Network_error
                (Error_types.Connection_failed error))))
          on_locked_error)
      (fun credentials ->
        cache_commit := Committed;
        on_success credentials.Social_core.access_token)
      (fun error ->
        discard_acquisition ~account_id ~cache_commit;
        on_error error)

  let ensure_valid_token_with_outcome ~with_refresh_lock ~account_id on_success on_error =
    let cache_commit = ref Pending in
    with_refresh_lock ~account_id
      (fun success failure ->
        ensure_valid_token_unlocked ~cache_commit ~require_persistence:true
          ~reused:(fun token -> Social_credential_outcome.Reused token)
          ~renewed:(fun token -> Social_credential_outcome.Renewed token)
          ~account_id success failure)
      (fun outcome -> cache_commit := Committed; on_success outcome)
      (fun error -> discard_acquisition ~account_id ~cache_commit; on_error error)

  end : sig
    val ensure_valid_token_with_outcome :
      with_refresh_lock:(account_id:string ->
        ((string Social_credential_outcome.t -> unit) -> (Error_types.error -> unit) -> unit) ->
        (string Social_credential_outcome.t -> unit) -> (Error_types.error -> unit) -> unit) ->
      account_id:string -> (string Social_credential_outcome.t -> unit) ->
      (Error_types.error -> unit) -> unit
    val ensure_valid_token :
      account_id:string -> (string -> unit) ->
      (Error_types.error -> unit) -> unit
  end)

  (** Ensure valid session token and also return the DID *)
  let ensure_valid_token_with_did ~account_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        match Hashtbl.find_opt session_cache account_id with
        | Some (did, _, _, _) -> on_success (did, access_jwt)
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
            (

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
                      if not (is_safe_remote_media_url url) then
                        on_err (Error_types.Validation_error [Error_types.Invalid_url "Unsafe media URL"])
                      else
                        Http.get url
                        (fun media_resp ->
                          if media_resp.status >= 200 && media_resp.status < 300 then
                            let raw_mime_type =
                              List.assoc_opt "content-type" media_resp.headers
                              |> Option.value ~default:"application/octet-stream"
                            in
                            (* Strip MIME parameters (e.g. charset) *)
                            let mime_type =
                              match String.split_on_char ';' raw_mime_type with
                              | base :: _ -> String.trim base
                              | [] -> raw_mime_type
                            in
                            (* Reject non-media MIME types (e.g. XML error pages from CDNs) *)
                            let is_media_mime =
                              String.starts_with ~prefix:"image/" mime_type
                              || String.starts_with ~prefix:"video/" mime_type
                            in
                            if not is_media_mime then
                              on_err (Error_types.make_api_error
                                ~platform:Platform_types.Bluesky
                                ~status_code:415
                                ~message:(Printf.sprintf "Media URL returned unsupported content type '%s' instead of image/* or video/*: %s" mime_type url) ())
                            else
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
                        let url = Printf.sprintf "%s/xrpc/com.atproto.repo.createRecord" (service_url ()) in
                        let body = `Assoc [
                          ("repo", `String did);
                          ("collection", `String "app.bsky.feed.post");
                          ("record", post_record);
                        ] in
                        let body_str = Yojson.Basic.to_string body in
                        let headers = [
                          ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
                          ("Content-Type", "application/json");
                        ] in
                        
                        Http.post ~headers ~body:body_str url
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
                              on_result (Error_types.Failure (parse_xrpc_response_error response)))
                          (fun err -> on_result (Error_types.Failure (Error_types.Network_error 
                            (Error_types.Connection_failed err))))))
                      (fun _ -> on_result (Error_types.Failure (Error_types.Internal_error 
                        "Facet extraction failed"))))
                  (fun err -> on_result (Error_types.Failure err))))
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

  (** Reply to a post.

      [reply_to.parent] is the post answered; [reply_to.root] the first post
      of its thread (equal to [parent] for a top-level post). Get both
      [uri] and [cid] from a thread read ([getPostThread]) or from the id a
      posting function returned ({!strong_ref_of_post_id}). The reply is
      posted with the same text, media, facet and link-card handling as
      {!post_single}, and the outcome carries the new post's ["uri|cid"].
  *)
  let reply_to_post ~account_id ~(reply_to : reply_ref) ~text ~media_urls ?(alt_texts=[]) ?(widths=[]) ?(heights=[]) ?(skip_enrichments=false) ?(validate_media_before_upload=false) on_result =
    post_with_reply_impl ~account_id ~text ~media_urls ~alt_texts ~widths ~heights ~skip_enrichments ~validate_media_before_upload
      ~reply_refs:(Some (reply_to.root.uri, reply_to.root.cid, reply_to.parent.uri, reply_to.parent.cid))
      on_result

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
      @param reply_to Attach the first post as a reply under this reference
             (the rest chain under it as usual). Use it to post a thread
             as a reply, or to resume a thread that failed part-way:
             pass the original root and the last post that went out, and
             only the remaining texts. Default: the first post starts a
             new thread.
      @param on_result Callback receiving the outcome with thread_result.
             When a post after the first fails, the outcome is
             [Partial_success] with [failed_at_index = Some i] and the
             ["uri|cid"] ids of the posts that went out.
  *)
  let post_thread ~account_id ~texts ~media_urls_per_post ?(alt_texts_per_post=[]) ?(widths_per_post=[]) ?(heights_per_post=[]) ?(skip_enrichments=false) ?(validate_media_before_upload=false) ?(reply_to : reply_ref option) on_result =
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
        
        let initial_root, initial_parent =
          match reply_to with
          | None -> (None, None)
          | Some { root; parent } ->
              (Some (root.uri, root.cid), Some (parent.uri, parent.cid))
        in
        post_thread_items texts media_with_meta_per_post initial_root initial_parent []
  
  (** {1 Public API - Post Management} *)
  
  (** Delete a post from Bluesky *)
  let delete_post ~account_id ~post_uri on_result =
    ensure_valid_token_with_did ~account_id
      (fun (repo_did, access_jwt) ->
            let uri_parts = String.split_on_char '/' post_uri in
            let rkey = List.nth uri_parts (List.length uri_parts - 1) in
            
            let url = Printf.sprintf "%s/xrpc/com.atproto.repo.deleteRecord" (service_url ()) in
            let body = `Assoc [
              ("repo", `String repo_did);
              ("collection", `String "app.bsky.feed.post");
              ("rkey", `String rkey);
            ] in
            let body_str = Yojson.Basic.to_string body in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
              ("Content-Type", "application/json");
            ] in
            
            Http.post ~headers ~body:body_str url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  on_result (Error_types.Success ())
                else
                  on_result (Error_types.Failure (parse_xrpc_response_error response)))
              (fun err -> on_result (Error_types.Failure (Error_types.Network_error 
                (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error_types.Failure err))
  
  (** {1 Public API - Interactions} *)
  
  (** Like a post *)
  let like_post ~account_id ~post_uri ~post_cid on_result =
    ensure_valid_token_with_did ~account_id
      (fun (repo_did, access_jwt) ->
            let now = Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 (Ptime_clock.now ()) in
            
            let url = Printf.sprintf "%s/xrpc/com.atproto.repo.createRecord" (service_url ()) in
            let body = `Assoc [
              ("repo", `String repo_did);
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
            
            Http.post ~headers ~body:body_str url
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
                  on_result (Error (parse_xrpc_response_error response)))
              (fun err -> on_result (Error (Error_types.Network_error 
                (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Unlike a post *)
  let unlike_post ~account_id ~like_uri on_result =
    ensure_valid_token_with_did ~account_id
      (fun (repo_did, access_jwt) ->
            let uri_parts = String.split_on_char '/' like_uri in
            let rkey = List.nth uri_parts (List.length uri_parts - 1) in
            
            let url = Printf.sprintf "%s/xrpc/com.atproto.repo.deleteRecord" (service_url ()) in
            let body = `Assoc [
              ("repo", `String repo_did);
              ("collection", `String "app.bsky.feed.like");
              ("rkey", `String rkey);
            ] in
            let body_str = Yojson.Basic.to_string body in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
              ("Content-Type", "application/json");
            ] in
            
            Http.post ~headers ~body:body_str url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  on_result (Ok ())
                else
                  on_result (Error (parse_xrpc_response_error response)))
              (fun err -> on_result (Error (Error_types.Network_error 
                (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Repost a post *)
  let repost ~account_id ~post_uri ~post_cid on_result =
    ensure_valid_token_with_did ~account_id
      (fun (repo_did, access_jwt) ->
            let now = Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 (Ptime_clock.now ()) in
            
            let url = Printf.sprintf "%s/xrpc/com.atproto.repo.createRecord" (service_url ()) in
            let body = `Assoc [
              ("repo", `String repo_did);
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
            
            Http.post ~headers ~body:body_str url
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
                  on_result (Error (parse_xrpc_response_error response)))
              (fun err -> on_result (Error (Error_types.Network_error 
                (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Unrepost *)
  let unrepost ~account_id ~repost_uri on_result =
    ensure_valid_token_with_did ~account_id
      (fun (repo_did, access_jwt) ->
            let uri_parts = String.split_on_char '/' repost_uri in
            let rkey = List.nth uri_parts (List.length uri_parts - 1) in
            
            let url = Printf.sprintf "%s/xrpc/com.atproto.repo.deleteRecord" (service_url ()) in
            let body = `Assoc [
              ("repo", `String repo_did);
              ("collection", `String "app.bsky.feed.repost");
              ("rkey", `String rkey);
            ] in
            let body_str = Yojson.Basic.to_string body in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
              ("Content-Type", "application/json");
            ] in
            
            Http.post ~headers ~body:body_str url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  on_result (Ok ())
                else
                  on_result (Error (parse_xrpc_response_error response)))
              (fun err -> on_result (Error (Error_types.Network_error 
                (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** {1 Public API - Social Graph} *)
  
  (** Follow a user *)
  let follow ~account_id ~did on_result =
    ensure_valid_token_with_did ~account_id
      (fun (repo_did, access_jwt) ->
            let now = Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 (Ptime_clock.now ()) in
            
            let url = Printf.sprintf "%s/xrpc/com.atproto.repo.createRecord" (service_url ()) in
            let body = `Assoc [
              ("repo", `String repo_did);
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
            
            Http.post ~headers ~body:body_str url
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
                  on_result (Error (parse_xrpc_response_error response)))
              (fun err -> on_result (Error (Error_types.Network_error 
                (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Unfollow a user *)
  let unfollow ~account_id ~follow_uri on_result =
    ensure_valid_token_with_did ~account_id
      (fun (repo_did, access_jwt) ->
            let uri_parts = String.split_on_char '/' follow_uri in
            let rkey = List.nth uri_parts (List.length uri_parts - 1) in
            
            let url = Printf.sprintf "%s/xrpc/com.atproto.repo.deleteRecord" (service_url ()) in
            let body = `Assoc [
              ("repo", `String repo_did);
              ("collection", `String "app.bsky.graph.follow");
              ("rkey", `String rkey);
            ] in
            let body_str = Yojson.Basic.to_string body in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
              ("Content-Type", "application/json");
            ] in
            
            Http.post ~headers ~body:body_str url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  on_result (Ok ())
                else
                  on_result (Error (parse_xrpc_response_error response)))
              (fun err -> on_result (Error (Error_types.Network_error 
                (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** {1 Public API - Read Operations} *)
  
  (** Get a user profile *)
  let get_profile ~account_id ~actor on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let url = Printf.sprintf "%s/xrpc/app.bsky.actor.getProfile?actor=%s" 
          (service_url ()) (Uri.pct_encode actor) in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Http.get ~headers url
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
              on_result (Error (parse_xrpc_response_error response)))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Get a post thread *)
  let get_post_thread ~account_id ~post_uri on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let url = Printf.sprintf "%s/xrpc/app.bsky.feed.getPostThread?uri=%s" 
          (service_url ()) (Uri.pct_encode post_uri) in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Http.get ~headers url
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
              on_result (Error (parse_xrpc_response_error response)))
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
        let url = Printf.sprintf "%s/xrpc/app.bsky.feed.getTimeline%s" (service_url ()) query_string in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              match parse_json_object_or_error ~context:"Failed to parse timeline" ~body:response.body with
              | Ok json ->
                  (match ensure_array_field ~json ~field:"feed" ~context:"Failed to parse timeline" with
                   | Ok () -> on_result (Ok json)
                   | Error err -> on_result (Error err))
              | Error err -> on_result (Error err)
            else
              on_result (Error (parse_xrpc_response_error response)))
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
            (
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
                      if not (is_safe_remote_media_url url) then
                        on_err (Error_types.Validation_error [Error_types.Invalid_url "Unsafe media URL"])
                      else
                        Http.get url
                        (fun media_resp ->
                          if media_resp.status >= 200 && media_resp.status < 300 then
                            let raw_mime_type =
                              List.assoc_opt "content-type" media_resp.headers
                              |> Option.value ~default:"application/octet-stream"
                            in
                            (* Strip MIME parameters (e.g. charset) *)
                            let mime_type =
                              match String.split_on_char ';' raw_mime_type with
                              | base :: _ -> String.trim base
                              | [] -> raw_mime_type
                            in
                            (* Reject non-media MIME types (e.g. XML error pages from CDNs) *)
                            let is_media_mime =
                              String.starts_with ~prefix:"image/" mime_type
                              || String.starts_with ~prefix:"video/" mime_type
                            in
                            if not is_media_mime then
                              on_err (Error_types.make_api_error
                                ~platform:Platform_types.Bluesky
                                ~status_code:415
                                ~message:(Printf.sprintf "Media URL returned unsupported content type '%s' instead of image/* or video/*: %s" mime_type url) ())
                            else
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
                        
                        let url = Printf.sprintf "%s/xrpc/com.atproto.repo.createRecord" (service_url ()) in
                        let body = `Assoc [
                          ("repo", `String did);
                          ("collection", `String "app.bsky.feed.post");
                          ("record", `Assoc final_record);
                        ] in
                        let body_str = Yojson.Basic.to_string body in
                        let headers = [
                          ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
                          ("Content-Type", "application/json");
                        ] in
                        
                        Http.post ~headers ~body:body_str url
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
                              on_result (Error_types.Failure (parse_xrpc_response_error response)))
                          (fun err -> on_result (Error_types.Failure (Error_types.Network_error 
                            (Error_types.Connection_failed err)))))
                      (fun _ -> on_result (Error_types.Failure (Error_types.Internal_error 
                        "Facet extraction failed"))))
                  (fun err -> on_result (Error_types.Failure err))))
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
          (service_url ()) query_string in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              match parse_json_object_or_error ~context:"Failed to parse notifications" ~body:response.body with
              | Ok json ->
                  (match ensure_array_field ~json ~field:"notifications" ~context:"Failed to parse notifications" with
                   | Ok () -> on_result (Ok json)
                   | Error err -> on_result (Error err))
              | Error err -> on_result (Error err)
            else
              on_result (Error (parse_xrpc_response_error response)))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Count unread notifications *)
  let count_unread_notifications ~account_id on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let url = Printf.sprintf "%s/xrpc/app.bsky.notification.getUnreadCount" (service_url ()) in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Http.get ~headers url
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
              on_result (Error (parse_xrpc_response_error response)))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Update seen notifications *)
  let update_seen_notifications ~account_id on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let now = Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 (Ptime_clock.now ()) in
        let url = Printf.sprintf "%s/xrpc/app.bsky.notification.updateSeen" (service_url ()) in
        let body = `Assoc [("seenAt", `String now)] in
        let body_str = Yojson.Basic.to_string body in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
          ("Content-Type", "application/json");
        ] in
        
        Http.post ~headers ~body:body_str url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_result (Ok ())
            else
              on_result (Error (parse_xrpc_response_error response)))
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
          (service_url ()) query_string in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              match parse_json_object_or_error ~context:"Failed to parse search results" ~body:response.body with
              | Ok json ->
                  (match ensure_array_field ~json ~field:"actors" ~context:"Failed to parse search results" with
                   | Ok () -> on_result (Ok json)
                   | Error err -> on_result (Error err))
              | Error err -> on_result (Error err)
            else
              on_result (Error (parse_xrpc_response_error response)))
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
          (service_url ()) query_string in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              match parse_json_object_or_error ~context:"Failed to parse search results" ~body:response.body with
              | Ok json ->
                  (match ensure_array_field ~json ~field:"posts" ~context:"Failed to parse search results" with
                   | Ok () -> on_result (Ok json)
                   | Error err -> on_result (Error err))
              | Error err -> on_result (Error err)
            else
              on_result (Error (parse_xrpc_response_error response)))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** {1 Public API - Moderation} *)
  
  (** Mute an actor *)
  let mute_actor ~account_id ~actor on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let url = Printf.sprintf "%s/xrpc/app.bsky.graph.muteActor" (service_url ()) in
        let body = `Assoc [("actor", `String actor)] in
        let body_str = Yojson.Basic.to_string body in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
          ("Content-Type", "application/json");
        ] in
        
        Http.post ~headers ~body:body_str url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_result (Ok ())
            else
              on_result (Error (parse_xrpc_response_error response)))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Unmute an actor *)
  let unmute_actor ~account_id ~actor on_result =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let url = Printf.sprintf "%s/xrpc/app.bsky.graph.unmuteActor" (service_url ()) in
        let body = `Assoc [("actor", `String actor)] in
        let body_str = Yojson.Basic.to_string body in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
          ("Content-Type", "application/json");
        ] in
        
        Http.post ~headers ~body:body_str url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_result (Ok ())
            else
              on_result (Error (parse_xrpc_response_error response)))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Block an actor *)
  let block_actor ~account_id ~actor on_result =
    ensure_valid_token_with_did ~account_id
      (fun (repo_did, access_jwt) ->
            let now = Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 (Ptime_clock.now ()) in
            
            let url = Printf.sprintf "%s/xrpc/com.atproto.repo.createRecord" (service_url ()) in
            let body = `Assoc [
              ("repo", `String repo_did);
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
            
            Http.post ~headers ~body:body_str url
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
                  on_result (Error (parse_xrpc_response_error response)))
              (fun err -> on_result (Error (Error_types.Network_error 
                (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))
  
  (** Unblock an actor *)
  let unblock_actor ~account_id ~block_uri on_result =
    ensure_valid_token_with_did ~account_id
      (fun (repo_did, access_jwt) ->
            let uri_parts = String.split_on_char '/' block_uri in
            let rkey = List.nth uri_parts (List.length uri_parts - 1) in
            
            let url = Printf.sprintf "%s/xrpc/com.atproto.repo.deleteRecord" (service_url ()) in
            let body = `Assoc [
              ("repo", `String repo_did);
              ("collection", `String "app.bsky.graph.block");
              ("rkey", `String rkey);
            ] in
            let body_str = Yojson.Basic.to_string body in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
              ("Content-Type", "application/json");
            ] in
            
            Http.post ~headers ~body:body_str url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  on_result (Ok ())
                else
                  on_result (Error (parse_xrpc_response_error response)))
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
          (service_url ()) query_string in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              match parse_json_object_or_error ~context:"Failed to parse author feed" ~body:response.body with
              | Ok json ->
                  (match ensure_array_field ~json ~field:"feed" ~context:"Failed to parse author feed" with
                   | Ok () -> on_result (Ok json)
                   | Error err -> on_result (Error err))
              | Error err -> on_result (Error err)
            else
              on_result (Error (parse_xrpc_response_error response)))
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
          (service_url ()) query_string in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              match parse_json_object_or_error ~context:"Failed to parse likes" ~body:response.body with
              | Ok json ->
                  (match ensure_array_field ~json ~field:"likes" ~context:"Failed to parse likes" with
                   | Ok () -> on_result (Ok json)
                   | Error err -> on_result (Error err))
              | Error err -> on_result (Error err)
            else
              on_result (Error (parse_xrpc_response_error response)))
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
          (service_url ()) query_string in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              match parse_json_object_or_error ~context:"Failed to parse reposts" ~body:response.body with
              | Ok json ->
                  (match ensure_array_field ~json ~field:"repostedBy" ~context:"Failed to parse reposts" with
                   | Ok () -> on_result (Ok json)
                   | Error err -> on_result (Error err))
              | Error err -> on_result (Error err)
            else
              on_result (Error (parse_xrpc_response_error response)))
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
          (service_url ()) query_string in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              match parse_json_object_or_error ~context:"Failed to parse followers" ~body:response.body with
              | Ok json ->
                  (match ensure_array_field ~json ~field:"followers" ~context:"Failed to parse followers" with
                   | Ok () -> on_result (Ok json)
                   | Error err -> on_result (Error err))
              | Error err -> on_result (Error err)
            else
              on_result (Error (parse_xrpc_response_error response)))
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
          (service_url ()) query_string in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              match parse_json_object_or_error ~context:"Failed to parse follows" ~body:response.body with
              | Ok json ->
                  (match ensure_array_field ~json ~field:"follows" ~context:"Failed to parse follows" with
                   | Ok () -> on_result (Ok json)
                   | Error err -> on_result (Error err))
              | Error err -> on_result (Error err)
            else
              on_result (Error (parse_xrpc_response_error response)))
          (fun err -> on_result (Error (Error_types.Network_error 
            (Error_types.Connection_failed err)))))
      (fun err -> on_result (Error err))

  (** {1 Typed reads}

      Account-bound versions of {!Reads.Make}, sent to {!service_url} with
      the account's session ({!ensure_valid_token}, cached for four
      minutes). The older JSON-returning readers ([get_post_thread],
      [get_author_feed], [list_notifications]) are unchanged. *)

  module Reads_http = Reads.Make (Http)

  let with_session ~account_id on_result read =
    ensure_valid_token ~account_id read (fun err -> on_result (Stdlib.Error err))

  (** {!Reads.Make.get_post_thread} for a stored account. *)
  let get_post_thread_view ~account_id ~post_uri ?depth ?parent_height on_result =
    with_session ~account_id on_result (fun access_jwt ->
        Reads_http.get_post_thread ~service_url:(service_url ()) ~access_jwt ~post_uri
          ?depth ?parent_height on_result)

  (** {!Reads.Make.get_author_feed} for a stored account. *)
  let get_author_feed_page ~account_id ~actor ?limit ?cursor ?filter ?include_pins
      on_result =
    with_session ~account_id on_result (fun access_jwt ->
        Reads_http.get_author_feed ~service_url:(service_url ()) ~access_jwt ~actor
          ?limit ?cursor ?filter ?include_pins on_result)

  (** {!Reads.Make.get_posts} for a stored account (batches of 25). *)
  let get_posts ~account_id ~post_uris on_result =
    with_session ~account_id on_result (fun access_jwt ->
        Reads_http.get_posts ~service_url:(service_url ()) ~access_jwt ~post_uris
          on_result)

  (** {!Reads.Make.get_profile} for a stored account: a typed profile with
      counts. The older [get_profile] returns the raw JSON and is
      unchanged. *)
  let get_profile_view ~account_id ~actor on_result =
    with_session ~account_id on_result (fun access_jwt ->
        Reads_http.get_profile ~service_url:(service_url ()) ~access_jwt ~actor on_result)

  (** {!Reads.Make.list_notifications} for a stored account. *)
  let list_notifications_page ~account_id ?limit ?cursor ?reasons on_result =
    with_session ~account_id on_result (fun access_jwt ->
        Reads_http.list_notifications ~service_url:(service_url ()) ~access_jwt
          ?limit ?cursor ?reasons on_result)
end

(** Make functor with {!Default_options} (the Bluesky-hosted entryway). *)
module Make_with_options (Config : CONFIG) (Options : OPTIONS) =
  Make_internal (struct
    include Config
    let with_refresh_lock ~account_id:_ run on_success on_error =
      run on_success on_error
  end) (Options) (struct let required = false end)

module Make (Config : CONFIG) = Make_with_options (Config) (Default_options)

module Make_coordinated_with_options (Config : COORDINATED_CONFIG) (Options : OPTIONS) =
  Make_internal (Config) (Options) (struct let required = true end)

module Make_coordinated (Config : COORDINATED_CONFIG) =
  Make_coordinated_with_options (Config) (Default_options)

(** Coordinated credential operations without legacy account-health writes.
    All typed results, credential persistence, and locking are preserved. *)
module Make_credentials_only (Config : COORDINATED_CONFIG) =
  Make_coordinated (struct
    include Config
    let update_health_status = Health_status.ignore_updates
  end)

(** Explicit provider options without legacy account-health writes. *)
module Make_credentials_only_with_options
    (Config : COORDINATED_CONFIG) (Options : OPTIONS) =
  Make_coordinated_with_options (struct
    include Config
    let update_health_status = Health_status.ignore_updates
  end) (Options)
