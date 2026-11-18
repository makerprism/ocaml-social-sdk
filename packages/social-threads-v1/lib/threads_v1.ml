open Social_core

(** {1 API version and endpoints}

    The single source of truth for the Threads API version this package
    targets. Use these instead of writing ["v1.0"] in application code. *)

(** Threads API version, e.g. ["v1.0"]. *)
let api_version = "v1.0"

(** Unversioned Threads Graph API host.

    Meta documents the API as reachable on both [graph.threads.net] and
    [graph.threads.com]; its examples use [graph.threads.com]. Both answer
    identically (checked 2026-11-17), so this keeps the host consumers
    already allow-list and pin in tests. *)
let graph_api_host = "https://graph.threads.net"

(** Versioned API base URL, ["https://graph.threads.net/v1.0"]. *)
let threads_api_base = graph_api_host ^ "/" ^ api_version

(** Long-lived token exchange ([th_exchange_token]). *)
let threads_token_exchange_endpoint = graph_api_host ^ "/access_token"

(** Long-lived token refresh ([th_refresh_token]). *)
let threads_token_refresh_endpoint = graph_api_host ^ "/refresh_access_token"

(** Threads authorization window.

    Meta's docs show [https://threads.com/oauth/authorize]. As of 2026-11-17
    [threads.net], [www.threads.net] and [threads.com] all answer with a 301
    to [https://www.threads.com/oauth/authorize], which serves the window.
    The host has been switched back and forth over reports that a redirect
    hop breaks Meta's EU cookie-consent step; change it only together with a
    live EU authorization test. *)
let oauth_authorize_url = "https://threads.net/oauth/authorize"

(** Authorization-code exchange endpoint. *)
let token_endpoint = graph_api_host ^ "/oauth/access_token"

(** Typed Threads Insights metrics and values ({!Threads_v1_insights}); the
    requests are [Make.get_user_insights] and [Make.get_media_insights]. *)
module Insights = Threads_v1_insights

(** Typed post listings retaining the full JSON item. *)
module Posts = Threads_v1_posts

(** Typed location search results ({!Threads_v1_locations}); the request is
    [Make.search_locations]. *)
module Locations = Threads_v1_locations

(** Hosts a [paging.next] / [paging.previous] URL may point at before a
    reader sends the caller's token to it ({!Social_meta_graph_paging.resolve_page_url}).
    Meta documents both graph hosts for the Threads API. *)
let paging_url_hosts = [ "graph.threads.net"; "graph.threads.com" ]

(* [Social_token_expiry] clamps to one year and also accepts a numeric string. *)
let parse_expires_in json = Social_token_expiry.expires_in_of_json json

let expires_at_of_expires_in expires_in =
  Option.bind expires_in (fun seconds -> Social_token_expiry.expires_at_of_expires_in seconds)

let parse_credentials_from_json json =
  let open Yojson.Basic.Util in
  try
    let access_token =
      json
      |> member "access_token"
      |> to_string
      |> String.trim
    in
    if access_token = "" then
      Error "empty access token in OAuth response"
    else
      let token_type_str =
        let parsed =
          try Some (json |> member "token_type" |> to_string)
          with _ -> None
        in
        match parsed with
        | Some v when String.trim v <> "" -> String.trim v
        | _ -> "Bearer"
      in
      let expires_at = parse_expires_in json |> expires_at_of_expires_in in
      let scope = try Some (json |> member "scope" |> to_string) with _ -> None in
      Ok ({ access_token; refresh_token = None; expires_at; auth_type = auth_type_of_string token_type_str; scope } : credentials)
  with exn ->
    Error (Printf.sprintf "invalid OAuth response payload: %s" (Printexc.to_string exn))

let find_header headers key =
  let key_lower = String.lowercase_ascii key in
  headers
  |> List.find_map (fun (k, v) ->
         if String.lowercase_ascii k = key_lower then Some v else None)

let parse_retry_after headers =
  let clamp_retry_after seconds =
    if seconds < 0 then 0 else if seconds > 86400 then 86400 else seconds
  in
  let parse_retry_after_float value =
    match classify_float value with
    | FP_nan -> None
    | FP_infinite ->
        if value > 0.0 then Some 86400 else Some 0
    | FP_zero | FP_normal | FP_subnormal ->
        let rounded = if value > 0.0 then ceil value else value in
        if rounded >= 86400.0 then Some 86400
        else if rounded <= 0.0 then Some 0
        else Some (int_of_float rounded)
  in
  match find_header headers "retry-after" with
  | Some raw ->
      (try
         let value = int_of_string (String.trim raw) in
         Some (clamp_retry_after value)
       with _ ->
         (try
            let value = float_of_string (String.trim raw) in
            parse_retry_after_float value
          with _ -> None))
  | None -> None

let request_id_of_headers headers =
  match find_header headers "x-fb-trace-id" with
  | Some request_id -> Some request_id
  | None -> find_header headers "x-fb-request-id"

let is_sensitive_key key =
  match String.lowercase_ascii key with
  | "access_token"
  | "refresh_token"
  | "client_secret"
  | "authorization"
  | "code" -> true
  | _ -> false

(* OAuth authorization codes are strings. Graph error codes are numbers and
   must survive in [raw_response] so {!Social_provider_error_detail} can read them. *)
let is_sensitive_field key (value : Yojson.Basic.t) =
  is_sensitive_key key
  &&
  match value with
  | `Int _ -> String.lowercase_ascii key <> "code"
  | `Null | `Bool _ | `Float _ | `String _ | `Assoc _ | `List _ -> true

let contains_substring haystack needle =
  let h_len = String.length haystack in
  let n_len = String.length needle in
  if n_len = 0 then true
  else if n_len > h_len then false
  else
    let rec loop i =
      if i + n_len > h_len then false
      else if String.sub haystack i n_len = needle then true
      else loop (i + 1)
    in
    loop 0

let sanitize_non_json_body response_body =
  let lower = String.lowercase_ascii response_body in
  if List.exists (contains_substring lower)
       [ "access_token";
         "refresh_token";
         "client_secret";
         "authorization";
         "code=";
         "\"code\"" ]
  then "[REDACTED_NON_JSON_ERROR_BODY]"
  else response_body

let sanitize_text_if_sensitive text =
  let lower = String.lowercase_ascii text in
  if List.exists (contains_substring lower)
       [ "access_token";
         "refresh_token";
         "client_secret";
         "authorization";
         "code=";
         "\"code\"" ]
  then "[REDACTED_SENSITIVE_TEXT]"
  else text

let rec redact_json_sensitive_fields = function
  | `Assoc fields ->
      `Assoc
        (List.map
           (fun (key, value) ->
             if is_sensitive_field key value then (key, `String "[REDACTED]")
             else (key, redact_json_sensitive_fields value))
           fields)
  | `List values -> `List (List.map redact_json_sensitive_fields values)
  | other -> other

let sanitize_response_body response_body =
  try
    response_body
    |> Yojson.Basic.from_string
    |> redact_json_sensitive_fields
    |> Yojson.Basic.to_string
  with _ -> sanitize_non_json_body response_body

let constant_time_equal a b =
  let len_a = String.length a in
  let len_b = String.length b in
  let max_len = if len_a > len_b then len_a else len_b in
  let diff = ref (len_a lxor len_b) in
  let get_char s len i = if i < len then Char.code s.[i] else 0 in
  for i = 0 to max_len - 1 do
    diff := !diff lor (get_char a len_a i lxor get_char b len_b i)
  done;
  !diff = 0

(* Threads has always appended the Graph code, type and subcode to the
   message as " [code=N;type=T;subcode=S]"; consumers may read it, so it
   stays. *)
let provider_metadata_suffix (graph : Social_meta_graph_error.t) =
  let code = Option.value ~default:0 graph.code in
  let provider_type = Option.value ~default:"" graph.error_type in
  if code <> 0 || provider_type <> "" || graph.subcode <> None then
    Printf.sprintf " [code=%d%s%s]" code
      (if provider_type = "" then "" else ";type=" ^ provider_type)
      (match graph.subcode with
       | Some subcode -> Printf.sprintf ";subcode=%d" subcode
       | None -> "")
  else ""

let with_message_suffix suffix (error : Error_types.error) =
  match error with
  | Error_types.Api_error api_error ->
      Error_types.Api_error { api_error with message = api_error.message ^ suffix }
  | Error_types.Auth_error (Error_types.Insufficient_permissions permissions) ->
      Error_types.Auth_error
        (Error_types.Insufficient_permissions
           { permissions with
             platform_message =
               Option.map (fun m -> m ^ suffix) permissions.platform_message })
  | Error_types.Auth_error
      ( Error_types.Token_expired | Error_types.Token_invalid
      | Error_types.Token_revoked | Error_types.Refresh_failed _
      | Error_types.Missing_credentials )
  | Error_types.Validation_error _ | Error_types.Rate_limited _
  | Error_types.Network_error _ | Error_types.Duplicate_content
  | Error_types.Content_policy_violation _ | Error_types.Resource_not_found _
  | Error_types.Internal_error _ ->
      error

(** Graph error body as an SDK error, through the table shared with the
    Facebook and Instagram packages ({!Social_meta_graph_error}). Secrets are
    scrubbed from the stored body and the message. *)
let parse_api_error ~status_code ~headers ~response_body =
  let error =
    Social_meta_graph_error.to_error
      ~platform:Platform_types.Threads
      ~status_code
      ~required_permissions:["threads_basic"]
      ?retry_after_seconds:(parse_retry_after headers)
      ?request_id:(request_id_of_headers headers)
      ~redact_body:sanitize_response_body
      ~redact_message:sanitize_text_if_sensitive
      response_body
  in
  match Social_meta_graph_error.of_body response_body with
  | Some graph -> with_message_suffix (provider_metadata_suffix graph) error
  | None -> error

let network_error err =
  Social_meta_graph_error.transport_error (sanitize_text_if_sensitive err)

module OAuth = struct
  (** Thread OAuth scopes as an ADT with string fallback for future scopes.
      Use [Custom s] for scopes not yet modeled in the ADT.

      Full list from Meta Developer Portal:
      - threads_basic: Display user's own posts
      - threads_content_publish: Create and publish content
      - threads_delete: Delete user's posts
      - threads_keyword_search: Search and fetch content by keyword
      - threads_location_tagging: Search locations and tag media with location
      - threads_manage_insights: Access profile insights (analytics)
      - threads_manage_mentions: Fetch mentions
      - threads_manage_replies: Create, hide/show replies, control who can reply
      - threads_profile_discovery: Access public profiles and posts
      - threads_read_replies: Read replies to user's threads
      - threads_share_to_instagram: Share Threads posts to linked Instagram account *)
  type scope =
    | Threads_basic
    | Threads_content_publish
    | Threads_delete
    | Threads_keyword_search
    | Threads_location_tagging
    | Threads_manage_insights
    | Threads_manage_mentions
    | Threads_manage_replies
    | Threads_profile_discovery
    | Threads_read_replies
    | Threads_share_to_instagram
    | Custom of string

  (** Convert a scope to its string representation. *)
  let scope_to_string = function
    | Threads_basic -> "threads_basic"
    | Threads_content_publish -> "threads_content_publish"
    | Threads_delete -> "threads_delete"
    | Threads_keyword_search -> "threads_keyword_search"
    | Threads_location_tagging -> "threads_location_tagging"
    | Threads_manage_insights -> "threads_manage_insights"
    | Threads_manage_mentions -> "threads_manage_mentions"
    | Threads_manage_replies -> "threads_manage_replies"
    | Threads_profile_discovery -> "threads_profile_discovery"
    | Threads_read_replies -> "threads_read_replies"
    | Threads_share_to_instagram -> "threads_share_to_instagram"
    | Custom s -> s

  (** Convert a scope list to the new Meta params[scope] JSON array format.
      Meta changed from ?scope=xxx to ?params[scope]=["xxx","yyy"] *)
  let scopes_to_json_array scopes =
    scopes
    |> List.map scope_to_string
    |> List.map (fun s -> Printf.sprintf "\"%s\"" s)
    |> String.concat ","
    |> Printf.sprintf "[%s]"

  (** Helper to create custom scopes from strings. *)
  let custom_scope s = Custom s

  module Presets = struct
    let basic = [Threads_basic]
    let publish = [Threads_basic; Threads_content_publish]
    let publish_with_analytics = [Threads_basic; Threads_content_publish; Threads_manage_insights; Threads_location_tagging]
    let read = [Threads_basic]
  end

  (** Backward compatibility: string lists for legacy code *)
  module Scopes = struct
    let basic = ["threads_basic"]
    let publish = ["threads_basic"; "threads_content_publish"]
    let publish_with_analytics = ["threads_basic"; "threads_content_publish"; "threads_manage_insights"; "threads_location_tagging"]
    let read = ["threads_basic"]
  end

  let authorization_endpoint = oauth_authorize_url
  let token_endpoint = token_endpoint

  (** The Threads authorization window URL: the one supported builder,
      shared with {!get_authorization_url} and [Make.get_oauth_url].

      [GET {!oauth_authorize_url}?client_id&redirect_uri&scope=a,b&response_type=code&state]
      as documented in
      {{:https://developers.facebook.com/docs/threads/get-started/get-access-tokens-and-permissions}}
      ("[scope]: A comma-separated list, or URL-encoded space-separated
      list"). Plain [scope], not [params[scope]]: the JSON-array form makes
      Threads answer [InvalidScopeRequested]. The host stays
      {!oauth_authorize_url}; see its note about EU consent redirects.

      @param scopes Permission names; use [List.map scope_to_string] for
        typed scopes.
      @param extra_params Appended as given. *)
  let authorize_url ~client_id ~redirect_uri ~state ~scopes ?(extra_params = []) () =
    let params =
      [ ("client_id", client_id);
        ("redirect_uri", redirect_uri);
        ("scope", Social_oauth_scope.join Social_oauth_scope.Comma scopes);
        ("response_type", "code");
        ("state", state) ]
      @ extra_params
    in
    let query = Uri.encoded_of_query (List.map (fun (k, v) -> (k, [ v ])) params) in
    Printf.sprintf "%s?%s" authorization_endpoint query

  (** Typed-scope form of {!authorize_url}. *)
  let get_authorization_url ~client_id ~redirect_uri ~state ~scopes =
    authorize_url ~client_id ~redirect_uri ~state ~scopes:(List.map scope_to_string scopes) ()

  (** A Threads user profile
      ({{:https://developers.facebook.com/docs/threads/threads-profiles}}). *)
  type profile = {
    id : string;
    username : string option;
    name : string option;
    profile_picture_url : string option;  (** [threads_profile_picture_url] *)
    biography : string option;  (** [threads_biography] *)
    is_verified : bool;
  }

  (** Every documented profile field. *)
  let profile_fields =
    [ "id"; "username"; "name"; "threads_profile_picture_url"; "threads_biography"; "is_verified" ]

  (** Parses a [/me] or [/{user_id}] profile body. [Error] without an [id]
      (string or number). Empty strings read as [None]. *)
  let parse_profile body : (profile, string) Stdlib.result =
    let text fields key =
      match List.assoc_opt key fields with
      | Some (`String "") | None -> None
      | Some (`String value) -> Some value
      | Some (`Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _) -> None
    in
    match Yojson.Basic.from_string body with
    | exception Yojson.Json_error message -> Stdlib.Error ("profile response is not JSON: " ^ message)
    | `Assoc fields -> (
        let id =
          match List.assoc_opt "id" fields with
          | Some (`String id) when id <> "" -> Some id
          | Some (`Int id) -> Some (string_of_int id)
          | Some (`String _ | `Null | `Bool _ | `Float _ | `List _ | `Assoc _) | None -> None
        in
        match id with
        | None -> Stdlib.Error "profile response has no id"
        | Some id ->
            Stdlib.Ok
              {
                id;
                username = text fields "username";
                name = text fields "name";
                profile_picture_url = text fields "threads_profile_picture_url";
                biography = text fields "threads_biography";
                is_verified =
                  (match List.assoc_opt "is_verified" fields with
                   | Some (`Bool verified) -> verified
                   | Some (`Null | `Int _ | `Float _ | `String _ | `List _ | `Assoc _) | None -> false);
              })
    | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ ->
        Stdlib.Error "profile response is not an object"

  module Make (Http : HTTP_CLIENT) = struct
    let exchange_code ~client_id ~client_secret ~redirect_uri ~code on_success on_error =
      let body =
        Social_core.form_urlencode_kvs
          [ ("client_id", client_id);
            ("client_secret", client_secret);
            ("grant_type", "authorization_code");
            ("redirect_uri", redirect_uri);
            ("code", code) ]
      in
      let headers = [ ("Content-Type", "application/x-www-form-urlencoded") ] in
      Http.post ~headers ~body token_endpoint
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              (match parse_credentials_from_json json with
               | Ok creds -> on_success creds
               | Error msg -> on_error (Printf.sprintf "Failed to parse token response: %s" msg))
            with exn ->
              on_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string exn))
          else
            let response_body = sanitize_response_body response.body in
            on_error
              (Printf.sprintf
                  "Threads OAuth token exchange failed (%d): %s"
                  response.status
                  response_body))
        on_error

    let exchange_for_long_lived_token ~client_secret ~short_lived_token on_success on_error =
      let query =
        Uri.encoded_of_query
          [ ("grant_type", ["th_exchange_token"]);
            ("client_secret", [client_secret]);
            ("access_token", [short_lived_token]) ]
      in
      let url = Printf.sprintf "%s?%s" threads_token_exchange_endpoint query in
      Http.get url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              (match parse_credentials_from_json json with
               | Ok creds -> on_success creds
               | Error msg ->
                   on_error (Printf.sprintf "Failed to parse long-lived token response: %s" msg))
            with exn ->
              on_error
                (Printf.sprintf "Failed to parse long-lived token response: %s" (Printexc.to_string exn))
          else
            let response_body = sanitize_response_body response.body in
            on_error
              (Printf.sprintf
                  "Threads long-lived token exchange failed (%d): %s"
                  response.status
                  response_body))
        on_error

    let refresh_token ~long_lived_token on_success on_error =
      let query =
        Uri.encoded_of_query
          [ ("grant_type", ["th_refresh_token"]);
            ("access_token", [long_lived_token]) ]
      in
      let url = Printf.sprintf "%s?%s" threads_token_refresh_endpoint query in
      Http.get url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              (match parse_credentials_from_json json with
               | Ok creds -> on_success creds
               | Error msg -> on_error (Printf.sprintf "Failed to parse refresh response: %s" msg))
            with exn ->
              on_error (Printf.sprintf "Failed to parse refresh response: %s" (Printexc.to_string exn))
          else
            let response_body = sanitize_response_body response.body in
            on_error
              (Printf.sprintf
                  "Threads token refresh failed (%d): %s"
                  response.status
                  response_body))
        on_error

    (** Typed refresh failures for callers deciding whether to retry or reconnect.
        The legacy [refresh_token] keeps its string-error contract.
        {{:https://developers.facebook.com/docs/threads/get-started/long-lived-tokens/}Token refresh}
        uses [GET /refresh_access_token] with [th_refresh_token]. *)
    let refresh_token_typed ~long_lived_token on_success on_error =
      let query =
        Uri.encoded_of_query
          [ ("grant_type", ["th_refresh_token"]);
            ("access_token", [long_lived_token]) ]
      in
      let url = Printf.sprintf "%s?%s" threads_token_refresh_endpoint query in
      Http.get url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            let parsed =
              try parse_credentials_from_json (Yojson.Basic.from_string response.body)
              with Yojson.Json_error _ -> Error "Invalid JSON"
            in
            match parsed with
            | Ok credentials -> on_success credentials
            | Error message ->
                on_error (Error_types.Internal_error
                  ("Failed to parse refresh response: " ^ sanitize_text_if_sensitive message))
          else
            on_error (parse_api_error ~status_code:response.status
              ~headers:response.headers ~response_body:response.body))
        (fun message ->
          on_error (Social_meta_graph_error.transport_error
            (sanitize_text_if_sensitive message)))

    (** The profile of the token's user, [GET /me?fields=...]
        ({{:https://developers.facebook.com/docs/threads/threads-profiles}}),
        token-based so it works in an OAuth callback before an account is
        stored. Needs [threads_basic].

        @param fields Defaults to {!profile_fields}. Fields left out read as
          [None] ([false] for [is_verified]); [id] is always requested. *)
    let get_me ~access_token ?(fields = profile_fields) on_result =
      let fields = if List.mem "id" fields then fields else "id" :: fields in
      let query =
        Uri.encoded_of_query
          [ ("fields", [ String.concat "," fields ]); ("access_token", [ access_token ]) ]
      in
      let url = Printf.sprintf "%s/me?%s" threads_api_base query in
      Http.get url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            match parse_profile response.body with
            | Stdlib.Ok profile -> on_result (Stdlib.Ok profile)
            | Stdlib.Error message -> on_result (Stdlib.Error (Error_types.Internal_error message))
          else
            on_result
              (Stdlib.Error
                 (parse_api_error ~status_code:response.status ~headers:response.headers
                    ~response_body:response.body)))
        (fun err -> on_result (Stdlib.Error (network_error err)))

    (** Inspect a token with [GET /debug_token]
        ({{:https://developers.facebook.com/docs/threads/reference/debug}}):
        validity, granted scopes, expiry and user id as Meta recorded them.

        @param app_access_token The Threads app access token,
          {!Social_meta_debug_token.threads_app_access_token}
          (["TH|app_id|app_secret"],
          {{:https://developers.facebook.com/docs/threads/get-started/app-access-tokens}}),
          or a tester's user token for the same app. Keep it server-side.
        @param input_token The token to inspect.

        A token Meta considers invalid is [Ok] with [is_valid = false];
        [Error] means the call itself failed. *)
    let debug_token_info ~app_access_token ~input_token on_result =
      let query =
        Uri.encoded_of_query
          [ ("input_token", [ input_token ]); ("access_token", [ app_access_token ]) ]
      in
      let url = Printf.sprintf "%s/debug_token?%s" threads_api_base query in
      Http.get url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            match Social_meta_debug_token.of_body response.body with
            | Stdlib.Ok info -> on_result (Stdlib.Ok info)
            | Stdlib.Error message -> on_result (Stdlib.Error (Error_types.Internal_error message))
          else
            on_result
              (Stdlib.Error
                 (parse_api_error ~status_code:response.status ~headers:response.headers
                    ~response_body:response.body)))
        (fun err -> on_result (Stdlib.Error (network_error err)))
  end
end

type thread_user = {
  id : string;
  username : string option;
  name : string option;
}

(** Moderation state of a reply ([hide_status] field). Documented values:
    [NOT_HUSHED], [UNHUSHED], [HIDDEN], [COVERED], [BLOCKED], [RESTRICTED].
    [Unknown_hide_status] keeps any value Meta adds later instead of failing
    the whole page. *)
type hide_status =
  | Not_hushed
  | Unhushed
  | Hidden
  | Covered
  | Blocked
  | Restricted
  | Unknown_hide_status of string

let hide_status_of_string raw =
  match String.uppercase_ascii raw with
  | "NOT_HUSHED" -> Not_hushed
  | "UNHUSHED" -> Unhushed
  | "HIDDEN" -> Hidden
  | "COVERED" -> Covered
  | "BLOCKED" -> Blocked
  | "RESTRICTED" -> Restricted
  | _ -> Unknown_hide_status raw

(** Approval state of a reply held for approval ([reply_approval_status],
    returned by the [pending_replies] edge). *)
type reply_approval_status =
  | Approval_pending
  | Approval_ignored
  | Unknown_approval_status of string

let reply_approval_status_of_string raw =
  match String.lowercase_ascii raw with
  | "pending" -> Approval_pending
  | "ignored" -> Approval_ignored
  | _ -> Unknown_approval_status raw

let string_of_reply_approval_status = function
  | Approval_pending -> "pending"
  | Approval_ignored -> "ignored"
  | Unknown_approval_status raw -> raw

(** A Threads media object: a post, or a reply read from the [replies],
    [conversation] or [pending_replies] edges.

    The reply-only fields ([username] aside) are [None]/[false] on top-level
    posts. [owner_id] is only returned for top-level posts you own, never on
    replies. *)
type thread_post = {
  id : string;
  text : string option;
  timestamp : string option;
  permalink : string option;
  media_type : string option;
  media_url : string option;
  thumbnail_url : string option;
  shortcode : string option;
  is_quote_post : bool;
  has_replies : bool;
  reply_audience : string option;
  owner_id : string option;
  username : string option;
      (** Author username. Only returned for public users and yourself. *)
  is_reply : bool;
  root_post_id : string option;  (** Top-level post of the conversation. *)
  replied_to_id : string option;  (** Direct parent of this reply. *)
  is_reply_owned_by_me : bool option;
  hide_status : hide_status option;
  reply_approval_status : reply_approval_status option;
      (** Only returned by the [pending_replies] edge. *)
}

type insight_metric =
  | Views
  | Likes
  | Replies
  | Reposts
  | Quotes
  | Shares

type account_insight_value = {
  value : int;
  end_time : string option;
}

type account_insight = {
  metric : insight_metric;
  period : string option;
  values : account_insight_value list;
}

type post_insight = {
  metric : insight_metric;
  period : string option;
  value : int;
}

(** Metrics {!Make.get_post_insights} requests (media level). *)
let insight_metric_names = [ "views"; "likes"; "replies"; "reposts"; "quotes"; "shares" ]

(** Metrics {!Make.get_account_insights} requests. [shares] is a media
    metric; the user insights endpoint does not document it. *)
let account_insight_metric_names = [ "views"; "likes"; "replies"; "reposts"; "quotes" ]

let string_of_insight_metric = function
  | Views -> "views"
  | Likes -> "likes"
  | Replies -> "replies"
  | Reposts -> "reposts"
  | Quotes -> "quotes"
  | Shares -> "shares"

let insight_canonical_metric_keys =
  Analytics_normalization.canonical_metric_keys_of_provider_metrics
    ~provider:Analytics_normalization.Threads
    insight_metric_names

let canonical_metric_of_insight_metric metric =
  Analytics_normalization.threads_metric_to_canonical
    (string_of_insight_metric metric)

let canonical_time_granularity_of_period = function
  | None -> None
  | Some period ->
      (match String.lowercase_ascii (String.trim period) with
       | "hour" -> Some Analytics_types.Hour
       | "day" -> Some Analytics_types.Day
       | "week" -> Some Analytics_types.Week
       | "month" -> Some Analytics_types.Month
       | "lifetime" -> Some Analytics_types.Lifetime
       | _ -> None)

let canonical_time_range_of_period period =
  match canonical_time_granularity_of_period period with
  | None -> None
  | Some granularity ->
      Some
        {
          Analytics_types.since = None;
          until_ = None;
          granularity = Some granularity;
        }

let to_canonical_account_insights_series (account_insights : account_insight list) =
  account_insights
  |> List.filter_map (fun (insight : account_insight) ->
         match canonical_metric_of_insight_metric insight.metric with
         | None -> None
         | Some metric ->
              let points =
                List.map
                  (fun (value : account_insight_value) ->
                    Analytics_types.make_datapoint ?timestamp:value.end_time value.value)
                  insight.values
              in
             Some
               (Analytics_types.make_series
                  ?time_range:(canonical_time_range_of_period insight.period)
                  ~metric
                  ~scope:Analytics_types.Account
                  ~provider_metric:(string_of_insight_metric insight.metric)
                  points))

let to_canonical_post_insights_series (post_insights : post_insight list) =
  post_insights
  |> List.filter_map (fun (insight : post_insight) ->
         match canonical_metric_of_insight_metric insight.metric with
         | None -> None
         | Some metric ->
             Some
               (Analytics_types.make_series
                  ?time_range:(canonical_time_range_of_period insight.period)
                  ~metric
                  ~scope:Analytics_types.Post
                  ~provider_metric:(string_of_insight_metric insight.metric)
                  [ Analytics_types.make_datapoint insight.value ]))

(** Reads [permalink] from a [GET /{id}?fields=permalink] response body.
    [Ok None] when the post exists but Meta returns no permalink for it
    (absent, [null] or empty); [Error] when the body is not a JSON object
    or the field is not a string. *)
let parse_permalink_response body : (string option, string) Stdlib.result =
  match Yojson.Basic.from_string body with
  | exception Yojson.Json_error msg -> Error msg
  | `Assoc fields -> (
      match List.assoc_opt "permalink" fields with
      | None | Some `Null | Some (`String "") -> Ok None
      | Some (`String permalink) -> Ok (Some permalink)
      | Some (`Bool _ | `Int _ | `Float _ | `List _ | `Assoc _) ->
          Error "permalink is not a string")
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ ->
      Error "expected a JSON object"

type media_kind =
  | Image
  | Video
  | GIF

(** Lifecycle state of a media container, from the [status] field of
    [GET /{container_id}?fields=status,error_message].
    See https://developers.facebook.com/docs/threads/troubleshooting *)
type container_status =
  | Finished  (** Ready to be published. *)
  | In_progress  (** Still processing; poll again. *)
  | Published
      (** The container's media object has already been published. Publishing
          it again is not possible; a retry after an ambiguous publish
          failure should treat this as success. *)
  | Expired
      (** Not published within 24 hours of creation. A new container must be
          created. *)
  | Error_status of string
      (** [ERROR], or a status this SDK does not know. Carries the
          [error_message] field (for example [FAILED_PROCESSING_VIDEO]) when
          present, otherwise the raw status. *)

type publishing_limit = {
  quota_usage : int;
  quota_total : int;
  reply_quota_usage : int;
  reply_quota_total : int;
}

let post_fields =
  "id,text,timestamp,permalink,media_type,media_url,thumbnail_url,shortcode,is_quote_post,has_replies,reply_audience,owner"

(** Fields requested on the [replies] and [conversation] edges, per
    https://developers.facebook.com/docs/threads/retrieve-and-manage-replies/replies-and-conversations.
    [owner] is left out: Meta only returns it on top-level posts you own. *)
let reply_fields =
  "id,text,username,permalink,timestamp,media_type,media_url,thumbnail_url,shortcode,is_quote_post,has_replies,reply_audience,root_post,replied_to,is_reply,is_reply_owned_by_me,hide_status"

(** [reply_fields] plus [reply_approval_status], for the [pending_replies] edge. *)
let pending_reply_fields = reply_fields ^ ",reply_approval_status"

let parse_thread_post json =
  let open Yojson.Basic.Util in
  {
    id = json |> member "id" |> to_string;
    text =
      (try Some (json |> member "text" |> to_string)
       with _ -> None);
    timestamp =
      (try Some (json |> member "timestamp" |> to_string)
       with _ -> None);
    permalink =
      (try Some (json |> member "permalink" |> to_string)
       with _ -> None);
    media_type =
      (try Some (json |> member "media_type" |> to_string)
       with _ -> None);
    media_url =
      (try Some (json |> member "media_url" |> to_string)
       with _ -> None);
    thumbnail_url =
      (try Some (json |> member "thumbnail_url" |> to_string)
       with _ -> None);
    shortcode =
      (try Some (json |> member "shortcode" |> to_string)
       with _ -> None);
    is_quote_post =
      (try json |> member "is_quote_post" |> to_bool
       with _ -> false);
    has_replies =
      (try json |> member "has_replies" |> to_bool
       with _ -> false);
    reply_audience =
      (try Some (json |> member "reply_audience" |> to_string)
       with _ -> None);
    owner_id =
      (try Some (json |> member "owner" |> member "id" |> to_string)
       with _ -> None);
    username =
      (try Some (json |> member "username" |> to_string)
       with _ -> None);
    is_reply =
      (try json |> member "is_reply" |> to_bool
       with _ -> false);
    root_post_id =
      (try Some (json |> member "root_post" |> member "id" |> to_string)
       with _ -> None);
    replied_to_id =
      (try Some (json |> member "replied_to" |> member "id" |> to_string)
       with _ -> None);
    is_reply_owned_by_me =
      (try Some (json |> member "is_reply_owned_by_me" |> to_bool)
       with _ -> None);
    hide_status =
      (try Some (json |> member "hide_status" |> to_string |> hide_status_of_string)
       with _ -> None);
    reply_approval_status =
      (try
         Some
           (json |> member "reply_approval_status" |> to_string
          |> reply_approval_status_of_string)
       with _ -> None);
  }

module type CONFIG = sig
  include Social_core.CONFIG
  module Http : Social_core.HTTP_CLIENT

  (** Non-blocking timer used for read-retry backoff and container polling.
      See {!Social_core.SLEEP}. *)
  include Social_core.SLEEP
end

module Make (Config : CONFIG) = struct
  module OAuth_http = OAuth.Make (Config.Http)

  let max_text_length = 500
  let max_alt_text_length = 1000
  let min_carousel_items = 2
  let max_carousel_items = 20
  let min_poll_options = 2
  let max_poll_options = 4
  let max_poll_option_length = 25

  let is_blank value = String.trim value = ""

  let has_surrounding_whitespace value = value <> String.trim value

  let contains_substring haystack needle =
    let h_len = String.length haystack in
    let n_len = String.length needle in
    if n_len = 0 then true
    else if n_len > h_len then false
    else
      let rec loop i =
        if i + n_len > h_len then false
        else if String.sub haystack i n_len = needle then true
        else loop (i + 1)
      in
      loop 0

  let classify_credentials_error err =
    let lower = String.lowercase_ascii (String.trim err) in
    let has_credentials_context =
      contains_substring lower "credential"
    in
    let indicates_missing =
      (String.starts_with ~prefix:"missing" lower && has_credentials_context)
      || (contains_substring lower "not found" && has_credentials_context)
      || String.starts_with ~prefix:"no credentials" lower
      || contains_substring lower "no credential"
      || contains_substring lower "missing credentials"
      || contains_substring lower "credentials missing"
    in
    if indicates_missing then `Missing else `Backend

  let parse_retry_attempts raw =
    try
      let value = int_of_string (String.trim raw) in
      if value < 1 then 1 else if value > 10 then 10 else value
    with _ -> 3

  let parse_non_negative_int ~default ~max_value raw =
    try
      let value = int_of_string (String.trim raw) in
      if value < 0 then 0 else if value > max_value then max_value else value
    with _ -> default

  let parse_bool ?(default = false) raw =
    match String.lowercase_ascii (String.trim raw) with
    | "1" | "true" | "yes" | "on" -> true
    | "0" | "false" | "no" | "off" -> false
    | _ -> default

  let max_read_retry_attempts () =
    match Config.get_env "THREADS_MAX_READ_RETRY_ATTEMPTS" with
    | Some raw -> parse_retry_attempts raw
    | None -> 3

  let token_expiry_skew_seconds () =
    match Config.get_env "THREADS_TOKEN_EXPIRY_SKEW_SECONDS" with
    | Some raw -> parse_non_negative_int ~default:60 ~max_value:3600 raw
    | None -> 60

  let auto_refresh_token_enabled () =
    match Config.get_env "THREADS_AUTO_REFRESH_TOKEN" with
    | Some raw -> parse_bool ~default:false raw
    | None -> false

  let retry_on_429 () =
    match Config.get_env "THREADS_READ_RETRY_ON_429" with
    | Some raw -> parse_bool ~default:false raw
    | None -> false

  let retry_on_5xx () =
    match Config.get_env "THREADS_READ_RETRY_ON_5XX" with
    | Some raw -> parse_bool ~default:true raw
    | None -> true

  let retry_on_network_error () =
    match Config.get_env "THREADS_READ_RETRY_ON_NETWORK" with
    | Some raw -> parse_bool ~default:true raw
    | None -> true

  let http_get_with_retry url on_success on_error =
    let max_attempts = max_read_retry_attempts () in
    let should_retry_429 = retry_on_429 () in
    let should_retry_5xx = retry_on_5xx () in
    let retry_network = retry_on_network_error () in
    let retry_status status =
      (status = 429 && should_retry_429) || (status >= 500 && should_retry_5xx)
    in
    let backoff_seconds attempt =
      min 2.0 (0.1 *. (2.0 ** float_of_int (max 0 (attempt - 1))))
    in
    let rec loop attempt =
      Config.Http.get url
        (fun response ->
          if retry_status response.status && attempt < max_attempts then
            (* If the backoff cannot be scheduled, hand back the transient
               response as if retries were exhausted. *)
            Config.sleep ~seconds:(backoff_seconds attempt)
              (fun () -> loop (attempt + 1))
              (fun _ -> on_success response)
          else
            on_success response)
        (fun err ->
          if retry_network && attempt < max_attempts then
            Config.sleep ~seconds:(backoff_seconds attempt)
              (fun () -> loop (attempt + 1))
              (fun _ -> on_error err)
          else
            on_error err)
    in
    loop 1

  let http_post_no_retry ?(headers = []) ?(body = "") url on_success on_error =
    Config.Http.post ~headers ~body url on_success on_error

  let http_delete url on_success on_error =
    Config.Http.delete url on_success on_error

  let is_token_expired = function
    | None -> false
    | Some expires_at ->
        (match Social_refresh.Time.needs_refresh
                 ~refresh_window_seconds:(token_expiry_skew_seconds ())
                 expires_at
         with
         | Ok refresh_needed -> refresh_needed
         | Error _ -> true)

  let with_valid_credentials ~account_id on_success on_error =
    let map_credentials_error err =
      if classify_credentials_error err = `Missing then
        Error_types.Auth_error Error_types.Missing_credentials
      else
        Error_types.Internal_error ("Failed to load credentials: " ^ err)
    in
    let load_credentials ~account_id on_loaded on_load_error =
      Config.get_credentials ~account_id
        (fun creds ->
           let token = String.trim creds.access_token in
           if token = "" then
             on_load_error "missing credentials"
           else
             on_loaded { creds with access_token = token })
        on_load_error
    in
    let perform_refresh ~credentials on_refresh_success on_refresh_error =
      if auto_refresh_token_enabled () then
        OAuth_http.refresh_token_typed ~long_lived_token:credentials.Social_core.access_token
          on_refresh_success on_refresh_error
      else
        on_refresh_error (Error_types.Auth_error Error_types.Token_expired)
    in
    let persist_credentials ~account_id ~credentials on_persisted on_persist_error =
      Config.update_credentials ~account_id ~credentials
        (fun () -> on_persisted ())
        on_persist_error
    in
    let update_health ~account_id ~status ~error_message on_health_ok on_health_error =
      Config.update_health_status ~account_id ~status ~error_message on_health_ok on_health_error
    in
    Social_refresh.Orchestrator.ensure_valid_access_token
      ~policy:{ Social_refresh.refresh_window_seconds = token_expiry_skew_seconds () }
      ~map_load_error:map_credentials_error
      ~account_id
      ~load_credentials
      ~perform_refresh
      ~persist_credentials
      ~update_health
      on_success
      on_error

  let validate_text_length text =
    let length = String.length (String.trim text) in
    if length > max_text_length then
      Some (Error_types.Text_too_long { length; max = max_text_length })
    else
      None

  let validate_post_content ~text ~media_urls =
    let trimmed = String.trim text in
    if trimmed = "" && media_urls = [] then
      Some Error_types.Text_empty
    else
      validate_text_length text

  let validate_alt_text alt_text =
    match alt_text with
    | None -> None
    | Some txt ->
        let length = String.length (String.trim txt) in
        if length > max_alt_text_length then
          Some (Error_types.Text_too_long { length; max = max_alt_text_length })
        else
          None

  let validate_topic_tag topic_tag =
    match topic_tag with
    | None -> None
    | Some tag ->
        let trimmed = String.trim tag in
        if trimmed = "" then None
        else if String.contains trimmed '.' || String.contains trimmed '&' then
          Some (Error_types.Topic_tag_invalid
            (Printf.sprintf "must not contain periods or ampersands: %s" trimmed))
        else
          None

  let validate_poll_options options =
    let count = List.length options in
    if count < min_poll_options then
      [ Error_types.Too_few_items { count; min = min_poll_options; item_kind = "poll options" } ]
    else if count > max_poll_options then
      [ Error_types.Too_many_media { count; max = max_poll_options } ]
    else
      List.filter_map
        (fun option_text ->
          let trimmed = String.trim option_text in
          let length = String.length trimmed in
          if length = 0 then
            Some Error_types.Text_empty
          else if length > max_poll_option_length then
            Some (Error_types.Text_too_long { length; max = max_poll_option_length })
          else
            None)
        options

  let is_http_url url =
    let lower = String.lowercase_ascii (String.trim url) in
    String.length lower > 8
    &&
    (String.starts_with ~prefix:"https://" lower || String.starts_with ~prefix:"http://" lower)

  let media_kind_of_url url =
    let lower = String.lowercase_ascii (String.trim url) in
    let path =
      try Uri.path (Uri.of_string lower)
      with _ -> lower
    in
    if Filename.check_suffix path ".gif"
    then Some GIF
    else if Filename.check_suffix path ".jpg"
       || Filename.check_suffix path ".jpeg"
       || Filename.check_suffix path ".png"
       || Filename.check_suffix path ".webp"
    then Some Image
    else if Filename.check_suffix path ".mp4"
            || Filename.check_suffix path ".mov"
            || Filename.check_suffix path ".avi"
            || Filename.check_suffix path ".mpeg"
    then Some Video
    else None

  let normalize_media_url url =
    String.trim url

  let normalize_optional_non_empty = function
    | None -> None
    | Some value ->
        let trimmed = String.trim value in
        if trimmed = "" then None else Some trimmed

  let normalize_idempotency_key = function
    | None -> None
    | Some key ->
        let trimmed = String.trim key in
        if trimmed = "" then None else Some trimmed

  let validate_media_urls ?(max_items = 1) media_urls =
    let count = List.length media_urls in
    if count = 0 then None
    else if count > max_items then
      Some (Error_types.Too_many_media { count; max = max_items })
    else
      let errors =
        List.filter_map
          (fun url ->
            let normalized = normalize_media_url url in
            if not (is_http_url normalized) then
              Some (Error_types.Invalid_url normalized)
            else
              match media_kind_of_url normalized with
              | Some _ -> None
              | None -> Some (Error_types.Media_unsupported_format normalized))
          media_urls
      in
      match errors with
      | [] -> None
      | first :: _ -> Some first

  let insight_metric_of_string raw_metric =
    match String.lowercase_ascii (String.trim raw_metric) with
    | "views" -> Some Views
    | "likes" -> Some Likes
    | "replies" -> Some Replies
    | "reposts" -> Some Reposts
    | "quotes" -> Some Quotes
    | "shares" -> Some Shares
    | _ -> None

  let parse_int_value json =
    let open Yojson.Basic.Util in
    try json |> to_int
    with _ -> int_of_float (json |> to_float)

  let parse_account_insights_response response_body =
    let open Yojson.Basic.Util in
    try
      let json = Yojson.Basic.from_string response_body in
      let data_items = json |> member "data" |> to_list in
      let rec parse_values acc = function
        | [] -> Ok (List.rev acc)
        | value_item :: rest ->
            let value =
              try Ok (parse_int_value (value_item |> member "value"))
              with _ ->
                try Ok (parse_int_value value_item)
                with exn ->
                  Error (Printf.sprintf "invalid insight value: %s" (Printexc.to_string exn))
            in
            (match value with
             | Error msg -> Error msg
             | Ok value ->
                 let end_time =
                   try Some (value_item |> member "end_time" |> to_string)
                   with _ -> None
                 in
                 parse_values ({ value; end_time } :: acc) rest)
      in
      let rec parse_items acc = function
        | [] -> Ok (List.rev acc)
        | item :: rest ->
            let metric_name = item |> member "name" |> to_string in
            (match insight_metric_of_string metric_name with
             | None -> Error ("unsupported insight metric: " ^ metric_name)
             | Some metric ->
                 let period =
                   try Some (item |> member "period" |> to_string)
                   with _ -> None
                 in
                 (* User metrics other than [views] answer with
                    [total_value: {value}] instead of [values]; read it as
                    one bucket without [end_time]. *)
                 let value_items =
                   match item |> member "values", item |> member "total_value" with
                   | `List items, _ -> items
                   | _, (`Assoc _ as total) -> [ total ]
                   | _, _ -> item |> member "values" |> to_list
                 in
                 (match parse_values [] value_items with
                  | Error msg -> Error msg
                  | Ok values ->
                      parse_items ({ metric; period; values } :: acc) rest))
      in
      parse_items [] data_items
    with exn ->
      Error (Printexc.to_string exn)

  let parse_post_insights_response response_body =
    let open Yojson.Basic.Util in
    try
      let json = Yojson.Basic.from_string response_body in
      let data_items = json |> member "data" |> to_list in
      let rec parse_items acc = function
        | [] -> Ok (List.rev acc)
        | item :: rest ->
            let metric_name = item |> member "name" |> to_string in
            (match insight_metric_of_string metric_name with
             | None -> Error ("unsupported insight metric: " ^ metric_name)
             | Some metric ->
                 let period =
                   try Some (item |> member "period" |> to_string)
                   with _ -> None
                 in
                 let value_result =
                   try Ok (parse_int_value (item |> member "value"))
                   with _ ->
                   try Ok (parse_int_value (item |> member "total_value" |> member "value"))
                   with _ ->
                     try
                       let values = item |> member "values" |> to_list in
                       match values with
                       | first :: _ ->
                           (try Ok (parse_int_value (first |> member "value"))
                            with _ -> Ok (parse_int_value first))
                       | [] -> Error "missing post insight value"
                     with exn ->
                       Error (Printf.sprintf "invalid post insight value: %s" (Printexc.to_string exn))
                 in
                 (match value_result with
                  | Error msg -> Error msg
                  | Ok value -> parse_items ({ metric; period; value } :: acc) rest))
      in
      parse_items [] data_items
    with exn ->
      Error (Printexc.to_string exn)

  let fetch_me_with_access_token ~access_token on_result =
    let query =
      Uri.encoded_of_query
        [ ("fields", ["id,username,name"]);
          ("access_token", [access_token]) ]
    in
    let url = Printf.sprintf "%s/me?%s" threads_api_base query in
    http_get_with_retry url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            let open Yojson.Basic.Util in
            let user =
              {
                id = json |> member "id" |> to_string;
                username =
                  (try Some (json |> member "username" |> to_string)
                   with _ -> None);
                name =
                  (try Some (json |> member "name" |> to_string)
                   with _ -> None);
              }
            in
            on_result (Ok user)
          with exn ->
            on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse /me response: %s" (Printexc.to_string exn))))
        else
          on_result
            (Error
               (parse_api_error
                  ~status_code:response.status
                  ~headers:response.headers
                  ~response_body:response.body)))
      (fun err -> on_result (Error (network_error err)))

  let get_user_id ~access_token on_result =
    fetch_me_with_access_token ~access_token
      (function
        | Ok me -> on_result (Ok me.id)
        | Error err -> on_result (Error err))

  let encode_poll_options options =
    List.mapi (fun i option_text ->
      let key = Printf.sprintf "poll_attachment[options][%d][text]" i in
      (key, String.trim option_text)
    ) options

  let create_text_container
      ~access_token
      ~user_id
      ~text
      ?reply_to_id
      ?idempotency_key
      ?reply_control
      ?topic_tag
      ?quoted_post_id
      ?location_id
      ?(is_ghost_post = false)
      ?(auto_publish = false)
      ?poll_options
      on_result =
    let normalized_text = String.trim text in
    let normalized_idempotency_key = normalize_idempotency_key idempotency_key in
    let body =
      Social_core.form_urlencode_kvs
        ([ ("media_type", "TEXT");
           ("text", normalized_text);
           ("access_token", access_token) ]
         @
         (match reply_to_id with
          | Some id -> [ ("reply_to_id", id) ]
          | None -> [])
         @
         (match reply_control with
          | Some v when String.trim v <> "" -> [ ("reply_control", String.trim v) ]
          | _ -> [])
         @
         (match topic_tag with
          | Some tag when String.trim tag <> "" -> [ ("text_post_app_tags", String.trim tag) ]
          | _ -> [])
         @
         (match normalized_idempotency_key with
          | Some key -> [ ("client_request_id", key) ]
          | None -> [])
         @
         (match quoted_post_id with
          | Some id when String.trim id <> "" -> [ ("quoted_post_id", String.trim id) ]
          | _ -> [])
         @
         (match location_id with
          | Some id when String.trim id <> "" -> [ ("location_id", String.trim id) ]
          | _ -> [])
         @
         (if is_ghost_post then [ ("is_ghost_post", "true") ] else [])
         @
         (if auto_publish then [ ("auto_publish_text", "true") ] else [])
         @
         (match poll_options with
          | Some options -> encode_poll_options options
          | None -> []))
    in
    let url = Printf.sprintf "%s/%s/threads" threads_api_base user_id in
    let headers = [ ("Content-Type", "application/x-www-form-urlencoded") ] in
    http_post_no_retry ~headers ~body url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            let open Yojson.Basic.Util in
            let creation_id = json |> member "id" |> to_string in
            on_result (Ok creation_id)
          with exn ->
            on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse container response: %s" (Printexc.to_string exn))))
        else
          on_result
            (Error
               (parse_api_error
                  ~status_code:response.status
                  ~headers:response.headers
                  ~response_body:response.body)))
      (fun err -> on_result (Error (network_error err)))

  let create_media_container
      ~access_token
      ~user_id
      ~text
      ~media_url
      ~kind
      ?(is_carousel_item = false)
      ?alt_text
      ?reply_to_id
      ?idempotency_key
      ?reply_control
      ?topic_tag
      ?quoted_post_id
      ?location_id
      on_result =
    let normalized_text = String.trim text in
    let normalized_idempotency_key = normalize_idempotency_key idempotency_key in
    let media_field, media_type =
      match kind with
      | Image -> ("image_url", "IMAGE")
      | Video -> ("video_url", "VIDEO")
      | GIF -> ("image_url", "GIF")
    in
    let body =
      Social_core.form_urlencode_kvs
         ([ ("media_type", media_type);
           (media_field, media_url);
           ("access_token", access_token) ]
         @
         (if is_carousel_item then [ ("is_carousel_item", "true") ] else [])
         @
         (match alt_text with
          | Some txt when String.trim txt <> "" -> [("alt_text", String.trim txt)]
          | _ -> [])
         @
         (if normalized_text = "" || is_carousel_item then [] else [ ("text", normalized_text) ])
         @
         (match reply_to_id with
          | Some id -> [ ("reply_to_id", id) ]
          | None -> [])
         @
         (match reply_control with
          | Some v when String.trim v <> "" -> [ ("reply_control", String.trim v) ]
          | _ -> [])
         @
         (match topic_tag with
          | Some tag when String.trim tag <> "" -> [ ("text_post_app_tags", String.trim tag) ]
          | _ -> [])
         @
         (match normalized_idempotency_key with
          | Some key -> [ ("client_request_id", key) ]
          | None -> [])
         @
         (match quoted_post_id with
          | Some id when String.trim id <> "" -> [ ("quoted_post_id", String.trim id) ]
          | _ -> [])
         @
         (match location_id with
          | Some id when String.trim id <> "" -> [ ("location_id", String.trim id) ]
          | _ -> []))
    in
    let url = Printf.sprintf "%s/%s/threads" threads_api_base user_id in
    let headers = [ ("Content-Type", "application/x-www-form-urlencoded") ] in
    http_post_no_retry ~headers ~body url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            let open Yojson.Basic.Util in
            let creation_id = json |> member "id" |> to_string in
            on_result (Ok creation_id)
          with exn ->
            on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse media container response: %s" (Printexc.to_string exn))))
        else
          on_result
            (Error
               (parse_api_error
                  ~status_code:response.status
                  ~headers:response.headers
                  ~response_body:response.body)))
      (fun err -> on_result (Error (network_error err)))

  let create_container_for_post
      ~access_token
      ~user_id
      ~text
      ~media_urls
      ?alt_text
      ?reply_to_id
      ?idempotency_key
      ?reply_control
      ?topic_tag
      ?quoted_post_id
      ?location_id
      ?(is_ghost_post = false)
      ?(auto_publish = false)
      ?poll_options
      on_result =
    match media_urls with
    | [] ->
        create_text_container
          ~access_token
          ~user_id
          ~text
          ?reply_to_id
          ?idempotency_key
          ?reply_control
          ?topic_tag
          ?quoted_post_id
          ?location_id
          ~is_ghost_post
          ~auto_publish
          ?poll_options
          on_result
    | [ media_url ] ->
        let normalized = normalize_media_url media_url in
        (match media_kind_of_url normalized with
         | Some kind ->
             create_media_container
               ~access_token
               ~user_id
               ~text
               ~media_url:normalized
               ~kind
               ?alt_text
               ?reply_to_id
               ?idempotency_key
               ?reply_control
               ?topic_tag
               ?quoted_post_id
               ?location_id
               on_result
         | None ->
             on_result
               (Error
                  (Error_types.Validation_error
                     [ Error_types.Media_unsupported_format normalized ])))
    | urls ->
        on_result
          (Error
             (Error_types.Validation_error
                [ Error_types.Too_many_media { count = List.length urls; max = 1 } ]))

  let publish_container ~access_token ~user_id ~creation_id on_result =
    let body =
      Social_core.form_urlencode_kvs
        [ ("creation_id", creation_id);
          ("access_token", access_token) ]
    in
    let url = Printf.sprintf "%s/%s/threads_publish" threads_api_base user_id in
    let headers = [ ("Content-Type", "application/x-www-form-urlencoded") ] in
    http_post_no_retry ~headers ~body url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            let open Yojson.Basic.Util in
            let post_id = json |> member "id" |> to_string in
            on_result (Ok post_id)
          with exn ->
            on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse publish response: %s" (Printexc.to_string exn))))
        else
          on_result
            (Error
               (parse_api_error
                  ~status_code:response.status
                  ~headers:response.headers
                  ~response_body:response.body)))
      (fun err -> on_result (Error (network_error err)))

  let check_container_status ~access_token ~container_id on_result =
    let query =
      Uri.encoded_of_query
        [ ("fields", ["status,error_message"]);
          ("access_token", [access_token]) ]
    in
    let url = Printf.sprintf "%s/%s?%s" threads_api_base container_id query in
    http_get_with_retry url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            let open Yojson.Basic.Util in
            let status =
              try json |> member "status" |> to_string
              with _ -> ""
            in
            let error_message =
              try Some (json |> member "error_message" |> to_string)
              with _ -> None
            in
            let container_st =
              match String.uppercase_ascii status with
              | "FINISHED" -> Finished
              | "IN_PROGRESS" -> In_progress
              | "PUBLISHED" -> Published
              | "EXPIRED" -> Expired
              | _ ->
                  let msg =
                    match error_message with
                    | Some m when m <> "" -> m
                    | _ -> status
                  in
                  Error_status msg
            in
            on_result (Ok container_st)
          with exn ->
            on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse container status response: %s" (Printexc.to_string exn))))
        else
          on_result
            (Error
               (parse_api_error
                  ~status_code:response.status
                  ~headers:response.headers
                  ~response_body:response.body)))
      (fun err -> on_result (Error (network_error err)))

  (** Poll [check_container_status] until the container is ready to publish.

      Returns [Ok container_id] once the status is [FINISHED]. An already
      [PUBLISHED] container yields [Error Duplicate_content], so a caller
      retrying an ambiguous publish can tell "already live" apart from a
      processing failure. [EXPIRED], [ERROR] and exhausting [max_attempts]
      yield [Error (Internal_error _)]. *)
  let poll_container_status
      ?(poll_interval = 2.0)
      ?(max_attempts = 10)
      ~access_token
      ~container_id
      on_result =
    let rec loop attempt =
      if attempt > max_attempts then
        on_result (Error (Error_types.Internal_error
          (Printf.sprintf "Container %s still processing after %d attempts" container_id max_attempts)))
      else
        check_container_status ~access_token ~container_id
          (function
            | Ok Finished -> on_result (Ok container_id)
            | Ok In_progress ->
                let delay = poll_interval *. (float_of_int (min attempt 5)) in
                Config.sleep ~seconds:delay
                  (fun () -> loop (attempt + 1))
                  (fun err ->
                    on_result (Error (Error_types.Internal_error
                      (Printf.sprintf "Container %s status poll interrupted: %s" container_id err))))
            | Ok Published -> on_result (Error Error_types.Duplicate_content)
            | Ok Expired ->
                on_result (Error (Error_types.Internal_error
                  (Printf.sprintf
                     "Container %s expired before it was published (containers expire 24 hours after creation)"
                     container_id)))
            | Ok (Error_status msg) ->
                on_result (Error (Error_types.Internal_error
                  (Printf.sprintf "Container %s processing failed: %s" container_id msg)))
            | Error err -> on_result (Error err))
    in
    loop 1

  let carousel_stage_error ~stage = function
    | Error_types.Api_error { status_code; message; platform; raw_response; request_id } ->
        Error_types.Api_error
          {
            status_code;
            message = Printf.sprintf "%s: %s" stage message;
            platform;
            raw_response;
            request_id;
          }
    | Error_types.Internal_error message ->
        Error_types.Internal_error (Printf.sprintf "%s: %s" stage message)
    | err -> err

  let create_carousel_item_container
      ~access_token
      ~user_id
      ~media_url
      ~kind
      ?alt_text
      on_result =
    create_media_container
      ~access_token
      ~user_id
      ~text:""
      ~media_url
      ~kind
      ~is_carousel_item:true
      ?alt_text
      on_result

  let rec create_carousel_children
      ~access_token
      ~user_id
      ~media_urls_with_alt
      ~acc
      on_result =
    match media_urls_with_alt with
    | [] -> on_result (Ok (List.rev acc))
    | (url, alt_text) :: rest ->
        let normalized = normalize_media_url url in
        (match media_kind_of_url normalized with
         | Some kind ->
             create_carousel_item_container
               ~access_token
               ~user_id
               ~media_url:normalized
               ~kind
               ?alt_text
               (function
                 | Ok child_id ->
                     poll_container_status
                       ~access_token
                       ~container_id:child_id
                       (function
                         | Ok _child_id ->
                             create_carousel_children
                               ~access_token
                               ~user_id
                               ~media_urls_with_alt:rest
                               ~acc:(child_id :: acc)
                               on_result
                         | Error err ->
                             on_result
                               (Error
                                  (carousel_stage_error
                                     ~stage:"carousel child container processing failed"
                                     err)))
                 | Error err ->
                     on_result
                       (Error
                          (carousel_stage_error
                             ~stage:"carousel child container create failed"
                             err)))
         | None ->
             on_result
               (Error
                  (Error_types.Validation_error
                     [ Error_types.Media_unsupported_format normalized ])))

  let create_carousel_container
      ~access_token
      ~user_id
      ~children_ids
      ~text
      ?topic_tag
      ?reply_control
      ?quoted_post_id
      ?location_id
      on_result =
    let normalized_text = String.trim text in
    let body =
      Social_core.form_urlencode_kvs
        ([ ("media_type", "CAROUSEL");
           ("children", String.concat "," children_ids);
           ("access_token", access_token) ]
         @
         (if normalized_text = "" then [] else [ ("text", normalized_text) ])
         @
         (match reply_control with
          | Some v when String.trim v <> "" -> [ ("reply_control", String.trim v) ]
          | _ -> [])
         @
         (match topic_tag with
          | Some tag when String.trim tag <> "" -> [ ("text_post_app_tags", String.trim tag) ]
          | _ -> [])
         @
         (match quoted_post_id with
          | Some id when String.trim id <> "" -> [ ("quoted_post_id", String.trim id) ]
          | _ -> [])
         @
         (match location_id with
          | Some id when String.trim id <> "" -> [ ("location_id", String.trim id) ]
          | _ -> []))
    in
    let url = Printf.sprintf "%s/%s/threads" threads_api_base user_id in
    let headers = [ ("Content-Type", "application/x-www-form-urlencoded") ] in
    http_post_no_retry ~headers ~body url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            let open Yojson.Basic.Util in
            let creation_id = json |> member "id" |> to_string in
            on_result (Ok creation_id)
          with exn ->
            on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse carousel container response: %s" (Printexc.to_string exn))))
        else
          on_result
            (Error
               (parse_api_error
                  ~status_code:response.status
                  ~headers:response.headers
                  ~response_body:response.body)))
      (fun err -> on_result (Error (network_error err)))

  let post_carousel ~account_id ~text ~media_urls ?(alt_texts=[]) ?topic_tag ?reply_control ?quoted_post_id ?location_id on_result =
    let validation_errors =
      [ validate_media_urls ~max_items:max_carousel_items media_urls;
        validate_topic_tag topic_tag ]
      |> List.filter_map (fun x -> x)
    in
    let count = List.length media_urls in
    let validation_errors =
      if count < min_carousel_items then
        (Error_types.Too_many_media { count; max = min_carousel_items }) :: validation_errors
      else
        validation_errors
    in
    if validation_errors <> [] then
      on_result (Error_types.Failure (Error_types.Validation_error validation_errors))
    else
      let media_urls_with_alt =
        List.mapi (fun i url ->
          let alt_text = try List.nth alt_texts i with _ -> None in
          (url, alt_text)
        ) media_urls
      in
      with_valid_credentials ~account_id
        (fun creds ->
          get_user_id ~access_token:creds.access_token
            (function
              | Error err -> on_result (Error_types.Failure err)
              | Ok user_id ->
                  create_carousel_children
                    ~access_token:creds.access_token
                    ~user_id
                    ~media_urls_with_alt
                    ~acc:[]
                    (function
                      | Error err -> on_result (Error_types.Failure err)
                      | Ok children_ids ->
                          create_carousel_container
                            ~access_token:creds.access_token
                            ~user_id
                            ~children_ids
                            ~text
                            ?topic_tag
                            ?reply_control
                            ?quoted_post_id
                            ?location_id
                            (function
                              | Error err ->
                                  on_result
                                    (Error_types.Failure
                                       (carousel_stage_error
                                          ~stage:"carousel parent container create failed"
                                          err))
                              | Ok carousel_id ->
                                  poll_container_status
                                    ~access_token:creds.access_token
                                    ~container_id:carousel_id
                                    (function
                                      | Error err ->
                                          on_result
                                            (Error_types.Failure
                                               (carousel_stage_error
                                                  ~stage:"carousel parent container processing failed"
                                                  err))
                                      | Ok _container_id ->
                                          publish_container
                                            ~access_token:creds.access_token
                                            ~user_id
                                            ~creation_id:carousel_id
                                            (function
                                              | Ok post_id -> on_result (Error_types.Success post_id)
                                              | Error err ->
                                                  on_result
                                                    (Error_types.Failure
                                                       (carousel_stage_error
                                                          ~stage:"carousel publish failed"
                                                          err))))))))
        (fun err -> on_result (Error_types.Failure err))

  let parse_posts_response response_body =
    let json = Yojson.Basic.from_string response_body in
    let open Yojson.Basic.Util in
    let posts =
      json |> member "data" |> to_list
      |> List.map parse_thread_post
    in
    let next_after =
      try
        Some
          (json
           |> member "paging"
           |> member "cursors"
           |> member "after"
           |> to_string)
      with _ -> None
    in
    (posts, next_after)

  let invalid_input message =
    Error_types.Validation_error [ Error_types.Invalid_url message ]

  (* URL of one page of a reply edge. A [Page_url] is used as Meta returned
     it (after the host and edge checks), with the caller's token in place of
     the one Meta echoed into it; the other positions rebuild the request
     from [media_id] and the options. *)
  let reply_edge_url ~edge ~fields ~extra_params ~access_token ~media_id
      ~position ?reverse ~limit () =
    match (position : Social_meta_graph_paging.position) with
    | Page_url url -> (
        match
          Social_meta_graph_paging.resolve_page_url ~allowed_hosts:paging_url_hosts
            ~edge url
        with
        | Ok uri ->
            Ok (Uri.to_string (Uri.add_query_param uri ("access_token", [ access_token ])))
        | Error message -> Error (invalid_input message))
    | First | After _ | Before _ ->
        if String.trim media_id = "" then
          Error (invalid_input "media_id must not be empty")
        else
          let limit = if limit < 1 then 1 else if limit > 100 then 100 else limit in
          let params =
            [ ("fields", [ fields ]); ("limit", [ string_of_int limit ]) ]
            @ (match reverse with
               | Some reverse -> [ ("reverse", [ string_of_bool reverse ]) ]
               | None -> [])
            @ extra_params
            @ [ ("access_token", [ access_token ]) ]
            @ Social_meta_graph_paging.cursor_query position
          in
          Ok
            (Printf.sprintf "%s/%s/%s?%s" threads_api_base (String.trim media_id)
               edge (Uri.encoded_of_query params))

  (* Threads documents cursors-only pages. Supply a followable URL so the
     shared Graph paging helpers work without changing Facebook's rules. *)
  let reply_page_with_cursor ~url (page : 'a Social_meta_graph_paging.page) =
    match page.data with
    | [] -> { page with next_url = None; paging = None }
    | _ :: _ ->
        match page.next_url, Option.bind page.paging (fun cursors -> cursors.after) with
        | None, Some after when String.trim after <> "" ->
            let uri = Uri.remove_query_param (Uri.of_string url) "before" in
            let uri = Uri.remove_query_param uri "after" in
            { page with next_url = Some (Uri.to_string (Uri.add_query_param uri ("after", [ after ]))) }
        | (Some _ | None), (Some _ | None) -> page

  (* Shared reader for the reply edges of a media object. They take the same
     paging parameters and return the same media-object page shape. *)
  let get_reply_edge_page ~edge ~fields ~extra_params ~access_token ~media_id
      ?(position = Social_meta_graph_paging.First) ?reverse ?(limit = 20) on_result =
    match
      reply_edge_url ~edge ~fields ~extra_params ~access_token ~media_id
        ~position ?reverse ~limit ()
    with
    | Error err -> on_result (Error err)
    | Ok url ->
        http_get_with_retry url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              match Social_meta_graph_paging.of_body ~item:parse_thread_post response.body with
              | Ok page -> on_result (Ok (reply_page_with_cursor ~url page))
              | Error msg ->
                  on_result
                    (Error
                       (Error_types.Internal_error
                          (Printf.sprintf "Failed to parse %s response: %s" edge msg)))
            else
              on_result
                (Error
                   (parse_api_error
                      ~status_code:response.status
                      ~headers:response.headers
                      ~response_body:response.body)))
          (fun err -> on_result (Error (network_error err)))

  (** Token-based [GET /{threads-user-id}/threads]. [fields] must include [id].
      See {!Posts} for the documented response and paging contract. *)
  let get_user_posts_page ~threads_user_id ~access_token ~fields
      ?(position = Social_meta_graph_paging.First) ?(limit = 20) on_result =
    match
      reply_edge_url ~edge:"threads" ~fields ~extra_params:[] ~access_token
        ~media_id:(Uri.pct_encode (String.trim threads_user_id))
        ~position ~limit ()
    with
    | Error err -> on_result (Error err)
    | Ok url ->
        http_get_with_retry url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              let item json =
                match Posts.of_json json with
                | Ok post -> post
                | Error message -> failwith message
              in
              match Social_meta_graph_paging.of_body ~item response.body with
              | Ok page -> on_result (Ok (reply_page_with_cursor ~url page))
              | Error message ->
                  on_result
                    (Error
                       (Error_types.Internal_error
                          ("Failed to parse threads response: " ^ message)))
            else
              on_result
                (Error
                   (parse_api_error ~status_code:response.status
                      ~headers:response.headers ~response_body:response.body)))
          (fun err -> on_result (Error (network_error err)))

  let pending_replies_extra_params = function
    | Some status ->
        [ ("approval_status", [ string_of_reply_approval_status status ]) ]
    | None -> []

  (** One page of the immediate replies to a post or reply
      ([GET /{media_id}/replies], fields {!reply_fields}), with a token.
      [reverse] (API default [true]) sorts newest first; [limit] is clamped
      to 1..100 (default 20). [position] selects the page by cursor or by a
      [paging.next] URL, see {!Social_meta_graph_paging}. When only an after
      cursor is returned on a nonempty page, [next_url] is built from the
      request options and cursor. Empty pages stop traversal. Use [has_replies] to
      decide whether to descend further. Needs [threads_read_replies].

      @see <https://developers.facebook.com/docs/threads/retrieve-and-manage-replies/replies-and-conversations> *)
  let get_replies_page ~access_token ~media_id ?position ?reverse ?limit on_result =
    get_reply_edge_page ~edge:"replies" ~fields:reply_fields ~extra_params:[]
      ~access_token ~media_id ?position ?reverse ?limit on_result

  (** One page of every reply under a top-level post, at any depth, as one
      flat list ([GET /{media_id}/conversation], fields {!reply_fields}).
      Rebuild the tree with [replied_to_id]; [root_post_id] names the
      top-level post. Options as {!get_replies_page}.

      @see <https://developers.facebook.com/docs/threads/retrieve-and-manage-replies/replies-and-conversations> *)
  let get_conversation_page ~access_token ~media_id ?position ?reverse ?limit
      on_result =
    get_reply_edge_page ~edge:"conversation" ~fields:reply_fields
      ~extra_params:[] ~access_token ~media_id ?position ?reverse ?limit
      on_result

  (** One page of the replies held for approval on a post that has reply
      approvals enabled ([GET /{media_id}/pending_replies], fields
      {!pending_reply_fields}). [approval_status] filters to pending or
      ignored replies; omitted, both are returned. Options as
      {!get_replies_page}.

      @see <https://developers.facebook.com/docs/threads/reply-management> *)
  let get_pending_replies_page ~access_token ~media_id ?position ?reverse
      ?approval_status ?limit on_result =
    get_reply_edge_page ~edge:"pending_replies" ~fields:pending_reply_fields
      ~extra_params:(pending_replies_extra_params approval_status)
      ~access_token ~media_id ?position ?reverse ?limit on_result

  (* The account-bound readers keep their original result: the items and
     [paging.cursors.after]. An empty page terminates traversal. *)
  let with_account_page ~account_id ~media_id ?after read on_result =
    if String.trim media_id = "" then
      on_result (Error (invalid_input "media_id must not be empty"))
    else
      with_valid_credentials ~account_id
        (fun creds ->
          let position =
            match normalize_optional_non_empty after with
            | Some cursor -> Social_meta_graph_paging.After cursor
            | None -> Social_meta_graph_paging.First
          in
          read ~access_token:creds.access_token ~position (function
            | Ok (page : thread_post Social_meta_graph_paging.page) ->
                on_result
                  (Ok
                     ( page.data,
                       Option.bind page.paging (fun (c : Social_meta_graph_paging.cursors) ->
                           c.after) ))
            | Error err -> on_result (Error err)))
        (fun err -> on_result (Error err))

  (** Immediate replies to a post or reply ([GET /{media_id}/replies]) with
      the account's stored credentials. Returns the items and
      [paging.cursors.after] while the page is nonempty. An empty page ends
      traversal. Token readers also support {!Social_meta_graph_paging.next_after}. *)
  let get_replies ~account_id ~media_id ?after ?reverse ?limit on_result =
    with_account_page ~account_id ~media_id ?after
      (fun ~access_token ~position ->
        get_replies_page ~access_token ~media_id ~position ?reverse ?limit)
      on_result

  (** All replies under a top-level post at any depth, flattened
      ([GET /{media_id}/conversation]), with the account's stored
      credentials. Result as {!get_replies}; see {!get_conversation_page}. *)
  let get_conversation ~account_id ~media_id ?after ?reverse ?limit on_result =
    with_account_page ~account_id ~media_id ?after
      (fun ~access_token ~position ->
        get_conversation_page ~access_token ~media_id ~position ?reverse ?limit)
      on_result

  (** Replies held for approval ([GET /{media_id}/pending_replies]), with the
      account's stored credentials. Result as {!get_replies}; see
      {!get_pending_replies_page}. *)
  let get_pending_replies ~account_id ~media_id ?after ?reverse ?approval_status
      ?limit on_result =
    with_account_page ~account_id ~media_id ?after
      (fun ~access_token ~position ->
        get_pending_replies_page ~access_token ~media_id ~position ?reverse
          ?approval_status ?limit)
      on_result

  let get_publishing_limit ~account_id on_result =
    with_valid_credentials ~account_id
      (fun creds ->
        get_user_id ~access_token:creds.access_token
          (function
            | Error err -> on_result (Error err)
            | Ok user_id ->
                let query =
                  Uri.encoded_of_query
                    [ ("fields", ["quota_usage,config"]);
                      ("access_token", [creds.access_token]) ]
                in
                let url =
                  Printf.sprintf "%s/%s/threads_publishing_limit?%s" threads_api_base user_id query
                in
                http_get_with_retry url
                  (fun response ->
                    if response.status >= 200 && response.status < 300 then
                      try
                        let json = Yojson.Basic.from_string response.body in
                        let open Yojson.Basic.Util in
                        let data = json |> member "data" |> to_list in
                        let item =
                          match data with
                          | first :: _ -> first
                          | [] -> json
                        in
                        let quota_usage =
                          try item |> member "quota_usage" |> to_int
                          with _ -> 0
                        in
                        let config_obj =
                          try item |> member "config"
                          with _ -> `Null
                        in
                        let quota_total =
                          try config_obj |> member "quota_total" |> to_int
                          with _ -> 250
                        in
                        let reply_quota_usage =
                          try config_obj |> member "reply_quota_usage" |> to_int
                          with _ ->
                            (try item |> member "reply_quota_usage" |> to_int
                             with _ -> 0)
                        in
                        let reply_quota_total =
                          try config_obj |> member "reply_quota_total" |> to_int
                          with _ -> 1000
                        in
                        on_result
                          (Ok
                             {
                               quota_usage;
                               quota_total;
                               reply_quota_usage;
                               reply_quota_total;
                             })
                      with exn ->
                        on_result
                          (Error
                             (Error_types.Internal_error
                                (Printf.sprintf
                                   "Failed to parse publishing limit response: %s"
                                   (Printexc.to_string exn))))
                    else
                      on_result
                        (Error
                           (parse_api_error
                              ~status_code:response.status
                              ~headers:response.headers
                              ~response_body:response.body)))
                  (fun err -> on_result (Error (network_error err)))))
      (fun err -> on_result (Error err))

  let delete_post ~account_id ~post_id on_result =
    if String.trim post_id = "" then
      on_result (Error (Error_types.Validation_error [ Error_types.Invalid_url "post_id must not be empty" ]))
    else
    with_valid_credentials ~account_id
      (fun creds ->
        let query =
          Uri.encoded_of_query
            [ ("access_token", [creds.access_token]) ]
        in
        let url = Printf.sprintf "%s/%s?%s" threads_api_base (String.trim post_id) query in
        http_delete url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_result (Ok ())
            else
              on_result
                (Error
                   (parse_api_error
                      ~status_code:response.status
                      ~headers:response.headers
                      ~response_body:response.body)))
          (fun err -> on_result (Error (network_error err))))
      (fun err -> on_result (Error err))

  (* Reply moderation goes through two dedicated edges on the reply node
     (https://developers.facebook.com/docs/threads/reply-management):
     [manage_reply] with [hide=true|false] for hiding a published top-level
     reply, and [manage_pending_reply] with [approve=true|false] for replies
     held for approval. *)
  type reply_moderation_edge =
    | Manage_reply
    | Manage_pending_reply

  let reply_moderation_edge_path = function
    | Manage_reply -> "manage_reply"
    | Manage_pending_reply -> "manage_pending_reply"

  let reply_moderation ~access_token ~reply_id ~edge ~params on_result =
    if String.trim reply_id = "" then
      on_result (Error (invalid_input "reply_id must not be empty"))
    else
      let body =
        Social_core.form_urlencode_kvs (params @ [ ("access_token", access_token) ])
      in
      let url =
        Printf.sprintf "%s/%s/%s" threads_api_base (String.trim reply_id)
          (reply_moderation_edge_path edge)
      in
      let headers = [ ("Content-Type", "application/x-www-form-urlencoded") ] in
      http_post_no_retry ~headers ~body url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            on_result (Ok ())
          else
            on_result
              (Error
                 (parse_api_error
                    ~status_code:response.status
                    ~headers:response.headers
                    ~response_body:response.body)))
        (fun err -> on_result (Error (network_error err)))

  (** Hide ([hide:true]) or unhide a top-level reply with a token
      ([POST /{reply_id}/manage_reply]). Nested replies follow their parent
      and cannot be hidden on their own. Needs [threads_manage_replies].

      @see <https://developers.facebook.com/docs/threads/reply-management> *)
  let manage_reply ~access_token ~reply_id ~hide on_result =
    reply_moderation ~access_token ~reply_id ~edge:Manage_reply
      ~params:[ ("hide", string_of_bool hide) ] on_result

  (** Approve ([approve:true]) or ignore a reply held for approval with a
      token ([POST /{reply_id}/manage_pending_reply]). Ignored replies can
      still be approved later. Needs [threads_manage_replies].

      @see <https://developers.facebook.com/docs/threads/reply-management> *)
  let manage_pending_reply ~access_token ~reply_id ~approve on_result =
    reply_moderation ~access_token ~reply_id ~edge:Manage_pending_reply
      ~params:[ ("approve", string_of_bool approve) ] on_result

  let reply_action ~account_id ~reply_id ~edge ~params on_result =
    if String.trim reply_id = "" then
      on_result (Error (invalid_input "reply_id must not be empty"))
    else
      with_valid_credentials ~account_id
        (fun creds ->
          reply_moderation ~access_token:creds.access_token ~reply_id ~edge
            ~params on_result)
        (fun err -> on_result (Error err))

  (** Approve a reply held for approval
      ([POST /{reply_id}/manage_pending_reply], [approve=true]). *)
  let approve_reply ~account_id ~reply_id on_result =
    reply_action ~account_id ~reply_id ~edge:Manage_pending_reply
      ~params:[ ("approve", "true") ] on_result

  (** Ignore a reply held for approval
      ([POST /{reply_id}/manage_pending_reply], [approve=false]). Ignored
      replies can still be approved later. *)
  let ignore_reply ~account_id ~reply_id on_result =
    reply_action ~account_id ~reply_id ~edge:Manage_pending_reply
      ~params:[ ("approve", "false") ] on_result

  (** Hide a top-level reply ([POST /{reply_id}/manage_reply], [hide=true]).
      Nested replies follow their parent. *)
  let hide_reply ~account_id ~reply_id on_result =
    reply_action ~account_id ~reply_id ~edge:Manage_reply
      ~params:[ ("hide", "true") ] on_result

  (** Unhide a top-level reply ([POST /{reply_id}/manage_reply], [hide=false]). *)
  let unhide_reply ~account_id ~reply_id on_result =
    reply_action ~account_id ~reply_id ~edge:Manage_reply
      ~params:[ ("hide", "false") ] on_result

  let get_oauth_url ~redirect_uri ~state ~scopes on_success on_error =
    let raw_client_id = Config.get_env "THREADS_CLIENT_ID" |> Option.value ~default:"" in
    let raw_configured_redirect_uri =
      Config.get_env "THREADS_REDIRECT_URI" |> Option.value ~default:""
    in
    let client_id = String.trim raw_client_id in
    let configured_redirect_uri =
      let v = String.trim raw_configured_redirect_uri in
      if v = "" then None else Some v
    in
    if client_id = "" then
      on_error "Threads client ID not configured"
    else if has_surrounding_whitespace raw_client_id then
      on_error "Threads client ID must not contain leading or trailing whitespace"
    else if has_surrounding_whitespace raw_configured_redirect_uri then
      on_error "Configured Threads redirect URI must not contain leading or trailing whitespace"
    else if is_blank redirect_uri then
      on_error "Threads redirect URI must not be empty"
    else if has_surrounding_whitespace redirect_uri then
      on_error "Threads redirect URI must not contain leading or trailing whitespace"
    else if is_blank state then
      on_error "Threads OAuth state must not be empty"
    else if has_surrounding_whitespace state then
      on_error "Threads OAuth state must not contain leading or trailing whitespace"
    else
      (match configured_redirect_uri with
       | Some configured when configured <> redirect_uri ->
           on_error "Threads redirect URI must exactly match THREADS_REDIRECT_URI"
       | _ ->
           let url =
             OAuth.get_authorization_url
               ~client_id
               ~redirect_uri
               ~state
               ~scopes
           in
           on_success url)

  let exchange_code ~code ~redirect_uri on_success on_error =
    let raw_client_id = Config.get_env "THREADS_CLIENT_ID" |> Option.value ~default:"" in
    let raw_client_secret = Config.get_env "THREADS_CLIENT_SECRET" |> Option.value ~default:"" in
    let raw_configured_redirect_uri =
      Config.get_env "THREADS_REDIRECT_URI" |> Option.value ~default:""
    in
    let configured_redirect_uri =
      let v = String.trim raw_configured_redirect_uri in
      if v = "" then None else Some v
    in
    let client_id = String.trim raw_client_id in
    let client_secret = String.trim raw_client_secret in
    if client_id = "" || client_secret = "" then
      on_error "Threads OAuth credentials not configured"
    else if has_surrounding_whitespace raw_client_id || has_surrounding_whitespace raw_client_secret then
      on_error "Threads OAuth credentials must not contain leading or trailing whitespace"
    else if has_surrounding_whitespace raw_configured_redirect_uri then
      on_error "Configured Threads redirect URI must not contain leading or trailing whitespace"
    else if is_blank code then
      on_error "Threads authorization code must not be empty"
    else if has_surrounding_whitespace code then
      on_error "Threads authorization code must not contain leading or trailing whitespace"
    else if is_blank redirect_uri then
      on_error "Threads redirect URI must not be empty"
    else if has_surrounding_whitespace redirect_uri then
      on_error "Threads redirect URI must not contain leading or trailing whitespace"
    else if
      (match configured_redirect_uri with
       | Some configured -> configured <> redirect_uri
       | None -> false)
    then
      on_error "Threads redirect URI must exactly match THREADS_REDIRECT_URI"
    else
      OAuth_http.exchange_code
        ~client_id
        ~client_secret
        ~redirect_uri
        ~code
        on_success
        on_error

  let exchange_for_long_lived_token ~short_lived_token on_success on_error =
    let raw_client_secret = Config.get_env "THREADS_CLIENT_SECRET" |> Option.value ~default:"" in
    let client_secret = String.trim raw_client_secret in
    if client_secret = "" then
      on_error "Threads client secret not configured"
    else if has_surrounding_whitespace raw_client_secret then
      on_error "Threads client secret must not contain leading or trailing whitespace"
    else if is_blank short_lived_token then
      on_error "Threads short-lived token must not be empty"
    else if has_surrounding_whitespace short_lived_token then
      on_error "Threads short-lived token must not contain leading or trailing whitespace"
    else
      OAuth_http.exchange_for_long_lived_token
        ~client_secret
        ~short_lived_token
        on_success
        on_error

  let refresh_token ~long_lived_token on_success on_error =
    if is_blank long_lived_token then
      on_error "Threads long-lived token must not be empty"
    else if has_surrounding_whitespace long_lived_token then
      on_error "Threads long-lived token must not contain leading or trailing whitespace"
    else
      OAuth_http.refresh_token ~long_lived_token on_success on_error

  let refresh_account_credentials ~account_id on_success on_error =
    Config.get_credentials ~account_id
      (fun creds ->
        let token = String.trim creds.access_token in
        if token = "" then
          on_error "Threads credentials missing for refresh"
        else
          OAuth_http.refresh_token ~long_lived_token:token
            (fun refreshed_creds ->
              let refreshed_token = String.trim refreshed_creds.access_token in
              if refreshed_token = "" then
                on_error "Threads refresh returned empty access token"
              else
                let normalized_refreshed =
                  { refreshed_creds with access_token = refreshed_token }
                in
                Config.update_credentials ~account_id ~credentials:normalized_refreshed
                  (fun () -> on_success normalized_refreshed)
                  (fun err ->
                    on_error
                      (Printf.sprintf
                         "Threads credentials refresh succeeded but persistence failed: %s"
                         err)))
            (fun err -> on_error ("Threads refresh failed: " ^ err)))
      (fun err ->
        match classify_credentials_error err with
        | `Missing -> on_error "Threads credentials missing for refresh"
        | `Backend ->
            on_error
              (Printf.sprintf
                 "Threads credentials refresh load failed: %s"
                 err))

  let validate_oauth_state ~expected ~received on_success on_error =
    if String.trim expected = "" || String.trim received = "" then
      on_error "OAuth state must not be empty"
    else if has_surrounding_whitespace expected || has_surrounding_whitespace received then
      on_error "OAuth state must not contain leading or trailing whitespace"
    else if not (constant_time_equal expected received) then
      on_error "OAuth state mismatch"
    else
      on_success ()

  let get_me ~account_id on_result =
    with_valid_credentials ~account_id
      (fun creds -> fetch_me_with_access_token ~access_token:creds.access_token on_result)
      (fun err -> on_result (Error err))

  let get_posts ~account_id ?after ?(limit = 20) on_result =
    with_valid_credentials ~account_id
      (fun creds ->
        get_user_id ~access_token:creds.access_token
          (function
            | Error err -> on_result (Error err)
            | Ok user_id ->
                let limit = if limit < 1 then 1 else if limit > 100 then 100 else limit in
                let params =
                  let normalized_after = normalize_optional_non_empty after in
                  [ ("fields", [post_fields]);
                    ("limit", [string_of_int limit]);
                    ("access_token", [creds.access_token]) ]
                  @
                  (match normalized_after with
                   | Some cursor -> [ ("after", [cursor]) ]
                   | None -> [])
                in
                let query = Uri.encoded_of_query params in
                let url = Printf.sprintf "%s/%s/threads?%s" threads_api_base user_id query in
                http_get_with_retry url
                  (fun response ->
                    if response.status >= 200 && response.status < 300 then
                      (try
                        let posts, next_after = parse_posts_response response.body in
                        on_result (Ok (posts, next_after))
                      with exn ->
                        on_result
                          (Error
                             (Error_types.Internal_error
                                (Printf.sprintf "Failed to parse posts response: %s" (Printexc.to_string exn)))))
                    else
                      on_result
                        (Error
                           (parse_api_error
                              ~status_code:response.status
                              ~headers:response.headers
                              ~response_body:response.body)))
                  (fun err -> on_result (Error (network_error err)))))
      (fun err -> on_result (Error err))

  (** Account insights as [account_insight] records ([views], [likes],
      [replies], [reposts], [quotes]; total-value metrics become one value
      without [end_time]).

      Deprecated: new code should use {!get_user_insights_for_account},
      which also covers [clicks] and [followers_count] and keeps the
      response shape ({!Insights.value}). *)
  let get_account_insights ~account_id ~since ~until on_result =
    with_valid_credentials ~account_id
      (fun creds ->
        get_user_id ~access_token:creds.access_token
          (function
            | Error err -> on_result (Error err)
            | Ok user_id ->
                let metrics_param = String.concat "," account_insight_metric_names in
                let query =
                  Uri.encoded_of_query
                    [ ("metric", [metrics_param]);
                      ("period", ["day"]);
                      ("since", [String.trim since]);
                      ("until", [String.trim until]);
                      ("access_token", [creds.access_token]) ]
                in
                let url =
                  Printf.sprintf "%s/%s/threads_insights?%s" threads_api_base user_id query
                in
                http_get_with_retry url
                  (fun response ->
                    if response.status >= 200 && response.status < 300 then
                      (match parse_account_insights_response response.body with
                       | Ok insights -> on_result (Ok insights)
                       | Error msg ->
                           on_result
                             (Error
                                (Error_types.Internal_error
                                   (Printf.sprintf
                                      "Failed to parse account insights response: %s"
                                      msg))))
                    else
                      on_result
                        (Error
                           (parse_api_error
                              ~status_code:response.status
                              ~headers:response.headers
                              ~response_body:response.body)))
                  (fun err -> on_result (Error (network_error err)))))
      (fun err -> on_result (Error err))

  let get_account_insights_canonical ~account_id ~since ~until on_result =
    get_account_insights ~account_id ~since ~until
      (function
        | Ok insights -> on_result (Ok (to_canonical_account_insights_series insights))
        | Error err -> on_result (Error err))

  let get_post_insights ~account_id ~post_id on_result =
    with_valid_credentials ~account_id
      (fun creds ->
        let metrics_param = String.concat "," insight_metric_names in
        let query =
          Uri.encoded_of_query
            [ ("metric", [metrics_param]);
              ("access_token", [creds.access_token]) ]
        in
        let url =
          Printf.sprintf
            "%s/%s/insights?%s"
            threads_api_base
            (String.trim post_id)
            query
        in
        http_get_with_retry url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              (match parse_post_insights_response response.body with
               | Ok insights -> on_result (Ok insights)
               | Error msg ->
                   on_result
                     (Error
                        (Error_types.Internal_error
                           (Printf.sprintf
                              "Failed to parse post insights response: %s"
                              msg))))
            else
              on_result
                (Error
                   (parse_api_error
                      ~status_code:response.status
                      ~headers:response.headers
                      ~response_body:response.body)))
          (fun err -> on_result (Error (network_error err))))
      (fun err -> on_result (Error err))

  let get_post_insights_canonical ~account_id ~post_id on_result =
    get_post_insights ~account_id ~post_id
      (function
        | Ok insights -> on_result (Ok (to_canonical_post_insights_series insights))
        | Error err -> on_result (Error err))

  let get_insights_url url on_result =
    http_get_with_retry url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          match Insights.parse_insights response.body with
          | Ok insights -> on_result (Ok insights)
          | Error msg ->
              on_result
                (Error
                   (Error_types.Internal_error
                      (Printf.sprintf "Failed to parse insights response: %s" msg)))
        else
          on_result
            (Error
               (parse_api_error ~status_code:response.status
                  ~headers:response.headers ~response_body:response.body)))
      (fun err -> on_result (Error (network_error err)))

  let metric_param metrics = String.concat "," (List.map Insights.metric_to_string metrics)

  (** User insights: [GET /{user_id}/threads_insights?metric=...]
      ({!Insights.user_metrics} by default). [since] / [until] are a Unix
      timestamp or a date ([YYYY-MM-DD]); omitted, Meta reports yesterday
      through today. No [period] is sent: the endpoint documents none.
      Each metric keeps the shape Meta sent ({!Insights.value}): [views] a
      time series, [followers_count] and the interaction counts a total,
      [clicks] per-URL totals. Needs [threads_manage_insights].

      @see <https://developers.facebook.com/docs/threads/insights> *)
  let get_user_insights ~access_token ~user_id ?(metrics = Insights.user_metrics) ?since
      ?until on_result =
    let optional key = function
      | Some value -> [ (key, [ String.trim value ]) ]
      | None -> []
    in
    let query =
      Uri.encoded_of_query
        ([ ("metric", [ metric_param metrics ]) ]
        @ optional "since" since @ optional "until" until
        @ [ ("access_token", [ access_token ]) ])
    in
    get_insights_url
      (Printf.sprintf "%s/%s/threads_insights?%s" threads_api_base
         (Uri.pct_encode (String.trim user_id)) query)
      on_result

  (** {!get_user_insights} with the account's stored (and, if configured,
      refreshed) credentials. Pass [?user_id] when it is known; otherwise it
      is read from [GET /me] first. *)
  let get_user_insights_for_account ~account_id ?user_id ?metrics ?since ?until on_result =
    with_valid_credentials ~account_id
      (fun creds ->
        let access_token = creds.access_token in
        match user_id with
        | Some user_id ->
            get_user_insights ~access_token ~user_id ?metrics ?since ?until on_result
        | None ->
            get_user_id ~access_token (function
              | Ok user_id ->
                  get_user_insights ~access_token ~user_id ?metrics ?since ?until on_result
              | Error err -> on_result (Error err)))
      (fun err -> on_result (Error err))

  (** Media insights: [GET /{media_id}/insights?metric=...]
      ({!Insights.media_metrics} by default). Counts do not include nested
      replies' metrics; a repost facade answers with an empty list.

      @see <https://developers.facebook.com/docs/threads/insights> *)
  let get_media_insights ~access_token ~media_id ?(metrics = Insights.media_metrics) on_result =
    let query =
      Uri.encoded_of_query
        [ ("metric", [ metric_param metrics ]); ("access_token", [ access_token ]) ]
    in
    get_insights_url
      (Printf.sprintf "%s/%s/insights?%s" threads_api_base
         (Uri.pct_encode (String.trim media_id)) query)
      on_result

  (** {!get_media_insights} with the account's stored credentials. *)
  let get_media_insights_for_account ~account_id ~media_id ?metrics on_result =
    with_valid_credentials ~account_id
      (fun creds ->
        get_media_insights ~access_token:creds.access_token ~media_id ?metrics on_result)
      (fun err -> on_result (Error err))

  (** Permalink of a post or reply by id: [GET /{post_id}?fields=permalink].
      [Ok None] when Meta returns no permalink for the object. Takes a token,
      so it works without stored credentials; reads go through the same
      retry policy as the other readers.

      @see <https://developers.facebook.com/docs/threads/threads-media> *)
  let get_permalink ~access_token ~post_id on_result =
    let query =
      Uri.encoded_of_query
        [ ("fields", [ "permalink" ]); ("access_token", [ access_token ]) ]
    in
    let url =
      Printf.sprintf "%s/%s?%s" threads_api_base (String.trim post_id) query
    in
    http_get_with_retry url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          match parse_permalink_response response.body with
          | Ok permalink -> on_result (Ok permalink)
          | Error msg ->
              on_result
                (Error
                   (Error_types.Internal_error
                      (Printf.sprintf "Failed to parse permalink response: %s"
                         msg)))
        else
          on_result
            (Error
               (parse_api_error ~status_code:response.status
                  ~headers:response.headers ~response_body:response.body)))
      (fun err -> on_result (Error (network_error err)))

  (** {!get_permalink} with the account's stored (and, if configured,
      refreshed) credentials. *)
  let get_permalink_for_account ~account_id ~post_id on_result =
    with_valid_credentials ~account_id
      (fun creds ->
        get_permalink ~access_token:creds.access_token ~post_id on_result)
      (fun err -> on_result (Error err))

  (* [GET /location_search] query: the text, the point, or both, plus every
     documented field (the endpoint returns only [id] by default). *)
  let location_search_query ~access_token ?query ?coordinates () =
    let text =
      match Option.map String.trim query with
      | Some q when q <> "" -> [ ("q", [ q ]) ]
      | Some _ | None -> []
    in
    let point =
      match (coordinates : Locations.coordinates option) with
      | Some { latitude; longitude } ->
          [ ("latitude", [ Printf.sprintf "%.6f" latitude ]);
            ("longitude", [ Printf.sprintf "%.6f" longitude ]) ]
      | None -> []
    in
    match text, point with
    | [], [] -> Error (Error_types.Validation_error [ Error_types.Text_empty ])
    | _, _ ->
        Ok (text @ point @ [ ("fields", [ Locations.fields ]); ("access_token", [ access_token ]) ])

  (** Places to tag a post with: [GET /location_search] with a text
      [?query], a point [?coordinates], or both (at least one, otherwise
      [Validation_error [Text_empty]] and no request). Requests every documented field
      ({!Locations.fields}); results are not paginated. A result's [id] is
      the [location_id] that [post_single] and the container calls take.
      Takes a token, so it works without stored credentials; the read goes
      through the same retry policy as the other readers.

      Needs [threads_basic] and [threads_location_tagging].

      @see <https://developers.facebook.com/docs/threads/reference/location-search>
      @see <https://developers.facebook.com/docs/threads/create-posts/location-tagging> *)
  let search_locations ~access_token ?query ?coordinates on_result =
    match location_search_query ~access_token ?query ?coordinates () with
    | Error err -> on_result (Error err)
    | Ok params ->
        let url =
          Printf.sprintf "%s/location_search?%s" threads_api_base
            (Uri.encoded_of_query params)
        in
        http_get_with_retry url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              match Locations.parse_response response.body with
              | Ok locations -> on_result (Ok locations)
              | Error msg ->
                  on_result
                    (Error
                       (Error_types.Internal_error
                          (Printf.sprintf "Failed to parse location search response: %s" msg)))
            else
              on_result
                (Error
                   (parse_api_error ~status_code:response.status
                      ~headers:response.headers ~response_body:response.body)))
          (fun err -> on_result (Error (network_error err)))

  (** {!search_locations} with the account's stored (and, if configured,
      refreshed) credentials. *)
  let search_locations_for_account ~account_id ?query ?coordinates on_result =
    with_valid_credentials ~account_id
      (fun creds ->
        search_locations ~access_token:creds.access_token ?query ?coordinates on_result)
      (fun err -> on_result (Error err))

  let post_single
      ~account_id
      ~text
      ~media_urls
      ?(alt_texts=[])
      ?idempotency_key
      ?reply_control
      ?topic_tag
      ?quoted_post_id
      ?location_id
      ?(is_ghost_post = false)
      ?poll_options
      on_result =
    let validation_errors =
      [ validate_post_content ~text ~media_urls;
        validate_media_urls media_urls;
        validate_topic_tag topic_tag ]
      |> List.filter_map (fun x -> x)
    in
    let validation_errors =
      match alt_texts with
      | [] -> validation_errors
      | first :: _ ->
          (match validate_alt_text first with
           | Some err -> err :: validation_errors
           | None -> validation_errors)
    in
    let validation_errors =
      match poll_options with
      | None -> validation_errors
      | Some options ->
          let poll_errors = validate_poll_options options in
          let media_conflict =
            if media_urls <> [] then
              [ Error_types.Option_conflict "polls cannot be combined with media" ]
            else
              []
          in
          media_conflict @ poll_errors @ validation_errors
    in
    let validation_errors =
      if is_ghost_post && media_urls <> [] then
        (Error_types.Option_conflict "ghost posts must be text-only") :: validation_errors
      else
        validation_errors
    in
    if validation_errors <> [] then
      on_result (Error_types.Failure (Error_types.Validation_error validation_errors))
    else
      let use_auto_publish = media_urls = [] && poll_options = None in
      with_valid_credentials ~account_id
        (fun creds ->
          let on_user_id = function
            | Error err -> on_result (Error_types.Failure err)
            | Ok user_id ->
              if use_auto_publish then
                create_text_container
                  ~access_token:creds.access_token
                  ~user_id
                  ~text
                  ?idempotency_key
                  ?reply_control
                  ?topic_tag
                  ?quoted_post_id
                  ?location_id
                  ~auto_publish:true
                  (function
                    | Ok post_id -> on_result (Error_types.Success post_id)
                    | Error err -> on_result (Error_types.Failure err))
              else
                let on_container = function
                  | Error err -> on_result (Error_types.Failure err)
                  | Ok creation_id ->
                        publish_container ~access_token:creds.access_token ~user_id ~creation_id
                          (function
                            | Ok post_id -> on_result (Error_types.Success post_id)
                            | Error err -> on_result (Error_types.Failure err))
                  in
                  let alt_text = match alt_texts with x :: _ -> x | [] -> None in
                  create_container_for_post
                    ~access_token:creds.access_token
                    ~user_id
                    ~text
                    ~media_urls
                    ?alt_text
                    ?idempotency_key
                    ?reply_control
                    ?topic_tag
                    ?quoted_post_id
                    ?location_id
                    ~is_ghost_post
                    ?poll_options
                    on_container
          in
          get_user_id ~access_token:creds.access_token on_user_id)
        (fun err -> on_result (Error_types.Failure err))

  let post_thread
      ~account_id
      ~texts
      ~media_urls_per_post
      ?(alt_texts_per_post=[])
      ?idempotency_key
      ?reply_control
      ?topic_tag
      ?quoted_post_id
      ?location_id
      on_result =
    if texts = [] then
      on_result (Error_types.Failure (Error_types.Validation_error [ Error_types.Thread_empty ]))
    else
      let rec drop n xs =
        if n <= 0 then xs
        else
          match xs with
          | [] -> []
          | _ :: rest -> drop (n - 1) rest
      in
      let extra_media_entries =
        drop (List.length texts) media_urls_per_post
      in
      if extra_media_entries <> [] then
        on_result
          (Error_types.Failure
             (Error_types.Validation_error
                [
                  Error_types.Too_many_media
                    {
                      count = List.length media_urls_per_post;
                      max = List.length texts;
                    };
                ]))
      else
      let normalized_thread_idempotency = normalize_idempotency_key idempotency_key in
      let media_urls_for_index i =
        try List.nth media_urls_per_post i
        with _ -> []
      in
      let alt_text_for_index i =
        try
          match List.nth alt_texts_per_post i with
          | x :: _ -> x
          | [] -> None
        with _ -> None
      in
      let thread_validation_errors =
        let base_errors =
          texts
          |> List.mapi (fun index text ->
                 let post_errors =
                   [ validate_post_content ~text ~media_urls:(media_urls_for_index index);
                     validate_media_urls (media_urls_for_index index) ]
                   |> List.filter_map (fun x -> x)
                 in
                 if post_errors = [] then None
                 else Some (Error_types.Thread_post_invalid { index; errors = post_errors }))
          |> List.filter_map (fun x -> x)
        in
        let topic_tag_error =
          match validate_topic_tag topic_tag with
          | Some err -> [err]
          | None -> []
        in
        base_errors @ topic_tag_error
      in
      if thread_validation_errors <> [] then
        on_result (Error_types.Failure (Error_types.Validation_error thread_validation_errors))
      else
        with_valid_credentials ~account_id
          (fun creds ->
            let chain_failure_result ~index ~posted_ids ~total_requested err =
              if posted_ids = [] then
                Error_types.Failure err
              else
                Error_types.Partial_success
                  {
                    result =
                      {
                        Error_types.posted_ids = List.rev posted_ids;
                        failed_at_index = Some index;
                        total_requested;
                      };
                    warnings =
                      [
                        Error_types.Generic_warning
                          {
                            code = "threads_chain_failed";
                            message = Error_types.error_to_string err;
                            recoverable = false;
                          };
                      ];
                  }
            in
            let on_user_id = function
              | Error err -> on_result (Error_types.Failure err)
              | Ok user_id ->
                  let total_requested = List.length texts in
                  let rec post_chain index previous_post_id posted_ids remaining_texts =
                    match remaining_texts with
                    | [] ->
                        on_result
                          (Error_types.Success
                             {
                               Error_types.posted_ids = List.rev posted_ids;
                               failed_at_index = None;
                               total_requested;
                             })
                    | text :: rest ->
                        let alt_text = alt_text_for_index index in
                        create_container_for_post
                          ~access_token:creds.access_token
                          ~user_id
                          ~text
                          ~media_urls:(media_urls_for_index index)
                          ?alt_text
                          ?reply_to_id:previous_post_id
                          ?idempotency_key:(match normalized_thread_idempotency with
                            | Some base -> Some (base ^ "-" ^ string_of_int index)
                            | _ -> None)
                          ?reply_control
                          ?topic_tag
                          ?quoted_post_id:(if index = 0 then quoted_post_id else None)
                          ?location_id:(if index = 0 then location_id else None)
                          (function
                            | Error err ->
                                on_result
                                  (chain_failure_result ~index ~posted_ids ~total_requested err)
                            | Ok creation_id ->
                                publish_container
                                  ~access_token:creds.access_token
                                  ~user_id
                                  ~creation_id
                                  (function
                                    | Error err ->
                                        on_result
                                          (chain_failure_result ~index ~posted_ids ~total_requested err)
                                    | Ok post_id ->
                                        post_chain (index + 1) (Some post_id) (post_id :: posted_ids) rest))
                  in
                  post_chain 0 None [] texts
            in
            get_user_id ~access_token:creds.access_token on_user_id)
          (fun err -> on_result (Error_types.Failure err))
end
