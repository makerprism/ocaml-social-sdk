open Social_core

let threads_api_base = "https://graph.threads.net/v1.0"
let threads_token_exchange_endpoint = "https://graph.threads.net/v1.0/access_token"
let threads_token_refresh_endpoint = "https://graph.threads.net/v1.0/refresh_access_token"

let parse_expires_in json =
  let normalize_expires_in seconds =
    if seconds < 0 then 0 else if seconds > 31536000 then 31536000 else seconds
  in
  let normalize_expires_in_float value =
    match classify_float value with
    | FP_nan -> None
    | FP_infinite ->
        if value > 0.0 then Some 31536000 else Some 0
    | FP_zero | FP_normal | FP_subnormal ->
        let normalized_value = if value > 0.0 then ceil value else value in
        Some (normalize_expires_in (int_of_float normalized_value))
  in
  let open Yojson.Basic.Util in
  try Some (json |> member "expires_in" |> to_int |> normalize_expires_in)
  with _ ->
    try
      json
      |> member "expires_in"
      |> to_float
      |> normalize_expires_in_float
    with _ -> None

let expires_at_of_expires_in expires_in =
  match expires_in with
  | None -> None
  | Some seconds ->
      let now = Ptime_clock.now () in
      (match Ptime.add_span now (Ptime.Span.of_int_s seconds) with
       | Some expiry -> Some (Ptime.to_rfc3339 expiry)
       | None -> None)

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
      Ok ({ access_token; refresh_token = None; expires_at; auth_type = auth_type_of_string token_type_str } : credentials)
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
             if is_sensitive_key key then (key, `String "[REDACTED]")
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

let parse_api_error ~status_code ~headers ~response_body =
  let sanitized_response_body = sanitize_response_body response_body in
  if status_code = 429 then
    Error_types.Rate_limited {
      retry_after_seconds = parse_retry_after headers;
      limit = None;
      remaining = Some 0;
      reset_at = None;
    }
  else
    try
      let json = Yojson.Basic.from_string response_body in
      let open Yojson.Basic.Util in
      let err = json |> member "error" in
      let message =
        try err |> member "message" |> to_string
        with _ -> sanitized_response_body
      in
      let message = sanitize_text_if_sensitive message in
      let code =
        try err |> member "code" |> to_int
        with _ -> 0
      in
      let provider_type =
        try err |> member "type" |> to_string
        with _ -> ""
      in
      let provider_subcode =
        try Some (err |> member "error_subcode" |> to_int)
        with _ -> None
      in
      let provider_metadata_suffix =
        if code <> 0 || provider_type <> "" || provider_subcode <> None then
          Printf.sprintf
            " [code=%d%s%s]"
            code
            (if provider_type = "" then "" else ";type=" ^ provider_type)
            (match provider_subcode with
             | Some subcode -> Printf.sprintf ";subcode=%d" subcode
             | None -> "")
        else
          ""
      in
      match code with
      | 190 -> Error_types.Auth_error Error_types.Token_expired
      | 102 -> Error_types.Auth_error Error_types.Token_invalid
      | 10 | 200 -> Error_types.Auth_error (Error_types.Insufficient_permissions ["threads_basic"])
      | 4 | 32 | 613 ->
          Error_types.Rate_limited {
            retry_after_seconds = parse_retry_after headers;
            limit = None;
            remaining = Some 0;
            reset_at = None;
          }
      | _ ->
          Error_types.Api_error {
            status_code;
            message = message ^ provider_metadata_suffix;
            platform = Platform_types.Threads;
            raw_response = Some sanitized_response_body;
            request_id = request_id_of_headers headers;
          }
    with _ ->
      Error_types.Api_error {
        status_code;
        message = sanitized_response_body;
        platform = Platform_types.Threads;
        raw_response = Some sanitized_response_body;
        request_id = request_id_of_headers headers;
      }

let network_error err =
  Error_types.Network_error (Error_types.Connection_failed (sanitize_text_if_sensitive err))

module OAuth = struct
  module Scopes = struct
    let basic = ["threads_basic"]
    let publish = ["threads_basic"; "threads_content_publish"]
    let read = ["threads_basic"]
  end

  let authorization_endpoint = "https://www.threads.com/oauth/authorize"
  let token_endpoint = "https://graph.threads.net/v1.0/oauth/access_token"

  let get_authorization_url ~client_id ~redirect_uri ~state ~scopes =
    let scope = String.concat "," scopes in
    let query =
      Uri.encoded_of_query
        [ ("client_id", [client_id]);
          ("redirect_uri", [redirect_uri]);
          ("scope", [scope]);
          ("response_type", ["code"]);
          ("state", [state]) ]
    in
    Printf.sprintf "%s?%s" authorization_endpoint query

  module Make (Http : HTTP_CLIENT) = struct
    let exchange_code ~client_id ~client_secret ~redirect_uri ~code on_success on_error =
      let body =
        Uri.encoded_of_query
          [ ("client_id", [client_id]);
            ("client_secret", [client_secret]);
            ("grant_type", ["authorization_code"]);
            ("redirect_uri", [redirect_uri]);
            ("code", [code]) ]
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
  end
end

type thread_user = {
  id : string;
  username : string option;
  name : string option;
}

type thread_post = {
  id : string;
  text : string option;
  timestamp : string option;
  permalink : string option;
}

type insight_metric =
  | Views
  | Likes
  | Replies
  | Reposts
  | Quotes

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

let insight_metric_names = [ "views"; "likes"; "replies"; "reposts"; "quotes" ]

let string_of_insight_metric = function
  | Views -> "views"
  | Likes -> "likes"
  | Replies -> "replies"
  | Reposts -> "reposts"
  | Quotes -> "quotes"

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

type media_kind =
  | Image
  | Video
  | GIF

type container_status =
  | Finished
  | In_progress
  | Error_status of string

type publishing_limit = {
  quota_usage : int;
  quota_total : int;
  reply_quota_usage : int;
  reply_quota_total : int;
}

module type CONFIG = sig
  include Social_core.CONFIG
  module Http : Social_core.HTTP_CLIENT
  val sleep : float -> (unit -> 'a) -> 'a
end

module Make (Config : CONFIG) = struct
  module OAuth_http = OAuth.Make (Config.Http)

  let max_text_length = 500

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
            let _ = Unix.select [] [] [] (backoff_seconds attempt) in
            loop (attempt + 1)
          else
            on_success response)
        (fun err ->
          if retry_network && attempt < max_attempts then
            let _ = Unix.select [] [] [] (backoff_seconds attempt) in
            loop (attempt + 1)
          else
            on_error err)
    in
    loop 1

  let http_post_no_retry ?(headers = []) ?(body = "") url on_success on_error =
    Config.Http.post ~headers ~body url on_success on_error

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
        OAuth_http.refresh_token ~long_lived_token:credentials.Social_core.access_token
          (fun refreshed_creds ->
             let refreshed_token = String.trim refreshed_creds.access_token in
             if refreshed_token = "" then
               on_refresh_error (Error_types.Auth_error Error_types.Token_expired)
             else
               on_refresh_success { refreshed_creds with access_token = refreshed_token })
          (fun _ -> on_refresh_error (Error_types.Auth_error Error_types.Token_expired))
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
                 let value_items = item |> member "values" |> to_list in
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

  let create_text_container
      ~access_token
      ~user_id
      ~text
      ?reply_to_id
      ?idempotency_key
      ?reply_control
      ?topic_tag
      on_result =
    let normalized_text = String.trim text in
    let normalized_idempotency_key = normalize_idempotency_key idempotency_key in
    let body =
      Uri.encoded_of_query
        ([ ("media_type", ["TEXT"]);
           ("text", [normalized_text]);
           ("access_token", [access_token]) ]
         @
         (match reply_to_id with
          | Some id -> [ ("reply_to_id", [id]) ]
          | None -> [])
         @
         (match reply_control with
          | Some v when String.trim v <> "" -> [ ("reply_control", [String.trim v]) ]
          | _ -> [])
         @
         (match topic_tag with
          | Some tag when String.trim tag <> "" -> [ ("text_post_app_tags", [String.trim tag]) ]
          | _ -> [])
         @
         (match normalized_idempotency_key with
          | Some key -> [ ("client_request_id", [key]) ]
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
      Uri.encoded_of_query
         ([ ("media_type", [media_type]);
           (media_field, [media_url]);
           ("access_token", [access_token]) ]
         @
         (if is_carousel_item then [ ("is_carousel_item", ["true"]) ] else [])
         @
         (match alt_text with
          | Some txt when String.trim txt <> "" -> [("alt_text", [String.trim txt])]
          | _ -> [])
         @
         (if normalized_text = "" || is_carousel_item then [] else [ ("text", [normalized_text]) ])
         @
         (match reply_to_id with
          | Some id -> [ ("reply_to_id", [id]) ]
          | None -> [])
         @
         (match reply_control with
          | Some v when String.trim v <> "" -> [ ("reply_control", [String.trim v]) ]
          | _ -> [])
         @
         (match topic_tag with
          | Some tag when String.trim tag <> "" -> [ ("text_post_app_tags", [String.trim tag]) ]
          | _ -> [])
         @
         (match normalized_idempotency_key with
          | Some key -> [ ("client_request_id", [key]) ]
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
      Uri.encoded_of_query
        [ ("creation_id", [creation_id]);
          ("access_token", [access_token]) ]
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
                Config.sleep delay (fun () -> loop (attempt + 1))
            | Ok (Error_status msg) ->
                on_result (Error (Error_types.Internal_error
                  (Printf.sprintf "Container %s processing failed: %s" container_id msg)))
            | Error err -> on_result (Error err))
    in
    loop 1

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
                     create_carousel_children
                       ~access_token
                       ~user_id
                       ~media_urls_with_alt:rest
                       ~acc:(child_id :: acc)
                       on_result
                 | Error err -> on_result (Error err))
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
      on_result =
    let normalized_text = String.trim text in
    let body =
      Uri.encoded_of_query
        ([ ("media_type", ["CAROUSEL"]);
           ("children", [String.concat "," children_ids]);
           ("access_token", [access_token]) ]
         @
         (if normalized_text = "" then [] else [ ("text", [normalized_text]) ])
         @
         (match reply_control with
          | Some v when String.trim v <> "" -> [ ("reply_control", [String.trim v]) ]
          | _ -> [])
         @
         (match topic_tag with
          | Some tag when String.trim tag <> "" -> [ ("text_post_app_tags", [String.trim tag]) ]
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

  let post_carousel ~account_id ~text ~media_urls ?(alt_texts=[]) ?topic_tag ?reply_control on_result =
    let validation_errors =
      [ validate_media_urls ~max_items:20 media_urls ]
      |> List.filter_map (fun x -> x)
    in
    let count = List.length media_urls in
    let validation_errors =
      if count < 2 then
        (Error_types.Too_many_media { count; max = 2 }) :: validation_errors
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
                            (function
                              | Error err -> on_result (Error_types.Failure err)
                              | Ok carousel_id ->
                                  poll_container_status
                                    ~access_token:creds.access_token
                                    ~container_id:carousel_id
                                    (function
                                      | Error err -> on_result (Error_types.Failure err)
                                      | Ok _container_id ->
                                          publish_container
                                            ~access_token:creds.access_token
                                            ~user_id
                                            ~creation_id:carousel_id
                                            (function
                                              | Ok post_id -> on_result (Error_types.Success post_id)
                                              | Error err -> on_result (Error_types.Failure err)))))))
        (fun err -> on_result (Error_types.Failure err))

  let get_replies ~account_id ~media_id ?after ?(limit = 20) on_result =
    if String.trim media_id = "" then
      on_result (Error (Error_types.Validation_error [ Error_types.Invalid_url "media_id must not be empty" ]))
    else
    with_valid_credentials ~account_id
      (fun creds ->
        let limit = if limit < 1 then 1 else if limit > 100 then 100 else limit in
        let params =
          let normalized_after = normalize_optional_non_empty after in
          [ ("fields", ["id,text,timestamp,permalink"]);
            ("limit", [string_of_int limit]);
            ("access_token", [creds.access_token]) ]
          @
          (match normalized_after with
           | Some cursor -> [ ("after", [cursor]) ]
           | None -> [])
        in
        let query = Uri.encoded_of_query params in
        let url = Printf.sprintf "%s/%s/replies?%s" threads_api_base (String.trim media_id) query in
        http_get_with_retry url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let open Yojson.Basic.Util in
                let posts =
                  json |> member "data" |> to_list
                  |> List.map (fun item ->
                         {
                           id = item |> member "id" |> to_string;
                           text =
                             (try Some (item |> member "text" |> to_string)
                              with _ -> None);
                           timestamp =
                             (try Some (item |> member "timestamp" |> to_string)
                              with _ -> None);
                           permalink =
                             (try Some (item |> member "permalink" |> to_string)
                              with _ -> None);
                         })
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
                on_result (Ok (posts, next_after))
              with exn ->
                on_result
                  (Error
                     (Error_types.Internal_error
                        (Printf.sprintf "Failed to parse replies response: %s" (Printexc.to_string exn))))
            else
              on_result
                (Error
                   (parse_api_error
                      ~status_code:response.status
                      ~headers:response.headers
                      ~response_body:response.body)))
          (fun err -> on_result (Error (network_error err))))
      (fun err -> on_result (Error err))

  let get_conversation ~account_id ~media_id ?after ?(limit = 20) on_result =
    if String.trim media_id = "" then
      on_result (Error (Error_types.Validation_error [ Error_types.Invalid_url "media_id must not be empty" ]))
    else
    with_valid_credentials ~account_id
      (fun creds ->
        let limit = if limit < 1 then 1 else if limit > 100 then 100 else limit in
        let params =
          let normalized_after = normalize_optional_non_empty after in
          [ ("fields", ["id,text,timestamp,permalink"]);
            ("limit", [string_of_int limit]);
            ("access_token", [creds.access_token]) ]
          @
          (match normalized_after with
           | Some cursor -> [ ("after", [cursor]) ]
           | None -> [])
        in
        let query = Uri.encoded_of_query params in
        let url = Printf.sprintf "%s/%s/conversation?%s" threads_api_base (String.trim media_id) query in
        http_get_with_retry url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let open Yojson.Basic.Util in
                let posts =
                  json |> member "data" |> to_list
                  |> List.map (fun item ->
                         {
                           id = item |> member "id" |> to_string;
                           text =
                             (try Some (item |> member "text" |> to_string)
                              with _ -> None);
                           timestamp =
                             (try Some (item |> member "timestamp" |> to_string)
                              with _ -> None);
                           permalink =
                             (try Some (item |> member "permalink" |> to_string)
                              with _ -> None);
                         })
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
                on_result (Ok (posts, next_after))
              with exn ->
                on_result
                  (Error
                     (Error_types.Internal_error
                        (Printf.sprintf "Failed to parse conversation response: %s" (Printexc.to_string exn))))
            else
              on_result
                (Error
                   (parse_api_error
                      ~status_code:response.status
                      ~headers:response.headers
                      ~response_body:response.body)))
          (fun err -> on_result (Error (network_error err))))
      (fun err -> on_result (Error err))

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

  let get_oauth_url ~redirect_uri ~state on_success on_error =
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
               ~scopes:OAuth.Scopes.publish
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
                  [ ("fields", ["id,text,timestamp,permalink"]);
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
                      try
                        let json = Yojson.Basic.from_string response.body in
                        let open Yojson.Basic.Util in
                        let posts =
                          json |> member "data" |> to_list
                          |> List.map (fun item ->
                                 {
                                   id = item |> member "id" |> to_string;
                                   text =
                                     (try Some (item |> member "text" |> to_string)
                                      with _ -> None);
                                   timestamp =
                                     (try Some (item |> member "timestamp" |> to_string)
                                      with _ -> None);
                                   permalink =
                                     (try Some (item |> member "permalink" |> to_string)
                                      with _ -> None);
                                 })
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
                        on_result (Ok (posts, next_after))
                      with exn ->
                        on_result
                          (Error
                             (Error_types.Internal_error
                                (Printf.sprintf "Failed to parse posts response: %s" (Printexc.to_string exn))))
                    else
                      on_result
                        (Error
                           (parse_api_error
                              ~status_code:response.status
                              ~headers:response.headers
                              ~response_body:response.body)))
                  (fun err -> on_result (Error (network_error err)))))
      (fun err -> on_result (Error err))

  let get_account_insights ~account_id ~since ~until on_result =
    with_valid_credentials ~account_id
      (fun creds ->
        get_user_id ~access_token:creds.access_token
          (function
            | Error err -> on_result (Error err)
            | Ok user_id ->
                let metrics_param = String.concat "," insight_metric_names in
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

  let post_single ~account_id ~text ~media_urls ?(alt_texts=[]) ?idempotency_key ?reply_control ?topic_tag on_result =
    let validation_errors =
      [ validate_post_content ~text ~media_urls; validate_media_urls media_urls ]
      |> List.filter_map (fun x -> x)
    in
    if validation_errors <> [] then
      on_result (Error_types.Failure (Error_types.Validation_error validation_errors))
    else
        with_valid_credentials ~account_id
          (fun creds ->
            let on_user_id = function
              | Error err -> on_result (Error_types.Failure err)
              | Ok user_id ->
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
                    on_container
            in
            get_user_id ~access_token:creds.access_token on_user_id)
          (fun err -> on_result (Error_types.Failure err))

  let post_thread ~account_id ~texts ~media_urls_per_post ?(alt_texts_per_post=[]) ?idempotency_key ?reply_control ?topic_tag on_result =
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
