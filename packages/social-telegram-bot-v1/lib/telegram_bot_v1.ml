(** Telegram Bot API provider focused on channel/group broadcast posting.

    Scope boundaries for this package:
    - Broadcast posting to channels/groups only
    - No direct messaging workflows
    - No update polling or webhook command handling
*)

open Social_core

let api_base = "https://api.telegram.org"
let max_message_text_length = 4096
let max_caption_length = 1024

module type CONFIG = sig
  module Http : HTTP_CLIENT

  val get_env : string -> string option
  val get_credentials : account_id:string -> (credentials -> unit) -> (string -> unit) -> unit
  val update_credentials : account_id:string -> credentials:credentials -> (unit -> unit) -> (string -> unit) -> unit
  val encrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val decrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val update_health_status : account_id:string -> status:string -> error_message:string option -> (unit -> unit) -> (string -> unit) -> unit

  (** Resolve account to chat target. The resolved chat id must be a broadcast target
      (channel/supergroup/group), not a direct-message user id. *)
  val get_chat_id : account_id:string -> (string -> unit) -> (string -> unit) -> unit
end

module Make (Config : CONFIG) = struct
  let redact_token token s =
    if token = "" then s
    else
      try Str.global_replace (Str.regexp_string token) "[REDACTED]" s
      with _ -> s

  let build_api_url ~token ~method_name =
    Printf.sprintf "%s/bot%s/%s" api_base token method_name

  let is_http_url url =
    try
      let uri = Uri.of_string url in
      match Uri.scheme uri, Uri.host uri with
      | Some scheme, Some host when host <> "" ->
          let normalized = String.lowercase_ascii scheme in
          normalized = "http" || normalized = "https"
      | _ -> false
    with _ -> false

  let media_method_and_field media_url =
    let lower = String.lowercase_ascii media_url in
    if String.ends_with ~suffix:".mp4" lower
       || String.ends_with ~suffix:".mov" lower
       || String.ends_with ~suffix:".webm" lower
       || String.ends_with ~suffix:".mkv" lower
    then Some ("sendVideo", "video")
    else if String.ends_with ~suffix:".jpg" lower
            || String.ends_with ~suffix:".jpeg" lower
            || String.ends_with ~suffix:".png" lower
            || String.ends_with ~suffix:".webp" lower
            || String.ends_with ~suffix:".gif" lower
    then Some ("sendPhoto", "photo")
    else None

  let validate_token token =
    let trimmed = String.trim token in
    if trimmed = "" then
      Error (Error_types.Auth_error Error_types.Missing_credentials)
    else if not (String.contains trimmed ':') then
      Error (Error_types.Auth_error Error_types.Token_invalid)
    else Ok trimmed

  let validate_chat_id_for_broadcast chat_id =
    let trimmed = String.trim chat_id in
    if trimmed = "" then
      Error (Error_types.Internal_error "Missing Telegram chat id")
    else
      match int_of_string_opt trimmed with
      | Some n when n < 0 -> Ok trimmed
      | Some _ | None ->
          Error (Error_types.Internal_error "Broadcast posting only supports negative channel/group chat ids in v1")

  let validate_post ~text ~media_count =
    let errors = ref [] in
    if String.length text = 0 then errors := Error_types.Text_empty :: !errors;
    if media_count > 1 then
      errors := Error_types.Too_many_media { count = media_count; max = 1 } :: !errors;
    let text_limit = if media_count = 0 then max_message_text_length else max_caption_length in
    if String.length text > text_limit then
      errors := Error_types.Text_too_long { length = String.length text; max = text_limit } :: !errors;
    if !errors = [] then Ok () else Error (List.rev !errors)

  let validate_thread ~texts ~media_urls_per_post =
    if texts = [] then Error [Error_types.Thread_empty]
    else if List.length texts <> List.length media_urls_per_post then
      Error [Error_types.Thread_post_invalid {
        index = min (List.length texts) (List.length media_urls_per_post);
        errors = [Error_types.Text_empty];
      }]
    else
      let errors = ref [] in
      List.iteri (fun i text ->
        let media_count =
          match List.nth_opt media_urls_per_post i with
          | Some media -> List.length media
          | None -> 0
        in
        match validate_post ~text ~media_count with
        | Ok () -> ()
        | Error post_errors ->
            errors := Error_types.Thread_post_invalid { index = i; errors = post_errors } :: !errors
      ) texts;
      if !errors = [] then Ok () else Error (List.rev !errors)

  let parse_retry_after response_headers json =
    let from_headers =
      let rec find = function
        | [] -> None
        | (k, v) :: rest ->
            if String.lowercase_ascii k = "retry-after" then int_of_string_opt v
            else find rest
      in
      find response_headers
    in
    match from_headers with
    | Some _ as value -> value
    | None ->
        try
          let open Yojson.Basic.Util in
          Some (json |> member "parameters" |> member "retry_after" |> to_int)
        with _ -> None

  let api_error_from_telegram ~status_code ~response_headers ~response_body ~token =
    let redacted_body = redact_token token response_body in
    let default_api_error message =
      Error_types.Api_error {
        status_code;
        message;
        platform = Platform_types.Telegram;
        raw_response = Some redacted_body;
        request_id = None;
      }
    in
    try
      let open Yojson.Basic.Util in
      let json = Yojson.Basic.from_string response_body in
      let error_code =
        try json |> member "error_code" |> to_int
        with _ -> status_code
      in
      let description =
        try json |> member "description" |> to_string
        with _ -> redacted_body
      in
      let safe_description = redact_token token description in
      if error_code = 401 || status_code = 401 then
        Error_types.Auth_error Error_types.Token_invalid
      else if error_code = 403 || status_code = 403 then
        Error_types.Auth_error (Error_types.Insufficient_permissions ["chat:write"])
      else if error_code = 429 || status_code = 429 then
        Error_types.Rate_limited {
          retry_after_seconds = parse_retry_after response_headers json;
          limit = None;
          remaining = Some 0;
          reset_at = None;
        }
      else if status_code >= 500 then
        default_api_error safe_description
      else
        default_api_error safe_description
    with _ ->
      if status_code = 401 then Error_types.Auth_error Error_types.Token_invalid
      else if status_code = 403 then Error_types.Auth_error (Error_types.Insufficient_permissions ["chat:write"])
      else if status_code = 429 then
        Error_types.Rate_limited {
          retry_after_seconds = Some 60;
          limit = None;
          remaining = Some 0;
          reset_at = None;
        }
      else
        default_api_error redacted_body

  let update_health_status_for_result ~account_id = function
    | Ok _ ->
        Config.update_health_status ~account_id ~status:"healthy" ~error_message:None
          (fun () -> ()) (fun _ -> ())
    | Error (Error_types.Auth_error Error_types.Token_invalid) ->
        Config.update_health_status ~account_id ~status:"invalid_token"
          ~error_message:(Some "Telegram bot token invalid")
          (fun () -> ()) (fun _ -> ())
    | Error (Error_types.Auth_error (Error_types.Insufficient_permissions _)) ->
        Config.update_health_status ~account_id ~status:"insufficient_permissions"
          ~error_message:(Some "Telegram bot lacks channel/group posting permissions")
          (fun () -> ()) (fun _ -> ())
    | _ -> ()

  let perform_form_post_json ~token ~method_name ~params on_ok on_result =
    let url = build_api_url ~token ~method_name in
    let body =
      Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) params)
    in
    let headers = [ ("Content-Type", "application/x-www-form-urlencoded") ] in
    Config.Http.post ~headers ~body url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let open Yojson.Basic.Util in
            let json = Yojson.Basic.from_string response.body in
            let ok =
              try json |> member "ok" |> to_bool
              with _ -> false
            in
            if not ok then
              on_result (Error (api_error_from_telegram
                ~status_code:response.status
                ~response_headers:response.headers
                ~response_body:response.body
                ~token))
            else
              on_ok json
          with _ ->
            on_result (Error (Error_types.Internal_error "Failed to parse Telegram response payload"))
        else
          on_result (Error (api_error_from_telegram
            ~status_code:response.status
            ~response_headers:response.headers
            ~response_body:response.body
            ~token)))
      (fun err ->
        on_result (Error (Error_types.Network_error
          (Error_types.Connection_failed (redact_token token err)))))

  let perform_form_post ~token ~method_name ~params on_result =
    let on_ok json =
      let open Yojson.Basic.Util in
      let result = json |> member "result" in
      let message_id =
        try string_of_int (result |> member "message_id" |> to_int)
        with _ ->
          try result |> member "message_id" |> to_string
          with _ -> ""
      in
      if message_id = "" then
        on_result (Error (Error_types.Internal_error "Telegram response missing message_id"))
      else
        on_result (Ok message_id)
    in
    perform_form_post_json ~token ~method_name ~params on_ok on_result

  let perform_form_post_expect_ok ~token ~method_name ~params on_result =
    let on_ok _json = on_result (Ok ()) in
    perform_form_post_json ~token ~method_name ~params on_ok on_result

  let resolve_target ~account_id on_result =
    Config.get_credentials ~account_id
      (fun credentials ->
        match validate_token credentials.access_token with
        | Error err ->
            let result = Error err in
            update_health_status_for_result ~account_id result;
            on_result result
        | Ok token ->
            Config.get_chat_id ~account_id
              (fun chat_id ->
                match validate_chat_id_for_broadcast chat_id with
                | Error err ->
                    let result = Error err in
                    update_health_status_for_result ~account_id result;
                    on_result result
                | Ok resolved_chat_id ->
                    on_result (Ok (token, resolved_chat_id)))
              (fun err ->
                on_result (Error (Error_types.Internal_error (redact_token token err)))))
      (fun err ->
        let result = Error (Error_types.Internal_error err) in
        update_health_status_for_result ~account_id result;
        on_result result)

  let validate_access ~account_id on_result =
    resolve_target ~account_id
      (function
        | Error err -> on_result (Error err)
        | Ok (token, _chat_id) ->
            perform_form_post_expect_ok
              ~token
              ~method_name:"getMe"
              ~params:[]
              (fun result ->
                update_health_status_for_result ~account_id result;
                match result with
                | Ok _ -> on_result (Ok ())
                | Error e -> on_result (Error e)))

  let post_single ~account_id ~text ~media_urls ?(alt_texts=[]) on_result =
    let _ = alt_texts in
    match validate_post ~text ~media_count:(List.length media_urls) with
    | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        let media_error =
          match media_urls with
          | [] -> None
          | [url] when not (is_http_url url) -> Some (Error_types.Validation_error [Error_types.Invalid_url url])
          | [url] ->
              (match media_method_and_field url with
               | Some _ -> None
               | None -> Some (Error_types.Validation_error [Error_types.Media_unsupported_format url]))
          | _ -> None
        in
        (match media_error with
         | Some err -> on_result (Error_types.Failure err)
         | None ->
             resolve_target ~account_id
               (function
                 | Error err -> on_result (Error_types.Failure err)
                 | Ok (token, chat_id) ->
                     let on_post_result result =
                       update_health_status_for_result ~account_id result;
                       match result with
                       | Ok message_id -> on_result (Error_types.Success message_id)
                       | Error err -> on_result (Error_types.Failure err)
                     in
                     match media_urls with
                     | [] ->
                         perform_form_post
                           ~token
                           ~method_name:"sendMessage"
                           ~params:[("chat_id", chat_id); ("text", text)]
                           on_post_result
                     | [media_url] ->
                         let method_name, media_field =
                           match media_method_and_field media_url with
                           | Some value -> value
                           | None -> ("sendPhoto", "photo")
                         in
                         perform_form_post
                           ~token
                           ~method_name
                           ~params:[("chat_id", chat_id); (media_field, media_url); ("caption", text)]
                           on_post_result
                     | _ ->
                          perform_form_post
                            ~token
                            ~method_name:"sendMessage"
                            ~params:[("chat_id", chat_id); ("text", text)]
                            on_post_result))

  let post_thread ~account_id ~texts ~media_urls_per_post ?(alt_texts_per_post=[]) on_result =
    let _ = alt_texts_per_post in
    match validate_thread ~texts ~media_urls_per_post with
    | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        let url_errors = ref [] in
        List.iteri (fun i media_urls ->
          match media_urls with
          | [] -> ()
          | [url] ->
              if not (is_http_url url) then
                url_errors := Error_types.Thread_post_invalid {
                  index = i;
                  errors = [Error_types.Invalid_url url];
                } :: !url_errors
              else if media_method_and_field url = None then
                url_errors := Error_types.Thread_post_invalid {
                  index = i;
                  errors = [Error_types.Media_unsupported_format url];
                } :: !url_errors
          | _ -> ()
        ) media_urls_per_post;
        if !url_errors <> [] then
          on_result (Error_types.Failure (Error_types.Validation_error (List.rev !url_errors)))
        else
          resolve_target ~account_id
            (fun result ->
              match result with
              | Error err ->
                  update_health_status_for_result ~account_id (Error err);
                  on_result (Error_types.Failure err)
              | Ok (token, chat_id) ->
                  let total_requested = List.length texts in
                  let rec loop idx posted_ids remaining_texts remaining_media =
                    match remaining_texts, remaining_media with
                    | [], [] ->
                        update_health_status_for_result ~account_id (Ok ());
                        on_result (Error_types.Success {
                          Error_types.posted_ids = List.rev posted_ids;
                          failed_at_index = None;
                          total_requested;
                        })
                    | text :: rest_texts, media :: rest_media ->
                        let perform_single on_single =
                          match media with
                          | [] ->
                              perform_form_post
                                ~token
                                ~method_name:"sendMessage"
                                ~params:[("chat_id", chat_id); ("text", text)]
                                on_single
                          | [media_url] ->
                              let method_name, media_field =
                                match media_method_and_field media_url with
                                | Some value -> value
                                | None -> ("", "")
                              in
                              if method_name = "" then
                                on_single (Error (Error_types.Validation_error [Error_types.Media_unsupported_format media_url]))
                              else
                                perform_form_post
                                  ~token
                                  ~method_name
                                  ~params:[("chat_id", chat_id); (media_field, media_url); ("caption", text)]
                                  on_single
                          | _ ->
                              on_single (Error (Error_types.Validation_error [Error_types.Too_many_media { count = List.length media; max = 1 }]))
                        in
                        perform_single (function
                          | Ok message_id ->
                              loop (idx + 1) (message_id :: posted_ids) rest_texts rest_media
                          | Error err ->
                              update_health_status_for_result ~account_id (Error err);
                              let thread_result = {
                                Error_types.posted_ids = List.rev posted_ids;
                                failed_at_index = Some idx;
                                total_requested;
                              } in
                              if posted_ids <> [] then
                                on_result (Error_types.Partial_success {
                                  result = thread_result;
                                  warnings = [Error_types.Generic_warning {
                                    code = "thread_incomplete";
                                    message = Error_types.error_to_string err;
                                    recoverable = false;
                                  }];
                                })
                              else
                                on_result (Error_types.Failure err))
                    | _ ->
                        on_result (Error_types.Failure (Error_types.Validation_error [Error_types.Thread_empty]))
                  in
                  loop 0 [] texts media_urls_per_post)

  let validate_content ~text =
    let media_count = 0 in
    match validate_post ~text ~media_count with
    | Ok () -> Ok ()
    | Error errs -> Error (Error_types.error_to_string (Error_types.Validation_error errs))
end
