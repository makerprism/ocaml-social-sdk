type t = {
  code : int option;
  subcode : int option;
  error_type : string option;
  message : string option;
  user_title : string option;
  user_message : string option;
  fbtrace_id : string option;
  is_transient : bool option;
}

let member key (json : Yojson.Basic.t) =
  match json with
  | `Assoc fields -> List.assoc_opt key fields
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> None

let string_field key json =
  match member key json with
  | Some (`String s) when String.trim s <> "" -> Some s
  | Some (`String _ | `Null | `Bool _ | `Int _ | `Float _ | `Assoc _ | `List _)
  | None ->
      None

let int_field key json =
  match member key json with
  | Some (`Int n) -> Some n
  | Some (`String s) -> int_of_string_opt (String.trim s)
  | Some (`Null | `Bool _ | `Float _ | `Assoc _ | `List _) | None -> None

let bool_field key json =
  match member key json with
  | Some (`Bool b) -> Some b
  | Some (`Null | `Int _ | `Float _ | `String _ | `Assoc _ | `List _) | None ->
      None

let of_body body =
  match Yojson.Basic.from_string body with
  | exception Yojson.Json_error _ -> None
  | json -> (
      match member "error" json with
      | Some (`Assoc _ as err) ->
          Some
            {
              code = int_field "code" err;
              subcode = int_field "error_subcode" err;
              error_type = string_field "type" err;
              message = string_field "message" err;
              user_title = string_field "error_user_title" err;
              user_message = string_field "error_user_msg" err;
              fbtrace_id = string_field "fbtrace_id" err;
              is_transient = bool_field "is_transient" err;
            }
      | Some (`Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _) | None
        ->
          None)

type token_reason =
  | Expired
  | App_not_installed
  | User_checkpointed
  | Password_changed
  | Unconfirmed_user
  | Invalid_access_token
  | Invalid_session
  | Unspecified

type classification =
  | Resource_not_found
  | Invalid_request
  | Transient
  | Rate_limited
  | Token_invalid of token_reason
  | Permission_denied
  | Duplicate_content
  | Unclassified

let token_reason_of_subcode = function
  | Some 458 -> App_not_installed
  | Some 459 -> User_checkpointed
  | Some 460 -> Password_changed
  | Some 463 -> Expired
  | Some 464 -> Unconfirmed_user
  | Some 467 -> Invalid_access_token
  | Some 492 -> Invalid_session
  | Some _ | None -> Unspecified

let is_rate_limit_code code =
  match code with
  | 4 | 9 | 17 | 32 | 341 | 368 | 613 -> true
  | other -> other >= 80000 && other <= 80014

let is_permission_code code =
  match code with
  | 3 | 10 -> true
  | other -> other >= 200 && other <= 299

let classify t =
  match t.code with
  | Some 100 -> (
      match t.subcode with
      | Some 33 -> Resource_not_found
      | Some _ | None -> Invalid_request)
  | Some (1 | 2) -> (
      match t.subcode, t.is_transient with
      | None, (Some _ | None) | Some _, Some true -> Transient
      | Some _, (Some false | None) -> Unclassified)
  | Some (102 | 190) -> Token_invalid (token_reason_of_subcode t.subcode)
  | Some 506 -> Duplicate_content
  | Some code when is_rate_limit_code code -> Rate_limited
  | Some code when is_permission_code code -> Permission_denied
  | Some _ -> Unclassified
  | None -> (
      match t.error_type with
      | Some "OAuthException" -> Token_invalid (token_reason_of_subcode t.subcode)
      | Some _ | None -> Unclassified)

let auth_error_of_token_reason = function
  | Expired | Invalid_access_token -> Error_types.Token_expired
  | App_not_installed | Password_changed -> Error_types.Token_revoked
  | User_checkpointed | Unconfirmed_user | Invalid_session | Unspecified ->
      Error_types.Token_invalid

let rate_limited retry_after_seconds =
  Error_types.Rate_limited
    { retry_after_seconds; limit = None; remaining = Some 0; reset_at = None }

let to_error ~platform ~status_code ~required_permissions ?retry_after_seconds
    ?request_id ?(redact_body = Fun.id) ?(redact_message = Fun.id) body =
  let stored_body = redact_body body in
  let api_error ~message ~trace_id =
    Error_types.Api_error
      {
        status_code;
        message;
        platform;
        raw_response = Some stored_body;
        request_id = (match request_id with Some _ -> request_id | None -> trace_id);
      }
  in
  if status_code = 429 then rate_limited retry_after_seconds
  else
    match of_body body with
    | None -> api_error ~message:stored_body ~trace_id:None
    | Some graph -> (
        let message =
          match graph.message with
          | Some m -> redact_message m
          | None -> stored_body
        in
        match classify graph with
        | Rate_limited -> rate_limited retry_after_seconds
        | Token_invalid reason ->
            Error_types.Auth_error (auth_error_of_token_reason reason)
        | Permission_denied ->
            Error_types.Auth_error
              (Error_types.Insufficient_permissions
                 { required = required_permissions; platform_message = Some message })
        | Resource_not_found -> Error_types.Resource_not_found message
        | Duplicate_content -> Error_types.Duplicate_content
        | Invalid_request | Transient | Unclassified ->
            api_error ~message ~trace_id:graph.fbtrace_id)

let transport_error message =
  Error_types.Network_error (Error_types.Connection_failed message)
