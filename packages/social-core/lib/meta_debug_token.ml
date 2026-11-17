type expiry =
  | Never_expires
  | Expires_at of int

type granular_scope = {
  scope : string;
  target_ids : string list option;
}

type token_error = {
  code : int option;
  subcode : int option;
  message : string option;
}

type t = {
  is_valid : bool;
  app_id : string option;
  application : string option;
  token_type : string option;
  user_id : string option;
  profile_id : string option;
  scopes : string list;
  granular_scopes : granular_scope list;
  expires_at : expiry option;
  data_access_expires_at : int option;
  issued_at : int option;
  error : token_error option;
}

let member key = function
  | `Assoc fields -> List.assoc_opt key fields
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> None

let string_member key json =
  match member key json with
  | Some (`String value) -> Some value
  | Some (`Int value) -> Some (string_of_int value)
  | Some (`Null | `Bool _ | `Float _ | `List _ | `Assoc _) | None -> None

let int_member key json =
  match member key json with
  | Some (`Int value) -> Some value
  | Some (`String value) -> int_of_string_opt value
  | Some (`Null | `Bool _ | `Float _ | `List _ | `Assoc _) | None -> None

let id_of = function
  | `String id -> Some id
  | `Int id -> Some (string_of_int id)
  | `Null | `Bool _ | `Float _ | `List _ | `Assoc _ -> None

let string_of = function
  | `String value when String.trim value <> "" -> Some (String.trim value)
  | `String _ | `Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _ -> None

let list_member key json =
  match member key json with
  | Some (`List items) -> items
  | Some (`Null | `Bool _ | `Int _ | `Float _ | `String _ | `Assoc _) | None -> []

let granular_scope_of entry =
  match member "scope" entry with
  | Some (`String scope) ->
      let target_ids =
        match member "target_ids" entry with
        | Some (`List ids) -> Some (List.filter_map id_of ids)
        | Some (`Null | `Bool _ | `Int _ | `Float _ | `String _ | `Assoc _)
        | None ->
            None
      in
      Some { scope; target_ids }
  | Some (`Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _) | None -> None

let token_error_of data =
  match member "error" data with
  | Some (`Assoc _ as error) ->
      Some
        {
          code = int_member "code" error;
          subcode = int_member "subcode" error;
          message = string_member "message" error;
        }
  | Some (`Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _) | None -> None

let of_data data is_valid =
  {
    is_valid;
    app_id = string_member "app_id" data;
    application = string_member "application" data;
    token_type = string_member "type" data;
    user_id = string_member "user_id" data;
    profile_id = string_member "profile_id" data;
    scopes = List.filter_map string_of (list_member "scopes" data);
    granular_scopes =
      List.filter_map granular_scope_of (list_member "granular_scopes" data);
    expires_at =
      Option.map
        (fun seconds -> if seconds = 0 then Never_expires else Expires_at seconds)
        (int_member "expires_at" data);
    data_access_expires_at = int_member "data_access_expires_at" data;
    issued_at = int_member "issued_at" data;
    error = token_error_of data;
  }

let of_json json =
  match member "data" json with
  | Some (`Assoc _ as data) -> (
      match member "is_valid" data with
      | Some (`Bool is_valid) -> Ok (of_data data is_valid)
      | Some (`Null | `Int _ | `Float _ | `String _ | `List _ | `Assoc _) | None ->
          Error "debug_token response has no boolean data.is_valid")
  | Some (`Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _) | None ->
      Error "debug_token response has no data object"

let of_body body =
  match Yojson.Basic.from_string body with
  | json -> of_json json
  | exception Yojson.Json_error message ->
      Error ("debug_token response is not JSON: " ^ message)

let expires_at_rfc3339 t =
  match t.expires_at with
  | None | Some Never_expires -> None
  | Some (Expires_at seconds) ->
      Option.map Ptime.to_rfc3339 (Ptime.of_float_s (float_of_int seconds))

let target_ids ~scope t =
  List.find_map
    (fun (entry : granular_scope) ->
      if Oauth_scope.equal entry.scope scope then Some entry.target_ids else None)
    t.granular_scopes
  |> Option.join

let granted_for_target ~target_id ~scope t =
  Oauth_scope.mem scope t.scopes
  &&
  match target_ids ~scope t with
  | None -> true
  | Some targets -> List.mem target_id targets

let app_access_token ~app_id ~app_secret = app_id ^ "|" ^ app_secret

let threads_app_access_token ~app_id ~app_secret =
  "TH|" ^ app_id ^ "|" ^ app_secret
