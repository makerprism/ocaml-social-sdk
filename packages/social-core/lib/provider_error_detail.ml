type t = {
  http_status : int;
  code : string option;
  subcode : int option;
  error_type : string option;
  message : string option;
  trace_id : string option;
  body_excerpt : string option;
}

let excerpt_max_bytes = 1024

(* What one envelope yields; [t] adds the status and the excerpt. *)
type fields = {
  f_code : string option;
  f_subcode : int option;
  f_type : string option;
  f_message : string option;
  f_trace : string option;
}

let no_fields =
  { f_code = None; f_subcode = None; f_type = None; f_message = None; f_trace = None }

let first_some options = List.find_map Fun.id options

(* Field-wise: [primary] wins, [fallback] fills its gaps. *)
let merge primary fallback =
  {
    f_code = first_some [ primary.f_code; fallback.f_code ];
    f_subcode = first_some [ primary.f_subcode; fallback.f_subcode ];
    f_type = first_some [ primary.f_type; fallback.f_type ];
    f_message = first_some [ primary.f_message; fallback.f_message ];
    f_trace = first_some [ primary.f_trace; fallback.f_trace ];
  }

let member key (json : Yojson.Basic.t) =
  match json with
  | `Assoc fields -> List.assoc_opt key fields
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> None

let non_empty s = if String.trim s = "" then None else Some s

let string_field key json =
  match member key json with
  | Some (`String s) -> non_empty s
  | Some (`Null | `Bool _ | `Int _ | `Float _ | `Assoc _ | `List _) | None ->
      None

let int_field key json =
  match member key json with
  | Some (`Int n) -> Some n
  | Some (`String s) -> int_of_string_opt (String.trim s)
  | Some (`Null | `Bool _ | `Float _ | `Assoc _ | `List _) | None -> None

(* A code may be numeric (Graph, Pinterest) or symbolic (TikTok, LinkedIn). *)
let code_field key json =
  match member key json with
  | Some (`Int n) -> Some (string_of_int n)
  | Some (`String s) -> non_empty s
  | Some (`Null | `Bool _ | `Float _ | `Assoc _ | `List _) | None -> None

let first_list_item key json =
  match member key json with
  | Some (`List (first :: _)) -> Some first
  | Some (`List [])
  | Some (`Null | `Bool _ | `Int _ | `Float _ | `String _ | `Assoc _)
  | None ->
      None

let has_whitespace s =
  String.exists (fun c -> c = ' ' || c = '\t' || c = '\n' || c = '\r') s

(* Meta Graph, TikTok and Google all nest an object under "error". Google's
   [code] repeats the HTTP status, so its [errors[0].reason] is the code that
   says something. *)
let of_error_object err =
  let google_reason =
    Option.bind (first_list_item "errors" err) (string_field "reason")
  in
  {
    f_code = first_some [ google_reason; code_field "code" err ];
    f_subcode = int_field "error_subcode" err;
    f_type = first_some [ string_field "type" err; string_field "status" err ];
    f_message =
      first_some [ string_field "message" err; string_field "error_user_msg" err ];
    f_trace = first_some [ string_field "fbtrace_id" err; string_field "log_id" err ];
  }

let of_error_string json name =
  match
    first_some [ string_field "message" json; string_field "error_description" json ]
  with
  | Some description -> { no_fields with f_code = Some name; f_message = Some description }
  | None when has_whitespace name -> { no_fields with f_message = Some name }
  | None -> { no_fields with f_code = Some name }

let of_problem_details json =
  {
    no_fields with
    f_code = string_field "title" json;
    f_message = string_field "detail" json;
    f_type = string_field "type" json;
  }

let of_errors_list json =
  match first_list_item "errors" json with
  | Some first ->
      { no_fields with f_code = code_field "code" first; f_message = string_field "message" first }
  | None -> no_fields

let of_reddit_json_errors json =
  match Option.bind (member "json" json) (first_list_item "errors") with
  | Some (`List (`String code :: rest)) ->
      let message =
        match rest with
        | `String m :: _ -> non_empty m
        | (`Null | `Bool _ | `Int _ | `Float _ | `Assoc _ | `List _) :: _
        | [] ->
            None
      in
      { no_fields with f_code = non_empty code; f_message = message }
  | Some (`List _ | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `Assoc _)
  | None ->
      no_fields

(* LinkedIn sends a symbolic [code] next to a numeric [serviceErrorCode]
   (older responses only the latter); Pinterest a numeric [code]. *)
let of_top_level_code json =
  let service_code = int_field "serviceErrorCode" json in
  match code_field "code" json with
  | Some code ->
      { no_fields with f_code = Some code; f_subcode = service_code;
        f_message = string_field "message" json }
  | None ->
      { no_fields with f_code = Option.map string_of_int service_code;
        f_message = string_field "message" json }

let fields_of_json json =
  match member "error" json with
  | Some (`Assoc _ as err) -> of_error_object err
  | Some (`String name) -> of_error_string json name
  | Some (`Int n) ->
      { no_fields with f_code = Some (string_of_int n); f_message = string_field "message" json }
  | Some (`Null | `Bool _ | `Float _ | `List _) | None ->
      List.fold_left merge no_fields
        [
          of_problem_details json;
          of_errors_list json;
          of_reddit_json_errors json;
          of_top_level_code json;
        ]

let is_utf8_continuation c = Char.code c land 0xC0 = 0x80

let excerpt body =
  if body = "" then None
  else if String.length body <= excerpt_max_bytes then Some body
  else
    (* Keep bytes [0, n); back off while byte [n] continues a sequence that
       started before it. *)
    let rec cut n =
      if n > 0 && is_utf8_continuation body.[n] then cut (n - 1) else n
    in
    Some (String.sub body 0 (cut excerpt_max_bytes))

let of_response ~status_code ?request_id body =
  let fields =
    match Yojson.Basic.from_string body with
    | json -> fields_of_json json
    | exception Yojson.Json_error _ -> no_fields
  in
  {
    http_status = status_code;
    code = fields.f_code;
    subcode = fields.f_subcode;
    error_type = fields.f_type;
    message = fields.f_message;
    trace_id = first_some [ Option.bind request_id non_empty; fields.f_trace ];
    body_excerpt = excerpt body;
  }

let of_api_error (e : Error_types.api_error) =
  match e.raw_response with
  | Some body -> of_response ~status_code:e.status_code ?request_id:e.request_id body
  | None ->
      {
        http_status = e.status_code;
        code = None;
        subcode = None;
        error_type = None;
        message = None;
        trace_id = e.request_id;
        body_excerpt = None;
      }

let of_error : Error_types.error -> t option = function
  | Error_types.Api_error e -> Some (of_api_error e)
  | Error_types.Validation_error _
  | Error_types.Auth_error _
  | Error_types.Rate_limited _
  | Error_types.Network_error _
  | Error_types.Duplicate_content
  | Error_types.Content_policy_violation _
  | Error_types.Resource_not_found _
  | Error_types.Internal_error _ ->
      None
