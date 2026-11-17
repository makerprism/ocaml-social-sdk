let account_metric_types =
  [ "ENGAGEMENT"; "ENGAGEMENT_RATE"; "IMPRESSION"; "OUTBOUND_CLICK"; "OUTBOUND_CLICK_RATE";
    "PIN_CLICK"; "PIN_CLICK_RATE"; "SAVE"; "SAVE_RATE" ]

let pin_metric_types =
  [ "IMPRESSION"; "OUTBOUND_CLICK"; "PIN_CLICK"; "SAVE"; "SAVE_RATE"; "TOTAL_COMMENTS";
    "TOTAL_REACTIONS"; "USER_FOLLOW"; "PROFILE_VISIT" ]

type daily = { date : string; data_status : string option; metrics : (string * float) list }

type group = {
  daily_metrics : daily list;
  summary_metrics : (string * float) list;
  lifetime_metrics : (string * float) list;
}

type report = (string * group) list

let ( let* ) = Result.bind

let field key = function
  | `Assoc fields -> List.assoc_opt key fields
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> None

let string_field key json =
  match field key json with
  | Some (`String s) -> Some s
  | Some (`Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _) | None -> None

let map_result f items =
  List.fold_right
    (fun item acc ->
      match (acc, f item) with
      | Ok values, Ok value -> Ok (value :: values)
      | (Error _ as error), _ -> error
      | Ok _, (Error _ as error) -> error)
    items (Ok [])

(* A metric map: name to number. Absent or null is an empty map. *)
let number_map ~what = function
  | None | Some `Null -> Ok []
  | Some (`Assoc entries) ->
      map_result
        (fun (name, value) ->
          match value with
          | `Int n -> Ok (name, float_of_int n)
          | `Float f -> Ok (name, f)
          | `Null | `Bool _ | `String _ | `List _ | `Assoc _ ->
              Error (Printf.sprintf "%s.%s is not a number" what name))
        entries
  | Some (`Bool _ | `Int _ | `Float _ | `String _ | `List _) -> Error (what ^ " is not an object")

let parse_daily json =
  match string_field "date" json with
  | None -> Error "daily_metrics[] entry has no date"
  | Some date ->
      let* metrics = number_map ~what:("daily_metrics[" ^ date ^ "].metrics") (field "metrics" json) in
      Ok { date; data_status = string_field "data_status" json; metrics }

let parse_group (key, json) =
  match json with
  | `Assoc _ ->
      let* daily_metrics =
        match field "daily_metrics" json with
        | None | Some `Null -> Ok []
        | Some (`List items) -> map_result parse_daily items
        | Some (`Bool _ | `Int _ | `Float _ | `String _ | `Assoc _) ->
            Error (key ^ ".daily_metrics is not an array")
      in
      let* summary_metrics = number_map ~what:(key ^ ".summary_metrics") (field "summary_metrics" json) in
      let* lifetime_metrics = number_map ~what:(key ^ ".lifetime_metrics") (field "lifetime_metrics" json) in
      Ok (key, { daily_metrics; summary_metrics; lifetime_metrics })
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> Error (key ^ " is not an object")

let parse_report body =
  match Yojson.Basic.from_string body with
  | exception Yojson.Json_error msg -> Error ("analytics response is not JSON: " ^ msg)
  | `Assoc groups -> map_result parse_group groups
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ ->
      Error "analytics response is not an object"

let unsplit = function
  | [ (_, group) ] -> Some group
  | groups -> List.assoc_opt "all" groups

let summary name group = List.assoc_opt name group.summary_metrics
let summary_count name group = Option.map int_of_float (summary name group)
