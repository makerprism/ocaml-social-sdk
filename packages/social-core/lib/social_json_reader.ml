(** Shared readers for JSON responses of provider APIs. See the interface
    for the coercion rules; they differ on purpose and every difference is
    named there. *)

type json = Yojson.Basic.t

let member key = function
  | `Assoc pairs -> Option.value ~default:`Null (List.assoc_opt key pairs)
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> `Null

let field key = function
  | `Assoc pairs -> List.assoc_opt key pairs
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> None

let path keys json = List.fold_left (fun json key -> member key json) json keys

let string_opt = function
  | `String s -> Some s
  | `Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _ -> None

let nonempty_string_opt = function
  | `String s when s <> "" -> Some s
  | `String _ | `Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _ ->
      None

let bool_opt = function
  | `Bool b -> Some b
  | `Null | `Int _ | `Float _ | `String _ | `List _ | `Assoc _ -> None

let int_opt = function
  | `Int i -> Some i
  | `Null | `Bool _ | `Float _ | `String _ | `List _ | `Assoc _ -> None

let int_value ~what = function
  | Some (`Int n) -> Ok n
  | Some (`Float f) when Float.is_integer f -> Ok (int_of_float f)
  | Some (`Float _ | `Null | `Bool _ | `String _ | `List _ | `Assoc _) | None
    ->
      Error (what ^ " is not an integer")

let data_list json =
  match member "data" json with
  | `List items -> items
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `Assoc _ -> []

let map_result f items =
  List.fold_right
    (fun item acc ->
      match (acc, f item) with
      | Ok values, Ok value -> Ok (value :: values)
      | (Error _ as error), _ -> error
      | Ok _, (Error _ as error) -> error)
    items (Ok [])
