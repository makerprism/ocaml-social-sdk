type video = { id : string }

let parse body =
  match Yojson.Basic.from_string body with
  | exception Yojson.Json_error message ->
      Error ("video creation response is not JSON: " ^ message)
  | `Assoc fields ->
      (match List.assoc_opt "id" fields with
       | Some (`String id) -> Ok { id }
       | Some (`Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _) | None ->
           Error ("video creation response has no string id: " ^ body))
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ ->
      Error ("video creation response is not a JSON object: " ^ body)
