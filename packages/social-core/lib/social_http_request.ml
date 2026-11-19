(** Request-building mechanics shared by the provider clients; the
    interface documents the encoding contracts. *)

type headers = (string * string) list

let repeated_query_param name values =
  List.map (fun value -> (name, [ value ])) values

let repeated_query_pairs params =
  List.concat_map (fun (key, values) -> repeated_query_param key values) params

let percent_encoded_query params =
  String.concat "&"
    (List.map
       (fun (key, value) -> Printf.sprintf "%s=%s" key (Uri.pct_encode value))
       params)

let bearer_pair token = ("Authorization", "Bearer " ^ token)

let bearer_header token = [ bearer_pair token ]

let bearer_header_opt = function
  | Some token -> bearer_header token
  | None -> []

let json_content_type = [ ("Content-Type", "application/json") ]
