type filter =
  | Toplevel
  | Stream

type order =
  | Chronological
  | Reverse_chronological

let string_of_filter = function
  | Toplevel -> "toplevel"
  | Stream -> "stream"

let string_of_order = function
  | Chronological -> "chronological"
  | Reverse_chronological -> "reverse_chronological"

type author = {
  author_id : string;
  author_name : string option;
}

type comment = {
  id : string;
  message : string option;
  created_time : string option;
  from : author option;
  permalink_url : string option;
  parent_id : string option;
}

let fields = "id,message,created_time,from{id,name},permalink_url,parent{id}"

let member key = function
  | `Assoc pairs -> Option.value ~default:`Null (List.assoc_opt key pairs)
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> `Null

let string_opt = function
  | `String s when s <> "" -> Some s
  | `String _ | `Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _ -> None

let parse_author json =
  Option.map
    (fun author_id -> { author_id; author_name = string_opt (member "name" json) })
    (string_opt (member "id" json))

let parse_comment json =
  match string_opt (member "id" json) with
  | None -> failwith "comment without an id"
  | Some id ->
      {
        id;
        message = string_opt (member "message" json);
        created_time = string_opt (member "created_time" json);
        from = parse_author (member "from" json);
        permalink_url = string_opt (member "permalink_url" json);
        parent_id = string_opt (member "id" (member "parent" json));
      }
