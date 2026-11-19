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

module J = Social_json_reader

let parse_author json =
  Option.map
    (fun author_id -> { author_id; author_name = J.nonempty_string_opt (J.member "name" json) })
    (J.nonempty_string_opt (J.member "id" json))

let parse_comment json =
  match J.nonempty_string_opt (J.member "id" json) with
  | None -> failwith "comment without an id"
  | Some id ->
      {
        id;
        message = J.nonempty_string_opt (J.member "message" json);
        created_time = J.nonempty_string_opt (J.member "created_time" json);
        from = parse_author (J.member "from" json);
        permalink_url = J.nonempty_string_opt (J.member "permalink_url" json);
        parent_id = J.nonempty_string_opt (J.member "id" (J.member "parent" json));
      }
