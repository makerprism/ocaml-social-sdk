type comment = {
  id : string;
  text : string option;
  username : string option;
  timestamp : string option;
  hidden : bool option;
  parent_id : string option;
  like_count : int option;
  replies : comment Meta_graph_paging.page option;
}

let reply_fields = "id,text,username,timestamp,hidden,parent_id,like_count"

let comment_fields ~replies_limit =
  Printf.sprintf "%s,replies.limit(%d){%s}" reply_fields replies_limit reply_fields

let member key = function
  | `Assoc pairs -> Option.value ~default:`Null (List.assoc_opt key pairs)
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> `Null

let string_opt = function
  | `String s when s <> "" -> Some s
  | `String _ | `Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _ -> None

let bool_opt = function
  | `Bool b -> Some b
  | `String _ | `Null | `Int _ | `Float _ | `List _ | `Assoc _ -> None

let int_opt = function
  | `Int n -> Some n
  | `String _ | `Null | `Bool _ | `Float _ | `List _ | `Assoc _ -> None

let rec parse_comment json =
  match string_opt (member "id" json) with
  | None -> failwith "comment without an id"
  | Some id ->
      {
        id;
        text = string_opt (member "text" json);
        username = string_opt (member "username" json);
        timestamp = string_opt (member "timestamp" json);
        hidden = bool_opt (member "hidden" json);
        parent_id = string_opt (member "parent_id" json);
        like_count = int_opt (member "like_count" json);
        replies =
          (match member "replies" json with
           | `Assoc _ as replies ->
               Result.to_option (Meta_graph_paging.parse ~item:parse_comment replies)
           | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> None);
      }
