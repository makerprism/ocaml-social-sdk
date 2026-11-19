type comment = {
  id : string;
  text : string option;
  username : string option;
  timestamp : string option;
  hidden : bool option;
  parent_id : string option;
  like_count : int option;
  replies : comment Social_meta_graph_paging.page option;
}

let reply_fields = "id,text,username,timestamp,hidden,parent_id,like_count"

let comment_fields ~replies_limit =
  Printf.sprintf "%s,replies.limit(%d){%s}" reply_fields replies_limit reply_fields

module J = Social_json_reader

let rec parse_comment json =
  match J.nonempty_string_opt (J.member "id" json) with
  | None -> failwith "comment without an id"
  | Some id ->
      {
        id;
        text = J.nonempty_string_opt (J.member "text" json);
        username = J.nonempty_string_opt (J.member "username" json);
        timestamp = J.nonempty_string_opt (J.member "timestamp" json);
        hidden = J.bool_opt (J.member "hidden" json);
        parent_id = J.nonempty_string_opt (J.member "parent_id" json);
        like_count = J.int_opt (J.member "like_count" json);
        replies =
          (match J.member "replies" json with
           | `Assoc _ as replies ->
               Result.to_option (Social_meta_graph_paging.parse ~item:parse_comment replies)
           | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> None);
      }
