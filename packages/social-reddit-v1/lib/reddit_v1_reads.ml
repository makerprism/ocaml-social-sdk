type subreddit = {
  id : string;
  display_name : string;
  subscribers : int option;
  over18 : bool option;
  user_is_moderator : bool option;
  link_flair_enabled : bool option;
  icon_img : string option;
  raw : Yojson.Basic.t;
}

type subreddit_page = {
  data : subreddit list;
  after : string option;
  before : string option;
}

type flair = {
  id : string;
  text : string option;
  text_editable : bool option;
  background_color : string option;
  text_color : string option;
  raw : Yojson.Basic.t;
}

let ( let* ) = Result.bind

let member key = function
  | `Assoc fields -> Option.value ~default:`Null (List.assoc_opt key fields)
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> `Null

let string_option = function
  | `String s -> Some s
  | `Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _ -> None

let bool_option = function
  | `Bool b -> Some b
  | `Null | `String _ | `Int _ | `Float _ | `List _ | `Assoc _ -> None

let int_option = function
  | `Int n -> Some n
  | `Null | `String _ | `Bool _ | `Float _ | `List _ | `Assoc _ -> None

let required key json =
  match string_option (member key json) with
  | Some value when value <> "" -> Ok value
  | None | Some _ -> Error ("Missing string field: " ^ key)

let subreddit_of_json raw =
  let* id = required "id" raw in
  let* display_name = required "display_name" raw in
  Ok
    {
      id;
      display_name;
      subscribers = int_option (member "subscribers" raw);
      over18 = bool_option (member "over18" raw);
      user_is_moderator = bool_option (member "user_is_moderator" raw);
      link_flair_enabled = bool_option (member "link_flair_enabled" raw);
      icon_img = string_option (member "icon_img" raw);
      raw;
    }

let about_of_json json = subreddit_of_json (member "data" json)

let parse_list parse = function
  | `List items ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | item :: rest ->
            let* value = parse item in
            loop (value :: acc) rest
      in
      loop [] items
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `Assoc _ ->
      Error "Expected an array"

let page_of_json json =
  let data = member "data" json in
  let* children = parse_list about_of_json (member "children" data) in
  Ok
    {
      data = children;
      after = string_option (member "after" data);
      before = string_option (member "before" data);
    }

let flair_of_json raw =
  let* id = required "id" raw in
  Ok
    {
      id;
      text = string_option (member "text" raw);
      text_editable = bool_option (member "text_editable" raw);
      background_color = string_option (member "background_color" raw);
      text_color = string_option (member "text_color" raw);
      raw;
    }

let flairs_of_json json = parse_list flair_of_json json
