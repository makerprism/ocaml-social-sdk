type post = {
  id : string;
  text : string option;
  media_type : string option;
  media_url : string option;
  permalink : string option;
  timestamp : string option;
  username : string option;
  raw : Yojson.Basic.t;
}

let member name = function
  | `Assoc fields -> Option.value ~default:`Null (List.assoc_opt name fields)
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> `Null

let string_option = function
  | `String value -> Some value
  | `Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _ -> None

let of_json raw =
  match string_option (member "id" raw) with
  | None | Some "" -> Error "post without a nonempty string id"
  | Some id ->
      Ok
        {
          id;
          text = string_option (member "text" raw);
          media_type = string_option (member "media_type" raw);
          media_url = string_option (member "media_url" raw);
          permalink = string_option (member "permalink" raw);
          timestamp = string_option (member "timestamp" raw);
          username = string_option (member "username" raw);
          raw;
        }
