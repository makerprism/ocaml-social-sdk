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

module J = Social_json_reader

let of_json raw =
  match J.string_opt (J.member "id" raw) with
  | None | Some "" -> Error "post without a nonempty string id"
  | Some id ->
      Ok
        {
          id;
          text = J.string_opt (J.member "text" raw);
          media_type = J.string_opt (J.member "media_type" raw);
          media_url = J.string_opt (J.member "media_url" raw);
          permalink = J.string_opt (J.member "permalink" raw);
          timestamp = J.string_opt (J.member "timestamp" raw);
          username = J.string_opt (J.member "username" raw);
          raw;
        }
