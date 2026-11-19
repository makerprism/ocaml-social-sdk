type post = {
  id : string;
  text : string option;
  media_type : string option;
  media_url : string option;
  thumbnail_url : string option;
  permalink : string option;
  timestamp : string option;
  username : string option;
  raw : Yojson.Basic.t;
}

module J = Social_json_reader

(** The fields the user post listing requests so {!of_json} fills every
    field of [post]: the common media fields of the
    [/{threads-user-id}/threads] edge, all readable with [threads_basic]. *)
let list_fields =
  "id,text,media_type,media_url,thumbnail_url,permalink,timestamp,username"

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
          thumbnail_url = J.string_opt (J.member "thumbnail_url" raw);
          permalink = J.string_opt (J.member "permalink" raw);
          timestamp = J.string_opt (J.member "timestamp" raw);
          username = J.string_opt (J.member "username" raw);
          raw;
        }
