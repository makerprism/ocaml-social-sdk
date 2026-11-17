type public_metrics = {
  followers_count : int option;
  following_count : int option;
  tweet_count : int option;
  listed_count : int option;
  like_count : int option;
  media_count : int option;
}

type t = {
  id : string;
  username : string;
  name : string;
  description : string option;
  profile_image_url : string option;
  location : string option;
  url : string option;
  created_at : string option;
  protected : bool option;
  verified : bool option;
  verified_type : string option;
  public_metrics : public_metrics option;
}

let me_url_base = "https://api.x.com/2/users/me"

let default_user_fields =
  [ "description"; "profile_image_url"; "location"; "url"; "created_at"; "protected";
    "verified"; "verified_type"; "public_metrics" ]

let me_url ~user_fields =
  match user_fields with
  | [] -> me_url_base
  | _ :: _ -> me_url_base ^ "?" ^ Uri.encoded_of_query [ ("user.fields", [ String.concat "," user_fields ]) ]

let field key = function
  | `Assoc fields -> List.assoc_opt key fields
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> None

let text key json =
  match field key json with
  | Some (`String value) when value <> "" -> Some value
  | Some (`String _ | `Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _) | None -> None

let flag key json =
  match field key json with
  | Some (`Bool value) -> Some value
  | Some (`Null | `Int _ | `Float _ | `String _ | `List _ | `Assoc _) | None -> None

let count key json =
  match field key json with
  | Some (`Int value) -> Some value
  | Some (`Null | `Bool _ | `Float _ | `String _ | `List _ | `Assoc _) | None -> None

let metrics_of json =
  {
    followers_count = count "followers_count" json;
    following_count = count "following_count" json;
    tweet_count =
      (match count "tweet_count" json with
       | Some _ as value -> value
       | None -> count "post_count" json);
    listed_count = count "listed_count" json;
    like_count = count "like_count" json;
    media_count = count "media_count" json;
  }

let parse body =
  match Yojson.Basic.from_string body with
  | exception Yojson.Json_error message -> Error ("users/me response is not JSON: " ^ message)
  | json -> (
      let data = Option.value (field "data" json) ~default:`Null in
      match text "id" data, text "username" data, text "name" data with
      | Some id, Some username, Some name ->
          Ok
            {
              id;
              username;
              name;
              description = text "description" data;
              profile_image_url = text "profile_image_url" data;
              location = text "location" data;
              url = text "url" data;
              created_at = text "created_at" data;
              protected = flag "protected" data;
              verified = flag "verified" data;
              verified_type = text "verified_type" data;
              public_metrics =
                (match field "public_metrics" data with
                 | Some (`Assoc _ as metrics) -> Some (metrics_of metrics)
                 | Some (`Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _) | None -> None);
            }
      | (Some _ | None), _, _ -> Error "users/me response has no data.id, data.username or data.name")

let detail_of body =
  match Yojson.Basic.from_string body with
  | json -> (
      match text "detail" json with
      | Some _ as detail -> detail
      | None -> (
          match field "errors" json with
          | Some (`List (first :: _)) -> text "message" first
          | Some (`List [] | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `Assoc _) | None ->
              None))
  | exception Yojson.Json_error _ -> None

let error_of_response ~status_code ~headers body =
  match status_code with
  | 401 -> Error_types.Auth_error Error_types.Token_invalid
  | 403 ->
      Error_types.Auth_error
        (Error_types.Insufficient_permissions
           { required = [ "tweet.read"; "users.read" ]; platform_message = detail_of body })
  | 429 ->
      let retry_after_seconds =
        let reset =
          List.find_map
            (fun (key, value) ->
              if String.lowercase_ascii key = "x-rate-limit-reset" then int_of_string_opt value else None)
            headers
        in
        Option.map (fun reset -> max 1 (reset - int_of_float (Unix.time ()))) reset
      in
      Error_types.make_rate_limited ?retry_after_seconds ~remaining:0 ()
  | _ ->
      Error_types.make_api_error ~platform:Platform_types.Twitter ~status_code
        ~message:(Option.value (detail_of body) ~default:"API error") ~raw_response:body ()
