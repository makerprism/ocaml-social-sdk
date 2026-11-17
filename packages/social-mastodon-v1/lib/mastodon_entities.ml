(* Typed Mastodon entities. See the .mli for sources and contracts. *)

type json = Yojson.Basic.t

let ( let* ) = Result.bind

(* {1 JSON field access}

   [field] treats an explicit [null] like an absent key: Mastodon serialises
   every nullable attribute as [null], never omits it. *)

let field name (json : json) =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt name fields with
      | Some `Null | None -> None
      | Some value -> Some value)
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> None

let type_error name expected = Error (Printf.sprintf "%s: expected %s" name expected)

let string_opt name json =
  match field name json with
  | None -> Ok None
  | Some (`String value) -> Ok (Some value)
  | Some (`Null | `Bool _ | `Int _ | `Float _ | `Assoc _ | `List _) ->
      type_error name "a string"

let non_empty_string_opt name json =
  let* value = string_opt name json in
  match value with
  | Some "" | None -> Ok None
  | Some _ as present -> Ok present

let string_req name json =
  let* value = string_opt name json in
  match value with
  | Some value -> Ok value
  | None -> Error (Printf.sprintf "%s: missing" name)

let bool_req name json =
  match field name json with
  | Some (`Bool value) -> Ok value
  | None -> Error (Printf.sprintf "%s: missing" name)
  | Some (`Null | `String _ | `Int _ | `Float _ | `Assoc _ | `List _) ->
      type_error name "a boolean"

let int_req name json =
  match field name json with
  | Some (`Int value) -> Ok value
  | None -> Error (Printf.sprintf "%s: missing" name)
  | Some (`Null | `Bool _ | `String _ | `Float _ | `Assoc _ | `List _) ->
      type_error name "an integer"

(* Tail-recursive: a context can hold 4,096 descendants. *)
let map_result f items =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | item :: rest -> (
        match f item with
        | Ok value -> loop (value :: acc) rest
        | Error _ as error -> error)
  in
  loop [] items

let list_req name f json =
  match field name json with
  | Some (`List items) -> map_result f items
  | None -> Error (Printf.sprintf "%s: missing" name)
  | Some (`Null | `Bool _ | `String _ | `Int _ | `Float _ | `Assoc _) ->
      type_error name "an array"

let array_of_json name f (json : json) =
  match json with
  | `List items -> map_result f items
  | `Null | `Bool _ | `String _ | `Int _ | `Float _ | `Assoc _ ->
      type_error name "an array"

let object_req name f json =
  match field name json with
  | Some (`Assoc _ as value) -> f value
  | None -> Error (Printf.sprintf "%s: missing" name)
  | Some (`Null | `Bool _ | `String _ | `Int _ | `Float _ | `List _) ->
      type_error name "an object"

let object_opt name f json =
  match field name json with
  | Some (`Assoc _ as value) -> Result.map Option.some (f value)
  | None -> Ok None
  | Some (`Null | `Bool _ | `String _ | `Int _ | `Float _ | `List _) ->
      type_error name "an object"

let prefix_error context = Result.map_error (fun msg -> context ^ "." ^ msg)

let header_value headers key =
  let target = String.lowercase_ascii key in
  List.find_map
    (fun (name, value) ->
      if String.equal (String.lowercase_ascii name) target then Some value
      else None)
    headers

(* {1 Shared types} *)

type visibility = Public | Unlisted | Private | Direct

let visibility_to_string = function
  | Public -> "public"
  | Unlisted -> "unlisted"
  | Private -> "private"
  | Direct -> "direct"

let visibility_of_string = function
  | "public" -> Some Public
  | "unlisted" -> Some Unlisted
  | "private" -> Some Private
  | "direct" -> Some Direct
  | _ -> None

type account_summary = {
  id : string;
  acct : string;
  username : string option;
  display_name : string option;
  url : string option;
}

let lenient_string name json =
  match string_opt name json with Ok (Some value) -> value | Ok None | Error _ -> ""

let lenient_non_empty name json =
  match non_empty_string_opt name json with Ok value -> value | Error _ -> None

let account_summary_of_json json : account_summary =
  {
    id = lenient_string "id" json;
    acct = lenient_string "acct" json;
    username = lenient_non_empty "username" json;
    display_name = lenient_non_empty "display_name" json;
    url = lenient_non_empty "url" json;
  }

let account_summary_of_json_result json =
  let* id = string_req "id" json in
  let* acct = string_req "acct" json in
  let* username = non_empty_string_opt "username" json in
  let* display_name = non_empty_string_opt "display_name" json in
  let* url = non_empty_string_opt "url" json in
  Ok ({ id; acct; username; display_name; url } : account_summary)

type account = {
  summary : account_summary;
  followers_count : int;
  following_count : int;
  statuses_count : int;
  last_status_at : string option;
  created_at : string;
  locked : bool;
  bot : bool;
  hide_collections : bool option;
}

let bool_opt name json =
  match field name json with
  | None -> Ok None
  | Some (`Bool value) -> Ok (Some value)
  | Some (`Null | `String _ | `Int _ | `Float _ | `Assoc _ | `List _) -> type_error name "a boolean"

let account_of_json json =
  let* summary = account_summary_of_json_result json in
  (let* followers_count = int_req "followers_count" json in
   let* following_count = int_req "following_count" json in
   let* statuses_count = int_req "statuses_count" json in
   let* last_status_at = string_opt "last_status_at" json in
   let* created_at = string_req "created_at" json in
   let* locked = bool_req "locked" json in
   let* bot = bool_req "bot" json in
   let* hide_collections = bool_opt "hide_collections" json in
   Ok
     { summary; followers_count; following_count; statuses_count; last_status_at; created_at;
       locked; bot; hide_collections })
  |> prefix_error ("account " ^ summary.id)

type pagination_cursors = {
  next_max_id : string option;
  prev_since_id : string option;
  prev_min_id : string option;
}

let empty_cursors = { next_max_id = None; prev_since_id = None; prev_min_id = None }

let link_entry_regexp = Str.regexp "<\\([^>]+\\)>;[ ]*rel=\"\\([^\"]+\\)\""

let parse_link_entry entry =
  let trimmed = String.trim entry in
  if Str.string_match link_entry_regexp trimmed 0 then
    Some (Str.matched_group 2 trimmed, Uri.of_string (Str.matched_group 1 trimmed))
  else None

let first_some primary fallback =
  match primary with Some _ -> primary | None -> fallback

let add_link_cursor cursors (rel, uri) =
  match rel with
  | "next" ->
      {
        cursors with
        next_max_id = first_some (Uri.get_query_param uri "max_id") cursors.next_max_id;
      }
  | "prev" ->
      {
        cursors with
        prev_since_id =
          first_some (Uri.get_query_param uri "since_id") cursors.prev_since_id;
        prev_min_id = first_some (Uri.get_query_param uri "min_id") cursors.prev_min_id;
      }
  | _ -> cursors

let pagination_cursors_of_headers headers =
  match header_value headers "link" with
  | None -> empty_cursors
  | Some link_header ->
      link_header
      |> String.split_on_char ','
      |> List.filter_map parse_link_entry
      |> List.fold_left add_link_cursor empty_cursors

(* {1 Status} *)

type status_visibility = Visibility of visibility | Other_visibility of string

type mention = { id : string; username : string; acct : string; url : string }

type status = {
  id : string;
  uri : string;
  url : string option;
  created_at : string;
  edited_at : string option;
  account : account_summary;
  content : string;
  spoiler_text : string;
  sensitive : bool;
  visibility : status_visibility;
  language : string option;
  in_reply_to_id : string option;
  in_reply_to_account_id : string option;
  reblog : status option;
  mentions : mention list;
  replies_count : int;
  reblogs_count : int;
  favourites_count : int;
}

let mention_of_json json =
  let* id = string_req "id" json in
  let* username = string_req "username" json in
  let* acct = string_req "acct" json in
  let* url = string_req "url" json in
  Ok ({ id; username; acct; url } : mention)

let status_visibility_of_string value =
  match visibility_of_string value with
  | Some visibility -> Visibility visibility
  | None -> Other_visibility value

let rec status_of_json json =
  let* id = string_req "id" json in
  prefix_error ("status " ^ id) (status_fields_of_json ~id json)

and status_fields_of_json ~id json =
  let* uri = string_req "uri" json in
  let* url = non_empty_string_opt "url" json in
  let* created_at = string_req "created_at" json in
  let* edited_at = string_opt "edited_at" json in
  let* account =
    object_req "account" account_summary_of_json_result json
    |> prefix_error "account"
  in
  let* content = string_req "content" json in
  let* spoiler_text = string_req "spoiler_text" json in
  let* sensitive = bool_req "sensitive" json in
  let* visibility = string_req "visibility" json in
  let* language = string_opt "language" json in
  let* in_reply_to_id = string_opt "in_reply_to_id" json in
  let* in_reply_to_account_id = string_opt "in_reply_to_account_id" json in
  let* reblog = object_opt "reblog" status_of_json json in
  let* mentions = list_req "mentions" mention_of_json json in
  let* replies_count = int_req "replies_count" json in
  let* reblogs_count = int_req "reblogs_count" json in
  let* favourites_count = int_req "favourites_count" json in
  Ok
    {
      id;
      uri;
      url;
      created_at;
      edited_at;
      account;
      content;
      spoiler_text;
      sensitive;
      visibility = status_visibility_of_string visibility;
      language;
      in_reply_to_id;
      in_reply_to_account_id;
      reblog;
      mentions;
      replies_count;
      reblogs_count;
      favourites_count;
    }

let statuses_of_json json = array_of_json "statuses" status_of_json json

(* {1 Context} *)

type async_refresh = {
  refresh_id : string;
  retry_seconds : int;
  result_count : int option;
}

(* [id="<string>", retry=<int>, result_count=<int>] (a structured-field
   dictionary). The id is opaque and may contain "=" and "-", never a quote. *)
let async_refresh_param_regexp =
  Str.regexp "[ ]*\\([a-z_]+\\)=\\(\"\\([^\"]*\\)\"\\|\\([0-9]+\\)\\)[ ]*"

let parse_async_refresh_param part =
  if Str.string_match async_refresh_param_regexp part 0
     && Str.match_end () = String.length part
  then
    let key = Str.matched_group 1 part in
    let value =
      try Str.matched_group 3 part with Not_found -> Str.matched_group 4 part
    in
    Some (key, value)
  else None

let async_refresh_of_headers headers =
  match header_value headers "mastodon-async-refresh" with
  | None -> None
  | Some value -> (
      let params =
        value |> String.split_on_char ',' |> List.filter_map parse_async_refresh_param
      in
      let int_param key = Option.bind (List.assoc_opt key params) int_of_string_opt in
      match (List.assoc_opt "id" params, int_param "retry") with
      | Some refresh_id, Some retry_seconds ->
          Some { refresh_id; retry_seconds; result_count = int_param "result_count" }
      | Some _, None | None, Some _ | None, None -> None)

type status_context = {
  ancestors : status list;
  descendants : status list;
  async_refresh : async_refresh option;
}

let status_context_of_response ~headers json =
  let* ancestors = list_req "ancestors" status_of_json json in
  let* descendants = list_req "descendants" status_of_json json in
  Ok { ancestors; descendants; async_refresh = async_refresh_of_headers headers }

(* {1 Notifications} *)

type notification_type =
  | Mention
  | Status
  | Reblog
  | Follow
  | Follow_request
  | Favourite
  | Poll
  | Update
  | Admin_sign_up
  | Admin_report
  | Severed_relationships
  | Moderation_warning
  | Quote
  | Quoted_update
  | Added_to_collection
  | Collection_update
  | Other_notification_type of string

let notification_type_to_string = function
  | Mention -> "mention"
  | Status -> "status"
  | Reblog -> "reblog"
  | Follow -> "follow"
  | Follow_request -> "follow_request"
  | Favourite -> "favourite"
  | Poll -> "poll"
  | Update -> "update"
  | Admin_sign_up -> "admin.sign_up"
  | Admin_report -> "admin.report"
  | Severed_relationships -> "severed_relationships"
  | Moderation_warning -> "moderation_warning"
  | Quote -> "quote"
  | Quoted_update -> "quoted_update"
  | Added_to_collection -> "added_to_collection"
  | Collection_update -> "collection_update"
  | Other_notification_type value -> value

let notification_type_of_string = function
  | "mention" -> Mention
  | "status" -> Status
  | "reblog" -> Reblog
  | "follow" -> Follow
  | "follow_request" -> Follow_request
  | "favourite" -> Favourite
  | "poll" -> Poll
  | "update" -> Update
  | "admin.sign_up" -> Admin_sign_up
  | "admin.report" -> Admin_report
  | "severed_relationships" -> Severed_relationships
  | "moderation_warning" -> Moderation_warning
  | "quote" -> Quote
  | "quoted_update" -> Quoted_update
  | "added_to_collection" -> Added_to_collection
  | "collection_update" -> Collection_update
  | other -> Other_notification_type other

type notification = {
  id : string;
  notification_type : notification_type;
  group_key : string option;
  created_at : string;
  account : account_summary;
  status : status option;
}

let notification_fields_of_json ~id json =
  let* notification_type = string_req "type" json in
  let* group_key = string_opt "group_key" json in
  let* created_at = string_req "created_at" json in
  let* account =
    object_req "account" account_summary_of_json_result json
    |> prefix_error "account"
  in
  let* status = object_opt "status" status_of_json json in
  Ok
    {
      id;
      notification_type = notification_type_of_string notification_type;
      group_key;
      created_at;
      account;
      status;
    }

let notification_of_json json =
  let* id = string_req "id" json in
  prefix_error ("notification " ^ id) (notification_fields_of_json ~id json)

type notification_page = {
  notifications : notification list;
  cursors : pagination_cursors;
}

let notification_page_of_response ~headers json =
  let* notifications = array_of_json "notifications" notification_of_json json in
  Ok
    ({ notifications; cursors = pagination_cursors_of_headers headers }
      : notification_page)

(* {1 Status pages} *)

type status_page = { statuses : status list; cursors : pagination_cursors }

let status_page_of_response ~headers json =
  let* statuses = statuses_of_json json in
  Ok ({ statuses; cursors = pagination_cursors_of_headers headers } : status_page)

(* {1 WebFinger} *)

type webfinger_link = {
  rel : string;
  link_type : string option;
  href : string option;
  template : string option;
}

type webfinger = { subject : string; aliases : string list; links : webfinger_link list }

let webfinger_link_of_json json =
  match string_opt "rel" json with
  | Ok (Some rel) ->
      let optional name = match string_opt name json with Ok value -> value | Error _ -> None in
      Some
        {
          rel;
          link_type = optional "type";
          href = optional "href";
          template = optional "template";
        }
  | Ok None | Error _ -> None

let json_strings (json : json) =
  match json with
  | `List items ->
      List.filter_map
        (function
          | `String value -> Some value
          | `Null | `Bool _ | `Int _ | `Float _ | `Assoc _ | `List _ -> None)
        items
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `Assoc _ -> []

let json_objects (json : json) =
  match json with
  | `List items -> items
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `Assoc _ -> []

let webfinger_of_json json =
  let* subject = string_req "subject" json in
  let aliases = Option.fold ~none:[] ~some:json_strings (field "aliases" json) in
  let links =
    Option.fold ~none:[] ~some:json_objects (field "links" json)
    |> List.filter_map webfinger_link_of_json
  in
  Ok { subject; aliases; links }

let is_activitypub_media_type media_type =
  match
    media_type
    |> String.split_on_char ';'
    |> List.map (fun part -> String.lowercase_ascii (String.trim part))
  with
  | [ "application/activity+json" ] -> true
  | [ "application/ld+json"; profile ] ->
      String.equal profile "profile=\"https://www.w3.org/ns/activitystreams\""
  | _ -> false

let webfinger_actor_url webfinger =
  List.find_map
    (fun link ->
      match (link.rel, link.link_type, link.href) with
      | "self", Some media_type, Some href when is_activitypub_media_type media_type ->
          Some href
      | _ -> None)
    webfinger.links

let webfinger_profile_page webfinger =
  List.find_map
    (fun link ->
      match (link.rel, link.href) with
      | "http://webfinger.net/rel/profile-page", Some href -> Some href
      | _ -> None)
    webfinger.links

let strip_prefix ~prefix value =
  if String.starts_with ~prefix value then
    String.sub value (String.length prefix) (String.length value - String.length prefix)
  else value

let split_acct value =
  let bare = value |> String.trim |> strip_prefix ~prefix:"acct:" |> strip_prefix ~prefix:"@" in
  match String.split_on_char '@' bare with
  | [ username; domain ] when username <> "" && domain <> "" -> Some (username, domain)
  | _ -> None

let webfinger_resource ~username ~domain = Printf.sprintf "acct:%s@%s" username domain

(* {1 HTML to plain text} *)

(* Blocks that end a line. [p] is handled separately (blank line). *)
let line_block_tags = [ "li"; "div"; "blockquote"; "pre"; "ul"; "ol"; "h1"; "h2"; "h3"; "h4"; "h5"; "h6" ]

(* [Some (closing, name)] for the tag text between '<' and '>'. *)
let tag_name tag =
  let closing = String.length tag > 0 && tag.[0] = '/' in
  let body = if closing then String.sub tag 1 (String.length tag - 1) else tag in
  let stop =
    String.to_seq body
    |> Seq.fold_lefti
         (fun found index char ->
           match (found, char) with
           | Some _, _ -> found
           | None, (' ' | '\t' | '\n' | '\r' | '/') -> Some index
           | None, _ -> None)
         None
    |> Option.value ~default:(String.length body)
  in
  (closing, String.lowercase_ascii (String.sub body 0 stop))

let tag_separator tag =
  match tag_name tag with
  | _, "br" -> "\n"
  | true, "p" -> "\n\n"
  | true, name when List.mem name line_block_tags -> "\n"
  | (true | false), _ -> ""

let named_entities =
  [ ("amp", "&"); ("lt", "<"); ("gt", ">"); ("quot", "\""); ("apos", "'"); ("nbsp", "\xC2\xA0") ]

let utf8_of_code_point code =
  if Uchar.is_valid code then (
    let buffer = Buffer.create 4 in
    Buffer.add_utf_8_uchar buffer (Uchar.of_int code);
    Some (Buffer.contents buffer))
  else None

(* [Some text] for the entity name between '&' and ';'. *)
let decode_entity name =
  let length = String.length name in
  if length > 2 && name.[0] = '#' && (name.[1] = 'x' || name.[1] = 'X') then
    Option.bind (int_of_string_opt ("0x" ^ String.sub name 2 (length - 2))) utf8_of_code_point
  else if length > 1 && name.[0] = '#' then
    let digits = String.sub name 1 (length - 1) in
    if String.for_all (fun c -> c >= '0' && c <= '9') digits then
      Option.bind (int_of_string_opt digits) utf8_of_code_point
    else None
  else List.assoc_opt name named_entities

(* Entities are at most a few characters; a lone '&' must not swallow the
   rest of the text looking for ';'. *)
let max_entity_length = 10

let rec strip_html buffer html index =
  let length = String.length html in
  if index >= length then ()
  else
    match html.[index] with
    | '<' -> (
        match String.index_from_opt html index '>' with
        | Some close ->
            Buffer.add_string buffer
              (tag_separator (String.sub html (index + 1) (close - index - 1)));
            strip_html buffer html (close + 1)
        | None ->
            Buffer.add_string buffer (String.sub html index (length - index)))
    | '&' -> (
        let decoded =
          match String.index_from_opt html index ';' with
          | Some semi when semi - index - 1 <= max_entity_length ->
              Option.map
                (fun text -> (text, semi + 1))
                (decode_entity (String.sub html (index + 1) (semi - index - 1)))
          | Some _ | None -> None
        in
        match decoded with
        | Some (text, next) ->
            Buffer.add_string buffer text;
            strip_html buffer html next
        | None ->
            Buffer.add_char buffer '&';
            strip_html buffer html (index + 1))
    | char ->
        Buffer.add_char buffer char;
        strip_html buffer html (index + 1)

let collapse_blank_lines text =
  Str.global_replace (Str.regexp "\n\n\n+") "\n\n" text

let html_to_plain_text html =
  let buffer = Buffer.create (String.length html) in
  strip_html buffer html 0;
  Buffer.contents buffer |> collapse_blank_lines |> String.trim

(* {1 URL helpers} *)

let status_web_url (status : status) =
  match status.url with
  | Some url when String.trim url <> "" -> url
  | Some _ | None -> status.uri

let status_id_of_url url =
  let uri = Uri.of_string (String.trim url) in
  match Uri.scheme uri with
  | Some ("http" | "https") -> (
      match
        Uri.path uri |> String.split_on_char '/' |> List.filter (fun s -> s <> "") |> List.rev
      with
      | last :: _ :: _ -> Some last
      | [ _ ] | [] -> None)
  | Some _ | None -> None

let strip_trailing_slashes value =
  let rec last_kept index = if index > 0 && value.[index - 1] = '/' then last_kept (index - 1) else index in
  String.sub value 0 (last_kept (String.length value))

let normalize_instance_url value =
  let trimmed = String.trim value in
  let with_scheme =
    if String.starts_with ~prefix:"http://" trimmed || String.starts_with ~prefix:"https://" trimmed
    then trimmed
    else "https://" ^ trimmed
  in
  let normalized = strip_trailing_slashes with_scheme in
  match Uri.host (Uri.of_string normalized) with
  | Some host when String.trim host <> "" -> Some normalized
  | Some _ | None -> None

let federated_acct ~username ~instance_url =
  let username = username |> String.trim |> strip_prefix ~prefix:"@" in
  if String.contains username '@' then username
  else
    match Uri.host (Uri.of_string instance_url) with
    | Some host when String.trim host <> "" ->
        Printf.sprintf "%s@%s" username (String.lowercase_ascii (String.trim host))
    | Some _ | None -> username
