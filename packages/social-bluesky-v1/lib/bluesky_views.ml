(* Typed app.bsky views. See the .mli for sources and contracts. *)

type json = Yojson.Basic.t

let ( let* ) = Result.bind

(* {1 JSON field access} *)

let field name (json : json) =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt name fields with
      | Some `Null | None -> None
      | Some value -> Some value)
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> None

let missing name = Error (Printf.sprintf "%s: missing" name)
let type_error name expected = Error (Printf.sprintf "%s: expected %s" name expected)

let string_opt name json =
  match field name json with
  | None -> Ok None
  | Some (`String value) -> Ok (Some value)
  | Some (`Null | `Bool _ | `Int _ | `Float _ | `Assoc _ | `List _) ->
      type_error name "a string"

let string_req name json =
  let* value = string_opt name json in
  match value with Some value -> Ok value | None -> missing name

let int_opt name json =
  match field name json with
  | None -> Ok None
  | Some (`Int value) -> Ok (Some value)
  | Some (`Null | `Bool _ | `String _ | `Float _ | `Assoc _ | `List _) ->
      type_error name "an integer"

let bool_req name json =
  match field name json with
  | Some (`Bool value) -> Ok value
  | None -> missing name
  | Some (`Null | `String _ | `Int _ | `Float _ | `Assoc _ | `List _) ->
      type_error name "a boolean"

let object_req name json =
  match field name json with
  | Some (`Assoc _ as value) -> Ok value
  | None -> missing name
  | Some (`Null | `Bool _ | `String _ | `Int _ | `Float _ | `List _) ->
      type_error name "an object"

(* Tail-recursive: a thread level or a feed page can be long. *)
let map_result f items =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | item :: rest -> (
        match f item with
        | Ok value -> loop (value :: acc) rest
        | Error _ as error -> error)
  in
  loop [] items

let list_field ~required name f json =
  match field name json with
  | Some (`List items) -> map_result f items
  | None -> if required then missing name else Ok []
  | Some (`Null | `Bool _ | `String _ | `Int _ | `Float _ | `Assoc _) ->
      type_error name "an array"

let prefix_error context = Result.map_error (fun msg -> context ^ "." ^ msg)

(* {1 References} *)

type strong_ref = { uri : string; cid : string }
type reply_ref = { root : strong_ref; parent : strong_ref }

let strong_ref_of_json json : strong_ref option =
  match (string_opt "uri" json, string_opt "cid" json) with
  | Ok (Some uri), Ok (Some cid) -> Some { uri; cid }
  | (Ok _ | Error _), (Ok _ | Error _) -> None

let post_record_reply record =
  match field "reply" record with
  | None -> None
  | Some reply -> (
      match
        ( Option.bind (field "root" reply) strong_ref_of_json,
          Option.bind (field "parent" reply) strong_ref_of_json )
      with
      | Some root, Some parent -> Some { root; parent }
      | Some _, None | None, Some _ | None, None -> None)

let post_record_text record =
  match string_opt "text" record with Ok text -> text | Error _ -> None

(* {1 Posts} *)

type profile_basic = {
  did : string;
  handle : string;
  display_name : string option;
  avatar : string option;
}

let profile_basic_of_json json =
  let* did = string_req "did" json in
  let* handle = string_req "handle" json in
  let* display_name = string_opt "displayName" json in
  let* avatar = string_opt "avatar" json in
  Ok { did; handle; display_name; avatar }

let profile_field name json =
  let* profile = object_req name json in
  profile_basic_of_json profile |> prefix_error name

type post_view = {
  uri : string;
  cid : string;
  author : profile_basic;
  record : json;
  text : string;
  created_at : string;
  reply : reply_ref option;
  indexed_at : string;
  reply_count : int option;
  repost_count : int option;
  like_count : int option;
  quote_count : int option;
}

let post_view_fields_of_json ~uri json =
  let* cid = string_req "cid" json in
  let* author = profile_field "author" json in
  let* record = object_req "record" json in
  let* text = string_req "text" record |> prefix_error "record" in
  let* created_at = string_req "createdAt" record |> prefix_error "record" in
  let* indexed_at = string_req "indexedAt" json in
  let* reply_count = int_opt "replyCount" json in
  let* repost_count = int_opt "repostCount" json in
  let* like_count = int_opt "likeCount" json in
  let* quote_count = int_opt "quoteCount" json in
  Ok
    {
      uri;
      cid;
      author;
      record;
      text;
      created_at;
      reply = post_record_reply record;
      indexed_at;
      reply_count;
      repost_count;
      like_count;
      quote_count;
    }

let post_view_of_json json =
  let* uri = string_req "uri" json in
  post_view_fields_of_json ~uri json |> prefix_error ("post " ^ uri)

type blocked_post = { uri : string; author_did : string }

type 'post post_union =
  | Post of 'post
  | Not_found_post of string
  | Blocked_post of blocked_post
  | Unknown_post of string

let blocked_post_of_json json =
  let* uri = string_req "uri" json in
  let* author = object_req "author" json in
  let* author_did = string_req "did" author |> prefix_error "author" in
  Ok (Blocked_post { uri; author_did })

(* [view_type] is the [$type] of the member that carries the post itself
   ([#postView] in reply refs, [#threadViewPost] in threads). *)
let post_union_of_json ~view_type parse json =
  let* type_name = string_req "$type" json in
  if String.equal type_name view_type then Result.map (fun post -> Post post) (parse json)
  else
    match type_name with
    | "app.bsky.feed.defs#notFoundPost" ->
        Result.map (fun uri -> Not_found_post uri) (string_req "uri" json)
    | "app.bsky.feed.defs#blockedPost" -> blocked_post_of_json json
    | other -> Ok (Unknown_post other)

(* {1 Threads} *)

type thread = thread_view_post post_union

and thread_view_post = {
  post : post_view;
  parent : thread option;
  replies : thread list;
}

let rec thread_of_json json =
  post_union_of_json ~view_type:"app.bsky.feed.defs#threadViewPost"
    thread_view_post_of_json json

and thread_view_post_of_json json =
  let* post_json = object_req "post" json in
  let* post = post_view_of_json post_json |> prefix_error "post" in
  let* parent =
    match field "parent" json with
    | None -> Ok None
    | Some parent -> Result.map Option.some (thread_of_json parent)
  in
  let* replies = list_field ~required:false "replies" thread_of_json json in
  Ok { post; parent; replies }

let post_thread_of_json json =
  match field "thread" json with
  | Some thread -> thread_of_json thread |> prefix_error "thread"
  | None -> missing "thread"

(* {1 Feeds} *)

type feed_reply = {
  root : post_view post_union;
  parent : post_view post_union;
  grandparent_author : profile_basic option;
}

type feed_reason =
  | Reason_repost of { by : profile_basic; indexed_at : string }
  | Reason_pin
  | Other_feed_reason of string

type feed_view_post = {
  post : post_view;
  reply : feed_reply option;
  reason : feed_reason option;
}

let reply_post_union json =
  post_union_of_json ~view_type:"app.bsky.feed.defs#postView" post_view_of_json json

let feed_reply_of_json json =
  let* root_json = object_req "root" json in
  let* root = reply_post_union root_json |> prefix_error "root" in
  let* parent_json = object_req "parent" json in
  let* parent = reply_post_union parent_json |> prefix_error "parent" in
  let* grandparent_author =
    match field "grandparentAuthor" json with
    | None -> Ok None
    | Some author ->
        Result.map Option.some
          (profile_basic_of_json author |> prefix_error "grandparentAuthor")
  in
  Ok { root; parent; grandparent_author }

let feed_reason_of_json json =
  let* type_name = string_req "$type" json in
  match type_name with
  | "app.bsky.feed.defs#reasonRepost" ->
      let* by = profile_field "by" json in
      let* indexed_at = string_req "indexedAt" json in
      Ok (Reason_repost { by; indexed_at })
  | "app.bsky.feed.defs#reasonPin" -> Ok Reason_pin
  | other -> Ok (Other_feed_reason other)

let optional_object name parse json =
  match field name json with
  | None -> Ok None
  | Some value -> Result.map Option.some (parse value |> prefix_error name)

let feed_view_post_of_json json =
  let* post_json = object_req "post" json in
  let* post = post_view_of_json post_json in
  let* reply = optional_object "reply" feed_reply_of_json json in
  let* reason = optional_object "reason" feed_reason_of_json json in
  Ok ({ post; reply; reason } : feed_view_post)

type author_feed_page = { feed : feed_view_post list; cursor : string option }

let author_feed_page_of_json json =
  let* feed = list_field ~required:true "feed" feed_view_post_of_json json in
  let* cursor = string_opt "cursor" json in
  Ok ({ feed; cursor } : author_feed_page)

type author_feed_filter =
  | Posts_with_replies
  | Posts_no_replies
  | Posts_with_media
  | Posts_and_author_threads
  | Posts_with_video

let author_feed_filter_to_string = function
  | Posts_with_replies -> "posts_with_replies"
  | Posts_no_replies -> "posts_no_replies"
  | Posts_with_media -> "posts_with_media"
  | Posts_and_author_threads -> "posts_and_author_threads"
  | Posts_with_video -> "posts_with_video"

let posts_of_json json = list_field ~required:true "posts" post_view_of_json json

(* {1 Notifications} *)

type notification_reason =
  | Like
  | Repost
  | Follow
  | Mention
  | Reply
  | Quote
  | Starterpack_joined
  | Verified
  | Unverified
  | Like_via_repost
  | Repost_via_repost
  | Subscribed_post
  | Contact_match
  | Other_notification_reason of string

let notification_reason_to_string = function
  | Like -> "like"
  | Repost -> "repost"
  | Follow -> "follow"
  | Mention -> "mention"
  | Reply -> "reply"
  | Quote -> "quote"
  | Starterpack_joined -> "starterpack-joined"
  | Verified -> "verified"
  | Unverified -> "unverified"
  | Like_via_repost -> "like-via-repost"
  | Repost_via_repost -> "repost-via-repost"
  | Subscribed_post -> "subscribed-post"
  | Contact_match -> "contact-match"
  | Other_notification_reason value -> value

let notification_reason_of_string = function
  | "like" -> Like
  | "repost" -> Repost
  | "follow" -> Follow
  | "mention" -> Mention
  | "reply" -> Reply
  | "quote" -> Quote
  | "starterpack-joined" -> Starterpack_joined
  | "verified" -> Verified
  | "unverified" -> Unverified
  | "like-via-repost" -> Like_via_repost
  | "repost-via-repost" -> Repost_via_repost
  | "subscribed-post" -> Subscribed_post
  | "contact-match" -> Contact_match
  | other -> Other_notification_reason other

type notification = {
  uri : string;
  cid : string;
  author : profile_basic;
  reason : notification_reason;
  reason_subject : string option;
  record : json;
  is_read : bool;
  indexed_at : string;
}

let notification_fields_of_json ~uri json =
  let* cid = string_req "cid" json in
  let* author = profile_field "author" json in
  let* reason = string_req "reason" json in
  let* reason_subject = string_opt "reasonSubject" json in
  let* record = object_req "record" json in
  let* is_read = bool_req "isRead" json in
  let* indexed_at = string_req "indexedAt" json in
  Ok
    ({
       uri;
       cid;
       author;
       reason = notification_reason_of_string reason;
       reason_subject;
       record;
       is_read;
       indexed_at;
     }
      : notification)

let notification_of_json json =
  let* uri = string_req "uri" json in
  notification_fields_of_json ~uri json |> prefix_error ("notification " ^ uri)

type notifications_page = {
  notifications : notification list;
  cursor : string option;
  seen_at : string option;
}

let notifications_page_of_json json =
  let* notifications =
    list_field ~required:true "notifications" notification_of_json json
  in
  let* cursor = string_opt "cursor" json in
  let* seen_at = string_opt "seenAt" json in
  Ok ({ notifications; cursor; seen_at } : notifications_page)

(* {1 Identity} *)

let did_of_resolve_handle_json json =
  let* did = string_req "did" json in
  if String.equal did "" then missing "did" else Ok did

(* {1 URL helpers} *)

let rkey_of_uri uri =
  let prefix = "at://" in
  if String.starts_with ~prefix uri then
    let path = String.sub uri (String.length prefix) (String.length uri - String.length prefix) in
    match String.split_on_char '/' path with
    | [ authority; collection; rkey ] when authority <> "" && collection <> "" && rkey <> "" ->
        Some rkey
    | _ -> None
  else None

let post_web_url ~handle ~uri =
  Option.map
    (fun rkey -> Printf.sprintf "https://bsky.app/profile/%s/post/%s" handle rkey)
    (rkey_of_uri uri)
