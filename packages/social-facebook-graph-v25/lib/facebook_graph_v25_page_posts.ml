type edge =
  | Posts
  | Feed
  | Published_posts

let string_of_edge = function
  | Posts -> "posts"
  | Feed -> "feed"
  | Published_posts -> "published_posts"

type attachment = {
  attachment_type : string option;
  media_type : string option;
  url : string option;
  image_src : string option;
  target_id : string option;
  subattachments : attachment list;
}

type page_post = {
  id : string;
  message : string option;
  created_time : string option;
  full_picture : string option;
  permalink_url : string option;
  attachments : attachment list;
  insights : Facebook_graph_v25_insights.metric list;
  comment_count : int option;
  reaction_count : int option;
  share_count : int option;
}

let basic_fields =
  "id,message,created_time,full_picture,permalink_url,\
   attachments{type,media_type,url,media,subattachments,target{id}}"

let analytics_fields =
  basic_fields
  ^ ",insights.metric(post_reactions_by_type_total,\
     post_clicks,post_clicks_by_type).as(insights),\
     comments.summary(true).limit(0).as(comments_summary),\
     reactions.summary(true).limit(0).as(reactions_summary),shares"

let member key = function
  | `Assoc pairs -> Option.value ~default:`Null (List.assoc_opt key pairs)
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> `Null

let path keys json = List.fold_left (fun json key -> member key json) json keys

let string_opt = function
  | `String s when s <> "" -> Some s
  | `String _ | `Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _ -> None

let int_opt = function
  | `Int i -> Some i
  | `String _ | `Null | `Bool _ | `Float _ | `List _ | `Assoc _ -> None

let data_list json =
  match member "data" json with
  | `List items -> items
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `Assoc _ -> []

(* Albums nest one level (subattachments of an attachment); the recursion
   follows the response, which Meta bounds. *)
let rec parse_attachment json =
  {
    attachment_type = string_opt (member "type" json);
    media_type = string_opt (member "media_type" json);
    url = string_opt (member "url" json);
    image_src = string_opt (path [ "media"; "image"; "src" ] json);
    target_id = string_opt (path [ "target"; "id" ] json);
    subattachments = List.map parse_attachment (data_list (member "subattachments" json));
  }

let parse_page_post json =
  match string_opt (member "id" json) with
  | None -> failwith "page post without an id"
  | Some id ->
      let insights =
        match member "insights" json with
        | `Null -> []
        | expansion -> (
            match Facebook_graph_v25_insights.metrics_of_json expansion with
            | Ok metrics -> metrics
            | Error _ -> [])
      in
      {
        id;
        message = string_opt (member "message" json);
        created_time = string_opt (member "created_time" json);
        full_picture = string_opt (member "full_picture" json);
        permalink_url = string_opt (member "permalink_url" json);
        attachments = List.map parse_attachment (data_list (member "attachments" json));
        insights;
        comment_count = int_opt (path [ "comments_summary"; "summary"; "total_count" ] json);
        reaction_count = int_opt (path [ "reactions_summary"; "summary"; "total_count" ] json);
        share_count = int_opt (path [ "shares"; "count" ] json);
      }

let first_attachment post =
  match post.attachments with
  | first :: _ -> Some first
  | [] -> None

let media_type post = Option.bind (first_attachment post) (fun a -> a.media_type)

let contains ~needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec at i = i + n <= h && (String.sub haystack i n = needle || at (i + 1)) in
  at 0

let is_reel post =
  match media_type post, post.permalink_url with
  | Some "video", Some url -> contains ~needle:"/reel/" url
  | Some _, _ | None, _ -> false

let video_id_of post =
  match first_attachment post with
  | Some { media_type = Some "video"; target_id; _ } -> target_id
  | Some _ | None -> None
