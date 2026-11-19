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

module J = Social_json_reader

(* Albums nest one level (subattachments of an attachment); the recursion
   follows the response, which Meta bounds. *)
let rec parse_attachment json =
  {
    attachment_type = J.nonempty_string_opt (J.member "type" json);
    media_type = J.nonempty_string_opt (J.member "media_type" json);
    url = J.nonempty_string_opt (J.member "url" json);
    image_src = J.nonempty_string_opt (J.path [ "media"; "image"; "src" ] json);
    target_id = J.nonempty_string_opt (J.path [ "target"; "id" ] json);
    subattachments = List.map parse_attachment (J.data_list (J.member "subattachments" json));
  }

let parse_page_post json =
  match J.nonempty_string_opt (J.member "id" json) with
  | None -> failwith "page post without an id"
  | Some id ->
      let insights =
        match J.member "insights" json with
        | `Null -> []
        | expansion -> (
            match Facebook_graph_v25_insights.metrics_of_json expansion with
            | Ok metrics -> metrics
            | Error _ -> [])
      in
      {
        id;
        message = J.nonempty_string_opt (J.member "message" json);
        created_time = J.nonempty_string_opt (J.member "created_time" json);
        full_picture = J.nonempty_string_opt (J.member "full_picture" json);
        permalink_url = J.nonempty_string_opt (J.member "permalink_url" json);
        attachments = List.map parse_attachment (J.data_list (J.member "attachments" json));
        insights;
        comment_count = J.int_opt (J.path [ "comments_summary"; "summary"; "total_count" ] json);
        reaction_count = J.int_opt (J.path [ "reactions_summary"; "summary"; "total_count" ] json);
        share_count = J.int_opt (J.path [ "shares"; "count" ] json);
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
