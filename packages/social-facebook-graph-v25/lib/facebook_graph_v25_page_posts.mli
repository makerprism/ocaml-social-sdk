(** Facebook Page posts: the typed shape of the Page feed edges
    ([GET /{page-id}/posts], [/feed], [/published_posts]), exposed as
    [Social_facebook_graph_v25.Page_posts].

    The request lives in [Make] ([get_page_posts_page],
    [get_page_posts_page_for_account]); this module is pure.

    Platform facts, from the
    {{:https://developers.facebook.com/docs/graph-api/reference/page/feed/}
    Page Feed reference} (checked 2026-11-17):
    - [/feed] holds the Page's own posts and links and also public posts by
      others that tag the Page. [/posts] lists only the posts the Page
      published; [/published_posts] only its published (not scheduled)
      posts. Each is a cursor-paged list ({!Social_meta_graph_paging}).
    - [limit] is at most 100; a larger value is an error. Meta returns
      roughly 600 ranked published posts per year.
    - Reading needs a Page access token, a Page task of CREATE_CONTENT,
      MANAGE or MODERATE, and [pages_read_engagement] plus
      [pages_read_user_content].
    - The post-level [type], [link], [object_id] and similar fields are
      deprecated since v3.3; [attachments] carries the media instead.
    - A field expansion such as [insights.metric(...)] is dropped from the
      whole response, without an error, when any listed metric is invalid
      for a post. {!analytics_fields} lists a metric set that survives every
      post type (see its note).
    - Reels are videos whose permalink contains [/reel/]; their views live
      on [/{video-id}/video_insights], where the video id is the first
      attachment's [target.id]. *)

(** Which Page feed edge to read. *)
type edge =
  | Posts  (** Posts the Page published. *)
  | Feed  (** The Page's posts plus public posts that tag the Page. *)
  | Published_posts  (** Only published (not scheduled) posts of the Page. *)

val string_of_edge : edge -> string

(** One [attachments.data] item. [media_type] and [attachment_type] stay
    strings: Meta documents examples (["photo"], ["video"], ["link"],
    ["album"]; ["video_inline"], ["share"], ...) but no closed set. *)
type attachment = {
  attachment_type : string option;  (** [type] *)
  media_type : string option;  (** [media_type] *)
  url : string option;
  image_src : string option;  (** [media.image.src] *)
  target_id : string option;  (** [target.id]: the photo or video object *)
  subattachments : attachment list;  (** [subattachments.data] (albums) *)
}

type page_post = {
  id : string;
  message : string option;
  created_time : string option;  (** ISO 8601, e.g. ["2026-11-16T09:12:44+0000"] *)
  full_picture : string option;
  permalink_url : string option;
  attachments : attachment list;
  insights : Facebook_graph_v25_insights.metric list;
      (** The [insights] expansion; [[]] when it was not requested or Meta
          dropped it (see the module notes). *)
  comment_count : int option;
      (** [comments_summary.summary.total_count] ({!analytics_fields}). *)
  reaction_count : int option;
      (** [reactions_summary.summary.total_count] ({!analytics_fields}). *)
  share_count : int option;  (** [shares.count]; absent when never shared. *)
}

val basic_fields : string
(** Post fields without engagement:
    ["id,message,created_time,full_picture,permalink_url,attachments{type,media_type,url,media,subattachments,target{id}}"]. *)

val analytics_fields : string
(** {!basic_fields} plus inline insights ([post_impressions_unique],
    [post_reactions_by_type_total], [post_clicks], [post_clicks_by_type],
    aliased [insights]), comment and reaction summaries (aliased
    [comments_summary] / [reactions_summary]) and [shares]. The metric set
    is the one that has been seen to survive every post type in v25; the
    successors [post_total_media_view_unique] and
    [post_impressions_organic_unique] made Meta drop the whole expansion
    when tried on a live Page. *)

val parse_page_post : Yojson.Basic.t -> page_post
(** Raises [Failure] when the item has no string [id]; every other field is
    optional. An unreadable [insights] expansion reads as [[]]. *)

val media_type : page_post -> string option
(** The first attachment's [media_type]. *)

val is_reel : page_post -> bool
(** A video whose permalink contains [/reel/]. *)

val video_id_of : page_post -> string option
(** The first attachment's [target.id] when its [media_type] is
    ["video"]: the id for [/{video-id}/video_insights]. *)
