(** Instagram media comments: the typed shape of [GET /{ig-media-id}/comments]
    and [GET /{ig-comment-id}/replies], exposed as
    [Social_instagram_standalone_v25.Comments].

    The requests live in [Make_nonblocking] ([get_comments_page],
    [get_comment_replies_page], [reply_to_comment], [set_comment_hidden],
    [delete_comment]); this module is pure.

    Platform facts the types follow (Instagram API with Instagram Login,
    [graph.instagram.com], permissions [instagram_business_basic] and
    [instagram_business_manage_comments]):
    - {{:https://developers.facebook.com/docs/instagram-platform/instagram-graph-api/reference/ig-media/comments}
      Media comments}: newest first, at most 50 per page, top-level comments
      only. Replies come only through the [replies] field expansion, whose
      own [paging] leads to further pages of [/{ig-comment-id}/replies].
    - {{:https://developers.facebook.com/docs/instagram-platform/instagram-graph-api/reference/ig-comment}
      Comment node}: [id], [text], [username], [timestamp], [hidden],
      [parent_id] (set on replies), [like_count], [replies]. [username]
      needs [instagram_business_manage_comments] for media the account owns.
      Comments by the media owner stay visible even when hidden.
    - {{:https://developers.facebook.com/docs/instagram-platform/instagram-graph-api/reference/ig-comment/replies}
      Replies}: you can only reply to top-level comments (a reply to a reply
      is added to the top-level comment), not to hidden comments, and not on
      live video.
    - Instagram's API has no endpoint for liking a comment, so this package
      offers none. *)

type comment = {
  id : string;
  text : string option;
  username : string option;
      (** The commenter's username; see the permission note above. *)
  timestamp : string option;  (** ISO 8601, e.g. ["2026-11-16T09:12:44+0000"] *)
  hidden : bool option;
  parent_id : string option;  (** The top-level comment a reply belongs to. *)
  like_count : int option;
  replies : comment Meta_graph_paging.page option;
      (** First page of the [replies] expansion; [None] on replies and when
          the response has no [replies] object (no replies, or not
          requested). Fetch the rest with [get_comment_replies_page]. *)
}

val reply_fields : string
(** ["id,text,username,timestamp,hidden,parent_id,like_count"]: the fields
    read from a reply. *)

val comment_fields : replies_limit:int -> string
(** {!reply_fields} plus [replies.limit(replies_limit){<reply_fields>}]. *)

val parse_comment : Yojson.Basic.t -> comment
(** Raises [Failure] when the item has no string [id]; every other field is
    optional. A malformed [replies] object is read as [None]. *)
