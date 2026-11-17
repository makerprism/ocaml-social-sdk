(** Facebook Page post comments: the typed shape of
    [GET /{object-id}/comments], exposed as
    [Social_facebook_graph_v25.Comments].

    The requests themselves live in [Make] ([get_comments_page],
    [reply_to_comment], [set_comment_hidden], [delete_comment],
    [like_object], [unlike_object]); this module is pure.

    Platform facts the types follow:
    - {{:https://developers.facebook.com/docs/graph-api/reference/object/comments/}
      Comments edge}: [filter=toplevel] (default) lists top-level comments;
      [filter=stream] lists every level as one flat, chronological list,
      "useful for moderation tools", in which [parent] places a reply under
      its comment. [order] ([chronological] / [reverse_chronological]) is
      documented as the order the edge reports in its [summary]; the edge
      also takes it as a request parameter, which the reference's parameter
      table does not list.
    - {{:https://developers.facebook.com/docs/graph-api/reference/comment/}
      Comment node}: [parent] is "the comment this is a reply to"; comments
      have their own [/comments] edge.
    - Facebook shows comments two levels deep, so a reply meant for a
      second-level comment belongs on its top-level comment (the
      second-level comment's [parent_id]). Choosing that target is the
      caller's decision.
    - [from] is not always returned. Meta leaves out other people's profile
      fields unless the app has
      {{:https://developers.facebook.com/docs/features-reference/business-asset-user-profile-access}
      Business Asset User Profile Access} or the person authorised the app,
      so it is optional here. *)

(** [filter] request parameter. *)
type filter =
  | Toplevel  (** Top-level comments only (the API default). *)
  | Stream  (** Every level as one flat list. *)

(** [order] request parameter. *)
type order =
  | Chronological  (** Oldest first. *)
  | Reverse_chronological  (** Newest first. *)

val string_of_filter : filter -> string
val string_of_order : order -> string

(** The commenting Page or person ([from{id,name}]). *)
type author = {
  author_id : string;  (** A Page-scoped user id, or the Page's id *)
  author_name : string option;
}

type comment = {
  id : string;
  message : string option;  (** [None] for an attachment-only comment *)
  created_time : string option;  (** ISO 8601, e.g. ["2026-11-16T09:12:44+0000"] *)
  from : author option;  (** See the module notes: Meta can omit it. *)
  permalink_url : string option;
  parent_id : string option;
      (** [parent{id}]: the comment this one replies to; [None] for a
          top-level comment. *)
}

val fields : string
(** ["id,message,created_time,from{id,name},permalink_url,parent{id}"], the
    field list {!parse_comment} reads. *)

val parse_comment : Yojson.Basic.t -> comment
(** Raises [Failure] when the item has no string [id]; every other field is
    optional. *)
