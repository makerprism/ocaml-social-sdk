(** Token-based user post listings, exposed as [Posts].
    {{:https://developers.facebook.com/documentation/threads/retrieve-and-discover-posts/retrieve-posts}Official
     fields and response reference}. Use [get_user_media_page] (Instagram) or
    [get_user_posts_page] (Threads) with [~access_token], [~fields] including
    [id], and optional [~position] ([First], [After], [Before], or checked
    [Page_url]). No stored account is needed. Requires [threads_basic]. [limit]
    defaults to 20 and is clamped to 1..100. Page URLs retain their query
    options and replace embedded credentials. Threads cursor-only nonempty pages
    get a synthesized next URL; an empty page ends traversal. Instagram
    preserves Meta's next URL as returned. *)

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
(** Selected common fields; unrequested fields are [None]. [raw] preserves the
    complete item, including unknown fields and nested expansions. *)

val of_json : Yojson.Basic.t -> (post, string) result
(** Requires a nonempty string [id]; other fields are optional. *)
