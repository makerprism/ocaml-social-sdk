(** Token-based user post listings, exposed as [Posts].
    {{:https://developers.facebook.com/documentation/instagram-platform/reference/instagram-media}Official
     fields and response reference}. Use [get_user_media_page] (Instagram) or
    [get_user_posts_page] (Threads) with [~access_token], [~fields] including
    [id], and optional [~position] ([First], [After], [Before], or checked
    [Page_url]). No stored account is needed. The
    {{:https://developers.facebook.com/documentation/instagram-platform/instagram-graph-api/reference/ig-user/media}user
     media edge} returns published media objects. [limit] is clamped to 1..50.
    Page URLs retain their query options and replace embedded credentials.
    Threads cursor-only nonempty pages get a synthesized next URL; an empty page
    ends traversal. Instagram preserves Meta's next URL as returned. *)

type post = {
  id : string;
  caption : string option;
  media_type : string option;
  media_url : string option;
  thumbnail_url : string option;
      (** Video poster frame; returned only for videos, so clients fall back
          to it when [media_url] is absent on a [VIDEO]
          ({{:https://developers.facebook.com/docs/instagram-platform/instagram-graph-api/reference/ig-media}IG
           media reference}). *)
  permalink : string option;
  timestamp : string option;
  media_product_type : string option;
      (** Where the media surfaces: [FEED], [REELS], [STORY] or [AD]
          ({{:https://developers.facebook.com/docs/instagram-platform/instagram-graph-api/reference/ig-media}IG
           media reference}). *)
  username : string option;
  raw : Yojson.Basic.t;
}
(** Selected common fields; unrequested fields are [None]. [raw] preserves the
    complete item, including unknown fields and nested expansions. *)

val of_json : Yojson.Basic.t -> (post, string) result
(** Requires a nonempty string [id]; other fields are optional. *)

val list_fields : string
(** The [fields] parameter the user media listing requests, as a
    comma-joined string: every field of [post] except [username]. The
    listing operations use it as their default selection. *)
