(** Typed [app.bsky] views (post, thread, feed item, notification) and the
    pure helpers that go with them.

    Parsers take the JSON an AppView sent and return [Error msg] when a
    property the lexicon marks as required is missing or has the wrong type.
    The HTTP calls that use them are {!Social_bluesky_v1.Reads} (token based
    or public, no account store) and {!Social_bluesky_v1.Make_with_options}
    (account bound).

    Unions are open in atproto: a server may send a [$type] this library does
    not know yet. Every union here keeps such a member as an [Unknown_*] /
    [Other_*] case instead of failing the whole response.

    Sources (lexicons from bluesky-social/atproto [main], checked 2026-11-17):
    - [app.bsky.feed.defs] (postView, threadViewPost, notFoundPost,
      blockedPost, feedViewPost, replyRef, reasonRepost, reasonPin):
      {{:https://github.com/bluesky-social/atproto/blob/main/lexicons/app/bsky/feed/defs.json}}
    - [app.bsky.feed.post] (record [text], [createdAt], [reply]):
      {{:https://github.com/bluesky-social/atproto/blob/main/lexicons/app/bsky/feed/post.json}}
    - [app.bsky.notification.listNotifications]:
      {{:https://docs.bsky.app/docs/api/app-bsky-notification-list-notifications}}
    - [app.bsky.feed.getAuthorFeed]:
      {{:https://docs.bsky.app/docs/api/app-bsky-feed-get-author-feed}}
    - Lexicon unions are open: {{:https://atproto.com/specs/lexicon#union}} *)

(** {1 References} *)

(** [com.atproto.repo.strongRef]: a record's AT URI and the CID of the exact
    version. Re-exported as {!Social_bluesky_v1.strong_ref}. *)
type strong_ref = { uri : string; cid : string }

(** [app.bsky.feed.post#replyRef]: [parent] is the post replied to, [root]
    the first post of the thread. Re-exported as
    {!Social_bluesky_v1.reply_ref}. *)
type reply_ref = { root : strong_ref; parent : strong_ref }

(** {1 Posts}

    Several records below share field names ([uri], [cid], [author],
    [indexed_at], [cursor]); annotate the type where OCaml cannot infer it. *)

(** [app.bsky.actor.defs#profileViewBasic] (also read from the larger
    [#profileView]). *)
type profile_basic = {
  did : string;
  handle : string;
  display_name : string option;
  avatar : string option;
}

(** [app.bsky.feed.defs#postView]. *)
type post_view = {
  uri : string;
  cid : string;
  author : profile_basic;
  record : Yojson.Basic.t;  (** The raw [app.bsky.feed.post] record *)
  text : string;  (** [record.text] *)
  created_at : string;  (** [record.createdAt], as claimed by the author *)
  reply : reply_ref option;  (** [record.reply], when the post is a reply *)
  indexed_at : string;  (** When the AppView saw it *)
  reply_count : int option;
  repost_count : int option;
  like_count : int option;
  quote_count : int option;
}

val post_view_of_json : Yojson.Basic.t -> (post_view, string) result

(** [app.bsky.feed.defs#blockedPost]. *)
type blocked_post = { uri : string; author_did : string }

(** One member of the [#postView | #notFoundPost | #blockedPost] union (with
    [#threadViewPost] in place of [#postView] inside threads). *)
type 'post post_union =
  | Post of 'post
  | Not_found_post of string  (** The URI of a deleted or unknown post *)
  | Blocked_post of blocked_post
  | Unknown_post of string  (** A [$type] this library does not know *)

(** {1 Profiles} *)

(** [app.bsky.actor.defs#profileViewDetailed], the answer of
    [app.bsky.actor.getProfile]. Only [did] and [handle] are required by
    the lexicon; counts are [None] when the AppView leaves them out.
    {{:https://github.com/bluesky-social/atproto/blob/main/packages/api/lexicons/app/bsky/actor/defs.json}}
    {{:https://docs.bsky.app/docs/api/app-bsky-actor-get-profile}} *)
type profile_detailed = {
  did : string;
  handle : string;
  display_name : string option;
  description : string option;
  avatar : string option;
  banner : string option;
  followers_count : int option;  (** [followersCount] *)
  follows_count : int option;  (** [followsCount] *)
  posts_count : int option;  (** [postsCount] *)
  created_at : string option;  (** [createdAt] *)
  indexed_at : string option;  (** [indexedAt] *)
}

val profile_detailed_of_json : Yojson.Basic.t -> (profile_detailed, string) result

(** {1 Threads} *)

(** [app.bsky.feed.getPostThread] output [thread]. *)
type thread = thread_view_post post_union

(** [app.bsky.feed.defs#threadViewPost]. *)
and thread_view_post = {
  post : post_view;
  parent : thread option;
      (** Up to [parentHeight] ancestors, nested through [parent]. *)
  replies : thread list;
      (** Direct replies, each with its own replies up to [depth]. Empty
          both when there are none and when [depth] cut the tree off here;
          compare with [post.reply_count]. *)
}

val thread_of_json : Yojson.Basic.t -> (thread, string) result
(** Reads a thread union member (the [thread] property, not the whole
    response). Recursion is bounded by the server's [depth] /
    [parentHeight] limits (at most 1000). *)

val post_thread_of_json : Yojson.Basic.t -> (thread, string) result
(** A whole [getPostThread] response: its required [thread] property. *)

(** {1 Feeds} *)

(** [app.bsky.feed.defs#replyRef] (the hydrated one in a feed item, not the
    record's strong refs). *)
type feed_reply = {
  root : post_view post_union;
  parent : post_view post_union;
  grandparent_author : profile_basic option;
}

(** [feedViewPost.reason]. *)
type feed_reason =
  | Reason_repost of { by : profile_basic; indexed_at : string }
  | Reason_pin  (** The author's pinned post ([includePins]) *)
  | Other_feed_reason of string  (** Unknown [$type] *)

(** [app.bsky.feed.defs#feedViewPost]. *)
type feed_view_post = {
  post : post_view;
  reply : feed_reply option;
  reason : feed_reason option;
      (** [Some (Reason_repost _)] when the item is a repost by the feed's
          author, not their own post. *)
}

val feed_view_post_of_json : Yojson.Basic.t -> (feed_view_post, string) result

type author_feed_page = { feed : feed_view_post list; cursor : string option }

val author_feed_page_of_json : Yojson.Basic.t -> (author_feed_page, string) result

(** [getAuthorFeed] [filter] known values. *)
type author_feed_filter =
  | Posts_with_replies  (** Server default *)
  | Posts_no_replies
  | Posts_with_media
  | Posts_and_author_threads
  | Posts_with_video

val author_feed_filter_to_string : author_feed_filter -> string

val posts_of_json : Yojson.Basic.t -> (post_view list, string) result
(** A [getPosts] response ([{"posts": [...]}]). *)

(** {1 Notifications} *)

(** [listNotifications#notification.reason] known values. *)
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

val notification_reason_to_string : notification_reason -> string
(** The wire value, e.g. ["starterpack-joined"]. *)

val notification_reason_of_string : string -> notification_reason

type notification = {
  uri : string;  (** The record that caused it (the reply, the like, ...) *)
  cid : string;
  author : profile_basic;
  reason : notification_reason;
  reason_subject : string option;
      (** The subject's AT URI, e.g. the liked post. For [Reply] the parent
          is in the record's [reply]; see {!post_record_reply}. *)
  record : Yojson.Basic.t;
  is_read : bool;
  indexed_at : string;
}

val notification_of_json : Yojson.Basic.t -> (notification, string) result

type notifications_page = {
  notifications : notification list;
  cursor : string option;
  seen_at : string option;
}

val notifications_page_of_json :
  Yojson.Basic.t -> (notifications_page, string) result

(** {1 Identity} *)

val did_of_resolve_handle_json : Yojson.Basic.t -> (string, string) result
(** The non-empty [did] of a [com.atproto.identity.resolveHandle] response. *)

(** {1 Pure helpers} *)

val post_record_text : Yojson.Basic.t -> string option
(** [text] of an [app.bsky.feed.post] record (e.g. a notification record). *)

val post_record_reply : Yojson.Basic.t -> reply_ref option
(** [reply] of an [app.bsky.feed.post] record, when both strong refs are
    complete. *)

val rkey_of_uri : string -> string option
(** The record key of [at://<authority>/<collection>/<rkey>]; [None] for
    anything else. *)

val post_web_url : handle:string -> uri:string -> string option
(** [https://bsky.app/profile/<handle>/post/<rkey>] for a post AT URI. A DID
    works in place of the handle. [None] when [uri] has no record key. *)
