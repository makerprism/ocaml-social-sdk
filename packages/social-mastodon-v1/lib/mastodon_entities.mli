(** Typed Mastodon REST entities and the pure helpers that go with them.

    Parsers take the JSON the instance sent and return [Error msg] when a
    field the documentation marks as required is missing or has the wrong
    type. The HTTP calls that use them live in {!Social_mastodon_v1.Reads} (token
    based, no account store) and {!Social_mastodon_v1.Make} (account bound).

    Sources (checked 2026-11-17):
    - Status: {{:https://docs.joinmastodon.org/entities/Status/}}
    - Context: {{:https://docs.joinmastodon.org/entities/Context/}}
    - Notification: {{:https://docs.joinmastodon.org/entities/Notification/}}
    - Pagination (Link header):
      {{:https://docs.joinmastodon.org/api/guidelines/#pagination}}
    - Async refreshes:
      {{:https://docs.joinmastodon.org/methods/async_refreshes/}}
    - WebFinger: {{:https://docs.joinmastodon.org/spec/webfinger/}} and
      {{:https://www.rfc-editor.org/rfc/rfc7033} RFC 7033} *)

(** {1 Shared types}

    [visibility], [account_summary] and [pagination_cursors] are re-exported
    unchanged by {!Social_mastodon_v1}, so existing code keeps compiling. *)

(** Visibility levels for statuses. *)
type visibility =
  | Public  (** Visible to everyone, shown in public timelines *)
  | Unlisted  (** Visible to everyone, but not in public timelines *)
  | Private  (** Visible to followers only *)
  | Direct  (** Visible to mentioned users only *)

val visibility_to_string : visibility -> string

val visibility_of_string : string -> visibility option
(** [None] for anything but the four documented values. *)

type account_summary = {
  id : string;
  acct : string;
      (** WebFinger address without the [acct:] prefix: [username] for a
          local account, [username@domain] for a remote one. *)
  username : string option;
  display_name : string option;  (** [None] when empty *)
  url : string option;  (** The profile page *)
}

val account_summary_of_json : Yojson.Basic.t -> account_summary
(** Lenient: a missing [id] or [acct] becomes [""]. Kept for the existing
    readers in {!Social_mastodon_v1.Make}; the typed status parsers below require
    both. *)

val account_summary_of_json_result :
  Yojson.Basic.t -> (account_summary, string) result
(** Strict: [id] and [acct] must be strings. *)

(** Cursors from the [Link] response header. Pass [next_max_id] as [max_id]
    for the next (older) page, [prev_min_id] as [min_id] for the previous
    (newer) one. *)
type pagination_cursors = {
  next_max_id : string option;
  prev_since_id : string option;
  prev_min_id : string option;
}

val pagination_cursors_of_headers :
  (string * string) list -> pagination_cursors
(** Reads the [Link] header (any case), e.g.
    [<https://host/api/v1/notifications?max_id=7>; rel="next",
     <https://host/api/v1/notifications?min_id=9>; rel="prev"]. All cursors
    are [None] when the header is absent, which is how Mastodon signals the
    last page. *)

(** {1 Account} *)

(** [Account] entity with its counts, as [GET /api/v1/accounts/:id] and
    [GET /api/v1/accounts/verify_credentials] return it.
    {{:https://docs.joinmastodon.org/entities/Account/}} *)
type account = {
  summary : account_summary;  (** [id], [acct], [username], [display_name], [url] *)
  followers_count : int;  (** "The reported followers of this profile." *)
  following_count : int;  (** "The reported follows of this profile." *)
  statuses_count : int;  (** "How many statuses are attached to this account." *)
  last_status_at : string option;  (** Date only (3.1.0+); [None] without statuses *)
  created_at : string;  (** Midnight of the creation day (3.4.0+) *)
  locked : bool;
  bot : bool;
  hide_collections : bool option;
      (** 4.3.0+: the user hides their follows and followers lists. The
          counts are still reported. *)
}

val account_of_json : Yojson.Basic.t -> (account, string) result
(** Requires [id], [acct], the three counts (documented as integers, never
    null), [created_at], [locked] and [bot]. *)

(** {1 Status}

    [Social_mastodon_v1.Make.reply_to_status_exact ~account_id ~in_reply_to_id
    ~text ?visibility ?idempotency_key callback] returns this typed status in
    an [Error_types.api_result]. It sends [text] unchanged to
    {{:https://docs.joinmastodon.org/methods/statuses/#create}POST /api/v1/statuses},
    with [in_reply_to_id], [visibility], and an [Idempotency-Key] header.
    Pass the target's local status ID on the posting instance. Supply a stable
    key for retries; omission generates a new UUID. The response provides
    [id], [uri], nullable [url], and [created_at] as documented by the
    {{:https://docs.joinmastodon.org/entities/Status/}Status entity reference}.
    The existing [reply_to_status] still adds the author's mention. *)

(** A visibility outside the four Mastodon values. Other servers that
    implement this API send more (Pleroma and Akkoma send ["local"] and
    ["list"]); keeping the value lets callers decide instead of dropping the
    status. *)
type status_visibility =
  | Visibility of visibility
  | Other_visibility of string

(** [Status::Mention]. *)
type mention = {
  id : string;  (** Account id *)
  username : string;
  acct : string;
  url : string;  (** Profile page *)
}

type status = {
  id : string;  (** Database id on the instance that answered *)
  uri : string;  (** ActivityPub id, used for federation *)
  url : string option;  (** HTML page *)
  created_at : string;  (** ISO 8601 *)
  edited_at : string option;
  account : account_summary;
  content : string;  (** HTML; see {!html_to_plain_text} *)
  spoiler_text : string;  (** Content warning, [""] when none *)
  sensitive : bool;
  visibility : status_visibility;
  language : string option;
  in_reply_to_id : string option;
  in_reply_to_account_id : string option;
      (** "Might be the ID of the account that authored the status being
          replied to. This sometimes skips over self-replies." Compare
          against [in_reply_to_id]'s author when it matters. *)
  reblog : status option;  (** The boosted status when this is a boost *)
  mentions : mention list;
  replies_count : int;
  reblogs_count : int;
  favourites_count : int;
}

val status_of_json : Yojson.Basic.t -> (status, string) result
(** Requires [id], [uri], [created_at], [account] (with [id] and [acct]),
    [content], [visibility], [sensitive], [spoiler_text], [mentions] and the
    three counts, which the Status entity documents as always present. *)

val statuses_of_json : Yojson.Basic.t -> (status list, string) result
(** A JSON array of statuses. One malformed status fails the whole list. *)

(** {1 Context} *)

(** The experimental [Mastodon-Async-Refresh] header (Mastodon 4.5+). Present
    when the instance started fetching missing replies in the background;
    asking again after [retry_seconds] may return more descendants. *)
type async_refresh = {
  refresh_id : string;
  retry_seconds : int;
  result_count : int option;
}

val async_refresh_of_headers : (string * string) list -> async_refresh option
(** Parses [id="<string>", retry=<int>, result_count=<int>]; [None] when the
    header is absent or has no [id] / [retry]. *)

(** [GET /api/v1/statuses/:id/context]. *)
type status_context = {
  ancestors : status list;  (** Parents, oldest first *)
  descendants : status list;
      (** Replies in tree order (depth first). Rebuild the tree with
          [in_reply_to_id]. *)
  async_refresh : async_refresh option;
}

val status_context_of_response :
  headers:(string * string) list ->
  Yojson.Basic.t ->
  (status_context, string) result

(** {1 Notifications} *)

(** The notification types documented for Mastodon 4.6 (API version 10).
    Servers add types over time; an unknown one is kept as
    [Other_notification_type]. *)
type notification_type =
  | Mention
  | Status  (** Someone you enabled notifications for has posted *)
  | Reblog
  | Follow
  | Follow_request
  | Favourite
  | Poll
  | Update  (** A status you boosted was edited *)
  | Admin_sign_up
  | Admin_report
  | Severed_relationships
  | Moderation_warning
  | Quote
  | Quoted_update
  | Added_to_collection
  | Collection_update
  | Other_notification_type of string

val notification_type_to_string : notification_type -> string
(** The wire value, e.g. ["admin.sign_up"]. *)

val notification_type_of_string : string -> notification_type

type notification = {
  id : string;
  notification_type : notification_type;
  group_key : string option;  (** Mastodon 4.3+ *)
  created_at : string;
  account : account_summary;  (** Who caused it *)
  status : status option;
      (** The status for [Mention], [Status], [Reblog], [Favourite], [Poll],
          [Update], [Quote] and [Quoted_update]. *)
}

val notification_of_json : Yojson.Basic.t -> (notification, string) result

type notification_page = {
  notifications : notification list;
  cursors : pagination_cursors;
}

val notification_page_of_response :
  headers:(string * string) list ->
  Yojson.Basic.t ->
  (notification_page, string) result

(** {1 Status pages} *)

type status_page = {
  statuses : status list;
  cursors : pagination_cursors;
}

val status_page_of_response :
  headers:(string * string) list ->
  Yojson.Basic.t ->
  (status_page, string) result

(** {1 WebFinger} *)

type webfinger_link = {
  rel : string;
  link_type : string option;  (** [type], e.g. ["application/activity+json"] *)
  href : string option;
  template : string option;
}

(** A JSON Resource Descriptor (RFC 7033 section 4.4). *)
type webfinger = {
  subject : string;  (** e.g. ["acct:Gargron@mastodon.social"] *)
  aliases : string list;
  links : webfinger_link list;
}

val webfinger_of_json : Yojson.Basic.t -> (webfinger, string) result
(** Requires [subject]; links without [rel] are dropped (RFC 7033 4.4.4.1
    makes [rel] mandatory). *)

val webfinger_actor_url : webfinger -> string option
(** The [href] of the [rel="self"] link whose type is
    [application/activity+json] or
    [application/ld+json; profile="https://www.w3.org/ns/activitystreams"]:
    the ActivityPub actor, which is what Mastodon itself requires. *)

val webfinger_profile_page : webfinger -> string option
(** The [href] of the [http://webfinger.net/rel/profile-page] link. *)

val split_acct : string -> (string * string) option
(** [split_acct "@alice@example.social"] is [Some ("alice", "example.social")].
    Accepts an optional leading [@] or [acct:] prefix. [None] unless there is
    exactly one non-empty username and one non-empty domain. *)

val webfinger_resource : username:string -> domain:string -> string
(** ["acct:username@domain"]. *)

(** {1 Pure helpers} *)

val html_to_plain_text : string -> string
(** Plain text of a status [content]. [<br>] becomes a newline, paragraphs
    are separated by a blank line, list items and other blocks end with a
    newline, tags are dropped (link text is kept, including the [invisible]
    spans Mastodon uses to shorten URLs visually, so links come out whole),
    named entities [&amp; &lt; &gt; &quot; &apos; &nbsp;] and numeric
    entities are decoded, and the result is trimmed. *)

val status_web_url : status -> string
(** [url] when present and not blank, else [uri]. *)

val status_id_of_url : string -> string option
(** The last path segment of a status URL, e.g. ["109"] for
    [https://mastodon.social/@alice/109] or
    [https://mastodon.social/users/alice/statuses/109]. Ignores a query, a
    fragment and a trailing slash. The id is only meaningful on the instance
    that issued the URL. [None] for anything that is not an http(s) URL with
    a path. *)

val normalize_instance_url : string -> string option
(** Trims, adds [https://] when no scheme is given and drops trailing
    slashes: [" mastodon.social/ "] becomes ["https://mastodon.social"].
    [None] when there is no host. *)

val federated_acct : username:string -> instance_url:string -> string
(** The full [user@host] address of an account on [instance_url]. A leading
    [@] is dropped; a username that already contains [@] is returned as is;
    the host is lower-cased. Returns the bare username when [instance_url]
    has no host. *)
