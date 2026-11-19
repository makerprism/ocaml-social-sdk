(** Facebook Page, post and video insights: metric sets valid on Graph
    v25 and the typed shape of an insights response, exposed as
    [Social_facebook_graph_v25.Insights].

    The requests live in [Make] ([get_page_insights], [get_post_insights],
    [get_video_insights] and their [_for_account] variants); this module is
    pure.

    Platform facts the defaults follow (checked 2026-11-17):
    - {{:https://developers.facebook.com/docs/platforminsights/page/deprecated-metrics}
      Deprecated Page metrics}: [page_impressions*] (alternative
      [page_media_view]), [page_impressions_unique] (alternative
      [page_total_media_view_unique]), [page_posts_impressions*],
      [page_fans] (alternative [page_follows]), [page_fan_adds*],
      [post_impressions] (alternative [post_media_view]),
      [post_impressions_unique] (alternative
      [post_total_media_view_unique]) and the other [post_impressions_*]
      metrics return an invalid-metric error on every API version.
      {{:https://developers.facebook.com/docs/graph-api/changelog/version25.0}
      The v25.0 changelog} lists the same removals "when v26.0 is
      released", and the
      {{:https://developers.facebook.com/docs/graph-api/reference/insights}
      metric reference} still shows some of them; the deprecation page is
      the more specific source.
    - The same v25.0 changelog removes [page_video_views_unique],
      [total_video_impressions], [total_video_impressions_unique],
      [total_video_views_unique] and the paid/organic unique video views.
    - {{:https://developers.facebook.com/docs/graph-api/reference/video/video_insights}
      Video insights} ([GET /{video-id}/video_insights]) take [metric],
      [period], [since], [until]; Reels metrics ([blue_reels_play_count],
      [fb_reels_total_plays], ...) are [lifetime].
    - Response: [{"data": [{name, period, values: [{value, end_time?}],
      title, description, id}], "paging": {...}}]. A value is a number or,
      for [*_by_type*] metrics, an object of counts per type. Lifetime
      values carry no [end_time].
    - Page insights need [read_insights] and [pages_read_engagement]; one
      [since]/[until] window spans at most 90 days. *)

(** [period] request parameter. *)
type period = Day | Week | Days_28 | Month | Lifetime | Total_over_range

val period_to_string : period -> string
val period_of_string : string -> period option

val page_metrics : string list
(** [page_media_view], [page_total_media_view_unique], [page_follows],
    [page_daily_follows_unique], [page_post_engagements],
    [page_video_views]: v25 metrics that are not on Meta's deprecation
    list and accept [period=day]. [page_follows] is the Page's follower
    total as of each day (Meta describes it as follows minus unfollows
    since creation), not a daily gain; [page_daily_follows_unique] is the
    daily gain. *)

val post_metrics : string list
(** [post_media_view], [post_total_media_view_unique],
    [post_reactions_by_type_total], [post_clicks], [post_clicks_by_type]:
    the v25 successors of [post_impressions] / [post_impressions_unique]
    plus the reaction and click breakdowns ([lifetime]).

    Verify live: in May 2026 this package observed that a Page feed field
    expansion ([insights.metric(...)] in [get_page_posts]) listing
    [post_total_media_view_unique] came back without any [insights], while
    [post_impressions_unique] worked, contrary to the deprecation notice.
    [get_page_posts] still requests the legacy name; this list follows the
    documentation. Pass [?metrics] to choose. *)

val reels_metrics : string list
(** [fb_reels_total_plays], [blue_reels_play_count],
    [fb_reels_replay_count], [post_video_avg_time_watched],
    [post_video_view_time]: Reels metrics from the video insights
    reference ([lifetime]). *)

val video_metrics : string list
(** [total_video_views], [total_video_complete_views],
    [total_video_avg_time_watched], [total_video_view_total_time]: regular
    video metrics from the video insights reference that the v25.0
    changelog does not remove ([lifetime]). *)

val deprecated_metrics : string list
(** Metrics Meta documents as deprecated or removed for every API version
    (see the module notes). Requesting one makes the whole call fail. *)

val is_deprecated_metric : string -> bool

(** One value: a count, or counts per type (e.g.
    [post_reactions_by_type_total] → [[("like", 3); ("love", 1)]]). *)
type value = Count of int | By_type of (string * int) list

type point = { value : value; end_time : string option }

type metric = {
  name : string;
  period : period option;  (** [None] when absent or not a documented period *)
  title : string option;
  description : string option;
  points : point list;
      (** [values] that carry a figure, in the order Meta sent them *)
}

val metrics_of_json : Yojson.Basic.t -> (metric list, string) result
(** Reads an insights [data] array, or an object holding one: the response
    root, or the [insights] object a field expansion such as
    [insights.metric(...)] embeds in a post. A point whose value is
    missing, [null], or neither a number nor an object of numbers carries
    no figure and is left out of [points], so one empty metric never costs
    the others. [Error] when the root holds no [data] array or an entry
    has no [name]. *)

val parse : string -> (metric list, string) result
(** {!metrics_of_json} on a response body. *)

val find : string -> metric list -> metric option
(** The metric with that [name]. *)

val count : value -> int
(** A count as is; counts per type summed. *)

val latest : metric -> value option
(** The value of the last point. *)

val latest_count : string -> metric list -> int option
(** {!count} of {!latest} for the named metric. *)

val by_type : string -> metric list -> (string * int) list option
(** The per-type counts of the named metric's last point, when it is
    [By_type]. *)

(** {1 Typed metrics, descriptors and projections}

    A response series is identified by its free-form [name]. The typed
    metrics below give the figures a Page owner reads from the three
    insights edges a fixed identity, so a consumer can ask for "the
    unique viewers of this post" without tracking Meta's renames; the
    descriptor carries the request name, the older names that still
    report the same figure, what the metric counts and how its values
    aggregate. Consumers that only pass names through keep the string
    values above. *)

type typed_metric =
  | Page_follows
  | Page_daily_follows_unique
  | Page_media_view
  | Page_unique_viewers
  | Post_media_view
  | Post_unique_viewers
  | Post_organic_unique_viewers
  | Post_reactions_by_type_total
  | Post_clicks
  | Post_clicks_by_type
  | Reel_plays
(** The metrics this module has descriptors for. Constructors name the
    figure, not the request string: [Post_unique_viewers] is
    [post_total_media_view_unique] on Graph v25 and
    [post_impressions_unique] before the rename. *)

val all_typed_metrics : typed_metric list
(** Every typed metric, in the order [typed_metric_of_name] and
    {!series} prefer. *)

(** What a metric counts. Two metrics whose names suggest the same
    thing can count different units: [post_impressions_unique] sounds
    like views but counts distinct viewers, and
    [fb_reels_total_plays] counts play sessions including replays
    while [blue_reels_play_count] excludes them (see the video
    insights reference). *)
type unit_ =
  | Views
  | Unique_viewers
  | Followers
  | Clicks
  | Reactions

(** How the values of a metric aggregate over time. *)
type aggregation =
  | Running_total  (** each point is the total as of its [end_time] *)
  | Per_period  (** each point covers its period only (day, week, ...) *)
  | Lifetime_value  (** one value for the object's whole life; no [end_time] *)

(** The insights edges that accept a metric. *)
type edge =
  | Page_edge  (** [GET /{page-id}/insights] *)
  | Post_edge  (** [GET /{post-id}/insights], and post field expansions *)
  | Video_edge  (** [GET /{video-id}/video_insights] *)

type descriptor = {
  request_name : string;
      (** The [metric] parameter value. For renamed metrics this is the
          current name; the deprecated name is in [aliases]. *)
  aliases : string list;
      (** Older names reporting the same figure, best first: a series
          of that name answers for the metric. Every alias of a
          descriptor is on [deprecated_metrics] unless noted. *)
  unit_ : unit_;
  aggregation : aggregation;
  edges : edge list;
}

val descriptor : typed_metric -> descriptor

val typed_metric_of_name : string -> typed_metric option
(** Resolves a response name (request name or alias) to its typed
    metric. Two descriptors never share a request name or alias. *)

val request_names : typed_metric list -> string list
(** The [metric] request parameter for typed metrics, for the request
    functions above, which take names. *)

val series : typed_metric -> metric list -> metric option
(** The response series reporting the typed metric: the first series
    whose name is the metric's request name or one of its aliases, in
    descriptor order regardless of the order Meta listed the series
    in. A rename therefore cannot change which figure wins; a present
    series with no figure still answers [Some] (see
    {!metrics_of_json} on empty values). *)

val reel_plays_metrics : string list
(** The request names of the Reel plays family,
    [fb_reels_total_plays] then its aliases: the alias set and its
    preference order, for the [metric] parameter of
    [get_video_insights]. [fb_reels_total_views] is not in Meta's
    video insights reference but has been observed in responses;
    [blue_reels_play_count] is documented to exclude replays while
    [fb_reels_total_plays] includes them, so the alias chain prefers
    the documented total. *)

val reel_plays : metric list -> (string * int) option
(** Reel plays of one video: the count of the first point of the first
    family member that carries one (Reels metrics are
    [period=lifetime], so a series carries one point; a family member
    present without a figure does not answer). Returns the provider
    metric that answered with the count. *)

val follower_totals_by_day : metric list -> (string * int) list
(** [(day, follower_total)] from the [page_follows] series:
    [page_follows] reports the follower {i total} as of each day (a
    running total, not a daily gain; the daily gain is
    [page_daily_follows_unique]), so every point is an absolute
    count. The day is the date part ([YYYY-MM-DD]) of [end_time];
    points without a usable [end_time] and [By_type] values are left
    out. [] when no series of the family answered. *)

val to_canonical_series :
  scope:Analytics_types.scope -> metric list -> Analytics_types.series list
(** Canonical series for the metrics [Analytics_normalization.facebook_metrics]
    knows; others are left out. One series per canonical metric: when two
    metrics map to the same one (e.g. [post_media_view] and
    [post_total_media_view_unique] to [Views]), the first in the list wins.
    [By_type] values become their sum. *)
