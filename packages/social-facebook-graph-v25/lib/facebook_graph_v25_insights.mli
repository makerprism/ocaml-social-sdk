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
  points : point list;  (** [values], in the order Meta sent them *)
}

val metrics_of_json : Yojson.Basic.t -> (metric list, string) result
(** Reads an insights [data] array, or an object holding one: the response
    root, or the [insights] object a field expansion such as
    [insights.metric(...)] embeds in a post. A value that is neither a
    number nor an object of numbers is [Error]. *)

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

val to_canonical_series :
  scope:Analytics_types.scope -> metric list -> Analytics_types.series list
(** Canonical series for the metrics [Analytics_normalization.facebook_metrics]
    knows; others are left out. One series per canonical metric: when two
    metrics map to the same one (e.g. [post_media_view] and
    [post_total_media_view_unique] to [Views]), the first in the list wins.
    [By_type] values become their sum. *)
