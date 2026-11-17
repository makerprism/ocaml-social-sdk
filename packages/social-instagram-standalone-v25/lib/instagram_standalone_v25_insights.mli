(** Instagram insights (Instagram API with Instagram Login, v25): metric
    sets per media product, account totals, the daily [follower_count]
    insight and the absolute profile counts, exposed as
    [Social_instagram_standalone_v25.Insights].

    The requests live in [Make] / [Make_nonblocking]
    ([get_media_metrics], [get_account_totals],
    [get_daily_follower_changes], [get_account_counts] and their
    [_for_account] variants); this module is pure.

    {b Two follower numbers that must not be confused.}
    - {!account_counts.followers_count} is the profile field
      [followers_count]: "Total number of Instagram users following the
      user". An absolute count.
      {{:https://developers.facebook.com/docs/instagram-platform/instagram-graph-api/reference/ig-user}}
    - {!follower_change} is the [follower_count] {e insight} with
      [period=day]: one value per day describing that day's change (Meta's
      earlier reference called it the number of new followers per day). It
      is never the follower total. Summing changes backwards from the
      absolute count is the caller's business.

    Platform facts the types follow (checked 2026-11-17):
    - {{:https://developers.facebook.com/docs/instagram-platform/reference/instagram-media/insights}
      IG Media insights} (lifetime values, [values: [{value}]] and/or
      [total_value]): FEED supports [comments], [follows], [likes],
      [profile_activity], [profile_visits], [reach], [reposts], [saved],
      [shares], [total_interactions], [views]; REELS [comments],
      [ig_reels_avg_watch_time], [ig_reels_video_view_total_time], [likes],
      [reach], [reposts], [saved], [shares], [total_interactions], [views];
      STORY [follows], [navigation], [profile_activity], [profile_visits],
      [reach], [replies], [reposts], [shares], [total_interactions],
      [views]. [facebook_views] covers all three. Asking for a metric the
      product does not support fails the whole call. Story metrics exist for
      24 hours only; media inside a carousel have none.
    - [impressions], [plays], [video_views], [clips_replays_count],
      [ig_reels_aggregated_all_plays_count] and the [carousel_album_*]
      metrics are deprecated
      ({{:https://developers.facebook.com/docs/instagram-platform/changelog}
      changelog}).
    - {{:https://developers.facebook.com/docs/instagram-platform/api-reference/instagram-user/insights}
      IG User insights}: [views], [reach], [accounts_engaged],
      [total_interactions], [likes], [comments], [shares], [saves],
      [replies], [reposts], [profile_links_taps], [follows_and_unfollows]
      take [period=day] with [metric_type=total_value] ([reach] also allows
      [time_series]). [views] is documented with [total_value] only.
      [follower_count] is no longer in the metric table; the reference
      still says it is unavailable below 100 followers. *)

(** [media_product_type] of an IG Media. *)
type media_product = Feed | Reels | Story

val media_product_of_string : string -> media_product option
(** ["FEED"], ["REELS"], ["STORY"] (any case). [None] for ["AD"] and
    anything else. *)

val media_product_to_string : media_product -> string

(** {1 Media insights} *)

type media_metric =
  | Views
  | Reach
  | Likes
  | Comments
  | Shares
  | Saved
  | Total_interactions
  | Follows
  | Profile_visits
  | Profile_activity
  | Replies
  | Navigation
  | Reposts
  | Ig_reels_avg_watch_time
  | Ig_reels_video_view_total_time
  | Facebook_views

val media_metric_to_string : media_metric -> string
val media_metric_of_string : string -> media_metric option

val supported_media_metrics : media_product -> media_metric list
(** Every metric the reference lists for the product. *)

val default_media_metrics : media_product -> media_metric list
(** What [get_media_metrics] asks for when given no list: the supported
    metrics without breakdown-only or "in development" extras.
    - [Feed]: [views], [reach], [likes], [comments], [shares], [saved],
      [total_interactions], [follows], [profile_visits]
    - [Reels]: [views], [reach], [likes], [comments], [shares], [saved],
      [total_interactions], [ig_reels_avg_watch_time]
    - [Story]: [views], [reach], [shares], [total_interactions], [follows],
      [profile_visits], [replies] *)

val unsupported_media_metrics : media_product -> media_metric list -> media_metric list
(** The metrics in the list that the product does not support. *)

type media_insight = { metric : media_metric; value : int }

val parse_media_insights : string -> (media_insight list, string) result
(** Reads [values[0].value], else [total_value.value]. Unknown metric names
    are skipped, and so is an entry with neither value (no figure yet), so
    one empty metric never costs the others. [Error] only when the body is
    not JSON or has no [data] array. *)

(** {1 Account totals} *)

type account_metric =
  | Account_views
  | Account_reach
  | Accounts_engaged
  | Account_total_interactions
  | Account_likes
  | Account_comments
  | Account_shares
  | Account_saves
  | Account_replies
  | Account_reposts
  | Profile_links_taps
  | Follows_and_unfollows

val account_metric_to_string : account_metric -> string
(** The API name, e.g. [Account_views] is ["views"]. *)

val account_metric_of_string : string -> account_metric option

val default_account_metrics : account_metric list
(** [views], [reach], [accounts_engaged], [total_interactions], [likes],
    [comments], [shares], [saves], [replies]. [follows_and_unfollows] (not
    returned below 100 followers), [profile_links_taps] and [reposts] are
    available on request. *)

type account_total = { metric : account_metric; total : int }

val parse_account_totals : string -> (account_total list, string) result
(** Reads [total_value.value] (the [metric_type=total_value] shape). A
    metric Meta has no data for, or whose [total_value] has no integer, is
    left out (Meta answers with an empty set rather than [0]). *)

(** {1 Followers} *)

(** One day of the [follower_count] insight. [end_time] is the end of the
    day, ISO 8601. [change] is the day's value as Meta reports it, not a
    follower total. *)
type follower_change = { end_time : string option; change : int }

val parse_follower_changes : string -> (follower_change list, string) result
(** The [follower_count] entry's [values], in the order Meta sent them;
    [Ok []] when the response has none (e.g. under 100 followers). A day
    without an integer value is left out. *)

(** Absolute counts from the profile ([GET /{id}?fields=...]). [None] when
    Meta does not return the field. *)
type account_counts = {
  followers_count : int option;
  follows_count : int option;
  media_count : int option;
}

val account_count_fields : string list
(** [id], [followers_count], [follows_count], [media_count]. *)

val parse_account_counts : string -> (account_counts, string) result
