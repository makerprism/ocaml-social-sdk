(** Threads Insights API: typed metrics and response values for user
    ([GET /{threads-user-id}/threads_insights]) and media
    ([GET /{threads-media-id}/insights]) insights, exposed as
    [Social_threads_v1.Insights].

    The requests live in [Make] ([get_user_insights],
    [get_user_insights_for_account], [get_media_insights],
    [get_media_insights_for_account]); this module is pure.

    Platform facts the types follow, from
    {{:https://developers.facebook.com/docs/threads/insights} Threads
    Insights} (checked 2026-11-17):
    - Media metrics: [views], [likes], [replies], [reposts], [quotes],
      [shares]. The response carries each in [values] (one [{value}], period
      [lifetime]).
    - User metrics: [views] (time series), [likes], [replies], [reposts],
      [quotes], [followers_count] (total value), [clicks] (link total
      values), [follower_demographics] (needs a [breakdown]; not modelled
      here). [shares] is a media metric only; asking for it at user level
      is not documented.
    - User [views] is "the number of times your profile was viewed", not
      post views.
    - A time-series metric answers with [values: [{value, end_time}]], a
      total-value metric with [total_value: {value}], [clicks] with
      [link_total_values: [{value, link_url}]]. The docs show [views] in
      both the time-series and the total-value example, so the parser
      reads whichever shape arrives.
    - [since] / [until] take a Unix timestamp or a date; without them the
      range is yesterday through today. Timestamps before [1712991600]
      (2024-04-13) are rejected. [followers_count] and
      [follower_demographics] ignore [since] / [until].
    - Needs [threads_basic] and [threads_manage_insights]. *)

(** One insights metric. User-level and media-level metrics overlap; see
    {!user_metrics} and {!media_metrics} for which level accepts which. *)
type metric =
  | Views
  | Likes
  | Replies
  | Reposts
  | Quotes
  | Shares  (** Media level only. *)
  | Clicks  (** User level only: clicks on URLs the user shared. *)
  | Followers_count  (** User level only: total followers. *)

val metric_to_string : metric -> string
(** The API name, e.g. [Followers_count] is ["followers_count"]. *)

val metric_of_string : string -> metric option
(** Inverse of {!metric_to_string}; case and surrounding blanks are ignored.
    [None] for names this module does not model (e.g.
    [follower_demographics]). *)

val user_metrics : metric list
(** Every documented user metric except [follower_demographics]:
    [views], [likes], [replies], [reposts], [quotes], [clicks],
    [followers_count]. The default of [get_user_insights]. *)

val media_metrics : metric list
(** Every documented media metric: [views], [likes], [replies], [reposts],
    [quotes], [shares]. The default of [get_media_insights]. *)

(** A time-series bucket. [end_time] is ISO 8601, e.g.
    ["2024-07-12T08:00:00+0000"]; media buckets carry none. *)
type point = { value : int; end_time : string option }

(** One URL's clicks inside [link_total_values]. *)
type link_total = { link_url : string; link_clicks : int }

(** A metric's value in whichever shape Meta sent it. *)
type value =
  | Time_series of point list  (** [values] *)
  | Total of int  (** [total_value.value] *)
  | Link_totals of link_total list  (** [link_total_values] *)

type insight = {
  metric : metric;
  period : string option;  (** e.g. ["day"], ["lifetime"] *)
  value : value;
}

val parse_insights : string -> (insight list, string) result
(** Parses an insights response body ([{"data": [...]}]). Entries naming a
    metric this module does not model are skipped. A bucket, link total or
    [total_value] without an integer value carries no figure and is left
    out (Threads leaves the day still being computed empty); an entry left
    with none of the three value shapes is skipped, so one empty metric
    never costs the others. An entry with a non-empty [values] array is read
    as a time series even when it also carries [total_value]; with an empty
    one, [total_value] (or [link_total_values]) is read instead. [Error]
    only when the body is not JSON or has no [data] array. *)

val find : metric -> insight list -> insight option

val latest_value : ?skip_zero:bool -> value -> int option
(** The current figure: the last bucket of a time series (with
    [~skip_zero:true], the last non-zero one: Threads can report the day
    still being computed as [0]), a total as is, link totals summed. [None]
    for an empty series. *)

val sum : value -> int
(** A time series summed over its buckets, a total as is, link totals
    summed. *)

val to_canonical_series :
  scope:Analytics_types.scope -> insight list -> Analytics_types.series list
(** Canonical series. [~scope:Account] reads user metrics: [views] becomes
    [Profile_visits] (Meta documents user [views] as profile views),
    [clicks] [Outbound_clicks], [followers_count] [Followers]. Any other
    scope reads media metrics: [views] becomes [Views]. A link-total value
    becomes one point with the sum. *)
