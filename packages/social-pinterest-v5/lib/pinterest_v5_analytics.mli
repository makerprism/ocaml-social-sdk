(** Pinterest analytics reports: the typed shape of
    [GET /v5/user_account/analytics] and [GET /v5/pins/{pin_id}/analytics],
    exposed as [Social_pinterest_v5.Analytics].

    The requests live in [OAuth.Make] ([get_user_account_analytics],
    [get_pin_analytics], token based) and [Make]
    ([get_account_analytics_report], [get_pin_analytics_report]); this
    module is pure.

    Platform facts (Pinterest API v5 OpenAPI description, version 5.28.0,
    {{:https://github.com/pinterest/api-description/blob/main/v5/openapi.yaml}},
    checked 2026-11-17; the developers.pinterest.com pages render the same
    spec):
    - The response is an object keyed by split group ([AnalyticsResponse] /
      [PinAnalyticsResponse]: [additionalProperties]). Without
      [split_field] there is one group; Pinterest's examples name it
      ["all"].
    - Each group has [daily_metrics] ([{date, data_status, metrics}]) and
      [summary_metrics] (metric name to number over the range); pin reports
      add [lifetime_metrics] (metric name to integer).
    - [start_date] / [end_date] are [YYYY-MM-DD]; the start can be at most
      90 days back and the range at most 90 days.
    - User account [metric_types]: [ENGAGEMENT], [ENGAGEMENT_RATE],
      [IMPRESSION], [OUTBOUND_CLICK], [OUTBOUND_CLICK_RATE], [PIN_CLICK],
      [PIN_CLICK_RATE], [SAVE], [SAVE_RATE]; omitted means all. Pin
      [metric_types] is required: standard [IMPRESSION], [OUTBOUND_CLICK],
      [PIN_CLICK], [SAVE], [SAVE_RATE], [TOTAL_COMMENTS],
      [TOTAL_REACTIONS], [USER_FOLLOW], [PROFILE_VISIT], or video
      [VIDEO_MRC_VIEW], [VIDEO_AVG_WATCH_TIME], [VIDEO_V50_WATCH_TIME],
      [QUARTILE_95_PERCENT_VIEW], [VIDEO_10S_VIEW], [VIDEO_START].
    - Needs [user_accounts:read] (account) or [pins:read] and
      [boards:read] (pin). *)

val account_metric_types : string list
(** Every documented user account metric type. *)

val pin_metric_types : string list
(** [IMPRESSION], [OUTBOUND_CLICK], [PIN_CLICK], [SAVE], [SAVE_RATE],
    [TOTAL_COMMENTS], [TOTAL_REACTIONS], [USER_FOLLOW], [PROFILE_VISIT]:
    the standard pin metric types. *)

(** One day. [data_status] is e.g. ["READY"], ["PROCESSING"],
    ["ESTIMATE"] or a [BEFORE_*] reason the day has no data. *)
type daily = {
  date : string;
  data_status : string option;
  metrics : (string * float) list;
}

type group = {
  daily_metrics : daily list;
  summary_metrics : (string * float) list;
  lifetime_metrics : (string * float) list;  (** Pin reports only *)
}

(** Groups by split value, in the order Pinterest sent them. *)
type report = (string * group) list

val parse_report : string -> (report, string) result
(** [Error] when the body is not a JSON object, a group is not an object, a
    day has no [date], or a metric value is not a number. *)

val unsplit : report -> group option
(** The group of a report requested without [split_field]: ["all"], or the
    only group when there is exactly one. *)

val summary : string -> group -> float option
(** A summary metric by name (e.g. ["IMPRESSION"]). *)

val summary_count : string -> group -> int option
(** {!summary} rounded down to an integer, for count metrics. *)
