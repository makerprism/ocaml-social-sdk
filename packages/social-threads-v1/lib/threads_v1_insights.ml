type metric =
  | Views
  | Likes
  | Replies
  | Reposts
  | Quotes
  | Shares
  | Clicks
  | Followers_count

let metric_to_string = function
  | Views -> "views"
  | Likes -> "likes"
  | Replies -> "replies"
  | Reposts -> "reposts"
  | Quotes -> "quotes"
  | Shares -> "shares"
  | Clicks -> "clicks"
  | Followers_count -> "followers_count"

let all_metrics = [ Views; Likes; Replies; Reposts; Quotes; Shares; Clicks; Followers_count ]

let metric_of_string raw =
  let name = String.lowercase_ascii (String.trim raw) in
  List.find_opt (fun metric -> String.equal (metric_to_string metric) name) all_metrics

let user_metrics = [ Views; Likes; Replies; Reposts; Quotes; Clicks; Followers_count ]
let media_metrics = [ Views; Likes; Replies; Reposts; Quotes; Shares ]

type point = { value : int; end_time : string option }
type link_total = { link_url : string; link_clicks : int }

type value =
  | Time_series of point list
  | Total of int
  | Link_totals of link_total list

type insight = { metric : metric; period : string option; value : value }

module J = Social_json_reader

(* A bucket or link total without an integer value carries no figure:
   Threads leaves the day still being computed empty, and one such entry
   must not cost the caller every other metric of the response. *)
let parse_point json =
  match J.int_value ~what:"values[].value" (J.field "value" json) with
  | Ok value -> Some { value; end_time = J.string_opt (J.member "end_time" json) }
  | Error _ -> None

let parse_link_total json =
  match (J.string_opt (J.member "link_url" json), J.int_value ~what:"value" (J.field "value" json)) with
  | Some link_url, Ok link_clicks -> Some { link_url; link_clicks }
  | None, (Ok _ | Error _) | Some _, Error _ -> None

(* The three documented shapes. A non-empty [values] array is read first:
   an entry that also carries [total_value] keeps its per-day buckets.
   [None] when the entry carries none of them, or a [total_value] without
   an integer. *)
let parse_value json =
  match (J.field "values" json, J.field "link_total_values" json, J.field "total_value" json) with
  | Some (`List (_ :: _ as items)), _, _ -> Some (Time_series (List.filter_map parse_point items))
  | _, Some (`List items), _ -> Some (Link_totals (List.filter_map parse_link_total items))
  | _, _, Some (`Assoc _ as total) -> (
      match J.int_value ~what:"total_value.value" (J.field "value" total) with
      | Ok n -> Some (Total n)
      | Error _ -> None)
  | Some (`List []), _, _ -> Some (Time_series [])
  | _, _, _ -> None

let parse_entry json =
  match Option.bind (J.string_opt (J.member "name" json)) metric_of_string with
  | None -> None
  | Some metric ->
      Option.map
        (fun value -> { metric; period = J.string_opt (J.member "period" json); value })
        (parse_value json)

let parse_insights body =
  match Yojson.Basic.from_string body with
  | exception Yojson.Json_error msg -> Error ("insights response is not JSON: " ^ msg)
  | json -> (
      match J.field "data" json with
      | Some (`List entries) -> Ok (List.filter_map parse_entry entries)
      | Some (`Null | `Bool _ | `Int _ | `Float _ | `String _ | `Assoc _) | None ->
          Error "insights response has no data array")

let find metric insights = List.find_opt (fun insight -> insight.metric = metric) insights

let sum_links links = List.fold_left (fun acc link -> acc + link.link_clicks) 0 links

let latest_value ?(skip_zero = false) = function
  | Total n -> Some n
  | Link_totals links -> Some (sum_links links)
  | Time_series points ->
      List.rev points
      |> List.find_map (fun (point : point) ->
             if skip_zero && point.value = 0 then None else Some point.value)

let sum = function
  | Total n -> n
  | Link_totals links -> sum_links links
  | Time_series points -> List.fold_left (fun acc (point : point) -> acc + point.value) 0 points

let canonical_metric ~scope metric =
  match (scope, metric) with
  | Analytics_types.Account, Views -> Analytics_types.Profile_visits
  | Analytics_types.(Page | Profile | Post | Media | Video | Pin | Channel), Views ->
      Analytics_types.Views
  | _, Likes -> Analytics_types.Likes
  | _, Replies -> Analytics_types.Replies
  | _, Reposts -> Analytics_types.Reposts
  | _, Quotes -> Analytics_types.Quotes
  | _, Shares -> Analytics_types.Shares
  | _, Clicks -> Analytics_types.Outbound_clicks
  | _, Followers_count -> Analytics_types.Followers

let canonical_points = function
  | Time_series points ->
      List.map
        (fun (point : point) -> Analytics_types.make_datapoint ?timestamp:point.end_time point.value)
        points
  | Total n -> [ Analytics_types.make_datapoint n ]
  | Link_totals links -> [ Analytics_types.make_datapoint (sum_links links) ]

let granularity_of_period = function
  | Some "day" -> Some Analytics_types.Day
  | Some "lifetime" -> Some Analytics_types.Lifetime
  | Some _ | None -> None

let to_canonical_series ~scope insights =
  List.map
    (fun insight ->
      let time_range =
        Option.map
          (fun granularity ->
            { Analytics_types.since = None; until_ = None; granularity = Some granularity })
          (granularity_of_period insight.period)
      in
      Analytics_types.make_series ?time_range
        ~metric:(canonical_metric ~scope insight.metric)
        ~scope
        ~provider_metric:(metric_to_string insight.metric)
        (canonical_points insight.value))
    insights
