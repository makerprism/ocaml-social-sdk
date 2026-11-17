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

let field key = function
  | `Assoc fields -> List.assoc_opt key fields
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> None

let string_field key json =
  match field key json with
  | Some (`String s) -> Some s
  | Some (`Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _) | None -> None

(* Meta sends counts as JSON integers; a float that is a whole number is
   accepted too because some JSON encoders emit [12.0]. *)
let int_value ~what = function
  | Some (`Int n) -> Ok n
  | Some (`Float f) when Float.is_integer f -> Ok (int_of_float f)
  | Some (`Float _ | `Null | `Bool _ | `String _ | `List _ | `Assoc _) | None ->
      Error (what ^ " is not an integer")

let map_result f items =
  List.fold_right
    (fun item acc ->
      match (acc, f item) with
      | Ok values, Ok value -> Ok (value :: values)
      | (Error _ as error), _ -> error
      | Ok _, (Error _ as error) -> error)
    items (Ok [])

let parse_point json =
  int_value ~what:"values[].value" (field "value" json)
  |> Result.map (fun value -> { value; end_time = string_field "end_time" json })

let parse_link_total json =
  match string_field "link_url" json with
  | None -> Error "link_total_values[] has no link_url"
  | Some link_url ->
      int_value ~what:"link_total_values[].value" (field "value" json)
      |> Result.map (fun link_clicks -> { link_url; link_clicks })

(* The three documented shapes, tried in the order the docs present them
   for the metric kinds. *)
let parse_value json =
  match (field "link_total_values" json, field "total_value" json, field "values" json) with
  | Some (`List items), _, _ -> map_result parse_link_total items |> Result.map (fun l -> Link_totals l)
  | _, Some (`Assoc _ as total), _ ->
      int_value ~what:"total_value.value" (field "value" total) |> Result.map (fun n -> Total n)
  | _, _, Some (`List items) -> map_result parse_point items |> Result.map (fun l -> Time_series l)
  | _, _, _ -> Error "entry has no values, total_value or link_total_values"

let parse_entry json =
  match Option.bind (string_field "name" json) metric_of_string with
  | None -> Ok None
  | Some metric ->
      parse_value json
      |> Result.map (fun value -> Some { metric; period = string_field "period" json; value })
      |> Result.map_error (fun msg -> metric_to_string metric ^ ": " ^ msg)

let parse_insights body =
  match Yojson.Basic.from_string body with
  | exception Yojson.Json_error msg -> Error ("insights response is not JSON: " ^ msg)
  | json -> (
      match field "data" json with
      | Some (`List entries) -> map_result parse_entry entries |> Result.map (List.filter_map Fun.id)
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
