type period = Day | Week | Days_28 | Month | Lifetime | Total_over_range

let period_to_string = function
  | Day -> "day"
  | Week -> "week"
  | Days_28 -> "days_28"
  | Month -> "month"
  | Lifetime -> "lifetime"
  | Total_over_range -> "total_over_range"

let period_of_string raw =
  List.find_opt
    (fun period -> String.equal (period_to_string period) raw)
    [ Day; Week; Days_28; Month; Lifetime; Total_over_range ]

let page_metrics =
  [ "page_media_view"; "page_total_media_view_unique"; "page_follows";
    "page_daily_follows_unique"; "page_post_engagements"; "page_video_views" ]

let post_metrics =
  [ "post_media_view"; "post_total_media_view_unique"; "post_reactions_by_type_total";
    "post_clicks"; "post_clicks_by_type" ]

let reels_metrics =
  [ "fb_reels_total_plays"; "blue_reels_play_count"; "fb_reels_replay_count";
    "post_video_avg_time_watched"; "post_video_view_time" ]

let video_metrics =
  [ "total_video_views"; "total_video_complete_views"; "total_video_avg_time_watched";
    "total_video_view_total_time" ]

let deprecated_metrics =
  [ (* Page impressions and reach *)
    "page_impressions"; "page_impressions_unique"; "page_impressions_paid";
    "page_impressions_paid_unique"; "page_impressions_viral"; "page_impressions_viral_unique";
    "page_impressions_nonviral"; "page_impressions_nonviral_unique";
    (* Page posts impressions *)
    "page_posts_impressions"; "page_posts_impressions_unique"; "page_posts_impressions_paid";
    "page_posts_impressions_paid_unique"; "page_posts_impressions_organic_unique";
    "page_posts_served_impressions_organic_unique"; "page_posts_impressions_viral";
    "page_posts_impressions_viral_unique"; "page_posts_impressions_nonviral";
    "page_posts_impressions_nonviral_unique";
    (* Page fans *)
    "page_fans"; "page_fans_locale"; "page_fans_city"; "page_fans_country"; "page_fan_adds";
    "page_fan_adds_unique"; "page_fan_removes"; "page_fan_removes_unique";
    "page_engaged_users";
    (* Post impressions *)
    "post_impressions"; "post_impressions_unique"; "post_impressions_paid";
    "post_impressions_paid_unique"; "post_impressions_fan"; "post_impressions_fan_unique";
    "post_impressions_organic"; "post_impressions_organic_unique"; "post_impressions_viral";
    "post_impressions_viral_unique"; "post_impressions_nonviral";
    "post_impressions_nonviral_unique"; "post_activity"; "post_activity_unique";
    (* Video unique views and impressions (v25.0 changelog) *)
    "page_video_views_unique"; "total_video_impressions"; "total_video_impressions_unique";
    "total_video_views_unique"; "total_video_views_organic_unique";
    "total_video_views_paid_unique"; "post_video_views_unique";
    "post_video_views_organic_unique"; "post_video_views_paid_unique" ]

let is_deprecated_metric name =
  List.mem (String.lowercase_ascii (String.trim name)) deprecated_metrics

type value = Count of int | By_type of (string * int) list
type point = { value : value; end_time : string option }

type metric = {
  name : string;
  period : period option;
  title : string option;
  description : string option;
  points : point list;
}

let field key = function
  | `Assoc fields -> List.assoc_opt key fields
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> None

let string_field key json =
  match field key json with
  | Some (`String s) -> Some s
  | Some (`Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _) | None -> None

(* Averages such as [post_video_avg_time_watched] can arrive as floats;
   they are truncated to whole units like the other counts. *)
let int_of_number = function
  | `Int n -> Some n
  | `Float f -> Some (int_of_float f)
  | `Null | `Bool _ | `String _ | `List _ | `Assoc _ -> None

let map_result f items =
  List.fold_right
    (fun item acc ->
      match (acc, f item) with
      | Ok values, Ok value -> Ok (value :: values)
      | (Error _ as error), _ -> error
      | Ok _, (Error _ as error) -> error)
    items (Ok [])

let parse_value ~name = function
  | `Assoc counts ->
      map_result
        (fun (key, json) ->
          match int_of_number json with
          | Some n -> Ok (key, n)
          | None -> Error (Printf.sprintf "%s: value.%s is not a number" name key))
        counts
      |> Result.map (fun counts -> By_type counts)
  | `Int n -> Ok (Count n)
  | `Float f -> Ok (Count (int_of_float f))
  | `Null | `Bool _ | `String _ | `List _ -> Error (name ^ ": value is not a number or an object")

let parse_point ~name json =
  match field "value" json with
  | None -> Error (name ^ ": values[] entry has no value")
  | Some value_json ->
      parse_value ~name value_json
      |> Result.map (fun value -> { value; end_time = string_field "end_time" json })

let parse_metric json =
  match string_field "name" json with
  | None -> Error "insights entry has no name"
  | Some name ->
      let values =
        match field "values" json with
        | Some (`List items) -> items
        | Some (`Null | `Bool _ | `Int _ | `Float _ | `String _ | `Assoc _) | None -> []
      in
      map_result (parse_point ~name) values
      |> Result.map (fun points ->
             { name; points;
               period = Option.bind (string_field "period" json) period_of_string;
               title = string_field "title" json;
               description = string_field "description" json })

let metrics_of_json json =
  match json with
  | `List entries -> map_result parse_metric entries
  | `Assoc _ -> (
      match field "data" json with
      | Some (`List entries) -> map_result parse_metric entries
      | Some (`Null | `Bool _ | `Int _ | `Float _ | `String _ | `Assoc _) | None ->
          Error "insights response has no data array")
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ -> Error "insights response is not an object"

let parse body =
  match Yojson.Basic.from_string body with
  | exception Yojson.Json_error msg -> Error ("insights response is not JSON: " ^ msg)
  | json -> metrics_of_json json

let find name metrics = List.find_opt (fun (metric : metric) -> String.equal metric.name name) metrics

let count = function
  | Count n -> n
  | By_type counts -> List.fold_left (fun acc (_, n) -> acc + n) 0 counts

let latest (metric : metric) =
  match List.rev metric.points with
  | last :: _ -> Some last.value
  | [] -> None

let latest_count name metrics = Option.map count (Option.bind (find name metrics) latest)

let by_type name metrics =
  match Option.bind (find name metrics) latest with
  | Some (By_type counts) -> Some counts
  | Some (Count _) | None -> None

let granularity = function
  | Day -> Some Analytics_types.Day
  | Week -> Some Analytics_types.Week
  | Month -> Some Analytics_types.Month
  | Lifetime -> Some Analytics_types.Lifetime
  | Days_28 | Total_over_range -> None

let to_canonical_series ~scope metrics =
  let canonical (metric : metric) =
    Option.map (fun canonical -> (canonical, metric))
      (Analytics_normalization.facebook_metric_to_canonical metric.name)
  in
  (* One series per canonical metric: the first metric mapping to it wins. *)
  let first_per_canonical =
    List.fold_left
      (fun acc (canonical, metric) ->
        if List.mem_assoc canonical acc then acc else (canonical, metric) :: acc)
      [] (List.filter_map canonical metrics)
    |> List.rev
  in
  List.map
    (fun (canonical, (metric : metric)) ->
      let time_range =
        Option.map
          (fun granularity ->
            { Analytics_types.since = None; until_ = None; granularity = Some granularity })
          (Option.bind metric.period granularity)
      in
      Analytics_types.make_series ?time_range ~metric:canonical ~scope ~provider_metric:metric.name
        (List.map
           (fun point -> Analytics_types.make_datapoint ?timestamp:point.end_time (count point.value))
           metric.points))
    first_per_canonical
