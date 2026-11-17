type media_product = Feed | Reels | Story

let media_product_to_string = function Feed -> "FEED" | Reels -> "REELS" | Story -> "STORY"

let media_product_of_string raw =
  List.find_opt
    (fun product -> String.equal (media_product_to_string product) (String.uppercase_ascii (String.trim raw)))
    [ Feed; Reels; Story ]

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

let media_metric_to_string = function
  | Views -> "views"
  | Reach -> "reach"
  | Likes -> "likes"
  | Comments -> "comments"
  | Shares -> "shares"
  | Saved -> "saved"
  | Total_interactions -> "total_interactions"
  | Follows -> "follows"
  | Profile_visits -> "profile_visits"
  | Profile_activity -> "profile_activity"
  | Replies -> "replies"
  | Navigation -> "navigation"
  | Reposts -> "reposts"
  | Ig_reels_avg_watch_time -> "ig_reels_avg_watch_time"
  | Ig_reels_video_view_total_time -> "ig_reels_video_view_total_time"
  | Facebook_views -> "facebook_views"

let all_media_metrics =
  [ Views; Reach; Likes; Comments; Shares; Saved; Total_interactions; Follows; Profile_visits;
    Profile_activity; Replies; Navigation; Reposts; Ig_reels_avg_watch_time;
    Ig_reels_video_view_total_time; Facebook_views ]

let normalize raw = String.lowercase_ascii (String.trim raw)

let media_metric_of_string raw =
  List.find_opt (fun metric -> String.equal (media_metric_to_string metric) (normalize raw)) all_media_metrics

let supported_media_metrics = function
  | Feed ->
      [ Views; Reach; Likes; Comments; Shares; Saved; Total_interactions; Follows; Profile_visits;
        Profile_activity; Reposts; Facebook_views ]
  | Reels ->
      [ Views; Reach; Likes; Comments; Shares; Saved; Total_interactions; Ig_reels_avg_watch_time;
        Ig_reels_video_view_total_time; Reposts; Facebook_views ]
  | Story ->
      [ Views; Reach; Shares; Total_interactions; Follows; Profile_visits; Profile_activity; Replies;
        Navigation; Reposts; Facebook_views ]

let default_media_metrics = function
  | Feed -> [ Views; Reach; Likes; Comments; Shares; Saved; Total_interactions; Follows; Profile_visits ]
  | Reels -> [ Views; Reach; Likes; Comments; Shares; Saved; Total_interactions; Ig_reels_avg_watch_time ]
  | Story -> [ Views; Reach; Shares; Total_interactions; Follows; Profile_visits; Replies ]

let unsupported_media_metrics product metrics =
  let supported = supported_media_metrics product in
  List.filter (fun metric -> not (List.mem metric supported)) metrics

type media_insight = { metric : media_metric; value : int }

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

let account_metric_to_string = function
  | Account_views -> "views"
  | Account_reach -> "reach"
  | Accounts_engaged -> "accounts_engaged"
  | Account_total_interactions -> "total_interactions"
  | Account_likes -> "likes"
  | Account_comments -> "comments"
  | Account_shares -> "shares"
  | Account_saves -> "saves"
  | Account_replies -> "replies"
  | Account_reposts -> "reposts"
  | Profile_links_taps -> "profile_links_taps"
  | Follows_and_unfollows -> "follows_and_unfollows"

let all_account_metrics =
  [ Account_views; Account_reach; Accounts_engaged; Account_total_interactions; Account_likes;
    Account_comments; Account_shares; Account_saves; Account_replies; Account_reposts;
    Profile_links_taps; Follows_and_unfollows ]

let account_metric_of_string raw =
  List.find_opt (fun metric -> String.equal (account_metric_to_string metric) (normalize raw)) all_account_metrics

let default_account_metrics =
  [ Account_views; Account_reach; Accounts_engaged; Account_total_interactions; Account_likes;
    Account_comments; Account_shares; Account_saves; Account_replies ]

type account_total = { metric : account_metric; total : int }
type follower_change = { end_time : string option; change : int }

type account_counts = {
  followers_count : int option;
  follows_count : int option;
  media_count : int option;
}

let account_count_fields = [ "id"; "followers_count"; "follows_count"; "media_count" ]

(* JSON helpers *)

let field key = function
  | `Assoc fields -> List.assoc_opt key fields
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> None

let int_of = function
  | Some (`Int n) -> Some n
  | Some (`Float f) -> Some (int_of_float f)
  | Some (`Null | `Bool _ | `String _ | `List _ | `Assoc _) | None -> None

let string_of = function
  | Some (`String s) -> Some s
  | Some (`Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _) | None -> None

let list_of = function
  | Some (`List items) -> items
  | Some (`Null | `Bool _ | `Int _ | `Float _ | `String _ | `Assoc _) | None -> []

let map_result f items =
  List.fold_right
    (fun item acc ->
      match (acc, f item) with
      | Ok values, Ok value -> Ok (value :: values)
      | (Error _ as error), _ -> error
      | Ok _, (Error _ as error) -> error)
    items (Ok [])

let parse_json body =
  match Yojson.Basic.from_string body with
  | exception Yojson.Json_error msg -> Error ("insights response is not JSON: " ^ msg)
  | json -> Ok json

let data_entries body =
  Result.bind (parse_json body) (fun json ->
      match field "data" json with
      | Some (`List entries) -> Ok entries
      | Some (`Null | `Bool _ | `Int _ | `Float _ | `String _ | `Assoc _) | None ->
          Error "insights response has no data array")

let first_value entry =
  match list_of (field "values" entry) with
  | first :: _ -> int_of (field "value" first)
  | [] -> None

let total_value entry = int_of (Option.bind (field "total_value" entry) (field "value"))

let parse_media_insights body =
  Result.bind (data_entries body) (fun entries ->
      map_result
        (fun entry ->
          match Option.bind (string_of (field "name" entry)) media_metric_of_string with
          | None -> Ok None
          | Some metric -> (
              match (first_value entry, total_value entry) with
              | Some value, _ | None, Some value -> Ok (Some { metric; value })
              | None, None -> Error (media_metric_to_string metric ^ ": entry has no value")))
        entries
      |> Result.map (List.filter_map Fun.id))

let parse_account_totals body =
  Result.bind (data_entries body) (fun entries ->
      map_result
        (fun entry ->
          match Option.bind (string_of (field "name" entry)) account_metric_of_string with
          | None -> Ok None
          | Some metric -> (
              match total_value entry with
              | Some total -> Ok (Some { metric; total })
              | None -> Error (account_metric_to_string metric ^ ": entry has no total_value")))
        entries
      |> Result.map (List.filter_map Fun.id))

let parse_follower_changes body =
  Result.bind (data_entries body) (fun entries ->
      match
        List.find_opt (fun entry -> string_of (field "name" entry) = Some "follower_count") entries
      with
      | None -> Ok []
      | Some entry ->
          map_result
            (fun point ->
              match int_of (field "value" point) with
              | Some change -> Ok { end_time = string_of (field "end_time" point); change }
              | None -> Error "follower_count: values[] entry has no integer value")
            (list_of (field "values" entry)))

let parse_account_counts body =
  Result.bind (parse_json body) (fun json ->
      match json with
      | `Assoc _ ->
          Ok
            { followers_count = int_of (field "followers_count" json);
              follows_count = int_of (field "follows_count" json);
              media_count = int_of (field "media_count" json) }
      | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ ->
          Error "profile response is not an object")
