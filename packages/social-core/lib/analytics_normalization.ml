(** Helpers for mapping provider-local metrics to canonical metrics. *)

open Analytics_types

type provider =
  | Facebook
  | Instagram
  | Threads
  | Pinterest
  | TikTok
  | X
  | YouTube

let provider_key = function
  | Facebook -> "facebook"
  | Instagram -> "instagram"
  | Threads -> "threads"
  | Pinterest -> "pinterest"
  | TikTok -> "tiktok"
  | X -> "x"
  | YouTube -> "youtube"

let provider_of_string value =
  match String.lowercase_ascii (String.trim value) with
  | "facebook" -> Some Facebook
  | "instagram" -> Some Instagram
  | "threads" -> Some Threads
  | "pinterest" -> Some Pinterest
  | "tiktok" -> Some TikTok
  | "x" | "twitter" -> Some X
  | "youtube" -> Some YouTube
  | _ -> None

let normalize_metric_token raw =
  raw
  |> String.trim
  |> String.lowercase_ascii
  |> String.map (function
       | '-' | ' ' -> '_'
       | c -> c)

let facebook_metrics =
  [ ("page_impressions_unique", Impressions);
    ("page_posts_impressions_unique", Impressions);
    ("page_post_engagements", Engagements);
    ("page_daily_follows", Follows);
    ("page_video_views", Views);
    ("post_impressions_unique", Impressions);
    ("post_reactions_by_type_total", Reactions);
    ("post_clicks", Clicks);
    ("post_clicks_by_type", Clicks) ]

let instagram_metrics =
  [ ("follower_count", Followers);
    ("reach", Reach);
    ("likes", Likes);
    ("views", Views);
    ("comments", Comments);
    ("shares", Shares);
    ("saves", Saves);
    ("saved", Saves);
    ("replies", Replies) ]

let threads_metrics =
  [ ("views", Views);
    ("likes", Likes);
    ("replies", Replies);
    ("reposts", Reposts);
    ("quotes", Quotes);
    ("shares", Shares) ]

let pinterest_metrics =
  [ ("impression", Impressions);
    ("impressions", Impressions);
    ("pin_click", Pin_clicks);
    ("outbound_click", Outbound_clicks);
    ("save", Saves) ]

let tiktok_metrics =
  [ ("follower_count", Followers);
    ("following_count", Following);
    ("likes_count", Likes);
    ("like_count", Likes);
    ("comment_count", Comments);
    ("share_count", Shares);
    ("view_count", Views);
    ("video_count", Videos);
    ("collect_count", Saves) ]

let x_metrics =
  [ ("impression_count", Impressions);
    ("impressions", Impressions);
    ("like_count", Likes);
    ("likes", Likes);
    ("reply_count", Replies);
    ("replies", Replies);
    ("retweet_count", Reposts);
    ("repost_count", Reposts);
    ("reposts", Reposts);
    ("quote_count", Quotes);
    ("quotes", Quotes);
    ("bookmark_count", Bookmarks);
    ("bookmarks", Bookmarks);
    ("profile_clicks", Profile_visits);
    ("user_profile_clicks", Profile_visits);
    ("profile_visits", Profile_visits);
    ("url_link_clicks", Outbound_clicks);
    ("link_clicks", Outbound_clicks);
    ("engagements", Engagements);
    ("video_view_count", Views);
    ("follower_count", Followers);
    ("following_count", Following) ]

let youtube_metrics =
  [ ("view_count", Views);
    ("viewcount", Views);
    ("views", Views);
    ("like_count", Likes);
    ("likecount", Likes);
    ("likes", Likes);
    ("comment_count", Comments);
    ("commentcount", Comments);
    ("comments", Comments);
    ("subscriber_count", Subscribers);
    ("subscribercount", Subscribers);
    ("video_count", Videos);
    ("videocount", Videos);
    ("favorite_count", Reactions);
    ("favoritecount", Reactions);
    ("watch_time", Watch_time);
    ("watchtime", Watch_time);
    ("estimated_minutes_watched", Watch_time);
    ("impression_count", Impressions);
    ("impressions", Impressions);
    ("share_count", Shares);
    ("shares", Shares) ]

let provider_metrics = function
  | Facebook -> facebook_metrics
  | Instagram -> instagram_metrics
  | Threads -> threads_metrics
  | Pinterest -> pinterest_metrics
  | TikTok -> tiktok_metrics
  | X -> x_metrics
  | YouTube -> youtube_metrics

let provider_metric_to_canonical ~provider raw_metric =
  let metric = normalize_metric_token raw_metric in
  List.assoc_opt metric (provider_metrics provider)

let facebook_metric_to_canonical raw_metric =
  provider_metric_to_canonical ~provider:Facebook raw_metric

let instagram_metric_to_canonical raw_metric =
  provider_metric_to_canonical ~provider:Instagram raw_metric

let threads_metric_to_canonical raw_metric =
  provider_metric_to_canonical ~provider:Threads raw_metric

let pinterest_metric_to_canonical raw_metric =
  provider_metric_to_canonical ~provider:Pinterest raw_metric

let tiktok_metric_to_canonical raw_metric =
  provider_metric_to_canonical ~provider:TikTok raw_metric

let x_metric_to_canonical raw_metric =
  provider_metric_to_canonical ~provider:X raw_metric

let youtube_metric_to_canonical raw_metric =
  provider_metric_to_canonical ~provider:YouTube raw_metric

type normalized_metric = {
  provider_metric : string;
  canonical_metric : canonical_metric;
}

let normalize_provider_metrics ~provider metrics =
  let rec loop seen acc = function
    | [] -> List.rev acc
    | provider_metric :: rest ->
        (match provider_metric_to_canonical ~provider provider_metric with
         | None -> loop seen acc rest
         | Some canonical_metric ->
             if List.mem canonical_metric seen then
               loop seen acc rest
             else
               loop
                 (canonical_metric :: seen)
                 ({ provider_metric; canonical_metric } :: acc)
                 rest)
  in
  loop [] [] metrics

let canonical_metrics_of_provider_metrics ~provider metrics =
  normalize_provider_metrics ~provider metrics
  |> List.map (fun item -> item.canonical_metric)

let canonical_metric_keys_of_provider_metrics ~provider metrics =
  canonical_metrics_of_provider_metrics ~provider metrics
  |> List.map canonical_metric_key
