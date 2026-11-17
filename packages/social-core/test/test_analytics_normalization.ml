open Analytics_types
open Analytics_normalization

let fail_expected_some ~context expected_key =
  failwith
    (Printf.sprintf
       "%s expected Some %s but got None"
       context
       expected_key)

let assert_metric ~context ~expected actual =
  match actual with
  | None -> fail_expected_some ~context (canonical_metric_key expected)
  | Some metric ->
      if metric <> expected then
        failwith
          (Printf.sprintf
             "%s expected %s but got %s"
             context
             (canonical_metric_key expected)
             (canonical_metric_key metric))

let assert_string_list ~context expected actual =
  if expected <> actual then
    let expected_text = String.concat "," expected in
    let actual_text = String.concat "," actual in
    failwith
      (Printf.sprintf
         "%s expected [%s] but got [%s]"
         context
         expected_text
         actual_text)

let test_provider_mappings () =
  assert_metric
    ~context:"facebook page_impressions_unique"
    ~expected:Impressions
    (facebook_metric_to_canonical "page_impressions_unique");
  assert_metric
    ~context:"instagram saved"
    ~expected:Saves
    (instagram_metric_to_canonical "saved");
  assert_metric
    ~context:"threads reposts"
    ~expected:Reposts
    (threads_metric_to_canonical "reposts");
  assert_metric
    ~context:"pinterest IMPRESSION"
    ~expected:Impressions
    (pinterest_metric_to_canonical " IMPRESSION ");
  assert_metric
    ~context:"tiktok likes_count"
    ~expected:Likes
    (tiktok_metric_to_canonical "likes_count");
  assert_metric
    ~context:"x retweet_count"
    ~expected:Reposts
    (x_metric_to_canonical "retweet_count");
  assert_metric
    ~context:"youtube viewCount"
    ~expected:Views
    (youtube_metric_to_canonical "viewCount");
  List.iter
    (fun (name, expected) ->
      assert_metric ~context:("facebook " ^ name) ~expected (facebook_metric_to_canonical name))
    [ ("page_media_view", Views); ("page_total_media_view_unique", Reach);
      ("page_follows", Followers); ("page_daily_follows_unique", Follows);
      ("post_media_view", Views); ("fb_reels_total_plays", Views) ];
  (* Regression: the daily follower_count insight was mapped to Followers,
     so a one-day change could overwrite the follower total. *)
  assert_metric ~context:"instagram follower_count" ~expected:Follows
    (instagram_metric_to_canonical "follower_count");
  assert_metric ~context:"instagram followers_count" ~expected:Followers
    (instagram_metric_to_canonical "followers_count");
  print_endline "ok: provider mapping rules";

  if facebook_metric_to_canonical "does_not_exist" <> None then
    failwith "facebook unknown metric should return None";
  print_endline "ok: unknown metrics are ignored"

let test_stable_order_and_dedupe () =
  let normalized =
    normalize_provider_metrics
      ~provider:TikTok
      [ "view_count";
        "like_count";
        "likes_count";
        "comment_count";
        "view_count";
        "share_count";
        "unknown_metric" ]
  in
  let canonical_keys =
    List.map (fun item -> canonical_metric_key item.canonical_metric) normalized
  in
  let provider_metrics = List.map (fun item -> item.provider_metric) normalized in
  assert_string_list
    ~context:"canonical key ordering"
    [ "views"; "likes"; "comments"; "shares" ]
    canonical_keys;
  assert_string_list
    ~context:"provider metric first occurrence retained"
    [ "view_count"; "like_count"; "comment_count"; "share_count" ]
    provider_metrics;
  print_endline "ok: stable ordering and dedupe"

let test_key_projection_helper () =
  let keys =
    canonical_metric_keys_of_provider_metrics
      ~provider:Facebook
      [ "post_clicks";
        "post_clicks_by_type";
        "page_post_engagements";
        "page_post_engagements" ]
  in
  assert_string_list
    ~context:"canonical_metric_keys_of_provider_metrics"
    [ "clicks"; "engagements" ]
    keys;
  print_endline "ok: canonical metric key projection"

let () =
  test_provider_mappings ();
  test_stable_order_and_dedupe ();
  test_key_projection_helper ();
  print_endline "All analytics normalization tests passed."
