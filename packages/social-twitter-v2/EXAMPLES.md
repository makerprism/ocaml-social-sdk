# Twitter API v2 - Real-World Examples

This document provides practical, real-world examples of using the Twitter v2 package.

## Table of Contents

1. [Social Media Management](#social-media-management)
2. [Content Discovery](#content-discovery)
3. [Audience Engagement](#audience-engagement)
4. [Analytics & Monitoring](#analytics--monitoring)
5. [Automation & Bots](#automation--bots)

## Social Media Management

### Example 1: Scheduled Tweet Posting

```ocaml
(** Post a scheduled tweet with media *)
let schedule_tweet ~account_id ~text ~image_url ~scheduled_time =
  (* Wait until scheduled time *)
  let now = Unix.time () in
  let wait_seconds = int_of_float (scheduled_time -. now) in
  
  if wait_seconds > 0 then (
    Printf.printf "Waiting %d seconds until scheduled time...\n" wait_seconds;
    Unix.sleep wait_seconds
  );
  
  (* Post tweet *)
  Twitter.post_single
    ~account_id
    ~text
    ~media_urls:[image_url]
    (fun tweet_id ->
      Printf.printf "[%s] Tweet posted: https://twitter.com/i/web/status/%s\n"
        (string_of_float (Unix.time ())) tweet_id)
    (fun error ->
      Printf.eprintf "Failed to post scheduled tweet: %s\n" error)

(* Usage *)
let () =
  let tomorrow = Unix.time () +. (24.0 *. 3600.0) in
  schedule_tweet
    ~account_id:"brand_account"
    ~text:"Happy Monday! Check out our new product launch 🚀"
    ~image_url:"https://cdn.example.com/product.jpg"
    ~scheduled_time:tomorrow
```

### Example 2: Thread Series with Images

```ocaml
(** Post a tutorial thread with images *)
let post_tutorial_thread ~account_id ~sections =
  (* sections is a list of (text, image_url option) *)
  let texts = List.map fst sections in
  let media_urls_per_post = List.map (fun (_, img_opt) ->
    match img_opt with
    | Some url -> [url]
    | None -> []
  ) sections in
  
  Twitter.post_thread
    ~account_id
    ~texts
    ~media_urls_per_post
    (fun tweet_ids ->
      Printf.printf "Thread posted with %d tweets!\n" (List.length tweet_ids);
      Printf.printf "First tweet: https://twitter.com/i/web/status/%s\n"
        (List.hd tweet_ids))
    (fun error ->
      Printf.eprintf "Thread failed: %s\n" error)

(* Usage *)
let () =
  let tutorial_sections = [
    ("1/ Welcome to OCaml Basics! In this thread, I'll cover:\n\n• Pattern matching\n• Types\n• Modules\n\nLet's dive in 🏊", 
     Some "https://cdn.example.com/intro.jpg");
    
    ("2/ Pattern matching is one of OCaml's most powerful features.\n\nIt lets you deconstruct data structures elegantly:",
     Some "https://cdn.example.com/pattern-match.jpg");
    
    ("3/ OCaml's type system catches bugs at compile time.\n\nNo more runtime type errors!",
     Some "https://cdn.example.com/types.jpg");
    
    ("4/ That's it for today! Follow for more OCaml tips 🐫\n\n#OCaml #FunctionalProgramming",
     None);
  ] in
  
  post_tutorial_thread
    ~account_id:"ocaml_tutorials"
    ~sections:tutorial_sections
```

### Example 3: Content Calendar Manager

```ocaml
(** Manage a content calendar *)
type scheduled_post = {
  id: string;
  text: string;
  media_urls: string list;
  post_time: float;
  posted: bool;
}

let post_calendar_item ~account_id ~item =
  Twitter.post_single
    ~account_id
    ~text:item.text
    ~media_urls:item.media_urls
    (fun tweet_id ->
      Printf.printf "✓ Posted: %s (tweet: %s)\n" item.id tweet_id;
      { item with posted = true })
    (fun error ->
      Printf.eprintf "✗ Failed to post %s: %s\n" item.id error;
      item)

let process_calendar ~account_id ~calendar =
  let now = Unix.time () in
  List.filter_map (fun item ->
    if item.posted then
      None  (* Already posted *)
    else if item.post_time <= now then (
      let updated = post_calendar_item ~account_id ~item in
      Some updated
    ) else
      Some item  (* Not yet time *)
  ) calendar
```

## Content Discovery

### Example 4: Trending Topics Monitor

```ocaml
(** Monitor trending topics and save relevant tweets *)
let monitor_trending_topic ~account_id ~topic ~max_tweets =
  let rec fetch_tweets ?(next_token=None) acc =
    if List.length acc >= max_tweets then
      acc
    else
      Twitter.search_tweets
        ~account_id
        ~query:topic
        ~max_results:(min 100 (max_tweets - List.length acc))
        ~next_token
        ~tweet_fields:["created_at"; "public_metrics"; "author_id"]
        ~expansions:["author_id"]
        ()
        (fun json ->
          let open Yojson.Basic.Util in
          let tweets = json |> member "data" |> to_list in
          let new_acc = acc @ tweets in
          
          Printf.printf "Fetched %d tweets (total: %d)\n" 
            (List.length tweets) (List.length new_acc);
          
          let meta = Twitter.parse_pagination_meta json in
          match meta.next_token with
          | Some token when List.length new_acc < max_tweets ->
              Unix.sleep 1;  (* Rate limit courtesy *)
              fetch_tweets ~next_token:(Some token) new_acc
          | _ ->
              new_acc)
        (fun error ->
          Printf.eprintf "Search failed: %s\n" error;
          acc)
  in
  fetch_tweets []

(* Usage *)
let () =
  let tweets = monitor_trending_topic
    ~account_id:"monitor_account"
    ~topic:"#OCaml OR #FunctionalProgramming"
    ~max_tweets:500 in
  
  Printf.printf "Collected %d tweets about OCaml\n" (List.length tweets)
```

### Example 5: Influencer Content Curator

```ocaml
(** Curate and retweet content from influencers *)
let curate_from_influencer ~account_id ~username ~keywords =
  (* First, get the influencer's user ID *)
  Twitter.get_user_by_username
    ~account_id
    ~username
    ()
    (fun user_json ->
      let open Yojson.Basic.Util in
      let user_id = user_json |> member "data" |> member "id" |> to_string in
      
      (* Get their recent tweets *)
      Twitter.get_user_timeline
        ~account_id
        ~user_id
        ~max_results:20
        ~tweet_fields:["created_at"; "public_metrics"]
        ()
        (fun timeline_json ->
          let tweets = timeline_json |> member "data" |> to_list in
          
          (* Filter for relevant keywords *)
          let relevant = List.filter (fun tweet ->
            let text = tweet |> member "text" |> to_string |> String.lowercase_ascii in
            List.exists (fun keyword ->
              String.contains text (String.get keyword 0)
            ) keywords
          ) tweets in
          
          Printf.printf "Found %d relevant tweets from @%s\n" 
            (List.length relevant) username;
          
          (* Retweet the best ones *)
          List.iter (fun tweet ->
            let tweet_id = tweet |> member "id" |> to_string in
            let metrics = tweet |> member "public_metrics" in
            let likes = metrics |> member "like_count" |> to_int in
            
            if likes > 100 then (
              Twitter.retweet
                ~account_id
                ~tweet_id
                (fun () -> Printf.printf "Retweeted: %s\n" tweet_id)
                (fun err -> Printf.eprintf "RT failed: %s\n" err)
            )
          ) relevant)
        (fun error ->
          Printf.eprintf "Timeline fetch failed: %s\n" error))
    (fun error ->
      Printf.eprintf "User lookup failed: %s\n" error)

(* Usage *)
let () =
  curate_from_influencer
    ~account_id:"curator_account"
    ~username:"OCamlLabs"
    ~keywords:["type"; "functional"; "compiler"; "performance"]
```

## Audience Engagement

### Example 6: Auto-Reply to Mentions

```ocaml
(** Automatically reply to mentions with helpful info *)
let auto_reply_to_mentions ~account_id ~keywords_responses =
  (* Get authenticated user first *)
  Twitter.get_me ~account_id ()
    (fun me_json ->
      let open Yojson.Basic.Util in
      let my_username = me_json |> member "data" |> member "username" |> to_string in
      
      (* Search for mentions *)
      Twitter.search_tweets
        ~account_id
        ~query:(Printf.sprintf "@%s" my_username)
        ~max_results:100
        ()
        (fun results_json ->
          let tweets = results_json |> member "data" |> to_list in
          
          List.iter (fun tweet ->
            let tweet_id = tweet |> member "id" |> to_string in
            let text = tweet |> member "text" |> to_string |> String.lowercase_ascii in
            
            (* Find matching keyword *)
            let response_opt = List.find_opt (fun (keyword, _) ->
              String.contains text (String.get keyword 0)
            ) keywords_responses in
            
            match response_opt with
            | Some (_, response) ->
                Twitter.reply_to_tweet
                  ~account_id
                  ~text:response
                  ~reply_to_tweet_id:tweet_id
                  ~media_urls:[]
                  (fun reply_id -> 
                    Printf.printf "Replied to %s with %s\n" tweet_id reply_id)
                  (fun err -> 
                    Printf.eprintf "Reply failed: %s\n" err)
            | None ->
                Printf.printf "No matching keyword for tweet %s\n" tweet_id
          ) tweets)
        (fun error ->
          Printf.eprintf "Mention search failed: %s\n" error))
    (fun error ->
      Printf.eprintf "Get user failed: %s\n" error)

(* Usage *)
let () =
  let keywords_and_responses = [
    ("help", "Hi! Thanks for reaching out. Check our docs at https://example.com/docs or DM for support!");
    ("pricing", "Our pricing starts at $9/month. Full details: https://example.com/pricing");
    ("demo", "Want a demo? Book one here: https://example.com/demo");
  ] in
  
  auto_reply_to_mentions
    ~account_id:"support_account"
    ~keywords_responses:keywords_and_responses
```

### Example 7: Thank Followers

```ocaml
(** Thank new followers *)
let thank_follower ~account_id ~follower_id =
  Twitter.get_user_by_id
    ~account_id
    ~user_id:follower_id
    ()
    (fun user_json ->
      let open Yojson.Basic.Util in
      let username = user_json |> member "data" |> member "username" |> to_string in
      
      (* Create personalized thank you tweet *)
      let thank_you = Printf.sprintf 
        "Thanks for following, @%s! 🙏 We share daily OCaml tips and tutorials. What topics interest you most?" 
        username in
      
      Twitter.post_single
        ~account_id
        ~text:thank_you
        ~media_urls:[]
        (fun tweet_id ->
          Printf.printf "Thanked @%s (tweet: %s)\n" username tweet_id)
        (fun error ->
          Printf.eprintf "Thank you tweet failed: %s\n" error))
    (fun error ->
      Printf.eprintf "User lookup failed: %s\n" error)
```

### Example 8: Like & Bookmark Valuable Content

```ocaml
(** Like and bookmark high-quality content *)
let engage_with_quality_content ~account_id ~topic ~quality_threshold =
  Twitter.search_tweets
    ~account_id
    ~query:topic
    ~max_results:50
    ~tweet_fields:["public_metrics"; "created_at"]
    ()
    (fun json ->
      let open Yojson.Basic.Util in
      let tweets = json |> member "data" |> to_list in
      
      List.iter (fun tweet ->
        let tweet_id = tweet |> member "id" |> to_string in
        let metrics = tweet |> member "public_metrics" in
        let likes = metrics |> member "like_count" |> to_int in
        let retweets = metrics |> member "retweet_count" |> to_int in
        
        (* Calculate engagement score *)
        let score = likes + (retweets * 2) in
        
        if score > quality_threshold then (
          (* Like it *)
          Twitter.like_tweet
            ~account_id
            ~tweet_id
            (fun () -> Printf.printf "Liked tweet %s (score: %d)\n" tweet_id score)
            (fun _ -> ());
          
          (* Bookmark for later *)
          Twitter.bookmark_tweet
            ~account_id
            ~tweet_id
            (fun () -> Printf.printf "Bookmarked tweet %s\n" tweet_id)
            (fun _ -> ())
        )
      ) tweets)
    (fun error ->
      Printf.eprintf "Search failed: %s\n" error)

(* Usage *)
let () =
  engage_with_quality_content
    ~account_id:"curator_account"
    ~topic:"#MachineLearning"
    ~quality_threshold:1000  (* 1000+ engagement score *)
```

## Analytics & Monitoring

### Example 9: Track Competitor Activity

```ocaml
(** Monitor competitor tweets and engagement *)
type competitor_activity = {
  username: string;
  tweet_count: int;
  avg_likes: float;
  avg_retweets: float;
  top_tweet_id: string option;
}

let analyze_competitor ~account_id ~username =
  Twitter.get_user_by_username
    ~account_id
    ~username
    ()
    (fun user_json ->
      let open Yojson.Basic.Util in
      let user_id = user_json |> member "data" |> member "id" |> to_string in
      
      Twitter.get_user_timeline
        ~account_id
        ~user_id
        ~max_results:100
        ~tweet_fields:["public_metrics"; "created_at"]
        ()
        (fun timeline_json ->
          let tweets = timeline_json |> member "data" |> to_list in
          
          let total_likes = ref 0 in
          let total_retweets = ref 0 in
          let max_engagement = ref 0 in
          let top_tweet = ref None in
          
          List.iter (fun tweet ->
            let metrics = tweet |> member "public_metrics" in
            let likes = metrics |> member "like_count" |> to_int in
            let retweets = metrics |> member "retweet_count" |> to_int in
            let engagement = likes + retweets in
            
            total_likes := !total_likes + likes;
            total_retweets := !total_retweets + retweets;
            
            if engagement > !max_engagement then (
              max_engagement := engagement;
              top_tweet := Some (tweet |> member "id" |> to_string)
            )
          ) tweets;
          
          let count = List.length tweets in
          let activity = {
            username;
            tweet_count = count;
            avg_likes = float_of_int !total_likes /. float_of_int count;
            avg_retweets = float_of_int !total_retweets /. float_of_int count;
            top_tweet_id = !top_tweet;
          } in
          
          Printf.printf "\n=== Competitor Analysis: @%s ===\n" username;
          Printf.printf "Tweets analyzed: %d\n" activity.tweet_count;
          Printf.printf "Avg likes: %.1f\n" activity.avg_likes;
          Printf.printf "Avg retweets: %.1f\n" activity.avg_retweets;
          (match activity.top_tweet_id with
           | Some id -> Printf.printf "Top tweet: https://twitter.com/i/web/status/%s\n" id
           | None -> ());
          Printf.printf "===============================\n\n")
        (fun error ->
          Printf.eprintf "Timeline failed: %s\n" error))
    (fun error ->
      Printf.eprintf "User lookup failed: %s\n" error)

(* Usage *)
let () =
  let competitors = ["CompanyA"; "CompanyB"; "CompanyC"] in
  List.iter (fun competitor ->
    analyze_competitor ~account_id:"analytics_account" ~username:competitor;
    Unix.sleep 2  (* Rate limit courtesy *)
  ) competitors
```

### Example 10: Sentiment Analysis Pipeline

```ocaml
(** Collect tweets for sentiment analysis *)
let collect_for_sentiment ~account_id ~brand_name ~output_file =
  let rec collect ?(next_token=None) acc =
    Twitter.search_tweets
      ~account_id
      ~query:brand_name
      ~max_results:100
      ~next_token
      ~tweet_fields:["created_at"; "author_id"; "public_metrics"]
      ()
      (fun json ->
        let open Yojson.Basic.Util in
        let tweets = json |> member "data" |> to_list in
        let new_acc = acc @ tweets in
        
        Printf.printf "Collected %d tweets (total: %d)\n" 
          (List.length tweets) (List.length new_acc);
        
        if List.length new_acc >= 1000 then (
          (* Save to file *)
          let oc = open_out output_file in
          let json_out = `List new_acc in
          Yojson.Basic.to_channel oc json_out;
          close_out oc;
          Printf.printf "Saved %d tweets to %s\n" (List.length new_acc) output_file
        ) else (
          let meta = Twitter.parse_pagination_meta json in
          match meta.next_token with
          | Some token ->
              Unix.sleep 1;
              collect ~next_token:(Some token) new_acc
          | None ->
              Printf.printf "Reached end of results with %d tweets\n" 
                (List.length new_acc)
        ))
      (fun error ->
        Printf.eprintf "Search failed: %s\n" error)
  in
  collect []

(* Usage *)
let () =
  collect_for_sentiment
    ~account_id:"analytics_account"
    ~brand_name:"YourBrand OR #YourBrand"
    ~output_file:"sentiment_data.json"
```

## Automation & Bots

### Example 11: Weather Bot

```ocaml
(** Post daily weather updates *)
let post_weather_update ~account_id ~location ~temperature ~condition =
  let emoji = match condition with
    | "sunny" -> "☀️"
    | "cloudy" -> "☁️"
    | "rainy" -> "🌧️"
    | "snowy" -> "❄️"
    | _ -> "🌤️"
  in
  
  let text = Printf.sprintf 
    "%s Weather Update for %s\n\n🌡️ Temperature: %d°C\n%s %s\n\n#Weather #%s"
    emoji location temperature emoji (String.capitalize_ascii condition) location in
  
  Twitter.post_single
    ~account_id
    ~text
    ~media_urls:[]
    (fun tweet_id ->
      Printf.printf "Weather update posted: %s\n" tweet_id)
    (fun error ->
      Printf.eprintf "Failed to post weather: %s\n" error)

(* Run daily *)
let () =
  post_weather_update
    ~account_id:"weather_bot"
    ~location:"London"
    ~temperature:18
    ~condition:"cloudy"
```

### Example 12: Quote Bot

```ocaml
(** Post inspirational quotes *)
let quotes = [
  "The best time to plant a tree was 20 years ago. The second best time is now.";
  "Success is not final, failure is not fatal: it is the courage to continue that counts.";
  "The only way to do great work is to love what you do.";
]

let post_daily_quote ~account_id =
  (* Pick random quote *)
  Random.self_init ();
  let quote = List.nth quotes (Random.int (List.length quotes)) in
  
  let text = Printf.sprintf "💭 Daily Inspiration\n\n\"%s\"\n\n#Motivation #Quotes" quote in
  
  Twitter.post_single
    ~account_id
    ~text
    ~media_urls:[]
    (fun tweet_id ->
      Printf.printf "Quote posted: %s\n" tweet_id)
    (fun error ->
      Printf.eprintf "Failed to post quote: %s\n" error)
```

### Example 13: News Aggregator Bot

```ocaml
(** Retweet news from verified sources *)
let aggregate_news ~account_id ~sources ~topics =
  List.iter (fun source ->
    Twitter.get_user_by_username
      ~account_id
      ~username:source
      ~user_fields:["verified"]
      ()
      (fun user_json ->
        let open Yojson.Basic.Util in
        let verified = try 
          user_json |> member "data" |> member "verified" |> to_bool 
        with _ -> false in
        
        if verified then (
          let user_id = user_json |> member "data" |> member "id" |> to_string in
          
          Twitter.get_user_timeline
            ~account_id
            ~user_id
            ~max_results:5
            ()
            (fun timeline_json ->
              let tweets = timeline_json |> member "data" |> to_list in
              
              List.iter (fun tweet ->
                let text = tweet |> member "text" |> to_string |> String.lowercase_ascii in
                let tweet_id = tweet |> member "id" |> to_string in
                
                (* Check if tweet matches topics *)
                let matches = List.exists (fun topic ->
                  String.contains text (String.get (String.lowercase_ascii topic) 0)
                ) topics in
                
                if matches then (
                  Twitter.retweet
                    ~account_id
                    ~tweet_id
                    (fun () -> Printf.printf "Retweeted news from @%s\n" source)
                    (fun _ -> ())
                )
              ) tweets)
            (fun _ -> ()))
        else
          Printf.printf "Source @%s is not verified, skipping\n" source)
      (fun _ -> ());
    
    Unix.sleep 2  (* Rate limit courtesy *)
  ) sources

(* Usage *)
let () =
  aggregate_news
    ~account_id:"news_aggregator"
    ~sources:["BBCWorld"; "CNN"; "Reuters"]
    ~topics:["technology"; "science"; "innovation"]
```

## Tips & Best Practices

### Rate Limiting

Always respect rate limits:

```ocaml
(* Add delays between requests *)
let with_rate_limit f =
  f ();
  Unix.sleep 1  (* 1 second between requests *)

(* Check rate limit headers *)
let check_rate_limits response_headers =
  match Twitter.parse_rate_limit_headers response_headers with
  | Some info when info.remaining < 10 ->
      Printf.printf "WARNING: Only %d requests remaining!\n" info.remaining;
      let wait_time = info.reset - int_of_float (Unix.time ()) in
      if wait_time > 0 then
        Printf.printf "Rate limit resets in %d seconds\n" wait_time
  | _ -> ()
```

### Error Handling

Always implement proper error handling:

```ocaml
let safe_post ~account_id ~text =
  Twitter.post_single
    ~account_id
    ~text
    ~media_urls:[]
    (fun tweet_id ->
      Printf.printf "✓ Success: %s\n" tweet_id)
    (fun error ->
      (* Log error *)
      Printf.eprintf "✗ Error: %s\n" error;
      
      (* Retry logic *)
      if String.contains error 't' then (
        Printf.printf "Retrying in 5 seconds...\n";
        Unix.sleep 5;
        Twitter.post_single ~account_id ~text ~media_urls:[] 
          (fun id -> Printf.printf "✓ Retry success: %s\n" id)
          (fun err -> Printf.eprintf "✗ Retry failed: %s\n" err)
      ))
```

### Pagination Pattern

Common pagination pattern:

```ocaml
let rec fetch_all ~account_id ~query ?(next_token=None) ?(acc=[]) () =
  Twitter.search_tweets
    ~account_id
    ~query
    ~next_token
    ()
    (fun json ->
      let open Yojson.Basic.Util in
      let tweets = json |> member "data" |> to_list in
      let new_acc = acc @ tweets in
      
      let meta = Twitter.parse_pagination_meta json in
      match meta.next_token with
      | Some token ->
          Unix.sleep 1;  (* Rate limit courtesy *)
          fetch_all ~account_id ~query ~next_token:(Some token) ~acc:new_acc ()
      | None ->
          Printf.printf "Fetched total: %d tweets\n" (List.length new_acc);
          new_acc)
    (fun error ->
      Printf.eprintf "Pagination error: %s\n" error;
      acc)
```

## Conclusion

These examples demonstrate real-world use cases for the Twitter v2 package. For more examples, check the test suite and README.

Happy building! 🚀
