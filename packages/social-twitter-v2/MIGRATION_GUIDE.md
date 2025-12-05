# Migration Guide: Twitter v2 Package Enhancement

## Overview

The Twitter v2 package has been significantly enhanced with 35+ new features. This guide helps you migrate from the basic implementation to the feature-complete version.

## What's New

### Summary of Changes

- **20+ new API endpoints** added
- **Tweet operations**: Now includes delete, get, search, timeline
- **User operations**: Get user info, follow/unfollow, block/unblock
- **Engagement**: Like, retweet, quote, reply, bookmark
- **Media upload**: Added chunked upload for large videos
- **Pagination**: Full cursor-based pagination support
- **Expansions**: Support for v2 expansions and field selection
- **Rate limiting**: Parse API response headers

### Breaking Changes

#### 1. Thread Posting Behavior Changed

**Before** (simplified version):
```ocaml
(* Only posted the first tweet *)
Twitter.post_thread
  ~account_id:"account"
  ~texts:["First"; "Second"; "Third"]
  ~media_urls_per_post:[["url1.jpg"]; []; []]
  (fun tweet_ids -> (* Only 1 tweet ID returned *))
  on_error
```

**After** (full implementation):
```ocaml
(* Posts all tweets in sequence with proper threading *)
Twitter.post_thread
  ~account_id:"account"
  ~texts:["First"; "Second"; "Third"]
  ~media_urls_per_post:[["url1.jpg"]; []; []]
  (fun tweet_ids -> 
    (* Returns list with 3 tweet IDs *)
    Printf.printf "Posted %d tweets\n" (List.length tweet_ids))
  on_error
```

**Impact**: If you were using `post_thread`, you'll now get ALL tweets posted, not just the first one.

**Action Required**: Review any code using `post_thread` to ensure it handles multiple tweet IDs.

#### 2. New Optional Parameters

Several functions now accept optional parameters for expansions and fields:

```ocaml
(* Old signature *)
val get_tweet : 
  account_id:string -> 
  tweet_id:string -> 
  unit -> 
  (Yojson.Basic.t -> unit) -> 
  (string -> unit) -> 
  unit

(* New signature *)
val get_tweet :
  account_id:string ->
  tweet_id:string ->
  ?expansions:string list ->
  ?tweet_fields:string list ->
  unit ->
  (Yojson.Basic.t -> unit) ->
  (string -> unit) ->
  unit
```

**Impact**: None - these are optional parameters with sensible defaults.

**Action Required**: None, unless you want to use expansions/fields.

## New Features Guide

### 1. Tweet READ Operations

#### Get a Tweet by ID

```ocaml
(* Basic usage *)
Twitter.get_tweet
  ~account_id:"my_account"
  ~tweet_id:"1234567890"
  ()
  (fun json -> 
    let open Yojson.Basic.Util in
    let text = json |> member "data" |> member "text" |> to_string in
    Printf.printf "Tweet: %s\n" text)
  (fun error -> Printf.eprintf "Error: %s\n" error)

(* With expansions and fields *)
Twitter.get_tweet
  ~account_id:"my_account"
  ~tweet_id:"1234567890"
  ~expansions:["author_id"; "referenced_tweets.id"]
  ~tweet_fields:["created_at"; "public_metrics"; "entities"]
  ()
  on_success
  on_error
```

#### Delete a Tweet

```ocaml
Twitter.delete_tweet
  ~account_id:"my_account"
  ~tweet_id:"1234567890"
  (fun () -> print_endline "Tweet deleted successfully")
  (fun error -> Printf.eprintf "Failed to delete: %s\n" error)
```

#### Search Tweets

```ocaml
(* Basic search *)
Twitter.search_tweets
  ~account_id:"my_account"
  ~query:"OCaml programming"
  ~max_results:10
  ()
  (fun json -> (* Process results *))
  on_error

(* With pagination *)
Twitter.search_tweets
  ~account_id:"my_account"
  ~query:"OCaml programming"
  ~max_results:50
  ~next_token:(Some "pagination_token_here")
  ~tweet_fields:["created_at"; "author_id"]
  ()
  (fun json ->
    (* Extract pagination metadata *)
    let meta = Twitter.parse_pagination_meta json in
    match meta.next_token with
    | Some token -> 
        print_endline "More results available";
        (* Fetch next page with token *)
    | None -> 
        print_endline "No more results")
  on_error
```

#### Get User Timeline

```ocaml
Twitter.get_user_timeline
  ~account_id:"my_account"
  ~user_id:"user_12345"
  ~max_results:20
  ~pagination_token:None
  ~expansions:["author_id"]
  ~tweet_fields:["created_at"; "public_metrics"]
  ()
  on_success
  on_error
```

### 2. User Operations

#### Get User Information

```ocaml
(* By ID *)
Twitter.get_user_by_id
  ~account_id:"my_account"
  ~user_id:"12345"
  ~user_fields:["description"; "public_metrics"; "verified"]
  ()
  (fun json ->
    let open Yojson.Basic.Util in
    let user = json |> member "data" in
    let username = user |> member "username" |> to_string in
    Printf.printf "Username: @%s\n" username)
  on_error

(* By username *)
Twitter.get_user_by_username
  ~account_id:"my_account"
  ~username:"elonmusk"
  ~user_fields:["profile_image_url"; "verified"]
  ()
  on_success
  on_error

(* Get authenticated user *)
Twitter.get_me
  ~account_id:"my_account"
  ~user_fields:["created_at"]
  ()
  on_success
  on_error
```

#### Follow/Unfollow Users

```ocaml
(* Follow *)
Twitter.follow_user
  ~account_id:"my_account"
  ~target_user_id:"12345"
  (fun () -> print_endline "Successfully followed")
  on_error

(* Unfollow *)
Twitter.unfollow_user
  ~account_id:"my_account"
  ~target_user_id:"12345"
  (fun () -> print_endline "Successfully unfollowed")
  on_error
```

#### Block/Unblock Users

```ocaml
(* Block *)
Twitter.block_user
  ~account_id:"my_account"
  ~target_user_id:"12345"
  (fun () -> print_endline "User blocked")
  on_error

(* Unblock *)
Twitter.unblock_user
  ~account_id:"my_account"
  ~target_user_id:"12345"
  (fun () -> print_endline "User unblocked")
  on_error
```

### 3. Engagement Operations

#### Like/Unlike Tweets

```ocaml
(* Like *)
Twitter.like_tweet
  ~account_id:"my_account"
  ~tweet_id:"1234567890"
  (fun () -> print_endline "Liked!")
  on_error

(* Unlike *)
Twitter.unlike_tweet
  ~account_id:"my_account"
  ~tweet_id:"1234567890"
  (fun () -> print_endline "Unliked!")
  on_error
```

#### Retweet/Unretweet

```ocaml
(* Retweet *)
Twitter.retweet
  ~account_id:"my_account"
  ~tweet_id:"1234567890"
  (fun () -> print_endline "Retweeted!")
  on_error

(* Unretweet *)
Twitter.unretweet
  ~account_id:"my_account"
  ~tweet_id:"1234567890"
  (fun () -> print_endline "Unretweeted!")
  on_error
```

#### Quote Tweet

```ocaml
Twitter.quote_tweet
  ~account_id:"my_account"
  ~text:"Great article! Everyone should read this."
  ~quoted_tweet_id:"1234567890"
  ~media_urls:[]
  (fun tweet_id -> 
    Printf.printf "Quote tweet posted: %s\n" tweet_id)
  on_error
```

#### Reply to Tweet

```ocaml
Twitter.reply_to_tweet
  ~account_id:"my_account"
  ~text:"Thanks for sharing this!"
  ~reply_to_tweet_id:"1234567890"
  ~media_urls:["https://example.com/image.jpg"]
  (fun tweet_id -> 
    Printf.printf "Reply posted: %s\n" tweet_id)
  on_error
```

#### Bookmarks

```ocaml
(* Bookmark *)
Twitter.bookmark_tweet
  ~account_id:"my_account"
  ~tweet_id:"1234567890"
  (fun () -> print_endline "Bookmarked!")
  on_error

(* Remove bookmark *)
Twitter.remove_bookmark
  ~account_id:"my_account"
  ~tweet_id:"1234567890"
  (fun () -> print_endline "Bookmark removed!")
  on_error
```

### 4. Chunked Media Upload

For large videos or when you want to track upload progress:

```ocaml
(* Read video file *)
let video_data = (* read file bytes *) in

(* Upload with chunked upload *)
Twitter.upload_media_chunked
  ~access_token:"your_access_token"
  ~media_data:video_data
  ~mime_type:"video/mp4"
  ~alt_text:(Some "Video description for screen readers")
  ()
  (fun media_id ->
    (* Now use media_id in a tweet *)
    Twitter.post_single_with_media_ids
      ~account_id:"my_account"
      ~text:"Check out my video!"
      ~media_ids:[media_id]
      (fun tweet_id -> Printf.printf "Posted: %s\n" tweet_id)
      on_error)
  on_error
```

**Note**: Chunked upload uses INIT/APPEND/FINALIZE workflow and splits files into 5MB chunks.

### 5. Pagination

Use pagination for large result sets:

```ocaml
let rec fetch_all_tweets ?(next_token=None) acc =
  Twitter.search_tweets
    ~account_id:"my_account"
    ~query:"#OCaml"
    ~max_results:100
    ~next_token
    ()
    (fun json ->
      let open Yojson.Basic.Util in
      (* Get tweets from this page *)
      let tweets = json |> member "data" |> to_list in
      let all_tweets = acc @ tweets in
      
      (* Check for more pages *)
      let meta = Twitter.parse_pagination_meta json in
      match meta.next_token with
      | Some token ->
          Printf.printf "Fetched %d tweets, getting more...\n" 
            (List.length all_tweets);
          fetch_all_tweets ~next_token:(Some token) all_tweets
      | None ->
          Printf.printf "Done! Total: %d tweets\n" (List.length all_tweets))
    (fun error -> Printf.eprintf "Error: %s\n" error)
in
fetch_all_tweets []
```

### 6. Rate Limit Parsing

The library now parses rate limit headers automatically:

```ocaml
(* Rate limit info is available in response headers *)
let parse_and_handle_rate_limit headers =
  match Twitter.parse_rate_limit_headers headers with
  | Some info ->
      Printf.printf "Rate limit: %d/%d remaining\n" 
        info.remaining info.limit;
      Printf.printf "Resets at: %d\n" info.reset;
      
      if info.remaining < 10 then
        print_endline "WARNING: Rate limit nearly exhausted!"
  | None ->
      print_endline "No rate limit info in headers"
```

## Migration Checklist

### For Existing Code

- [ ] Review `post_thread` usage - now posts all tweets
- [ ] Consider using new READ operations for better functionality
- [ ] Add expansions/fields to optimize API calls
- [ ] Implement pagination for large result sets
- [ ] Switch to chunked upload for videos > 5MB
- [ ] Add error handling for new operations

### For New Features

- [ ] Explore user operations (follow, block)
- [ ] Add engagement features (like, retweet)
- [ ] Use bookmarks for saving tweets
- [ ] Implement quote tweets and replies
- [ ] Parse rate limit headers for better resource management

## Testing

Update your tests to cover new functionality:

```ocaml
(* Test new features *)
let test_get_tweet () =
  Twitter.get_tweet
    ~account_id:"test"
    ~tweet_id:"123"
    ()
    (fun json -> assert (json <> `Null))
    (fun error -> failwith error)

let test_delete_tweet () =
  Twitter.delete_tweet
    ~account_id:"test"
    ~tweet_id:"123"
    (fun () -> print_endline "✓ Delete works")
    (fun error -> failwith error)

let test_pagination () =
  Twitter.search_tweets
    ~account_id:"test"
    ~query:"test"
    ()
    (fun json ->
      let meta = Twitter.parse_pagination_meta json in
      assert (meta.result_count >= 0))
    (fun error -> failwith error)
```

## Performance Considerations

### Expansions & Fields

Use expansions and fields to reduce API calls:

```ocaml
(* Instead of multiple API calls *)
Twitter.get_tweet ~account_id ~tweet_id () on_success on_error;
Twitter.get_user_by_id ~account_id ~user_id () on_success on_error;

(* Use expansions to get everything in one call *)
Twitter.get_tweet
  ~account_id
  ~tweet_id
  ~expansions:["author_id"]
  ~user_fields:["username"; "verified"]
  ()
  (fun json ->
    (* Tweet and author data included *))
  on_error
```

### Pagination

Respect rate limits when paginating:

```ocaml
let rec fetch_with_delay ?(next_token=None) delay acc =
  (* Add delay between requests *)
  Unix.sleep delay;
  
  Twitter.search_tweets
    ~account_id:"my_account"
    ~query:"OCaml"
    ~next_token
    ()
    (fun json ->
      let meta = Twitter.parse_pagination_meta json in
      match meta.next_token with
      | Some token -> 
          fetch_with_delay ~next_token:(Some token) delay acc
      | None -> 
          (* Done *))
    on_error
in
fetch_with_delay 2 []  (* 2 second delay between requests *)
```

## Support

For issues or questions:
- Check the updated README.md
- Review FEATURE_COMPARISON.md for complete feature list
- See test/test_twitter.ml for usage examples

## Conclusion

The enhanced Twitter v2 package provides comprehensive coverage of the Twitter API v2 with 35+ new features while maintaining backward compatibility (except for `post_thread` behavior). The CPS architecture remains unchanged, ensuring your existing code continues to work.

Happy tweeting! 🐦
