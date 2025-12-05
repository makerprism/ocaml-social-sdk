# Twitter API v2 - Quick Reference Card

One-page reference for the most common operations.

## Setup

```ocaml
module Twitter = Social_twitter_v2.Twitter_v2.Make(My_config)
```

## Tweet Operations

| Operation | Function | Example |
|-----------|----------|---------|
| **Post tweet** | `post_single` | `Twitter.post_single ~account_id ~text ~media_urls on_success on_error` |
| **Delete tweet** | `delete_tweet` | `Twitter.delete_tweet ~account_id ~tweet_id on_success on_error` |
| **Get tweet** | `get_tweet` | `Twitter.get_tweet ~account_id ~tweet_id ~expansions ~tweet_fields () on_success on_error` |
| **Search tweets** | `search_tweets` | `Twitter.search_tweets ~account_id ~query ~max_results ~next_token () on_success on_error` |
| **Post thread** | `post_thread` | `Twitter.post_thread ~account_id ~texts ~media_urls_per_post on_success on_error` |
| **Reply** | `reply_to_tweet` | `Twitter.reply_to_tweet ~account_id ~text ~reply_to_tweet_id ~media_urls on_success on_error` |
| **Quote** | `quote_tweet` | `Twitter.quote_tweet ~account_id ~text ~quoted_tweet_id ~media_urls on_success on_error` |
| **Timeline** | `get_user_timeline` | `Twitter.get_user_timeline ~account_id ~user_id ~max_results () on_success on_error` |

## User Operations

| Operation | Function | Example |
|-----------|----------|---------|
| **Get user (ID)** | `get_user_by_id` | `Twitter.get_user_by_id ~account_id ~user_id ~user_fields () on_success on_error` |
| **Get user (name)** | `get_user_by_username` | `Twitter.get_user_by_username ~account_id ~username () on_success on_error` |
| **Get me** | `get_me` | `Twitter.get_me ~account_id () on_success on_error` |
| **Follow** | `follow_user` | `Twitter.follow_user ~account_id ~target_user_id on_success on_error` |
| **Unfollow** | `unfollow_user` | `Twitter.unfollow_user ~account_id ~target_user_id on_success on_error` |
| **Block** | `block_user` | `Twitter.block_user ~account_id ~target_user_id on_success on_error` |
| **Unblock** | `unblock_user` | `Twitter.unblock_user ~account_id ~target_user_id on_success on_error` |

## Engagement Operations

| Operation | Function | Example |
|-----------|----------|---------|
| **Like** | `like_tweet` | `Twitter.like_tweet ~account_id ~tweet_id on_success on_error` |
| **Unlike** | `unlike_tweet` | `Twitter.unlike_tweet ~account_id ~tweet_id on_success on_error` |
| **Retweet** | `retweet` | `Twitter.retweet ~account_id ~tweet_id on_success on_error` |
| **Unretweet** | `unretweet` | `Twitter.unretweet ~account_id ~tweet_id on_success on_error` |
| **Bookmark** | `bookmark_tweet` | `Twitter.bookmark_tweet ~account_id ~tweet_id on_success on_error` |
| **Remove bookmark** | `remove_bookmark` | `Twitter.remove_bookmark ~account_id ~tweet_id on_success on_error` |

## Media Upload

| Operation | Function | Example |
|-----------|----------|---------|
| **Simple upload** | `upload_media` | `Twitter.upload_media ~access_token ~media_data ~mime_type on_success on_error` |
| **Chunked upload** | `upload_media_chunked` | `Twitter.upload_media_chunked ~access_token ~media_data ~mime_type ~alt_text () on_success on_error` |

## OAuth

| Operation | Function | Example |
|-----------|----------|---------|
| **Get auth URL** | `get_oauth_url` | `Twitter.get_oauth_url ~state ~code_verifier` |
| **Exchange code** | `exchange_code` | `Twitter.exchange_code ~code ~code_verifier on_success on_error` |

## Common Patterns

### Post with Media

```ocaml
Twitter.post_single
  ~account_id:"account"
  ~text:"Check this out!"
  ~media_urls:["https://example.com/image.jpg"]
  (fun tweet_id -> Printf.printf "Posted: %s\n" tweet_id)
  (fun error -> Printf.eprintf "Error: %s\n" error)
```

### Search with Pagination

```ocaml
Twitter.search_tweets
  ~account_id:"account"
  ~query:"#OCaml"
  ~max_results:100
  ~next_token:(Some "token")
  ~expansions:["author_id"]
  ~tweet_fields:["created_at"; "public_metrics"]
  ()
  (fun json ->
    let meta = Twitter.parse_pagination_meta json in
    match meta.next_token with
    | Some token -> (* fetch next page *)
    | None -> (* done *))
  on_error
```

### Post Thread

```ocaml
Twitter.post_thread
  ~account_id:"account"
  ~texts:["Tweet 1"; "Tweet 2"; "Tweet 3"]
  ~media_urls_per_post:[["url1.jpg"]; []; ["url3.jpg"]]
  (fun tweet_ids -> 
    Printf.printf "Posted %d tweets\n" (List.length tweet_ids))
  on_error
```

### Engage with Tweet

```ocaml
(* Like and retweet *)
Twitter.like_tweet ~account_id ~tweet_id
  (fun () ->
    Twitter.retweet ~account_id ~tweet_id
      (fun () -> print_endline "Liked and retweeted!")
      on_error)
  on_error
```

## Common Parameters

### Expansions
```ocaml
~expansions:[
  "author_id";
  "referenced_tweets.id";
  "referenced_tweets.id.author_id";
  "in_reply_to_user_id";
  "attachments.media_keys";
  "geo.place_id";
]
```

### Tweet Fields
```ocaml
~tweet_fields:[
  "created_at";
  "public_metrics";
  "author_id";
  "conversation_id";
  "entities";
  "referenced_tweets";
]
```

### User Fields
```ocaml
~user_fields:[
  "created_at";
  "description";
  "public_metrics";
  "verified";
  "profile_image_url";
  "url";
]
```

## Media Limits

| Type | Max Size | Max Duration |
|------|----------|--------------|
| Image | 5 MB | N/A |
| GIF | 15 MB | N/A |
| Video | 512 MB | 140 seconds |

## Rate Limits (Free Tier)

- **Tweets**: 15 posts per 24 hours
- **Reads**: Varies by endpoint (check headers)

## Parse Response Helpers

```ocaml
(* Pagination *)
let meta = Twitter.parse_pagination_meta json in
meta.next_token      (* string option *)
meta.previous_token  (* string option *)
meta.result_count    (* int *)

(* Rate limits *)
let info = Twitter.parse_rate_limit_headers headers in
info.limit      (* int *)
info.remaining  (* int *)
info.reset      (* int - Unix timestamp *)
```

## Error Handling Pattern

```ocaml
let on_success result =
  (* Handle success *)
  Printf.printf "Success: %s\n" result

let on_error error =
  (* Handle error *)
  Printf.eprintf "Error: %s\n" error;
  (* Optionally retry *)
  if String.contains error '4' && String.contains error '2' && String.contains error '9' then
    Printf.eprintf "Rate limited!\n"
```

## Tips

1. **Always use expansions** to reduce API calls
2. **Parse rate limit headers** to avoid hitting limits
3. **Add delays** between paginated requests (1-2 seconds)
4. **Use chunked upload** for videos > 5MB
5. **Validate content** before posting (280 char limit)
6. **Handle errors gracefully** with retry logic
7. **Use field selection** to optimize response size

## Links

- Full docs: `README.md`
- Examples: `EXAMPLES.md`
- Migration guide: `MIGRATION_GUIDE.md`
- Feature comparison: `FEATURE_COMPARISON.md`
