# Twitter API v1.1 Provider for OCaml

**OAuth 1.0a Authentication & Legacy API Support**

This package provides OCaml bindings for Twitter API v1.1 with a focus on features not available in v2:
- OAuth 1.0a signature-based authentication
- Streaming API (real-time tweet streams)
- Collections API (curated tweet collections)
- Saved Searches API
- Enhanced media upload with chunking
- Geo API and oEmbed endpoints

## When to Use This Package

### Use Twitter v1.1 (`social-twitter-v1`) When:

1. **OAuth 1.0a Required**: Your app needs OAuth 1.0a authentication (signature-based)
2. **Streaming API**: You need real-time streaming of tweets (`statuses/filter`, `statuses/sample`)
3. **Collections**: You're building curated tweet collections
4. **Legacy Integrations**: Existing system requires v1.1 compatibility
5. **V2 Fallback**: When certain v2 features aren't working as expected

### Use Twitter v2 (`social-twitter-v2`) When:

- You need modern features (Spaces, Communities, bookmarks, polls)
- OAuth 2.0 is acceptable (easier to implement)
- You want comprehensive API coverage (70% of all endpoints)
- You need better rate limits and performance
- **This is the recommended default choice**

## Installation

```bash
opam install social-twitter-v1
```

Or add to your `dune-project`:

```lisp
(depends
  ...
  social-twitter-v1)
```

## Features

### OAuth 1.0a Authentication
- HMAC-SHA1 signature generation
- Automatic nonce and timestamp creation
- Compliant with OAuth 1.0a specification

### Streaming API
- **Filter Stream**: Track keywords in real-time
- **Sample Stream**: 1% random sample of all tweets

### Collections API
- Create curated collections
- Add/remove tweets from collections
- Manage collection metadata

### Media Upload
- **Chunked Upload**: Upload large files (videos, images)
  - INIT phase: Initialize upload
  - APPEND phase: Upload chunks
  - FINALIZE phase: Complete upload
  - STATUS: Check processing status
- Automatic chunking with `upload_media_chunked` helper

### Additional APIs
- **Saved Searches**: Create and manage saved searches
- **oEmbed**: Get embeddable HTML for tweets
- **Geo API**: Reverse geocoding for coordinates

## Usage

### Basic Setup

```ocaml
open Social_provider_core

(* Configure the provider *)
module Config = struct
  module Http = My_http_client  (* Your HTTP client implementation *)
  
  let get_env key = 
    (* Return environment variables *)
    Unix.getenv_opt key
  
  let get_credentials ~account_id on_success on_error =
    (* Fetch OAuth 1.0a credentials from your storage *)
    (* Note: refresh_token field should contain token_secret *)
    on_success {
      access_token = "oauth_token";
      refresh_token = Some "oauth_token_secret";
      expires_at = None;  (* OAuth 1.0a tokens don't expire *)
    }
  
  (* Implement other required config functions... *)
end

module Twitter = Social_twitter_v1.Make(Config)
```

### Streaming API

```ocaml
(* Track keywords in real-time *)
Twitter.stream_filter
  ~account_id:"user123"
  ~track:["OCaml"; "functional programming"; "#FP"]
  ~on_tweet:(fun tweet_json ->
    Printf.printf "New tweet: %s\n" tweet_json)
  ~on_error:(fun error ->
    Printf.eprintf "Stream error: %s\n" error)

(* Random 1% sample stream *)
Twitter.stream_sample
  ~account_id:"user123"
  ~on_tweet:(fun tweet_json ->
    Printf.printf "Sample tweet: %s\n" tweet_json)
  ~on_error:(fun error ->
    Printf.eprintf "Stream error: %s\n" error)
```

**Note**: The current implementation provides basic streaming support. For production use, you'll need a streaming HTTP client that can handle newline-delimited JSON.

### Collections API

```ocaml
(* Create a collection *)
Twitter.create_collection
  ~account_id:"user123"
  ~name:"Best OCaml Tweets"
  ~description:(Some "Curated collection of OCaml wisdom")
  ~url:(Some "https://example.com/ocaml")
  (fun json ->
    let open Yojson.Basic.Util in
    let collection_id = json
      |> member "response"
      |> member "timeline_id"
      |> to_string in
    Printf.printf "Created collection: %s\n" collection_id)
  (fun error ->
    Printf.eprintf "Error: %s\n" error)

(* Add tweet to collection *)
Twitter.add_to_collection
  ~account_id:"user123"
  ~collection_id:"custom-123456"
  ~tweet_id:"987654321"
  (fun () -> Printf.printf "Tweet added!\n")
  (fun error -> Printf.eprintf "Error: %s\n" error)
```

### Chunked Media Upload

```ocaml
(* Simple helper - automatically chunks and uploads *)
let video_data = (* Read your video file *) in

Twitter.upload_media_chunked
  ~account_id:"user123"
  ~media_data:video_data
  ~media_type:"video/mp4"
  ~chunk_size:5_000_000  (* 5MB chunks *)
  ()
  (fun (media_id, processing_info) ->
    match processing_info with
    | Some json ->
        (* Video requires async processing *)
        Printf.printf "Media uploaded: %s (processing...)\n" media_id;
        (* Poll STATUS endpoint until processing completes *)
    | None ->
        (* Media ready immediately *)
        Printf.printf "Media uploaded: %s (ready!)\n" media_id)
  (fun error ->
    Printf.eprintf "Upload failed: %s\n" error)
```

### Manual Chunked Upload

For more control over the upload process:

```ocaml
(* Step 1: Initialize *)
Twitter.upload_media_init
  ~account_id:"user123"
  ~total_bytes:(String.length video_data)
  ~media_type:"video/mp4"
  (fun media_id ->
    (* Step 2: Upload chunks *)
    let chunk = String.sub video_data 0 5000000 in
    Twitter.upload_media_append
      ~account_id:"user123"
      ~media_id
      ~media_data:chunk
      ~segment_index:0
      (fun () ->
        (* Step 3: Finalize *)
        Twitter.upload_media_finalize
          ~account_id:"user123"
          ~media_id
          (fun (json, processing) ->
            Printf.printf "Upload complete!\n")
          (fun error -> Printf.eprintf "Error: %s\n" error))
      (fun error -> Printf.eprintf "Error: %s\n" error))
  (fun error -> Printf.eprintf "Error: %s\n" error)
```

### Saved Searches

```ocaml
Twitter.create_saved_search
  ~account_id:"user123"
  ~query:"#OCaml lang:en"
  (fun json ->
    let open Yojson.Basic.Util in
    let search_id = json |> member "id_str" |> to_string in
    Printf.printf "Saved search: %s\n" search_id)
  (fun error -> Printf.eprintf "Error: %s\n" error)
```

### oEmbed (Embeddable Tweets)

```ocaml
Twitter.get_oembed
  ~tweet_id:"123456789"
  ~max_width:(Some 400)
  ~hide_media:false
  ()
  (fun json ->
    let open Yojson.Basic.Util in
    let html = json |> member "html" |> to_string in
    Printf.printf "Embed code: %s\n" html)
  (fun error -> Printf.eprintf "Error: %s\n" error)
```

### Geo API

```ocaml
Twitter.reverse_geocode
  ~lat:37.7821
  ~long:(-122.4093)
  ~granularity:"city"
  ()
  (fun json ->
    Printf.printf "Location: %s\n" (Yojson.Basic.to_string json))
  (fun error -> Printf.eprintf "Error: %s\n" error)
```

## OAuth 1.0a Setup

Twitter v1.1 uses OAuth 1.0a, which requires:

1. **Consumer Key** (API Key)
2. **Consumer Secret** (API Secret)
3. **Access Token** (oauth_token)
4. **Token Secret** (oauth_token_secret)

Set these in your environment:

```bash
export TWITTER_CONSUMER_KEY="your_consumer_key"
export TWITTER_CONSUMER_SECRET="your_consumer_secret"
```

Store the access token and token secret in your credentials storage:
- `access_token` = oauth_token
- `refresh_token` = oauth_token_secret (reused field)

## Architecture

This package follows the same Continuation-Passing Style (CPS) architecture as `social-twitter-v2`:

- **Runtime Agnostic**: Works with any OCaml runtime (Lwt, Async, native)
- **Callback-based**: All operations use `on_success` and `on_error` callbacks
- **Modular**: HTTP client is injected via functor
- **Type-safe**: Strong typing throughout the API

## API Coverage

### Implemented (v1.1-Specific Features)

- ✅ OAuth 1.0a authentication
- ✅ Streaming API (filter, sample)
- ✅ Collections API (create, add entries)
- ✅ Saved Searches API
- ✅ Chunked media upload (INIT, APPEND, FINALIZE, STATUS)
- ✅ oEmbed API
- ✅ Geo API (reverse geocoding)

### Not Implemented (Use v2 Instead)

- Tweet CRUD operations → Use `social-twitter-v2`
- User operations → Use `social-twitter-v2`
- Timeline operations → Use `social-twitter-v2`
- Lists API → Use `social-twitter-v2`
- Direct Messages → Use `social-twitter-v2`

**Recommendation**: Use this package alongside `social-twitter-v2` for comprehensive coverage.

## Comparison: v1.1 vs v2

| Feature | v1.1 | v2 | Recommendation |
|---------|------|----|--------------| 
| OAuth | 1.0a (HMAC-SHA1) | 2.0 (Bearer) | v2 (simpler) |
| Authentication | Signature required | Bearer token | v2 (easier) |
| Streaming | ✅ Real-time | ❌ Not available | v1.1 |
| Collections | ✅ Curated tweets | ❌ Not available | v1.1 |
| Saved Searches | ✅ | ❌ | v1.1 |
| Geo API | ✅ Places | ❌ Limited | v1.1 |
| Tweet CRUD | ✅ Basic | ✅ Enhanced | v2 |
| Polls | ❌ | ✅ | v2 |
| Spaces | ❌ | ✅ | v2 |
| Bookmarks | ❌ | ✅ | v2 |
| Lists | ✅ Basic | ✅ Complete | v2 |
| Rate Limits | Lower | Higher | v2 |
| Modern Features | ❌ | ✅ | v2 |

**Bottom Line**: Use v2 by default. Use v1.1 only for streaming, collections, or OAuth 1.0a requirements.

## Testing

Run tests:

```bash
dune test
```

The test suite includes:
- OAuth 1.0a signature generation
- Collections API (create, add)
- Saved searches
- Media upload (all phases)
- oEmbed and Geo APIs
- Streaming endpoints

## Performance Considerations

### Streaming API
- Streaming connections should be kept alive
- Implement reconnection logic for network interruptions
- Handle rate limit errors (420 status)

### Chunked Upload
- Default chunk size: 5MB
- Maximum file size: Varies by media type
- Videos require async processing (check STATUS)

### Rate Limits
- Streaming: Connection-based limits
- Media upload: 15 uploads per 15 minutes
- Collections: Varies by endpoint
- Check response headers for limit info

## Migration from v1.1-only Code

If you're currently using v1.1 for everything:

1. **Keep v1.1 for**: Streaming, Collections, Saved Searches
2. **Migrate to v2 for**: Tweets, Users, Timelines, Lists, DMs
3. **Both packages can coexist** in the same project
4. **Share credentials storage** between both providers

## Contributing

This package is part of the feedmansion.com social media automation platform.

Issues and pull requests welcome at: https://github.com/feedmansion/feedmansion.com

## License

[Add your license here]

## Related Packages

- **social-twitter-v2**: Modern Twitter API v2 client (recommended default)
- **social-provider-core**: Core types and interfaces
- **social-provider-lwt**: Lwt-based provider implementations

## Support

For questions about:
- **Which API to use**: See comparison table above
- **OAuth 1.0a setup**: Check Twitter Developer Portal
- **Streaming issues**: Ensure your HTTP client supports streaming responses
- **General help**: Open an issue on GitHub

---

**Recommendation**: Start with `social-twitter-v2`. Only use this package if you specifically need OAuth 1.0a, streaming, or collections.
