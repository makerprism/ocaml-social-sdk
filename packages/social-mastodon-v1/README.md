# social-mastodon-v1

Mastodon API client for OCaml with OAuth 2.0 support.

> **Status:** This library was primarily built using LLMs. OAuth 2.0 and posting functionality have been used successfully. Read operations and other features should be considered untested. Expect breaking changes as we work towards stability.

## Features

### Implemented ✅

#### Authentication
- OAuth 2.0 app registration
- Authorization URL generation
- Token exchange from authorization code
- Credential verification

#### Status Operations
- **Post statuses** with full options:
  - Text content
  - Media attachments (images, videos, GIFs)
  - Visibility levels (public, unlisted, private, direct)
  - Content warnings / spoiler text
  - Sensitive media flag
  - Language specification
  - In-reply-to for threading
  - Polls (multiple choice, expiration, hidden totals)
  - Scheduled publishing
  - Idempotency keys
- **Thread posting** with media support
- **Edit statuses** (Mastodon 3.5.0+)
- **Delete statuses**

#### Interactions
- Favorite / unfavorite statuses
- Boost (reblog) / unboost statuses with visibility control
- Bookmark / unbookmark statuses

#### Media
- Upload images, videos, and GIFs
- Media descriptions (alt text)
- Focus points for cropping
- Update media after upload

#### Validation
- Content length validation (configurable)
- Media size and format validation
- Poll validation

### Visibility Levels

```ocaml
type visibility = 
  | Public      (* Visible to everyone, shown in public timelines *)
  | Unlisted    (* Visible to everyone, but not in public timelines *)
  | Private     (* Visible to followers only *)
  | Direct      (* Visible to mentioned users only *)
```

### Poll Support

```ocaml
type poll = {
  options: poll_option list;      (* 2-4 options *)
  expires_in: int;                (* Duration in seconds *)
  multiple: bool;                 (* Allow multiple choices *)
  hide_totals: bool;              (* Hide vote counts until end *)
}
```

## Usage Examples

### OAuth Flow

```ocaml
(* 1. Register your app *)
Mastodon.register_app
  ~instance_url:"https://mastodon.social"
  ~client_name:"My App"
  ~redirect_uris:"urn:ietf:wg:oauth:2.0:oob"
  ~scopes:"read write follow"
  ~website:"https://myapp.example"
  (fun (client_id, client_secret) ->
    (* Save client_id and client_secret *)
    
    (* 2. Get authorization URL *)
    let auth_url = Mastodon.get_oauth_url
      ~instance_url:"https://mastodon.social"
      ~client_id
      ~redirect_uri:"urn:ietf:wg:oauth:2.0:oob"
      ~scopes:"read write follow"
      () in
    
    (* Direct user to auth_url, they get a code *)
    
    (* 3. Exchange code for token *)
    Mastodon.exchange_code
      ~instance_url:"https://mastodon.social"
      ~client_id
      ~client_secret
      ~redirect_uri:"urn:ietf:wg:oauth:2.0:oob"
      ~code:"USER_CODE_HERE"
      (fun credentials ->
        (* Save credentials for future use *)
        ())
      on_error)
  on_error
```

### Post a Simple Status

```ocaml
Mastodon.post_single
  ~account_id:"user_123"
  ~text:"Hello from OCaml! 👋"
  ~media_urls:[]
  (fun status_id -> 
    Printf.printf "Posted: %s\n" status_id)
  (fun err -> 
    Printf.eprintf "Error: %s\n" err)
```

### Post with Options

```ocaml
Mastodon.post_single
  ~account_id:"user_123"
  ~text:"Sensitive content behind a warning"
  ~media_urls:["https://example.com/image.jpg"]
  ~visibility:Unlisted
  ~sensitive:true
  ~spoiler_text:(Some "Click to reveal")
  ~language:(Some "en")
  (fun status_id -> ...)
  on_error
```

### Post a Poll

```ocaml
let poll = {
  Mastodon_v1.options = [
    {title = "Option A"};
    {title = "Option B"};
    {title = "Option C"};
  ];
  expires_in = 86400;  (* 24 hours *)
  multiple = false;
  hide_totals = false;
} in

Mastodon.post_single
  ~account_id:"user_123"
  ~text:"What's your favorite?"
  ~media_urls:[]
  ~poll:(Some poll)
  on_success
  on_error
```

### Post a Thread

```ocaml
Mastodon.post_thread
  ~account_id:"user_123"
  ~texts:[
    "This is the first post in a thread";
    "This is the second post";
    "And this is the third post with an image";
  ]
  ~media_urls_per_post:[
    [];  (* No media in first post *)
    [];  (* No media in second post *)
    ["https://example.com/image.jpg"];  (* Image in third post *)
  ]
  ~visibility:Public
  (fun status_ids ->
    Printf.printf "Posted thread with %d statuses\n" (List.length status_ids))
  on_error
```

### Edit a Status

```ocaml
Mastodon.edit_status
  ~account_id:"user_123"
  ~status_id:"123456"
  ~text:"Updated content"
  ~visibility:(Some Unlisted)
  (fun edited_id ->
    Printf.printf "Edited: %s\n" edited_id)
  on_error
```

### Delete a Status

```ocaml
Mastodon.delete_status
  ~account_id:"user_123"
  ~status_id:"123456"
  (fun () -> Printf.printf "Deleted!\n")
  on_error
```

### Favorite, Boost, and Bookmark

```ocaml
(* Favorite *)
Mastodon.favorite_status ~account_id ~status_id on_success on_error

(* Boost with custom visibility *)
Mastodon.boost_status ~account_id ~status_id 
  ~visibility:(Some Unlisted) on_success on_error

(* Bookmark *)
Mastodon.bookmark_status ~account_id ~status_id on_success on_error
```

## Important Notes

### Instance URL Storage

Currently, the instance URL is stored in the `expires_at` field of the credentials for backward compatibility. This is a temporary solution and will be migrated to proper Mastodon-specific credentials storage in the future.

### Character Limits

The default character limit is 500, but many Mastodon instances allow 1000, 5000, or more characters. The actual limit should be fetched from the `/api/v1/instance` endpoint (not yet implemented).

### Media Limits

Default media limits are:
- Images: 10MB
- Videos: 100MB, max 2 hours
- GIFs: 10MB

Actual limits vary by instance and should be fetched from the instance configuration.

### Token Expiration

Mastodon tokens typically don't expire unless revoked. The package currently doesn't implement token refresh, as it's not needed for most Mastodon instances.

## Not Yet Implemented

- Fetch instance configuration
- Timeline reading
- Account operations (follow, unfollow, etc.)
- Notifications
- Search
- Streaming API
- Filters
- Lists
- Direct messages
- And many more...

See the [full Mastodon API documentation](https://docs.joinmastodon.org/methods/) for complete API coverage.

## Status

🚧 Active development - Core features implemented, many advanced features pending

## License

MIT
