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
      (fun err -> Printf.eprintf "Error: %s\n" err))
  (fun err -> Printf.eprintf "Error: %s\n" err)
```

### Post a Simple Status

```ocaml
Mastodon.post_single
  ~account_id:"user_123"
  ~text:"Hello from OCaml! 👋"
  ~media_urls:[]
  (function
    | Social_core.Error_types.Success status_id -> 
        Printf.printf "Posted: %s\n" status_id
    | Social_core.Error_types.Partial_success { result = status_id; warnings } ->
        Printf.printf "Posted: %s with %d warnings\n" status_id (List.length warnings)
    | Social_core.Error_types.Failure err ->
        Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))
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
  (function
    | Social_core.Error_types.Success status_id -> 
        Printf.printf "Posted: %s\n" status_id
    | Social_core.Error_types.Partial_success { result = status_id; _ } ->
        Printf.printf "Posted with warnings: %s\n" status_id
    | Social_core.Error_types.Failure err ->
        Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))
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
  (function
    | Social_core.Error_types.Success status_id -> 
        Printf.printf "Poll posted: %s\n" status_id
    | Social_core.Error_types.Partial_success { result = status_id; warnings } ->
        Printf.printf "Poll posted: %s (with %d warnings)\n" status_id (List.length warnings)
    | Social_core.Error_types.Failure err ->
        Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))
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
  (function
    | Social_core.Error_types.Success result ->
        Printf.printf "Posted thread with %d statuses\n" (List.length result.posted_ids)
    | Social_core.Error_types.Partial_success { result; _ } ->
        Printf.printf "Posted %d/%d statuses\n" 
          (List.length result.posted_ids) result.total_requested
    | Social_core.Error_types.Failure err ->
        Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### Edit a Status

```ocaml
Mastodon.edit_status
  ~account_id:"user_123"
  ~status_id:"123456"
  ~text:"Updated content"
  ~visibility:(Some Unlisted)
  (function
    | Ok edited_id -> Printf.printf "Edited: %s\n" edited_id
    | Error err -> 
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### Delete a Status

```ocaml
Mastodon.delete_status
  ~account_id:"user_123"
  ~status_id:"123456"
  (function
    | Social_core.Error_types.Success () -> Printf.printf "Deleted!\n"
    | Social_core.Error_types.Partial_success _ -> Printf.printf "Deleted!\n"
    | Social_core.Error_types.Failure err ->
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### Favorite, Boost, and Bookmark

```ocaml
(* Favorite *)
Mastodon.favorite_status ~account_id ~status_id
  (function
    | Ok () -> Printf.printf "Favorited!\n"
    | Error err -> 
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Boost with custom visibility *)
Mastodon.boost_status ~account_id ~status_id ~visibility:(Some Unlisted)
  (function
    | Ok () -> Printf.printf "Boosted!\n"
    | Error err -> 
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))

(* Bookmark *)
Mastodon.bookmark_status ~account_id ~status_id
  (function
    | Ok () -> Printf.printf "Bookmarked!\n"
    | Error err -> 
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))
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

## OAuth & Required Scopes

**Always request the correct scopes during app registration and authorization** - API calls will fail if you don't have the required permissions.

### OAuth Details

| Property | Value |
|----------|-------|
| PKCE | Supported (optional) |
| Token lifetime | Never expires (until revoked) |
| Refresh tokens | Not needed |
| App registration | Required per-instance |

### Required Scopes by Operation

| Operation | Required Scopes |
|-----------|-----------------|
| Read profile/posts | `read` |
| Post status | `read`, `write` |
| Post with media | `read`, `write` |
| Edit status | `read`, `write` |
| Delete status | `read`, `write` |
| Favorite/unfavorite | `read`, `write` |
| Boost/unboost | `read`, `write` |
| Bookmark | `read`, `write` |
| Follow accounts | `read`, `write`, `follow` |
| Push notifications | `push` |

### Using Scope Helpers

```ocaml
open Social_mastodon_v1.Mastodon_v1

(* Predefined scope sets *)
let read_scopes = OAuth.Scopes.read    (* ["read"] *)
let write_scopes = OAuth.Scopes.write  (* ["read"; "write"; "follow"] *)

(* Generate auth URL with correct scopes *)
let auth_url = OAuth.get_authorization_url
  ~instance_url:"https://mastodon.social"
  ~client_id:"your_client_id"
  ~redirect_uri:"https://your-app.com/callback"
  ~scopes:"read write follow"
  ()
```

### OAuth Metadata

```ocaml
let () =
  let open Social_mastodon_v1.Mastodon_v1.OAuth.Metadata in
  Printf.printf "PKCE supported: %b\n" supports_pkce;       (* true *)
  Printf.printf "Refresh supported: %b\n" supports_refresh; (* false *)
  Printf.printf "Token lifetime: %s\n" 
    (match token_lifetime_seconds with 
     | Some s -> Printf.sprintf "%d seconds" s 
     | None -> "never expires")                             (* never expires *)
```

### Read: Home Timeline and Status Lookup

```ocaml
Mastodon.get_home_timeline
  ~account_id:"mastodon-account"
  ~limit:20
  (function
    | Ok statuses -> Printf.printf "Fetched %d statuses\n" (List.length statuses)
    | Error err -> Printf.printf "Error: %s\n" (Error_types.error_to_string err)
  )

Mastodon.get_status
  ~account_id:"mastodon-account"
  ~status_id:"109999999999999999"
  (function
    | Ok status -> Printf.printf "Status %s: %s\n" status.id status.content
    | Error err -> Printf.printf "Error: %s\n" (Error_types.error_to_string err)
  )

Mastodon.search_accounts
  ~account_id:"mastodon-account"
  ~query:"ocaml"
  ~limit:10
  (function
    | Ok accounts -> Printf.printf "Found %d accounts\n" (List.length accounts)
    | Error err -> Printf.printf "Error: %s\n" (Error_types.error_to_string err)
  )

Mastodon.get_notifications
  ~account_id:"mastodon-account"
  ~limit:20
  ~exclude_types:["follow"]
  (function
    | Ok notifications -> Printf.printf "Fetched %d notifications\n" (List.length notifications)
    | Error err -> Printf.printf "Error: %s\n" (Error_types.error_to_string err)
  )

Mastodon.get_instance_limits
  ~account_id:"mastodon-account"
  (function
    | Ok limits ->
        Printf.printf "max chars: %d, max media: %d\n" limits.max_status_chars limits.max_media_attachments
    | Error err -> Printf.printf "Error: %s\n" (Error_types.error_to_string err)
  )
```

### Read: Threads, Notifications and WebFinger (typed)

`Social_mastodon_v1.Entities` holds the typed Status, Context, Notification
and WebFinger entities. `Reads.Make (Http)` calls them with a token you hold
(or none, for public data); `Make` has the same calls for a stored account.

```ocaml
module Reads = Social_mastodon_v1.Reads.Make (Social_lwt.Cohttp_client.Make)
module E = Social_mastodon_v1.Entities

(* Replies under a status, rebuilt as a tree with in_reply_to_id *)
Reads.get_status_context ~instance_url:"https://mastodon.social"
  ~access_token ~status_id:"109999999999999999"
  (function
    | Ok { E.descendants; _ } ->
        List.iter
          (fun (s : E.status) ->
            Printf.printf "%s replied to %s: %s\n" s.account.acct
              (Option.value s.in_reply_to_id ~default:"?")
              (E.html_to_plain_text s.content))
          descendants
    | Error err -> prerr_endline (Error_types.error_to_string err))

(* New mentions since the last seen notification *)
Reads.get_notifications ~instance_url ~access_token ~types:[ E.Mention ]
  ~min_id:last_seen_id
  (function
    | Ok page -> (* page.notifications, page.cursors.prev_min_id *) ()
    | Error _ -> ())

(* Does alice@example.social exist? Vet the domain before calling. *)
Reads.webfinger ~username:"alice" ~domain:"example.social" (function
  | Ok jrd -> ignore (E.webfinger_actor_url jrd)
  | Error (Error_types.Resource_not_found _) -> (* no such account *) ()
  | Error _ -> ())
```

## Not Yet Implemented

- Timeline reading (partial; home/public/tag timelines and single status lookup are implemented)
- Account operations (partial; follow/unfollow/block/unblock/mute/unmute, follow request list/approve/reject, account read operations including followers/following, account lookup by acct, and featured tags reads are implemented)
- Notifications (partial; fetch/list with type filters and `Link` cursors, and dismiss/clear are implemented; grouped notifications (`/api/v2/notifications`) are not)
- Search (partial; status/account/hashtag search and combined search payload parsing are implemented)
- Streaming API
- Filters (partial; list/create/update/delete via `/api/v2/filters` are implemented)
- Lists (partial; list enumeration/timeline, list members/account-lists reads, and list management operations are implemented)
- Direct messages (partial; conversation list/read/remove are implemented via `/api/v1/conversations`)
- And many more...

See the [full Mastodon API documentation](https://docs.joinmastodon.org/methods/) for complete API coverage.

## Integration Note

If you instantiate `Social_mastodon_v1.Make` with a custom config module, implement:

```ocaml
val sleep : seconds:float -> (unit -> unit) -> (string -> unit) -> unit
```

This is used by media processing polling to avoid hardcoded runtime assumptions.

## Status

🚧 Active development - Core features implemented, many advanced features pending

## License

MIT

WebFinger validates host inputs with `Social_public_host.is_public_host`. The
HTTP adapter must also reject private resolved addresses and unsafe redirects
to prevent DNS rebinding. The helper performs no DNS or runtime-specific I/O.

### Replies with exact text

`Make.reply_to_status_exact ~account_id ~in_reply_to_id ~text ?visibility
?idempotency_key callback` returns `Entities.status Error_types.api_result`,
including `id`, nullable `url`, `uri`, and `created_at`. It sends the text
unchanged and uses the local target status ID on the posting instance.
Visibility defaults to `Public`; pass a stable idempotency key for retries
(or omit it to generate a fresh UUID). The existing `reply_to_status` keeps
its automatic mention behavior.
