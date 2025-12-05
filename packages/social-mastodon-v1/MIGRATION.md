# Migration Guide: Upgrading to New Mastodon Package

This guide helps you upgrade from the previous version to the new, feature-complete Mastodon package.

## Breaking Changes

### 1. Thread Posting Media Parameter

**Before:**
```ocaml
Mastodon.post_thread
  ~account_id:"user_123"
  ~texts:["First"; "Second"; "Third"]
  ~media_urls_per_post:_  (* This was ignored! *)
  on_success
  on_error
```

**After:**
```ocaml
Mastodon.post_thread
  ~account_id:"user_123"
  ~texts:["First"; "Second"; "Third"]
  ~media_urls_per_post:[
    [];  (* No media in first post *)
    [];  (* No media in second post *)
    ["https://example.com/image.jpg"];  (* Image in third *)
  ]
  on_success
  on_error
```

**Migration:** You must now provide a list of media URL lists, one for each post in the thread. If you don't want media, pass empty lists.

### 2. OAuth Functions Now Work

**Before:**
```ocaml
(* This returned an error! *)
Mastodon.get_oauth_url ~state:_ ~code_verifier:_ =
  "Mastodon uses app tokens, not OAuth"
```

**After:**
```ocaml
(* Proper OAuth flow *)
let auth_url = Mastodon.get_oauth_url
  ~instance_url:"https://mastodon.social"
  ~client_id:"your_client_id"
  ~redirect_uri:"urn:ietf:wg:oauth:2.0:oob"
  ~scopes:"read write follow"
  () in
(* auth_url is now a real URL *)
```

**Migration:** If you were working around the OAuth errors, you can now use the proper OAuth flow.

### 3. Visibility Type Change

**Before:**
```ocaml
(* Visibility was hardcoded to "public" string internally *)
```

**After:**
```ocaml
(* Use the visibility type *)
open Social_mastodon_v1

Mastodon.post_single
  ~account_id:"user_123"
  ~text:"Test"
  ~media_urls:[]
  ~visibility:Unlisted  (* or Public, Private, Direct *)
  on_success
  on_error
```

**Migration:** Add `open Social_mastodon_v1` to access visibility types, or use fully qualified names like `Social_mastodon_v1.Public`.

## New Optional Parameters

All new parameters are optional, so existing code will continue to work. However, you can now use:

### post_single

```ocaml
Mastodon.post_single
  ~account_id:"user_123"
  ~text:"Test"
  ~media_urls:[]
  
  (* New optional parameters: *)
  ~visibility:Public              (* Default: Public *)
  ~sensitive:false                (* Default: false *)
  ~spoiler_text:(Some "Warning")  (* Default: None *)
  ~in_reply_to_id:(Some "12345")  (* Default: None *)
  ~language:(Some "en")            (* Default: None *)
  ~poll:(Some my_poll)            (* Default: None *)
  ~scheduled_at:(Some "2024-01-01T00:00:00Z")  (* Default: None *)
  ~idempotency_key:(Some "my-key") (* Default: auto-generated UUID *)
  
  on_success
  on_error
```

### post_thread

```ocaml
Mastodon.post_thread
  ~account_id:"user_123"
  ~texts:["First"; "Second"]
  ~media_urls_per_post:[[];[]]  (* Now required! *)
  
  (* New optional parameters: *)
  ~visibility:Public              (* Default: Public *)
  ~sensitive:false                (* Default: false *)
  ~spoiler_text:(Some "Warning")  (* Default: None *)
  
  on_success
  on_error
```

## New Features You Can Now Use

### 1. Delete Posts

```ocaml
Mastodon.delete_status
  ~account_id:"user_123"
  ~status_id:"54321"
  (fun () -> Printf.printf "Deleted!\n")
  on_error
```

### 2. Edit Posts

```ocaml
Mastodon.edit_status
  ~account_id:"user_123"
  ~status_id:"54321"
  ~text:"Updated content"
  ~visibility:(Some Unlisted)
  ~sensitive:(Some true)
  on_success
  on_error
```

### 3. Create Polls

```ocaml
open Social_mastodon_v1

let poll = {
  options = [
    {title = "Option A"};
    {title = "Option B"};
    {title = "Option C"};
  ];
  expires_in = 86400;  (* 24 hours in seconds *)
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

### 4. Schedule Posts

```ocaml
Mastodon.post_single
  ~account_id:"user_123"
  ~text:"This will be posted tomorrow"
  ~media_urls:[]
  ~scheduled_at:(Some "2024-12-01T12:00:00Z")
  on_success
  on_error
```

### 5. Interactions

```ocaml
(* Favorite *)
Mastodon.favorite_status 
  ~account_id:"user_123" 
  ~status_id:"54321"
  on_success 
  on_error

(* Boost with custom visibility *)
Mastodon.boost_status 
  ~account_id:"user_123" 
  ~status_id:"54321"
  ~visibility:(Some Unlisted)
  on_success 
  on_error

(* Bookmark *)
Mastodon.bookmark_status 
  ~account_id:"user_123" 
  ~status_id:"54321"
  on_success 
  on_error
```

### 6. Content Warnings

```ocaml
Mastodon.post_single
  ~account_id:"user_123"
  ~text:"Spoiler alert: Something happens!"
  ~media_urls:[]
  ~spoiler_text:(Some "Spoilers ahead!")
  ~sensitive:true
  on_success
  on_error
```

### 7. OAuth Registration

```ocaml
(* Step 1: Register your app *)
Mastodon.register_app
  ~instance_url:"https://mastodon.social"
  ~client_name:"My App"
  ~redirect_uris:"urn:ietf:wg:oauth:2.0:oob"
  ~scopes:"read write follow"
  ~website:"https://myapp.example"
  (fun (client_id, client_secret) ->
    (* Save these! *)
    
    (* Step 2: Get auth URL *)
    let auth_url = Mastodon.get_oauth_url
      ~instance_url:"https://mastodon.social"
      ~client_id
      ~redirect_uri:"urn:ietf:wg:oauth:2.0:oob"
      ~scopes:"read write follow"
      () in
    
    (* Direct user to auth_url, they paste code back *)
    
    (* Step 3: Exchange code for token *)
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

## Validation Changes

### validate_content

**Before:**
```ocaml
let result = Mastodon.validate_content ~text:"Hello" in
```

**After:**
```ocaml
(* Default 500 character limit *)
let result = Mastodon.validate_content ~text:"Hello" () in

(* Or custom limit *)
let result = Mastodon.validate_content ~text:"Hello" ~max_length:1000 () in
```

**Migration:** Add `()` unit parameter at the end if you're not using the max_length parameter.

## Instance URL Storage

The package now uses a dedicated `mastodon_credentials` type internally, but for backward compatibility, the instance URL is still stored in the `expires_at` field of the standard credentials type.

**No migration needed** - this is handled automatically.

If you want to access the instance URL:

```ocaml
(* The instance URL is in credentials.expires_at *)
Config.get_credentials ~account_id
  (fun credentials ->
    match credentials.expires_at with
    | Some url -> Printf.printf "Instance: %s\n" url
    | None -> Printf.printf "No instance URL\n")
  on_error
```

## Idempotency Keys

Previously, idempotency keys were random numbers (1-1000000), which didn't prevent duplicates.

Now they are proper UUIDs:
- Auto-generated for each request
- Properly prevent duplicate posts
- Can be overridden if needed

**No migration needed** - this is automatic and improves reliability.

## Error Messages

Error messages are now more detailed:

**Before:**
```
Mastodon API error (422)
```

**After:**
```
Mastodon API error (422): {"error":"Validation failed: Text can't be blank"}
```

**Migration:** Update any error parsing to handle the additional detail.

## Testing Your Migration

Use this checklist to verify your migration:

- [ ] Thread posting with media works (provide media_urls_per_post lists)
- [ ] OAuth flow works if you use it
- [ ] Visibility types compile (add `open Social_mastodon_v1` if needed)
- [ ] Existing posts still work without new parameters
- [ ] Error handling works with new detailed messages
- [ ] validate_content calls include unit parameter `()`

## Example: Complete Migration

**Before:**
```ocaml
(* Old code *)
Mastodon.post_single
  ~account_id:"user_123"
  ~text:"Hello Mastodon!"
  ~media_urls:[]
  (fun post_id -> Printf.printf "Posted: %s\n" post_id)
  (fun err -> Printf.eprintf "Error: %s\n" err)

Mastodon.post_thread
  ~account_id:"user_123"
  ~texts:["First"; "Second"]
  ~media_urls_per_post:_  (* Ignored anyway *)
  on_success
  on_error
```

**After:**
```ocaml
open Social_mastodon_v1

(* Simple post - still works the same *)
Mastodon.post_single
  ~account_id:"user_123"
  ~text:"Hello Mastodon!"
  ~media_urls:[]
  (fun post_id -> Printf.printf "Posted: %s\n" post_id)
  (fun err -> Printf.eprintf "Error: %s\n" err)

(* Thread - must provide media lists *)
Mastodon.post_thread
  ~account_id:"user_123"
  ~texts:["First"; "Second"]
  ~media_urls_per_post:[[];[]]  (* Empty lists for no media *)
  on_success
  on_error

(* Now you can also use new features! *)
Mastodon.post_single
  ~account_id:"user_123"
  ~text:"CW example"
  ~media_urls:[]
  ~visibility:Unlisted
  ~spoiler_text:(Some "Content warning")
  ~sensitive:true
  (fun post_id -> Printf.printf "Posted: %s\n" post_id)
  (fun err -> Printf.eprintf "Error: %s\n" err)
```

## Support

If you encounter migration issues:

1. Check the README.md for examples
2. Review IMPROVEMENTS.md for detailed changes
3. Look at test/test_mastodon_v1.ml for working examples
4. File an issue if you find bugs

## Deprecation Timeline

No features are deprecated. The old API still works with these exceptions:

- `media_urls_per_post` parameter must now be provided (was ignored before)
- OAuth functions now work (were broken before)
- `validate_content` needs unit parameter for optional args

There is no timeline for removing backward compatibility as the changes are minimal and well-tested.
