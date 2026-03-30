# social-facebook-graph-v21

OCaml library for Facebook Graph API v21 integration (Facebook Pages) with runtime-agnostic design.

> **Status:** This package has automated unit tests in this repository. Real Facebook app/page integration behavior should still be validated in your environment.

## Features

### Core Functionality
- **OAuth 2.0 Authentication**: Full OAuth flow for Facebook Pages
- **Page Posting**: Post text and images to Facebook Pages
- **Photo Upload**: Multipart upload support for images
- **Long-Lived Tokens**: OAuth code exchange supports short-lived -> long-lived token flow
- **Runtime Agnostic**: Works with Lwt, Eio, or synchronous runtimes via CPS

### Advanced Features
- **Pagination Support**: Cursor-based pagination for collections
- **Rate Limit Tracking**: Automatic parsing of `X-App-Usage` headers
- **Field Selection**: Optimize requests by specifying which fields to return
- **Typed Error Handling**: Structured error codes with retry recommendations
- **Batch Requests**: Combine up to 50 API calls into a single HTTP request
- **App Secret Proof**: HMAC-SHA256 signing for enhanced security
- **Generic API Methods**: `get`, `post`, `delete` for any Graph API endpoint

## Installation

### From source
```bash
cd packages/social-facebook-graph-v21
dune build
dune install
```

## Usage

### Basic Example

```ocaml
open Social_provider_core
open Social_facebook_graph_v21

module Config = struct
  module Http = Social_provider_lwt.Cohttp_client
  
  let get_env = Sys.getenv_opt
  let get_credentials ~account_id on_success on_error = (* ... *)
  let update_credentials ~account_id ~credentials on_success on_error = (* ... *)
  let get_page_id ~account_id on_success on_error = (* ... *)
  
  (* Rate limit tracking callback *)
  let on_rate_limit_update info =
    Printf.printf "Rate limit: %d calls, %.1f%% used\n" 
      info.call_count info.percentage_used
  
  (* ... other required functions *)
end

module Facebook = Facebook_graph_v21.Make(Config)

(* OAuth flow *)
let start_oauth () =
  Facebook.get_oauth_url 
    ~redirect_uri:"https://myapp.com/callback"
    ~state:"random_state_123"
    (fun url -> Printf.printf "Visit: %s\n" url; ())
    (fun err -> Printf.eprintf "Error: %s\n" err)

(* Exchange code for tokens *)
let complete_oauth code =
  Facebook.exchange_code 
    ~code 
    ~redirect_uri:"https://myapp.com/callback"
    (fun credentials ->
      Printf.printf "Got access token: %s\n" credentials.access_token;
      ())
    (fun err -> Printf.eprintf "Error: %s\n" err)

(* Post to Facebook Page *)
let post_to_page account_id =
  let text = "Hello from OCaml! 🚀" in
  let media_urls = ["https://example.com/image.jpg"] in
  
  Facebook.post_single ~account_id ~text ~media_urls
    (function
      | Social_core.Error_types.Success post_id ->
          Printf.printf "Posted successfully: %s\n" post_id
      | Social_core.Error_types.Partial_success { result = post_id; warnings } ->
          Printf.printf "Posted: %s with %d warnings\n" post_id (List.length warnings)
      | Social_core.Error_types.Failure err ->
          Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

## OAuth Scopes

Required Facebook permissions:

- `pages_read_engagement` - Read page insights
- `pages_manage_posts` - Create and manage posts (including videos and Reels)
- `pages_show_list` - List user's pages

## Configuration

Set these environment variables:

```bash
FACEBOOK_APP_ID=your_app_id
FACEBOOK_APP_SECRET=your_app_secret
FACEBOOK_REDIRECT_URI=https://yourapp.com/callback
```

## Platform Constraints

### Text
- Maximum length: 5,000 characters (recommended for engagement)
- Facebook technically supports up to ~63,000 characters

### Images
- Uploaded as unpublished photos, then attached to posts
- Multiple images supported via `attached_media` parameter
- Multipart/form-data upload

### Videos (Reels)
- Facebook Reels supported via `post_reel` function
- Uses three-step upload process: initialize, upload binary, publish
- Maximum duration: 90 seconds
- Recommended aspect ratio: 9:16 (vertical)
- Supported formats: MP4, MOV

### Threading
- Facebook doesn't support thread/chain posting
- Only single posts allowed

## Token Management

- **Long-lived user tokens** are exchanged from short-lived OAuth tokens
- **No refresh token flow** - Facebook OAuth for Pages does not provide refresh tokens
- **Page token recovery**: posting flows attempt one-time `/me/accounts` recovery on auth-like failures
- **Recovery fallback token storage**: the provider stores the user token in `credentials.refresh_token` as an internal fallback source for later page-token recovery attempts
- Use 24-hour buffer to warn users before expiry

### Credential Lifecycle

- **After OAuth onboarding** (`exchange_code_and_get_pages` + app persistence):
  - `credentials.access_token`: selected Page access token used for posting
  - `credentials.refresh_token`: selected long-lived user token (fallback source for future recovery)
- **After successful posting recovery**:
  - `credentials.access_token`: refreshed/recovered Page access token
  - `credentials.refresh_token`: preserved user token used for `/me/accounts` fallback

## API Reference

### High-Level Functions

#### `get_oauth_url`
Generate OAuth authorization URL.

#### `exchange_code`
Exchange authorization code for a long-lived user access token.

#### `exchange_code_and_get_pages`
Exchange code, get long-lived user token, and fetch managed Pages with Page access tokens.

#### `post_single`
Post to Facebook Page with optional images.

#### `post_thread`
Posts only first item (Facebook limitation).

#### `validate_content`
Validate post content length.

#### `upload_photo`
Upload photo to page (used internally).

#### `post_reel`
Post a Reel (short-form vertical video) to a Facebook Page.

```ocaml
Facebook.post_reel ~account_id ~text:"Check out this video!" 
  ~video_url:"https://example.com/video.mp4"
  (fun post_id -> Printf.printf "Reel posted: %s\n" post_id)
  (fun err -> Printf.eprintf "Error: %s\n" err)
```

### Generic API Methods

#### `get ~path ~access_token ?fields ?required_permissions`
Make a GET request to any Graph API endpoint.

You can pass `~required_permissions:[...]` to override default path-based permission mapping in typed permission errors.

#### `post ~path ~access_token ~params ?required_permissions`
Make a POST request to any Graph API endpoint.

Supports optional `~required_permissions:[...]` override for typed permission errors.

#### `delete ~path ~access_token ?required_permissions`
Make a DELETE request to any Graph API endpoint.

Supports optional `~required_permissions:[...]` override for typed permission errors.

#### `batch_request ~requests ~access_token ?required_permissions`
Execute up to 50 Graph API operations in one batch request.

Supports optional `~required_permissions:[...]` override for typed permission errors.

```ocaml
(* Get user info with specific fields *)
Facebook.get ~path:"me" ~access_token ~fields:["id"; "name"; "email"]
  (function
    | Ok response -> Printf.printf "Response: %s\n" response.body
    | Error err -> 
        Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

#### `get_page ~path ~access_token ?fields ?cursor parse_data`
Get a paginated collection with automatic cursor support.

```ocaml
let parse_posts json =
  let open Yojson.Basic.Util in
  json |> to_list |> List.map (fun post ->
    post |> member "message" |> to_string_option
  )
in

Facebook.get_page ~path:"me/posts" ~access_token parse_posts
  (function
    | Ok page ->
        List.iter (function
          | Some msg -> Printf.printf "Post: %s\n" msg
          | None -> ()
        ) page.data;
        
        (* Fetch next page if available *)
        (match page.paging with
        | Some cursors ->
            (match cursors.after with
             | Some cursor ->
                 Facebook.get_next_page ~path:"me/posts" ~access_token 
                   ~cursor parse_posts (function
                     | Ok next_page -> (* process next page *) ()
                     | Error err -> Printf.eprintf "Error: %s\n" 
                         (Social_core.Error_types.error_to_string err))
             | None -> ())
        | None -> ())
    | Error err -> 
        Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

#### `post ~path ~access_token ~params`
Make a POST request to any Graph API endpoint.

```ocaml
let params = [
  ("message", "Hello from OCaml!");
  ("link", "https://example.com");
] in

Facebook.post ~path:"me/feed" ~access_token ~params
  (function
    | Ok response -> Printf.printf "Posted: %s\n" response.body
    | Error err -> 
        Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

#### `delete ~path ~access_token`
Delete a Graph API object.

```ocaml
Facebook.delete ~path:"123456_post_id" ~access_token
  (function
    | Ok response -> print_endline "Post deleted"
    | Error err -> 
        Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

#### `batch_request ~requests ~access_token`
Execute multiple requests in a single API call (max 50).

```ocaml
open Facebook_graph_v21

let requests = [
  { method_ = `GET; relative_url = "me"; body = None; name = Some "user" };
  { method_ = `GET; relative_url = "me/posts"; body = None; name = None };
  { method_ = `POST; 
    relative_url = "me/feed"; 
    body = Some "message=Batch post!"; 
    name = None };
] in

Facebook.batch_request ~requests ~access_token
  (function
    | Ok results ->
        List.iteri (fun i result ->
          Printf.printf "Request %d: HTTP %d\n%s\n" 
            i result.code result.body
        ) results
    | Error err -> 
        Printf.eprintf "Batch failed: %s\n" (Social_core.Error_types.error_to_string err))
```

### Error Handling

The library now provides structured error information:

```ocaml
(* Error codes are automatically parsed *)
type facebook_error_code = 
  | Invalid_token        (* 190 - Re-authentication required *)
  | Rate_limit_exceeded  (* 4, 17, 32, 613 - Wait before retrying *)
  | Permission_denied    (* 200, 299, 10 - Check permissions *)
  | Invalid_parameter    (* 100 - Fix request parameters *)
  | Temporarily_unavailable  (* 2, 368 - Retry later *)
  | Duplicate_post       (* 506 - Content already posted *)
  | Unknown of int

(* Errors include retry recommendations *)
type facebook_error = {
  message : string;
  error_type : string;
  code : facebook_error_code;
  subcode : int option;
  fbtrace_id : string option;
  should_retry : bool;
  retry_after_seconds : int option;
}
```

### Rate Limiting

Rate limit information is automatically tracked:

```ocaml
(* Implement callback in your Config module *)
let on_rate_limit_update info =
  if info.percentage_used > 80.0 then
    Printf.printf "⚠️  Warning: %.1f%% of rate limit used\n" 
      info.percentage_used;
  
  (* Log to monitoring system *)
  log_metric "facebook_api_calls" info.call_count;
  log_metric "facebook_cpu_time" info.total_cputime
```

### Security

App Secret Proof is automatically added to all requests when `FACEBOOK_APP_SECRET` is set:

```bash
# Set in environment
export FACEBOOK_APP_SECRET=your_app_secret
```

All requests now use `Authorization: Bearer <token>` headers instead of URL parameters for better security.

## Testing

Run the test suite:

```bash
cd packages/social-facebook-graph-v21
dune test
```

Tests include:
- OAuth URL generation
- Token exchange
- Photo upload
- Content validation
- Token expiry handling

## License

MIT

## Related Packages

- `social-core` - Core interfaces and types
- `social-lwt` - Lwt runtime adapters
- `social-instagram-graph-v21` - Instagram API (shares Graph API)
- `social-linkedin-v2` - LinkedIn API v2
- `social-twitter-v2` - Twitter API v2
