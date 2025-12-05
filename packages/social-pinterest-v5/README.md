# social-pinterest-v5

OCaml library for Pinterest API v5.

> **Warning:** This library was primarily built using LLMs and has not been tested. Expect breaking changes.

## 🚀 New Features (Based on Popular Libraries)

### Core Enhancements
- ✅ **Automatic Token Refresh** - Never worry about expired tokens
- ✅ **Rate Limiting with Exponential Backoff** - Handles API limits gracefully
- ✅ **Structured Error Types** - Better error handling and debugging
- ✅ **Enhanced Board Management** - Create boards, search by name/ID
- ✅ **Search API** - Search pins, boards, and users
- ✅ **User Profile Management** - Get and manage user profiles
- ✅ **Bulk Operations** - Create multiple pins efficiently
- ✅ **Debug Logging** - Comprehensive logging for troubleshooting
- ✅ **Request Retry Logic** - Automatic retry on failures
- ✅ **Response Caching** - Optional caching support

## Installation

```bash
cd packages/social-pinterest-v5
dune build
dune install
```

## Quick Start

### Basic Pin Creation (Backward Compatible)

```ocaml
open Social_pinterest_v5

module Pinterest = Pinterest_v5.Make(YourConfig)

(* Simple pin creation - works like before *)
Pinterest.post_single 
  ~account_id:"user123"
  ~text:"Check out this pin!"
  ~media_urls:["https://example.com/image.jpg"]
  (fun pin_id -> Printf.printf "Created: %s\n" pin_id)
  (fun err -> Printf.eprintf "Error: %s\n" err)
```

### Advanced Features

#### Automatic Token Refresh

```ocaml
(* Tokens are automatically refreshed when expired *)
(* No manual intervention needed! *)
Pinterest.ensure_valid_token ~account_id:"user123"
  (fun token -> 
    (* Token is guaranteed to be valid *)
    Printf.printf "Valid token: %s\n" token)
  (fun err -> Printf.eprintf "Error: %s\n" err)
```

#### Board Management

```ocaml
(* Create a new board *)
Pinterest.create_board
  ~access_token:token
  ~name:"My Recipe Collection"
  ~description:(Some "Delicious recipes I want to try")
  ~privacy:"PUBLIC"  (* or "PRIVATE" *)
  (fun board_id -> Printf.printf "Created board: %s\n" board_id)
  handle_error

(* Get board by name or ID *)
Pinterest.get_board
  ~access_token:token
  ~board_identifier:"My Recipe Collection"  (* works with name or ID *)
  (fun board -> Printf.printf "Found board: %s (ID: %s)\n" board.name board.id)
  handle_error

(* Get all boards with pagination *)
Pinterest.get_all_boards
  ~access_token:token
  ~page_size:50
  (fun boards -> 
    List.iter (fun b -> 
      Printf.printf "- %s (%s)\n" b.name b.id
    ) boards)
  handle_error
```

#### Search Functionality

```ocaml
(* Search for pins *)
Pinterest.search
  ~access_token:token
  ~query:"vegan recipes"
  ~scope:Pins
  ?bookmark:None  (* for pagination *)
  (fun results -> 
    (* Process search results as JSON *)
    process_results results)
  handle_error

(* Search scopes available: *)
(* - Pins: Search all pins *)
(* - Boards: Search boards *)
(* - Users: Search user accounts *)
(* - MyPins: Search your own pins *)
(* - Videos: Search video pins *)
```

#### User Profile Management

```ocaml
(* Get current user profile *)
Pinterest.get_user_profile
  ~access_token:token
  ?username:None  (* None = current user *)
  (fun profile ->
    Printf.printf "User: %s\n" profile.username;
    Printf.printf "Followers: %d\n" profile.follower_count;
    Printf.printf "Boards: %d\n" profile.board_count;
    Printf.printf "Monthly views: %s\n" 
      (match profile.monthly_views with
       | Some v -> string_of_int v
       | None -> "N/A"))
  handle_error

(* Get specific user profile *)
Pinterest.get_user_profile
  ~access_token:token
  ~username:(Some "pinterest")
  handle_profile
  handle_error
```

#### Bulk Pin Creation

```ocaml
(* Create multiple pins efficiently *)
let pins = [
  ("board_id_1", "Title 1", "Description 1", "https://example.com/img1.jpg", None);
  ("board_id_1", "Title 2", "Description 2", "https://example.com/img2.jpg", Some "https://link.com");
  ("board_id_2", "Title 3", "Description 3", "https://example.com/img3.jpg", None);
] in

Pinterest.create_pins_bulk
  ~access_token:token
  ~pins
  (fun result ->
    Printf.printf "Success: %d pins created\n" (List.length result.successful);
    Printf.printf "Failed: %d pins\n" (List.length result.failed);
    List.iter (fun (id, err) -> 
      Printf.eprintf "  Pin %s failed: %s\n" id err
    ) result.failed)
  handle_error
```

#### Structured Error Handling

```ocaml
(* Handle different error types *)
let handle_pinterest_error = function
  | Pinterest_v5_enhanced.AuthorizationError msg ->
      Printf.eprintf "Auth error: %s\n" msg;
      (* Trigger re-authentication *)
  | Pinterest_v5_enhanced.RateLimitError info ->
      Printf.eprintf "Rate limited. Retry after: %f seconds\n" 
        (info.reset_at -. Unix.time ());
      (* Schedule retry *)
  | Pinterest_v5_enhanced.ValidationError msg ->
      Printf.eprintf "Validation failed: %s\n" msg;
      (* Fix input data *)
  | Pinterest_v5_enhanced.ServerError (code, msg) ->
      Printf.eprintf "Server error %d: %s\n" code msg;
      (* Retry or escalate *)
  | Pinterest_v5_enhanced.BoardNotFoundError board ->
      Printf.eprintf "Board not found: %s\n" board;
      (* Create board or use different one *)
  | _ -> Printf.eprintf "Unknown error\n"
```

#### Enhanced Configuration

```ocaml
module EnhancedConfig = struct
  module Http = Your_HTTP_Client
  
  (* Standard configuration *)
  let get_env = Sys.getenv_opt
  let get_credentials = your_credential_getter
  let update_credentials = your_credential_updater
  let encrypt = your_encryption
  let decrypt = your_decryption
  let update_health_status = your_health_updater
  
  (* New optional features *)
  let log level message =
    match level with
    | Debug -> if debug_enabled then print_endline ("[DEBUG] " ^ message)
    | Info -> print_endline ("[INFO] " ^ message)
    | Warning -> prerr_endline ("[WARN] " ^ message)
    | Error -> prerr_endline ("[ERROR] " ^ message)
  
  let current_time () = Unix.time ()
  
  (* Optional caching *)
  let cache = Hashtbl.create 100
  
  let get_cache key = 
    Hashtbl.find_opt cache key
  
  let set_cache key value ttl =
    Hashtbl.replace cache key value
    (* In production, implement TTL expiry *)
end
```

## Migration Guide

### Upgrading Your Config

The enhanced features require additional configuration functions. Update your config module:

```ocaml
module YourConfig = struct
  module Http = Your_HTTP_Client
  
  (* Existing required functions *)
  let get_env = ...
  let get_credentials = ...
  let update_credentials = ...
  let encrypt = ...
  let decrypt = ...
  let update_health_status = ...
  
  (* New required functions for enhanced features *)
  let log level message =
    (* Optional: implement logging or use no-op *)
    match level with
    | Debug -> if debug_enabled then print_endline ("[DEBUG] " ^ message)
    | Info -> print_endline ("[INFO] " ^ message)
    | Warning -> prerr_endline ("[WARN] " ^ message)
    | Error -> prerr_endline ("[ERROR] " ^ message)
  
  let current_time () = Unix.time ()
  
  let get_cache key = None  (* Optional: implement caching *)
  let set_cache key value ttl = ()  (* Optional: implement caching *)
end

module Pinterest = Pinterest_v5.Make(YourConfig)
```

### Key Improvements from Battle-Tested Libraries

| Feature | Before | After | Inspired By |
|---------|--------|-------|-------------|
| Token Management | Manual refresh | Automatic refresh | Official SDK |
| Rate Limiting | None | Exponential backoff | Official SDK |
| Board Selection | First board only | By name or ID | py3-pinterest |
| Error Handling | String errors | Typed errors | pinterest-api-php |
| Search | Not supported | Full search API | All libraries |
| Bulk Operations | Single pin only | Batch creation | Official SDK |
| User Profiles | Not supported | Full profile API | py3-pinterest |
| Debugging | None | Comprehensive logs | Official SDK |

## Environment Variables

```bash
# Required
PINTEREST_CLIENT_ID=your_client_id
PINTEREST_CLIENT_SECRET=your_client_secret

# Optional for enhanced features
PINTEREST_DEBUG=true  # Enable debug logging
PINTEREST_CACHE_TTL=3600  # Cache TTL in seconds
PINTEREST_MAX_RETRIES=3  # Max retry attempts
```

## API Limits & Best Practices

### Rate Limits (Handled Automatically)
- **Hourly limit**: 1,000 requests per hour
- **Daily limit**: 10,000 requests per day
- **Burst limit**: 10 requests per second

The library automatically:
- Tracks rate limit headers
- Implements exponential backoff
- Retries with jitter to avoid thundering herd

### Content Limits
- **Description**: 500 characters max
- **Title**: 100 characters max
- **Images per pin**: 1-5 images
- **Board name**: 50 characters max

### Performance Tips
1. **Use bulk operations** for multiple pins
2. **Enable caching** for frequently accessed data
3. **Reuse access tokens** (valid for 30 days)
4. **Batch API calls** when possible

## Testing

```bash
# Run original tests
dune test test_pinterest

# Run enhanced tests
dune test test_pinterest_enhanced

# Run all tests
dune test
```

## Examples

Check the `examples/` directory for:
- `token_refresh.ml` - Automatic token management
- `board_management.ml` - Advanced board operations
- `bulk_operations.ml` - Efficient bulk pinning
- `search_example.ml` - Search API usage
- `error_handling.ml` - Proper error handling
- `rate_limiting.ml` - Rate limit handling

## Comparison with Other Libraries

| Library | Stars | Language | Our Implementation |
|---------|-------|----------|-------------------|
| py3-pinterest | 353 | Python | ✅ Board management, ✅ Search |
| Official SDK | 70 | Python | ✅ Token refresh, ✅ Rate limiting |
| pinterest-api-php | 173 | PHP | ✅ Error types, ✅ Pagination |
| php-pinterest-bot | 411 | PHP | ✅ Bulk operations |

## Support

For issues or questions:
1. Check the [COMPARISON_REPORT.md](COMPARISON_REPORT.md)
2. Review test files for usage examples
3. Open an issue with debug logs enabled

## License

MIT

## Credits

Enhanced implementation inspired by:
- [Pinterest Official Python SDK](https://github.com/pinterest/pinterest-python-sdk)
- [py3-pinterest](https://github.com/bstoilov/py3-pinterest) (353 stars)
- [pinterest-api-php](https://github.com/dirkgroenen/pinterest-api-php) (173 stars)