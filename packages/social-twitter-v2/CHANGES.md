# Changes

## 0.0.1

Initial release.

### Added

#### Tweet Operations
- **`post_single`** - Post single tweets with media support
- **`post_thread`** - Post threads with proper reply chaining
- **`delete_tweet`** - Delete tweets by ID
- **`get_tweet`** - Get tweet by ID with expansions and field selection
- **`search_tweets`** - Search recent tweets with pagination support
- **`get_user_timeline`** - Get user's tweets with pagination
- **`reply_to_tweet`** - Reply to tweets with media support
- **`quote_tweet`** - Quote tweets with media support
- **Expansions support** - All read endpoints support v2 expansions
- **Field selection** - Support for tweet_fields, user_fields, etc.

#### User Operations
- **`get_user_by_id`** - Get user information by ID
- **`get_user_by_username`** - Get user information by username
- **`get_me`** - Get authenticated user information
- **`follow_user`** - Follow users
- **`unfollow_user`** - Unfollow users
- **`block_user`** - Block users
- **`unblock_user`** - Unblock users

#### Engagement Operations
- **`like_tweet`** - Like tweets
- **`unlike_tweet`** - Unlike tweets
- **`retweet`** - Retweet tweets
- **`unretweet`** - Remove retweets
- **`bookmark_tweet`** - Bookmark tweets
- **`remove_bookmark`** - Remove bookmarks

#### Media Upload
- **`upload_media`** - Simple media upload for images
- **`upload_media_chunked`** - Chunked upload for large videos (up to 512MB)
- **Alt text support** - Add accessibility descriptions to media
- **INIT/APPEND/FINALIZE workflow** - Proper 3-phase upload for large files

#### Authentication
- OAuth 2.0 with PKCE authentication
- Token refresh with 30-minute expiration buffer
- Credential management

#### Developer Experience
- **`parse_pagination_meta`** - Helper to extract pagination metadata
- **`parse_rate_limit_headers`** - Parse rate limit info from API responses
- **`rate_limit_info` type** - Structured rate limit information
- **`pagination_meta` type** - Structured pagination metadata
- **Pagination support** - next_token/pagination_token parameters on all list endpoints

#### Architecture
- CPS (Continuation Passing Style) implementation
- Runtime agnostic
- HTTP client agnostic
- Integrated with social-core
