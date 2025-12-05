# Changes

## [2.0.0] - 2025-01-XX

### Major Feature Release 🚀

This release transforms the package from a basic posting library into a feature-complete Twitter v2 implementation with 35+ new features.

### Added

#### Tweet Operations
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
- **`upload_media_chunked`** - Chunked upload for large videos (up to 512MB)
- **Alt text support** - Add accessibility descriptions to media
- **INIT/APPEND/FINALIZE workflow** - Proper 3-phase upload for large files

#### Developer Experience
- **`parse_pagination_meta`** - Helper to extract pagination metadata
- **`parse_rate_limit_headers`** - Parse rate limit info from API responses
- **`rate_limit_info` type** - Structured rate limit information
- **`pagination_meta` type** - Structured pagination metadata
- **Pagination support** - next_token/pagination_token parameters on all list endpoints

#### Documentation
- **FEATURE_COMPARISON.md** - Comprehensive comparison with Tweepy and node-twitter-api-v2
- **MIGRATION_GUIDE.md** - Complete migration guide with examples
- **EXAMPLES.md** - 13 real-world usage examples
- **QUICK_REFERENCE.md** - One-page quick reference card
- Updated **README.md** - Comprehensive feature list and API coverage

### Changed

#### Breaking Changes

- **`post_thread`** - Now posts ALL tweets in the thread (was previously simplified to only post first tweet)
  - **Before**: Returned single tweet ID
  - **After**: Returns list of all tweet IDs in thread
  - **Migration**: Update code to handle list of IDs instead of single ID

#### Improvements

- **Thread posting** - Complete implementation with proper reply chaining
- **Media upload** - Now supports both simple and chunked upload methods
- **Error messages** - More descriptive error messages with HTTP status codes
- **Token refresh** - Better handling of token expiration (30min buffer)

### Performance

- **Reduced API calls** - Expansions and field selection minimize round trips
- **Optimized media upload** - 5MB chunks for efficient large file uploads
- **Better rate limiting** - Parse response headers to track limits dynamically

### Statistics

- **Lines of code**: 496 → 1,425 (+187%)
- **Functions**: 8 → 28 (+250%)
- **Documentation**: 93 lines → 1,840 lines (+1,877%)
- **Feature coverage**: 21% → 47% of Twitter v2 API

### Compatibility

- **OCaml**: 4.08+
- **Dependencies**: yojson, uri, base64, ptime
- **Architecture**: CPS-based (runtime agnostic)

## [1.0.0] - 2024-XX-XX

### Initial Release

#### Features

- OAuth 2.0 with PKCE authentication
- Basic tweet posting
- Simple media upload (images only)
- Thread posting (simplified)
- Token refresh
- Content validation
- Media validation
- Rate limiting (15 posts/24hrs)

#### Architecture

- CPS (Continuation Passing Style) implementation
- Runtime agnostic
- HTTP client agnostic
- Integrated with social-provider-core

---

## Migration Guide

### From 1.x to 2.x

See [MIGRATION_GUIDE.md](./MIGRATION_GUIDE.md) for detailed migration instructions.

**Key Changes:**
1. `post_thread` now returns list of tweet IDs (was single ID)
2. 20+ new functions available
3. Optional parameters added for expansions/fields (backward compatible)

**Quick Migration:**

```ocaml
(* 1.x - Only first tweet posted *)
Twitter.post_thread ~texts ~media_urls_per_post
  (fun tweet_id -> (* single ID *))

(* 2.x - All tweets posted *)
Twitter.post_thread ~texts ~media_urls_per_post
  (fun tweet_ids -> (* list of IDs *)
    List.iter (fun id -> Printf.printf "Posted: %s\n" id) tweet_ids)
```

## Roadmap

### v2.1.0 (Planned)
- Streaming API (filtered streams)
- Stream rule management
- Auto-reconnection for streams

### v2.2.0 (Planned)
- Lists management
- Direct messages
- Mute/unmute users

### v2.3.0 (Planned)
- Get mentions timeline
- Get home timeline
- Spaces API

### v3.0.0 (Future)
- Batch operations
- Advanced analytics
- Webhook support

## Contributing

We welcome contributions! Areas where help is needed:

- **Streaming API** - Real-time tweet monitoring
- **Lists management** - CRUD operations for lists
- **Direct messages** - DM sending and receiving
- **Test coverage** - More comprehensive tests
- **Documentation** - More examples and tutorials

See [CONTRIBUTING.md](../../CONTRIBUTING.md) for guidelines.

## Support

- Issues: [GitHub Issues](https://github.com/yourusername/feedmansion/issues)
- Documentation: See README.md and EXAMPLES.md
- Community: [Discussions](https://github.com/yourusername/feedmansion/discussions)

## License

MIT - See [LICENSE](../../LICENSE) for details.

## Acknowledgments

- Twitter API v2 Documentation
- Inspiration from [Tweepy](https://github.com/tweepy/tweepy) and [node-twitter-api-v2](https://github.com/PLhery/twitter-api-v2)
- OCaml community for feedback and support

---

**Note**: This package provides comprehensive Twitter v2 API coverage while maintaining a unique CPS architecture that makes it runtime-agnostic - a feature not found in popular Python/JavaScript libraries.
