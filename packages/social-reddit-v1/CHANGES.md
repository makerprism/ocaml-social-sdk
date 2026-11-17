# Changelog

## 0.1.0 (Unreleased)

Initial release of the Reddit API v1 client.

### Changed

- The `User-Agent` is configurable. Reddit requires a per-app agent
  (`<platform>:<app ID>:<version> (by /u/<username>)`) and throttles generic
  ones; the SDK hard-coded its own. `Make` now reads `REDDIT_USER_AGENT` via
  `Config.get_env` per request (falling back to the new top-level
  `default_user_agent`), and `OAuth.Make.exchange_code` / `refresh_token` /
  `get_user_info` / `revoke_token` take `?user_agent`. The token-endpoint and
  revoke calls (`OAuth.Make`, `Make.exchange_code`, automatic refresh) sent
  no `User-Agent` at all before; they now send one. Additive: existing calls
  compile and behave as before apart from the added header.
- `SUBREDDIT_NOTALLOWED`/`SUBREDDIT_NOEXIST` and bare 403 responses now
  carry Reddit's verbatim `message`/`error` text via the new
  `platform_message` field on `Insufficient_permissions`. See
  `social-core/CHANGES.md` for the breaking variant change.

### Features

- **OAuth 2.0 authentication**
  - Authorization URL generation with scopes
  - Token exchange with Basic Auth
  - Token refresh for long-lived access
  - Token revocation

- **Post submission**
  - Self-posts (text with title and body)
  - Link posts (title with URL)
  - Image posts (title with image URL)
  - Crossposting between subreddits
  - Flair support (get available flairs, set on posts)
  - NSFW and spoiler flags

- **Subreddit management**
  - List moderated subreddits
  - Get subreddit flairs
  - Check flair requirements

- **Post management**
  - Delete posts

- **Validation**
  - Title length validation (max 300 chars)
  - Body length validation (max 40,000 chars)
  - Media size validation
  - URL validation

- **Error handling**
  - Rate limit detection with retry-after parsing
  - Authentication errors
  - Subreddit permission errors
  - API error mapping to SDK error types
