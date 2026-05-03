# Changelog

## 0.1.0 (Unreleased)

Initial release of the Reddit API v1 client.

### Changed

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
