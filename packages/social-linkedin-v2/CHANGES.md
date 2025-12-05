# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.1] - Unreleased

### Added

- OAuth 2.0 authentication with OpenID Connect
- Token refresh support (partner and standard flows)
- Profile API:
  - Get current user profile (name, email, picture, locale)
- Post operations:
  - Create posts with text and media
  - Thread posting with proper reply chains
  - Get single post by URN
  - Get posts with pagination
  - Batch get multiple posts
- Pagination support:
  - Structured `paging` and `collection_response` types
  - Scroller pattern for easy navigation
- Search API (FINDER pattern):
  - Search posts by keywords
  - Filter by author
- Engagement APIs:
  - Like/unlike posts
  - Comment on posts
  - Get post comments with pagination
  - Get engagement statistics (likes, comments, shares, impressions)
- Media upload with LinkedIn's asset registration flow
- Rich type system for API responses
- Content validation
- CPS-based architecture for runtime independence
