# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.1] - Unreleased

### Added

#### Authentication
- OAuth 2.0 authentication flow
- Token refresh support
- 60-day token expiry tracking
- App Secret Proof (HMAC-SHA256 signing)
- Authorization header token transmission

#### Page Operations
- Facebook Page posting (text + images)
- Photo upload with multipart support
- Field selection for API responses

#### Pagination Support
- `get_page` function for cursor-based pagination
- `get_next_page` helper for fetching subsequent pages
- Cursor-based and URL-based pagination support

#### Rate Limiting
- Automatic parsing of `X-App-Usage` response headers
- `rate_limit_info` type with usage metrics
- Callback hook for rate limit updates

#### Error Handling
- Typed error codes (Invalid_token, Rate_limit_exceeded, Permission_denied, etc.)
- Structured error responses with retry recommendations
- Facebook trace IDs for debugging

#### Batch Requests
- Combine up to 50 API calls in single request
- Support for dependent requests

#### Generic API Methods
- `get` - GET any endpoint with field selection
- `post` - POST any endpoint
- `delete` - DELETE any resource

#### Architecture
- CPS (Continuation Passing Style) implementation
- Runtime agnostic
- HTTP client agnostic
- Integrated with social-core
