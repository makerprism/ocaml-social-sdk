# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.1] - Unreleased

### Added

#### Authentication
- OAuth 2.0 authentication flow
- OAuth short-lived -> long-lived exchange support
- 60-day token expiry tracking
- App Secret Proof (HMAC-SHA256 signing)
- Authorization header token transmission
- Page token recovery from `/me/accounts` for posting retries

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
- Expanded Facebook/BUC rate limit code mapping (e.g. `80001`, `80002`)

#### Posting Reliability
- Indexed `attached_media[n]` payload for multi-photo feed posts
- Best-effort cleanup of uploaded unpublished photos when feed publish fails

#### Batch Requests
- Combine up to 50 API calls in single request
- Support for dependent requests

#### Generic API Methods
- `get` - GET any endpoint with field selection
- `post` - POST any endpoint
- `delete` - DELETE any resource

#### Analytics
- Added account analytics endpoint support:
  - `get_account_analytics` for `/v20.0/{id}/insights` with day-bucket metrics
- Added post analytics endpoint support:
  - `get_post_analytics` for `/v20.0/{post_id}/insights`
- Added canonical analytics conversion helpers for social-core metric normalization
- Added request-contract and parser tests for account/post analytics flows
- Added redaction guard for analytics network errors so query-token text is not surfaced in internal errors

#### Structured Error Handling
- Posting operations use `outcome` type with Success/Partial_success/Failure
- Non-posting operations use `api_result` type with Ok/Error

#### Architecture
- CPS (Continuation Passing Style) implementation
- Runtime agnostic
- HTTP client agnostic
- Integrated with social-core
