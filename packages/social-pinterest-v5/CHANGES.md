# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.1] - Unreleased

### Added

- OAuth 2.0 authentication flow
- Automatic token refresh with 30-day token validity
- Pin creation with title, description, and media
- Board management:
  - Create boards (public/private)
  - Get board by name or ID
  - List all boards with pagination
- Search API for pins, boards, and users
- User profile management
- Bulk pin creation for efficiency
- Rate limiting with exponential backoff
- Structured error types for better error handling
- Debug logging support
- Request retry logic with automatic retries
- Optional response caching
- Content validation (500 char description, 100 char title)
