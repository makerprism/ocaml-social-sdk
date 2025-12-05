# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.1] - Unreleased

### Added

#### Authentication
- OAuth 2.0 via Facebook integration
- Long-lived token exchange (60 days)
- Automatic token refresh with 7-day buffer
- Token expiry tracking

#### Posting
- Single image posts
- Single video posts (3-60 seconds)
- Carousel posts (2-10 images/videos)
- Reels support (3-90 seconds)
- Caption validation (2,200 chars, 30 hashtags)

#### Media Support
- Automatic media type detection from URL
- Image formats: .jpg, .jpeg, .png
- Video formats: .mp4, .mov (up to 100MB)
- Mixed media carousels

#### Container Publishing
- Two-step publishing (create container, publish)
- Smart polling with exponential backoff
- Container status checking

#### Error Handling
- Instagram API error code parsing
- User-friendly error messages
- Actionable guidance for common errors

#### Architecture
- CPS (Continuation Passing Style) implementation
- Runtime agnostic
- HTTP client agnostic
- Integrated with social-core
