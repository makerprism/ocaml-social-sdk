# Changes

## 0.1.0

Initial release.

### Added

- Google OAuth 2.0 authentication with PKCE support
- YouTube Shorts upload via resumable upload API
- Two-step upload process (initialize metadata, upload video)
- Access token management (1-hour tokens with automatic refresh)
- Refresh token support (long-lived, no expiration)
- Content validation for video descriptions (5,000 char limit)
- Automatic #Shorts tagging
- CPS-based architecture for runtime independence
