# Changes

## 0.0.1

Initial release.

### Added

- OAuth 2.0 authentication flow
  - App registration
  - Authorization URL generation
  - Token exchange from authorization code
  - Credential verification
- Status operations:
  - Post statuses with text, media, visibility, content warnings
  - Thread posting with media support
  - Edit statuses (Mastodon 3.5.0+)
  - Delete statuses
- Visibility levels: public, unlisted, private, direct
- Poll support with multiple choice, expiration, hidden totals
- Interactions: favorite/unfavorite, boost/unboost, bookmark/unbookmark
- Media upload with descriptions (alt text) and focus points
- Content validation (configurable character limits)
- Media size and format validation
- Scheduled publishing support
- Idempotency keys for safe retries
