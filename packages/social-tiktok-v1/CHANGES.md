# Changes

## 0.0.1

Initial release.

### Added

- OAuth 2.0 authentication with PKCE support
- Video posting via FILE_UPLOAD method
- Video posting via PULL_FROM_URL method
- Photo carousel posting (multiple images)
- Creator info query (posting capabilities and limits)
- Publish status tracking
- Chunked upload support for large videos (>5MB)
- Privacy level options (public, friends, private)
- Video constraints validation:
  - Duration: 3-600 seconds
  - Resolution: 360-4096px
  - Frame rate: 23-60 FPS
  - Formats: MP4, WebM, MOV
- CPS-based architecture for runtime independence
