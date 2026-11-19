# Unreleased

- Build the authorization URL query through the new `social-core` `Social_http_request.percent_encoded_query`. No request bytes change.

# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.1] - Unreleased

### Added

- **`Make.get_userinfo ~platform ~access_token`**: typed `GET https://www.googleapis.com/oauth2/v2/userinfo` as `userinfo = {id; email; verified_email; name; given_name; family_name; picture; locale; hd}` (also reads the OpenID Connect `sub` / `email_verified` shape). `get_user_info` (raw JSON) is unchanged.
- **`api_error ~platform ~required_scopes ~status_code body`**: one mapping of Google's error envelope for every Google package: 401 `Token_invalid`; 429 and 403 with reason `quotaExceeded` / `rateLimitExceeded` / `userRateLimitExceeded` / `dailyLimitExceeded` `Rate_limited`; other 403 `Insufficient_permissions`; 404 `Resource_not_found`; otherwise `Api_error` with the body.
- `parse_userinfo` (pure).
- First test suite for the package (`test/test_google_oauth.ml`): authorization URL with the RFC 7636 S256 known answer, typed userinfo, error mapping.
