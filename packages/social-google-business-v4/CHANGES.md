# Unreleased

- Add `ensure_valid_token_with_outcome ~with_refresh_lock` reporting reused or renewed-and-persisted tokens through an explicit completion boundary. Existing APIs remain compatible; credential-only functors never write health.

# Changelog

- Add `Make_credentials_only` for coordinated credential operations without account-health callbacks. Legacy functors retain their behavior.

- Add `COORDINATED_CONFIG` and `Make_coordinated` for account refresh locks held through persistence, with credential reload under the lock. Existing functors remain compatible.

## Unreleased

- Discovery sends pageSize 20 for accounts and 100 for locations. `list_all_*` returns `Validation_error` when `max_pages` stops before the final page; use a larger cap or the paged API instead of receiving a silently partial list.

### Added

- **Token-based, paginated discovery in `OAuth.Make(Http)`**, usable in an OAuth callback before an account is stored: `list_accounts_page` / `list_all_accounts` (`OAuth.business_account` with typed `account_type` and `role`) and `list_locations_page` / `list_all_locations ~account_name ?read_mask` (`OAuth.business_location`), following `nextPageToken` up to `?max_pages` (default 50). Errors through `Social_google_oauth.api_error`; retry/backoff on `Rate_limited` stays with the caller. The account-bound `list_accounts` / `list_locations` (raw JSON, first page only) are unchanged.
- Pure `OAuth.v4_location_name ~account_name ~location_name` (`accounts/{a}/locations/{l}` for the v4 `localPosts` API) and `OAuth.default_location_read_mask`.
- `OAuth.Make(Http).get_userinfo` (typed Google userinfo).

## 0.1.0 (2026-03-22)

- Initial release
- OAuth 2.0 with PKCE support (S256)
- Post standard updates, events, and offers
- Call-to-action support
- Photo attachments via source URL
- Account and location discovery
- Automatic token refresh via social-refresh
