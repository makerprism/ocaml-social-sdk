# HTTP Parity Matrix (Endpoint + Contract Tests)

Scope: current implementation in `packages/social-*/lib/*.ml` and contract-style assertions in `packages/social-*/test/test_*.ml`.

Interpretation:
- This document tracks endpoint/request-shape parity and contract-test coverage.
- It does not imply production hardening; align with the root `README.md` status legend (`⚠️` = implemented + automated tests, limited production validation).
- Current matrix coverage is partial and focuses on: Facebook, Instagram, Threads, Pinterest, LinkedIn, TikTok, Twitter v2, YouTube, and the typed Mastodon and Bluesky reads.

## Facebook

Rows describe `social-facebook-graph-v25` (Graph API `v25.0`, the `api_version` constant). The deprecated `social-facebook-graph-v21` has the same contracts on `v21.0`.

| Capability | Method | Path/query contract | Required headers | Payload shape | SDK function(s) | Contract test name(s) |
| --- | --- | --- | --- | --- | --- | --- |
| OAuth authorize URL | GET (browser redirect) | `https://www.facebook.com/v25.0/dialog/oauth?client_id&redirect_uri&state&scope&response_type=code&auth_type=rerequest` | n/a | query params only | `Facebook.get_oauth_url` | `test_oauth_url`, `test_oauth_url_permissions` |
| OAuth code -> long-lived token | GET + GET | `https://graph.facebook.com/v25.0/oauth/access_token?...` then same endpoint with `grant_type=fb_exchange_token` | none | query params: app creds + code / short token | `Facebook.exchange_code` | `test_token_exchange`, `test_exchange_code_and_get_pages`, `test_long_lived_token` |
| Publish page post (with attached media IDs) | POST | `https://graph.facebook.com/v25.0/{page_id}/feed` | `Authorization: Bearer <page_token>`, `Content-Type: application/x-www-form-urlencoded` | form body: `message`, indexed `attached_media[i]={"media_fbid":...}`, optional `appsecret_proof` | `Facebook.post_single` | `test_post_with_indexed_attached_media_payload`, `test_post_with_alt_text` |
| Publish Reel | POST + POST + POST | `POST https://graph.facebook.com/v25.0/{page_id}/video_reels` (`upload_phase=start`), then `POST https://rupload.facebook.com/video-upload/v25.0/{video_id}`, then `POST .../{page_id}/video_reels` (`upload_phase=finish`) | start/finish: `Content-Type: application/x-www-form-urlencoded`; upload: `Authorization: OAuth <page_token>`, `file_url: <video_url>` | start: `upload_phase`, `access_token`, optional `appsecret_proof`; upload: empty body; finish: `upload_phase`, `video_id`, `video_state=PUBLISHED`, `description`, optional `place`, `access_token`, optional `appsecret_proof`. Upload and finish must answer `{"success": true}` | `Facebook.upload_video_reel_typed`, `Facebook.post_reel` | `test_upload_video_reel`, `test_upload_video_reel_with_place`, `test_upload_video_reel_failures`, `test_post_reel_with_place_recovers_on_finish_auth_error` |
| Account analytics | GET | `https://graph.facebook.com/v25.0/{id}/insights?metric=page_impressions_unique,page_posts_impressions_unique,page_post_engagements,page_daily_follows,page_video_views&period=day&since&until&access_token` | none | none (query-token contract) | `Facebook.get_account_analytics` | `test_account_analytics_request_contract` |
| Post analytics | GET | `https://graph.facebook.com/v25.0/{post_id}/insights?metric=post_impressions_unique,post_reactions_by_type_total,post_clicks,post_clicks_by_type&access_token` | none | none (query-token contract) | `Facebook.get_post_analytics` | `test_post_analytics_request_contract` |
| Permalink by id | GET | `https://graph.facebook.com/v25.0/{post_id}?fields=permalink_url[&appsecret_proof]` | `Authorization: Bearer <token>` | none | `Facebook.get_permalink`, `Facebook.get_permalink_for_account` | `test_get_permalink` |
| Token inspection | GET | `https://graph.facebook.com/v25.0/debug_token?input_token&access_token=<app_id\|app_secret>` | none | none (query-token contract) | `OAuth_http.debug_token_info` | `test_debug_token_info_typed`, `test_debug_token_info_errors` |
| Granted / declined permissions | GET | `https://graph.facebook.com/v25.0/me/permissions?access_token[&appsecret_proof]` | none | none (query-token contract) | `OAuth_http.get_permissions` | `test_get_permissions` |

Known subtle bug traps:
- Account/post insights carry the token as a percent-encoded query parameter and send no `appsecret_proof`.
- Multi-photo publish requires indexed `attached_media[0..n]`; flat/unindexed payloads are rejected.
- Posting may require page-token recovery from user-token context; auth failures and rate-limit failures are handled differently.

## Instagram

Rows describe `social-instagram-graph-v21` (Instagram via Facebook Login, Graph API `v21.0`; deprecated, no v25 package yet) unless they name `social-instagram-standalone-v25` (Instagram Business Login, `graph.instagram.com/v25.0`).

| Capability | Method | Path/query contract | Required headers | Payload shape | SDK function(s) | Contract test name(s) |
| --- | --- | --- | --- | --- | --- | --- |
| OAuth authorize URL | GET (browser redirect) | `https://www.facebook.com/v21.0/dialog/oauth?...` | n/a | query params only | `Instagram.get_oauth_url` | `test_oauth_url` |
| OAuth code -> long-lived token | GET + GET | `https://graph.facebook.com/v21.0/oauth/access_token?...` then `grant_type=fb_exchange_token` call | none | query params with app creds, code, redirect | `Instagram.exchange_code` | `test_token_exchange` |
| Refresh long-lived token | GET | `https://graph.instagram.com/refresh_access_token?grant_type=ig_refresh_token&access_token[&appsecret_proof]` | none | query params only | `Instagram.refresh_token` | `test_refresh_token_flow`, `test_oauth_refresh_appsecret_proof` |
| Publish media container | POST (stepwise) | `POST /v21.0/{ig_user_id}/media` then `POST /v21.0/{ig_user_id}/media_publish` (plus status polling `GET /v21.0/{container_id}?fields=status_code,status`) | `Authorization: Bearer <token>`, `Content-Type: application/x-www-form-urlencoded` | form body includes `image_url` or `video_url`, `caption`, optional `media_type` (`VIDEO`/`REELS`/`CAROUSEL`/`STORIES`), optional `children`, optional `appsecret_proof` | `Instagram.create_image_container`, `Instagram.create_video_container`, `Instagram.publish_container`, `Instagram.post_single`, `Instagram.post_reel` | `test_create_container`, `test_publish_container`, `test_post_single`, `test_video_post_full_flow` |
| Account analytics (audience) | GET | `/v21.0/{id}/insights?metric=follower_count,reach&period=day&since&until&access_token` | none | none (query-token contract) | `Instagram.get_account_audience_insights` | `test_account_audience_insights_request_contract` |
| Account analytics (engagement) | GET | `/v21.0/{id}/insights?metric_type=total_value&metric=likes,views,comments,shares,saves,replies&period=day&since&until&access_token` | none | none (query-token contract) | `Instagram.get_account_engagement_insights` | `test_account_engagement_insights_request_contract` |
| Post analytics | GET | `/v21.0/{post_id}/insights?metric=views,reach,saved,likes,comments,shares&access_token` | none | none (query-token contract) | `Instagram.get_media_insights` | `test_media_insights_request_contract` |
| Permalink by id (`social-instagram-standalone-v25`) | GET | `https://graph.instagram.com/v25.0/{media_id}?fields=permalink[&appsecret_proof]` | `Authorization: Bearer <token>` | none | `Instagram.get_permalink`, `Instagram.get_permalink_for_account` | `test_get_permalink` |

Known subtle bug traps:
- Engagement insights require `metric_type=total_value`; omitting it changes response shape.
- Container-based publishing must poll status before `media_publish` for video/reel/carousel flows.
- URL media-type detection is extension-driven (`mp4/mov` vs image extensions), so unsupported URLs fail preflight.

## Threads

| Capability | Method | Path/query contract | Required headers | Payload shape | SDK function(s) | Contract test name(s) |
| --- | --- | --- | --- | --- | --- | --- |
| OAuth authorize URL | GET (browser redirect) | `https://threads.net/oauth/authorize?client_id&redirect_uri&scope&response_type=code&state` | n/a | query params only | `Threads.get_oauth_url` | `test_oauth_url`, `test_oauth_url_rejects_redirect_uri_mismatch_with_config` |
| OAuth code exchange | POST | `https://graph.threads.net/oauth/access_token` | `Content-Type: application/x-www-form-urlencoded` | form body: `client_id`, `client_secret`, `grant_type=authorization_code`, `redirect_uri`, `code` | `OAuth_http.exchange_code`, `Threads.exchange_code` | `test_exchange_code_success` |
| OAuth long-lived refresh | GET | `https://graph.threads.net/refresh_access_token?grant_type=th_refresh_token&access_token=<long_lived_token>` | none | query params only | `OAuth_http.refresh_token`, `Threads.refresh_token` | `test_refresh_token_success` |
| Publish post | POST + POST | `POST /v1.0/{user_id}/threads` then `POST /v1.0/{user_id}/threads_publish` | `Content-Type: application/x-www-form-urlencoded` | create body includes `media_type` (`TEXT`/`IMAGE`/`VIDEO`), `text`, media URL field, `access_token`, optional `reply_to_id`, `reply_control`, `client_request_id`; publish body includes `creation_id`, `access_token` | `Threads.post_single`, `Threads.create_container_for_post`, `Threads.publish_container` | `test_post_single_success`, `test_post_single_with_idempotency_key`, `test_post_single_reply_control_forwarded` |
| Account analytics | GET (+ identity lookup) | `GET /v1.0/me?fields=id,username,name&access_token=...` then `GET /v1.0/{user_id}/threads_insights?metric=views,likes,replies,reposts,quotes&period=day&since&until&access_token=...` | none | none (query-token contract) | `Threads.get_account_insights` | `test_get_account_insights_contract` |
| Post analytics | GET | `GET /v1.0/{post_id}/insights?metric=views,likes,replies,reposts,quotes&access_token=...` | none | none (query-token contract) | `Threads.get_post_insights` | `test_get_post_insights_contract` |
| Replies / conversation / pending replies | GET | `GET /v1.0/{media_id}/replies`, `/conversation`, `/pending_replies` with `fields=<reply_fields>` (`pending_reply_fields` for pending), `limit`, optional `reverse`, `approval_status`, `after`, `access_token` | none | none (query-token contract) | `Threads.get_replies`, `Threads.get_conversation`, `Threads.get_pending_replies` | `test_get_conversation_parses_reply_fields`, `test_get_replies_requests_reply_fields`, `test_get_pending_replies_approval_status` |
| Hide / unhide reply | POST | `POST /v1.0/{reply_id}/manage_reply` | `Content-Type: application/x-www-form-urlencoded` | form body: `hide=true\|false`, `access_token` | `Threads.hide_reply`, `Threads.unhide_reply` | `test_hide_reply_success`, `test_unhide_reply_success` |
| Approve / ignore pending reply | POST | `POST /v1.0/{reply_id}/manage_pending_reply` | `Content-Type: application/x-www-form-urlencoded` | form body: `approve=true\|false`, `access_token` | `Threads.approve_reply`, `Threads.ignore_reply` | `test_approve_reply_success`, `test_ignore_reply_success` |
| Permalink by id | GET | `GET /v1.0/{post_id}?fields=permalink&access_token=...` | none | none (query-token contract) | `Threads.get_permalink`, `Threads.get_permalink_for_account` | `test_get_permalink`, `test_get_permalink_for_account` |
| Token inspection | GET | `GET /v1.0/debug_token?input_token&access_token=TH\|app_id\|app_secret` | none | none (query-token contract) | `OAuth_http.debug_token_info` | `test_debug_token_info` |

Known subtle bug traps:
- For publish endpoints, `access_token` is part of form/query contract (not Authorization header).
- OAuth inputs are strict-trim validated (state/code/redirect/client values with surrounding whitespace are rejected).
- Thread posting includes idempotency/reply-control forwarding; extra media entries fail validation early.
- Hosts: `threads.net`, `www.threads.net` and `threads.com` authorize URLs all 301 to `https://www.threads.com/oauth/authorize` (checked 2026-11-17); the SDK keeps `threads.net`. `graph.threads.net` and `graph.threads.com` both serve the API; the SDK keeps `graph.threads.net`.

## Pinterest

| Capability | Method | Path/query contract | Required headers | Payload shape | SDK function(s) | Contract test name(s) |
| --- | --- | --- | --- | --- | --- | --- |
| OAuth authorize URL | GET (browser redirect) | `https://www.pinterest.com/oauth?client_id&redirect_uri&response_type=code&scope&state` | n/a | query params only | `Pinterest.get_oauth_url` | `test_oauth_url` |
| OAuth code exchange | POST | `https://api.pinterest.com/v5/oauth/token` | `Authorization: Basic <base64(client_id:client_secret)>`, `Content-Type: application/x-www-form-urlencoded` | form body: `grant_type=authorization_code`, `code`, `redirect_uri` | `Pinterest.exchange_code` | `test_token_exchange` |
| Publish pin (image/video flow) | GET + POST multipart + POST | board lookup `/v5/boards`, media upload `/v5/media`, then create pin `/v5/pins` | bearer auth on API calls; pin create uses `Content-Type: application/json` | JSON pin body includes `board_id`, `title` (truncated to 100), `description`, `media_source` (`image_base64` or `video_id`), optional `alt_text` / `cover_image_url` | `Pinterest.post_single` | `test_post_single_full_image_flow_with_alt_text`, `test_post_single_video_url_uses_video_media_flow`, `test_post_single_truncates_title_to_100_chars` |
| User account (typed) | GET | `https://api.pinterest.com/v5/user_account[?ad_account_id]` | `Authorization: Bearer <token>` | none | `OAuth_http.get_user_account`, `Pinterest.get_user_account` | `test_get_user_account_typed` |
| Account analytics | GET | `/v5/user_account/analytics?start_date&end_date` | `Authorization: Bearer <token>` | none (query contract) | `Pinterest.get_account_analytics` | `test_get_account_analytics_request_contract` |
| Post analytics (pin) | GET | `/v5/pins/{pin_id}/analytics?start_date&end_date&metric_types=IMPRESSION,PIN_CLICK,OUTBOUND_CLICK,SAVE` | `Authorization: Bearer <token>` | none (query contract) | `Pinterest.get_pin_analytics` | `test_get_pin_analytics_request_contract` |

Known subtle bug traps:
- OAuth token exchange uses Basic Auth header; putting client credentials only in body is non-parity.
- Pin creation always needs resolved `board_id`; publish flow is multi-step even for single-image posts.
- Unknown media extensions fall back to video-first detection logic and may pivot to image based on content type.

## LinkedIn

| Capability | Method | Path/query contract | Required headers | Payload shape | SDK function(s) | Contract test name(s) |
| --- | --- | --- | --- | --- | --- | --- |
| OAuth authorize URL | GET (browser redirect) | `https://www.linkedin.com/oauth/v2/authorization?response_type=code&client_id&redirect_uri&state&scope` | n/a | query params only | `LinkedIn.get_oauth_url` | `test_oauth_url`, `test_oauth_url_parameters`, `test_oauth_url_with_organization_scopes` |
| OAuth code exchange | POST | `https://www.linkedin.com/oauth/v2/accessToken` | `Content-Type: application/x-www-form-urlencoded` | form body: `grant_type=authorization_code`, `code`, `redirect_uri`, `client_id`, `client_secret` | `LinkedIn.exchange_code` | `test_token_exchange` |
| OAuth refresh | POST | `https://www.linkedin.com/oauth/v2/accessToken` | `Content-Type: application/x-www-form-urlencoded` | form body: `grant_type=refresh_token`, `refresh_token`, `client_id`, `client_secret` | `LinkedIn.refresh_token` | `test_token_refresh_standard` |
| Publish post (UGC) | POST | `https://api.linkedin.com/v2/ugcPosts` | `Authorization: Bearer <token>`, `Content-Type: application/json`, `X-Restli-Protocol-Version: 2.0.0` | JSON body with `author`, `lifecycleState=PUBLISHED`, `specificContent.com.linkedin.ugc.ShareContent`, `visibility` | `LinkedIn.post_single` | `test_ugcpost_request_body_structure`, `test_post_single_with_organization_author` |
| Account analytics | GET + GET | `https://api.linkedin.com/v2/organizationalEntityFollowerStatistics?q=organizationalEntity&organizationalEntity={urn}` then `https://api.linkedin.com/v2/organizationalEntityShareStatistics?q=organizationalEntity&organizationalEntity={urn}` | `Authorization: Bearer <token>`, `X-Restli-Protocol-Version: 2.0.0`, `Linkedin-Version: YYYYMM`, `X-RestLi-Method: FINDER` | none | `LinkedIn.get_account_analytics` | `test_get_account_analytics` |
| Post analytics (engagement proxy) | GET | `https://api.linkedin.com/v2/socialMetadata/{urlencoded_post_urn}` | `Authorization: Bearer <token>`, `X-Restli-Protocol-Version: 2.0.0` | none | `LinkedIn.get_post_engagement` | `test_get_post_engagement` |

Known subtle bug traps:
- REST.li headers are mandatory on many calls (`X-Restli-Protocol-Version`, finder/batch method headers where applicable).
- URN validation is strict (whitespace and malformed delimiters are rejected before network calls).
- OAuth redirect URI must exactly match configured `LINKEDIN_REDIRECT_URI` when configured.
- Account analytics currently targets organization/entity finder statistics endpoints and aggregates metrics from two requests.

## TikTok

| Capability | Method | Path/query contract | Required headers | Payload shape | SDK function(s) | Contract test name(s) |
| --- | --- | --- | --- | --- | --- | --- |
| OAuth authorize URL | GET (browser redirect) | `https://www.tiktok.com/v2/auth/authorize?client_key&redirect_uri&response_type=code&scope=a,b&state[&code_challenge&code_challenge_method=plain\|S256]` (S256 challenge is hex SHA-256) | n/a | query params only | `TikTok.get_oauth_url ?scopes ?pkce_method` / `Social_tiktok_v1.get_authorization_url` | `test_oauth_auth_url_required_params`, `test_get_oauth_url_includes_pkce_when_code_verifier_present`, `test_get_oauth_url_scopes_and_s256` |
| OAuth code exchange | POST | `https://open.tiktokapis.com/v2/oauth/token/` | `Content-Type: application/x-www-form-urlencoded` | form body: `client_key`, `client_secret`, `code`, `grant_type=authorization_code`, `redirect_uri`, optional `code_verifier` | `TikTok.exchange_code`, `TikTok.exchange_code_full`, `OAuth_http.exchange_code_full` / `refresh_token_full` | `test_oauth_exchange_request_contract`, `test_oauth_exchange_request_includes_code_verifier_when_present`, `test_exchange_code_full` |
| User info (typed) | GET | `https://open.tiktokapis.com/v2/user/info/?fields=open_id,...` | `Authorization: Bearer <token>` | none | `OAuth_http.get_user` | `test_get_user_typed` |
| Publish init | POST | `https://open.tiktokapis.com/v2/post/publish/video/init/` | `Authorization: Bearer <token>`, `Content-Type: application/json; charset=UTF-8` | JSON body with `post_info` and `source_info` (`source=FILE_UPLOAD`, `video_size`, `chunk_size`, `total_chunk_count`) | `TikTok.init_video_upload` | `test_init_video_upload_request_contract` |
| Video chunk upload | PUT | `upload_url` returned by init endpoint | `Content-Type`, `Content-Length`, `Content-Range` | raw video chunk bytes | `TikTok.upload_video_chunk` | `test_upload_video_chunk_headers_contract` |
| Publish status | POST | `https://open.tiktokapis.com/v2/post/publish/status/fetch/` | `Authorization: Bearer <token>`, `Content-Type: application/json; charset=UTF-8` | JSON body: `{ "publish_id": "..." }` | `TikTok.check_publish_status` | `test_check_publish_status_request_contract` |
| Account analytics | GET + POST + POST | `GET /v2/user/info/?fields=follower_count,following_count,likes_count,video_count`, then `POST /v2/video/list/?fields=id`, then `POST /v2/video/query/?fields=id,like_count,comment_count,share_count,view_count` | bearer on all calls; JSON content type for POSTs | list body: `{ "max_count": 20 }`; query body: `{ "filters": { "video_ids": [...] } }` | `TikTok.get_account_analytics` | `test_get_account_analytics_request_contract` |
| Post analytics | POST | `https://open.tiktokapis.com/v2/video/query/?fields=id,like_count,comment_count,share_count,view_count` | `Authorization: Bearer <token>`, `Content-Type: application/json; charset=UTF-8` | JSON body: `{ "filters": { "video_ids": ["<video_id>"] } }` | `TikTok.get_post_analytics` | `test_get_post_analytics_request_contract` |

Known subtle bug traps:
- Chunk uploads fail hard when `Content-Range` does not exactly match chunk size and total bytes.
- Publish flow is async: init/upload success still requires explicit status polling.
- Account analytics is a stitched 3-call flow; partial implementation of only one call yields misleading totals.

## Twitter v2 (X)

| Capability | Method | Path/query contract | Required headers | Payload shape | SDK function(s) | Contract test name(s) |
| --- | --- | --- | --- | --- | --- | --- |
| OAuth authorize URL | GET (browser redirect) | `https://x.com/i/oauth2/authorize?response_type=code&client_id&redirect_uri&scope&state&code_challenge&code_challenge_method=S256` | n/a | query params only | `Twitter.get_oauth_url`, `OAuth.get_authorization_url` | `test_oauth_url`, `test_oauth_url_pkce` |
| OAuth code exchange | POST | `https://api.twitter.com/2/oauth2/token` | `Authorization: Basic <base64(client_id:client_secret)>`, `Content-Type: application/x-www-form-urlencoded` | form body: `grant_type=authorization_code`, `code`, `redirect_uri`, `code_verifier` | `OAuth_contract_client.exchange_code`, `Twitter.exchange_code` | `test_oauth_exchange_code_request_contract` |
| OAuth refresh | POST | `https://api.twitter.com/2/oauth2/token` | `Authorization: Basic ...`, `Content-Type: application/x-www-form-urlencoded` | form body: `grant_type=refresh_token`, `refresh_token`, `client_id` | `OAuth_contract_client.refresh_token` | `test_oauth_refresh_request_contract` |
| Publish post | POST | `https://api.twitter.com/2/tweets` | `Authorization: Bearer <user_access_token>`, `Content-Type: application/json` | JSON body: `text`, optional `media.media_ids`, optional `reply_settings`, optional `community_id` | `Twitter.post_single` | `test_post_single_payload_contract`, `test_post_single_payload_contract_includes_reply_settings_and_community_id` |
| Account analytics | GET | `https://api.twitter.com/2/users/me?user.fields=id,username,name,public_metrics` | `Authorization: Bearer <user_access_token>` | none | `Twitter.get_account_analytics` | `test_get_account_analytics_request_contract` |
| Post analytics (public metrics) | GET | `https://api.twitter.com/2/tweets/{tweet_id}?tweet.fields=text,public_metrics` | `Authorization: Bearer <user_access_token>` | none | `Twitter.get_post_analytics` | `test_get_post_analytics_request_contract` |
| Post analytics (expanded metrics) | GET (fallback-capable) | `https://api.twitter.com/2/tweets/{tweet_id}?tweet.fields=text,public_metrics,non_public_metrics,organic_metrics,promoted_metrics` (optional fallback to public-only fields) | `Authorization: Bearer <user_access_token>` | none | `Twitter.get_post_analytics ~metric_set:Expanded_metrics` / `~metric_set:Expanded_metrics_with_fallback` | `test_get_post_analytics_expanded_request_contract`, `test_get_post_analytics_expanded_forbidden_fallback`, `test_get_post_analytics_expanded_forbidden_without_fallback` |

Known subtle bug traps:
- Read/write calls require user-context bearer token, not app-only bearer.
- Tweet payload parity options (`reply_settings`, `community_id`) must survive through helper APIs.
- Media upload base is `https://api.x.com/2` while tweet endpoints are `https://api.twitter.com/2`.

## YouTube

| Capability | Method | Path/query contract | Required headers | Payload shape | SDK function(s) | Contract test name(s) |
| --- | --- | --- | --- | --- | --- | --- |
| OAuth authorize URL | GET (browser redirect) | `https://accounts.google.com/o/oauth2/v2/auth?client_id&redirect_uri&response_type=code&scope&state&access_type=offline&prompt=consent&code_challenge&code_challenge_method=S256` | n/a | query params only | `YouTube.get_oauth_url` | `test_oauth_url` |
| OAuth code exchange | POST | `https://oauth2.googleapis.com/token` | `Content-Type: application/x-www-form-urlencoded` | form body: `grant_type=authorization_code`, `code`, `redirect_uri`, `client_id`, `client_secret`, `code_verifier` | `YouTube.exchange_code`, `OAuth.exchange_code` | `test_token_exchange` |
| Publish video (resumable) | POST + PUT | init `POST https://www.googleapis.com/upload/youtube/v3/videos?uploadType=resumable&part=snippet,status` then `PUT <Location upload URL>` | init: `Authorization`, `Content-Type: application/json`, `X-Upload-Content-Length`, `X-Upload-Content-Type`; upload: `Content-Type`, `Content-Length` | init JSON contains `snippet` and `status`, plus `recordingDetails.location` (`latitude`, `longitude`) and `part=...,recordingDetails` only when `recording_location` is given; upload body is raw video bytes | `YouTube.post_single` | `test_video_upload_init`, `test_video_resumable_upload`, `test_recording_location_included`, `test_recording_location_omitted` |
| Account analytics | GET | `https://www.googleapis.com/youtube/v3/channels?part=id,snippet,statistics&mine=true` | `Authorization: Bearer <token>` | none | `YouTube.get_account_analytics` | `test_get_account_analytics_contract_and_parsing` |
| Post analytics | GET | `https://www.googleapis.com/youtube/v3/videos?part=id,snippet,statistics&id={video_id}` | `Authorization: Bearer <token>` | none | `YouTube.get_post_analytics` | `test_get_post_analytics_contract_and_parsing` |
| Account analytics (reports time-series) | GET | `https://youtubeanalytics.googleapis.com/v2/reports?ids=channel==MINE&startDate={YYYY-MM-DD}&endDate={YYYY-MM-DD}&metrics={csv}&dimensions=day` | `Authorization: Bearer <token>` | none | `YouTube.get_account_analytics_report` | `test_get_account_analytics_report_contract_and_parsing` |
| Post analytics (reports time-series) | GET | `https://youtubeanalytics.googleapis.com/v2/reports?ids=channel==MINE&startDate={YYYY-MM-DD}&endDate={YYYY-MM-DD}&metrics={csv}&dimensions=day&filters=video=={video_id}` | `Authorization: Bearer <token>` | none | `YouTube.get_post_analytics_report` | `test_get_post_analytics_report_contract_and_parsing` |
| Thumbnail upload | POST multipart | `https://www.googleapis.com/upload/youtube/v3/thumbnails/set?videoId={video_id}&uploadType=multipart` | `Authorization: Bearer <token>` | multipart part `media` with image bytes and explicit content type | `YouTube.build_thumbnail_upload_request`, `YouTube.upload_thumbnail` | `test_build_thumbnail_upload_request_contract`, `test_upload_thumbnail_contract` |

Known subtle bug traps:
- Resumable upload must return `Location` on init; missing header is a hard failure.
- Video publish helper auto-appends `#Shorts` in description and truncates title to platform-safe length.
- Thumbnail upload accepts only image content types (`jpeg/jpg/png/webp`) in current validation.

## Mastodon

Rows describe the typed reads in `social-mastodon-v1` (`Reads.Make (Http)`; the `Make` wrappers send the same requests with the stored token after `verify_credentials`). `{instance}` is the account's instance URL.

| Capability | Method | Path/query contract | Required headers | Payload shape | SDK function(s) | Contract test name(s) |
| --- | --- | --- | --- | --- | --- | --- |
| Status | GET | `{instance}/api/v1/statuses/{id}` | `Authorization: Bearer <token>` when given (private statuses) | none | `Reads.get_status`, `Make.get_status_entity` | `test_get_status_typed`, `test_account_wrappers` |
| Status context (thread) | GET | `{instance}/api/v1/statuses/{id}/context` | `Authorization: Bearer <token>` when given (public: 40 ancestors / 60 descendants / depth 20) | none; `Mastodon-Async-Refresh` response header parsed | `Reads.get_status_context`, `Make.get_status_context` | `test_context_with_token`, `test_context_public_and_not_found`, `test_context_errors` |
| Notifications | GET | `{instance}/api/v1/notifications?limit&max_id&since_id&min_id&types[]&exclude_types[]&account_id` | `Authorization: Bearer <token>` (`read:notifications`) | none; cursors from `Link` | `Reads.get_notifications`, `Make.get_notifications_page` | `test_notifications_request_and_parse`, `test_notifications_last_page_and_scope_error` |
| Account statuses | GET | `{instance}/api/v1/accounts/{id}/statuses?limit&max_id&since_id&min_id&exclude_replies&exclude_reblogs&only_media&pinned` | `Authorization: Bearer <token>` when given | none; cursors from `Link` | `Reads.get_account_statuses`, `Make.get_account_statuses_page` | `test_account_statuses` |
| Account lookup | GET | `{instance}/api/v1/accounts/lookup?acct` | `Authorization: Bearer <token>` when given | none | `Reads.lookup_account` | `test_lookup_account` |
| WebFinger | GET | `https://{domain}/.well-known/webfinger?resource=acct:{username}@{domain}` | `Accept: application/jrd+json, application/json` | none | `Reads.webfinger` | `test_webfinger`, `test_webfinger_pure` |

Known subtle bug traps:
- A 404 from these reads is `Resource_not_found`; the older untyped readers (`Make.get_status`, `lookup_account_by_acct`) still report it as `Api_error`.
- Notification and status ids are only ordered within one instance; page with the `Link` cursors rather than comparing ids across instances.
- `webfinger` fetches a domain taken from user input; consumers must reject private and loopback hosts first.

## Bluesky

Rows describe the typed reads in `social-bluesky-v1` (`Reads.Make (Http)`; the `Make_with_options` wrappers send the same requests to the configured `service_url` with the account's session). `{service}` is a PDS, or `https://public.api.bsky.app` for unauthenticated reads.

| Capability | Method | Path/query contract | Required headers | Payload shape | SDK function(s) | Contract test name(s) |
| --- | --- | --- | --- | --- | --- | --- |
| Post thread | GET | `{service}/xrpc/app.bsky.feed.getPostThread?uri&depth&parentHeight` | `Authorization: Bearer <accessJwt>` when given | none | `Reads.get_post_thread`, `get_post_thread_view` | `test_post_thread`, `test_post_thread_parent_chain_and_defaults`, `test_post_thread_errors` |
| Author feed | GET | `{service}/xrpc/app.bsky.feed.getAuthorFeed?actor&limit&cursor&filter&includePins` | `Authorization: Bearer <accessJwt>` when given | none | `Reads.get_author_feed`, `get_author_feed_page` | `test_author_feed` |
| Posts by URI | GET | `{service}/xrpc/app.bsky.feed.getPosts?uris=..&uris=..` (at most 25 per call; longer lists are split) | `Authorization: Bearer <accessJwt>` when given | none | `Reads.get_posts`, `get_posts` | `test_get_posts_batches` |
| Notifications | GET | `{service}/xrpc/app.bsky.notification.listNotifications?limit&cursor&reasons=..&reasons=..` | `Authorization: Bearer <accessJwt>` | none | `Reads.list_notifications`, `list_notifications_page` | `test_list_notifications` |
| Resolve handle | GET | `{service}/xrpc/com.atproto.identity.resolveHandle?handle` | none | none | `Reads.resolve_handle` | `test_resolve_handle` |

Known subtle bug traps:
- XRPC array parameters repeat the key (`uris=a&uris=b`); `Uri.with_query` with a value list would send `uris=a,b`.
- `getPostThread` defaults to `depth=6`, which cuts long conversations off; a thread node at the cut has no `replies` even when `replyCount` is positive.
- A deleted thread root is HTTP 400 `NotFound` (`Resource_not_found`), not 404.
