# HTTP Parity Matrix (Endpoint + Contract Tests)

Scope: current implementation in `packages/social-*/lib/*.ml` and contract-style assertions in `packages/social-*/test/test_*.ml`.

Interpretation:
- This document tracks endpoint/request-shape parity and contract-test coverage.
- It does not imply production hardening; align with the root `README.md` status legend (`⚠️` = implemented + automated tests, limited production validation).
- Current matrix coverage is partial and focuses on: Facebook, Instagram, Threads, Pinterest, LinkedIn, TikTok, Twitter v2, and YouTube.

## Facebook

| Capability | Method | Path/query contract | Required headers | Payload shape | SDK function(s) | Contract test name(s) |
| --- | --- | --- | --- | --- | --- | --- |
| OAuth authorize URL | GET (browser redirect) | `https://www.facebook.com/v21.0/dialog/oauth?client_id&redirect_uri&state&scope&response_type=code&auth_type=rerequest` | n/a | query params only | `Facebook.get_oauth_url` | `test_oauth_url`, `test_oauth_url_permissions` |
| OAuth code -> long-lived token | GET + GET | `https://graph.facebook.com/v21.0/oauth/access_token?...` then same endpoint with `grant_type=fb_exchange_token` | none | query params: app creds + code / short token | `Facebook.exchange_code` | `test_token_exchange`, `test_exchange_code_and_get_pages`, `test_long_lived_token` |
| Publish page post (with attached media IDs) | POST | `https://graph.facebook.com/v21.0/{page_id}/feed` | `Authorization: Bearer <page_token>`, `Content-Type: application/x-www-form-urlencoded` | form body: `message`, indexed `attached_media[i]={"media_fbid":...}`, optional `appsecret_proof` | `Facebook.post_single` | `test_post_with_indexed_attached_media_payload`, `test_post_with_alt_text` |
| Account analytics | GET | `https://graph.facebook.com/v20.0/{id}/insights?metric=page_impressions_unique,page_posts_impressions_unique,page_post_engagements,page_daily_follows,page_video_views&period=day&since&until&access_token` | none | none (query-token contract) | `Facebook.get_account_analytics` | `test_account_analytics_request_contract` |
| Post analytics | GET | `https://graph.facebook.com/v20.0/{post_id}/insights?metric=post_impressions_unique,post_reactions_by_type_total,post_clicks,post_clicks_by_type&access_token` | none | none (query-token contract) | `Facebook.get_post_analytics` | `test_post_analytics_request_contract` |

Known subtle bug traps:
- Account/post insights are pinned to `v20.0` while most Graph calls use `v21.0`.
- Multi-photo publish requires indexed `attached_media[0..n]`; flat/unindexed payloads are rejected.
- Posting may require page-token recovery from user-token context; auth failures and rate-limit failures are handled differently.

## Instagram

| Capability | Method | Path/query contract | Required headers | Payload shape | SDK function(s) | Contract test name(s) |
| --- | --- | --- | --- | --- | --- | --- |
| OAuth authorize URL | GET (browser redirect) | `https://www.facebook.com/v21.0/dialog/oauth?...` | n/a | query params only | `Instagram.get_oauth_url` | `test_oauth_url` |
| OAuth code -> long-lived token | GET + GET | `https://graph.facebook.com/v21.0/oauth/access_token?...` then `grant_type=fb_exchange_token` call | none | query params with app creds, code, redirect | `Instagram.exchange_code` | `test_token_exchange` |
| Refresh long-lived token | GET | `https://graph.instagram.com/refresh_access_token?grant_type=ig_refresh_token&access_token[&appsecret_proof]` | none | query params only | `Instagram.refresh_token` | `test_refresh_token_flow`, `test_oauth_refresh_appsecret_proof` |
| Publish media container | POST (stepwise) | `POST /v21.0/{ig_user_id}/media` then `POST /v21.0/{ig_user_id}/media_publish` (plus status polling `GET /v21.0/{container_id}?fields=status_code,status`) | `Authorization: Bearer <token>`, `Content-Type: application/x-www-form-urlencoded` | form body includes `image_url` or `video_url`, `caption`, optional `media_type` (`VIDEO`/`REELS`/`CAROUSEL`/`STORIES`), optional `children`, optional `appsecret_proof` | `Instagram.create_image_container`, `Instagram.create_video_container`, `Instagram.publish_container`, `Instagram.post_single`, `Instagram.post_reel` | `test_create_container`, `test_publish_container`, `test_post_single`, `test_video_post_full_flow` |
| Account analytics (audience) | GET | `/v21.0/{id}/insights?metric=follower_count,reach&period=day&since&until&access_token` | none | none (query-token contract) | `Instagram.get_account_audience_insights` | `test_account_audience_insights_request_contract` |
| Account analytics (engagement) | GET | `/v21.0/{id}/insights?metric_type=total_value&metric=likes,views,comments,shares,saves,replies&period=day&since&until&access_token` | none | none (query-token contract) | `Instagram.get_account_engagement_insights` | `test_account_engagement_insights_request_contract` |
| Post analytics | GET | `/v21.0/{post_id}/insights?metric=views,reach,saved,likes,comments,shares&access_token` | none | none (query-token contract) | `Instagram.get_media_insights` | `test_media_insights_request_contract` |

Known subtle bug traps:
- Engagement insights require `metric_type=total_value`; omitting it changes response shape.
- Container-based publishing must poll status before `media_publish` for video/reel/carousel flows.
- URL media-type detection is extension-driven (`mp4/mov` vs image extensions), so unsupported URLs fail preflight.

## Threads

| Capability | Method | Path/query contract | Required headers | Payload shape | SDK function(s) | Contract test name(s) |
| --- | --- | --- | --- | --- | --- | --- |
| OAuth authorize URL | GET (browser redirect) | `https://www.threads.com/oauth/authorize?client_id&redirect_uri&scope&response_type=code&state` | n/a | query params only | `Threads.get_oauth_url` | `test_oauth_url`, `test_oauth_url_rejects_redirect_uri_mismatch_with_config` |
| OAuth code exchange | POST | `https://graph.threads.net/v1.0/oauth/access_token` | `Content-Type: application/x-www-form-urlencoded` | form body: `client_id`, `client_secret`, `grant_type=authorization_code`, `redirect_uri`, `code` | `OAuth_http.exchange_code`, `Threads.exchange_code` | `test_exchange_code_success` |
| OAuth long-lived refresh | GET | `https://graph.threads.net/v1.0/refresh_access_token?grant_type=th_refresh_token&access_token=<long_lived_token>` | none | query params only | `OAuth_http.refresh_token`, `Threads.refresh_token` | `test_refresh_token_success` |
| Publish post | POST + POST | `POST /v1.0/{user_id}/threads` then `POST /v1.0/{user_id}/threads_publish` | `Content-Type: application/x-www-form-urlencoded` | create body includes `media_type` (`TEXT`/`IMAGE`/`VIDEO`), `text`, media URL field, `access_token`, optional `reply_to_id`, `reply_control`, `client_request_id`; publish body includes `creation_id`, `access_token` | `Threads.post_single`, `Threads.create_container_for_post`, `Threads.publish_container` | `test_post_single_success`, `test_post_single_with_idempotency_key`, `test_post_single_reply_control_forwarded` |
| Account analytics | GET (+ identity lookup) | `GET /v1.0/me?fields=id,username,name&access_token=...` then `GET /v1.0/{user_id}/threads_insights?metric=views,likes,replies,reposts,quotes&period=day&since&until&access_token=...` | none | none (query-token contract) | `Threads.get_account_insights` | `test_get_account_insights_contract` |
| Post analytics | GET | `GET /v1.0/{post_id}/insights?metric=views,likes,replies,reposts,quotes&access_token=...` | none | none (query-token contract) | `Threads.get_post_insights` | `test_get_post_insights_contract` |

Known subtle bug traps:
- For publish endpoints, `access_token` is part of form/query contract (not Authorization header).
- OAuth inputs are strict-trim validated (state/code/redirect/client values with surrounding whitespace are rejected).
- Thread posting includes idempotency/reply-control forwarding; extra media entries fail validation early.

## Pinterest

| Capability | Method | Path/query contract | Required headers | Payload shape | SDK function(s) | Contract test name(s) |
| --- | --- | --- | --- | --- | --- | --- |
| OAuth authorize URL | GET (browser redirect) | `https://www.pinterest.com/oauth?client_id&redirect_uri&response_type=code&scope&state` | n/a | query params only | `Pinterest.get_oauth_url` | `test_oauth_url` |
| OAuth code exchange | POST | `https://api.pinterest.com/v5/oauth/token` | `Authorization: Basic <base64(client_id:client_secret)>`, `Content-Type: application/x-www-form-urlencoded` | form body: `grant_type=authorization_code`, `code`, `redirect_uri` | `Pinterest.exchange_code` | `test_token_exchange` |
| Publish pin (image/video flow) | GET + POST multipart + POST | board lookup `/v5/boards`, media upload `/v5/media`, then create pin `/v5/pins` | bearer auth on API calls; pin create uses `Content-Type: application/json` | JSON pin body includes `board_id`, `title` (truncated to 100), `description`, `media_source` (`image_base64` or `video_id`), optional `alt_text` / `cover_image_url` | `Pinterest.post_single` | `test_post_single_full_image_flow_with_alt_text`, `test_post_single_video_url_uses_video_media_flow`, `test_post_single_truncates_title_to_100_chars` |
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
| OAuth authorize URL | GET (browser redirect) | `https://www.tiktok.com/v2/auth/authorize?client_key&redirect_uri&response_type=code&scope&state` | n/a | query params only | `TikTok.get_oauth_url` / `Social_tiktok_v1.get_authorization_url` | `test_oauth_auth_url_required_params`, `test_get_oauth_url_includes_pkce_when_code_verifier_present` |
| OAuth code exchange | POST | `https://open.tiktokapis.com/v2/oauth/token/` | `Content-Type: application/x-www-form-urlencoded` | form body: `client_key`, `client_secret`, `code`, `grant_type=authorization_code`, `redirect_uri`, optional `code_verifier` | `TikTok.exchange_code` | `test_oauth_exchange_request_contract`, `test_oauth_exchange_request_includes_code_verifier_when_present` |
| Publish init | POST | `https://open.tiktokapis.com/v2/post/publish/video/init/` | `Authorization: Bearer <token>`, `Content-Type: application/json; charset=UTF-8` | JSON body with `post_info` and `source_info` (`source=FILE_UPLOAD`, `video_size`, `chunk_size`, `total_chunk_count`) | `TikTok.init_video_upload` | `test_init_video_upload_request_contract` |
| Video chunk upload | PUT | `upload_url` returned by init endpoint | `Content-Type`, `Content-Length`, `Content-Range` | raw video chunk bytes | `TikTok.upload_video_chunk` | `test_upload_video_chunk_headers_contract` |
| Publish status | POST | `https://open.tiktokapis.com/v2/post/publish/status/fetch/` | `Authorization: Bearer <token>`, `Content-Type: application/json; charset=UTF-8` | JSON body: `{ "publish_id": "..." }` | `TikTok.check_publish_status` | `test_check_publish_status_request_contract` |
| Account analytics | GET + POST + POST | `GET /v2/user/info/?fields=follower_count,following_count,likes_count,video_count`, then `POST /v2/video/list/?fields=id`, then `POST /v2/video/query/?fields=id,like_count,comment_count,share_count,view_count` | bearer on all calls; JSON content type for POSTs | list body: `{ "max_count": 20 }`; query body: `{ "filters": { "video_ids": [...] } }` | `TikTok.get_account_analytics` | `test_get_account_analytics_request_contract` |
| Post analytics | POST | `https://open.tiktokapis.com/v2/video/query/?fields=id,like_count,comment_count,share_count,view_count` | `Authorization: Bearer <token>`, `Content-Type: application/json; charset=UTF-8` | JSON body: `{ "filters": { "video_ids": ["<video_id>"] } }` | `TikTok.get_post_analytics` | `test_get_post_analytics_request_contract` |

Known subtle bug traps:
- Chunk uploads fail hard when `Content-Range` does not exactly match chunk size and total bytes.
- Publish flow is async: init/upload success still requires explicit status polling.
- Account analytics is a stitched 3-call flow; partial implementation of only one call yields misleading totals.

## Telegram

| Capability | Method | Path/query contract | Required headers | Payload shape | SDK function(s) | Contract test name(s) |
| --- | --- | --- | --- | --- | --- | --- |
| Token preflight check | GET | `https://api.telegram.org/bot{token}/getMe` | none | none | `Telegram.validate_access` | `test_validate_access_preflight_get_me` |
| Publish text message | POST | `https://api.telegram.org/bot{token}/sendMessage` | `Content-Type: application/json` | JSON body: `chat_id`, `text` | `Telegram.post_single` | `test_post_single_send_message_contract` |
| Publish photo | POST | `https://api.telegram.org/bot{token}/sendPhoto` | `Content-Type: application/json` | JSON body: `chat_id`, `photo`, optional `caption` | `Telegram.post_single` | `test_post_single_send_photo_contract` |
| Publish video | POST | `https://api.telegram.org/bot{token}/sendVideo` | `Content-Type: application/json` | JSON body: `chat_id`, `video`, optional `caption` | `Telegram.post_single` | `test_post_single_send_video_contract` |
| Sequential thread posting | POST (repeated) | repeated `sendMessage`/`sendPhoto`/`sendVideo` calls in order | `Content-Type: application/json` | per-item payload as above; stops on first hard failure and returns partial outcome if prior items succeeded | `Telegram.post_thread` | `test_post_thread_success`, `test_post_thread_partial_success_on_mid_failure`, `test_post_thread_first_failure_is_failure` |

Known subtle bug traps:
- Telegram can return HTTP 200 with `ok=false`; payload-level errors must be parsed and mapped.
- Bot tokens are embedded in path URLs; error surfaces must redact token-like values.
- Broadcast-only scope is enforced by rejecting likely DM targets (positive numeric chat IDs).

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
| Publish video (resumable) | POST + PUT | init `POST https://www.googleapis.com/upload/youtube/v3/videos?uploadType=resumable&part=snippet,status` then `PUT <Location upload URL>` | init: `Authorization`, `Content-Type: application/json`, `X-Upload-Content-Length`, `X-Upload-Content-Type`; upload: `Content-Type`, `Content-Length` | init JSON contains `snippet` and `status`; upload body is raw video bytes | `YouTube.post_single` | `test_video_upload_init`, `test_video_resumable_upload` |
| Account analytics | GET | `https://www.googleapis.com/youtube/v3/channels?part=id,snippet,statistics&mine=true` | `Authorization: Bearer <token>` | none | `YouTube.get_account_analytics` | `test_get_account_analytics_contract_and_parsing` |
| Post analytics | GET | `https://www.googleapis.com/youtube/v3/videos?part=id,snippet,statistics&id={video_id}` | `Authorization: Bearer <token>` | none | `YouTube.get_post_analytics` | `test_get_post_analytics_contract_and_parsing` |
| Account analytics (reports time-series) | GET | `https://youtubeanalytics.googleapis.com/v2/reports?ids=channel==MINE&startDate={YYYY-MM-DD}&endDate={YYYY-MM-DD}&metrics={csv}&dimensions=day` | `Authorization: Bearer <token>` | none | `YouTube.get_account_analytics_report` | `test_get_account_analytics_report_contract_and_parsing` |
| Post analytics (reports time-series) | GET | `https://youtubeanalytics.googleapis.com/v2/reports?ids=channel==MINE&startDate={YYYY-MM-DD}&endDate={YYYY-MM-DD}&metrics={csv}&dimensions=day&filters=video=={video_id}` | `Authorization: Bearer <token>` | none | `YouTube.get_post_analytics_report` | `test_get_post_analytics_report_contract_and_parsing` |
| Thumbnail upload | POST multipart | `https://www.googleapis.com/upload/youtube/v3/thumbnails/set?videoId={video_id}&uploadType=multipart` | `Authorization: Bearer <token>` | multipart part `media` with image bytes and explicit content type | `YouTube.build_thumbnail_upload_request`, `YouTube.upload_thumbnail` | `test_build_thumbnail_upload_request_contract`, `test_upload_thumbnail_contract` |

Known subtle bug traps:
- Resumable upload must return `Location` on init; missing header is a hard failure.
- Video publish helper auto-appends `#Shorts` in description and truncates title to platform-safe length.
- Thumbnail upload accepts only image content types (`jpeg/jpg/png/webp`) in current validation.
