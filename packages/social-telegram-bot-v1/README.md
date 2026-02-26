# social-telegram-bot-v1

Telegram Bot API connector for broadcast posting to channels and groups.

## Scope

- Broadcast posting only (`sendMessage`, `sendPhoto`, `sendVideo`)
- Channel/group targets only (direct-message style targets are rejected)
- No webhook polling/update handling
- No bot command framework

## Installation

```bash
opam install social-telegram-bot-v1
```

## Usage

```ocaml
open Social_telegram_bot_v1

module Telegram = Telegram_bot_v1.Make (Your_config)

let () =
  Telegram.post_single
    ~account_id:"account123"
    ~text:"Hello from OCaml"
    ~media_urls:[]
    (fun outcome ->
      match outcome with
      | Error_types.Success message_id ->
          Printf.printf "Posted message id=%s\n" message_id
      | Error_types.Failure err ->
          Printf.printf "Error: %s\n" (Error_types.error_to_string err)
      | Error_types.Partial_success _ -> ())
```

Your config module must resolve account targets via `get_chat_id` and return a channel/group chat id.

## Notes

- Bot token is read from `credentials.access_token`.
- `post_thread` is deterministic and sequential; it stops at first failure.
- Token redaction is applied to error surfaces.
