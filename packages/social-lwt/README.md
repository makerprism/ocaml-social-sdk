# social-lwt

Lwt adapters and HTTP client for social-core.

> **Warning:** This library is not production-ready. It was primarily built using LLMs and is under active development. Expect breaking changes.

## Features

- Convert CPS-style interfaces to Lwt promises
- Cohttp-based HTTP client implementation, optionally with a per-request deadline (`Cohttp_client.Make_with_options`)
- `Lwt_adapter.sleep`: a non-blocking `Social_core.SLEEP` for provider configs (retry backoff, media polling)
- Easy integration with Lwt-based applications

## Usage

```ocaml
open Lwt.Syntax

(* Use the Cohttp HTTP client *)
module Http = Social_lwt.Cohttp_client.Make

(* Or give every request a deadline (connect, headers and body). A request
   that misses it is cancelled and its error continuation receives
   "Request timed out after 10s"; providers report it as a Network_error. *)
module Http_10s = Social_lwt.Cohttp_client.Make_with_options (struct
  let timeout_seconds = Some 10.0
end)

(* Convert to Lwt-style interface *)
module Http_lwt = Social_lwt.Lwt_adapter.Http_to_lwt(Http)

(* Now use with providers *)
let%lwt response = Http_lwt.get "https://api.example.com/endpoint" in
Lwt.return response

(* Provider configs take the runtime's timer *)
module Threads = Social_threads_v1.Make (struct
  (* ... Http, credentials, etc. ... *)
  let sleep = Social_lwt.Lwt_adapter.sleep
end)
```

## License

MIT
