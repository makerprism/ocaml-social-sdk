# social-lwt

Lwt adapters and HTTP client for social-core.

> **Warning:** This library is not production-ready. It was primarily built using LLMs and is under active development. Expect breaking changes.

## Features

- Convert CPS-style interfaces to Lwt promises
- Cohttp-based HTTP client implementation
- `Lwt_adapter.sleep`: a non-blocking `Social_core.SLEEP` for provider configs (retry backoff, media polling)
- Easy integration with Lwt-based applications

## Usage

```ocaml
open Lwt.Syntax

(* Use the Cohttp HTTP client *)
module Http = Social_provider_lwt.Cohttp_client.Make

(* Convert to Lwt-style interface *)
module Http_lwt = Social_provider_lwt.Lwt_adapter.Http_to_lwt(Http)

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
