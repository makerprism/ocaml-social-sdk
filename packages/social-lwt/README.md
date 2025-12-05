# social-lwt

Lwt adapters and HTTP client for social-core.

> **Warning:** This library is not production-ready. It was primarily built using LLMs and is under active development. Expect breaking changes.

## Features

- Convert CPS-style interfaces to Lwt promises
- Cohttp-based HTTP client implementation
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
```

## License

MIT
