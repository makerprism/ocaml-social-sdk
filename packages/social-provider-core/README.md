# social-provider-core

Core interfaces for building social media API clients in OCaml.

> **Warning:** This library is not production-ready. It was primarily built using LLMs and is under active development. Expect breaking changes.

## Features

- **Runtime-agnostic**: Works with Lwt, Eio, Async, or synchronous code
- **HTTP-client-agnostic**: Use Cohttp, Curly, Httpaf, or any HTTP client
- **Zero async dependencies**: Pure CPS-style interfaces
- **Minimal dependencies**: Only `yojson` and `re`

## Architecture

This package provides the core interfaces and types that all social media provider implementations depend on. It uses continuation-passing style (CPS) to avoid locking into any specific async runtime or HTTP client library.

### Core Interfaces

- `HTTP_CLIENT`: Abstract HTTP client interface
- `STORAGE`: Abstract storage operations for media
- `CONFIG`: Abstract configuration and credential management

### Utility Modules

- `Platform_types`: Common types for social platforms
- `Content_validator`: Validate text and media for platforms
- `Thread_splitter`: Split content into thread posts
- `Url_extractor`: Extract URLs from text

## Usage

This package is meant to be used as a dependency for provider implementations. End users should use:

- Runtime adapters: `social-provider-lwt`, `social-provider-eio`
- Provider packages: `social-twitter-v2`, `social-bluesky-v1`, etc.

## Example

```ocaml
(* Implement the HTTP_CLIENT interface with your HTTP library *)
module My_http_client : Social_provider_core.HTTP_CLIENT = struct
  let get ?headers url on_success on_error =
    (* Your HTTP implementation here *)
    ...
end

(* Use with a provider *)
module Twitter = Social_twitter_v2.Make(struct
  module Http = My_http_client
  (* ... other config ... *)
end)
```

## License

MIT
