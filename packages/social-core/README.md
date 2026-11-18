# social-core

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

### Module naming

`social-core` is `(wrapped false)`: every module name lands in the global
namespace of each consumer. **Modules in an unwrapped library must carry the
package prefix (`Social_`)**, e.g. `Social_oauth_pkce`, never `Oauth_pkce`. A
generic name clashes with an application module of the same name. The
pre-prefix names `Secure_random`, `Oauth_pkce`, `Meta_graph_error`,
`Provider_error_detail` and `Meta_graph_paging` have been removed; use the corresponding `Social_`-prefixed modules.

## Usage

This package is meant to be used as a dependency for provider implementations. End users should use:

- Runtime adapters: `social-lwt`, `social-provider-eio`
- Provider packages: `social-twitter-v2`, `social-bluesky-v1`, etc.

## Example

```ocaml
(* Implement the HTTP_CLIENT interface with your HTTP library *)
module My_http_client : Social_core.HTTP_CLIENT = struct
  let get ?headers url on_success on_error =
    (* Your HTTP implementation here *)
    ...
end

(* Use with a provider *)
module Twitter = Social_twitter_v2.Make(struct
  module Http = My_http_client
  (* Providers that back off or poll (Threads, Twitter, Pinterest, Mastodon)
     also need a non-blocking timer, see [Social_core.SLEEP]. *)
  let sleep = Social_lwt.Lwt_adapter.sleep
  (* ... other config ... *)
end)
```

## License

MIT
