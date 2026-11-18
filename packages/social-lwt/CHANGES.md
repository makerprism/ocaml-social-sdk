# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.1] - Unreleased

### Fixed

- Route exceptions raised by sleep continuations to `on_error`, avoiding Lwt’s default process-exiting async exception hook.

- Close each Cohttp connection on timeout, transport failure, and completion, including stalls before headers and while reading the body.

### Added

- `Cohttp_client.Make_with_options (Options : OPTIONS)`: the Cohttp client with `timeout_seconds : float option`, a deadline for each request covering connect, headers and the whole body. A request that misses it is cancelled and reports `Cohttp_client.timeout_message seconds` ("Request timed out after 10s") to its error continuation. `Cohttp_client.Make` is now `Make_with_options (Default_options)` (no deadline) and behaves as before.
- `Lwt_adapter.sleep`, a non-blocking `Social_core.SLEEP` (backed by `Lwt_unix.sleep`) for provider configs.
- Lwt adapter for converting CPS-style interfaces to Lwt promises
- `Cohttp_client` module providing HTTP client implementation using Cohttp_lwt_unix
- `Lwt_adapter` module for wrapping CPS functions as Lwt.t values
- Integration with social-core interfaces
