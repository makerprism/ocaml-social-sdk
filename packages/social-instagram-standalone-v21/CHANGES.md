# Unreleased

- `CONFIG` inherits the social-core change: `encrypt` / `decrypt` are no longer required members (no SDK operation called them). Configurations that still define them keep compiling.

# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.1] - Unreleased

### Added

- **`Make_nonblocking (Config : NONBLOCKING_CONFIG)`**: the provider with
  container polling waiting through `Social_core.SLEEP`
  (`sleep : seconds:float -> (unit -> unit) -> (string -> unit) -> unit`),
  which a cooperative runtime implements without blocking. With Lwt:
  `let sleep = Social_lwt.Lwt_adapter.sleep`. A timer the runtime cannot
  schedule fails the post. `BASE_CONFIG` holds everything else `CONFIG`
  required.

### Deprecated

- **This package is deprecated.** Meta makes Graph API v21.0 unavailable on 2027-01-21. Use `social-instagram-standalone-v25`. The package stays and still builds; it gets fixes only.
- **`CONFIG.sleep : float -> (unit -> 'a) -> 'a` and `Make`.** The
  polymorphic return type can only be implemented by blocking the calling
  thread, which freezes an Lwt or Eio program for the whole backoff (up to
  13 s per poll). `Make` keeps working unchanged and now delegates to
  `Make_nonblocking`; migrate by replacing `sleep` and the functor name.

