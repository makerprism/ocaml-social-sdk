# Contributing to ocaml-social-sdk

Thank you for your interest in contributing!

## Development Setup

1. Clone the repository:
   ```bash
   git clone https://github.com/makerprism/ocaml-social-sdk.git
   cd ocaml-social-sdk
   ```

2. Install dependencies:
   ```bash
   opam install . --deps-only --with-test
   ```

3. Build:
   ```bash
   make build
   # or
   dune build
   ```

4. Run tests:
   ```bash
   make test
   # or
   dune runtest
   ```

## Project Structure

```
ocaml-social-sdk/
├── packages/
│   ├── social-provider-core/     # Core abstractions
│   ├── social-provider-lwt/      # Lwt runtime
│   ├── social-twitter-v1/        # Twitter v1.1 API
│   ├── social-twitter-v2/        # Twitter v2 API
│   ├── social-bluesky-v1/        # Bluesky API
│   ├── social-linkedin-v2/       # LinkedIn API
│   ├── social-mastodon-v1/       # Mastodon API
│   ├── social-facebook-graph-v21/# Facebook Graph API
│   ├── social-instagram-graph-v21/# Instagram Graph API
│   ├── social-youtube-data-v3/   # YouTube Data API
│   ├── social-pinterest-v5/      # Pinterest API
│   └── social-tiktok-v1/         # TikTok API
├── dune-project
├── dune-workspace
└── Makefile
```

## Adding a New Platform

1. Create a new directory: `packages/social-<platform>-v<version>/`
2. Add `dune-project` with package metadata
3. Implement the platform SDK in `lib/`
4. Add tests in `test/`
5. Update the root README

## Code Style

- Follow OCaml conventions
- Use meaningful names
- Add documentation comments for public APIs
- Keep functions small and focused
- Handle errors explicitly with Result types

## Pull Request Process

1. Create a feature branch
2. Make your changes
3. Ensure tests pass
4. Update documentation if needed
5. Submit a PR with a clear description

## Releasing

Releases are created by pushing tags in the format `<package>@<version>`:

```bash
git tag social-twitter-v2@0.2.0
git push origin social-twitter-v2@0.2.0
```

This triggers the release workflow automatically.
