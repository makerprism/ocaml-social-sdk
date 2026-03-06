# Agent Workspace Rules

These rules apply to all coding agents working in this repository.

## Purpose

This is an OCaml SDK for social media platform APIs (Twitter, LinkedIn, Bluesky, Mastodon, Facebook, Instagram, YouTube, Pinterest, Reddit, TikTok, Telegram). Runtime-agnostic design works with Lwt, Eio, or sync code.

**Scope:** Write-side (posting), OAuth flows, and limited read/analytics. See `README.md` for platform feature matrix.

## Building

```bash
dune build
```

If the build complains about missing dependencies, regenerate the lockfile first:

```bash
dune pkg lock
dune build
```

## Dune Lockfile Workflow

- If dependencies may have changed, the lockfile may be stale, or before running broader Dune operations after dependency-related edits, run:
  - `dune pkg lock`
- Treat `dune pkg lock` as the first recovery/sync step in that situation.
- Do not manually edit or manipulate files under `dune.lock/`.
- The only supported way to update `dune.lock/` is to regenerate it via `dune pkg lock`.

## Dependency Policy

- `dune` is not a declared project dependency and must never be added to package dependency lists in `dune-project`.

## Build Directory Safety

- Never run `dune clean`.
- Never delete or modify `_build/` manually (including `rm -rf _build`, partial deletes, or scripted cleanup).

## Security and Sensitive Data

- Never print or commit real OAuth tokens, API keys, app secrets, or user credentials.
- Redact token-like strings in logs and errors.
- Treat user content as sensitive; prefer minimal structured logs.

## Upstream PR Workflow

- Canonical upstream is `makerprism/ocaml-social-sdk`.
- Before opening/updating a PR to `upstream/main`, rebase your branch onto `upstream/main`.
- If already pushed, update with `git push --force-with-lease`.

## Documentation Maintenance

Docs that don't evolve with code become lies. Stale docs are bugs.

**After completing significant work:**

1. If you added a new platform package, update `README.md` packages table and feature matrix
2. If you changed API signatures, update usage examples in README
3. If you fixed a bug that was documented as a limitation, update the status indicator (⚠️ → ✅)
4. If you added/changed HTTP contracts, update `docs/parity-http-matrix.md`

**Rules:**
- Update docs in the same PR as code changes — never as a separate task
- README.md is the primary doc; keep it accurate
- The feature matrix in README.md is the source of truth for platform capabilities

## Definition of Done

- [ ] `dune build` passes
- [ ] Affected README sections updated (including feature matrix if applicable)
- [ ] `docs/parity-http-matrix.md` updated if HTTP contracts changed
- [ ] You can explain the change in one sentence
