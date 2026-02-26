# Agent Workspace Rules

These rules apply to all coding agents working in this repository.

## Building

```bash
dune build
```

If the build complains about missing dependencies, regenerate the lockfile first:

```bash
dune pkg lock
dune build
```

`dune pkg lock` is local workflow support only in this repository. Do not commit lockfiles.

## Dune Lockfile Workflow

- This repository is a library workspace: `dune.lock/` must not be tracked in git.
- Treat `dune pkg lock` outputs as ephemeral local artifacts.
- If dependencies may have changed, the lockfile may be stale, or before running broader Dune operations after dependency-related edits, run `dune pkg lock` locally.
- Do not manually edit or manipulate files under any `dune.lock/` directory.
- Never stage or commit `dune.lock/` changes (root, packages, examples, or any nested workspace).

## Dependency Policy

- `dune` is not a declared project dependency and must never be added to package dependency lists in `dune-project`.

## Build Directory Safety

- Never run `dune clean`.
- Never delete or modify `_build/` manually (including `rm -rf _build`, partial deletes, or scripted cleanup).
- If build artifacts seem inconsistent, do not clean `_build/`; use non-destructive steps instead (for example, re-run targeted commands and report issues).
