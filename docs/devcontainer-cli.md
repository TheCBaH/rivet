# Working the devcontainer locally

This repo's CI builds and drives the container with the `@devcontainers/cli`
(`devcontainer` on `$PATH`), never plain `docker build`/`docker run` — see
`.github/workflows/actions/devcontainer/action.yml`. Reproducing a CI leg
locally means running the same three verbs it does: `build`, `up`, `exec`.

## Prerequisites

```sh
npm install -g @devcontainers/cli   # already on PATH in this devcontainer
docker ps                            # confirm the daemon is reachable
```

## The OCaml version matters

`.devcontainer/devcontainer.json`'s `ocaml` feature reads its version from
`${localEnv:OCAML_VERSION}`, so it must be exported in the *host* shell
before calling `devcontainer`, not passed as a container env var:

```sh
export OCAML_VERSION=5.5.0   # one of: 4.14.3 5.2.1 5.3.0 5.4.1 5.5.0
                              # (see .github/workflows/ocaml-versions.yml)
```

Melange is only installable on the 4.14.3 image (`ocaml >= 4.14 & < 4.15`);
the `asm-melange-optin` gate and the melange leg specifically need that
version.

## Bring the container up

```sh
devcontainer up --workspace-folder .
```

- First build is slow: expect 15-20 minutes (CI's `devcontainer` step took
  ~15m53s-18m for `tool-gate`/`asm` jobs). Run it with `nohup ... &` or
  `run_in_background` rather than a foreground command with a short timeout.
- `up` builds the image if needed, then starts/reuses a container keyed off
  the workspace path + devcontainer.json content. Re-running `up` after a
  Dockerfile/devcontainer.json edit will rebuild only the changed layers.
- `.devcontainer/devcontainer-lock.json` is a generated-by-`up` artifact
  (feature version lockfile), not something to hand-edit.
- CI additionally passes `--platform <docker-platform>` and pre-seeds
  `--cache-from` a pushed `ghcr.io` image (see the action). Locally, without
  `--platform`, you get the host's native platform, which is enough for
  investigating amd64-only failures (e.g. `tool-gate`, `abi-conform`). To
  reproduce a `linux/i386` or `linux/arm/v7` leg on an amd64 host you need
  cross-platform emulation set up for Docker (binfmt/buildx) — prefer
  reasoning from the CI log first; only reach for a local cross build if the
  failure doesn't reproduce natively.

## Run commands inside it

```sh
devcontainer exec --workspace-folder . <command...>
```

CI always sets `PLATFORM` explicitly (`--remote-env PLATFORM=linux/amd64`,
etc.) because `uname -m` inside a 32-bit container on a 64-bit host reports
the *host's* arch, not the container's — several Makefile targets
(`compcert-configure`, cross-toolchain selection) key off `$PLATFORM`. Match
that when reproducing a specific leg:

```sh
devcontainer exec --remote-env PLATFORM=linux/amd64 --workspace-folder . \
  make asm-tool-gate
```

For a leg pinned to a non-default `devcontainer.json` (rare in this repo),
add `--config <path>` to both `up` and `exec`, matching the action's
`config` input.

## Useful `make` targets to run this way

All of these are plain `make <target>` run through `exec`, exactly as CI's
`EXEC` env var does (`${{ env.EXEC }} make ...` in `asm.yml`):

- `asm-fmt-check`, `asm-build`, `asm-fixtures-check`, `asm-test`,
  `asm-purity`, `asm-planted`, `asm-js-portable`, `asm-melange-optin` — the
  per-leg `asm` job steps.
- `asm-helpers`, `asm-exec` — the `abi-conform` job.
- `asm-tool-gate` — the `tool-gate` job (see `.ai/ci-triage.md` for how to
  debug a failure in this one specifically).

## Tearing down

```sh
docker ps                      # find the container devcontainer up created
docker rm -f <container>       # only if you want a clean rebuild
```

`devcontainer up` is idempotent and safe to re-run instead of tearing down;
prefer that unless the image itself needs to be rebuilt from scratch.
