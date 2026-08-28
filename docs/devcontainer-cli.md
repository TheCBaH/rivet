# Working with the devcontainer locally

This repository's CI builds and runs containers with `@devcontainers/cli`
(`devcontainer` on `PATH`), through
`.github/workflows/actions/devcontainer/action.yml`. Reproduce a CI leg with the
same `up` and `exec` operations rather than invoking `docker build` directly.

There are exactly two devcontainer configs in this repo:
`.devcontainer/devcontainer.json` (the main OCaml/CompCert/asm image, the
default `--config`) and `.devcontainer/devcontainers.cli/devcontainer.json`
(a small Debian-based meta-container whose only job is running
`@devcontainers/cli` itself via docker-outside-of-docker, used to drive the
other one from CI). There is no `javascript`/`octez`/`native-adapters`
feature-suite split — everything the main image needs (js_of_ocaml, Melange,
Rocq/`rocq-prover`, ...) is installed by the one `./features/ocaml` feature
invocation in `.devcontainer/devcontainer.json`.

## The image is Gentoo-based

`.devcontainer/Dockerfile` builds on `gentoo/stage3`, not Debian. Two things
follow from that:

- `devcontainer.json` uses
  `ghcr.io/thecbah/devcontainer.features/common-utils:2.5.10`, published
  from a real GitHub fork of `devcontainers/features`
  (https://github.com/TheCBaH/devcontainer.features, branch
  `gentoo-common-utils`; a PR to send this upstream is pending), not the
  upstream feature. Upstream hard-fails on any `/etc/os-release` `ID` outside
  debian/rhel/alpine (`main.sh` ends its distro-detection `if`/`elif` chain
  with `echo "Linux distro ${ID} not supported."; exit 1`), so it doesn't
  work on Gentoo unmodified. The fork's branch adds a `"gentoo"` branch to
  that detection and a matching `install_gentoo_packages()` (portage-atom
  equivalents of the Debian/Alpine package lists, verified against a synced
  portage tree rather than guessed) plus the rc-file path Gentoo actually
  uses (`/etc/bash/bashrc`; Gentoo's zsh already matched upstream's
  non-RHEL `/etc/zsh/zshrc` default). The user/group creation and sudoers
  setup were already fully distro-generic (`useradd`/`groupadd`, no
  branching) and needed no changes. See
  `src/common-utils/NOTES.md` on that branch for the full diff from
  upstream before updating either, so a future upstream bump doesn't
  silently drop the Gentoo branch. Publishing a new version: push the
  updated branch to `main` on the fork (`git push origin
  gentoo-common-utils:main`) and run `gh workflow run release.yaml --repo
  TheCBaH/devcontainer.features --ref main` — GitHub disables
  push-triggered Actions on a freshly forked repo until a workflow has run
  once via manual dispatch, and a personal-account `gh auth token` lacks
  the `write:packages` scope `devcontainer features publish` needs, so the
  Actions-run publish (using the auto-scoped `GITHUB_TOKEN`) is the
  reliable path, not the CLI directly.
- `./features/ocaml/install.sh` detects the package manager (`apt-get` vs.
  `emerge`) and branches internally, so the same feature config works on
  both this Gentoo image and a Debian/Ubuntu one. `system-packages` in
  `devcontainer.json` is still spelled with Debian package names (e.g.
  `libgmp-dev pkg-config`); on the Gentoo path the script translates known
  names to portage atoms (`dev-libs/gmp`, `dev-util/pkgconf`) via a small
  table in `install.sh`, and passes anything unmapped through as-is with a
  warning. Add new entries to that table (not to `devcontainer.json`) when a
  future `system-packages` addition needs a Gentoo-side name that differs
  from its Debian one.

Beyond `opam`/OCaml and `git`, the image's `RUN` also builds all six
cross toolchains (`i686-linux-gnu`, `x86_64-linux-gnu`,
`arm-linux-gnueabihf`, `aarch64-linux-gnu`, `riscv32-linux-gnu`,
`riscv64-linux-gnu`) via `crossdev`, `app-emulation/qemu` with
`QEMU_USER_TARGETS="i386 x86_64 arm aarch64 riscv32 riscv64"` and
`QEMU_SOFTMMU_TARGETS="arm x86_64"`, `dev-debug/gdb` (Gentoo's gdb is
multi-arch out of the box — no separate `gdb-multiarch` package needed),
and Node.js (upstream prebuilt tarball, matching the old Debian image's
approach, since portage's `net-libs/nodejs` version lags the pinned
`NODE_VERSION`).

Each `crossdev -t <triple>` call passes `--env 'MAKEOPTS="-j2"'`: the
Dockerfile's own global `MAKEOPTS` (`-j$(nproc)`) OOM-kills `cc1plus` while
building gcc's machine-generated matcher files on a memory-constrained
host (observed: `-j8` on ~4.5-6GB free RAM). `--env` (not `--genv`) scopes
the override to every crossdev stage — binutils, gcc *and* glibc — not
just gcc. Caches (`/var/cache/distfiles`, `/var/cache/binpkgs`,
`/var/tmp/portage`) are cleared after each triple, inside the same `RUN`
instruction that created them, so the intermediate build trees never become
part of a committed image layer.

`riscv32-linux-gnu` is a genuinely new addition (the old Debian image only
had `riscv64-linux-gnu` binutils+gcc, no libc) — it gives `asm`'s riscv32
profile a real glibc sysroot, which is what let it join `LIBC_SMOKE_TARGETS`
(see `asm/tools/lib/target.ml`'s `Libc_smoke` capability set).

## Select the OCaml version and container

The OCaml feature reads `${localEnv:OCAML_VERSION}` while the image is built, so
set it in the shell that runs `devcontainer up`. This repo's default (used when
unset) is `4.14.3`:

```sh
export OCAML_VERSION=4.14.3
devcontainer up --workspace-folder .
```

The versions and platforms CI actually builds are defined in
`.github/workflows/ocaml-versions.yml`: `4.14.3`, `5.2.1`, `5.3.0`, `5.4.1`,
`5.5.0`, each on `linux/amd64` and `linux/arm64`; `4.14.3` additionally on
`linux/i386` and `linux/arm/v7`. There is no s390x leg in this repo's CI.

CI also passes `--platform <docker-platform>` while building. Use that locally
only when Docker has the required native host or binfmt emulation. Its generated
`exec` prefix exports the matching `PLATFORM` as well, which the Makefile reads
(see `compcert-configure`'s comment) to pick a target when `uname -m` would be
unreliable (e.g. a 32-bit container on a 64-bit host):

```sh
devcontainer exec --remote-env PLATFORM=linux/arm64 \
  --workspace-folder . make asm-build
```

## Run checks

This repo has no top-level `ci`/`bench`/`build` Make targets. The real
equivalents:

```sh
devcontainer exec --workspace-folder . make compcert       # configure + Rocq proof + build ccomp
devcontainer exec --workspace-folder . make asm-ci          # the asm/ subproject's CI aggregate
```

`make asm-ci` bundles the pure-OCaml/dune checks that need no cross toolchain
or QEMU (`asm-fmt-check`, `asm-build`, `asm-test`, `tools-test`,
`tools-integration`, `tools-boundary`, `tools-matrix-diff`,
`tools-fixture-modes`, `tools-oracle-diff`, `tools-gasxref-diff`,
`asm-corpus-check-c`, `asm-purity`, `asm-planted`, `asm-cross-smoke-selftest`,
`asm-characterize-verify`, `asm-melange-optin`, `asm-js-portable`). It does
**not** include the toolchain-dependent targets CI runs as separate jobs:

```sh
devcontainer exec --workspace-folder . make asm-tool-gate                 # six qemu-user binaries + cross toolchains + gdb
devcontainer exec --workspace-folder . make asm-exec                      # runs test/oracle/exec.exe under QEMU per target
devcontainer exec --workspace-folder . make asm-fixture-oracle-riscv32    # one of the six asm/CompCert targets
devcontainer exec --workspace-folder . make asm-libc-cross-smoke          # full CompCert cross-builds, the libc-smoke target set
```

`tools/target-matrix.sh` (generated — see its header, regenerate with
`make tools-matrix` after editing `asm/tools/lib/target.ml`) is the single
source of truth for the six target profiles (`x86_32 x86_64 arm aarch64
riscv32 riscv64`) and which capability sets they belong to
(`ASSEMBLER_TARGETS`/`FIXTURE_TARGETS` are all six; `LIBC_SMOKE_TARGETS` is a
subset — check that file rather than assuming, it changes as riscv32 gains a
real libc toolchain).

`devcontainer up` is idempotent and reuses a container keyed by its workspace
and configuration. The generated `.devcontainer/devcontainer-lock.json` files
are feature-resolution artifacts and should not be edited manually.

## Nested devcontainer mount caveat (docker-outside-of-docker)

If you're running `devcontainer` commands from *inside* a devcontainer that
itself uses docker-outside-of-docker (bind-mounting the host's
`docker.sock`, as `.devcontainer/devcontainers.cli/devcontainer.json` does,
or as a Claude Code / CI agent container might) — not from a real host —
`devcontainer up`/`exec`'s bind mount breaks in a specific way: the daemon
you're talking to is the *host's* real dockerd, so it resolves the
workspace bind-mount's `source=` path in the *host's* filesystem namespace,
not the calling container's. `--workspace-folder /workspaces/err-trace`
(the path as the CLI process itself sees it, correct for reading
`devcontainer.json`/`Dockerfile`) gets reused as that `source=` too by
default — but the host's real filesystem has no such path, so the new
container ends up bind-mounting an empty/auto-created directory: no `.git`,
no `.devcontainer/fix-opam-owner.sh`, `postCreateCommand` fails immediately.
`devcontainer build` is unaffected (it uploads a tar context over the
Docker API, not a host bind-mount) — only `up`/`exec`'s live workspace mount
breaks.

Check whether you're in this situation at all:

```sh
docker inspect "$(hostname)" >/dev/null 2>&1 && echo "running inside a container with docker access" || echo "not nested (or no docker access) — plain devcontainer up --workspace-folder . just works"
```

If nested, discover the *real* host path — don't hardcode it, it's
per-machine/per-checkout. `devcontainer` labels every container it creates
with `devcontainer.local_folder`, so ask the host's dockerd (via the shared
socket) what it thinks your own container's workspace path is:

```sh
HOST_WS="$(docker inspect "$(hostname)" --format '{{index .Config.Labels "devcontainer.local_folder"}}')"
```

You can't just pass `$HOST_WS` as `--workspace-folder`, though — the CLI
also uses that value as a literal `cwd` for spawning `docker`/`git`, which
must exist in *your own* (calling) container's filesystem, not the host's.
So keep `--workspace-folder` as the in-container path, and instead inject
an explicit `workspaceMount`/`workspaceFolder` naming the discovered host
path via a generated `--override-config` — kept inside `.devcontainer/` so
its relative `dockerfile`/feature paths still resolve, and never committed:

```sh
HOST_WS="$(docker inspect "$(hostname)" --format '{{index .Config.Labels "devcontainer.local_folder"}}')"
OVERRIDE=.devcontainer/devcontainer.override.local.json
cp .devcontainer/devcontainer.json "$OVERRIDE"
sed -i "s#\"name\": \"CompCert devcontainer\",#&\n    \"workspaceFolder\": \"/workspaces/err-trace\",\n    \"workspaceMount\": \"source=${HOST_WS},target=/workspaces/err-trace,type=bind,consistency=cached\",#" "$OVERRIDE"

export OCAML_VERSION=4.14.3
devcontainer up --workspace-folder /workspaces/err-trace --override-config "/workspaces/err-trace/$OVERRIDE"
devcontainer exec --workspace-folder /workspaces/err-trace --override-config "/workspaces/err-trace/$OVERRIDE" \
  bash -lc 'opam --version && opam exec -- ocamlc -config | head -5 && opam exec -- dune --version'

rm "$OVERRIDE"   # scratch file — don't commit it
```

This was exercised end-to-end (`up`, `postCreateCommand`, `exec`) from
inside this repo's own `jolly_keller`-style nested session to validate the
Gentoo migration, since a plain `devcontainer up --workspace-folder .` run
that way silently mounts the wrong thing. On a real (non-nested) host, skip
all of this — plain `devcontainer up --workspace-folder .` already mounts
correctly, since there's no host/container path split to work around.

Check before trusting a validation run:

```sh
devcontainer exec --workspace-folder . bash -lc \
  'test -f Makefile && command -v opam && opam exec -- dune --version'
```

### When `up` itself fails

With an empty mount, `devcontainer up` does not merely produce a container with
no sources: it fails, because `postCreateCommand` runs a script that lives in
the workspace.

```json
{"outcome":"error","message":"Command failed: /bin/sh -c sudo .devcontainer/fix-opam-owner.sh $(id -u) $(id -g)","description":"opam of postCreateCommand from devcontainer.json failed.","containerId":"a3e60f1cb101..."}
```

The container named by `containerId` was created and is usable; only the
post-create step failed. Because `up` never completed, `devcontainer exec`
cannot drive it, so use `docker exec` and finish the setup by hand. Do what
`fix-opam-owner.sh` would have done — the image bakes `OPAMROOT` ownership as
uid 1000, while `updateRemoteUserUID` remaps the container's `vscode` user to
the host uid:

```sh
CONTAINER=<containerId from the up failure>
docker exec -u root "$CONTAINER" chown -R vscode:vscode /opt/opam
```

`docker exec` does not inherit the devcontainer environment — the container's
`Config.Env` carries only `PATH` — so pass `OPAMROOT` on every call:

```sh
docker exec -u vscode -e OPAMROOT=/opt/opam "$CONTAINER" \
  sh -lc 'opam exec -- dune --version'
```

### Validate a disposable snapshot

If the source is not mounted, validate a copy instead. Create a temporary
directory inside the target container, stream the working tree into it
(excluding local build and archive artifacts), and run checks there:

```sh
SNAPSHOT_PATH=$(devcontainer exec --workspace-folder . \
  mktemp -d /tmp/err-trace-review.XXXXXX | tail -n 1)
tar --exclude=.git --exclude=_build --exclude='*.tar.gz' -cf - . \
  | devcontainer exec --workspace-folder . \
      tar -xf - -C "$SNAPSHOT_PATH"
devcontainer exec --remote-env SNAPSHOT_PATH="$SNAPSHOT_PATH" \
  --workspace-folder . bash -lc 'cd "$SNAPSHOT_PATH" && make asm-ci'
```

When `up` failed as above, the same recipe works with `docker exec`, reading the
tree from its real path in this container rather than from `.`:

```sh
SNAP=$(docker exec -u vscode "$CONTAINER" mktemp -d /tmp/err-trace-review.XXXXXX | tr -d '\r')
tar --exclude=.git --exclude=_build --exclude='*.tar.gz' \
  -cf - -C /workspaces/err-trace . \
  | docker exec -i -u vscode "$CONTAINER" tar -xf - -C "$SNAP"
docker exec -u vscode -e OPAMROOT=/opt/opam "$CONTAINER" \
  sh -lc "cd $SNAP && make asm-ci"
```

Use a newly created path for each independent snapshot so stale files cannot
mask deletions. This is only a validation copy; make source edits in the shared
workspace.

## Container lifecycle

Prefer rerunning `devcontainer up` over deleting a container. If a genuinely
clean rebuild is necessary, first identify the exact container with
`docker ps`, then remove only that container explicitly.

Disk is usually the binding constraint for local `devcontainer build`/`up`
iteration, not CPU or network: `docker system df` shows where it's going, and
`docker builder prune -f`/`docker image prune -f` reclaim build-cache and
dangling-image space without touching any tagged, still-referenced image or
another checkout's container.
