# Investigating a failed CI run

This repo's Actions runs are on `TheCBaH/devcontainer.CompCert`. `gh` is
authenticated in this environment; these are the commands that actually work
here, including a couple of `gh` quirks worth knowing up front.

## From a run or job URL

A run URL looks like
`.../actions/runs/<run-id>/job/<job-id>`. Both IDs are usable directly:

```sh
gh run view <run-id> --repo TheCBaH/devcontainer.CompCert
```

This prints every job's status plus the `ANNOTATIONS` section (exit codes,
which step failed) — usually enough to tell which job(s) to dig into.

## Getting full logs for one job

`gh run view --job <job-id> --log` returned **empty output** in this repo
even for a real, completed, failed job — don't trust a blank result as "no
logs". Go straight to the API instead, which reliably returns the full raw
timestamped log:

```sh
gh api repos/TheCBaH/devcontainer.CompCert/actions/jobs/<job-id>/logs \
  > /tmp/job.log
```

Then grep for the failure:

```sh
grep -n "##\[error\]\|FAIL\|Error " /tmp/job.log
```

Each workflow step's output is delimited by `##[group]Run <step>` lines, so
searching for those first narrows down which step's output to read in full.

## Job IDs and conclusions for a run

```sh
gh run view <run-id> --repo TheCBaH/devcontainer.CompCert \
  --json jobs -q '.jobs[] | select(.conclusion=="failure") | .name'

gh run view <run-id> --repo TheCBaH/devcontainer.CompCert \
  --json jobs -q '.jobs[] | select(.name=="tool-gate") | .databaseId'
```

## Checking whether a failure is new or a repeat

```sh
gh run list --repo TheCBaH/devcontainer.CompCert --workflow asm --limit 15 \
  --json databaseId,conclusion,headBranch,createdAt
```

Then pull the same job across a few runs to see if the same check fails the
same way every time (a real bug) vs. intermittently (flakiness/resource
contention). `gh api .../jobs/<id>/logs` for each `databaseId` found this
way, as above.

## Reproducing locally

Once you know which `make` target failed (from the `##[group]Run
devcontainer exec ... make <target>` line), reproduce it inside the same
container setup — see `.ai/devcontainer-cli.md`. Match the job's env
exactly: `OCAML_VERSION` (top of the failed job's YAML step, e.g.
`OCAML_VERSION: 5.5.0`) and `PLATFORM` (from the matrix entry, e.g.
`linux/amd64`).

```sh
export OCAML_VERSION=5.5.0
devcontainer up --workspace-folder .
devcontainer exec --remote-env PLATFORM=linux/amd64 --workspace-folder . \
  make asm-tool-gate
```

If a gate script suppresses a sub-process's stderr on the success path (e.g.
`tools/asm-tool-gate.sh`'s `gdb_gate` redirects `qemu-system-aarch64`'s
stderr to a log file that's never printed), rerun the failing piece by hand
with output unsuppressed rather than trying to infer the cause from the
gate's one-line failure message.

## Notes specific to this repo

- Every `asm` job step is `${{ env.EXEC }} make <target>`, where `EXEC` is
  computed once in the `devcontainer` action step and captured by the
  `setup` step — so the exact reproduction command for any step is always
  `<EXEC value from the setup step> make <target from the failing step>`.
- `tool-gate`, `abi-conform`, and each `asm` matrix leg build/start their
  *own* container — a failure isolated to one OCaml version or platform
  won't reproduce on a different one.
