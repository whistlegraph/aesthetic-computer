# Native Aesel

`full` runs the complete Aesel TUI inside a two-thread C terminal host. Bun keeps
providers, tools, artifacts, Markdown, approvals, history, publishing, settings,
recovery and reify. The host forwards the terminal protocol, including mouse and
resize events; there is no second implementation of those features.

```sh
bun aesel/bin/build-bun.mjs
make -C aesel/experiments/c-tui
aesel/experiments/c-tui/full
```

Normal Aesel arguments work: `--pro`, `--piece FILE`, `--backend`, `--model`,
`--resume`, `--private`, and the existing publishing options. Existing account
and sharing requirements still apply. `/perf` uses a permission-restricted Node
helper when the main engine runs on Bun; Node 24 or newer must be installed.

C accepts an opening draft before Bun finishes loading. It buffers the exact
keystrokes and delivers them once the real editor is ready. Login, disclosure,
and genre prompts receive only fresh input, never that saved draft. After ready,
the complete Aesel editor owns input and rendering. This preserves features and
keeps the startup shortcut separate from the full-editor readiness measurement.

```sh
make -C aesel/experiments/c-tui full-test
make -C aesel/experiments/c-tui full-sanitize
python3 aesel/experiments/c-tui/bench-full.py --runs 10 --warmup 2 --out /tmp/native-full.json
python3 aesel/experiments/c-tui/bench-full.py --runs 7 --warmup 1 --settled --installed --out /tmp/native-installed.json
```

The full benchmark uses identical Bun bytecode and the same provider in both
arms. Warmup samples remain in the report. Gates require p95 opening input below
16.7 ms, typing below 2 ms, typing while working below 4 ms, streaming paint below
16.7 ms, and full-editor readiness within 15 ms of direct Bun. `--settled` waits
for the provider before timing Enter and checks reply latency against Bun.
`--installed` includes the shell launcher and its symlink. These are PTY output
timings, not physical pixels or real model speed. `core_ready_ms` starts inside
the C host and ends when the complete editor accepts input; provider readiness
is a later event.

The [October 1 installed-command run](results/native-full-landing.json.gz)
passed seven samples per target, plus one recorded warmup per target, with
unchanged source hashes. Medians:

| PTY measurement | C host + full Bun TUI | Full Bun TUI |
| --- | ---: | ---: |
| Accept opening input | 13.20 ms | 59.32 ms |
| Paint typed key while replying | 0.54 ms | 0.71 ms |
| First reply after Enter, provider ready | 135.65 ms | 143.43 ms |
| Streaming paint | 2.17 ms | 2.35 ms |

Installed opening p95 was 13.95 ms. The complete editor became ready 42.87 ms
after the C host started; the opening prompt is a buffered draft, not the whole
editor. The [direct-host run](results/native-full-verified.json.gz)
measured 5.16 ms opening input, excluding the outer shell launcher. Enter during
startup still waits for the provider: its first-reply medians were 369.75 ms for
the host and 294.28 ms for direct Bun. The host receives Enter earlier in that
test. None of these measurements promise faster model generation or cold-boot
performance.

The feature checks exercise opening edits, consent isolation, explicit approval
answers, provider switching and rollback, Markdown, Settings, Unicode editing,
mouse bytes, resize, bounded activity, recovery, and reify with preserved thread,
settings, draft, queue and preview. They use offline fixtures. Reusing the full
core makes its other features available; this does not mean every external
provider, media service or publishing destination was exercised live.

The optional command can be installed from the repository root:

```sh
mkdir -p "$HOME/.local/bin"
ln -s "$PWD/aesel/experiments/c-tui/full" "$HOME/.local/bin/aesel-native"
```

`results/` contains compressed raw benchmark reports, including the first shell
launcher run whose opening-latency gate failed before unnecessary subprocesses
were removed. Inspect a report with `gzip -dc results/REPORT.json.gz`.

## Reduced frontend comparison

Two pthreads run the C frontend: one reads keys and paints changed terminal rows;
the other starts the provider sidecar and services its pipes. A bounded command
queue and wakeup pipes connect them. Slow provider reads and writes leave the
editor responsive.

The sidecar runs on Bun and reuses Aesel's `AppServer` transport to talk to
`codex app-server`. This is a native terminal prototype, not a full C port.

From the repository root:

```sh
make -C aesel/experiments/c-tui
aesel/experiments/c-tui/run
```

It uses the installed Codex CLI and its normal account configuration. New and
resumed threads use a read-only sandbox. There is no approval UI or Aesel media
tool host. Optional arguments: `--model MODEL`, `--resume THREAD`. The thread ID
appears under the title. Set `AESEL_JS_RUNTIME=node` to use Node for the sidecar.

Enter sends a prompt. You can type the next draft while a reply streams; Enter
during an active turn keeps that draft. Ctrl-C stops the turn, Ctrl-U clears the
draft, and `/quit` exits. After a provider failure, `/retry` reconnects to the same
thread with an explicit continuation instead of blindly replaying the request.
Provider-owned retries get a 90-second deadline; this prototype requires manual
retry after terminal failure. A crashed sidecar leaves the draft visible but
requires reopening the prototype.

The experiment shows the current reply, with end-of-line UTF-8 editing, bracketed
paste, resize handling and a 512 KiB response cap. It does not yet provide Aesel's
artifacts, Markdown, scrollback, approvals, automatic recovery or durable UI
checkpoints. It does not replace the installed `ac` or `aesel` commands.

```sh
make -C aesel/experiments/c-tui test
make -C aesel/experiments/c-tui sanitize
bun test aesel/experiments/c-tui/bridge.test.mjs
python3 aesel/experiments/c-tui/bench.py --runs 7 --out /tmp/c-tui.json
```

The benchmark uses real PTYs, isolated accounts and the same deterministic
loopback response stream for C, the full Bun TUI and Codex. It checks typing
before provider readiness and during streaming. Failures count as failures;
changed source hashes invalidate the run. Results measure process startup and
PTY output, not physical display latency or real model speed. The C frontend
has fewer features, so this comparison does not isolate the effect of language
or threading alone.

The [October 1 local comparison](results/c-tui-verified.json.gz)
passed seven runs per target with unchanged source hashes. Medians:

| PTY measurement | C frontend | Full Bun TUI |
| --- | ---: | ---: |
| Accept first input | 3.55 ms | 59.76 ms |
| Paint typed key | 0.064 ms | 0.340 ms |
| First reply after Enter | 277.27 ms | 294.64 ms |

Startup p95 was 128.25 ms for C and 270.34 ms for Bun; those first-run outliers
are included. Both still used the Bun/Codex provider path. The largest observed
gain is local interaction latency, not model response speed.
