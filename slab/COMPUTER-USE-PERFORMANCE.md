# Computer-use latency

Zero intentional mouse hold is now the default for `frame_click` on updated
native hosts. An interleaved sweep tested 40, 10, 5, 2.5, 1, 0.5, 0.1, and 0 ms
in Chrome and a real AppKit button. All **1,800 single-click timing trials**
passed, including **100 zero-hold clicks per app** and zero-hold double/triple
checks. Zero hold means back-to-back event posting, not instantaneous delivery.

| Zero-hold verified loop | Samples | Median | p95 | Within 41.7 ms |
| --- | ---: | ---: | ---: | ---: |
| AppKit button | 100 | 18.80 ms | 44.68 ms | 92 |
| Chrome counter | 100 | 42.31 ms | 62.87 ms | 49 |
| Installed Frame MCP, Chrome | 25 | 46.87 ms | 72.29 ms | 7 |

24 fps requires 41.67 ms for the entire cycle. These tool-loop measurements
exclude model inference and startup. All clicks passed, but latency tails still
miss that budget. Explicit multi-click spacing remains 80 ms.

`frame_drag` now runs in resident Swift. It checks the source observation,
preallocates the path before mouse-down, uses absolute movement deadlines to
avoid accumulating scheduling overruns, and posts mouse-up on every dispatch
path. `holdMs`, `durationMs`, and `releaseMs` independently control pickup,
movement, and destination dwell. Timings and release posting appear in receipts.

The fast default with same-window compact verification is **0 ms pickup,
32 ms movement, 32 ms destination dwell**. Without that verification, or across
windows, movement remains 500 ms. Full-frame observation still defaults to
180 ms settling; benchmarks explicitly use `settleMs:0`. Older native helpers
retain their existing input path.

Two runs of the fast drag profile verified **200/200 drops and releases**. The
installed run measured **146.96 ms median / 206.67 ms p95** for the complete
verified operation. Destination acknowledgement is substantial; a 64 ms gesture
is not a 64 ms completed tool call. A zero-duration jump, 16/32 ms path/dwell,
and 32/16 ms path/dwell failed. One earlier trial may have been affected by a
human moving the pointer; both shorter profiles also failed in subsequent
retests. No failed gesture was replayed. Finder/cross-app minimum timings have
not been established by this fixture.

Validation: 25 focused JavaScript tests, Swift policy tests, native build/install,
installed-service click/drag checks, and release/count verification. Test apps
and browser windows were closed. [Raw timing and failure records](mathplay/results/blueberry-gesture-timing-2026-09-21.json).

## Earlier socket and compact-verification comparison

Frame's native socket and opt-in compact AX verification are installed in the
Blueberry Frame MCP and CLI. The installed-service counter run measured
**99.01 ms median**, **135.01 ms p95**, with **13/25 below 100 ms**. Every timed
click changed the counter exactly once. Median MCP response size was 717 bytes,
with no image. This measures the tool loop plus an independent rendered-counter
check, excluding model inference and startup; it is not consistently sub-100 ms.

| Warm native path, zero fixed settling | Samples | Median |
| --- | ---: | ---: |
| File transport + full returned Frame | 5 | 205.88 ms |
| Socket transport + full returned Frame | 5 | 116.57 ms |
| Socket + compact AX check, isolated MCP | 25 | 95.70 ms |
| Socket + compact AX check, installed MCP | 25 | 99.01 ms |

These sequential trials share the counter fixture but have different host load
and window layout. They establish working paths, not a precise attribution of
latency to each change. [Raw results and failed setup trials](mathplay/results/blueberry-compact-frame-2026-09-21.json)
include the earlier unverified initial click, which stopped without retry.

Local requests now use a same-user Unix socket, bounded framing, and correlated
request IDs. JPEG bytes remain binary and responses stay in memory. A missing
or refused socket before connection permits the legacy file path; a timeout,
disconnect, or mismatched response never replays a request. The legacy native
file watcher remains for older and remote callers.

`frame_click.verify` identifies an observed global point, AX role, attribute,
and exact expected string. Frame checks the role/window and requires a readable
value different from the expectation before sending input. It reads only that
point until the value matches or the deadline expires, checking foreground
identity and geometry. Success returns an explicitly labeled AX observation
with a new single-use token; failure returns a full Frame without another click.
The result wait uses bounded targeted polling, not AX notifications. Full Frame
remains the default; use `settleMs:0` explicitly for responsive controls.

That comparison retained machine leases, session binding, locked-desktop checks,
and the then-current 40 ms button hold. The installed guard probe took 1.55 ms total / 0.63 ms inside Swift.
Validation: 23 focused JS tests, Swift binding/verification policy checks,
native build/install, and live stale/moved-window, wrong-role, already-true,
consumed-token, and timeout-recovery checks. The fixture closes its own browser.

Earlier resident input work removed the pre-click screenshot and per-click
`osascript`, measuring 277 → 160 ms median in its own zero-settling comparison.
[Those samples](mathplay/results/blueberry-resident-input-2026-09-21.json) remain
separate from the current trials.

The [math sprint and native click results](mathplay/README.md) include
verified dynamic transitions, p95 latency, and the remaining native bottlenecks.
Puppet's full local HTTP/MCP counter test measured 33.13 ms median / 34.61 ms
p95 over 25 warm clicks. Two math sprints scored 60/60 with every click under
100 ms. The optional native `frame_click` setting `settleMs:0` reduced a small
alternating sample's median from 513 to 306 ms, with independent result checks.
It retains the default 180 ms settling for existing callers. Native window AX
scoping and this Frame option are installed on Blueberry; Neo is unchanged.

The benchmark now requires a new counter value after every click, rather than
accepting a success label left over from the first click. It reports 25 warm
samples, p95, maximum, and the count under 100 ms. The older measurements below
retain their original sample sizes and limitations.

Measured on Blueberry, September 21, 2026. The browser fixture is an isolated
headless Chrome page with one input and one button. These are local tool
execution times; they exclude model inference, remote network latency, and
application work. Seven warm samples per browser operation, after one first call.

| Operation | Before median | After median |
| --- | ---: | ---: |
| Puppet semantic snapshot | 2.22 ms | 1.67 ms |
| Puppet fill | 3.49 ms | 2.86 ms |
| Puppet click with verification | 32.02 ms | 33.74 ms |
| Frame transport through CLI / in process (mock capture) | 35.33 ms | 0.53 ms |

Click timing remains around 33 ms: Playwright still waits for actionability and
checks the postcondition. The inspector connection is reused, but an exact
target lookup still runs on every operation. A cached page's `isClosed()` alone
is insufficient: another client's close event can arrive late.

Captutor now imports Frame once, awaits capture without blocking its event
loop, and obtains post-action DOM evidence and the passive audit concurrently.
Each consequential action still waits for its audit. Custom `CAPTUTOR_FRAME`
CLI overrides remain supported through an asynchronous child process.

Local Frame requests use the native request/done files directly under the same
machine lease. Requests are published atomically; timeouts fail without reading
old pixels or retrying input. Remote SSH transport is unchanged. The transport
comparison above uses a mock capture and measures process overhead, not native
screenshot latency.

Five alternating pairs of successful local captures without OCR, targeting the
same window, measured the previous Bash path against the new path:

| Native Frame path | Total median | Transport overhead median |
| --- | ---: | ---: |
| Previous Bash path (`direct:true`) | 713 ms | 80 ms |
| In-process path | 624 ms | 26 ms |

Native processing varied substantially (total samples ranged from 366 to 852 ms),
so the 13% total reduction is a small local sample, not a universal speedup.
Transport overhead is elapsed time minus the native `timings_ms.wall` field.
One subsequent fast-OCR capture took 465 ms: contour detection used 324.7 ms,
OCR 98.4 ms, and capture 18.2 ms. Contour detection is the largest remaining cost
in that sample; it is already suppressed during Captutor recordings.

Other attempts encountered the login screen and `permission_needed`; they are
excluded. Neo had three successful pre-update captures (340–398 ms), but its
display became unavailable before the after measurement. Accurate-OCR medians
and a complete Captutor recording remain unmeasured in this session.

Puppet's sample page list fell from 357 to 230 bytes (36%). Exact page IDs and
URLs remain intact; `full:true` returns the original JSON state. These are byte
counts, not tokenizer measurements. Earlier list compaction also covers fleet,
prox, papers, calendar, and memory. All-day event end dates and memory ordering
across years are preserved.

The first JavaScript runtime (`e678ebb76`) was installed on Blueberry and Neo,
with prior runtimes and launch-agent backups retained. That round passed 32
focused local checks and nine semantic-browser checks against Neo's runtime.

The second round adds `visual:false` to Frame capture, focus, reframe, and click
tools (`--no-visual` in the CLI). This skips supplemental contour detection while
preserving fresh pixels, accessibility data, and requested OCR. Internal target
guards, change probes, and Captutor's passive audits skip contours automatically.
Ordinary Frame captures retain contour detection by default.

On Blueberry, six captures with pixels + AX and no contours took 58–218 ms.
Six with fast OCR and no contours took 61–132 ms. These were sequential mode
runs with substantial warm-up effects, not paired evidence of a fixed speedup.
Native action time additionally includes a fresh target guard, input, settling,
and the returned frame. The native app was rebuilt with the guarded installer
and installed on Blueberry and Neo; previous app bundles were retained.

Contour results now reuse one exactly matching bounded analysis buffer. Only
normalized boxes are cached: global coordinates, scale, and pointer-distance
ranking are recalculated. Changed pixels or dimensions invalidate it. A static
light/dark fixture measured approximately 3 ms uncached versus 0.1 ms cached;
the animated Terminal produced **no cache hits**, so it showed no cache benefit.
The focused JavaScript checks now pass 33 tests. Standalone Swift fixtures cover
shape detection, invalidation, origin, scale, and focus ranking.

An additional disposable website exercises the real Puppet HTTP/MCP path,
including verified clicks, typing, menu selection, a delayed update, and page
navigation. One complete run measured 16 ms for filling, 32–41 ms for ordinary
verified clicks, 67 ms for navigation, and 842 ms for the delayed update
(including its 300 ms application timer). Its first snapshot took 164 ms.
The native fixture click remains unverified: Blueberry's lock screen caused
Frame to return `permission_needed`, and the runner refused input.

Optional Jev trials send only generated fixture button labels to the existing
OpenRouter decision adapter. The first three decisions selected the correct
button in 261–486 ms; a second set took 172–355 ms, also correct. This is six
simple selections, not an accuracy study. Jev adds decision latency to a known
locator; it can help only when replacing a slower reasoning step. No comparison
against a general-purpose model's end-to-end task time was performed.

Reproduce browser timings (no user tabs or native input):

```sh
node slab/bin/computer-use-bench.mjs
```

Reproduce the disposable website test (isolated browser profile and daemons):

```sh
node slab/bin/computer-use-smoke.mjs
```

Add `--native` to open the fixture visibly and perform one guarded macOS click
on its observed button. An unlocked display is required. Add `--jev` to measure
three decisions using `OPENROUTER_API_KEY`; only generated fixture labels leave
the machine. The runner writes its JSON report and synthetic-page screenshot
to the system temporary directory, then removes its browser profile.

Blueberry's native app and JavaScript services are now running the second round
(`010366763f`); the live Frame schemas expose all four `visual` options and local
Puppet is connected. A launchd restart race was recovered before health checks.
Neo's native app installation succeeded, but it became unreachable over SSH
before its JavaScript refresh. Its JavaScript runtime remains `e678ebb76`.

With an unlocked local display, add `--native` for five quiet captures per OCR
mode. The report includes native stage timings and excludes failed captures;
it stores no screen text or images.

Focused regression checks:

```sh
node --test --test-concurrency=1 shared/toon.test.mjs slab/test/frame-local.test.mjs slab/test/computer-use-transport.test.mjs slab/test/computer-use-semantic.test.mjs slab/test/computer-use-protocol.test.mjs slab/test/computer-use-isolation.test.mjs captutor/test/frame-client.test.mjs
```
