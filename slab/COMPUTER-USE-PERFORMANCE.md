# Computer-use latency

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

With an unlocked local display, add `--native` for five quiet captures per OCR
mode. The report includes native stage timings and excludes failed captures;
it stores no screen text or images.

Focused regression checks:

```sh
node --test --test-concurrency=1 shared/toon.test.mjs slab/test/frame-local.test.mjs slab/test/computer-use-transport.test.mjs slab/test/computer-use-semantic.test.mjs slab/test/computer-use-protocol.test.mjs slab/test/computer-use-isolation.test.mjs captutor/test/frame-client.test.mjs
```
