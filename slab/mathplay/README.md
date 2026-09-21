# Math sprint

Thirty random addition, subtraction and multiplication questions. Each has
four shuffled answer buttons; answering advances immediately. The page records
visible question-to-answer times, including the initial wait for the player.

```sh
node slab/mathplay/serve.mjs
# http://127.0.0.1:7783/
node slab/bin/computer-use-smoke.mjs --math
```

The benchmark opens an isolated headless browser and temporary Puppet daemon,
then closes both. The solver reads the rendered accessibility question, computes
the arithmetic locally, and clicks an exact visible answer through HTTP/MCP.
Every click must produce a new correct-round result and exactly one score
increment. It uses no answer key, game-state access, DOM mutation, or Jev call.
This measures the tool loop; it does not include language-model inference.

Benchmark stdout contains a compact JSON summary and `reportPath`. The unique
report file retains every sample, native receipt, and fixture event trace;
`--full-report` also prints it. The historical `*-report.json` file still holds
the latest run. Successful and failed timing samples are summarized separately.

Blueberry, September 21, 2026: two runs scored **60/60**. All clicks verified in
under 100 ms. Run medians were **34.03 / 48.97 ms**, p95 **52.94 / 68.00 ms**,
and maxima **55.56 / 92.10 ms**. The 30-round loops took **1.13 / 1.50 seconds**
after initial observations of **231 / 401 ms**. These are small local-fixture
samples, not a latency guarantee for arbitrary sites. The second run overlapped
native compilation/installation work.

Native comparison:

```sh
node slab/bin/computer-use-smoke.mjs --native-click-bench
```

This opens one temporary visible counter page, alternates five default native
clicks with five `settleMs:0` clicks, checks each counter transition, and closes
the window. Median complete click times were **513.35 → 306.01 ms** (40.4% less).
Both paths keep fresh window guards and returned screenshots/accessibility.
Zero settling is opt-in: the immediate observation is not a promise that the
application has finished. Check the expected result before another action.
That earlier full-frame path missed the 100 ms target.

The resident-input implementation replaced the pre-click screenshot with a native window
guard and posts CoreGraphics events inside resident Swift. On the same counter
fixture, separate five-sample runs measured **276.68 → 159.98 ms median** with
`settleMs:0` (42.2% less). Ranges were 203–333 ms before and 132–191 ms after.
With the default 180 ms settling, medians were **482.98 → 418.50 ms**. This is
a small local comparison, not an arbitrary-app latency guarantee.

The guard alone took 3.73 ms inside Swift / 36.71 ms including transport and
returned no pixels. Live checks rejected stale observations and moved windows
without changing the counter. Each native click still returns a fresh Frame,
and these timings include an independent exact-counter result check. The
existing 40 ms down/up interval is retained, isolating the architecture changes.

Reproduce the previous implementation with
`node slab/bin/computer-use-smoke.mjs --native-click-bench --legacy-input`.
Raw comparison: [resident input](results/blueberry-resident-input-2026-09-21.json).
Both native and matching Frame MCP changes are installed on Blueberry. Older
native versions keep the legacy path until they advertise the new capability.

Frame now walks the captured window's AX subtree, excluding controls in
overlapping background windows. Six live Finder captures passed an explicit
front-visible/back-hidden file check. Warm captures ranged 112–242 ms; the first
capture after installation took 4.1 seconds. This establishes correct scope,
not an isolated latency improvement. The two test windows were closed.

Raw samples: [blueberry-2026-09-21.json](results/blueberry-2026-09-21.json).

Native socket and compact result verification are now implemented:

```sh
node slab/bin/computer-use-smoke.mjs --compact-bench
node slab/bin/computer-use-smoke.mjs --compact-bench --installed-frame
SLAB_FRAME_TRANSPORT=files node slab/bin/computer-use-smoke.mjs --native-click-bench
```

The installed Frame MCP on port 7767 completed 25 compact verified native clicks:
**99.01 ms median, 135.01 ms p95, 170.26 ms maximum; 13/25 under 100 ms**.
An isolated MCP run measured 95.70 ms median and 126.77 ms p95. Each compact
response contained no image and reported the old/new AX counter values;
the installed run's median response was 717 bytes. A separate browser check
confirmed each result. Model inference is excluded.

The native API takes `verify: {x, y, role: "AXStaticText", attribute: "AXValue",
equals: "Count: 3", timeoutMs: 250}` alongside an observed click point,
`observationId`, and `settleMs: 0`. Verification coordinates must come from an
observation of the actual result control. A successful result issues a new
single-use AX observation; it does not claim to have captured fresh pixels.
The schema is available through `frame_click` in the installed MCP.

An already-true condition or wrong role rejects before input. A result timeout
returns a full Frame with `verification.ok:false`, without repeating the click.
Stale observations, consumed tokens, moved windows, and disconnects are tested.
The fixture sizes only its own window after desktop tiling and closes it in
cleanup. Earlier failed startup trials, including one unverified initial click,
are retained in the [raw record](results/blueberry-compact-frame-2026-09-21.json).

Keep full Frame evidence for unfamiliar screens and uncertain results. The
remaining tail includes the 40 ms mouse hold, app/AX response, and host scheduling;
these small samples do not establish latency for Finder or other native apps.


Mouse-hold and drag experiments (September 21 follow-up):

```sh
node slab/bin/computer-use-smoke.mjs --hold-bench
SLAB_HOLD_SWEEP=2.5,1,0.5,0.1,0 node slab/bin/computer-use-smoke.mjs --hold-bench
swiftc slab/test/fixtures/frame-hold.swift -o /tmp/frame-hold-fixture
SLAB_HOLD_SWEEP=2.5,1,0.5,0.1,0 node slab/bin/frame-hold-appkit.mjs /tmp/frame-hold-fixture
SLAB_DRAG_DEFAULTS=1 SLAB_DRAG_SWEEP=32 SLAB_DRAG_REPEATS=100 node slab/bin/computer-use-smoke.mjs --drag-bench --installed-frame
```

These runs take over the pointer in owned disposable windows. Keep the physical
pointer idle during measurement. Native input remains observation/session-bound;
a failed result stops the run without retrying it. The drag page requires both a
trusted HTML5 drop and a trusted drag-end event before its release counter changes.
The AppKit fixture independently records actual NSButton actions on stdout.

Zero-hold clicks passed in both apps: median **18.80 ms AppKit / 42.31 ms Chrome**,
p95 **44.68 / 62.87 ms**, 100 samples per app. The installed service subsequently
passed 25 clicks at **46.87 ms median**. Zero hold is now the updated Frame MCP
click default; `holdMs` remains available for controls that need a delay.

For verified same-window drags, the default is zero pickup hold, 32 ms movement,
and 32 ms destination dwell before release. Two runs passed 200/200 complete
drop/release checks; shorter profiles failed, including retests after reported
pointer interference. The installed complete drag loop measured **146.96 ms
median / 206.67 ms p95**. Unverified and cross-window movement stays at 500 ms;
this experiment does not establish Finder/file-transfer minimum timings.

[Complete gesture evidence](results/blueberry-gesture-timing-2026-09-21.json).

For repeated fresh-page checks, set `SLAB_DRAG_BATCHES`; repeat counts apply to
each batch. The run stops on the first unverified drag, with no replay. Keep the
mouse and keyboard idle; mark interrupted trials as inconclusive.

```sh
SLAB_DRAG_DEFAULTS=1 SLAB_DRAG_SWEEP=32 SLAB_DRAG_REPEATS=20 SLAB_DRAG_BATCHES=5 node slab/bin/computer-use-smoke.mjs --drag-bench --installed-frame
```

The follow-up clean run passed 100/100 drops across five fresh pages at the
existing 32/32 ms timing: 140.20 ms median / 183.91 ms p95. A user reported
manual intervention during surrounding experiments, so those trials do not
justify changing the default. Native click verification remains the largest
measured click stage (30.55 ms median in the preceding 100-click run).
