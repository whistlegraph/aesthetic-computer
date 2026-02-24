# Keep Mint Performance Report

**Date:** 2026-02-23
**Trigger:** Observed looping behavior on `$2un` keep, user reports slow end-to-end experience

---

## Bug Fixes Applied (This Session)

Three bugs were fixed in `system/public/aesthetic.computer/disks/keep.mjs`:

### Fix 1 — Trailing buffer missing `return` on error (Critical)
**Line ~1382** — When an SSE `error` event lands in the trailing buffer after stream close,
`handleEvent` was called but execution fell through into auto-retry logic. Now returns immediately.

```diff
- if (eventType) await handleEvent(eventType, eventData);
+ if (eventType) {
+   await handleEvent(eventType, eventData);
+   if (eventType === "error") return;
+ }
```

**Effect before fix:** Every explicit server error triggered 1-2 automatic retries before
finally showing the error — repeating validate → bundle → IPFS → thumbnail for no reason.

### Fix 2 — Auto-retry on explicit server errors (High)
**Line ~1408** — `runProcess` auto-retried when the stream ended without `preparedData`,
even when `lastEventType === "error"` (the server already told us why it failed).

```diff
- if (!mintCancelled) {
+ const hadServerError = lastEventType === "error";
+ if (!mintCancelled && !hadServerError) {
```

**Effect before fix:** Thumbnail failures (the most common failure mode) triggered 2
automatic pipeline restarts totalling ~25-50 extra seconds before the user saw an error.

### Fix 3 — Thumbnail step shown as "done" while still waiting (Visual)
**Line ~1272** — Any `thumbnail` SSE message was immediately marked `done` (✓ green).
While the server is *awaiting* the thumbnail, the step showed ✓ with "Awaiting 256x256@2x WebP..."
— confusingly implying success during the longest wait.

```diff
- setStep("thumbnail", "done", detail);
+ setStep("thumbnail", isWaiting ? "active" : "done", detail);
```

---

## Pipeline Timing Breakdown (from logs, `$2un`)

| Stage        | Duration  | Runs per retry? |
|--------------|-----------|-----------------|
| Connect wallet | ~0.1s   | Yes             |
| Validate     | ~3.4s     | Yes             |
| Details      | <0.1s     | Yes             |
| Analyze      | ~0.4s     | Yes             |
| Bundle       | ~1.4s     | Yes (unless cached) |
| IPFS upload  | ~0.6s     | Yes (unless cached) |
| **Thumbnail await** | **8–45s** | **Yes (unless cached)** |
| Metadata     | —         | Yes             |

**Total happy path:** ~7s for all stages before thumbnail, then 8-45s thumbnail wait.
**Total with thumbnail failure + 2 auto-retries (pre-fix):** 3× the pipeline = 20-150s wasted.

---

## Performance Wins (Opportunities)

### P0 — Thumbnail is the critical path for new mints

The oven `/grab-ipfs` endpoint takes 8-45 seconds. It runs in parallel with the rest of
the pipeline, but the pipeline only takes 7s while the thumbnail takes 8-45s — so the
thumbnail is *always* the bottleneck for fresh mints.

**Options:**
1. **Client-side thumbnail generation** — Capture the current canvas frame in the browser
   and send it as a WebP blob with the keep-mint request. Eliminates oven entirely.
   The piece is already running in the user's browser — no headless rendering needed.
   *Estimated savings: 8-45s.*

2. **Async thumbnail with pre-upload** — Fire off thumbnail generation when the user
   opens keep.mjs (not when they click "Keep"). By the time they read the UI and click
   through, the thumbnail may already be done.

3. **Reduce oven duration** — Currently `duration: 8000` (8s render wait). For KidLisp
   pieces that render instantly, this is pure waste. Add a `quickCapture` mode that
   captures after 1-2s and falls back to 8s only if the first attempt yields a blank frame.

### P1 — Bundle + IPFS re-upload on every retry

When a thumbnail fails, the retry redoes bundle packing (1.4s) and IPFS upload (0.6s) even
though the bundle is identical. With Fix 1+2 in place, auto-retries on explicit errors are
now suppressed — but user-clicked retries (and unexpected disconnects) still redo this work.

**Win:** The `isCachedMediaValid` check already skips bundle+IPFS when regenerate=false
and the source hash matches. But on `regenerate=true` retries this cache is bypassed.
Consider a separate `regenerateThumbnailOnly` flag so we can force a new thumbnail
without re-uploading an identical bundle.

### P2 — `STEP_DELAY = 350ms` adds visible latency

The client delays 350ms between processing each SSE event to give users time to see each
step tick by. With ~8 stage events at 350ms each, that's ~2.8s of artificial delay.

**Win:** Reduce to 150ms. The visual cadence still reads cleanly but saves ~1.6s.
Alternatively, make this configurable: fast when retrying, slower on first attempt.

### P3 — Auth token adds ~0.5s on cold starts

`getTokenSilently()` runs in parallel with the initial fetch setup (good), but on cold
Auth0 sessions this blocks wallet connection. Already optimized — no change needed.

### P4 — MongoDB credential fetch on cold Netlify invocations

`getPinataCredentials()` and `getTezosCredentials()` query MongoDB the first time, then
cache in module-level variables. On cold Netlify function starts these add ~200-400ms.

**Win:** Store Pinata JWT in an environment variable directly (it's static) instead of
looking it up in MongoDB every cold start. Saves one DB round-trip per cold invocation.

### P5 — Netlify function timeout vs thumbnail timeout mismatch

Netlify streaming functions have a 26-second default background timeout. The
`THUMBNAIL_TIMEOUT_MS = 45000` (45s) exceeds this. If the thumbnail takes >26s, Netlify
may kill the function before the error SSE event is flushed — leaving the client with no
error event and triggering the (now-fixed) auto-retry.

**Win:** Lower `THUMBNAIL_TIMEOUT_MS` to 20s to stay safely under Netlify limits, or
verify the streaming timeout is set higher in netlify.toml.

### P6 — Specific thumbnailing failures for animated/scrolling pieces

`$2un` has source `blue, ink rainbow, box, scroll 20`. The `scroll` command causes
continuous animation. The oven captures after 8s of render, but if the piece never
settles on a "good" frame, every capture will succeed (returning a valid WebP) — this
isn't a failure path. If it's failing, it's likely an oven-side rendering error.

**Recommended:** Add oven-side logging that surfaces *why* a piece fails to thumbnail
(crash, blank frame, timeout). Surface that specific reason in the error SSE message
so users know whether to retry or contact support.

---

## Summary Priority Table

| # | Win                              | Effort | Impact   |
|---|----------------------------------|--------|----------|
| 1 | Client-side thumbnail capture    | High   | Very High — eliminates 8-45s wait |
| 2 | Thumbnail-only regeneration flag | Medium | High — saves bundle/IPFS re-upload on retry |
| 3 | Reduce `STEP_DELAY` 350ms→150ms  | Low    | Medium — saves ~1.6s |
| 4 | Pinata JWT in env var            | Low    | Low — saves ~200ms cold start |
| 5 | Lower thumbnail timeout to 20s   | Low    | Medium — prevents Netlify kill-before-error |
| 6 | Oven quick-capture mode          | Medium | High — reduces thumbnail from 8s+ to 1-2s |
| 7 | Pre-warm thumbnail on keep open  | Medium | High — overlaps wait with user reading UI |
