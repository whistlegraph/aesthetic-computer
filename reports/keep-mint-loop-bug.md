# Keep Mint Infinite Loop Bug Report

**Date:** 2026-02-15
**Reproduction:** Keeping `$kl1` — progresses through IPFS then loops back to "awaiting thumbnail"

---

## Full Keep/Mint Work Sequence

### Client: `system/public/aesthetic.computer/disks/keep.mjs`

**Timeline steps** (lines 361-373):
```
wallet → validate → analyze → thumbnail → bundle → ipfs → metadata → review → sign → complete
```

**Entry points** — `runProcess(forceRegenerate = false)` called from:
- Initial keep button press (line 3117)
- Error retry button (line 3323)
- Enter key on error (line 3693)
- **Automatic retry** (line 1409) — when stream ends without `preparedData`

### Server: `system/netlify/functions/keep-mint.mjs`

**Stages executed in order** (lines 1-14 comment):
1. Validate auth/ownership/not-minted
2. **START thumbnail in parallel** (fires off to oven, does NOT await yet)
3. Analyze KidLisp source
4. Generate HTML bundle (fetch from `/api/bundle-html`)
5. Upload bundle to IPFS (Pinata)
6. **AWAIT thumbnail** (now waits for the promise from step 2)
7. Upload metadata JSON to IPFS
8. Send `"prepared"` event with mint parameters

### SSE Communication

Server streams events via SSE. Client reads with `response.body.getReader()`.

| SSE Event    | Purpose                                    |
|--------------|--------------------------------------------|
| `progress`   | Stage updates `{ stage, message }`         |
| `prepared`   | All data ready, sends mint parameters      |
| `error`      | Fatal failure `{ error: "..." }`           |
| `complete`   | Minting transaction confirmed              |

---

## API Calls During Keep

| # | Caller        | Endpoint                       | Purpose                   |
|---|---------------|--------------------------------|---------------------------|
| 1 | Client        | `POST /api/keep-mint`          | Start mint preparation    |
| 2 | keep-mint.mjs | `POST {oven}/grab-ipfs`        | Generate thumbnail + pin  |
| 3 | keep-mint.mjs | `GET /api/bundle-html?code=X`  | Generate HTML bundle      |
| 4 | keep-mint.mjs | Pinata API                     | Pin bundle to IPFS        |
| 5 | keep-mint.mjs | Pinata API                     | Pin metadata JSON to IPFS |
| 6 | Client        | `POST /api/keep-confirm`       | Confirm after wallet sign |

---

## The Bug: Root Cause Analysis

### PRIMARY ISSUE: Missing `return` on error in trailing buffer handler

**File:** `keep.mjs`, lines 1370-1381

The SSE stream reader has two processing paths:

**Path A — Main read loop (lines 1353-1367):**
```javascript
if (eventType) {
    await handleEvent(eventType, eventData);
    if (eventType === "error") return;     // <-- EXITS runProcess
}
```

**Path B — Trailing buffer after stream ends (lines 1370-1381):**
```javascript
if (buffer.trim()) {
    // ... parse event ...
    if (eventType) await handleEvent(eventType, eventData);
    // !! NO "return" on error !! <-- BUG
}
```

If the server's error SSE event lands in the trailing buffer (the last incomplete chunk
after the stream closes), `handleEvent` marks the step as error in the UI, **but execution
falls through** to the retry logic at line 1403.

### SECONDARY ISSUE: Auto-retry without user consent

**File:** `keep.mjs`, lines 1403-1409

```javascript
if (!preparedData) {
    const lastStage = lastEventData?.stage || lastEventType || "unknown";
    const tail = lastEventData?.message ? ` (last: ${lastEventData.message})` : "";
    if (!forceRegenerate && !mintCancelled) {
        setStep("validate", "active", `Retrying with regenerate... (${lastStage})`);
        await delay(350);
        return await runProcess(true);   // <-- AUTOMATIC RETRY
    }
    setStep("validate", "error", `Server did not return prepared data (${lastStage})${tail}`);
}
```

When the stream ends without a `"prepared"` event (because thumbnail failed and error
was swallowed by trailing buffer path), this code automatically retries with
`forceRegenerate = true`. This means:

- `regenerate: true` is sent to the server (line 1202)
- Server sets `useCachedMedia = false` (line 549: `!regenerate && isCachedMediaValid(piece)`)
- **Entire pipeline restarts from scratch** — new thumbnail, new bundle, new IPFS uploads

### TERTIARY ISSUE: Netlify function timeout can prevent error delivery

If the `thumbnailPromise` takes close to 45 seconds AND the Netlify function is
approaching its execution limit, the function may be killed before `send("error", ...)`
executes. In this case:

1. Stream closes with NO error event at all (not in main loop or trailing buffer)
2. `preparedData` is null
3. Auto-retry triggers (line 1409)
4. Second attempt: same timeout → BUT `forceRegenerate` is true, so retry is blocked
5. Shows error at line 1411

This path would only loop **once**, not infinitely.

---

## The Infinite Loop Sequence (for `$kl1`)

Here's the exact sequence that produces the infinite-appearing loop:

```
ATTEMPT 1 (forceRegenerate = false):
  1. Client calls POST /api/keep-mint { regenerate: false }
  2. Server: validate ✓ → start thumbnail (parallel)
  3. Server: analyze ✓ → bundle ✓ → IPFS upload ✓
  4. Server: await thumbnail... FAILS (oven can't render $kl1, timeout, or grab error)
  5. Server: throws "Thumbnail generation failed" → sends SSE error → closes stream
  6. Client: error event lands in trailing buffer (line 1380)
  7. Client: handleEvent marks step as error (UI flashes error briefly)
  8. Client: falls through to line 1403 — !preparedData is true
  9. Client: !forceRegenerate is true → retries with runProcess(true)
  10. Timeline partially resets, UI shows "Retrying with regenerate..."

ATTEMPT 2 (forceRegenerate = true):
  1. Client calls POST /api/keep-mint { regenerate: true }
  2. Server: useCachedMedia = false (because regenerate = true)
  3. Server: validate ✓ → start thumbnail (parallel) → SAME FAILURE
  4. Server: analyze ✓ → bundle ✓ → IPFS upload ✓ (re-does everything!)
  5. Server: await thumbnail... FAILS again
  6. Server: sends SSE error → closes stream
  7. Client: error lands in trailing buffer again
  8. Client: falls through to line 1403 — !preparedData is true
  9. Client: forceRegenerate IS true → goes to line 1411
  10. Shows error: "Server did not return prepared data (thumbnail)"
  11. UI enters error state

  → USER SEES: error state with retry button
  → USER CLICKS RETRY (line 3323): calls runProcess() with forceRegenerate=false
  → GOES BACK TO ATTEMPT 1
```

**The "infinite loop" is the combination of:**
1. One automatic retry (code-triggered, lines 1406-1409)
2. User clicking retry because the error message is vague and non-actionable
3. Each cycle wastes ~30-60 seconds going through validate → analyze → bundle → IPFS before hitting the thumbnail wall again

The user sees: **IPFS done → awaiting thumbnail → brief error flash → back to validating → IPFS done → awaiting thumbnail → error shown → clicks retry → repeat.**

---

## Specific Fixes Needed

### Fix 1: Add error return to trailing buffer handler (Critical)
**File:** `keep.mjs`, line 1380

```diff
- if (eventType) await handleEvent(eventType, eventData);
+ if (eventType) {
+     await handleEvent(eventType, eventData);
+     if (eventType === "error") return;
+ }
```

This ensures error events in the trailing buffer properly exit `runProcess` instead of
falling through to the auto-retry logic.

### Fix 2: Don't auto-retry on explicit server errors (High)
**File:** `keep.mjs`, lines 1403-1412

The auto-retry should only trigger when the stream disconnected unexpectedly (no error
event received), not when the server explicitly sent an error:

```diff
  if (!preparedData) {
      const lastStage = lastEventData?.stage || lastEventType || "unknown";
      const tail = lastEventData?.message ? ` (last: ${lastEventData.message})` : "";
-     if (!forceRegenerate && !mintCancelled) {
+     const hadServerError = lastEventType === "error";
+     if (!forceRegenerate && !mintCancelled && !hadServerError) {
          setStep("validate", "active", `Retrying with regenerate... (${lastStage})`);
          await delay(350);
          return await runProcess(true);
      }
      setStep("validate", "error", `Server did not return prepared data (${lastStage})${tail}`);
  }
```

### Fix 3: Show actionable error message for thumbnail failures (Medium)
**File:** `keep.mjs`, error handler at line 1333-1338

Currently shows generic error. Should indicate that the piece couldn't render for
thumbnailing, so the user knows it's a piece-specific issue, not a transient network error
that "just needs another try."

---

## Why `$kl1` Specifically?

The thumbnail is generated by the oven's `/grab-ipfs` endpoint which:
1. Opens the piece in a headless browser
2. Waits for it to render
3. Captures a screenshot
4. Converts to WebP
5. Uploads to Pinata

If `$kl1` has rendering issues (requires user interaction, takes too long to load, uses
APIs not available in headless mode, or crashes), the oven will fail every time — making
the retry logic futile and the loop predictable.

The 45-second `THUMBNAIL_TIMEOUT_MS` (keep-mint.mjs line 572) combined with the oven's
8-second `duration` parameter (line 594) means the system waits a long time before
discovering the failure.
