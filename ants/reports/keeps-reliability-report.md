# Keeps Mint Flow — Reliability Report

**Date:** 2026-03-10
**Problem:** SSE connection drops during "Pin to IPFS" step — "Connection lost — server timed out"

---

## Current Architecture

```
Client (keeps.html)
  │  POST /api/keep-mint
  │  ← SSE stream (text/event-stream)
  │
  ▼
Netlify Streaming Function (keep-mint.mjs)
  │  180s hard ceiling (Netlify Pro)
  │
  ├─► Validate auth, ownership, not-minted
  ├─► START oven /grab-ipfs (async, non-blocking)
  ├─► Analyze source (1s)
  ├─► Fetch /api/bundle-html (5-15s, SWC cold starts)
  ├─► START IPFS bundle upload (async, non-blocking)
  ├─► Poll /grab-status every 2s (heartbeat)
  ├─► AWAIT thumbnail promise (up to 150s budget)
  ├─► AWAIT IPFS bundle upload
  ├─► Build + upload metadata JSON to IPFS
  └─► Send "prepared" SSE event → client signs tx
```

```
Oven Server (oven.aesthetic.computer)
  │  POST /grab-ipfs
  │  90s per-grab timeout
  │
  ├─► Queue grab (dedup by captureKey)
  ├─► Launch Puppeteer, load piece
  ├─► Capture frames (4-5s duration, 8fps)
  ├─► Encode WebP (256x256@2x, q70)
  ├─► Upload to Pinata IPFS (90s timeout)
  └─► Return { ipfsUri, grabDuration }
```

---

## Identified Failure Points

### 1. Oven Grab Timeout Mismatch
- **Oven internal timeout:** 90s (`GRAB_TIMEOUT_MS`)
- **keep-mint budget for thumbnail:** up to 150s
- **But:** The oven itself kills the grab at 90s. The extra 60s of budget on keep-mint's side just waits for a response that will never come. If the oven grab takes >90s, it's already dead but keep-mint doesn't know immediately.

### 2. Piece Loading in Oven is Slow
- The thumbnail step shows "Loading piece... 17s / 30s" — the piece takes 17+ seconds just to load in headless Chrome on the oven server.
- KidLisp pieces need the full AC runtime (boot.mjs → bios.mjs → disk.mjs ~572KB + KidLisp evaluator).
- **The `black` prewarm** was recently added but may not be effective for KidLisp pieces since they have a different evaluation path.

### 3. SSE Stream Drops Before Completion
- Netlify's 180s limit is the hard wall, but **intermediate proxies** (Cloudflare, CDN, browser) may drop idle SSE connections sooner.
- The 2s heartbeat polling to `/grab-status` generates progress events, but if the oven is slow to respond to those polls, the SSE stream can go quiet for several seconds.
- **Browser/proxy SSE timeout:** Some proxies drop connections after 60-120s of perceived inactivity, even if data trickles through.

### 4. No Resume / Idempotency
- If the SSE stream drops at any point, **all work is lost**. The client has no way to reconnect and pick up where it left off.
- Thumbnail may have already been baked and pinned to IPFS, but the client doesn't know.
- IPFS bundle may already be uploaded, but on retry it re-uploads.

### 5. Single-Function Bottleneck
- Everything runs in one Netlify function invocation with one 180s budget.
- Cold start + validate + bundle + oven + 2x IPFS uploads + metadata all compete for the same 180s window.
- Any slow step cascades into budget exhaustion for later steps.

---

## Proposed Architecture: Job-Based Mint Flow

Instead of a single long-lived SSE stream, split into a **job queue** pattern:

### Phase 1: Prepare (fire-and-forget)
```
Client POST /api/keep-prepare
  → Validates auth, creates a mintJobId in MongoDB
  → Kicks off background work (or returns immediately)
  → Returns: { mintJobId }

Background (or same function, short-lived):
  → Analyze source
  → Generate bundle HTML
  → Upload bundle to IPFS
  → Request oven thumbnail (async)
  → Store progress in MongoDB: { mintJobId, stage, artifactUri, thumbnailUri, ... }
```

### Phase 2: Poll (lightweight, resumable)
```
Client GET /api/keep-status?jobId=xxx
  → Returns current job state from MongoDB
  → { stage, progress, artifactUri, thumbnailUri, metadataUri, error }
  → Client polls every 2-3s (or use short SSE connections)
  → **Survives page refresh, network blips, proxy timeouts**
```

### Phase 3: Sign & Confirm (already exists)
```
Client signs tx with Beacon wallet
Client POST /api/keep-confirm
  → Records on-chain mint
```

### Benefits
- **Resumable:** Client can disconnect and reconnect; state lives in DB
- **No 180s ceiling pressure:** Each step runs independently
- **Oven can take as long as it needs:** Thumbnail baking is fully async
- **Retry individual steps:** If IPFS upload fails, retry just that step
- **Works across page refreshes:** User can close tab and come back

---

## Quick Wins (Without Architectural Change)

### A. Keep the SSE stream alive with heartbeat events
Add explicit keepalive pings every 5s during thumbnail wait:
```javascript
// In the heartbeat loop, always send something even if no oven progress
send(sse("progress", { stage: "thumbnail", message: "Baking...", keepalive: true }));
```
This prevents proxy/CDN idle timeouts.

### B. Increase oven grab timeout for keeps
The oven's 90s `GRAB_TIMEOUT_MS` is too tight for KidLisp pieces that take 17s just to load. Bump to 120s for keep grabs:
```javascript
const GRAB_TIMEOUT_MS = source === 'keep' ? 120000 : 90000;
```

### C. Cache the KidLisp runtime in oven
The oven loads the full AC runtime for every grab. Pre-caching the compiled KidLisp evaluator and disk.mjs in the browser context would cut 10-15s off load time.

### D. Return partial results on timeout
If the stream dies, store whatever was completed (bundle URI, thumbnail URI) in MongoDB so retries can skip completed steps:
```javascript
// On retry, check for existing partial results
const existing = await db.collection('keep-jobs').findOne({ piece, wallet });
if (existing?.artifactUri) skip bundle step;
if (existing?.thumbnailUri) skip thumbnail step;
```

### E. Decouple thumbnail from mint flow
Thumbnail is the slowest step. Bake it **before** the user clicks "Keep" — when they first visit the piece preview. Store the result. When mint starts, it's already ready.

---

## Recommendation

**Short term:** Implement quick wins A + D (heartbeat + partial result caching). This directly addresses the "connection lost" symptom.

**Medium term:** Implement quick win E (pre-bake thumbnails on piece view). This eliminates the biggest bottleneck.

**Long term:** Migrate to the job-based architecture. This makes the system fundamentally resilient to connection drops and removes the 180s ceiling entirely.

---

## Key Files

| File | Lines | Role |
|------|-------|------|
| `system/public/kidlisp.com/keeps.html` | ~5200 | Full keeps UI + mint flow client |
| `system/netlify/functions/keep-mint.mjs` | 2029 | SSE streaming mint orchestrator |
| `system/netlify/functions/keep-confirm.mjs` | 262 | Post-signature confirmation |
| `system/public/aesthetic.computer/lib/keeps-stream.mjs` | ~164 | SSE response parser |
| `oven/server.mjs` | — | Oven Express server |
| `oven/grabber.mjs` | 4310 | Screenshot capture + IPFS upload |
