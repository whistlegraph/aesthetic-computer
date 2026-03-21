# Oven Architecture Report

**Generated:** 2026-02-13
**Server:** oven.aesthetic.computer (137.184.237.166)
**Uptime:** 44 days (OS), ~12 min since last oven restart

---

## 1. Machine Specs

| Resource | Value |
|----------|-------|
| **CPU** | 2 vCPUs (Intel, DO-Regular) |
| **RAM** | 1.97 GB total, ~635 MB used, ~1.3 GB available |
| **Swap** | None configured |
| **Disk** | 58 GB, 6.7 GB used (12%) |
| **OS** | Ubuntu 24.04.3 LTS (kernel 6.8.0-90) |
| **Node** | v20.20.0 |
| **Chrome** | 143.0.7499.40 (headless, Puppeteer-managed) |
| **ffmpeg** | 6.1.1 (system package, WebP + H.264 support) |

### Current Memory Breakdown (at rest with 1 active grab)
- **Node (server.mjs):** ~175 MB (8.6% of RAM)
- **Chrome main process:** ~202 MB (10%)
- **Chrome GPU process:** ~157 MB (7.7%)
- **Chrome network service:** ~125 MB (6.2%)
- **Chrome renderer(s):** ~65-100 MB each (3-5%)
- **Caddy:** ~32 MB
- **Total Chrome footprint:** ~600-700 MB
- **Peak memory observed in logs:** **1.4 GB** (during heavy grab batches)

**Verdict:** With 2 GB total and no swap, the machine is memory-constrained. A single Chrome instance + Node already consumes ~850 MB at rest. During heavy workloads, peak memory hits 1.4 GB, leaving very little headroom. This is the primary bottleneck.

---

## 2. Architecture Overview

```
Internet → Caddy (port 443/80, gzip, TLS) → Express (port 3002) → Puppeteer (Chrome)
                                                                 → ffmpeg (WebP/MP4)
                                                                 → terser (JS minification)
                                                                 → DO Spaces (S3 storage)
                                                                 → MongoDB (metadata)
```

### Process Model
- **Single Node process** (`server.mjs`) — no clustering, no workers
- **Single Chrome browser** — shared instance, reused across all grab/icon/preview requests
- **Serial grab queue** — `grabRunning` boolean, one grab at a time, 100ms delay between jobs
- systemd manages the oven service with `Restart=always`, `RestartSec=10`

### Key Modules
| Module | Purpose | Size |
|--------|---------|------|
| `server.mjs` | Express routes + dashboard HTML | 104 KB |
| `grabber.mjs` | Screenshot/WebP/icon capture via Puppeteer | 127 KB |
| `baker.mjs` | Tape (MP4) baking pipeline | 24 KB |
| `bundler.mjs` | KidLisp/JS piece HTML bundle generation | 44 KB |

---

## 3. API Endpoints (41 routes)

### Core Operations
| Endpoint | Method | Purpose |
|----------|--------|---------|
| `/` | GET | Dashboard (real-time WebSocket updates) |
| `/health` | GET | Health check |
| `/status` | GET | Server status + recent bakes |
| `/grab-status` | GET | Active grabs + queue state |

### Tape Baking (MP4)
| Endpoint | Method | Purpose |
|----------|--------|---------|
| `/bake` | POST | Start tape bake (WebP frames → MP4) |
| `/bake-complete` | POST | Callback when bake finishes |
| `/bake-status` | POST | Check bake progress |

### Screenshots & WebP Captures (Grabber)
| Endpoint | Method | Purpose |
|----------|--------|---------|
| `/grab` | POST | Trigger grab (screenshot/animation) |
| `/grab/:format/:width/:height/:piece` | GET | Direct grab with params |
| `/grab-ipfs` | POST | Grab + IPFS upload |
| `/grab-cleanup` | POST | Clean stale grabs |
| `/grab-clear` | POST | Clear all active grabs |
| `/icon/:size/:piece.png` | GET | Piece icon (cached → DO Spaces) |
| `/icon/:size/:piece.webp` | GET | Piece icon as WebP |
| `/preview/:size/:piece.png` | GET | Piece preview screenshot |

### OG Images
| Endpoint | Method | Purpose |
|----------|--------|---------|
| `/kidlisp-og.png` | GET | KidLisp OG image (for social sharing) |
| `/kidlisp-og` | GET | KidLisp OG page (HTML) |
| `/kidlisp-og/status` | GET | OG cache status |
| `/kidlisp-og/preview` | GET | OG preview page |
| `/notepat-og.png` | GET | Notepat OG image |
| `/kidlisp-backdrop.webp` | GET | KidLisp backdrop animation |
| `/kidlisp-backdrop` | GET | KidLisp backdrop page |

### App Screenshots
| Endpoint | Method | Purpose |
|----------|--------|---------|
| `/app-screenshots` | GET | App screenshot dashboard |
| `/app-screenshots/:preset/:piece.png` | GET | Screenshot by preset |
| `/app-screenshots/download/:piece` | GET | Download all presets as ZIP |
| `/api/app-screenshots/:piece` | GET | JSON metadata for screenshots |

### Bundle (HTML offline bundles)
| Endpoint | Method | Purpose |
|----------|--------|---------|
| `/bundle-html` | GET | Generate HTML bundle (SSE streaming) |
| `/bundle-prewarm` | POST | Prewarm bundle cache |
| `/bundle-status` | GET | Bundle cache status |

### Misc
| Endpoint | Method | Purpose |
|----------|--------|---------|
| `/api/frozen` | GET | List frozen pieces |
| `/api/frozen/:piece` | DELETE | Unfreeze a piece |
| `/keeps/latest` | GET | Latest keep thumbnail |
| `/keeps/latest/:piece` | GET | Latest keep for specific piece |
| `/keeps/all` | GET | All latest IPFS uploads |

---

## 4. Current Issues

### 4.1 Terser Not Found (FIXED in latest deploy)
The error log shows **92 minification failures** with `Cannot find package 'terser'`. This was from a previous deploy where `npm install` wasn't run after `terser` was added to `package.json`. The latest deploy (today) resolved this — bundler is working and prewarm succeeds.

### 4.2 Repeated Service Crashes
The systemd journal shows **25 instances** of `Main process exited, code=exited, status=1/FAILURE`. These are likely from:
- Deploys that didn't run `npm install` before restarting
- OOM situations (no swap, peak memory hit 1.4 GB on a 2 GB machine)
- Chrome connection drops during heavy workloads

### 4.3 Serial Grab Queue (Primary Performance Bottleneck)
The grabber processes **one grab at a time** using a simple boolean lock:
```javascript
let grabRunning = false;  // Only one grab runs at a time
```
Currently there are **19 items in the queue** (1 capturing, 18 queued). Each grab takes roughly 30-40 seconds (load page + wait for ready signal + capture 16 frames + ffmpeg encode + upload to Spaces). That means the current queue will take **~10-13 minutes** to clear.

### 4.4 No Swap Space
With 2 GB RAM and Chrome eating 600-700 MB at rest, there's no safety net. If a grab hits a memory-heavy piece (or multiple Chrome renderer processes spawn), the OOM killer can terminate the process.

### 4.5 Low File Descriptor Limit
`ulimit -n` is 1024 (default). Chrome alone can use hundreds of FDs. Under heavy load this could cause `EMFILE` errors.

### 4.6 Stale PM2 Process
There's a PM2 daemon running (`PM2 v6.0.14`) from before the systemd migration. It's consuming 17 MB of RAM doing nothing.

---

## 5. Recommendations for Faster Parallel WebP Recording

### Priority 1: Upgrade the Droplet (Immediate Impact)

| Current | Recommended | Cost |
|---------|-------------|------|
| 2 vCPU / 2 GB | **4 vCPU / 8 GB** | ~$48/mo (vs ~$18/mo now) |

With 8 GB RAM you can comfortably run **3-4 concurrent Chrome tabs** for parallel captures. 4 vCPUs means ffmpeg encoding can happen in parallel without blocking grabs.

### Priority 2: Add Swap (Quick Win, Free)
```bash
fallocate -l 2G /swapfile
chmod 600 /swapfile
mkswap /swapfile
swapon /swapfile
echo '/swapfile none swap sw 0 0' >> /etc/fstab
```
This prevents OOM kills during peak usage. Even slow swap is better than crashing.

### Priority 3: Parallel Grab Workers (Architecture Change)

Replace the serial `grabRunning` boolean with a **concurrency pool**:

```
Current:  [Queue] → [Single Worker] → [Upload]
                        ↓
Proposed: [Queue] → [Worker 1] → [Upload]
                  → [Worker 2] → [Upload]
                  → [Worker 3] → [Upload]
```

**Implementation approach:**
1. Replace the single shared browser with a **browser page pool** — launch N pages (tabs) in the same Chrome instance
2. Replace `grabRunning` boolean with a semaphore/counter: `let grabsRunning = 0; const MAX_CONCURRENT_GRABS = 3;`
3. Each worker gets its own page from the pool, captures frames, encodes, uploads, then returns the page
4. Chrome tabs share memory more efficiently than separate browser instances (~65 MB per tab vs ~300+ MB per browser)

**Key changes in `grabber.mjs`:**
- `processGrabQueue()` — loop while `grabsRunning < MAX_CONCURRENT_GRABS && queue.length > 0`
- Page pool: pre-create N pages at startup, hand them out via `acquirePage()` / `releasePage()`
- ffmpeg calls already happen in child processes, so they parallelize naturally

**Expected improvement:** With 3 concurrent workers on a 4-CPU/8-GB droplet:
- Current: 19 queued items × ~35s each = **~11 minutes**
- Parallel: 19 items / 3 workers × ~35s = **~3.7 minutes** (3x speedup)

### Priority 4: Optimize Individual Grab Speed

- **Reduce `acPieceReady` timeout** from 30s to 10s — pieces that don't signal ready in 10s probably won't at 30s either
- **Skip Google Analytics** in capture mode — add `?noanalytics=true` param or block GA URLs in Chrome's request interception (eliminates `ERR_ABORTED` noise in logs)
- **Pre-render frame capture** — instead of 16 sequential `page.screenshot()` calls with delays, consider a client-side approach where the piece renders frames to an offscreen canvas and bundles them

### Priority 5: Separate Concerns (Long-term)

The oven server handles too many responsibilities in a single process:
- Screenshot/WebP capture (CPU + memory intensive)
- OG image generation (CPU intensive)
- Bundle HTML generation (CPU intensive during minification)
- Tape baking (CPU intensive)
- Dashboard serving
- Icon/preview caching

Consider splitting into:
1. **API gateway** (Express, lightweight) — routes, dashboard, status
2. **Capture workers** (Chrome + ffmpeg) — the heavy lifting, can be scaled independently
3. **Bundle worker** — terser minification, isolated from capture workload

This could be done with Node worker threads, separate processes, or even separate droplets behind a load balancer.

### Quick Wins (Do Now)

1. **Kill stale PM2:** `pm2 kill` — frees 17 MB
2. **Add swap:** 2 GB swapfile — prevents OOM crashes
3. **Increase file limits:** Add `LimitNOFILE=65536` to oven.service
4. **Clean up logs:** `journalctl --vacuum-time=7d`

---

## 6. Storage & CDN

| Storage | Bucket | Content |
|---------|--------|---------|
| DO Spaces (art) | `art-aesthetic-computer` | Source ZIPs, grab WebPs, icons |
| DO Spaces (blobs) | `at-blobs-aesthetic-computer` | Processed tapes (MP4), thumbnails |
| CDN | `art-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com` | Public CDN for grabs/icons |
| CDN | `at-blobs.aesthetic.computer` | Public CDN for tapes |

- ac-source on oven: **640 files** in `/opt/oven/ac-source/`
- Total oven directory: **168 MB** (including node_modules)

---

## 7. Bundle Cache Status

- **Cache state:** Warm (189 core files minified)
- **Git version:** `64512591a`
- **ac-source synced:** 640 files
- **Post-push hook:** Installed (`.git/hooks/pre-push` → `sync-source.sh`)
- **Prewarm:** Triggered on every `deploy.sh` restart

---

## 8. Summary

The oven is a **capable but resource-constrained** single-process server trying to do everything at once on a 2 vCPU / 2 GB droplet. The serial grab queue is the biggest performance bottleneck — with 18+ items queued, individual WebP recordings wait 10+ minutes.

**Fastest path to improvement:**
1. Add 2 GB swap (5 min, prevents crashes)
2. Upgrade to 4 vCPU / 8 GB ($30/mo more)
3. Implement parallel grab workers (code change in `grabber.mjs`)
4. Expected result: **3-4x faster WebP recording throughput**
