# Keep Modal Consolidation Report

**Date:** 2026-03-10
**File:** `system/public/kidlisp.com/keeps.html`

---

## Current State: Three Separate Modal Systems

The keeps.html page currently has **two distinct modal overlays** plus an artifact sub-panel, creating a fragmented, confusing UX:

### 1. Preview Modal (`.keeps-modal-overlay`, lines 2866–2883)
- **Purpose:** View a piece's source code + live iframe preview when clicking a card
- **Layout:** Split — left iframe preview + right sidebar (source, actions, info)
- **CSS:** lines 1675–1887
- **JS:** `openModal(entry)` at line 3546
- **Actions shown:** Edit, Download HTML, Keep on Tezos / Rekeep Media / Upgrade Legacy
- **Info shown:** Author, views, created date, contract address, network, legacy runtime note
- **Problem:** Clicking "Keep on Tezos" or "Rekeep Media" *closes this modal and opens the Mint Modal* — jarring two-step flow

### 2. Mint Modal (`.keeps-mint-overlay`, lines 2885–2908)
- **Purpose:** The actual keep/regenerate/upgrade flow
- **Layout:** Single column — piece info → live preview (4:3) → timeline → artifacts → action buttons
- **CSS:** lines 2093–2509
- **JS:** `openMintModal(entry, options)` at line 5025
- **Sub-components inside:**
  - `mint-piece-info` (line 2893): Shows `$code`, author, "Edit ↗", "HTML ↗", on-chain link group, latest prepared link group, legacy warnings — rendered by `renderMintPieceInfo()` at line 4472
  - `mint-preview-frame` (line 2894): Black box, 4:3 aspect ratio, live iframe
  - `mint-result-area` (line 2896): Where `renderMintArtifactPanel()` (line 4522) appends artifact boxes with thumbnail (100x100) + HTML iframe + HTML/THUMB/META/OBJKT link buttons
  - `mint-track-log` (line 2897): Timestamped activity log (max 120px)
  - Two action bars: start actions (line 2898) and confirm actions (line 2902)

### 3. Artifact Panel (inside Mint Modal, rendered dynamically)
- **JS:** `renderMintArtifactPanel()` at line 4522
- Shows: thumbnail (100×100), HTML iframe preview, HTML/THUMB/META/OBJKT links, Rekeep button
- Can be appended multiple times (on-chain vs latest prepared)
- **Problem:** These are extra preview boxes *below* the main preview, showing nearly the same thing in smaller boxes

### Market/Buy Tab (lines 4032–4109)
- Cards link directly to objkt.com — no modal at all
- No preview, no regenerate, no owner info
- `renderMarket()` at line 4032

### Kept Tab (lines 3340–3381)
- Row items click → `openMintModal(entry)` at line 3379
- **Problem:** Opens the full *mint* modal even just to view a kept piece — wrong affordance

---

## What's Wrong

1. **Two modals for one thing.** Preview modal shows the piece, then you close it and open the mint modal to act on it. Should be one modal with contextual actions.

2. **Three preview boxes in the mint flow.** The mint modal has:
   - The live preview iframe (4:3, line 2894)
   - The artifact thumbnail (100×100, line 4558)
   - The artifact HTML iframe (line 4561)

   All three show variations of the same piece. Overwhelming.

3. **Info overload in `renderMintPieceInfo`.** Shows on-chain URI group + latest prepared URI group + legacy warnings + author + edit/HTML links. Each URI group has up to 3 sub-links (HTML ↗, THUMB ↗, META ↗). That's up to 8+ tiny links in a dense info block.

4. **Kept tab opens the wrong modal.** Clicking a kept piece opens the mint/regenerate modal, but most users just want to *view* it — see the piece, see who owns it, maybe jump to objkt.

5. **Buy tab has no modal at all.** External redirect to objkt only. No preview, no context.

6. **No unified preview component.** Each context (preview, mint, kept, buy) builds its own ad-hoc layout.

---

## Proposed Consolidation: One Modal, One Preview Box

### Design: Single `KeepModal` with a fixed-aspect-ratio black preview box

```
┌─────────────────────────────────────┐
│ $code                          [×]  │
├─────────────────────────────────────┤
│                                     │
│   ┌───────────────────────────┐     │
│   │                           │     │
│   │     Black Preview Box     │     │
│   │      (4:3 aspect)         │     │
│   │                           │     │
│   │   [progress overlay if    │     │
│   │    baking/minting]        │     │
│   │                           │     │
│   └───────────────────────────┘     │
│                                     │
│   by @handle              Edit ↗    │
│                                     │
│   ┌─────────────────────────────┐   │
│   │ [Context-specific actions]  │   │
│   └─────────────────────────────┘   │
│                                     │
│   ┌─────────────────────────────┐   │
│   │ [Activity log if active]    │   │
│   └─────────────────────────────┘   │
│                                     │
│   [Cancel]            [Primary]     │
└─────────────────────────────────────┘
```

### Preview Box States

The single black preview box transitions between states:

| State | What's in the box |
|-------|------------------|
| **Live** | iframe running the piece from AC |
| **Baking** | Thumbnail/progress overlay on top of live piece |
| **Artifact** | Shows the baked HTML bundle iframe (replacing live) |
| **Static** | Thumbnail image only (for items you don't own) |

### Context-Specific Action Sections

One modal, but actions change based on context:

#### A. "My Unkept Piece" (from Unkept tab)
- **Preview:** Live iframe
- **Actions:**
  - `Keep on Tezos ꜩ` (primary, green)
  - `Edit in KidLisp.com` (purple)
  - `Download HTML` (secondary)

#### B. "My Kept Piece" (from Kept tab, or Unkept if already kept)
- **Preview:** Live iframe
- **Actions:**
  - `Regenerate Media` (primary, orange) — if owner/artist
  - `Upgrade Legacy Bundle` (shown only if legacy runtime detected)
  - `View on objkt ↗` (if token exists)
  - `Edit in KidLisp.com` (purple)
- **Info:** Token ID, contract, owner address

#### C. "Someone Else's Kept Piece" (from Kept tab, not owner)
- **Preview:** Live iframe or static thumbnail
- **Actions:**
  - `View on objkt ↗` (primary)
  - `View Owner ↗` (links to tzkt/objkt profile)
  - `Edit in KidLisp.com` (fork/view source)
- **Info:** Owner address, token ID, kept date

#### D. "Buy/Market Listing" (from Buy tab)
- **Preview:** Static thumbnail or live iframe
- **Actions:**
  - `Buy on objkt ↗` (primary, gold/amber) — or `View on objkt ↗` if sold
  - `View Seller ↗`
  - Price display: `X.XX ꜩ`
- **Info:** Listing status, seller, price

### Consolidation: What Gets Removed

1. **Kill the Preview Modal** (`.keeps-modal-overlay`, lines 2866–2883, CSS 1675–1887, JS `openModal()` at 3546–3648). Merge its sidebar content (source, actions, info) into the unified modal.

2. **Kill separate artifact preview boxes** inside mint modal (`.mint-artifact-preview` 100×100 thumbnail + `.mint-artifact-iframe`). Instead, after baking completes, swap the *main* preview box content from live iframe → baked artifact iframe. The thumbnail is already visible during the bake progress overlay.

3. **Kill `renderMintPieceInfo()` URI link dump** (lines 4472–4520). Replace with a simple info line: `$code by @handle` + a collapsible "Details" section for on-chain/IPFS links (only shown on demand).

4. **Kill `buildMintHeaderLinkGroup()`** (lines 4447–4469). The on-chain / latest-prepared dual-group is the main source of confusion. Move HTML/THUMB/META links to a simple row of small buttons below the preview box, shown only after artifacts exist.

### Implementation Plan

#### Phase 1: Unify the HTML Structure
- **Remove** `.keeps-modal-overlay` + `.keeps-modal` (lines 2866–2883)
- **Rename** `.keeps-mint-overlay` → `.keep-modal-overlay` (lines 2885–2908)
- **Restructure** mint modal body to: preview box → info bar → actions → log → footer buttons
- **Files:** `keeps.html` lines 2866–2908

#### Phase 2: Unify the CSS
- **Remove** `.keeps-modal-*` styles (lines 1675–1887)
- **Simplify** `.mint-artifact-*` styles — keep only `.mint-artifact-links` for the button row
- **Remove** `.mint-artifact-preview` (100×100 box) and `.mint-artifact-iframe` (inline iframe) — lines 2394–2425
- **Keep** `.mint-preview-frame` as the single preview (already 4:3 aspect, black bg) — lines 2172–2186

#### Phase 3: Unify the JavaScript
- **Replace** `openModal(entry)` (line 3546) with a unified `openKeepModal(entry, context)` where context = `'view' | 'keep' | 'regenerate' | 'buy'`
- **Merge** `openMintModal()` (line 5025) into the unified function
- **Refactor** `renderMintArtifactPanel()` (line 4522) to update the *main* preview box instead of appending sub-panels
- **Update** card click handlers:
  - Unkept cards (line 3438): `openKeepModal(entry, 'keep')`
  - Kept rows (line 3379): `openKeepModal(entry, 'view')`
  - Market cards: `openKeepModal(entry, 'buy')` instead of external link
- **Simplify** `renderMintPieceInfo()` (line 4472) to a one-line info bar

#### Phase 4: Market Tab Integration
- **Change** market cards from `<a href="objkt">` (line 4102) to clickable cards that open the unified modal in "buy" context
- Add live preview or thumbnail in the modal
- Keep "Buy on objkt ↗" as the primary action button

---

## Key Line References

| Component | Lines | Action |
|-----------|-------|--------|
| Preview modal HTML | 2866–2883 | Remove |
| Mint modal HTML | 2885–2908 | Keep & rename |
| Preview modal CSS | 1675–1887 | Remove |
| Mint modal CSS | 2093–2509 | Simplify |
| Artifact sub-panel CSS | 2363–2457 | Simplify (remove media boxes) |
| `openModal()` | 3546–3648 | Remove, merge into unified |
| `openMintModal()` | 5025–5107 | Refactor as unified entry point |
| `renderMintPieceInfo()` | 4472–4520 | Simplify to info bar |
| `renderMintArtifactPanel()` | 4522–4581 | Refactor to swap main preview |
| `buildMintHeaderLinkGroup()` | 4447–4469 | Remove or make collapsible |
| Kept row click handler | 3379 | Change to `openKeepModal(entry, 'view')` |
| Card click handler | 3438 | Change to `openKeepModal(entry, 'keep')` |
| Market card render | 4102–4109 | Change from `<a>` to modal opener |

---

## Summary

**Before:** 2 modal overlays + dynamic artifact sub-panels = 3+ preview boxes, 8+ URI links, confusing state transitions, no modal for buy items, wrong modal for kept items.

**After:** 1 modal, 1 black preview box (4:3), context-aware actions, clean info bar, activity log only when active. Same modal opens from all tabs with appropriate affordances.

---

## Part 2: Backend & Oven Dependency Map

### End-to-End Pipeline Diagram

```
┌──────────────┐     ┌──────────────────┐     ┌─────────────────┐     ┌──────────┐
│   Browser    │────▶│  Netlify Funcs    │────▶│  Oven Service   │────▶│  Pinata  │
│  (keeps.html)│◀────│  (system/)        │◀────│  (oven/)        │◀────│  (IPFS)  │
└──────┬───────┘     └────────┬─────────┘     └────────┬────────┘     └──────────┘
       │                      │                        │
       │  Beacon SDK          │  MongoDB               │  Puppeteer
       ▼                      ▼                        │  Sharp
┌──────────────┐     ┌──────────────────┐              │  ffmpeg
│ Tezos Wallet │     │    MongoDB       │              ▼
│ (Temple etc) │     │ (kidlisp, jobs,  │     ┌─────────────────┐
└──────────────┘     │  users, secrets) │     │  DO Spaces CDN  │
                     └────────┬─────────┘     │  (art.aesthetic  │
                              │               │   .computer)     │
                     ┌────────▼─────────┐     └─────────────────┘
                     │    TzKT API      │
                     │  (indexer)       │
                     └──────────────────┘
```

---

### Netlify Functions — Full Endpoint Map

#### 1. `POST /api/keep-prepare` — Job Creation
**File:** `system/netlify/functions/keep-prepare.mjs` (221 lines)

**What it does:**
1. Validates piece exists in `kidlisp` collection
2. Checks mint status via TzKT: `GET /v1/tokens?contract=...&metadata.symbol=...`
3. Validates auth (login required for new mints; optional for rebake if on-chain owner)
4. Checks wallet matches user's linked Tezos address (`users` collection)
5. Checks for existing active job (<5min old → reuse, unless `force=true`)
6. Creates job in `keep-jobs` collection via `upsertJob()`
7. Fires off background function: `POST /.netlify/functions/keep-prepare-background`

**Returns:** `{ jobId, status: "preparing", alreadyMinted?, tokenId?, resuming? }`

**Telemetry opportunity:** Currently only returns jobId. Could return estimated stage durations based on historical data.

---

#### 2. `POST /api/keep-prepare-background` — Async Pipeline (Background Function)
**File:** `system/netlify/functions/keep-prepare-background.mjs` (801 lines)
**Runtime:** Netlify Background Function — runs up to 15 minutes, returns 202 immediately.

This is where all the heavy lifting happens. Updates `keep-jobs` MongoDB record at each stage so the client can poll.

**Pipeline Stages with Timing:**

| # | Stage | What Happens | Calls | Typical Duration |
|---|-------|-------------|-------|-----------------|
| 1 | `validate` | Load piece from `kidlisp` collection | MongoDB | <1s |
| 2 | `analyze` | `analyzeKidLisp()` on source code — extract traits, deps | Local | <1s |
| 3 | `cache-check` | Check `kidlisp.ipfsMedia` — if source hash unchanged & <30 days, reuse IPFS URIs | MongoDB | <1s |
| 4 | `thumbnail` | `POST oven/grab-ipfs` — Puppeteer capture + Pinata upload | Oven → Pinata | 10–60s |
| 5 | `bundle` | `GET oven/pack-html?code=...&format=json` — minify, brotli, base64 | Oven | 5–30s |
| 6 | `ipfs-bundle` | Upload HTML to Pinata: `POST pinning/pinFileToIPFS` | Pinata | 5–30s |
| 7 | `metadata` | Build FA2 JSON, upload to Pinata: `POST pinning/pinJSONToIPFS` | Pinata | 2–10s |
| 8 | `security` | `runContractPreflight()` — verify contract hash, admin, fee, permit signer via TzKT | TzKT + RPC | 2–5s |
| 9 | `permit` | `buildKeepPermit()` — sign permit with Ed25519 server key | Local (Taquito) | <1s |
| 10 | `ready` | `markJobReady()` — store all URIs + Michelson params in job | MongoDB | <1s |

**Total typical: 30–120 seconds**

**MongoDB updates at each stage:**
```javascript
// After each stage:
await jobs.updateOne({ _id: jobId }, {
  $set: {
    stage: "thumbnail",
    stageMessage: "Generating preview thumbnail...",
    progress: 35,
    updatedAt: new Date(),
    // Partial results accumulate:
    thumbnailUri: "ipfs://Qm...",  // after stage 4
    artifactUri: "ipfs://Qm...",   // after stage 6
    metadataUri: "ipfs://Qm...",   // after stage 7
  }
});
```

**Error handling:** Any stage failure → `markJobFailed(jobId, error, stage)` → job status = "failed"

---

#### 3. `GET /api/keep-status` — Lightweight Poll
**File:** `system/netlify/functions/keep-status.mjs` (52 lines)

**What it does:** Reads `keep-jobs` collection, returns current state. No external calls.

**Params:** `?jobId=xxx` OR `?piece=xxx&wallet=tz1xxx`

**Returns:**
```json
{
  "stage": "thumbnail",
  "status": "processing",
  "stageMessage": "Generating preview thumbnail...",
  "progress": 35,
  "artifactUri": null,
  "thumbnailUri": "ipfs://QmXxx...",
  "metadataUri": null,
  "preparedData": null,
  "error": null
}
```

**Response time:** ~100ms (MongoDB read only)
**Poll interval:** Client polls every 2 seconds, max 450 polls (15 min timeout)

**Telemetry opportunity:** Could add `stageDurations: { validate: 800, analyze: 200, ... }` for granular timing feedback.

---

#### 4. `POST /api/keep-confirm` — Record Mint
**File:** `system/netlify/functions/keep-confirm.mjs` (379 lines)

**What it does after user signs tx in wallet:**
1. Verify auth (user must own piece)
2. Check tx status via TzKT: `GET /v1/operations/transactions/{hash}`
3. Resolve token ID from bigmap: `GET /v1/contracts/{addr}/bigmaps/content_hashes/keys/{key}`
   - **Retry:** 20 attempts × 1.5s delay (handles TzKT indexer lag — up to ~30s)
4. Update `kidlisp` record with full keep data:
   - `tezos.contracts[CONTRACT].{ txHash, tokenId, mintedAt, owner, minter, artifactUri, thumbnailUri, metadataUri }`
   - `kept.{ tokenId, txHash, walletAddress, network, contractAddress, keptAt }`
5. Clear `pendingKeep` if token confirmed

**Telemetry opportunity:** Return indexer wait time, block confirmation count.

---

#### 5. `POST /api/keep-mint` — Legacy SSE Streaming Endpoint
**File:** `system/netlify/functions/keep-mint.mjs` (1149 lines)

**Architecture:** Uses `@netlify/functions` `stream()` for TransformStream SSE.

**SSE Event Types:**
| Event | When | Data |
|-------|------|------|
| `progress` | Each stage transition | `{ stage, message }` |
| `details` | After validation | `{ piece, source, author, createdAt }` |
| `prepared` | Ready for signing | `{ michelsonParams, entrypoint, mintFee, contractAddress, network, rpcUrl }` |
| `complete` | Mint confirmed | `{ success, piece, tokenId, txHash, objktUrl }` |
| `error` | Any failure | `{ error: "message" }` |

**Note:** The job-based polling flow (`keep-prepare` → `keep-status`) has largely replaced this. But the SSE event structure is still the reference for what telemetry to surface.

---

#### 6. `POST /api/keep-update` — Metadata Sync (SSE)
**File:** `system/netlify/functions/keep-update.mjs` (562 lines)

**Purpose:** Regenerate + sync on-chain metadata for existing keeps.

**Authorization hierarchy:**
1. Admin — always allowed
2. Original minter/creator — always allowed (preserves objkt "Created by" attribution)
3. Token owner — only if `allowOwnerEdit: true` (changes objkt attribution)

**Prepare mode flow:**
1. Build new FA2 metadata JSON
2. Upload to Pinata
3. Build `edit_metadata` Michelson params with MichelsonMap
4. Send "prepared" SSE event → client signs via Beacon
5. Store pending URIs in MongoDB

---

#### 7. `POST /api/keep-update-confirm` — Confirm Metadata Update
**File:** `system/netlify/functions/keep-update-confirm.mjs` (263 lines)

Moves pending URIs to actual URIs after client-side tx confirmation.

---

#### 8. `GET /api/keeps-config` — Contract Configuration
**File:** `system/netlify/functions/keeps-config.mjs` (152 lines)

Returns: `{ contractAddress, network, profile, version, rpcUrl, tzktExplorer, objktBase }`
Source: MongoDB `secrets` collection (`_id: "tezos-kidlisp"`)

---

### Oven Service — Endpoint Map

**Server:** `oven/server.mjs` (3,020 lines)
**Host:** `oven.aesthetic.computer` (137.184.237.166)
**Process manager:** PM2 / systemd

#### Keep-Critical Endpoints

##### `POST /grab-ipfs` — Thumbnail Capture + IPFS Upload
**File:** `oven/grabber.mjs` line 2702

**Request:**
```json
{
  "piece": "$cow",
  "format": "webp",
  "width": 256, "height": 256,
  "density": 2,
  "duration": 4000,
  "fps": 8,
  "quality": 70,
  "pinataKey": "...",
  "pinataSecret": "..."
}
```

**Internal Pipeline:**
1. Launch Puppeteer headless browser
2. Navigate to `https://aesthetic.computer/$piece`
3. Wait `duration` ms, capture frames at `fps`
4. Encode WebP (or animated WebP/GIF) via Sharp
5. Upload to Pinata: `POST https://api.pinata.cloud/pinning/pinFileToIPFS`
6. Cache to DigitalOcean Spaces CDN (7-day TTL)
7. Broadcast via WebSocket to oven dashboard

**Returns:**
```json
{
  "success": true,
  "ipfsUri": "ipfs://QmXxx...",
  "ipfsCid": "QmXxx...",
  "piece": "$cow",
  "format": "webp",
  "mimeType": "image/webp",
  "size": 45230
}
```

**Timeout:** Configurable — `keep-prepare-background` sets 150s (`KEEP_MINT_THUMBNAIL_TIMEOUT_MS`)
**Fallback:** If primary oven fails, retries `OVEN_FALLBACK_URL` (always `https://oven.aesthetic.computer`)

**Telemetry available but not surfaced:**
- Puppeteer launch time
- Page load time
- Frame capture time per frame
- Sharp encoding time
- Pinata upload time + response
- Total elapsed

##### `GET /pack-html` or `/bundle-html` — HTML Bundle Generation
**File:** `oven/bundler.mjs` (1,507 lines), entry: `createBundle()` at line 673

**Query params:** `?code=$cow&format=json&noboxart=1&rebake=1&nocache=1`

**Internal Pipeline:**
1. Load piece source (KidLisp from AC filesystem or fetch from API)
2. Parse imports/dependencies
3. Bundle with minimal runtime: boot.mjs → bios.mjs → disk.mjs → kidlisp.mjs
4. Minify with SWC
5. Compress with Brotli
6. Embed WASM Brotli decoder for self-extraction
7. Generate box art (unless `noboxart=1`)
8. Return as base64 HTML

**Returns (format=json):**
```json
{
  "content": "<base64 HTML>",
  "filename": "$cow.lisp.html",
  "authorHandle": "@jeffrey",
  "userCode": "...",
  "packDate": "2026-03-10",
  "depCount": 15
}
```

**Caching:** In-memory per git commit + file-level caching. `nocache=1` bypasses.
**Timeout:** 60s from `keep-prepare-background`

**Telemetry available but not surfaced:**
- Source fetch time
- Dependency resolution time
- SWC minification time
- Brotli compression time + ratio
- Total bundle size (compressed vs raw)

##### `GET /grab/:format/:width/:height/:piece` — Preview Image (Non-IPFS)
**File:** `oven/grabber.mjs` line 2566

Used during polling to show a live preview thumbnail while baking:
```
GET /grab/webp/480/360/$cow?duration=3000&fps=8&quality=75&density=2&source=keep
```
Cached to DO Spaces CDN. Client fetches this as a "baking in progress" image.

##### `GET /keeps/latest/:piece` — Latest IPFS Thumbnail Redirect
**File:** `oven/server.mjs` line 1376
**Returns:** 302 redirect to `https://ipfs.aesthetic.computer/ipfs/{cid}`

---

### MongoDB Collections

#### `keep-jobs` — Async Job Tracking
**TTL:** 30 minutes (auto-expires via MongoDB TTL index on `expiresAt`)

```
{
  _id: ObjectId,
  piece: string,               // "$cow"
  wallet: string,              // "tz1..."
  status: "preparing" | "ready" | "failed" | "confirmed" | "expired",
  stage: "validate" | "analyze" | "thumbnail" | "bundle" | "ipfs" | "metadata" | "security" | "ready",
  stageMessage: string,        // Human-readable progress
  progress: 0-100,             // Percentage
  isRebake: boolean,
  regenerate: boolean,
  user: string,                // auth0 sub
  handle: string,              // @handle
  artifactUri: "ipfs://...",   // Accumulates as stages complete
  thumbnailUri: "ipfs://...",
  metadataUri: "ipfs://...",
  preparedData: {              // Ready for wallet signing
    michelsonParams: {...},
    entrypoint: "keep",
    mintFee: 2.5,
    contractAddress: "KT1...",
    network: "mainnet",
    rpcUrl: "https://mainnet.ecadinfra.com"
  },
  error: string,
  errorStage: string,
  createdAt: Date,
  updatedAt: Date,
  expiresAt: Date              // TTL index target
}
```

#### `kidlisp` — Piece Records with Keep Metadata

```
{
  code: string,                 // Unique piece name
  source: string,               // KidLisp source code
  user: string,                 // auth0 sub (creator)
  ipfsMedia: {                  // Bundle cache
    artifactUri: "ipfs://...",
    thumbnailUri: "ipfs://...",
    sourceHash: "sha256...",
    packDate: "YYYY-MM-DD",
    depCount: number,
    createdAt: Date
  },
  mediaHistory: [...],          // Previous IPFS media (last 20)
  pendingRebake: {              // During regeneration
    artifactUri, thumbnailUri, sourceHash, createdAt,
    network, contractAddress, contractProfile, contractVersion
  },
  tezos: {
    contracts: {
      "KT1...": {               // Keyed by contract address
        minted: boolean,
        tokenId: number,
        txHash: string,
        network: "mainnet",
        mintedAt: Date,
        owner: "tz1...",
        minter: "tz1...",
        artifactUri: "ipfs://...",
        thumbnailUri: "ipfs://...",
        metadataUri: "ipfs://...",
        lastUpdatedAt: Date,
        lastUpdateTxHash: string
      }
    },
    kept: {                     // Flat record (backward compat)
      tokenId, txHash, walletAddress, network,
      contractAddress, keptAt, keptBy
    }
  }
}
```

#### `users` — Wallet Linking
```
{ _id: "auth0|sub", tezos: { address: "tz1..." } }
```

#### `secrets` — Credentials
- `_id: "pinata"` → `{ apiKey, apiSecret, jwt }`
- `_id: "tezos-kidlisp"` → `{ address, publicKey, privateKey, network, keepsContract, currentKeepsProfile, currentKeepsVersion }`

---

### Tezos Smart Contract Interface

**Contract:** `KT1J15kADMuRWh9kJZzosBeRBYPjYr7RvhoN` (mainnet)

**Entrypoints:**

| Entrypoint | Purpose | Params |
|------------|---------|--------|
| `keep` | Mint new token | name, symbol, description, artifactUri, displayUri, thumbnailUri, decimals, creators, royalties, content_hash, metadata_uri, owner |
| `edit_metadata` | Update token metadata | token_id, token_info (MichelsonMap with `""` key → metadata URI) |

**Storage:**
- `next_token_id` — Counter
- `content_hashes` — Bigmap: bytes → token_id (dedup check)
- `token_metadata` — Bigmap: token_id → token_info
- `keep_fee` — Mint fee in mutez (2.5 XTZ = 2,500,000 mutez)
- `administrator` — Admin address
- `artist_royalty_bps`, `platform_royalty_bps` — Royalty splits

**FA2 Metadata Structure (uploaded to IPFS):**
```json
{
  "name": "$cow",
  "description": "(def cow ...source code...)",
  "artifactUri": "ipfs://QmBundle...",
  "displayUri": "ipfs://QmThumb...",
  "thumbnailUri": "ipfs://QmThumb...",
  "decimals": 0,
  "symbol": "cow",
  "creators": ["tz1creator..."],
  "royalties": { "decimals": 4, "shares": { "tz1creator...": "1000" } },
  "tags": ["$cow", "KidLisp", "Aesthetic Computer", "@jeffrey"],
  "formats": [{ "uri": "ipfs://QmBundle...", "mimeType": "text/html", "dimensions": { "value": "512x384" } }]
}
```

---

### Frontend Flow — Complete Telemetry Path

#### Current Client-Side State Machine

```
openMintModal()
  │
  ├─ renderMintPieceInfo()      ← piece info bar
  ├─ renderTimeline()           ← progress overlay in preview box
  │
  └─ startMintFlow()
       │
       ├─ STEP: wallet          ← connectWallet() via Beacon SDK
       │    └─ setMintStep('wallet', 'active'|'done', address)
       │
       ├─ STEP: validate        ← POST /api/keep-prepare → get jobId
       │    └─ setMintStep('validate', 'active'|'done'|'error')
       │
       └─ pollJobStatus(piece, jobId)  ← GET /api/keep-status every 2s
            │
            │  Each poll maps server stage → client steps:
            │
            ├─ stage: "analyze"     → setMintStep('analyze', 'active')
            ├─ stage: "thumbnail"   → setMintStep('thumbnail', 'active')
            │    └─ Fetch oven preview: GET /grab/webp/480/360/$piece
            │       └─ Display in preview box as "baking" image
            ├─ stage: "bundle"      → setMintStep('bundle', 'active')
            ├─ stage: "ipfs"        → setMintStep('ipfs', 'active')
            │    └─ When thumbnailUri arrives → swap preview to IPFS thumb
            ├─ stage: "metadata"    → setMintStep('metadata', 'active')
            │
            └─ status: "ready"      → STOP POLLING
                 │
                 ├─ If rebake + autoSync:
                 │    └─ syncRebakeMetadataOnChain()
                 │         ├─ POST /api/keep-update (SSE)
                 │         └─ beaconClient.requestOperation()
                 │
                 ├─ If rebake (manual):
                 │    └─ Show "Media regenerated" + artifact links
                 │
                 └─ If initial mint:
                      ├─ renderMintArtifactPanel() ← extra preview boxes (REMOVE)
                      ├─ Show "Confirm & Sign" button
                      │
                      └─ confirmMint()  ← user clicks
                           ├─ beaconClient.requestOperation()  ← wallet popup
                           ├─ POST /api/keep-confirm  ← record on-chain
                           └─ Show success + objkt link
```

#### Mint Step IDs (10 steps, defined in keeps/constants.mjs)

| # | Step ID | Label | Progress % |
|---|---------|-------|-----------|
| 1 | `wallet` | Connect Wallet | 5% |
| 2 | `validate` | Validate Piece | 10% |
| 3 | `analyze` | Analyze Source | 20% |
| 4 | `thumbnail` | Generate Preview | 35% |
| 5 | `bundle` | Pack HTML Bundle | 50% |
| 6 | `ipfs` | Pin to IPFS | 65% |
| 7 | `metadata` | Build Metadata | 75% |
| 8 | `review` | Review & Pay | 85% |
| 9 | `sign` | Sign Transaction | 92% |
| 10 | `complete` | Keep Complete! | 100% |

#### `setMintStep()` — Progress Overlay Renderer (line 4982)

Updates `#mint-progress-stage` (label + detail text) and `#mint-progress-bar-fill` (width %).
Status classes: `done` (green), `error` (red), default (purple).
On `complete` → hides overlay after 1.5s.

#### `addTrackEntry()` — Activity Log (line 5960)

Appends timestamped entries to `#mint-track-log` (max-height 120px, scrollable):
```
04:43:50 PM  Starting keep flow...
04:43:51 PM  Wallet connected: tz1gkf8E...KwBE
04:44:04 PM  Artifacts prepared, ready for signing
```

---

### Telemetry Gaps — What's Missing for Granular UI Feedback

The backend tracks stage transitions but **does not surface sub-stage timing**. Here's what could be added:

#### 1. Sub-Stage Timing in `keep-jobs`

Add `stageTiming` object to job record, updated by `keep-prepare-background`:

```javascript
stageTiming: {
  validate:  { startedAt, completedAt, durationMs: 800 },
  analyze:   { startedAt, completedAt, durationMs: 200 },
  thumbnail: {
    startedAt, completedAt, durationMs: 42000,
    sub: {
      puppeteerLaunch: 3200,
      pageLoad: 8500,
      frameCapture: 12000,
      sharpEncode: 2100,
      pinataUpload: 16200
    }
  },
  bundle: {
    startedAt, completedAt, durationMs: 18000,
    sub: {
      sourceFetch: 500,
      depResolve: 2000,
      swcMinify: 8000,
      brotliCompress: 3500,
      boxArt: 0,
      bundleSize: 245000,
      compressedSize: 62000,
      compressionRatio: 0.25
    }
  },
  ipfs: {
    startedAt, completedAt, durationMs: 12000,
    sub: {
      pinataUpload: 11500,
      cid: "QmXxx..."
    }
  },
  metadata: { startedAt, completedAt, durationMs: 3000 },
  security: {
    startedAt, completedAt, durationMs: 2500,
    alerts: [],
    preflightPassed: true
  },
  permit:    { startedAt, completedAt, durationMs: 150 }
}
```

**Implementation:** The background function already has `console.log` timing — just capture those values into the job record.

#### 2. Oven Telemetry Response Enhancement

Currently `/grab-ipfs` returns only the final result. Enhance to include:

```json
{
  "success": true,
  "ipfsUri": "ipfs://Qm...",
  "timing": {
    "puppeteerMs": 3200,
    "pageLoadMs": 8500,
    "captureMs": 12000,
    "encodeMs": 2100,
    "uploadMs": 16200,
    "totalMs": 42000
  },
  "meta": {
    "framesCaptured": 32,
    "outputSize": 45230,
    "format": "webp",
    "dimensions": "512x512"
  }
}
```

Similarly for `/pack-html`:
```json
{
  "content": "...",
  "timing": {
    "sourceFetchMs": 500,
    "depResolveMs": 2000,
    "minifyMs": 8000,
    "compressMs": 3500,
    "totalMs": 14000
  },
  "meta": {
    "rawSize": 245000,
    "compressedSize": 62000,
    "depCount": 15,
    "compressionRatio": 0.25
  }
}
```

#### 3. Client-Side Granular Progress

With sub-stage timing from the backend, the progress overlay can show:

```
┌───────────────────────────────┐
│                               │
│     [piece running live]      │
│                               │
│  ┌─────────────────────────┐  │
│  │ ▶ Generating Preview    │  │
│  │   Capturing frames...   │  │
│  │   ████████░░ 12/32      │  │
│  │   ~18s remaining        │  │
│  └─────────────────────────┘  │
└───────────────────────────────┘
```

Instead of the current coarse 10-step progress bar, show:
- **Sub-step label** (e.g., "Capturing frames...", "Compressing bundle...", "Uploading to IPFS...")
- **Sub-progress** within each stage (frame count, bytes uploaded, etc.)
- **Time estimate** based on `stageTiming` from previous jobs for this piece

#### 4. Historical Timing for Estimates

Store completed job timings in `kidlisp.keepHistory`:
```javascript
keepHistory: [{
  when: Date,
  totalMs: 85000,
  stageTiming: { ... },
  bundleSize: 62000,
  depCount: 15
}]
```

Use the average of last 3 jobs to estimate "~Xs remaining" during each stage.

---

### External Service Dependency Map

| Service | Used By | Endpoint | Timeout | Fallback |
|---------|---------|----------|---------|----------|
| **Oven** (grab-ipfs) | keep-prepare-bg | `POST oven.ac/grab-ipfs` | 150s | `OVEN_FALLBACK_URL` |
| **Oven** (pack-html) | keep-prepare-bg | `GET oven.ac/pack-html` | 60s | None |
| **Oven** (grab preview) | Client JS | `GET oven.ac/grab/webp/...` | N/A | Noise placeholder |
| **Pinata** (file) | keep-prepare-bg | `POST api.pinata.cloud/pinning/pinFileToIPFS` | 90s | None |
| **Pinata** (json) | keep-prepare-bg | `POST api.pinata.cloud/pinning/pinJSONToIPFS` | 30s | None |
| **TzKT** (tokens) | keep-prepare, keep-confirm | `GET api.tzkt.io/v1/tokens?...` | 10s | None |
| **TzKT** (bigmap) | keep-confirm | `GET .../bigmaps/content_hashes/keys/...` | 30s (20×1.5s retry) | None |
| **TzKT** (contract) | keep-prepare-bg (preflight) | `GET .../contracts/{addr}` | 10s | None |
| **Tezos RPC** | keep-prepare-bg, keep-mint | `POST mainnet.ecadinfra.com` | 30s | None |
| **MongoDB** | All functions | Atlas connection | 5s | None |
| **Beacon SDK** | Client JS | Wallet popup | User-dependent | None |
| **DO Spaces CDN** | Oven | `art.aesthetic.computer` | 10s | None |

---

### Security Preflight Checks (keep-prepare-background lines 264–346)

| Check | What | Source | Strict? |
|-------|------|--------|---------|
| Contract address | Matches `KEEP_MINT_EXPECTED_CONTRACT` | Env var | Yes |
| Admin address | Matches `KEEP_MINT_EXPECTED_ADMIN` | TzKT | Yes |
| Code hash | Matches `KEEP_MINT_EXPECTED_CODE_HASH` | TzKT | Yes |
| Type hash | Matches `KEEP_MINT_EXPECTED_TYPE_HASH` | TzKT | Yes |
| Keep fee | >= `KEEP_MINT_MIN_EXPECTED_FEE_MUTEZ` | RPC storage | Yes |
| Permit signer | Matches `KEEP_MINT_EXPECTED_PERMIT_SIGNER` | RPC storage | Yes |
| Recent operations | Scan for critical admin entrypoints | TzKT | Alert only |

Violations logged but only block if `KEEP_MINT_STRICT_PREFLIGHT=true` or `KEEP_MINT_BLOCK_ON_ALERT=true`.

---

### Key Environment Variables

| Variable | Default | Purpose |
|----------|---------|---------|
| `TEZOS_NETWORK` | `mainnet` | Active network |
| `OVEN_URL` | `https://oven.aesthetic.computer` | Oven service |
| `OVEN_FALLBACK_URL` | `https://oven.aesthetic.computer` | Oven fallback |
| `KEEP_MINT_THUMBNAIL_TIMEOUT_MS` | `150000` | Thumbnail timeout |
| `KEEP_MINT_PERMIT_TTL_MS` | `1200000` (20min) | Permit expiry |
| `KEEP_MINT_SIGNER_CACHE_TTL_MS` | `30000` | Signer credential cache |
| `KEEP_MINT_EXPECTED_CONTRACT` | — | Security check |
| `KEEP_MINT_EXPECTED_ADMIN` | — | Security check |
| `KEEP_MINT_STRICT_PREFLIGHT` | `false` | Block on violations |
| `USE_IPFS_GATEWAY_URLS` | `false` | Use gateway vs ipfs:// |
| `IPFS_GATEWAY` | `https://ipfs.aesthetic.computer` | Gateway URL |

---

## Part 3: Speed Optimizations

### Current Pipeline Timing (Sequential)

```
thumbnail (fire async) ──────────────────────────────────┐
bundle (await) ──────────────────┐                       │
                                 ├─ ipfs upload (async) ──┤
                                 │                        ├─ await both
                                 │                        │
metadata ─────────────────────── │ ←── needs artifact +   │
                                 │     thumbnail URIs     │
security preflight ──────────────┤ ←── sequential, after  │
                                 │     metadata!          │
permit ──────────────────────────┘                        │
```

**Problem:** Security preflight (2–5s) runs *after* metadata upload, but it doesn't depend on any IPFS URIs — it only checks the contract. It could run much earlier.

### Optimization 1: Parallelize Preflight with IPFS Uploads

**File:** `system/netlify/functions/keep-prepare-background.mjs`
**Saves:** 2–5s

Move `runContractPreflight()` to fire in parallel with the thumbnail/bundle/IPFS work. It only needs the contract address, which is known from the start.

**Current flow** (lines 678–686):
```
... await ipfs upload ...
... await thumbnail ...
security preflight ← SEQUENTIAL, blocks metadata
metadata upload
permit
ready
```

**New flow:**
```
preflight (fire async) ─────────────────────────────┐
thumbnail (fire async) ─────────────────────────┐   │
bundle (await) → ipfs upload (async) ───────────┤   │
                                    await all ──┼───┘
metadata upload (needs URIs + preflight.royalties)
permit
ready
```

### Optimization 2: Adaptive Poll Interval

**File:** `system/public/kidlisp.com/keeps.html`, `pollJobStatus()` at line 5243
**Saves:** ~1–2s perceived latency

Currently polls every 2000ms fixed. Change to:
- **First 10 polls (0–20s):** 800ms — fast feedback during validate/analyze/early thumbnail
- **Polls 11–30 (20–60s):** 1500ms — thumbnail/bundle usually in progress
- **Polls 31+ (60s+):** 3000ms — long-running IPFS uploads, less frequent is fine

### Optimization 3: Frame-Diff Static Detection in Grabber

**File:** `oven/grabber.mjs`, inside `grabPiece()`
**Saves:** 2–4s on static pieces (shorter capture + single-frame WebP)

After capturing the first 2–3 frames, compare pixel data. If frames are identical → piece is static → stop capturing early, output single-frame WebP instead of animated.

**Algorithm:**
1. Capture frame at t=500ms (after piece boot)
2. Capture frame at t=1500ms
3. Compare raw pixel buffers (Sharp `.raw()` → `Buffer.compare()`)
4. If identical → static: output single WebP from frame 1, skip remaining duration
5. If different → animated: continue full capture at configured fps/duration

No need to cache the static flag — just detect it each time during the grab. The comparison adds ~50ms but can save 2–4s of capture + encoding time.

### Optimization 4: Pre-warm Bundles on Deploy

**File:** oven deploy script / post-deploy hook
**Saves:** 5–30s on first keep after deploy (eliminates cold bundle cache)

The oven already has `POST /pack-prewarm`. After deploy, trigger it for all kept pieces:
```bash
# In deploy script, after restart:
curl -X POST https://oven.aesthetic.computer/pack-prewarm \
  -H "Content-Type: application/json" \
  -d '{"pieces": ["$kl1", "$cow", "$fib", ...]}'
```

Could also auto-populate the list from MongoDB `kidlisp` collection where `tezos.kept` exists.

### Implementation Order

| # | Optimization | Files | Effort | Impact |
|---|-------------|-------|--------|--------|
| 1 | Parallelize preflight | keep-prepare-background.mjs | Low | 2–5s saved |
| 2 | Adaptive polling | keeps.html | Low | ~1–2s perceived |
| 3 | Frame-diff static detection | oven/grabber.mjs | Medium | 2–4s on static pieces |
| 4 | Pre-warm on deploy | oven deploy script | Low | 5–30s on first keep post-deploy |
