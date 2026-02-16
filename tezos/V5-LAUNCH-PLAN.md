# Keeps FA2 v5 Launch Plan

**Date:** February 16, 2026
**Goal:** Deploy final production v5 contract on keeps.tez with revenue enabled
**Status:** Phase 1 in progress

---

## Current State

- **v4 staging contract** live on mainnet: `KT1ER1GyoeRNhkv6E57yKbBbEKi5ynKbaH3W`
  - Admin: staging wallet (`tz1dfoQDuxjwSgxdqJnisyKUxDHweade4Gzt`)
  - Fee: 0 XTZ (no revenue)
  - 1 token minted so far
- **v5 contract source** created: `tezos/keeps_fa2_v5.py`
  - Default fee: 2.5 XTZ (`sp.mutez(2500000)`)
  - Improved error messages (`MINTING_PAUSED`, `EDITING_PAUSED`)
  - All v4 features preserved (royalties, pause, admin transfer)
- **`KEEPS_STAGING = true`** still active in `lib/keeps/constants.mjs`
- **Old v2 reference fixed** in `kidlisp-keep.mjs` (was pointing to `KT1Ecs...`)

---

## Client Bugs to Fix Before Launch

These issues exist in `system/public/aesthetic.computer/disks/keep.mjs` and must
be resolved before v5 production deployment.

### Bug 1: Rebake triggers `jump()` to IPFS URL

**Symptom:** Clicking Rebake button causes navigation away from the piece via
`jump("out:https://ipfs.io/ipfs/Qm...")` instead of staying on the keep page.

**Root cause:** The `stickyScrubbing` on `rebakeBtn` triggers a `scrub` event
that interacts with `openUrl()` calls on overlapping buttons (htmlBtn, thumbBtn).
When the rebake button overlaps with asset link buttons, the `push` handler for
the link button fires during the same event cycle.

**Console evidence:**
```
UI: scrub -> Object
jump() called: out:https://ipfs.io/ipfs/Qmd7cuKDQBBE...
Jumping to web URL: https://ipfs.io/ipfs/Qmd7cuKDQBBE...
```

**Fix needed:** Either:
- Guard `openUrl` calls to not fire while `rebaking` is true
- Ensure rebake button handlers return early / stop event propagation
- Move button `act()` registrations so they don't overlap when rebake is active

### Bug 2: Paint function not updating during rebake progress

**Symptom:** Progress messages show in console (`REBAKE: progress Object`) but
the on-screen UI in `paint()` doesn't reflect them.

**Root cause:** The `rebakeProgress` text rendering at line ~2081 depends on
`rebaking` being true AND `rebakeProgress` being set. The paint section at line
2081 renders the progress text AFTER the button row, but the `y` cursor may
already be past the visible area. Additionally, if the rebake completes very
fast (SSE events arrive in a burst), the `_needsPaint?.()` calls may not
trigger visible repaints between events.

**Fix needed:**
- Ensure `rebakeProgress` text renders in a fixed, visible location (not
  dependent on accumulated `y` offset)
- Consider debouncing `_needsPaint` calls or ensuring the paint loop is
  actually running during async operations

### Bug 3: `keep~$berz` 404 after rebake navigation

**Symptom:** After the unwanted `jump()`, the system tries to load
`keep~$berz` as a piece path and gets a 404.

**Root cause:** This is a downstream effect of Bug 1. The `jump("out:...")` call
navigates away, and when the system tries to resolve the return path, it
constructs `keep~$berz` (the colon-separated piece + params format) which
doesn't exist as a loadable resource.

**Fix needed:** Fixing Bug 1 eliminates this issue.

---

## Launch Phases

### Phase 0: Client Bug Fixes (NOW)

| Task | File | Status |
|------|------|--------|
| Fix rebake button triggering `jump()` to IPFS | `disks/keep.mjs` | DONE |
| Fix paint not updating during rebake progress | `disks/keep.mjs` | DONE |
| Test rebake flow end-to-end in staging | Manual | TODO |
| Test "Update Chain" flow after rebake | Manual | TODO |

### Phase 1: v5 Contract (DONE)

| Task | File | Status |
|------|------|--------|
| Create `keeps_fa2_v5.py` from v4 | `tezos/keeps_fa2_v5.py` | DONE |
| Set default fee to 2.5 XTZ | `keeps_fa2_v5.py:95` | DONE |
| Improve error messages | `keeps_fa2_v5.py` | DONE |
| Fix stale v2 contract ref in kidlisp-keep | `kidlisp-keep.mjs:16` | DONE |

### Phase 2: Ghostnet Testing

| Task | Status |
|------|--------|
| Compile v5 with SmartPy | TODO |
| Deploy to Ghostnet (`--wallet=aesthetic`) | TODO |
| Test admin mint (no fee) | TODO |
| Test user mint (2.5 XTZ fee) | TODO |
| Test insufficient fee rejection | TODO |
| Test royalty metadata on TzKT | TODO |
| Test content hash dedup | TODO |
| Test emergency pause/unpause | TODO |
| Test admin transfer | TODO |
| Test metadata editing | TODO |
| Test fee adjustment | TODO |
| Test fee withdrawal | TODO |
| Test Oven thumbnail generation | TODO |
| Verify on ghostnet.objkt.com | TODO |

### Phase 3: Mainnet Staging

| Task | Status |
|------|--------|
| Deploy v5 to mainnet (`--wallet=staging`) | TODO |
| Test with real XTZ (mint 3 tokens) | TODO |
| Verify on objkt.com | TODO |
| Test secondary sale royalties | TODO |
| Test fee withdrawal (real XTZ) | TODO |
| End-to-end UI flow via aesthetic.computer | TODO |
| Go/No-Go decision | TODO |

### Phase 4: Production Deploy

| Task | Status |
|------|--------|
| Deploy v5 to mainnet (`--wallet=kidlisp`) | TODO |
| Update `constants.mjs` contract address | TODO |
| Set `KEEPS_STAGING = false` | TODO |
| Update env vars (Netlify) | TODO |
| Update `keep-mint.mjs` contract address | TODO |
| Update `keep-update.mjs` contract address | TODO |
| Update `keep-update-confirm.mjs` contract address | TODO |
| Update `kept.mjs` contract address | TODO |
| Update `wallet.mjs` contract address | TODO |
| Smoke test: first production mint | TODO |
| Verify on objkt.com | TODO |

### Phase 5: Communication & Revenue

| Task | Status |
|------|--------|
| Announce v5 launch | TODO |
| Monitor first week mints | TODO |
| Track fee collection | TODO |
| A/B test fee amount if needed | TODO |

---

## Files That Need Contract Address Updates at Launch

When v5 deploys, update the contract address in ALL of these:

**Constants (single source of truth):**
- `system/public/aesthetic.computer/lib/keeps/constants.mjs` — NETWORKS.mainnet.contract

**Backend functions (env var fallbacks):**
- `system/netlify/functions/keep-mint.mjs:31`
- `system/netlify/functions/keep-update.mjs:24`
- `system/netlify/functions/keep-update-confirm.mjs:12`
- `system/netlify/functions/kidlisp-keep.mjs:16`

**Frontend pieces (hardcoded refs):**
- `system/public/aesthetic.computer/disks/kept.mjs:13`
- `system/public/aesthetic.computer/disks/wallet.mjs:14`

**Feature flag:**
- `system/public/aesthetic.computer/lib/keeps/constants.mjs:10` — `KEEPS_STAGING = false`

---

## Key Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Fee amount | 2.5 XTZ (~$2.50) | Covers IPFS + margin, adjustable via `set_keep_fee` |
| Royalty | 10% (1000 bps) | Industry standard, adjustable via `set_default_royalty` |
| Old v2 contract | Keep both (no migration) | Zero disruption to existing holders |
| Solana | Parallel track | Don't delay Tezos revenue |
| Production admin | kidlisp wallet (`tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC` / keeps.tez) | Permanent production admin |
