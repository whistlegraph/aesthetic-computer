# Dependency Update Tracking

_Last Updated: June 2025_

This file tracks dependency versions and updates across the aesthetic-computer project.

---

## 📦 system/package.json (Node Dependencies)

Updated via `npx ncu -u` on June 2025:

| Package | Previous | Updated | Notes |
|---------|----------|---------|-------|
| @aws-sdk/client-s3 | 3.932.0 | 3.975.0 | Minor AWS SDK update |
| @aws-sdk/s3-request-presigner | 3.932.0 | 3.975.0 | Minor AWS SDK update |
| netlify-cli | 23.13.0 | 23.13.5 | Patch update |
| openai | 6.9.0 | 6.16.0 | Minor update |
| playwright-core | 1.56.0 | 1.58.0 | Minor update |
| puppeteer-core | 24.30.0 | 24.36.0 | Minor update |
| redis | 5.9.0 | 5.10.0 | Minor update |
| stripe | 19.3.1 | 20.2.0 | **⚠️ MAJOR UPDATE** - Review changelog! |
| three | 0.181.0 | 0.182.0 | Minor update |

### Action Taken ✅
- [x] Run `npm install` in `/workspaces/aesthetic-computer/system`
- [x] Review Stripe 20.x breaking changes: https://github.com/stripe/stripe-node/releases

### Stripe 19→20 Migration Notes
The v20 breaking changes are **LOW IMPACT** for this project:
- V2 API array serialization changes - Only affects `/v2` endpoints (not used here)
- V2 Event namespace moves - Not using V2 events
- Node.js 16+ required - Already on Node 20+

Usage locations reviewed:
- `system/netlify/functions/email.js` - customers.search, customers.update
- `system/netlify/functions/print.js` - checkout.sessions, webhooks, refunds
- `system/netlify/functions/gives.mjs` - sessions.list, subscriptions.list
- `system/netlify/functions/mug.js` - checkout.sessions, webhooks

All use standard v1 APIs unaffected by v20 breaking changes.

---

## 📁 /dep Folder (Vendored Client Libraries)

These are manually copied client-side dependencies in `system/public/aesthetic.computer/dep/`:

| Library | Vendored Version | Latest | Status | Notes |
|---------|-----------------|--------|--------|-------|
| **three.js** | **0.182.0** | 0.182.0 | ✅ **UPDATED** | Updated from r145 (June 2025) |
| aframe | v1.3.0 | 1.6.0+ | ⚠️ Outdated | Check compatibility |
| auth0-spa-js | 2.1.2 | 2.1.4+ | ✅ Close | Minor updates only |
| geckos.io-client | 2.3.2, 3.0.2 | 3.x | ✅ OK | Both versions present |
| web3 | Unknown (minified) | 4.x | ❓ Check | Version hard to determine |
| gl-matrix | Unknown | 3.4.3 | ❓ Check | Version hard to determine |
| jszip | Unknown (minified) | 3.10.1 | ❓ Check | |
| nanoid | Unknown | 5.x | ❓ Check | |
| idb.js | Unknown | 8.x | ❓ Check | |

### /dep Subfolders
- `@akamfoad/` - Custom/fork dependency
- `@mediapipe/` - MediaPipe vision models
- `ffmpeg/` - WASM FFmpeg
- `gif/`, `gifenc/` - GIF encoding
- `gpt3-tokenizer/` - GPT tokenizer
- `sm/` - Unknown
- `tasks-vision/` - MediaPipe tasks
- `wasmboy/` - GameBoy emulator
- `webpxmux/` - WebP muxer

### Three.js Update ✅ COMPLETED
The vendored Three.js has been updated from r145 to 0.182.0.

**Files updated:**
- `three.core.js` (1.4MB) - New modular core
- `three.module.js` (631KB) - ES module entry
- All addon files with fixed import paths (`from './three.module.js'`)

**Action Required:**
- [ ] Test 3D pieces thoroughly (use `3d.mjs` or similar pieces)

---

## 🐳 Dockerfile (CLI Tool Versions)

Located at `/workspaces/aesthetic-computer/Dockerfile`:

| Tool | Pinned Version | Latest | Status |
|------|---------------|--------|--------|
| Fedora | latest | latest | ✅ OK |
| Deno | v2.6.6 | 2.6.x | ✅ **UPDATED** |
| Stripe CLI | v1.34.0 | 1.34.x | ✅ **UPDATED** |
| redli | v0.15.0 | 0.15.x | ✅ OK |
| mkcert | v1.4.4 | 1.4.4 | ✅ OK |
| doctl | 1.149.0 | 1.149.x | ✅ **UPDATED** |
| Node (fnm) | lts-jod + 20.5.0 | 22.x LTS | ⚠️ Consider update |
| SmartPy | 0.22.0 | 0.23.x+ | ⚠️ Check |
| pytezos | >=3.9.0,<4 | 3.x | ✅ OK |
| Octez | v20.3-1 | v21.x+ | ⚠️ Check |

### Action Taken ✅
- [x] Update Stripe CLI: v1.30.0 → v1.34.0
- [x] Update doctl: 1.109.0 → 1.149.0  
- [x] Update Deno: v2.4.5 → v2.6.6

### Action Required
- [ ] Rebuild devcontainer to apply Dockerfile changes
- [ ] Consider Node.js 22 LTS upgrade

---

## 📝 entry.fish (Startup Script)

The 818-line startup script at `/workspaces/aesthetic-computer/entry.fish` handles:
- Environment setup
- Tool installation
- Service initialization

No pinned versions found in script - versions come from Dockerfile or package managers.

---

## 🔄 Update Procedures

### For package.json dependencies:
```bash
cd /workspaces/aesthetic-computer/system
npm install
npm test  # Run tests to verify
```

### For /dep vendored libraries:
1. Download new version from source
2. Replace files in `/dep/<library>/`
3. Test affected pieces thoroughly
4. Update this tracking document

### For Dockerfile:
1. Update version in Dockerfile
2. Rebuild devcontainer: `devcontainer rebuild`
3. Test all tools work correctly

---

## ⚠️ Known Issues

1. **Three.js Version Mismatch**: `/dep/three/` is r145, package.json is 0.182.0
   - Potential incompatibility if both are used
   - 37+ releases of bug fixes and features missing

2. **Stripe Major Update**: 19.x → 20.x may have breaking API changes
   - Review: https://github.com/stripe/stripe-node/blob/master/CHANGELOG.md

---

## � Security Audit (system/package.json)

After `npm install`, 31 vulnerabilities reported (20 low, 3 moderate, 8 high).

Most vulnerabilities stem from blockchain/wallet dependencies:
- `elliptic` - Cryptographic primitive issues
- `@taquito/*` - Tezos SDK chain
- `@walletconnect/*` - WalletConnect chain
- `@airgap/beacon-*` - Beacon wallet chain

**Note**: These are in the Tezos integration and may require major version upgrades to fix.

---

## 📅 Update History

| Date | Changes |
|------|---------|
| 2025-06 | Initial dependency audit, package.json updates via ncu, npm install completed |

