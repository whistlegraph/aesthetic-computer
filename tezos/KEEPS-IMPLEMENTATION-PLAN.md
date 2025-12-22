# Keeps Implementation Plan

## Overview
Enable users to mint self-contained KidLisp pieces as FA2 NFTs on Tezos.

**User Flow:**
```
keep $ceo → Preview bundle → Connect wallet → Upload to IPFS → Mint NFT
```

---

## Launch Plan (Ghostnet → Mainnet)

### Goals
- Ship a KEEP flow that is reliable, debuggable, and resistant to accidental duplicate IPFS uploads.
- Keep the “happy path” simple for creators while preserving an escape hatch (“rekeep” / regenerate media) for when runtime-dependent outputs change.

### Current Reality (Dec 2025)
- The minting endpoint is implemented as an SSE Netlify function (see `system/netlify/functions/keep-mint.mjs`).
- IPFS artifacts are pinned via Pinata credentials stored in MongoDB (`secrets.pinata`).
- Tezos minter credentials are stored in MongoDB (`secrets.tezos-kidlisp`) for server-side signing when using server-mint mode.
- Client UX uses the dedicated `keep` piece rather than doing mint logic inside `prompt`.
- IPFS media caching exists: `kidlisp.ipfsMedia` can be reused when source is unchanged; request can force refresh with `regenerate: true`.

**Recent Progress (Dec 18, 2025):**
- ✅ Contract deployed to Ghostnet (KT1... address obtained)
- ✅ kidlisp.com integration: Keeps tab UI with wallet connection (Temple/Kukai)
- ✅ Beacon SDK v4.0.12 integrated (avoided v4.6.3 IndexedDB issues)
- ✅ Temple wallet direct connection via postMessage API
- ✅ AC login integration for keeps tab (separate from Tezos wallet)
- ✅ MongoDB Tezos address storage and domain resolution
- ✅ Client-side wallet state management with piece transitions
- ⚠️ Temple wallet doesn't support message signing via postMessage
- 🔄 **CURRENT**: Testing keep flow in prompt.mjs/keep.mjs pieces
- 📋 **NEXT**: Harden Ghostnet implementation before mainnet deploy

**Recent Progress (Jan 2, 2025):**
- ✅ `wallet.mjs` piece now displays user's KidLisp pieces and existing Keeps
- ✅ Two-column layout: "Kept" (already minted) vs "Unkept" (mintable)
- ✅ KEEP buttons implemented with `ui.TextButton` for hover/down states
- ✅ KEEP button navigation fixed (`keep~$code` not `kept~$code`)
- ✅ `keep.mjs` shows 10-step minting timeline with wallet connection
- ✅ Pac-Man ghost sprites added for GHOSTNET badge in wallet.mjs and keep.mjs
- ✅ `keep-confirm` API endpoint redirect added to netlify.toml

**Recent Progress (Dec 21, 2025):**
- ✅ **Batch minting via Artery TUI**: Queue management system for keeping multiple pieces
  - `Q` key opens queue view, `K` browses user's pieces, `q`/space adds to queue
  - `A` processes entire queue automatically with 1.5s delay between mints
  - Queue shows piece names, hit counts, and processing status
- ✅ **Sort by hits**: API now supports `sort=hits` for all-time popularity ranking
  - `store-kidlisp.mjs` adds `sort` and `handle` query parameters
  - Server-side filtering by creator handle (`handle=@jeffrey`)
- ✅ **Kept status detection**: Fixed hex-decoding of TzKT bigmap keys
  - TzKT returns hex-encoded piece names (e.g., "636f77" = "cow")
  - Proper `Buffer.from(key, 'hex').toString('utf8')` decoding
- ✅ **keeps.mjs CLI enhancements**: 
  - `--to=<address>` flag for minting to specific wallet
  - `set-admin` command to change contract administrator
- ✅ **Batch minted 189 pieces** via TUI successfully on ghostnet v2
- ⚠️ **Artist attribution issue discovered**: v2 contract has wrong `firstMinter`
  - TzKT sets `firstMinter` to transaction sender (admin), not token recipient
  - objkt.com uses `firstMinter` for artist attribution when `creators` missing
  - Fix: Contract v3 allows users to call `keep()` directly

**Recent Progress (Dec 22, 2025):**
- ✅ **Contract v3 deployed**: `KT1StXrQNvRd9dNPpHdCGEstcGiBV6neq79K`
  - User-callable `keep()` entrypoint for proper `firstMinter` attribution
  - Non-admins can call with fee payment, must mint to self
  - All code references updated to new contract address
  - Contract tracking system in `contracts.json`
  - Deployment documented in README.md
- ✅ **Contract tracking system**: `contracts.json` in /tezos
  - Tracks all deployed contracts by network and version
  - Includes deployment date, wallet, status, notes
- ✅ **Updated README.md**: Wallet rolodex with contract history
- ✅ **Fixed creators array**: `["@handle", walletAddress]` format
  - Handle first for objkt.com artist display
  - Wallet address for on-chain attribution
- ✅ **COMPLETED**: V3 contract deployed to ghostnet
  - Contract: `KT1StXrQNvRd9dNPpHdCGEstcGiBV6neq79K`
  - User-callable `keep()` with `MUST_MINT_TO_SELF` enforcement
- 🔄 **CURRENT**: Testing v3 user-callable mint flow
- 📋 **NEXT**: Verify objkt.com shows correct artist attribution

### Phase A — Ghostnet hardening ✅ MOSTLY COMPLETE
- **Status**: Contract v3 deployed and active
- **Progress**: 189 tokens minted on v2 (deprecated), v3 ready for testing
- **Contract**: `KT1StXrQNvRd9dNPpHdCGEstcGiBV6neq79K`
- Success criteria:
  - ✅ SSE sessions stable
  - ✅ IPFS caching working
  - ✅ V3 contract deployed with user-callable `keep()`
  - ⏳ User is `firstMinter` when minting (needs testing)
  - ⏳ objkt.com shows correct artist (needs testing)
- **Remaining**:
  1. ✅ Deploy v3 contract to ghostnet
  2. ✅ Update contract addresses in code
  3. ⏳ Test user-callable mint flow in keep.mjs
  4. ⏳ Verify objkt.com artist attribution on v3 tokens

### Phase B — Mainnet staging (real XTZ)
- Use the `staging` wallet (mainnet) to deploy and test with a small set of keeps.
- Success criteria: 3–5 keeps, confirmed on tzkt + discoverable on objkt.
- Verify:
  - Contract address + network routing are correct
  - Metadata renders correctly on marketplaces
  - Gas/storage costs are acceptable and repeatable

### Phase C — Mainnet production
- Deploy the final contract using the `kidlisp` wallet.
- Freeze any mutable metadata policy decisions before public launch (what can be edited, for how long).

---

## Contract Deployment Guide

### Contract Source
- **File**: `tezos/keeps_fa2_v2.py` (SmartPy)
- **Version**: Track in `tezos/contracts.json`

### Pre-deployment Checklist
1. [ ] Verify contract source changes are committed
2. [ ] Check wallet balance: `node keeps.mjs balance [network]`
3. [ ] Review `contracts.json` for version history
4. [ ] Ensure RPC is healthy (ghostnet/mainnet)

### Deployment Steps

```bash
cd /workspaces/aesthetic-computer/tezos

# 1. Compile contract (if using SmartPy IDE or CLI)
# SmartPy compiles to Michelson automatically during deployment

# 2. Deploy to network
node keeps.mjs deploy [ghostnet|mainnet]

# 3. Verify deployment
node keeps.mjs status [network]
```

### Post-deployment Checklist
1. [ ] Update `contracts.json`:
   - Add new version entry with address, timestamp, status
   - Mark previous version as "deprecated"
2. [ ] Update `README.md` contract table
3. [ ] Update environment variables:
   - `TEZOS_KEEPS_CONTRACT` in Netlify
   - Contract address in `keep-mint.mjs`, `kidlisp-keep.mjs`
4. [ ] Test mint flow end-to-end
5. [ ] Verify on tzkt.io and objkt.com

### Updating Contract Address in Code

```bash
# Find all references to old contract address
grep -rn "KT1KRQAkCrgbYPAxzxaFbGm1FaUJdqBACxu9" system/ tezos/

# Files to update:
# - system/netlify/functions/keep-mint.mjs (CONTRACT_ADDRESS)
# - system/netlify/functions/kidlisp-keep.mjs (CONTRACT_ADDRESS)
# - tezos/keeps.mjs (contractAddresses)
# - Netlify env vars (TEZOS_KEEPS_CONTRACT)
```

### Contract Versioning Policy
- **v1, v2, etc.**: Major changes requiring redeployment
- Previous contracts remain viewable but marked deprecated
- Old tokens remain on-chain (cannot migrate)
- New mints go to latest contract only

---

### Operational checklist
- Pinata:
  - Ensure `secrets.pinata` exists and has working API key/secret/JWT
  - Confirm `ipfs.aesthetic.computer` gateway performance/uptime
- Tezos:
  - Ensure `secrets.tezos-kidlisp` exists and matches intended network
  - Confirm RPC health (mainnet/ghostnet)
  - Confirm contract storage/entrypoints match expected features (fee system, admin ops)
- Mongo:
  - Ensure indexes/fields won’t explode doc size (`ipfsMedia` should stay small)
  - Confirm writes on prepare/mint are safe (no secrets logged)
- Rollback plan:
  - If marketplace display breaks: regenerate only metadata + thumbnail (don’t redeploy contract)
  - If a contract bug exists: stop minting endpoint; redeploy contract; keep old contract viewable

---

## User Stories (Launch Quality)

### Creator (happy path)
**As a creator**, I want to keep my KidLisp piece as an NFT so I can share it as a collectible.
- Acceptance:
  - I can enter `keep $piece` and see a clear progress timeline.
  - I can review what will be minted (name/preview/network/fee) before signing.
  - After success I get links to tzkt + objkt.

### Creator (repeat attempts without IPFS spam)
**As a creator**, I want re-running “prepare” to reuse already-generated IPFS media so I don’t create duplicates.
- Acceptance:
  - Running prepare twice for the same unchanged source reuses `kidlisp.ipfsMedia`.
  - The UI indicates when cached media is used.

### Creator (rekeep / regenerate)
**As a creator**, I want a “Regenerate Media” action when the runtime output changed, without changing my source.
- Acceptance:
  - The client can request `regenerate: true` to force new thumbnail/bundle pins.
  - The resulting cached media is updated in MongoDB.
  - Old media is still addressable (IPFS is content-addressed) but is no longer referenced.

### Operator / Support
**As an operator**, I want enough telemetry to diagnose failures quickly.
- Acceptance:
  - SSE stages map clearly to server logs.
  - User-facing errors are specific (auth vs ownership vs oven vs pinata vs contract).
  - We can identify if failures cluster on a specific stage.

### Security
**As AC**, we need to ensure “keep” cannot be used to exfiltrate secrets or mint other users’ pieces.
- Acceptance:
  - Auth/ownership checks block non-owners (admin exception is explicit).
  - Pinata and Tezos secrets never leave the server; no client-visible leakage.
  - “Already minted” returns a safe response including public links only.

---

## Notes on IPFS duplicates and mutability
- IPFS is content-addressed, but the bundle/thumbnail can change even when the source is unchanged (runtime/version/environment changes).
- Strategy:
  - Default: reuse cached media for unchanged source to avoid spam.
  - Escape hatch: “rekeep” regenerates and updates references.
  - Optional future: record runtime/version metadata alongside `ipfsMedia` so cache invalidation can be smarter than “30 days”.

---

## Credentials & Resources (from vault)

### Pinata IPFS
- Location: `aesthetic-computer-vault/.env.pinata`
- Contains: `PINATA_API_KEY`, `PINATA_API_SECRET`, `PINATA_JWT`
- Gateway: `https://ipfs.aesthetic.computer/`

### Tezos Wallets
- Location: `aesthetic-computer-vault/tezos/`
- **kidlisp wallet**: For contract deployment & admin (~968 XTZ on Ghostnet)
- **aesthetic.tez**: Personal wallet (~14.6 XTZ on Ghostnet)

---

## Phase 1: Contract Deployment ⏳

### 1.1 Deploy to Ghostnet (Testnet)
**File:** `tezos/deploy-to-ghostnet.py` (lines 1-225)
**Contract:** `tezos/michelson-lib/keeps-fa2-complete.tz`

```bash
cd /workspaces/aesthetic-computer/tezos
python3 deploy-to-ghostnet.py
```

**What it does:**
- Uses octez-client with Ghostnet RPC (`https://ghostnet.ecadinfra.com`)
- Imports kidlisp key from vault (line 15-25)
- Creates initial storage with kidlisp as admin (line 117)
- Originates contract with 10 tez burn cap (line 163-171)
- Saves contract address to `contract-address.txt` (line 197)

### 1.2 Test Contract Functions
**Files:** 
- `tezos/mint-to-ghostnet.py` (lines 1-397) - Full mint workflow
- `tezos/test-transfer.js` - Transfer testing
- `tezos/test_edit_and_lock.js` - Metadata editing

### 1.3 Contract Entrypoints (from keeps-fa2-complete.tz)
```
keep(ac_url, content_hash, content_type, metadata_uri, owner)
update_metadata(token_id, metadata)  
freeze_metadata(token_id)
transfer(from_, txs[{to_, token_id, amount}])
balance_of(requests, callback)
update_operators(add_operator | remove_operator)
```

---

## Phase 2: IPFS Upload Endpoint

### 2.1 Create Netlify Function
**Create:** `system/netlify/functions/upload-ipfs.js`

**Reference existing code:**
- `tezos/upload-to-ipfs.py` (lines 1-228) - Python version using Pinata
- `tezos/upload_to_ipfs.js` (lines 1-130) - Node.js version

**Implementation:**
```javascript
// system/netlify/functions/upload-ipfs.js
const crypto = require('crypto');

// Load from environment (set in Netlify)
const PINATA_API_KEY = process.env.PINATA_API_KEY;
const PINATA_API_SECRET = process.env.PINATA_API_SECRET;
const IPFS_GATEWAY = 'https://ipfs.aesthetic.computer/';

exports.handler = async (event) => {
  if (event.httpMethod !== 'POST') {
    return { statusCode: 405, body: 'Method not allowed' };
  }

  const { content, filename } = JSON.parse(event.body);
  const buffer = Buffer.from(content, 'base64');
  const hash = crypto.createHash('sha256').update(buffer).digest('hex');

  // Upload to Pinata (see tezos/upload_to_ipfs.js lines 70-95)
  const form = new FormData();
  form.append('file', buffer, { filename });
  
  const response = await fetch('https://api.pinata.cloud/pinning/pinFileToIPFS', {
    method: 'POST',
    headers: {
      'pinata_api_key': PINATA_API_KEY,
      'pinata_secret_api_key': PINATA_API_SECRET,
    },
    body: form,
  });

  const result = await response.json();
  return {
    statusCode: 200,
    body: JSON.stringify({
      cid: result.IpfsHash,
      ipfsUri: `ipfs://${result.IpfsHash}`,
      gatewayUrl: `${IPFS_GATEWAY}ipfs/${result.IpfsHash}`,
      contentHash: `sha256:${hash}`,
    }),
  };
};
```

### 2.2 Add to netlify.toml
**File:** `system/netlify.toml` (around line 109)
```toml
[functions.upload-ipfs]
included_env_vars = ["PINATA_API_KEY", "PINATA_API_SECRET"]
```

---

## Phase 3: Wallet Integration

### 3.1 Dependencies
**File:** `system/package.json` (line 44+)

Already has `@taquito/taquito` referenced in netlify.toml line 109!
Need to add:
```json
"@taquito/beacon-wallet": "^21.0.0",
"@airgap/beacon-sdk": "^4.0.0"
```

### 3.2 Create Wallet Module
**Create:** `system/public/aesthetic.computer/lib/tezos.mjs`

**Note:** Empty file exists at `lib/kidlisp-keep.mjs` - can use or replace.

```javascript
// system/public/aesthetic.computer/lib/tezos.mjs
import { TezosToolkit } from '@taquito/taquito';
import { BeaconWallet } from '@taquito/beacon-wallet';

const GHOSTNET_RPC = 'https://ghostnet.ecadinfra.com';
const MAINNET_RPC = 'https://mainnet.api.tez.ie';

// Contract addresses (set after deployment)
const CONTRACTS = {
  ghostnet: null, // Set after deploy: 'KT1...'
  mainnet: null,  // Set for production
};

let wallet = null;
let tezos = null;

export async function connectWallet(network = 'ghostnet') {
  const rpc = network === 'mainnet' ? MAINNET_RPC : GHOSTNET_RPC;
  tezos = new TezosToolkit(rpc);
  
  wallet = new BeaconWallet({
    name: 'Aesthetic Computer',
    preferredNetwork: network === 'mainnet' ? 'mainnet' : 'ghostnet',
  });

  await wallet.requestPermissions({
    network: { type: network === 'mainnet' ? 'mainnet' : 'ghostnet' },
  });

  tezos.setWalletProvider(wallet);
  const address = await wallet.getPKH();
  
  return { address, network };
}

export async function disconnectWallet() {
  if (wallet) {
    await wallet.clearActiveAccount();
    wallet = null;
    tezos = null;
  }
}

export async function getConnectedAddress() {
  if (!wallet) return null;
  try {
    const account = await wallet.client.getActiveAccount();
    return account?.address || null;
  } catch {
    return null;
  }
}

export async function mintKeep({ 
  artifactUri, 
  contentHash, 
  contentType, 
  metadataUri = '',
  owner,
  network = 'ghostnet' 
}) {
  if (!tezos || !wallet) {
    throw new Error('Wallet not connected');
  }

  const contractAddress = CONTRACTS[network];
  if (!contractAddress) {
    throw new Error(`No contract deployed on ${network}`);
  }

  const contract = await tezos.wallet.at(contractAddress);
  
  const op = await contract.methods.keep(
    artifactUri,
    contentHash,
    contentType,
    metadataUri,
    owner
  ).send();

  await op.confirmation();
  return {
    hash: op.opHash,
    network,
    explorer: `https://${network === 'mainnet' ? '' : 'ghostnet.'}tzkt.io/${op.opHash}`,
  };
}
```

---

## Phase 4: `keep` Command in Prompt

### 4.1 Location in prompt.mjs
**File:** `system/public/aesthetic.computer/disks/prompt.mjs`
**Insert after:** Line 1735 (after `html`/`bundle` command block)

```javascript
  } else if (text.startsWith("keep ")) {
    // Mint a KidLisp piece as an NFT on Tezos
    const pieceCode = params[0];
    if (!pieceCode) {
      notice("Usage: keep $code", ["red"]);
      flashColor = [255, 0, 0];
      makeFlash($);
      return true;
    }

    const code = pieceCode.startsWith("$") ? pieceCode.slice(1) : pieceCode;
    
    // Step 1: Bundle the piece
    notice(`Bundling $${code}...`, ["yellow"]);
    
    try {
      // Fetch bundle using existing endpoint
      const bundleResponse = await fetch(`/api/bundle-html?code=${code}&format=json`);
      const bundleData = await bundleResponse.json();
      
      if (bundleData.error) {
        throw new Error(bundleData.error);
      }

      // Step 2: Show preview modal (TODO: implement modal UI)
      // For now, just show size and proceed
      notice(`Bundle ready: ${bundleData.sizeKB}KB`, ["cyan"]);
      
      // Step 3: Connect wallet
      const { connectWallet, mintKeep } = await import('../lib/tezos.mjs');
      notice("Connecting wallet...", ["yellow"]);
      const { address, network } = await connectWallet('ghostnet');
      notice(`Connected: ${address.slice(0, 8)}...`, ["lime"]);

      // Step 4: Upload to IPFS
      notice("Uploading to IPFS...", ["yellow"]);
      const ipfsResponse = await fetch('/api/upload-ipfs', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          content: bundleData.content,
          filename: bundleData.filename,
        }),
      });
      const ipfsData = await ipfsResponse.json();
      notice(`IPFS: ${ipfsData.cid.slice(0, 12)}...`, ["cyan"]);

      // Step 5: Mint NFT
      notice("Minting NFT...", ["yellow"]);
      const result = await mintKeep({
        artifactUri: ipfsData.ipfsUri,
        contentHash: ipfsData.contentHash,
        contentType: 'kidlisp',
        owner: address,
        network,
      });

      notice(`Minted! ${result.explorer}`, ["lime"]);
      flashColor = [0, 255, 0];
      
    } catch (err) {
      console.error("Keep error:", err);
      notice(`Failed: ${err.message}`, ["red"]);
      flashColor = [255, 0, 0];
    }

    makeFlash($);
    return true;
  }
```

---

## Phase 5: kidlisp.com Integration

### 5.1 Add "Keep" Button
**File:** `kidlisp.com/index.html`
**Location:** After line 1865 (after send/stop buttons in editor-header)

**Add button HTML (around line 1867):**
```html
<button id="keep-button" aria-label="Keep as NFT" title="Mint as NFT on Tezos" style="display: none;">
  <span style="font-size: 16px;">🔒</span>
</button>
```

**Add CSS (around line 420):**
```css
#keep-button {
  position: absolute;
  bottom: 8px;
  right: 64px; /* Position to left of play button */
  z-index: 100;
  width: 40px;
  height: 40px;
  background: rgb(156, 39, 176); /* Purple for Tezos */
  color: white;
  border: none;
  border-radius: 50%;
  cursor: pointer;
  box-shadow: 0 3px 6px rgba(0,0,0,0.3);
  transition: all 0.2s;
  display: flex;
  align-items: center;
  justify-content: center;
}

#keep-button:hover {
  background: rgb(186, 104, 200);
  transform: scale(1.05);
}
```

**Add JavaScript (around line 4800):**
```javascript
// Keep button - mint as NFT
const keepButton = document.getElementById('keep-button');
if (keepButton) {
  keepButton.addEventListener('click', async () => {
    const currentCode = localStorage.getItem('kidlisp-current-code');
    if (!currentCode) {
      alert('Save your piece first to mint it as an NFT');
      return;
    }
    
    // Open aesthetic.computer with keep command
    window.open(`https://aesthetic.computer/prompt~keep $${currentCode}`, '_blank');
  });
  
  // Show button when piece is saved
  const savedCode = localStorage.getItem('kidlisp-current-code');
  if (savedCode) {
    keepButton.style.display = 'flex';
  }
}
```

---

## Phase 6: Post-Mint Features

### 6.1 View Keeps Command
**Add to prompt.mjs after keep command:**
```javascript
} else if (text === "keeps" || text === "my keeps") {
  // List user's minted keeps
  const { getConnectedAddress } = await import('../lib/tezos.mjs');
  const address = await getConnectedAddress();
  
  if (!address) {
    notice("Connect wallet first: keep $code", ["yellow"]);
  } else {
    // Query TzKT API for user's tokens
    const tzktUrl = `https://api.ghostnet.tzkt.io/v1/tokens/balances?account=${address}&token.contract=${CONTRACT_ADDRESS}`;
    // ... display results
  }
}
```

### 6.2 Metadata Editing
**Contract already supports:** `update_metadata(token_id, metadata_map)`
- Only admin can call
- Pass map of key→bytes updates

### 6.3 Metadata Locking  
**Contract already supports:** `freeze_metadata(token_id)`
- Adds `__frozen` flag
- Irreversible

---

## File Map with Line Numbers

### Existing Files to Modify

| File | Lines | Changes |
|------|-------|---------|
| `system/netlify.toml` | ~109 | Add upload-ipfs function config |
| `system/package.json` | 44+ | Add beacon-wallet dependency |
| `system/public/aesthetic.computer/disks/prompt.mjs` | 1735+ | Add `keep` command |
| `kidlisp.com/index.html` | 1867, 420, 4800 | Add Keep button |

### New Files to Create

| File | Purpose |
|------|---------|
| `system/netlify/functions/upload-ipfs.js` | IPFS upload endpoint |
| `system/public/aesthetic.computer/lib/tezos.mjs` | Wallet integration |
| `tezos/contract-address.txt` | Deployed contract address |

### Reference Files (existing)

| File | Lines | Contents |
|------|-------|----------|
| `tezos/deploy-to-ghostnet.py` | 1-225 | Contract deployment |
| `tezos/mint-to-ghostnet.py` | 1-397 | Full mint workflow |
| `tezos/upload-to-ipfs.py` | 1-228 | Python IPFS upload |
| `tezos/upload_to_ipfs.js` | 1-130 | Node.js IPFS upload |
| `tezos/michelson-lib/keeps-fa2-complete.tz` | 1-3 | Compiled contract |
| `system/netlify/functions/bundle-html.js` | 1-1305 | Bundle generation |
| `system/netlify/functions/store-kidlisp.mjs` | 219-280 | Tezos integration hooks |

---

## Environment Variables

### Already in Netlify (need to add to dashboard)
```
PINATA_API_KEY=1db6937aae50b35599ce
PINATA_API_SECRET=9eac56f3a3c0ea4f8551bd86032d9d45a2fa4ba5c9e533984b56032433d4cf0a
```

### Add After Deployment
```
KEEPS_CONTRACT_GHOSTNET=KT1...  # From deploy output
KEEPS_CONTRACT_MAINNET=KT1...   # Future
```

---

## Current Status

| Component | Status | File | Notes |
|-----------|--------|------|-------|
| FA2 Contract | ✅ Ready | `michelson-lib/keeps-fa2-complete.tz` | 47 tests |
| Bundle API | ✅ Live | `functions/bundle-html.js` | Line 1-1305 |
| Wallets | ✅ Funded | vault/tezos/ | 968 XTZ |
| Deploy Script | ✅ Ready | `deploy-to-ghostnet.py` | Line 1-225 |
| Contract Deployed | ⏳ Next | - | Run deploy |
| IPFS Endpoint | ❌ TODO | Create `upload-ipfs.js` | |
| Wallet Module | ❌ TODO | Create `lib/tezos.mjs` | |
| `keep` Command | ❌ TODO | `prompt.mjs` line 1735 | |
| kidlisp.com Button | ⏳ Partial | `index.html` | UI polish done, button logic pending |
| kidlisp.com Console | ✅ Done | `index.html` | Console, Auth0 fixed Dec 16 |

### December 16, 2025 - Session Status

**What happened**: Got ahead of ourselves working on Phase 5 (kidlisp.com UI) before completing Phases 1-4. 

**Completed Phase 5 work**:
- Console panel now shows KidLisp-only output
- Welcome message, play/pause/stop states, syntax highlighting
- Error line/column display with Monaco decorations
- Fixed Auth0 SDK loading (AMD/Monaco conflict)

**Next priority**: Test and harden keep flow on Ghostnet
```bash
# Test keep command in aesthetic.computer
# Navigate to https://aesthetic.computer/prompt
keep $code

# Or use dedicated keep piece
# Navigate to https://aesthetic.computer/keep

# Monitor SSE stages and IPFS uploads
# Verify wallet connection and minting
# Test IPFS media caching (run prepare twice)
```

**Testing checklist:**
- [ ] `keep $code` command works in prompt.mjs
- [ ] keep.mjs piece loads and displays properly
- [ ] Wallet connection successful (Temple/Kukai)
- [ ] SSE progress stages complete without hanging
- [ ] IPFS uploads successful (thumbnail + bundle)
- [ ] Media caching reuses existing IPFS on second prepare
- [ ] `regenerate: true` forces new IPFS uploads
- [ ] Minting succeeds and returns tzkt/objkt links
- [ ] 20+ successful mints across different pieces
- [ ] No duplicate IPFS spam from repeated prepares

**Recent work completed (Dec 18, 2025)**:
- KidLisp screenshot system with console display
- Auto-play for URL-loaded codes  
- Screenshot filenames: `$code-@handle-timestamp.png` format
- Fixed codeId priority (prefer passed over cached)
- Debug logging for code caching pipeline
- Wallet integration in kidlisp.com Keeps tab

---

## Execution Order

1. **Deploy Contract**
   ```bash
   cd /workspaces/aesthetic-computer/tezos
   python3 deploy-to-ghostnet.py
   # Save KT1... address
   ```

2. **Create IPFS Endpoint**
   - Create `system/netlify/functions/upload-ipfs.js`
   - Add config to `system/netlify.toml`
   - Add env vars to Netlify dashboard

3. **Create Wallet Module**
   - Create `system/public/aesthetic.computer/lib/tezos.mjs`
   - Add beacon-wallet to `system/package.json`
   - Update contract address in module

4. **Add `keep` Command**
   - Edit `system/public/aesthetic.computer/disks/prompt.mjs`
   - Insert after line 1735

5. **Add kidlisp.com Button**
   - Edit `kidlisp.com/index.html`
   - Add button HTML, CSS, and JS

6. **Test End-to-End**
   - `keep $39j` in prompt
   - Click Keep button on kidlisp.com
