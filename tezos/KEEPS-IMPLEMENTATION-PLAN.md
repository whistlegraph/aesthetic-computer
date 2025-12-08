# Keeps Implementation Plan

## Overview
Enable users to mint self-contained KidLisp pieces as FA2 NFTs on Tezos.

**User Flow:**
```
keep $ceo → Preview bundle → Connect wallet → Upload to IPFS → Mint NFT
```

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
| kidlisp.com Button | ❌ TODO | `index.html` line 1867 | |

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
