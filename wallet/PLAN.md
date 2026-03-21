# Keeps Wallet — Beacon Protocol Implementation Plan

**Goal:** Make Keeps Wallet a drop-in replacement for Temple Wallet by implementing the Beacon SDK postMessage protocol. Any dApp using Beacon (keep.kidlisp.com, objkt.com, etc.) will see Keeps Wallet as a connectable wallet — no custom integration needed.

---

## 1. How Beacon Extension Communication Works

The Beacon SDK (used by dApps) discovers and communicates with browser extension wallets via `window.postMessage`. The protocol has three phases:

### Discovery (Ping/Pong)
```
dApp → window.postMessage({ target: "toExtension", payload: "ping" })
extension → window.postMessage({ target: "toPage", payload: "pong", sender: { id, name, iconURL } })
```
The dApp's `DappPostMessageTransport` sends a `ping` and collects `pong` responses to build a list of available wallets.

### Pairing (Key Exchange)
```
dApp → PostMessagePairingRequest { id, name, publicKey, version, icon?, appUrl? }
extension → PostMessagePairingResponse { id, name, publicKey, version, type: "postmessage-pairing-response", extensionId }
```
Both sides exchange Ed25519 public keys. All subsequent messages are encrypted using `crypto_box` (libsodium shared secret derived from both keypairs).

### Encrypted Message Exchange
```
dApp → { target: "toExtension", encryptedPayload: hex(nonce + ciphertext), targetId: extensionId }
extension → { target: "toPage", encryptedPayload: hex(nonce + ciphertext) }
```
Payloads are Beacon protocol messages (JSON) encrypted with the shared secret.

### Message Types (ExtensionMessageTarget)
```typescript
enum ExtensionMessageTarget {
  BACKGROUND = 'toBackground',
  PAGE = 'toPage',
  EXTENSION = 'toExtension'
}

interface ExtensionMessage<T> {
  target: ExtensionMessageTarget
  targetId?: string     // extension ID for routing
  sender?: { id, name, iconURL }
  payload: T            // 'ping' | 'pong' | encrypted payload string
}
```

### Beacon Message Types (inside encrypted payloads)
```
PermissionRequest → PermissionResponse
OperationRequest → OperationResponse
SignPayloadRequest → SignPayloadResponse
BroadcastRequest → (optional, can decline)
Disconnect
Acknowledge (sent immediately on receipt of any request)
Error (sent when request fails)
```

---

## 2. Implementation Steps

### Step 1: Add libsodium for Beacon Crypto

**File:** `wallet/extension/lib/beacon-crypto.mjs`

Beacon uses libsodium `crypto_box` for encrypted communication between dApp and wallet. We need:
- Ed25519 keypair generation (for wallet's Beacon identity — separate from Tezos keys)
- `crypto_box` / `crypto_box_open` for encrypting/decrypting messages
- Key exchange: convert Ed25519 keys to Curve25519 for `crypto_box`
- Nonce generation (24 bytes, prepended to ciphertext)

We already have libsodium in `lib/crypto.mjs` — extend or import from there.

**Persistent Beacon keypair:** Store a separate Ed25519 keypair seed in `chrome.storage.local` under `beacon_keypair_seed`. This is the wallet's Beacon identity (not the Tezos signing key). Generate on first use.

### Step 2: Beacon Protocol Handler in Background

**File:** `wallet/extension/beacon.mjs` (new)

This module handles the Beacon protocol logic:

```javascript
// Core functions needed:
getOrCreateBeaconKeypair()     // Persistent Beacon identity
encryptMessage(msg, peerPK)    // crypto_box with shared secret
decryptMessage(payload, peerPK) // crypto_box_open
handleBeaconRequest(msg)       // Route to permission/operation/sign handlers
buildBeaconResponse(request, data) // Construct proper response with id, version, senderId
```

**Request handlers:**

| Beacon Request | What Keeps Wallet Does |
|---|---|
| `PermissionRequest` | Return account address + public key. Scope: `sign`, `operation_request`. Opens confirmation popup. |
| `OperationRequest` | Forge operations via Tezos RPC, sign with wallet key, inject. Return `opHash`. Opens confirmation popup. |
| `SignPayloadRequest` | Sign raw bytes with ed25519 key. Return `edsig...` signature. Opens confirmation popup. |
| `BroadcastRequest` | Inject pre-signed operation to RPC node. Return `opHash`. |
| `Disconnect` | Clear active dApp session. |

**For every request:** immediately send an `Acknowledge` response (unencrypted `{ type: "acknowledge", id: request.id }`), then process and send the real response.

### Step 3: Rewrite Content Script for Beacon

**File:** `wallet/extension/content.js`

The content script becomes the message relay between the page and background:

```javascript
// 1. Listen for messages FROM the page (dApp → extension)
window.addEventListener('message', (event) => {
  if (event.source !== window) return;
  const msg = event.data;

  // Beacon ping
  if (msg?.target === 'toExtension' && msg?.payload === 'ping') {
    // Respond with pong + wallet info
    window.postMessage({
      target: 'toPage',
      payload: 'pong',
      sender: { id: EXTENSION_ID, name: 'Keeps Wallet', iconURL: ICON_URL }
    }, '*');
    return;
  }

  // Beacon encrypted message or pairing request
  if (msg?.target === 'toExtension') {
    chrome.runtime.sendMessage({ type: 'BEACON_MESSAGE', data: msg })
      .then(response => {
        if (response) {
          window.postMessage({ target: 'toPage', ...response }, '*');
        }
      });
    return;
  }

  // Keep existing KEEPS_* custom protocol as fallback
  if (msg?.type?.startsWith('KEEPS_')) { /* existing relay logic */ }
});

// 2. Listen for messages FROM background (extension → page)
chrome.runtime.onMessage.addListener((message) => {
  if (message.type === 'BEACON_RESPONSE') {
    window.postMessage({ target: 'toPage', ...message.data }, '*');
  }
  if (message.type === 'KEEPS_LOCKED') {
    window.postMessage({ type: 'KEEPS_LOCKED' }, '*');
  }
});
```

### Step 4: Update Inpage Script

**File:** `wallet/extension/inpage.js`

Keep the existing `window.keeps` API but also announce via Beacon's expected mechanism. The inpage script doesn't need to do much for Beacon — the content script handles the postMessage relay directly. But we should:

1. Dispatch a custom event announcing wallet availability:
```javascript
window.dispatchEvent(new CustomEvent('keeps:ready'));
```

2. Optionally set `window.tezos` or a similar global that some dApps check (Temple sets this too).

### Step 5: UI — Only Keeps, Alive

**Design philosophy:** The wallet is not a finance app. It's a living collection. You open it and you see your keeps running. Everything else is invisible until needed.

**Style guide:** Match `keep.kidlisp.com` — same CSS variables, fonts, and feel.

```css
/* Fonts */
--font-mono: 'Noto Sans Mono', 'SF Mono', 'Monaco', 'Menlo', 'Consolas', monospace;
--font-display: 'YWFTProcessing-Regular', monospace;  /* headings / logo */
--font-fun: 'Comic Relief', 'Comic Sans MS', cursive;

/* Light */
--bg-primary: #f7f7f7;
--bg-secondary: #e8e8e8;
--bg-tertiary: white;
--text-primary: #111;
--text-secondary: #333;
--text-tertiary: #666;
--border-color: #ddd;
--ac-purple: rgb(205, 92, 155);

/* Dark */
--bg-primary: #1e1e1e;
--bg-secondary: #252526;
--bg-tertiary: #2d2d30;
--text-primary: #d4d4d4;
--text-tertiary: #858585;
--border-color: #3e3e42;

/* Links in ac-purple, cursor from aesthetic.computer */
```

Auto theme (follows system `prefers-color-scheme`), with manual override via `data-theme`. Font files loaded from `https://aesthetic.computer/type/webfonts/`.

**One view, three states:**

**No wallet yet:**
A single demo keep runs full-bleed as a live `<canvas>`. Two words overlaid at the bottom: "Create" / "Import". Password fields appear inline when tapped — no separate view.

Import accepts seed phrases from **Temple** or **Kukai** (same BIP39 → ed25519 derivation). Your `aesthetic.tez` identity comes with you — same tz1 address, same keeps collection. The wallet shows your keeps immediately after import.

**Locked:**
Your most recent keep runs blurred behind a single password field centered on screen. Type, hit enter. No title, no logo, no chrome.

**Unlocked (the only real view):**
Your keeps in a grid. Each thumbnail is a live `<canvas>` running the KidLisp source code (~30KB evaluator). That's the whole UI. No transaction history, no settings pages.

- Balance hidden behind a small `$` icon in the corner — tap to reveal, tap again to hide
- Tap a keep → it expands to fill the panel, interactive (touch/click works)
- Tap edge or swipe → back to grid
- Connected dApps are invisible dots along the bottom edge (tap to disconnect)
- Network badge (ghostnet/mainnet) is a tiny pill in the corner, tap to toggle

**Confirmation overlay:**
When a Beacon request arrives, a translucent sheet slides up over whatever keep is currently displayed:
- dApp name
- What it wants (connect / sign / send)
- Approve / Reject
- No navigation away from the collection — it's an overlay

**File:** `wallet/extension/popup/popup.html` (rewrite)
**File:** `wallet/extension/popup/popup.js` (rewrite)

The popup loads the KidLisp evaluator (`kidlisp.mjs`) and creates a `<canvas>` per keep. Keeps metadata (including source `$code`) is fetched from TzKT. The evaluator runs each piece at thumbnail resolution (~100x100) for the grid, full resolution when expanded.

### Step 6: Update Manifest

**File:** `wallet/extension/manifest.json`

```json
{
  "content_scripts": [{
    "matches": [
      "https://aesthetic.computer/*",
      "https://*.aesthetic.computer/*",
      "https://keep.kidlisp.com/*",
      "https://*.kidlisp.com/*",
      "https://objkt.com/*",
      "http://localhost:8888/*",
      "https://localhost:8888/*",
      "<all_urls>"
    ]
  }]
}
```

For a true Temple replacement, we need `<all_urls>` so the wallet works on any Tezos dApp. Add `"permissions": ["storage", "activeTab"]` (already present).

### Step 7: dApp Session Management

**File:** `wallet/extension/beacon.mjs` (extends Step 2)

Track connected dApps:
```javascript
// Store in chrome.storage.local:
{
  beacon_peers: {
    [dAppId]: {
      name: "keep.kidlisp.com",
      publicKey: "...",
      connectedAt: timestamp,
      permissions: ["sign", "operation_request"]
    }
  }
}
```

On `Disconnect` message: remove peer. On lock: notify all connected dApps.
Connected dApps appear as small dots at the bottom edge of the keeps grid — tap to see name + disconnect. No separate view.

---

## 3. File Changes Summary

| File | Action | Description |
|---|---|---|
| `wallet/extension/lib/beacon-crypto.mjs` | **New** | Beacon-specific crypto (crypto_box, key exchange) |
| `wallet/extension/beacon.mjs` | **New** | Beacon protocol handler (request routing, responses) |
| `wallet/extension/content.js` | **Modify** | Add Beacon toExtension/toPage relay alongside existing KEEPS_ relay |
| `wallet/extension/inpage.js` | **Modify** | Keep window.keeps, add wallet announcement |
| `wallet/extension/background.js` | **Modify** | Add BEACON_MESSAGE handler, pending request queue, confirmation flow |
| `wallet/extension/popup/popup.html` | **Rewrite** | Living keeps grid — no finance UI, just running canvases |
| `wallet/extension/popup/popup.js` | **Rewrite** | KidLisp evaluator integration, keeps grid, confirmation overlay |
| `wallet/extension/manifest.json` | **Modify** | Add `<all_urls>` to content scripts, add kidlisp.com |
| `wallet/extension/package.json` | **Modify** | Add libsodium-wrappers if not already present |

---

## 4. Dependencies

Already have in `wallet/extension/package.json`:
- `tweetnacl` (ed25519 signing)
- `bip39` (mnemonic)
- `bs58check` (base58)

Need to add or verify:
- `libsodium-wrappers` — for `crypto_box` (Beacon uses this, not tweetnacl's box). Or use tweetnacl's `nacl.box` which is compatible (same algorithm: x25519-xsalsa20-poly1305).

**Decision:** Use `tweetnacl.box` since it's already a dependency. Same underlying crypto as libsodium's `crypto_box`. This avoids adding another dependency.

---

## 5. Testing Plan

1. **Unit test Beacon crypto** — encrypt/decrypt roundtrip with known test vectors
2. **Ping/pong discovery** — Load extension, open a page with Beacon SDK, verify wallet appears in wallet list
3. **Permission request** — Connect from keep.kidlisp.com, verify address returned matches wallet
4. **Operation request** — Mint a keep on ghostnet, verify confirmation popup appears, transaction succeeds
5. **Sign payload** — Sign arbitrary bytes, verify signature is valid ed25519
6. **Disconnect** — Disconnect from dApp, verify session cleared
7. **Auto-lock** — Verify Beacon requests fail gracefully when wallet is locked
8. **Multi-dApp** — Connect to two dApps simultaneously, verify independent sessions

---

## 6. Implementation Order

1. **Beacon crypto layer** (Step 1) — foundation
2. **Content script Beacon relay** (Step 3) — ping/pong first
3. **Background Beacon handler** (Step 2) — permission request flow
4. **Confirmation popup** (Step 5) — security gate
5. **Manifest update** (Step 6) — broader site support
6. **Operation & sign handlers** (Step 2 continued) — full transaction support
7. **Session management** (Step 7) — track connected dApps
8. **Popup UI updates** (Step 8) — connected dApps view
9. **Testing on keep.kidlisp.com** — end-to-end validation

---

## 7. Key References

- **Beacon SDK source:** `github.com/airgap-it/beacon-sdk` (branch: master)
  - `packages/beacon-transport-postmessage/src/` — PostMessage transport
  - `packages/beacon-types/src/types/ExtensionMessage.ts` — Message format
  - `packages/beacon-types/src/types/ExtensionMessageTarget.ts` — `toPage`/`toExtension` enum
  - `packages/beacon-types/src/types/beacon/BeaconMessageType.ts` — All 19 message types
  - `packages/beacon-types/src/types/PostMessagePairingRequest.ts` — Pairing request format
  - `packages/beacon-types/src/types/PostMessagePairingResponse.ts` — Pairing response format
- **Temple Wallet source:** `github.com/madfish-solutions/templewallet-extension` (branch: development)
  - `src/content-scripts/main.ts` — Content script with Beacon relay
  - `src/lib/temple/beacon.ts` — Beacon protocol handler (encrypt/decrypt, message routing)
- **Spire (reference wallet):** `github.com/airgap-it/spire` — AirGap's own Beacon extension wallet
- **TZIP-10 standard:** Wallet interaction standard that Beacon implements
- **Beacon docs:** `docs.walletbeacon.io`
