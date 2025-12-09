# Keeps Wallet - LLM Context

## Overview
Tezos wallet for aesthetic.computer "keeps" (NFTs). Two deployment modes:
1. **Integrated** - For logged-in prompt.ac users (server-stored encrypted keys)
2. **Extension** - Chrome extension for standalone/cross-dApp use

## Quick Build
```bash
cd wallet/extension
npm install
npm run build:icons
npm run build
# Load dist/ as unpacked extension in chrome://extensions
```

## Key Concepts

### Keeps
- NFTs minted on Tezos via aesthetic.computer
- Contract: See `tezos/keeps.mjs` for existing implementation
- Metadata stored on IPFS

### Wallet Architecture
```
Extension Mode:
  popup.js <--message--> background.js (has keys) <--taquito--> Tezos RPC

Integrated Mode:
  prompt.ac <--decrypt--> client-side keys <--taquito--> Tezos RPC
                ↑
        encrypted keys from server (user account)
```

### Tezos Key Derivation (lib/crypto.mjs)
```javascript
// From 24-word mnemonic to Tezos keypair
mnemonic → seed (bip39) → ed25519 keypair (libsodium) → tz1 address
// Encryption: Argon2id + XChaCha20-Poly1305
```

### Extension Files
```
wallet/extension/
├── manifest.json       # Chrome Manifest V3
├── background.js       # Service worker - key mgmt, signing, API calls
├── content.js          # Message relay between page and extension
├── inpage.js           # Creates window.keeps API for dApps
├── popup/
│   ├── popup.html      # Extension popup UI
│   └── popup.js        # Popup logic
├── lib/
│   └── crypto.mjs      # Key derivation, encryption, signing
├── scripts/
│   ├── build.mjs       # esbuild bundler
│   └── generate-icons.mjs
└── dist/               # Built extension (load this in Chrome)
```

### Important Related Files
- `tezos/keeps.mjs` - Existing keeps contract CLI
- `system/public/aesthetic.computer/disks/wallet.mjs` - Wallet UI piece  
- `system/public/aesthetic.computer/lib/wallet.mjs` - Beacon wallet state

### APIs Used
- **TzKT:** `https://api.tzkt.io` (mainnet), `https://api.ghostnet.tzkt.io` (testnet)
- **RPC:** `https://mainnet.api.tez.ie` or `https://ghostnet.teztnets.com`
- **IPFS:** `https://ipfs.io/ipfs/` for keeps metadata

### window.keeps API (injected by extension)
```javascript
// Check if extension installed
await keeps.isInstalled()  // true/false

// Get wallet state
await keeps.getState()  // { exists, unlocked, address, network }

// Connect (throws if locked/no wallet)
await keeps.connect()  // returns address

// Get data
await keeps.getBalance()  // { balance: number }
await keeps.getKeeps()    // { keeps: [...] }

// Sign operation
await keeps.sign({ forgedBytes: '...' })  // { signature, signedBytes }

// Listen for lock
keeps.onLocked(() => console.log('Wallet locked!'))
```

## Security Model
1. **Seed phrase encrypted** with Argon2id + XChaCha20-Poly1305
2. **Keys only in memory** while unlocked (15 min auto-lock)
3. **Page never sees keys** - signs via message passing
4. **User confirmation** required for signing (TODO: popup)

## Testing
- Use ghostnet for all development
- Faucet: https://faucet.ghostnet.teztnets.com/
- Run `./test-extension.sh` to launch Chromium with extension

## TODO
- [ ] Signing confirmation popup
- [ ] Integrated mode (server-side for logged-in users)
- [ ] Connect button in aesthetic.computer (detect window.keeps)
- [ ] Network switching UI in popup
- [ ] Transaction history in popup
