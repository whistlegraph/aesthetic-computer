# Keeps Wallet

A Tezos wallet for managing your aesthetic.computer keeps (NFTs).

## Quick Start

```bash
cd wallet/extension
npm install
npm run build:icons  # Generate icons
npm run build        # Build for Chrome
```

Then load in Chrome:
1. Open `chrome://extensions`
2. Enable "Developer mode" (top right toggle)
3. Click "Load unpacked"
4. Select `wallet/extension/dist`

## Two Modes

### 1. Integrated (Logged-in Users)
Users logged into prompt.ac get wallet functionality built-in:
- Encrypted keys stored server-side tied to their account
- No extension needed
- Seamless keeps minting, buying, selling
- Keys derived from account + password

### 2. Chrome Extension (Standalone)
For power users and cross-dApp usage:
- Self-custody keys (you control the seed phrase)
- Works on any Tezos dApp
- Portfolio view of all your keeps
- Clean aesthetic.computer UI

## Directory Structure

```
wallet/
├── README.md              # This file
├── llm.md                 # AI assistant context
├── extension/             # Chrome extension
│   ├── manifest.json
│   ├── background.js      # Service worker (key ops)
│   ├── content.js         # Page injection
│   ├── popup/             # Extension popup UI
│   └── lib/               # Shared crypto/tezos libs
└── integrated/            # Server-side wallet for logged-in users
    └── (future)
```

## Tech Stack

- **Tezos SDK:** @taquito/taquito
- **Crypto:** libsodium (ed25519 keys), bip39 (seed phrases)
- **Storage:** chrome.storage.local (extension), server-side encrypted (integrated)
- **NFT API:** TzKT for keeps metadata & history

## Security Model

### Extension
- Seed phrase encrypted with user password
- Keys never leave extension context
- Page requests signing via message passing
- User confirms each transaction in popup

### Integrated (logged-in)
- Keys encrypted with password-derived key
- Stored in user's account data
- Server never sees plaintext keys
- Decryption happens client-side only

## Keeps Features

- View your keeps collection
- Mint new keeps
- Transfer keeps
- List keeps for sale (objkt.com integration)
- View keep history & provenance
