# KidLisp - Tezos Integration

This directory contains the Tezos blockchain integration for aesthetic.computer's KidLisp language, implementing an FA2 fungible token system where each unique piece of KidLisp code can be minted as a token.

## 🎯 Overview

The KidLisp Meme Coin system automatically creates Tezos FA2 tokens for KidLisp code snippets, allowing creators to "economize" their code creations. Each unique piece of code becomes a fungible token with built-in creator royalties.

## 🏗️ Architecture

### Smart Contract (`contracts/kidlisp_meme_coin.py`)
- **Language**: SmartPy (Python-like syntax)
- **Standard**: FA2 (Tezos token standard)
- **Features**: Automatic minting, creator royalties, metadata linking

### Integration Library (`src/`)
- **Language**: TypeScript/JavaScript
- **Library**: Taquito for Tezos interaction
- **Purpose**: Bridge between AC's Node.js backend and Tezos

### Integration Point
- **File**: Modified `../system/netlify/functions/store-kidlisp.js`
- **Trigger**: Automatic minting when KidLisp code is cached
- **User System**: Integrates with existing @handle authentication

## 🚀 Quick Start

### 1. Install Dependencies

```bash
# Install development tools
./scripts/install.sh

# Or manually:
npm install
```

### 2. Configure Environment

```bash
# Copy and edit environment variables
cp .env.example .env
# Edit .env with your Tezos configuration
```

### 3. Compile Smart Contract

```bash
# Compile the FA2 contract
npm run compile:contract
```

### 4. Test Contract

```bash
# Run contract tests
npm run test:contract
```

### 5. Deploy to Testnet

```bash
# Deploy to Ghostnet (testnet)
npm run deploy:ghostnet
```

### 6. Integration Test

Test the integration with a sample KidLisp snippet through the store-kidlisp endpoint.

## 📋 Development Workflow

### Smart Contract Development

1. **Edit Contract**: Modify `contracts/kidlisp_meme_coin.py`
2. **Test Locally**: `npm run test:contract`
3. **Compile**: `npm run compile:contract`
4. **Deploy to Testnet**: `npm run deploy:ghostnet`
5. **Verify**: Check on Ghostnet block explorer

### Integration Development

1. **Edit Integration**: Modify `src/integration.js`
2. **Build**: `npm run build`
3. **Test**: Create test cases in `tests/`
4. **Update AC**: Modify `store-kidlisp.js` if needed

## 🪙 Token Economics

### Minting Criteria
- Minimum code length: 10 characters
- Must contain at least one function call
- Cannot be mostly comments

### Token Supply
- **Initial**: 1,000,000 tokens (1.0 with 6 decimals) per unique code
- **Additional**: More tokens can be minted for same code
- **Fee**: 0.1 tez minting fee to prevent spam

### Creator Benefits
- **Initial Allocation**: Creator receives all initial tokens
- **Royalties**: 5% royalty on all token transfers (configurable)
- **Attribution**: Permanent link to creator's @handle

## 🔧 Technical Details

### Wallet Generation
User Tezos wallets are generated deterministically from their @handle and authentication sub ID:

```javascript
// Simplified example
const seed = sha256(`tezos:${handle}:${userSub}:aesthetic.computer`);
const wallet = deriveFromSeed(seed);
```

### Token Identification
Each KidLisp code gets a unique token through SHA-256 hashing:

```javascript
const codeHash = sha256(kidlispCode.trim());
const tokenId = getOrCreateTokenId(codeHash);
```

### Metadata Structure
Tokens include rich metadata linking back to aesthetic.computer:

```json
{
  "name": "KidLisp Code #a1b2c3d4",
  "description": "A unique piece of KidLisp code by @creator",
  "image": "https://aesthetic.computer/kidlisp/preview/a1b2c3d4...",
  "animation_url": "https://aesthetic.computer/kidlisp/run/a1b2c3d4...",
  "external_url": "https://aesthetic.computer/$abc123",
  "attributes": [
    {"trait_type": "Language", "value": "KidLisp"},
    {"trait_type": "Creator", "value": "@creator"},
    {"trait_type": "Code Hash", "value": "a1b2c3d4..."}
  ]
}
```

## 🌐 Network Configuration

### Ghostnet (Testnet)
- **RPC**: `https://ghostnet.ecadinfra.com`
- **Explorer**: `https://ghostnet.tzkt.io`
- **Purpose**: Development and testing

### Mainnet (Production)
- **RPC**: `https://mainnet.api.tez.ie`
- **Explorer**: `https://tzkt.io`
- **Purpose**: Live deployment

## 📊 Monitoring & Analytics

### Contract Events
- Token minting events
- Transfer events with royalty distribution
- Creator attribution

### Integration Metrics
- Minting success/failure rates
- User adoption statistics
- Gas cost optimization

### User Experience
- Token balance display in AC interface
- Creator royalty tracking
- Secondary market integration potential

## 🔍 Testing

### Contract Tests
```bash
npm run test:contract  # SmartPy scenario tests
```

### Integration Tests
```bash
npm test  # Node.js integration tests
```

### Manual Testing
1. Deploy contract to Ghostnet
2. Test KidLisp code submission through AC
3. Verify token creation on block explorer
4. Test transfer and royalty functionality

## 🚢 Deployment

### Environment Variables
```bash
# Required for deployment
TEZOS_RPC_URL_GHOSTNET=https://ghostnet.ecadinfra.com
TEZOS_RPC_URL_MAINNET=https://mainnet.api.tez.ie
CONTRACT_ADMIN_SECRET_KEY=your_secret_key
KIDLISP_COIN_NAME=KidLisp
KIDLISP_COIN_SYMBOL=KLSP
```

### Deployment Commands
```bash
# Deploy to testnet
npm run deploy:ghostnet

# Deploy to mainnet (production)
npm run deploy:mainnet
```

### Post-Deployment
1. Update `.env` with contract address
2. Verify contract on block explorer
3. Test end-to-end functionality
4. Update AC production environment

## 🤝 Integration with Aesthetic Computer

### Store-KidLisp Integration
The integration automatically triggers when KidLisp code is cached:

```javascript
// In store-kidlisp.js
if (user?.sub) {
  const { integrateWithKidLispCache } = await import('../../../tezos/src/integration.js');
  const tezosResult = await integrateWithKidLispCache(source, user, code);
  
  if (tezosResult.minted) {
    // Token successfully minted
    // Update response with token info
  }
}
```

### User Experience
- Transparent minting (happens in background)
- Token info included in API responses
- QR codes can include token information
- Future: Token balance display in user profiles

## 🔮 Future Enhancements

### Phase 2: Marketplace
- Secondary token trading
- Token-gated access to AC features
- Creator earnings dashboard

### Phase 3: Governance
- Community voting on KidLisp language features
- Token-weighted governance proposals
- Creator royalty adjustments

### Phase 4: Cross-Chain
- Bridge to Ethereum (AC already has ETH integration)
- Multi-chain token support
- Unified creator economics

## 📚 Resources

### Tezos Development
- [Tezos Docs](https://docs.tezos.com/)
- [SmartPy](https://smartpy.io/)
- [Taquito](https://taquito.io/)
- [FA2 Standard](https://tzip.tezosagora.org/proposal/tzip-12/)

### Tools & Libraries
- [SmartPy IDE](https://smartpy.io/ide)
- [TzKT API](https://api.tzkt.io/)
- [Better Call Dev](https://better-call.dev/)

### Community
- [Tezos Discord](https://discord.gg/tezos)
- [Tezos Stack Exchange](https://tezos.stackexchange.com/)

## 📄 License

MIT License - See the main aesthetic.computer repository for details.

---

For more details, see `TODO.md` for the complete development roadmap.
