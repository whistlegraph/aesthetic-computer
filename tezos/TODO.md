# KidLisp Token on Tezos - Development Plan

## 🎯 Project Overview
Create a Tezos FA2 fungible token system that automatically mints tokens for each unique KidLisp code snippet stored in the system. This will "economize" the KidLisp language by creating scarcity and value for creative code expressions.

## 📋 Current Status (Updated: Aug 10, 2025)
- [x] Research Tezos development tools and ecosystem (2025)
- [x] Analyze existing AC infrastructure (Auth0, MongoDB, KidLisp system)
- [x] Set up Tezos development environment
  - [x] SmartPy 0.22.0 installed and working
  - [x] TypeScript compilation fixed
  - [x] Taquito integration configured
- [x] Create working smart contract
  - [x] kidlisp_working.py compiles and runs successfully
  - [x] FA2-style token with hash-based minting
  - [x] Deduplication logic to prevent duplicate tokens
  - [x] Transfer functionality and onchain views
- [x] Basic integration layer setup
  - [x] integration.js with token existence checking
  - [x] checkTokenExists and mintKidLispToken functions
- [ ] Deploy to Ghostnet for testing
- [ ] Full integration with store-kidlisp.js
- [ ] Implement user wallet generation system
- [ ] Set up royalty system for creators

## 🏗️ Architecture

### Smart Contract Layer (Tezos)
- **Language**: SmartPy (Python-like syntax, most popular)
- **Standard**: FA2 (Tezos token standard)
- **Features**:
  - Fungible tokens for each unique KidLisp snippet
  - Automatic minting when new code is cached
  - Royalty system for creators
  - Metadata linking to original KidLisp code

### Integration Layer (JavaScript/Taquito)
- **Library**: Taquito (TypeScript/JavaScript library for Tezos)
- **Integration Point**: `store-kidlisp.js` netlify function
- **Features**:
  - Automatic token minting on successful KidLisp caching
  - User wallet address generation based on @handle
  - Transaction management and error handling

### User System
- **Wallet Generation**: Deterministic wallet addresses for user @handles
- **Attribution**: Link tokens to creator's @handle from existing user system
- **Royalties**: Percentage-based royalty system for subsequent interactions

## 🛠️ Development Tasks

### Phase 1: Environment Setup ✅ COMPLETED
- [x] Install Tezos development tools
  - [x] SmartPy CLI (0.22.0 via pip)
  - [x] Taquito library (configured in TypeScript)
  - [x] Development environment in Docker container
- [x] Configure basic deployment structure
- [x] Fix TypeScript compilation issues

### Phase 2: Smart Contract Development 🔄 IN PROGRESS
- [x] Design FA2 contract for KidLisp tokens
  - [x] Token metadata structure
  - [x] Hash-based minting with deduplication
  - [x] Basic transfer functionality
  - [x] Onchain views for metadata
- [x] Working contract (kidlisp_working.py) that compiles successfully
- [ ] Write comprehensive test suite
- [ ] Security audit and review
- [ ] Deploy to Ghostnet (testnet) ⬅️ NEXT STEP
### Phase 3: Integration Development 🔄 STARTED
- [x] Basic integration layer (integration.js)
  - [x] checkTokenExists function
  - [x] mintKidLispToken with existence verification
  - [x] integrateWithKidLispCache function
- [ ] Extend store-kidlisp.js with Tezos integration
  - [ ] Wallet address generation for users
  - [ ] Automatic token minting workflow
  - [ ] Error handling and retry logic
  - [ ] Gas fee management
- [ ] Create user-facing API endpoints
  - [ ] Check token balance for user
  - [ ] View tokens for specific KidLisp code
  - [ ] Transfer/trade interface (future)

### Phase 4: User Experience
- [ ] Add token information to KidLisp QR codes
- [ ] Display token ownership in user profiles
- [ ] Show token creation history
- [ ] Integrate with existing @handle system

### Phase 5: Economics & Governance
- [ ] Define minting economics (fees, limits, etc.)
- [ ] Implement royalty distribution
- [ ] Create governance mechanism for contract upgrades
- [ ] Monitor and optimize gas costs

## 📋 Technical Specifications

### FA2 Contract Features (IMPLEMENTED)
```python
# KidLispToken FA2 Contract (kidlisp_working.py)
# - Each unique KidLisp snippet hash = one token type
# - Fungible within each type (multiple copies possible)
# - Metadata points to original KidLisp code
# - Hash-based deduplication prevents duplicate tokens
# - Transfer functionality with proper FA2 interface
# - Onchain views for metadata access
```

### Current Implementation Status
- ✅ Working smart contract: `contracts/kidlisp_working.py`
- ✅ Integration layer: `src/integration.js`
- ✅ TypeScript types: `src/index.ts` (compilation fixed)
- ✅ Environment: SmartPy 0.22.0 in Docker container
- 🔄 Ready for Ghostnet deployment testing

### Token Economics
- **Supply**: Initially unlimited (can be changed via governance)
- **Minting**: Automatic on KidLisp code storage
- **Distribution**: Creator gets initial allocation
- **Royalties**: Configurable percentage to creator on transfers
- **Fees**: Minimal minting fee to prevent spam

### Integration Points
1. **store-kidlisp.js**: Add Tezos minting after successful cache
2. **User Authentication**: Use existing Auth0 + @handle system
3. **Database**: Store token IDs alongside KidLisp cache records
4. **Frontend**: Display token info in KidLisp pieces

## 🔧 Development Tools & Dependencies

### Core Tools
- **SmartPy**: Contract development and testing
- **Taquito**: JavaScript/TypeScript Tezos integration
- **Octez**: Tezos node and client tools
- **Flextesa**: Local blockchain for testing

### Libraries & Packages
```json
{
  "dependencies": {
    "@taquito/taquito": "^19.0.0",
    "@taquito/beacon-wallet": "^19.0.0",
    "@taquito/utils": "^19.0.0",
    "@taquito/michel-codec": "^19.0.0"
  },
  "devDependencies": {
    "@types/node": "^20.0.0",
    "typescript": "^5.0.0"
  }
}
```

### Infrastructure
- **Testnet**: Ghostnet for development and testing
- **Mainnet**: Tezos mainnet for production
- **IPFS**: For storing token metadata (optional)
- **Indexer**: TzKT API for reading blockchain data

## 🌐 Integration with Existing AC System

### User Wallet Generation
```javascript
// Generate deterministic Tezos address from user @handle
function generateTezosWallet(handle, userSub) {
  // Use handle + userSub as seed for deterministic wallet
  // Store private key securely (encrypted with user's session)
  // Return public address for token operations
}
```

### Minting Workflow
```javascript
// In store-kidlisp.js, after successful caching:
if (user?.sub) {
  const wallet = await getTezosWallet(user.sub);
  const tokenId = hashToTokenId(hash);
  const metadata = createKidLispMetadata(source, code, user);
  await mintKidLispToken(wallet, tokenId, metadata);
}
```

### Royalty System
- Creator gets percentage of any future token transfers
- Configurable royalty percentage (default 5-10%)
- Automatic distribution via smart contract
- Creator identification via @handle system

## 🚀 Deployment Strategy

### Testnet Deployment
1. Deploy to Ghostnet with test parameters
2. Integrate with development version of store-kidlisp
3. Test complete workflow with test users
4. Verify gas costs and performance

### Mainnet Deployment
1. Final security review and audit
2. Deploy contract to Tezos mainnet
3. Update production store-kidlisp.js
4. Monitor initial usage and gas costs
5. Announce to AC community

## 📊 Success Metrics

### Technical Metrics
- Contract deployment success
- Integration reliability (>99% uptime)
- Gas cost optimization
- Transaction speed and confirmation times

### User Metrics
- Number of KidLisp tokens minted
- User adoption of token features
- Creator engagement with royalty system
- Community feedback and usage patterns

### Economic Metrics
- Token distribution patterns
- Royalty payments to creators
- Network fees and cost optimization
- Secondary market activity (if applicable)

## 🔮 Future Enhancements

### Phase 6+: Advanced Features
- [ ] Secondary marketplace for KidLisp tokens
- [ ] Token-gated access to premium AC features
- [ ] DAO governance for KidLisp language evolution
- [ ] Cross-chain bridge to Ethereum (existing AC ETH integration)
- [ ] Integration with existing AC NFT system
- [ ] Token staking and yield mechanisms
- [ ] Collaborative KidLisp development with shared royalties

### Community Features
- [ ] Token leaderboards and creator rankings
- [ ] Community voting on favorite KidLisp pieces
- [ ] Token-based rewards for code contributions
- [ ] Educational content about Tezos and FA2 tokens

---

## 📚 Resources

### Tezos Development
- [Tezos Developer Docs](https://docs.tezos.com/)
- [SmartPy Documentation](https://smartpy.io/)
- [Taquito Documentation](https://taquito.io/)
- [FA2 Token Standard](https://tzip.tezosagora.org/proposal/tzip-12/)

### Tools & Libraries
- [SmartPy IDE](https://smartpy.io/ide)
- [Taquito GitHub](https://github.com/ecadlabs/taquito)
- [TzKT API](https://api.tzkt.io/)
- [Flextesa](https://tezos.gitlab.io/flextesa/)

### Community
- [Tezos Discord](https://discord.gg/tezos)
- [Tezos Stack Exchange](https://tezos.stackexchange.com/)
- [TezosAgora Forum](https://forum.tezosagora.org/)

---

*This roadmap will be updated as development progresses and new requirements emerge.*
