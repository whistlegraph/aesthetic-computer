# Deploy FA2 Contract via Better Call Dev

Since we're experiencing Docker-in-Docker volume mounting limitations in the devcontainer, the quickest path to deployment is using Better Call Dev's web interface.

## Quick Deploy (5 minutes)

### 1. Open Better Call Dev
https://better-call.dev/deploy

### 2. Select Network
- Choose **Ghostnet** from the network dropdown

### 3. Upload Contract
- Click "Choose file" or drag-and-drop
- Select: `/workspaces/aesthetic-computer/tezos/michelson-lib/keeps-fa2-complete.tz`

### 4. Set Initial Storage
Paste this exact storage expression:
```
(Pair (Pair "tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC" {}) (Pair 0 (Pair {} {})))
```

**Storage Structure:**
- `tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC` = Admin address (kidlisp wallet)
- `{}` = Empty ledger (no tokens minted yet)
- `0` = Next token ID
- `{}` = Empty metadata big_map
- `{}` = Empty operators big_map

### 5. Connect Wallet
You'll need a Tezos wallet browser extension with the kidlisp key:

**Option A: Temple Wallet**
1. Install Temple Wallet extension
2. Import account using secret key: `edsk3gMUwPx6WEWCpTU39bdMNUDGRQ8UkRL9bWxoj7fQwTnzQheEh4`
3. Switch to Ghostnet network
4. Connect to Better Call Dev

**Option B: Kukai Wallet**
1. Install Kukai extension
2. Import with kidlisp secret key
3. Switch to Ghostnet
4. Connect to Better Call Dev

### 6. Deploy
- Review burn cap (should be ~10 tez)
- Click "Deploy"
- Confirm transaction in wallet
- Wait ~30 seconds for confirmation

### 7. Save Contract Address
Once deployed, you'll get a contract address like `KT1...`

Save it:
```bash
cd /workspaces/aesthetic-computer/tezos
echo "KT1..." > .contract-address
```

### 8. Verify Deployment
- Better Call Dev: `https://better-call.dev/ghostnet/KT1...`
- TzKT Explorer: `https://ghostnet.tzkt.io/KT1...`

## After Deployment

### Mint Your First Token
```bash
cd /workspaces/aesthetic-computer/tezos
python3 mint-to-ghostnet.py "https://aesthetic.computer/\$ceo" --contract KT1...
```

### Test Contract
```bash
cd michelson-lib
pytest test_fa2.py -v
```

## Troubleshooting

### "Insufficient Balance"
Get testnet tez:
- https://faucet.ghostnet.teztnets.com/
- Request for address: `tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC`

### "Parse Error" in Storage
- Make sure you copied the exact storage expression above
- No extra spaces or line breaks
- Must be a single line

### "Wallet Not Connected"
- Make sure wallet extension is installed
- Switch wallet to Ghostnet network
- Refresh Better Call Dev page and reconnect

## Why Better Call Dev?

**Docker-in-Docker Issues:**
- Devcontainer paths (`/workspaces/...`) don't exist on host Docker daemon
- Volume mounts fail with "path is not shared from host"
- Even /tmp doesn't work because devcontainer /tmp ≠ host /tmp

**Protocol Compatibility:**
- Local octez-client v20.3 doesn't support current Ghostnet protocol
- Latest octez Docker images have parsing issues with our Michelson format
- Better Call Dev always supports current protocols

**Simplicity:**
- No complex Docker setup needed
- Visual interface for reviewing contract before deployment
- Immediate feedback and error messages
- Built-in wallet integration

## Alternative: Deploy from Host Machine

If you prefer command-line deployment:

1. Copy contract to host machine (outside devcontainer)
2. Install octez-client on host
3. Deploy from host where Docker volume mounts work correctly

But Better Call Dev is faster and more reliable for one-time deployment.
