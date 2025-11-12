# SmartPy v0.23.1 Compilation Status

## Problem
TzKT doesn't recognize our FA2 contract as FA2 because it's missing TZIP-16 contract-level metadata.

## What We Built
- ✅ Docker image with SmartPy v0.23.1: `smartpy-amd64:0.23.1`
- ✅ QEMU emulation working (container runs as x86_64)
- ✅ SmartPy v0.23.1 Python library loads successfully
- ❌ **BLOCKER**: SmartPy's `oasis` binary crashes with "Illegal instruction" under QEMU

## Why QEMU Failed
The SmartPy `oasis` compiler binary (`/usr/local/lib/python3.11/site-packages/smartpy/smartpy-oasis-linux.exe`) uses x86_64 CPU instructions that QEMU user-mode emulation cannot handle. This is a known limitation when emulating complex binaries.

## Contract Ready for Compilation

**File**: `/workspaces/aesthetic-computer/tezos/keeps_fa2_test.py`

This contract includes:
- ✅ All FA2 entrypoints (keep, transfer, balance_of, update_operators)
- ✅ TZIP-16 contract metadata bigmap
- ✅ TZIP-21 token metadata
- ✅ Proper SmartPy v0.23.1 syntax

## Next Steps (on x86_64 machine)

### Option 1: Local Compilation with Docker
```bash
cd /workspaces/aesthetic-computer/tezos

# Image is already built and tagged
docker images | grep smartpy-amd64

# Run compilation
docker run --rm -v $(pwd):/workspace smartpy-amd64:0.23.1 sh -c "
  cd /workspace
  python3 keeps_fa2_test.py
  # Compiled output will be in ./keeps_fa2_enhanced/ directory
"
```

### Option 2: SmartPy Online IDE (Fastest)
1. Go to https://smartpy.io/ide
2. Copy content of `/workspaces/aesthetic-computer/tezos/keeps_fa2_test.py`
3. Paste into editor
4. Click "Compile"
5. Download the `.tz` file from the output
6. Save as `/workspaces/aesthetic-computer/tezos/keeps_fa2_with_metadata.tz`

### Option 3: Rebuild Docker Image on x86_64
```bash
# On x86_64 machine
cd /workspaces/aesthetic-computer/tezos
docker build -t smartpy-amd64:0.23.1 -f Dockerfile.smartpy .

# Compile
docker run --rm -v $(pwd):/workspace smartpy-amd64:0.23.1 python3 /workspace/keeps_fa2_test.py
```

## After Compilation

1. **Save the compiled Michelson**:
```bash
# From SmartPy IDE download or Docker output
cp keeps_fa2_enhanced/step_000_cont_0_contract.tz keeps_fa2_with_metadata.tz
```

2. **Update deployment script**:
```javascript
// In deploy_clean.js, line ~17
const contractCode = fs.readFileSync(
  "/workspaces/aesthetic-computer/tezos/keeps_fa2_with_metadata.tz",
  "utf-8"
);
```

3. **Update storage initialization** (already done in deploy_clean.js):
```javascript
const metadataMap = new MichelsonMap();
metadataMap.set("", Buffer.from("tezos-storage:content").toString("hex"));
metadataMap.set("content", Buffer.from(JSON.stringify({
  name: "Aesthetic Computer Keeps",
  description: "FA2 NFT contract for aesthetic.computer",
  version: "1.0.0",
  interfaces: ["TZIP-012", "TZIP-016"]
})).toString("hex"));

const storage = {
  administrator: "tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC",
  ledger: new MichelsonMap(),
  metadata: metadataMap,  // ← This is the key addition
  next_token_id: 0,
  operators: new MichelsonMap(),
  token_metadata: new MichelsonMap()
};
```

4. **Deploy**:
```bash
cd /workspaces/aesthetic-computer/tezos
node deploy_clean.js
```

5. **Verify on TzKT**:
- Check https://ghostnet.tzkt.io/[NEW_CONTRACT_ADDRESS]
- Should show `kind: asset` and `TZIPs: ['fa2']`

## Current Deployments

- **Without metadata**: `KT1Xb3oNDyCuNdnf9ssBEJX7GeJqBb8wMv7r` (not recognized as FA2)
- **Old version**: `KT1LkPM2F9zz8zxTQrkG6zcUPjNZj3FgmYYr` (missing metadata)

## Files Ready

- ✅ `keeps_fa2_test.py` - Complete SmartPy v0.23.1 contract
- ✅ `Dockerfile.smartpy` - Docker build file for SmartPy
- ✅ `deploy_clean.js` - Updated with metadata initialization
- ✅ `mint.js` - Ready to mint tokens
- ⏳ `keeps_fa2_with_metadata.tz` - Needs compilation (missing)

## Docker Image Details

**Tag**: `smartpy-amd64:0.23.1`
**Base**: `python:3.11-slim` (linux/amd64)
**SmartPy Version**: 0.23.1
**Status**: Built and ready, but requires x86_64 to run compilation

To verify image exists:
```bash
docker images | grep smartpy-amd64
```

To export/import on another machine:
```bash
# Export
docker save smartpy-amd64:0.23.1 | gzip > smartpy-amd64-0.23.1.tar.gz

# Import on x86_64 machine
docker load < smartpy-amd64-0.23.1.tar.gz
```
