# Fee System Recompile Status

## Current Situation

**Contract deployed:** `KT1NeytR5BHDfGBjG9ZuLkPd7nmufmH1icVc` (Ghostnet)  
**Status:** ⚠️ Missing `keep_fee` storage field and fee entrypoints

## Problem

The SmartPy contract source ([keeps_fa2_v2.py](./keeps_fa2_v2.py)) includes:
- `self.data.keep_fee = sp.tez(0)` storage field (line 69)
- `set_keep_fee(new_fee)` entrypoint (line 209)
- `withdraw_fees(destination)` entrypoint (line 220)

**But** the compiled Michelson in `KeepsFA2v2/` **does not include these** because:
1. The contract was compiled before `keep_fee` was added
2. SmartPy compilation requires x86_64 architecture (doesn't work on ARM64 devcontainer)

## Solution

Need to recompile on an x86_64 machine (Mac/Intel Linux/Windows):

```bash
cd tezos
python -m venv .venv
source .venv/bin/activate  # or .venv\Scripts\activate on Windows
pip install smartpy-tezos
python keeps_fa2_v2.py
```

This will regenerate:
- `KeepsFA2v2/step_002_cont_0_contract.tz` (with `keep_fee` entrypoints)
- `KeepsFA2v2/step_002_cont_0_storage.tz` (with `keep_fee` field)

Then redeploy:
```bash
node keeps.mjs deploy
```

## Current Functionality

**Working:**
- ✅ Minting (keep)
- ✅ Burning (burn_keep)
- ✅ Edit metadata
- ✅ Lock metadata
- ✅ Transfer (FA2)
- ✅ Balance queries

**Not Working (requires recompile):**
- ❌ `set_keep_fee` - set mint fee
- ❌ `withdraw_fees` - withdraw accumulated fees
- ❌ Fee validation in `keep` entrypoint

## Temporary Workaround

The current contract works perfectly for admin-only minting at 0 XTZ fee. The fee system is only needed when:
1. Opening minting to users (charge them per mint)
2. Want to collect fees from mints
3. Secondary market royalties (different system, TBD)

## References

- Contract source: [keeps_fa2_v2.py](./keeps_fa2_v2.py)
- Fee system docs: [KEEPS-FEE-SYSTEM.md](./KEEPS-FEE-SYSTEM.md)
- Deployment CLI: [keeps.mjs](./keeps.mjs)
- Admin panel: https://aesthetic.computer/keeps-multitool.html
