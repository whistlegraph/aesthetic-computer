# 🔮 Keeps Dashboard - Artery TUI Plan

## Overview
A dedicated Tezos development workbench in artery-tui for the Keeps FA2 NFT system.

## Features

### 1. Dashboard Header
- Contract address: `KT1NeytR5BHDfGBjG9ZuLkPd7nmufmH1icVc` (Ghostnet)
- Network indicator (Ghostnet/Mainnet)
- Current admin wallet (aesthetic/kidlisp)
- Links to tzkt explorer and objkt

### 2. Live Contract Status
- Token count (next_token_id from storage)
- Recent tokens with lock status 🔒
- Keep fee display
- Admin wallet balance

### 3. Engineering Tasks Panel
Parsed from `KEEPS-IMPLEMENTATION-PLAN.md`:
- Current phase (A/B/C)
- Current focus items
- Next steps checklist
- Success criteria progress

### 4. Quick Actions Menu
| Key | Action | Description |
|-----|--------|-------------|
| s | Status | Refresh contract status |
| b | Balance | Check wallet balance |
| t | Tokens | List recent tokens |
| m | Mint | Mint a test piece |
| u | Upload | Upload bundle to IPFS |
| l | Lock | Lock token metadata |
| f | Fishy | Jump to fishy terminal |
| r | Run Tests | Run keeps test suite |
| e | Explorer | Open tzkt in browser |
| o | Objkt | Open objkt collection |

### 5. Test Runner Integration
- Run end-to-end mint tests
- Test IPFS caching behavior
- Test SSE endpoint stages
- Track test mint count toward 20+ goal

### 6. Navigation
- `f` - Jump to 🐟-fishy buffer (for manual commands)
- `ESC` - Back to main menu
- Live status updates

## Implementation

### File Changes
1. `artery/artery-tui.mjs`:
   - Enhance `enterKeepsMode()` with dashboard state
   - Add `loadKeepsImplementationPlan()` to parse KEEPS-IMPLEMENTATION-PLAN.md
   - Add `fetchContractStatus()` for live token/fee info
   - Add `renderKeepsDashboard()` with all panels
   - Add fishy jump handler

### Dashboard Layout
```
╔══════════════════════════════════════════════════════════════════════╗
║  🔮 KEEPS - Tezos FA2 Development Workbench                          ║
╠══════════════════════════════════════════════════════════════════════╣
║ CONTRACT                          │ ENGINEERING TASK                  ║
║ ─────────────────────────────────┼──────────────────────────────────║
║ Network:  Ghostnet               │ Phase A: Ghostnet Hardening       ║
║ Address:  KT1Neyt...icVc         │ ──────────────────────────────── ║
║ Admin:    aesthetic              │ Current Focus:                    ║
║ Tokens:   9 minted               │ □ Test keep $code in prompt.mjs  ║
║ Fee:      0 XTZ (free)           │ □ Verify keep.mjs piece flow     ║
║ Balance:  ~14.6 XTZ              │ □ Validate SSE endpoint stages   ║
║                                  │ □ Confirm IPFS caching           ║
║ Recent Tokens:                   │ □ Achieve 20+ test mints (9/20)  ║
║   [8] $xyz 🔒                    │                                  ║
║   [7] $abc                       │ Next: Mainnet staging            ║
║   [6] $wand                      │                                  ║
╠══════════════════════════════════════════════════════════════════════╣
║ ACTIONS                                                              ║
║ [s] Status  [b] Balance  [t] Tokens  [m] Mint  [u] Upload  [l] Lock  ║
║ [f] Fishy   [r] Run Tests  [e] Explorer  [o] Objkt  [ESC] Back       ║
╠══════════════════════════════════════════════════════════════════════╣
║ OUTPUT                                                               ║
║ Ready. Select an action or press 'r' to run tests.                   ║
╚══════════════════════════════════════════════════════════════════════╝
```

## Implementation Steps
1. ✅ Create this plan
2. ✅ Parse implementation plan markdown for task status
3. ✅ Add TzKT API calls for live contract data
4. ✅ Build dashboard layout with panels
5. ✅ Add fishy jump functionality
6. ✅ Add test runner integration
7. 🔄 Test end-to-end flow

## Usage
Press `z` from the main Artery TUI menu to enter the Keeps dashboard.

### Quick Actions
- `s` - Refresh status from TzKT
- `t` - List tokens  
- `m` - Mint a piece
- `f` - Jump to 🐟-fishy terminal
- `r` - Run test suite
- `e` - Open tzkt explorer in browser
- `o` - Open objkt collection in browser
