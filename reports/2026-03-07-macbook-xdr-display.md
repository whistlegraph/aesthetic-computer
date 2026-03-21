# MacBook Pro XDR Display Issue

**Date:** 2026-03-07
**Machine:** Jeffrey's MacBook Pro M1 Pro (MacBookPro.attlocal.net)
**Symptom:** Built-in Liquid Retina XDR display is dark — backlight works but no visible content. External ASUS PA279 4K monitor works fine.

## Diagnostics (via SSH from Docker container)

### System Profiler
- GPU: Apple M1 Pro, 16 cores, Metal 4
- Built-in display detected as "Color LCD" — Liquid Retina XDR, 3024x1964 Retina
- ASUS PA279 detected as main display, 3840x2160
- **Mirroring is ON** — XDR is hardware mirror of ASUS (ASUS is master)

### Brightness / CoreBrightness Logs
- Brightness slider at 75% (0.75), SDR ~265 nits
- EDR headroom available (up to 16x), no errors in brightness pipeline
- Auto-brightness disabled
- `corebrightnessd` ramping and display link functioning normally

### Power Management
- `displaysleep` = 0 (never sleep display)
- `hibernatemode` = 3, `lowpowermode` = 0
- Sleep prevented by powerd

### WindowServer
- Two pids (774, 617) failed to act on pings before timeout — possible app hangs but not display-related

### Summary
No hardware errors reported by the system. The OS thinks the display is working. This matches a known firmware-level issue affecting M1 MacBook Pro models.

## Reference
- Apple Discussions thread: https://discussions.apple.com/thread/255679613
- Users report blurry/dark internal display after wake from sleep
- External monitors unaffected, suggesting firmware rather than GPU/hardware failure

## Recommended Fix

### Option A: DFU Firmware Restore (preferred)
Requires a second Mac with Apple Configurator 2 and USB-C cable.

1. Shut down the MacBook Pro
2. Connect USB-C from second Mac to MacBook's **front-left port**
3. Enter DFU mode:
   - Hold **Power + Left Control + Left Option + Right Shift** for 10 seconds
   - Release modifier keys, keep holding Power for 10 more seconds
   - Screen stays black (expected)
4. In Apple Configurator 2 on the second Mac:
   - **"Revive"** — reflashes firmware only, preserves data (try this first)
   - **"Restore"** — reflashes firmware + erases + reinstalls macOS (full wipe)

### Option B: macOS Recovery (no second Mac needed)
Does NOT reflash firmware — only reinstalls OS. May not fix the issue.

1. Shut down MacBook
2. Hold Power button until "Loading startup options"
3. Options > Continue (Recovery Mode)
4. Disk Utility > Erase "Macintosh HD" (APFS)
5. Reinstall macOS

### Option C: Apple Store
They can perform DFU restore — should be free (no parts replacement needed).

## Status
- [ ] Attempt DFU Revive
- [ ] If Revive fails, attempt DFU Restore (full wipe)
- [ ] Verify XDR display functionality after restore
