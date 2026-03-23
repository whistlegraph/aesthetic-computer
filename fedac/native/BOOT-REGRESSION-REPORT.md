# AC Native OS Boot Regression Report

**Date:** 2026-03-21
**Affected Machine:** ThinkPad Yoga 11e Gen 5 (Intel Gemini Lake, UHD 600/605)
**Symptom:** "no DRM device" — `/dev/dri/card0` never appears
**Last Working Build:** ~March 19, 2026 (devcontainer builds)
**Broken Since:** March 20, 2026 (oven builds began)

---

## Root Cause Analysis

The boot regression has **three compounding causes**, all introduced between March 19-21:

### 1. Firmware Loading Changed (HIGH IMPACT)

**Before (working):**
```
CONFIG_EXTRA_FIRMWARE="iwlwifi-9260-th-b0-jf-b0-46.ucode iwlwifi-cc-a0-77.ucode ..."
CONFIG_EXTRA_FIRMWARE_DIR="/workspaces/aesthetic-computer/fedac/native/build/firmware"
```
WiFi firmware was **compiled directly into the kernel binary**. While this was for WiFi, the kernel's firmware loading infrastructure was tested and working at early boot.

**After (broken):**
```
CONFIG_EXTRA_FIRMWARE=""
CONFIG_EXTRA_FIRMWARE_DIR=""
```
Changed to load firmware from initramfs at runtime. This was done to fix the oven build (the oven doesn't have the firmware at the devcontainer path). But it may have broken early firmware loading for i915 GPU, which also needs firmware blobs (DMC, GuC, HuC) before it can probe the GPU.

**Why this matters for the Yoga 11e:** Gemini Lake GPUs (GLK) require `glk_dmc_ver1_04.bin` firmware to initialize the display. If the firmware loader can't find it early enough, i915 fails to probe and `/dev/dri/card0` is never created.

### 2. Init Script Complexity (MEDIUM IMPACT)

**Before (working):** 35 lines
```sh
#!/bin/sh
mount -t devtmpfs devtmpfs /dev
mkdir -p /dev/pts /tmp /run
# ... basic mounts ...
# Wait for GPU (up to 1 second)
while [ ! -e /dev/dri/card0 ] && [ $i -lt 100 ]; do
  usleep 10000; i=$((i+1))
done
exec /ac-native /piece.mjs
```

**After (broken):** 163+ lines with:
- USB device detection and logging
- Crash recovery loop (5 retries)
- Claude credentials restoration
- seatd/cage Wayland setup
- zram swap with error checking
- cpufreq governor setting
- Diagnostic dump on failure

The added complexity introduces more failure points. Commands like `mkdir`, `sleep`, `mount` need to work — and when busybox is missing or shared libs are broken, the init fails silently.

### 3. Busybox vs Dynamic Binaries (MEDIUM IMPACT)

**Before (working):** Statically-linked busybox provided `/bin/sh` and all shell utilities. Zero shared library dependencies for basic commands.

**After (broken, local builds):** The devcontainer's `build-and-flash-initramfs.sh` copies individual Fedora binaries (bash, coreutils) which are dynamically linked and need `libc.so.6`, `libtinfo.so.6`, `libffi.so.8`, etc. If any lib is missing, basic commands like `sleep` and `mkdir` fail.

**After (broken, oven builds):** The oven correctly uses busybox (statically linked), but the binary (`ac-native`) is compiled with `musl-gcc` while shared libraries are glibc — they're incompatible. Later fixed to use `gcc`, but the initramfs was still missing the glibc dynamic linker initially.

---

## Timeline of Changes

| Date | Commit | Change | Impact |
|------|--------|--------|--------|
| Mar 19 | `1358c1f28` | Last known working build | Baseline |
| Mar 20 | `1d8a56758` | Added `ac-os pull` command | Minor |
| Mar 20 | `efeec75dc` | Oven build race condition fix | Started oven builds |
| Mar 20 | `11bb20c2f` | Cleared CONFIG_EXTRA_FIRMWARE | **BROKE firmware loading** |
| Mar 21 | `c847d82c2` | Added apt/Ubuntu package install | Fixed oven deps |
| Mar 21 | `8dc9ead1b` | musl linker fix attempt | Partial fix |
| Mar 21 | `d804c9776` | Switched to gcc from musl-gcc | Fixed linking |
| Mar 21 | `c4398bccd` | Added AMD/Nvidia/simpledrm GPU drivers | Added complexity |
| Mar 21 | `1df5dba77` | Created /dev/console in initramfs | Partial fix |
| Mar 21 | `97ec7cf23` | Added crash recovery to init | Added complexity |
| Mar 21 | `f9c4999da` | Fixed zstd firmware decompression | Fixed firmware format |
| Mar 21 | Multiple | Various init script changes | More complexity |

---

## The Fix

### Option A: Revert to Simple (Recommended)
1. **Restore `CONFIG_EXTRA_FIRMWARE`** with i915 GLK firmware added alongside WiFi firmware
2. **Revert init to the simple 35-line version** that was working
3. **Ensure busybox is always used** as `/bin/sh` (statically linked)
4. Add features back incrementally, testing on the Yoga 11e each time

### Option B: Fix Forward
1. Add `CONFIG_FW_LOADER_COMPRESS_ZSTD=y` to kernel config so firmware can load from compressed initramfs files
2. Ensure initramfs firmware is at `/lib/firmware/` accessible before i915 probes
3. Keep crash recovery but make it more robust (no shared lib deps in crash path)
4. Test with QEMU smoke test before every flash

### Option C: Hybrid
1. Build i915 firmware INTO the kernel (`CONFIG_EXTRA_FIRMWARE`) for reliability
2. Load other firmware (WiFi) from initramfs (less critical, happens after boot)
3. Keep simple init for boot, add features as post-boot services

---

## Oven vs Devcontainer Build Differences

| Aspect | Devcontainer (Fedora) | Oven (Ubuntu) |
|--------|----------------------|---------------|
| Compiler | gcc (glibc) | gcc (glibc) — was musl-gcc |
| Busybox | Not installed by default | Installed, statically linked |
| Shared libs | Fedora .so files | Ubuntu .so files |
| Mesa/GPU | Full Mesa stack available | Mesa installed via apt |
| Firefox | Available | Not available |
| Claude Code | Available (225MB) | Available |
| Kernel compile | 2 cores, ~15 min | 8 cores, ~2 min |
| Initramfs size | ~287MB (with Firefox+Claude) | ~200MB (no Firefox) |

---

## Recommended Next Steps

1. **Immediate:** Flash a build from the `1358c1f28` commit (last known working) to confirm the Yoga 11e still works with old code
2. **Then:** Apply Option C (hybrid) — bake GPU firmware in, load WiFi firmware from initramfs
3. **Then:** Add features back one at a time, testing each on the Yoga 11e
4. **Long-term:** QEMU smoke test in oven pipeline catches boot failures before upload
