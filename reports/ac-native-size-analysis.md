# AC Native OS Size Analysis

**Date:** 2026-03-19
**Build:** native-notepat-latest (kernel 6.14.2)
**Source:** Live data from `oven.aesthetic.computer:/opt/oven/native-cache/`

---

## 1. Size Summary

| Metric | Size |
|--------|------|
| **vmlinuz (compressed, shipped)** | **271 MB** (284,120,064 bytes) |
| Initramfs uncompressed (cpio) | 781 MB |
| Initramfs compressed (cpio.lz4) | 306 MB (320,097,197 bytes) |
| Initramfs root (on disk) | 783 MB |
| Kernel overhead (vmlinuz - initramfs) | ~-35 MB (kernel re-compresses with gzip) |
| vmlinux (uncompressed ELF) | 357 MB |
| Manifest entries | 1,893 files |

**Compression pipeline:** The initramfs is first compressed with LZ4 (781 MB -> 306 MB, 2.55x ratio). The kernel then wraps everything with gzip (`CONFIG_KERNEL_GZIP=y`), yielding a final vmlinuz of 271 MB. The effective compression ratio from initramfs root to vmlinuz is **2.89x**.

---

## 2. Component Breakdown

### 2.1 Top-Level Directory Sizes

| Directory | Size | % of 783 MB |
|-----------|------|-------------|
| `bin/` | 234 MB | 29.9% |
| `lib/` (firmware + x86_64 libs) | 275 MB | 35.1% |
| `lib64/` (shared libraries + DRI) | 263 MB | 33.6% |
| `usr/` (ALSA, XKB, sbin) | 8.4 MB | 1.1% |
| `pieces/` | 616 KB | 0.1% |
| `etc/` | 684 KB | 0.1% |
| Other (dev, proc, sys, scripts, sbin, var) | <100 KB | ~0% |

**Critical finding:** `lib/x86_64-linux-gnu/` and `lib64/` contain **214 MB of duplicated libraries** (same files, different inodes -- not hardlinks or symlinks). This is the single largest waste.

### 2.2 Detailed Component Breakdown

#### Core Binary

| Component | Size | Notes |
|-----------|------|-------|
| `ac-native` | 1.3 MB | The AC piece runner (dynamically linked, C + QuickJS) |

#### Claude Code

| Component | Size | Notes |
|-----------|------|-------|
| `bin/claude` | **226 MB** | Bun SEA binary, **not stripped**, 1,061 symbols exposed |

This single file accounts for **28.9%** of the uncompressed initramfs and is the #1 largest item.

#### JavaScript Runtime

| Component | Size | Notes |
|-----------|------|-------|
| `pieces/` (34 .mjs files) | 616 KB | clock.mjs (284K) is largest |
| `jslib/` | 4 KB | KidLisp bundle (empty -- bundled by ac-os step) |

#### Shared Libraries (lib64/) -- Top 15

| Library | Size | Purpose | Duplicate in lib/x86_64? |
|---------|------|---------|--------------------------|
| libLLVM.so.20.1 | **137 MB** | LLVM backend for Mesa gallium | YES (137 MB) |
| libgallium-25.2.8.so | **42 MB** | Mesa gallium GPU driver (iris/i915/swrast) | No (lib64 only) |
| libicudata.so.74 | **30 MB** | ICU Unicode data tables | YES (30 MB) |
| libcrypto.so.3 | 5.1 MB | OpenSSL crypto | YES |
| libflite_cmu_us_slt.so.1 | 4.0 MB | Flite TTS female voice (slt) | YES |
| libflite_cmu_us_slt.so.2.2 | 4.0 MB | Same voice, version duplicate in lib64 | No |
| libstdc++.so.6 | 2.5 MB | C++ standard library | YES |
| libicuuc.so.74 | 2.1 MB | ICU Unicode common | YES |
| libc.so.6 | 2.1 MB | glibc | YES |
| libgnutls.so.30 | 2.0 MB | GnuTLS | YES |
| libxml2.so.2 | 1.9 MB | XML parser | YES |
| libunistring.so.5 | 1.7 MB | Unicode string handling | YES |
| libp11-kit.so.0 | 1.7 MB | PKCS#11 module loader | YES |
| libflite_cmu_us_kal.so.1 | 1.4 MB | Flite TTS male voice (kal) | YES |
| libglib-2.0.so.0 | 1.3 MB | GLib | YES |

Total files: 121 shared libraries in lib64/ (50 are symlinks).

#### Library Duplication Detail

The build script copies libs to `lib64/` for most binaries, but `ac-native` libs go to `lib/x86_64-linux-gnu/` (via canonical path resolution). The result is two full copies:

| Path | Size | Role |
|------|------|------|
| `lib64/` | 263 MB | Used by claude, cage, busybox, git, curl, etc. |
| `lib/x86_64-linux-gnu/` | 217 MB | Used by ac-native (canonical Debian paths) |
| **Overlap** | **214 MB** | Same content, different inodes |

#### Firmware

| Category | Size | Files | Notes |
|----------|------|-------|-------|
| **i915 GPU** | **27 MB** | 128 blobs | All Intel GPU generations |
| **Intel SOF audio** | **31 MB** | ~180+ files | 4 directories (sof, sof-ipc4, sof-tplg, sof-ipc4-tplg) |
| WiFi (iwlwifi) | 2.0 MB | 4 ucodes | 9260, AX200, AX201 |
| Regulatory DB | 12 KB | 2 files | |
| **Total firmware** | **59 MB** | ~314 files | |

**i915 by generation:**

| Generation | Size | Blobs | Target hardware |
|------------|------|-------|-----------------|
| tgl (Tiger Lake) | 4.3 MB | 15 | 11th gen |
| icl (Ice Lake) | 3.0 MB | 10 | 10th gen |
| dg1/dg2 (Arc) | 4.6 MB | 16 | Discrete GPU |
| skl (Skylake) | 2.0 MB | 17 | 6th gen |
| kbl (Kaby Lake) | 1.9 MB | 13 | 7th/8th gen |
| bxt (Broxton) | 1.9 MB | 13 | Atom |
| ehl (Elkhart Lake) | 2.1 MB | 6 | Embedded |
| mtl (Meteor Lake) | 2.1 MB | 5 | 14th gen (current target) |
| adlp (Alder Lake) | 1.7 MB | 10 | 12th gen |
| glk (Gemini Lake) | 1.6 MB | 9 | Atom |
| cml (Comet Lake) | 1.2 MB | 6 | 10th gen |
| Other (rkl, cnl, xe3lpd, xe2lpd, bmg, adls) | <0.3 MB | 6 | Various |

**Intel SOF audio breakdown:**

| Directory | Size | Notes |
|-----------|------|-------|
| sof-tplg/ (legacy topologies) | 9.7 MB | Old IPC format |
| sof-ipc4/ (new firmware) | 7.7 MB | 11 platform dirs |
| sof/ (legacy firmware) | 6.7 MB | Old IPC format |
| sof-ipc4-tplg/ (new topologies) | 6.4 MB | 129 topology files |

SOF IPC4 platforms: adl, adl-n, adl-s, arl, arl-s, lnl, mtl, rpl, rpl-s, tgl, tgl-h

#### Mesa GPU Stack

| Component | Size | Notes |
|-----------|------|-------|
| libLLVM.so.20.1 | 137 MB | JIT compiler for gallium shaders |
| libgallium-25.2.8.so | 42 MB | Monolithic driver (iris + i915 + swrast) |
| lib64/dri/ (driver stubs) | 120 KB | 1 real file + 4 symlinks |
| EGL/GBM/GLES libs | ~2 MB | Symlinks into lib64/ |
| **Total Mesa (deduplicated)** | **~181 MB** | libLLVM + gallium + supporting libs |

**Dependency chain:** `dri/iris_dri.so` -> `libdril_dri.so` -> `libgallium` -> `libLLVM.so.20.1` -> `libicudata.so.74`

#### System Tools

| Binary | Size | Notes |
|--------|------|-------|
| busybox | 2.1 MB | 45+ applet symlinks (sh, find, cp, mv, etc.) |
| git | 3.9 MB | For Claude Code git operations |
| awk (gawk) | 724 KB | GNU awk |
| ip | 756 KB | Network config |
| ac-trace | 412 KB | BPF/ftrace diagnostic tool |
| iw | 308 KB | WiFi scanner |
| curl | 292 KB | HTTP client |
| grep | 184 KB | Pattern search |
| dropbear | 165 KB | SSH daemon |
| sed | 112 KB | Stream editor |
| sfdisk | 108 KB | Partition tool (USB flash) |
| cage | 64 KB | Wayland kiosk compositor |
| seatd | 48 KB | Seat daemon for wlroots |
| mkfs.vfat | 52 KB | FAT32 formatter |
| dropbearkey | 47 KB | SSH key generator |
| jq | 32 KB | JSON processor |
| Other (head, cut, sleep, etc.) | ~200 KB | Small coreutils |

#### Flite TTS

| Component | Size | Copies | Notes |
|-----------|------|--------|-------|
| libflite_cmu_us_slt (female voice) | 4.0 MB | 3 copies (lib64 .so.1, .so.2.2, lib/x86_64) | **12 MB total** |
| libflite_cmu_us_kal (male voice) | 1.4 MB | 3 copies | **4.2 MB total** |
| libflite_cmulex (lexicon) | 600 KB | 3 copies | 1.8 MB total |
| libflite.so (core) | 228 KB | 3 copies | 684 KB total |
| libflite_usenglish.so | 168 KB | 3 copies | 504 KB total |
| **Total Flite (all copies)** | | | **~19 MB** |

The .so.1 and .so.2.2 files in lib64/ are also duplicates (different inodes, same content).

#### Audio Config

| Component | Size | Notes |
|-----------|------|-------|
| ALSA ucm2/ | 2.4 MB | Use Case Manager configs |
| ALSA topology/ | 364 KB | |
| ALSA cards/ | 272 KB | |
| ALSA pcm/ + ctl/ + conf | ~90 KB | |
| **Total ALSA** | **3.1 MB** | |

#### XKB Keyboard Data

| Component | Size | Notes |
|-----------|------|-------|
| symbols/ | 2.5 MB | All keyboard layouts worldwide |
| rules/ | 804 KB | Layout selection rules |
| geometry/ | 404 KB | Physical keyboard geometry |
| keycodes/ | 148 KB | |
| compat/ + types/ | 152 KB | |
| **Total XKB** | **4.0 MB** | |

#### Other

| Component | Size | Notes |
|-----------|------|-------|
| CA certificates | 444 KB | /etc/pki/ |
| SSL certs (duplicate CA) | 224 KB | /etc/ssl/ |
| init script | 2.9 KB | Boot script |
| scripts/ | 4 KB | upload-log.sh |
| dhclient-script | 4 KB | |
| git-credential-ac | 4 KB | PAT helper |

---

## 3. Shrink Opportunities

### 3.1 Library Deduplication (lib64/ vs lib/x86_64-linux-gnu/)

**Current waste: 214 MB of exact duplicates**

The build script (`build-and-flash.sh`) copies libs in two ways:
1. Per-binary `ldd` copies go to `lib64/`
2. ac-native's canonical-path copy goes to `lib/x86_64-linux-gnu/`

**Fix:** Replace `lib/x86_64-linux-gnu/` with a symlink to `lib64/` (or vice versa). The transitive dependency resolver (lines 740-769 of build-and-flash.sh) already copies everything to lib64/, so the x86_64-linux-gnu directory can be a symlink.

**Estimated savings:** 214 MB uncompressed, ~84 MB compressed
**Difficulty:** Low (single line change in build script)
**Risk:** Low (just a symlink, all paths still resolve)

### 3.2 Flite TTS Version Duplicates in lib64/

Both `.so.1` and `.so.2.2` exist as separate files with identical content (different inodes).

**Fix:** Make `.so.1` a symlink to `.so.2.2` (or vice versa).

**Estimated savings:** 6.4 MB (slt 4.0 MB + kal 1.4 MB + cmulex 600 KB + core 228 KB + usenglish 168 KB)
**Difficulty:** Low
**Risk:** Low

### 3.3 Claude Code Binary (226 MB)

The binary is a Bun Single Executable Application (SEA). It is **not stripped** (1,061 symbols, `file` reports "not stripped"). However, the build script contains a warning: "Do NOT strip -- Claude Code is a Bun SEA binary and stripping destroys the embedded JS blob."

**Options:**
1. **UPX compression:** UPX can compress ELF binaries ~50-70%. The binary would self-extract at startup (adds ~1-2 seconds boot time). UPX is not currently installed on the oven. Bun SEA binaries may or may not survive UPX -- needs testing.
   - **Potential savings:** 100-150 MB
   - **Difficulty:** Medium (needs testing)
   - **Risk:** Medium (may break Bun SEA resource extraction)

2. **Lazy load from USB:** Instead of embedding Claude in the initramfs, keep it on the FAT32 USB partition and mount + exec at runtime. The initramfs would ship without Claude, loading it on demand.
   - **Potential savings:** 226 MB from initramfs (but still on USB)
   - **Difficulty:** Medium (requires init script changes + mount logic)
   - **Risk:** Low (USB is always present at boot)

3. **Download on first use:** Only fetch the Claude binary when the user first invokes it (requires network).
   - **Potential savings:** 226 MB
   - **Difficulty:** Medium
   - **Risk:** High (requires network, slow first launch)

4. **Wait for smaller Claude builds:** Anthropic may release a more compact Claude Code binary in the future.

### 3.4 libLLVM.so.20.1 (137 MB)

This is the LLVM JIT compiler, required by Mesa gallium for shader compilation. The dependency chain is: `iris_dri.so` -> `libgallium` -> `libLLVM`.

**Options:**
1. **Build Mesa without LLVM:** Mesa can be built with `-Dllvm=disabled`. This removes software shader compilation (swrast) and some gallium optimizations, but the Intel iris driver can still work using Intel's proprietary shader compiler path (i965 backend, not gallium). However, the Ubuntu-shipped Mesa packages always include LLVM.
   - **Potential savings:** 137 MB (+ 30 MB libicudata which is only needed by libLLVM)
   - **Difficulty:** High (custom Mesa build required)
   - **Risk:** Medium (may degrade GPU performance or break some features)

2. **Strip LLVM:** The shipped libLLVM may contain debug sections or unnecessary targets. A custom LLVM build targeting only x86_64 backend would be significantly smaller (~30-50 MB).
   - **Potential savings:** 90-110 MB
   - **Difficulty:** High
   - **Risk:** Low (if built correctly)

3. **Use i965 DRI driver instead of iris/gallium:** For older Intel hardware, the classic i965 driver doesn't need LLVM. But for modern Intel (Gen 12+), iris is required.
   - Not recommended for current target hardware.

### 3.5 libicudata.so.74 (30 MB)

ICU Unicode data tables. Only loaded by libLLVM (which loads it via libxml2 -> libicuuc -> libicudata). Nothing else in the initramfs directly needs ICU.

**Fix:** Eliminating LLVM (3.4 above) would also eliminate ICU.

**Estimated savings:** 30 MB
**Difficulty:** Tied to LLVM removal
**Risk:** Same as LLVM removal

### 3.6 i915 GPU Firmware (27 MB, 128 blobs)

The build copies **all** i915 generations from the host. If AC Native OS only targets specific hardware (e.g., Meteor Lake laptops), most can be removed.

**Minimum set for Meteor Lake + Alder Lake:**
- mtl (5 blobs, 2.1 MB) -- 14th gen
- adlp (10 blobs, 1.7 MB) -- 12th gen
- tgl (15 blobs, 4.3 MB) -- 11th gen (backwards compat)

**Everything else removable:** skl, kbl, bxt, glk, icl, cml, ehl, dg1, dg2, rkl, cnl, xe3lpd, xe2lpd, bmg, adls

**Estimated savings:** ~19 MB (keep ~8 MB for mtl+adlp+tgl)
**Difficulty:** Low (filter in build-and-flash.sh line 369-383)
**Risk:** Medium (breaks support for older Intel hardware)

### 3.7 Intel SOF Audio Firmware (31 MB)

The build copies ALL SOF firmware (legacy IPC + IPC4, all platforms, all topologies).

**Options:**
1. **Remove legacy SOF (sof/ + sof-tplg/):** Modern Intel uses IPC4. Legacy is for pre-TGL.
   - **Savings:** 16.4 MB
   - **Risk:** Breaks audio on pre-Tiger Lake hardware

2. **Trim IPC4 topologies:** 129 topology files, most for specific codec combinations. The device likely only needs the HDA-generic topologies + its specific codec.
   - **Savings:** ~4-5 MB
   - **Risk:** Medium (need to identify correct topology)

3. **Trim IPC4 platforms:** Keep only mtl + arl (current targets), remove adl/tgl/rpl variants.
   - **Savings:** ~4 MB
   - **Risk:** Medium

**Total potential SOF savings:** ~20-25 MB
**Difficulty:** Medium (requires knowing exact target hardware)

### 3.8 Flite TTS Voice Models

Two voices are bundled: slt (female, 4.0 MB) and kal (male, 1.4 MB).

**Options:**
1. **Keep only one voice:** Remove kal (male) if unused.
   - **Savings:** ~1.4 MB (plus duplicates = ~4.2 MB)
   - **Difficulty:** Low
   - **Risk:** Low

2. **Use kal only (smaller):** kal is 1.4 MB vs slt at 4.0 MB.
   - **Savings:** ~2.6 MB (plus duplicates)
   - **Difficulty:** Low
   - **Risk:** Low (lower voice quality)

### 3.9 XKB Keyboard Data (4.0 MB)

The `symbols/` directory (2.5 MB) contains layouts for every language/country.

**Fix:** Keep only `us`, `gb`, and `inet` (for media keys). Remove all other layouts.

**Estimated savings:** ~2-3 MB
**Difficulty:** Low
**Risk:** Low (US keyboard is the primary target)

### 3.10 Git Binary (3.9 MB)

Git is bundled for Claude Code's git operations on-device.

**Options:**
1. **Keep it:** Claude Code is significantly more capable with git. At 3.9 MB, it's not a major contributor.
2. **Lazy-load from USB partition:** Similar to Claude binary lazy-loading.

**Recommendation:** Keep. The 3.9 MB is worth the capability.

### 3.11 Compression Algorithm

Currently: LZ4 for initramfs, gzip for kernel wrapping.

| Algorithm | Compress ratio | Decompress speed | Boot impact |
|-----------|---------------|------------------|-------------|
| LZ4 (current) | 2.55x | ~4 GB/s | Fastest boot |
| zstd | ~3.0-3.5x | ~1.5 GB/s | +100-200ms |
| xz/lzma | ~3.5-4.0x | ~200 MB/s | +1-3s |

The kernel already supports `CONFIG_RD_ZSTD=y`. Switching initramfs to zstd would save ~30-60 MB in vmlinuz at the cost of slightly slower boot.

For the kernel wrapper, switching from gzip to zstd (`CONFIG_KERNEL_ZSTD=y`) would save another ~10-20 MB.

**Total compression savings (zstd):** ~40-80 MB in final vmlinuz
**Difficulty:** Low (kernel config change)
**Risk:** Low (adds ~200-400ms boot time)

### 3.12 ALSA UCM2 Config (2.4 MB)

The UCM2 directory contains Use Case Manager configs for many different sound cards.

**Fix:** Keep only the configs for the target hardware's codec.

**Estimated savings:** ~2 MB
**Difficulty:** Medium (need to identify correct configs)
**Risk:** Low

---

## 4. Recommended Reductions (Prioritized)

| # | Change | Savings (uncompressed) | Est. vmlinuz savings | Difficulty | Risk |
|---|--------|----------------------|---------------------|------------|------|
| 1 | **Deduplicate lib/x86_64 -> lib64 symlink** | **214 MB** | **~84 MB** | Low | Low |
| 2 | **Remove legacy SOF firmware (sof/ + sof-tplg/)** | **16.4 MB** | ~6 MB | Low | Medium |
| 3 | **Trim i915 to target generations only** | **19 MB** | ~7 MB | Low | Medium |
| 4 | **Fix Flite .so.1/.so.2.2 duplicates (symlinks)** | **6.4 MB** | ~2 MB | Low | Low |
| 5 | **Switch to zstd compression** | 0 (compression only) | **~40-80 MB** | Low | Low |
| 6 | **Trim XKB to US layout only** | **3 MB** | ~1 MB | Low | Low |
| 7 | **Trim SOF IPC4 topologies + platforms** | **8 MB** | ~3 MB | Medium | Medium |
| 8 | **Lazy-load Claude from USB partition** | **226 MB** | **~89 MB** | Medium | Low |
| 9 | **Build Mesa without LLVM (custom package)** | **167 MB** | **~65 MB** | High | Medium |
| 10 | **UPX-compress Claude binary** | **~130 MB** | **~51 MB** | Medium | Medium |
| 11 | **Remove second TTS voice (kal)** | **4.2 MB** | ~1.5 MB | Low | Low |
| 12 | **Trim ALSA UCM2 configs** | **2 MB** | ~1 MB | Medium | Low |

### Quick Wins (items 1-6): ~259 MB uncompressed, ~140-180 MB vmlinuz savings

Applying just the top 6 low-difficulty changes would reduce the vmlinuz from **271 MB** to approximately **~90-130 MB**.

### Full Optimization (items 1-12): ~796 MB uncompressed savings

With all changes including LLVM removal and Claude lazy-loading, the vmlinuz could theoretically reach **~50-80 MB**.

### Fastest Single Fix

**Item #1 alone** (lib deduplication) saves 214 MB uncompressed / ~84 MB compressed. It requires changing approximately one line in `build-and-flash.sh` to create a symlink instead of a separate directory. This should be done immediately -- it's free savings from a build bug.
