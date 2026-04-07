# AC Native OS: NixOS vs Bare-Metal Pipeline Analysis

**Date:** 2026-04-06
**Context:** First NixOS USB build for @oskie's ThinkPad 11e Yoga. Compared against the existing custom bare-metal build (`ac-os build`).

## Session Summary

Built and iterated on a NixOS-based USB image across ~8 rebuilds. Fixed WiFi (regulatory DB, root privileges, PATH utilities), sound (ALSA config path), KidLisp (esbuild bundle), boot text (quiet params), cursor (Wayland wl_pointer_set_cursor), shutdown (systemctl vs reboot() syscall), and trackpad (double evdev+Wayland input). Each fix required a full rebuild + flash cycle (~15min each).

## Head-to-Head Comparison

| Metric | Bare-Metal (`ac-os build`) | NixOS USB |
|--------|---------------------------|-----------|
| **Image size** | ~200MB | ~7.3GB (trimmed from 9.1GB) |
| **Initrd size** | ~30MB (custom) | ~80MB (trimmed from 1.4GB) |
| **Boot time** | ~2s kernel + ~8ms to piece | ~55s loader + 7s kernel + 17s userspace |
| **Display** | DRM direct | cage Wayland compositor |
| **Input** | evdev direct | evdev -> libinput -> cage -> wl_pointer |
| **Audio** | ALSA direct (192kHz) | ALSA direct (works, needs ALSA_CONFIG_PATH) |
| **WiFi** | wpa_supplicant via system() | Same, but needed regulatory DB + root |
| **Shutdown** | reboot() syscall (PID 1) | systemctl poweroff (via systemd) |
| **Build time** | ~3min (make + initramfs + kernel) | ~10min (nix eval + build + QEMU disk image) |
| **Flash time** | ~30s (200MB) | ~8min (7.3GB) |
| **Reproducibility** | Depends on host packages | Fully reproducible (nix store) |
| **Package management** | Manual (dnf/apt on build host) | Declarative (flake.nix) |

## What NixOS Adds

1. **Reproducible builds** — flake.lock pins everything; anyone can rebuild the exact same image
2. **Declarative system config** — hardware.nix, kiosk.nix, wifi.nix are self-documenting
3. **Broad hardware support** — linux-firmware + latest kernel + all GPU drivers out of the box
4. **OTA potential** — NixOS profiles could enable atomic A/B upgrades
5. **Developer onboarding** — `nix build .#usb-image` is one command vs a multi-step pipeline

## What NixOS Costs

1. **35x larger image** (7.3GB vs 200MB) — dominates flash time and storage
2. **~45x slower boot** (~80s vs ~2s to piece running)
3. **Wayland indirection** — cage compositor adds latency, cursor conflicts, and the evdev double-read bug
4. **Iteration speed** — each rebuild is ~15min (build + flash) vs ~4min for bare-metal
5. **Complexity** — 7 config files + flake vs one Makefile + one build script
6. **Debugging** — layers obscure issues (took 8 iterations to fix what bare-metal gets right by default)
7. **KVM requirement** — disk image creation needs QEMU; can't build in standard Codespace

## Root Cause of Performance Gap

The bare-metal build was designed as a **single-process kiosk**: ac-native IS the init system (PID 1), owns DRM directly, reads evdev directly, talks to ALSA directly. Zero abstraction layers.

NixOS interposes: systemd -> seatd -> cage -> wlroots -> libinput -> ac-native. Each layer adds latency, indirection, and failure modes. The Wayland path alone caused 3 bugs (cursor, double input, shutdown) that don't exist in DRM mode.

## Recommendations

### Option A: Keep Bare-Metal for Production, NixOS for Development

Use `ac-os build/flash` for shipping USB sticks to @oskie and production hardware. Use NixOS for:
- CI/CD reproducible builds (oven)
- Testing on unknown hardware
- Development VMs

**Pros:** Ship the fast, proven pipeline now. NixOS matures in parallel.
**Cons:** Two pipelines to maintain.

### Option B: NixOS Without Cage (Hybrid)

Keep NixOS for system management but drop cage:
- Run ac-native directly on DRM (its native mode) instead of under cage
- Use systemd for services but bypass the compositor
- Keeps reproducibility + package management without the Wayland tax

**Implementation:** Change kiosk.nix to exec `ac-native` directly on tty1 instead of `cage -s -- ac-native`. Needs seat/DRM permission setup without a compositor.

**Pros:** Best of both worlds — NixOS reproducibility + bare-metal performance.
**Cons:** Needs DRM master permissions without cage/seatd; some integration work.

### Option C: NixOS With Minimal Initrd + Cage Bypass (Full Investment)

Fully optimize the NixOS path:
1. Custom initrd with only target hardware modules (~30MB)
2. Drop cage, run DRM direct with proper seat permissions
3. Reduce image to ~2-3GB
4. Boot target: <10s

**Pros:** Single pipeline, fully reproducible, fast.
**Cons:** Significant NixOS expertise needed; may fight NixOS conventions.

### Option D: Abandon NixOS for AC Native

Return to the proven `ac-os` pipeline. NixOS adds complexity without proportional benefit for a kiosk that targets known hardware.

**Pros:** Simplest path. Proven. Fast.
**Cons:** Loses reproducibility narrative. Build depends on host state.

## Verdict (Updated 2026-04-07)

**Option D confirmed after extensive testing.** We implemented Option B (DRM-direct, no cage) across ~20 build iterations. Results:

- Cage/Wayland bugs were fixed (cursor, double input, shutdown)
- DRM-direct mode works but runs at **27fps** (vs 60fps bare-metal)
- `present_us` consistently 24-28ms despite: removing frame_sync, disabling mitigations, disabling WiFi, disabling logind
- The NixOS generic kernel's DRM/i915 stack simply performs worse than the custom kernel
- Input required MAX_INPUT_DEVICES bump from 8→24 (ThinkPad has 18 evdev nodes)

**For @oskie and production: use `ac-os build` (bare-metal).** NixOS path is preserved in git for future work but not suitable for shipping today.

### What would fix NixOS performance
1. Custom kernel config matching the bare-metal build's `.config`
2. Or identify why the NixOS i915 DRM page flip takes 27ms vs 16ms (kernel config diff)
3. Or render directly into DRM buffer instead of fb_copy_scaled

## Open Questions

1. Can ac-native acquire DRM master without cage/seatd under systemd? (Needs testing — may just need `video` group + `logind` seat assignment)
2. Is the 1.4GB generic initrd still loading even with `includeDefaultModules = false`? (Check actual initrd size on next build)
3. Should the oven build both variants (bare-metal + NixOS) and let the user choose?
4. Would a USB3 stick eliminate the boot speed concern? (55s / ~10x = ~5s with USB3 speeds)
5. Is there value in NixOS for the NVMe install path? (Install to internal disk removes the USB boot speed issue entirely)

## Files Modified in This Session

### NixOS Config (fedac/nixos/)
- `flake.nix` — kidlispSrc, EFI partition, build filter, boot size
- `configuration.nix` — kidlispSrc passthrough, quiet boot, no getty
- `modules/hardware.nix` — wireless-regdb, slim initrd
- `modules/kiosk.nix` — root service, PATH utilities, ALSA/cursor env vars
- `modules/image.nix` — systemd-boot, silent boot params
- `packages/ac-native/default.nix` — esbuild kidlisp bundle

### ac-native C Source (fedac/native/src/)
- `ac-native.c` — ac_poweroff/ac_reboot helpers for systemd compat
- `input.c` — hide Wayland cursor, skip evdev under Wayland
- `audio.c` — aggressive mic compressor + hard limiter
