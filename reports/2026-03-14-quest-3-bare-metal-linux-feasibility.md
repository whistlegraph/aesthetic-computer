# Quest 3 Bare-Metal Linux: Feasibility Report

**Date:** 2026-03-14
**Goal:** Can we boot AC Native OS on a Meta Quest 3?

---

## TL;DR

The driver stack is **surprisingly ready** — Mesa explicitly supports the Quest 3's GPU, the kernel source is published with complete DSI panel init sequences, WiFi and audio have mainline drivers. But the **bootloader is locked** with no known public unlock method. Without that, nothing else matters.

Two realistic paths forward:
1. **Android NDK sideload** — port ac-native as a Quest app (weeks, not months)
2. **Wait for a bootloader exploit** — then the bare-metal path is viable

---

## 1. Hardware Overview

| Component | Part | Details |
|-----------|------|---------|
| **SoC** | Snapdragon XR2 Gen 2 ("Anorak") | Custom XR SoC, SM8550-adjacent but distinct (MSM ID 549) |
| **CPU** | 6-core Kryo | 2x GOLD + 4x GOLD_PLUS, ~2.36 GHz max |
| **GPU** | Adreno 740v3 | Chip ID `0x43050b00` |
| **Display** | Dual LCD panels | 2064x2208 per eye, MIPI DSI C-PHY, 72-120Hz, DSC compressed |
| **Panel vendors** | Sharp / JDI / BOE | All use Novatek DDIC, custom "Tokki" backlight over UART |
| **WiFi** | WCN7851 | Qualcomm "Kiwi" family, PCIe, WiFi 6E |
| **Audio** | 2x MAX98388 + AK4333 | Analog Devices class-D amps + AKM headphone DAC |
| **USB-C** | Cypress CYPD3177 | USB PD sink, DisplayPort alt mode supported |
| **Tracking** | "SyncBoss" MCU | nRF-series via SPI, proprietary SLAM — no FOSS drivers |

---

## 2. The Bootloader Problem (Showstopper)

**Status: BLOCKED**

Quest 3 uses the standard Qualcomm secure boot chain:
```
PBL → XBL (signed) → ABL (signed) → Android kernel (verified boot)
```

- Bootloader unlock requires an RSA-PSS-SHA-256 signed `unlock_token`
- The signing key lives on Meta's internal server (`our.internmc.facebook.com`)
- Meta does **not** offer a public developer unlock program for Quest devices
- No known exploits exist for Quest 3 (the Quest 2 fastboot overflow only worked on firmware v29 / May 2021)
- dm-verity enforced on system partitions
- Standard Qualcomm anti-rollback fuses

**Without bootloader access, everything below is academic.** But it's worth documenting because exploits do surface, and Meta's policy could change.

---

## 3. Kernel Source (Available)

Meta publishes Quest 3 kernel source (GPL compliance):

- **Repo:** `github.com/facebookincubator/oculus-linux-kernel`
- **Branch:** `oculus-quest3-kernel-master`
- **Kernel version:** Linux 5.10.237
- **Codename:** "Eureka"

### Device Tree (Complete)

Full device tree at `arch/arm64/boot/dts/oculus/eureka/`:

| File | Contents |
|------|----------|
| `eureka.dts` | Main DTS, compatible `"qcom,anorak", "oculus,eureka"` |
| `eureka-panel-sharp.dtsi` | **Complete DSI panel init sequences** (Sharp/Novatek) |
| `eureka-panel-jdi.dtsi` | JDI panel init sequences |
| `eureka-panel-boe.dtsi` | BOE panel init sequences |
| `eureka-audio.dtsi` | MAX98388 + AK4333 bindings |
| `eureka-camera.dtsi` | 6+ cameras (SLAM + passthrough + depth) |

**The DSI panel init sequences are the critical piece** — they contain the full MIPI DSI command payloads needed to bring up the displays. These are available and extractable.

---

## 4. Display Pipeline

### What's in mainline Linux

SM8550 DPU/MDSS support landed in kernel 6.3 (early 2023):

| Component | Mainline Status | Key Commit |
|-----------|----------------|------------|
| DPU (display controller) | Supported since 6.3 | `efcd0107727c` (Neil Armstrong, 2023-01) |
| DSI PHY (4nm) | Supported | `8b034e67711` (2023-01) |
| SmartDMA | Enabled 2025-05 | `3f5e910b33a3` |
| 3D merge (dual-panel) | Supported | Part of SM8550 catalog |

**DPU capabilities:** 4 display interfaces (2x DSI + 2x DP), max line width 5120px, UBWC 4.0, source split.

### What's missing for Quest 3 specifically

The "Anorak" XR2 Gen 2 is a **different SoC** from SM8550 (different MSM ID). Porting requires:

1. **New SoC definitions** — compatible strings, clock controllers (`dispcc-anorak`, `gcc-anorak`)
2. **Panel drivers** — C-PHY mode (not the more common D-PHY), dual-DSI with DSC compression
3. **Custom backlight** — The "Tokki" controller uses UART, not standard PWM/I2C
4. **Board device tree** — regulators, GPIOs, power sequencing

**The panel init sequences from Meta's kernel source can be fed through `linux-mdss-dsi-panel-driver-generator`** to auto-generate DRM panel drivers. Nobody has done this yet, but the data is there.

### Difficulty: Medium-High

The display IP is the same as SM8550, so the DPU driver works. The gaps are:
- C-PHY DSI (less common than D-PHY, but supported in the mainline `dsi_phy_7nm.c` driver)
- Dual-DSI synchronization (supported via 3D merge in DPU)
- DSC (Display Stream Compression) — supported in mainline MSM DSI driver
- Tokki backlight — custom driver needed (~100 lines for UART backlight)

---

## 5. GPU (Ready)

**Adreno 740v3 is explicitly supported in Mesa.** A developer specifically added Quest 3 support:

| Commit | Date | Description |
|--------|------|-------------|
| `0b509708` | 2024-03-26 | "Add A740v3 from Quest 3" — chip_id `0x43050b00` |
| `7968b356` | 2024-09-25 | "Fix A740v3 from Quest 3" — fixes image corruption |
| `0981f983` | 2024-10-25 | Enable 64-bit atomics on A740v3 |

**Driver status:**
- **Turnip (Vulkan):** Vulkan 1.4, 213/287 extensions (74.2%)
- **Freedreno (OpenGL):** OpenGL ES 3.2, desktop OpenGL 4.5
- **UBWC 4.0** compression supported

**Difficulty: Low** — this just works.

---

## 6. Other Hardware

| Component | Driver | Mainline? | Difficulty |
|-----------|--------|-----------|-----------|
| **WiFi** (WCN7851) | ath12k | Yes | Low |
| **Speakers** (MAX98388) | snd-soc-max98388 | Yes | Low |
| **Headphone DAC** (AK4333) | — | No driver found | Medium |
| **USB-C PD** (CYPD3177) | — | Needs driver | Medium |
| **DP alt mode** | MSM DP driver | Yes | Low |
| **Battery** (BQ27Z561) | bq27xxx_battery | Yes | Low |
| **Hall sensor** (AK09973) | — | Needs driver | Low |
| **SLAM tracking** (SyncBoss) | — | **Proprietary** | Very High |
| **Cameras** (SLAM/passthrough) | Qualcomm CSI | Needs proprietary FW | Very High |

**SLAM tracking and cameras are NOT feasible** to port. The SyncBoss MCU runs proprietary firmware and communicates over SPI with an undocumented protocol. Without 6DoF tracking, the headset would function as a fixed-display device only.

---

## 7. AC-Native Platform Portability

Analysis of the current codebase (`fedac/native/src/`):

### Portable Core (~60% of codebase) — No changes needed

| Module | Lines | Notes |
|--------|-------|-------|
| `graph.c` | ~350 | Pure C drawing primitives (line, box, circle, etc.) |
| `framebuffer.c` | ~80 | In-memory ARGB32 buffer management |
| `color.c` | — | Color math |
| `font.c` | — | BDF font loader/renderer |
| `js-bindings.c` | — | QuickJS API surface for pieces |
| QuickJS engine | — | Vendored, cross-platform |
| quirc (QR decode) | — | Vendored, portable C |
| qrcodegen | — | Vendored, portable C |

### Platform Layer (~40%, ~3,650 lines) — Must be replaced

| Module | Lines | Current | Quest replacement |
|--------|-------|---------|-------------------|
| `drm-display.c` | ~520 | Linux KMS/DRM + fbdev + SDL2 | EGL/GLES or OpenXR |
| `audio.c` | ~1200 | ALSA blocking thread, 32-voice synth | AAudio callback-based |
| `input.c` | ~650 | evdev + hidraw | Android InputManager or Quest hand tracking |
| `camera.c` | ~250 | V4L2 | Android Camera2 / Quest passthrough API |
| `wifi.c` | ~400 | wpa_supplicant + dhclient | Android ConnectivityManager |
| `tts.c` | ~200 | Flite | Android TextToSpeech |
| `ac-native.c` (boot) | ~1400 | PID 1 init, mount, signal handlers | Quest app lifecycle |

**The synth engine in `audio.c` is the gnarliest port** — it's written for ALSA's blocking model at 192kHz. Quest audio is callback-based at 48kHz. The synth/reverb/glitch code itself is portable, but the threading model needs a rewrite.

---

## 8. Feasibility Scorecard

| Component | Status | Blocker? |
|-----------|--------|----------|
| **Bootloader unlock** | No known method | **YES — showstopper** |
| Kernel source + device tree | Published, complete | No |
| Display panel init sequences | Available in DTS | No |
| DPU/MDSS driver (mainline) | SM8550 supported, anorak needs porting | No |
| GPU (Mesa Turnip/Freedreno) | Quest 3 A740v3 explicitly supported | No |
| WiFi (ath12k) | Mainline | No |
| Audio (speakers) | Mainline | No |
| 6DoF tracking | **Proprietary SyncBoss MCU** | Yes (for VR) |
| ac-native portable core | ~60% portable as-is | No |
| ac-native platform layer | ~3,650 lines to rewrite | No |

---

## 9. Paths Forward

### Path A: Android NDK Sideload (Recommended)

Build ac-native as a Quest 3 Android app. No bootloader unlock needed.

```
Quest 3 Android → AC Native APK → EGL+GLES / AAudio / Android input
```

- Cross-compile with Android NDK for aarch64
- Use the existing `USE_SDL` path (SDL2 runs on Android) for quick display
- Or use raw EGL/GLES for a flat 2D panel in VR
- Or use OpenXR for proper stereoscopic rendering
- Sideload via `adb install` (developer mode is free to enable)
- **Timeline: 2-3 weeks** for basic piece rendering

### Path B: Bare-Metal Linux (If Bootloader Unlocked)

If a bootloader exploit surfaces or Meta opens up:

1. Port mainline kernel to "anorak" SoC (use SM8550 as base + Meta's published DTS)
2. Generate panel drivers from the published DSI init sequences
3. Mesa GPU drivers work out of the box
4. Build ac-native with DRM backend (already exists) — the display pipeline is standard KMS
5. No 6DoF tracking (SyncBoss is proprietary) — headset becomes a fixed-position display
6. **Timeline: 2-3 months** for display + audio + basic input

### Path C: Alternative Hardware

If the goal is "VR headset running bare-metal Linux":

- **Lynx R1/R2** — Also XR2-based, designed to be open, unlocked bootloader. But extremely limited availability.
- **Simula VR** — Linux-native VR headset project (crowdfunded, unclear shipping status)
- **Steam Deck + VR** — x86_64, already runs Linux, could use existing ac-native with Monado OpenXR. Not a headset but could drive one.

---

## 10. Recommendation

**Start with Path A (Android NDK sideload).** It gets ac-native pieces rendering on Quest 3 hardware without any bootloader drama. The portable core (QuickJS + graph + framebuffer + font) compiles clean for aarch64. The platform layer rewrite is bounded and well-understood.

If you want the full bare-metal experience, **monitor the Quest 3 security research community** — bootloader exploits for Qualcomm devices do surface periodically, and when one does, the software stack is ready. The panel init sequences are published, the GPU drivers exist, and the display controller is mainline.

The irony of the Quest 3 situation: Meta did everything right for open-source compatibility (published GPL kernel source with complete device trees, use standard Qualcomm display/GPU IP that has mainline drivers) — except they locked the front door.
