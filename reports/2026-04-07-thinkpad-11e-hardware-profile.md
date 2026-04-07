# ThinkPad 11e Yoga Hardware Profile (for initrd optimization)

**Date:** 2026-04-07
**Machine:** Lenovo ThinkPad 11e Yoga (5th Gen)
**Build:** NixOS DRM-direct (no cage/Wayland)

## Hardware Summary

| Component | Details | Kernel Module |
|-----------|---------|---------------|
| **CPU** | Intel Core m3-8100Y @ 1.10GHz (4 cores) | - |
| **GPU** | Intel UHD Graphics 615 [8086:591c] | i915 |
| **RAM** | 8 GB | - |
| **WiFi** | Intel Wireless-AC 9260 [8086:2526] | iwlwifi, iwlmvm |
| **NVMe** | KIOXIA BG4 256GB [1e0f:0001] | nvme |
| **Audio** | Sunrise Point-LP HD Audio [8086:9d71] | snd_hda_intel |
| **USB** | Sunrise Point-LP USB 3.0 xHCI [8086:9d2f] | xhci_pci |
| **Touchpad** | Elan i2c [04f3:2a40] | elan_i2c, hid_multitouch |
| **Touchscreen** | ELAN901C i2c [04f3:2a40] | i2c_hid_acpi |
| **Bluetooth** | Intel (on WiFi card) | btusb, btintel |
| **Card Reader** | Realtek RTS522A [10ec:522a] | rtsx_pci |
| **DRM device** | /dev/dri/card1 (not card0!) | i915 |

## Input Devices (18 evdev nodes)

- event0: Lid Switch
- event1: Power Button
- event2: Sleep Button
- event3: Power Button
- event4: **AT Translated Set 2 keyboard** (main keyboard)
- event5-9: HDA audio jacks
- event10-12: ThinkPad Extra Buttons, sensor hub
- event13-14: ELAN touchpad (SMBus + i2c)
- event15-17: ELAN touchscreen (multitouch + stylus)

**Critical:** MAX_INPUT_DEVICES was 8, missing event4 (keyboard). Fixed to 24.

## Performance Profile (NixOS DRM-direct)

| Metric | Value | Target |
|--------|-------|--------|
| **Avg frame time** | 35-52ms (19-28 fps) | 16.7ms (60 fps) |
| **Max frame time** | 65ms | <20ms |
| **Paint time** | 9-16ms | OK |
| **Present (page flip)** | 24-28ms | 16ms (1 vblank) |
| **Slow frames** | 300/300 (100%) | <5% |
| **Boot time** | ~90s | <10s |

**Root cause of low FPS:** DRM page flip (`present_us`) takes 24-28ms instead of 16ms. Suggests vsync contention or double-sync with `frame_sync_60fps`. The old bare-metal build hit 60fps on the same hardware.

## Modules Needed for Slim Initrd

Based on this hardware profile, a targeted initrd would need:

```nix
boot.initrd.includeDefaultModules = false;
boot.initrd.availableKernelModules = [
  # Storage (USB boot)
  "xhci_pci" "usb_storage" "uas" "sd_mod"
  # NVMe (internal disk)
  "nvme"
  # GPU (i915 KMS)
  "i915"
  # Filesystems
  "ext4" "vfat" "nls_cp437" "nls_iso8859_1"
];

boot.kernelModules = [
  # WiFi
  "iwlwifi" "iwlmvm"
  # Audio
  "snd_hda_intel" "snd_hda_codec_realtek"
  # Input
  "elan_i2c" "i2c_hid_acpi" "hid_multitouch"
  # Bluetooth
  "btusb" "btintel"
];
```

This would reduce initrd from ~1.4GB to ~50MB, cutting boot from 55s to ~5s.

## Open Issues

1. **FPS**: present_us too high — investigate double vsync or page flip contention
2. **WiFi scan**: still running despite creds check — `/mnt` mount may differ
3. **card1 not card0**: ThinkPad 11e uses card1 for i915, needs wildcard in DRM wait
