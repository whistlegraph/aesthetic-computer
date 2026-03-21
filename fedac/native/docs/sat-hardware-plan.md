# SAT Hardware Compatibility Plan

## Email Summary

From: saturnpatty (bogdanborisov02@gmail.com) -- @sat
Subject: hardware
Date: 2026-03-16

They are asking about support for:

- **Device**: ARDOR NEO G15-I5ND302
- **CPU**: 12th Gen Intel Core i5-12450H (Alder Lake)
- **BIOS**: InsydeH2O v.1.07.12TDC
- Note: "yeah not exactly asus acer etc" (lesser-known OEM brand)

The ARDOR NEO G15 is a budget laptop from a smaller brand (likely Chinese OEM). The i5-12450H is an Alder Lake mobile processor with 8 cores (4P + 4E), Intel UHD Graphics (integrated).

## Current AC Native OS Hardware Profile

| Component | Tested On | Kernel Config |
|-----------|-----------|---------------|
| CPU | Intel Kaby Lake (7th gen) | x86_64, CONFIG_EFI=y |
| GPU | Intel i915 (Kaby Lake) | CONFIG_DRM_I915=y (built-in) |
| Audio | ALSA / HDA Intel | CONFIG_SND_HDA_INTEL=y, CONFIG_SND_HDA_CODEC_REALTEK=y |
| WiFi | Intel iwlwifi | CONFIG_IWLWIFI=y, CONFIG_IWLMVM=y |
| USB | XHCI + EHCI | CONFIG_USB_XHCI_HCD=y, CONFIG_USB_EHCI_HCD=y |
| Boot | UEFI USB | CONFIG_EFI_STUB=y |
| Display | DRM direct + Wayland (cage) | CONFIG_DRM_I915=y |

## Compatibility Assessment: ARDOR NEO G15 (i5-12450H)

### CPU -- LIKELY COMPATIBLE

The i5-12450H is x86_64 Alder Lake. AC Native OS runs on x86_64 with EFI stub boot. Alder Lake hybrid architecture (P-cores + E-cores) is supported in kernel 6.14.2 without special config. No issues expected.

### GPU -- LIKELY COMPATIBLE (with caveat)

The i5-12450H has Intel UHD Graphics (Alder Lake GT1). The i915 driver in kernel 6.14.2 supports Alder Lake. `CONFIG_DRM_I915=y` is already built-in and `CONFIG_DRM_I915_FORCE_PROBE=""` means it will auto-detect. This should work out of the box.

**Caveat**: If `DRM_I915_FORCE_PROBE` needs the Alder Lake PCI ID added, we would set it to the specific device ID (e.g., `4692` for UHD Graphics). Check `dmesg` on first boot.

### Audio -- NEEDS VERIFICATION

The current kernel has `CONFIG_SND_HDA_INTEL=y` and `CONFIG_SND_HDA_CODEC_REALTEK=y`. Budget laptops sometimes use different audio codecs. If the ARDOR NEO uses a Realtek HDA codec, it will work. If it uses an Intel SOF (Sound Open Firmware) based audio path (common on Alder Lake), we would need:

```
CONFIG_SND_SOC=y
CONFIG_SND_SOC_SOF_TOPLEVEL=y
CONFIG_SND_SOC_SOF_PCI=y
CONFIG_SND_SOC_SOF_ALDERLAKE=y
CONFIG_SND_SOC_INTEL_AVS=y  (possibly)
```

Currently `CONFIG_SND_SOC` is **not set**. This is the most likely failure point.

**Action required**: Boot the device and check `dmesg | grep -i snd` and `aplay -l` to determine the audio subsystem.

### WiFi -- UNKNOWN, LIKELY NEEDS ADDITIONAL DRIVER

Intel iwlwifi is built in (`CONFIG_IWLWIFI=y`), but the ARDOR NEO G15 (budget laptop) may use:
- **Realtek WiFi** (RTL8821CE, RTL8852BE, etc.) -- very common in budget laptops
- **MediaTek WiFi** (MT7921, MT7922) -- increasingly common

Neither Realtek nor MediaTek WiFi drivers are currently enabled in the kernel config. If the device does not have Intel WiFi, we would need to enable one of:

```
# For Realtek:
CONFIG_RTW88=y or CONFIG_RTW89=y
CONFIG_RTW89_8852BE=y  (or whichever chip)

# For MediaTek:
CONFIG_MT76=y
CONFIG_MT7921E=y
```

**Action required**: Check `lspci | grep -i net` on the device to identify the WiFi chipset. Also need firmware blobs in initramfs for the specific chip.

### UEFI Boot -- LIKELY COMPATIBLE

InsydeH2O is a standard UEFI firmware vendor (used by many OEMs including Lenovo, HP, Acer). `CONFIG_EFI_STUB=y` and `CONFIG_EFI_HANDOVER_PROTOCOL=y` are set. USB boot should work if Secure Boot is disabled in BIOS.

**Note**: Some InsydeH2O firmware versions have quirky boot menus. May need to press F2/F12/Del during POST to access boot device selection.

### USB -- COMPATIBLE

Alder Lake uses XHCI for USB 3.x. `CONFIG_USB_XHCI_HCD=y` and `CONFIG_USB_XHCI_PCI=y` are set. No issues expected.

## Summary

| Component | Status | Blocking? |
|-----------|--------|-----------|
| CPU (i5-12450H) | Should work | No |
| GPU (Intel UHD, Alder Lake) | Should work (i915) | No |
| Audio | May need SOF support | Possibly |
| WiFi | Likely needs non-Intel driver | Probably |
| UEFI Boot | Should work (InsydeH2O) | No |
| USB | Should work | No |

## Recommended Next Steps

1. **Get physical access to the device** (or have @sat run `lspci -nn` and `lsusb` from any live Linux USB)
2. **Identify WiFi chipset** -- this determines whether a kernel config change is needed
3. **Identify audio codec** -- check if SOF is required for Alder Lake audio on this board
4. **Test boot** -- try current AC Native OS USB image; CPU/GPU/USB/UEFI should just work
5. **Kernel config update** -- add missing drivers based on findings, rebuild with `ac-os build`
6. **Firmware blobs** -- add any required firmware files (WiFi, audio) to initramfs

The device is x86_64 Intel, which is the same architecture AC Native OS targets. The main risks are WiFi and audio drivers for a budget OEM board. A quick `lspci` dump from @sat would resolve all unknowns.
