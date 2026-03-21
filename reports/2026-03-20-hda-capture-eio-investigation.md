# HDA Capture EIO Investigation — 2026-03-20

## Summary

Mic sampling in ac-native (notepat sampler) stopped working after kernel config
changes made between March 10-18. Capture device `hw:0,0` opens and configures
but `snd_pcm_readi` returns EIO (Input/output error) on the first read. No audio
data is ever captured.

## Machine

- ThinkPad X13 Gen 1 (Kaby Lake, `0x8086:0x9d71` HDA controller)
- Realtek codec on HDA Intel PCH (card 0)
- ALSA devices: `pcmC0D0p` (playback), `pcmC0D0c` (capture), `pcmC0D3p` (HDMI)
- Playback works fine throughout — only capture is broken

## Timeline

| Date | Commit | What happened |
|------|--------|---------------|
| Mar 10 | `3c083a1f9` | Mic sampling introduced — **working** |
| Mar 10-13 | various | Sampling refined, looping added — **working** |
| Mar 17 | `8e8894f17` | `CONFIG_INTEL_IOMMU=y` added |
| Mar 18 | `de64cf00c` | `CONFIG_SND_HDA_I915` disabled |
| Mar 18 | `a046beabd` | `CONFIG_SND_SOC_SOF_*` added |
| Mar 19-20 | various | Capture broken — EIO on `snd_pcm_readi` |

## What was tried (and failed)

1. **Device order** — switched from `plughw:0,0` back to `hw:0,0` first. No effect (EIO persists).
2. **Period/buffer tuning** — tried period=128/buf=512 (hung forever), ALSA defaults period=32768/buf=1M (hung forever), period=1024/buf=8192 (EIO).
3. **ALSA mixer setup** — enabled Capture Switch, set Capture Volume to 90%, enabled Internal Mic Boost. Mixer reports success but EIO persists.
4. **`intel_iommu=off`** on CMDLINE — no effect.
5. **Removed `CONFIG_SND_SOC_SOF_*`** — no effect.
6. **Removed `CONFIG_SND_SOC_SOF_*` AND restored `CONFIG_SND_HDA_I915=y`** — not tested in isolation.

## Root cause (probable)

Unknown which specific config change causes the EIO. Multiple kernel config
additions between March 10-18 changed the HDA driver environment:

- `CONFIG_INTEL_IOMMU=y` — changes DMA mapping for all PCI devices
- `CONFIG_SND_HDA_I915` disabled — changes HDA power domain management
- `CONFIG_SND_SOC_SOF_HDA=y` — registers alternative HDA driver
- `CONFIG_ZRAM=y`, `CONFIG_BPF_SYSCALL=y`, `CONFIG_NAMESPACES=y`, etc. — general kernel bloat

These interact in complex ways with the HDA controller. Rather than bisecting
individual options, the fix restores the exact working kernel config from
`3c083a1f9` with minimal CMDLINE additions preserved.

## Fix applied

Commit `2233325de` restores the kernel config from `3c083a1f9` (last known
working state for capture). Preserved from the current config:

- `snd_hda_intel.power_save=0` — prevents codec suspend between uses
- `i8042.reset i8042.nomux` — keyboard reliability
- `loglevel=4` — debug visibility (was `quiet loglevel=0`)
- `CONFIG_CMDLINE_OVERRIDE` disabled — allows bootloader param overrides

## Features lost by reverting

The following were added post-March-10 and are now removed:

| Feature | Config | Impact |
|---------|--------|--------|
| Intel IOMMU | `INTEL_IOMMU=y` | DMA isolation — not needed on single-user bare metal |
| SOF audio | `SND_SOC_SOF_*` | Needed for Alder/Meteor Lake, not Kaby Lake |
| SATA/AHCI | `ATA=y, SATA_AHCI=y` | Internal disk access — only needed for install-to-disk |
| Camera/UVC | `USB_VIDEO_CLASS=y` | Webcam support |
| Intel Xe GPU | `DRM_XE=y` | Meteor Lake GPU — not needed on Kaby Lake |
| Dell drivers | `DELL_LAPTOP=y` etc. | Dell keyboard/WMI support |
| Realtek WiFi | `RTW88=y, RTW89=y` | WiFi on Realtek-based laptops |
| MediaTek WiFi | `MT76=y` etc. | WiFi on MediaTek-based laptops |
| USB-C TypeC | `TYPEC=y, UCSI=y` | USB-C power role swap |
| zram swap | `ZRAM=y` | Compressed swap |
| BPF | `BPF_SYSCALL=y` | eBPF programs |
| Namespaces | `NAMESPACES=y` | Container isolation |
| debugfs | `DEBUG_FS=y` | Kernel debug filesystem |
| Touchscreen | `HID_MULTITOUCH=y` | Multi-touch input |
| USB printer | `USB_PRINTER=y` | Thermal printer support |
| SD card | `MMC=y` | SD/MMC card reader |

## Follow-up needed

1. **Test the reverted kernel** — confirm capture works again
2. **Bisect the configs** — re-add features one at a time to find the exact
   culprit. Priority suspects: `INTEL_IOMMU`, `SND_HDA_I915` disable, `SOF_HDA`
3. **Re-add needed features** — SATA (install-to-disk), zram, Realtek/MediaTek
   WiFi should be restored once the capture-breaking option is identified
4. **Consider per-machine kernel configs** — Kaby Lake ThinkPad vs Meteor Lake
   laptop have different driver needs

## Related crashes fixed in this session

1. **Division by zero** (`3568275b5`) — `sample_len=0` caused SIGFPE in audio
   thread modulo operations. Fixed with `if (slen <= 0)` guard.
2. **Batched event race** (`6aa5f0f84`) — all key events processed in single
   frame, recording start/stop had zero time between them. Fixed with spin-wait
   in `audio_mic_stop` for ring buffer data.
