# FedAC Native Score

`fedac/native` ships a USB-bootable kernel image with embedded initramfs and pieces.

## Definition Of Shipped

All of the following must be true:

1. `build/vmlinuz` is rebuilt from current `HEAD`.
2. Release is uploaded to CDN (`upload-release.sh`).
3. CDN metadata and hash verify.
4. Running devices can detect the new version and enter update flow.
5. At least one target medium (USB/internal EFI) is flashed and readback-verified.

## Release Procedure

```bash
cd fedac/native

# 1) Build image
bash scripts/build-and-flash.sh --skip-binary
sha256sum build/vmlinuz

# 2) Publish CDN release (updates latest.version/latest.sha256/releases.json)
bash scripts/upload-release.sh build/vmlinuz

# 3) Verify CDN state
curl -fsSL https://releases.aesthetic.computer/os/native-notepat-latest.version
curl -fsSL https://releases.aesthetic.computer/os/native-notepat-latest.sha256
curl -fsSL https://releases.aesthetic.computer/os/releases.json | jq '.latest'
```

## Update Signal Expectations

- Native notepat checks `native-notepat-latest.version` on WiFi connect and periodic background checks.
- When remote version differs from local `system.version`, UI shows update availability and emits audible notification.
- Download+flash uses:
  - `system.fetchBinary(...)` -> `/tmp/vmlinuz.new`
  - `system.flashUpdate(...)` -> boot EFI partition
  - `system.reboot()` after successful flash

## Operational Checks

- USB logs must be checked on every release candidate:
  - `ac-native.log` for `[fetch]`, `[fetchBinary]`, `[flash]`, `[mic]`, `[sample]`
  - `ac-audio.log` for ALSA/capture diagnostics
- Any `curl exit=77` or cert-path errors are a release blocker.
- Any repeated mic open/close race errors are a release blocker.
