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

### Quick commands (fish shell)

```bash
ac-os build         # Build binary + initramfs + kernel
ac-os flash         # Build + flash USB
ac-os upload        # Upload current build as OTA release
ac-os flash+upload  # Build + flash + upload
```

### Manual steps

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

## Credentials & Secrets

- **DO Spaces (OTA upload)**: `aesthetic-computer-vault/fedac/native/upload.env.gpg`
  - GPG-encrypted with Jeffrey's key (`77E1473C0FF13AB2`)
  - Decrypt: `gpg --decrypt aesthetic-computer-vault/fedac/native/upload.env.gpg > /tmp/upload.env`
  - Contains: `DO_SPACES_KEY`, `DO_SPACES_SECRET`
- **GPG private key**: `.tmp-key-jeffrey-private.asc` (in repo root, gitignored)
- **Handle colors API**: `https://aesthetic.computer/.netlify/functions/handle-colors` (public, no auth)

## Update Signal Expectations

- Native notepat checks `native-notepat-latest.version` on WiFi connect and periodic background checks.
- When remote version differs from local `system.version`, UI shows update availability and emits audible notification.
- Download+flash uses:
  - `system.fetchBinary(...)` -> `/tmp/vmlinuz.new`
  - `system.flashUpdate(...)` -> boot EFI partition
  - `system.reboot()` after successful flash

## USB Flash Methods

### Method 1: build-and-flash.sh (full pipeline)

Builds binary, packs initramfs, compiles kernel, partitions + flashes USB.

```bash
cd fedac/native
bash scripts/build-and-flash.sh --flash /dev/sdX
# Options: --skip-kernel (reuse existing vmlinuz), --skip-binary (reuse ac-native)
```

Requires privileged access to block devices. In devcontainer, `sfdisk`/`mkfs` may lack permissions.

### Method 2: Docker privileged container (from devcontainer)

When the devcontainer can't directly access block devices, use the Docker host:

```bash
# 1. Build binary and initramfs inside devcontainer
cd fedac/native
make CC=gcc                              # builds build/ac-native
bash scripts/build-and-flash.sh --skip-kernel --skip-binary  # rebuilds initramfs only

# 2. Rebuild kernel to embed new initramfs (uses cached objects, fast)
cd build/linux-6.14.2
make -j$(nproc) bzImage
cp arch/x86/boot/bzImage ../vmlinuz

# 3. Flash via privileged Docker container (host path: /home/me/aesthetic-computer)
sudo docker run --rm --privileged \
  -v /home/me/aesthetic-computer/fedac/native/build:/build:ro \
  -v /dev:/dev \
  fedora:41 bash -c '
    dnf install -y -q dosfstools util-linux
    DISK=/dev/sdX
    umount ${DISK}1 2>/dev/null || true
    sfdisk --force $DISK <<EOF
label: gpt
type=C12A7328-F81F-11D2-BA4B-00A0C93EC93B, size=512M
EOF
    sleep 1
    mkfs.vfat -F 32 -n ACBOOT ${DISK}1
    mkdir -p /mnt/efi && mount ${DISK}1 /mnt/efi
    mkdir -p /mnt/efi/EFI/BOOT
    cp /build/vmlinuz /mnt/efi/EFI/BOOT/BOOTX64.EFI
    sync && sleep 1 && sync
    sha256sum /mnt/efi/EFI/BOOT/BOOTX64.EFI /build/vmlinuz
    umount /mnt/efi
'
```

**Key detail**: devcontainer workspace is at `/workspaces/aesthetic-computer` but the host path is `/home/me/aesthetic-computer`. Docker bind mounts must use the host path.

### Method 3: Reflash existing USB (no repartition)

If USB already has an EFI partition, skip partitioning:

```bash
sudo docker run --rm --privileged \
  -v /home/me/aesthetic-computer/fedac/native/build:/build:ro \
  -v /dev:/dev \
  fedora:41 bash -c '
    dnf install -y -q dosfstools
    mount /dev/sdX1 /mnt
    cp /build/vmlinuz /mnt/EFI/BOOT/BOOTX64.EFI
    sync && sleep 1 && sync
    sha256sum /mnt/EFI/BOOT/BOOTX64.EFI /build/vmlinuz
    umount /mnt
'
```

### Verification

Always verify SHA256 match between source vmlinuz and flashed BOOTX64.EFI:

```bash
sha256sum build/vmlinuz  # local
# Must match the sha256sum printed inside the docker container
```

### Device node creation

If `lsblk` shows the USB but `/dev/sdX` doesn't exist in the devcontainer:

```bash
sudo mknod /dev/sda b 8 0
sudo mknod /dev/sda1 b 8 1
```

## OTA Update Hardening (2026-03-11)

Critical fixes applied to the OTA flash path:

1. **posix_fadvise instead of drop_caches**: `drop_caches=3` was destroying tmpfs-backed source file pages. Now uses `POSIX_FADV_DONTNEED` on only the destination file.
2. **Fresh mount for flash**: Always mounts EFI partition at `/tmp/efi` instead of reusing `/mnt` (which may lack `EFI/BOOT/`).
3. **EFI directory validation**: Aborts flash if `EFI/BOOT/` not found on mounted partition.
4. **Source file pre-flight**: Validates source exists and is >1MB before copying.
5. **Double sync with delay**: `syncfs` + `sync` + 500ms sleep + `sync` before verify and before reboot, giving vfat write-back time to flush.
6. **Error logging**: syncfs failures, mount failures, and device detection logged with errno.

## Architecture

See [internals.md](internals.md) for the full boot sequence and system architecture narrative.

## Operational Checks

- USB logs must be checked on every release candidate:
  - `ac-native.log` for `[fetch]`, `[fetchBinary]`, `[flash]`, `[verify]`, `[mic]`, `[sample]`
  - `ac-audio.log` for ALSA/capture diagnostics
- Any `curl exit=77` or cert-path errors are a release blocker.
- Any repeated mic open/close race errors are a release blocker.
- Flash verify must report `OK: N bytes match` — any `MISMATCH` is a release blocker.
