# FedAC Wayland Kiosk Build Process

Date: March 3, 2026

This note documents the current local workflow used to build and flash a FedAC
Wayland kiosk image for a bundled piece (for example `notepat`).

## 1) Build host and paths

- Build host: ThinkPad on LAN (`me@192.168.1.211`)
- Script: `fedac/scripts/make-kiosk-piece-usb.sh`
- Output image path pattern: `fedac/out/notepat-fedora-wayland-<tag>.img`
- Work dir override (required on this host): `--work-base /home/me/fedac-work`

Reason for `--work-base`: `/tmp` is a tmpfs on this machine and can run out of
space during rootfs copy + EROFS creation.

## 2) Runtime mode

The kiosk launcher is Wayland-first:

- Primary compositor: `weston`
- Fallback compositor: `cage`
- Browser: Firefox kiosk mode

The kiosk URL keeps the AC HUD/corner label enabled:

- `?density=<n>&nogap=true&noauth=true`

Important: do not force `desktop=true`, `nolabel=true`, or `solo=true` if the
prompt HUD/corner label should remain visible.
Also avoid `device=true` here: device mode intentionally suppresses AC HUD
corner labels.

## 2.5) Local bundle rebuild (when oven is down)

If `oven.aesthetic.computer` is unreachable, rebuild the local piece bundle
from this repo:

```bash
cd /home/me/aesthetic-computer
node fedac/scripts/build-local-piece-bundle.mjs notepat \
  --out /home/me/aesthetic-computer/fedac/out/notepat-local-piece.html \
  --density 8 \
  --nocompress
```

## 3) Build command (image only)

```bash
cd /home/me/aesthetic-computer
sudo bash fedac/scripts/make-kiosk-piece-usb.sh notepat \
  --bundle /home/me/aesthetic-computer/fedac/out/notepat-local-piece.html \
  --rebuild-bundle \
  --image /home/me/aesthetic-computer/fedac/out/notepat-fedora-wayland-fix7.img \
  --image-size 6 \
  --work-base /home/me/fedac-work \
  --yes
```

## 4) Flash + verify command

```bash
IMG=/home/me/aesthetic-computer/fedac/out/notepat-fedora-wayland-fix7.img
DEV=/dev/sda

sudo umount ${DEV}?* 2>/dev/null || true
sudo dd if="$IMG" of="$DEV" bs=4M status=progress conv=fsync
sync

IMG_BYTES=$(stat -c%s "$IMG")
IMG_BLOCKS=$(((IMG_BYTES + 4194303)/4194304))
IMG_HASH=$(sha256sum "$IMG" | awk '{print $1}')
DEV_HASH=$(sudo dd if="$DEV" bs=4M count="$IMG_BLOCKS" status=none | \
  head -c "$IMG_BYTES" | sha256sum | awk '{print $1}')

echo "image sha256:  $IMG_HASH"
echo "device sha256: $DEV_HASH"
test "$IMG_HASH" = "$DEV_HASH"
```

## 5) USB log inspection after boot

```bash
sudo mount -L FEDAC-PIECE /mnt/piece
sudo ls -la /mnt/piece
sudo tail -n 200 /mnt/piece/kiosk.log
sudo tail -n 200 /mnt/piece/kiosk-wayland.log
sudo tail -n 200 /mnt/piece/kiosk-firefox.log
```

If `/mnt/piece` is empty after boot, the USB image may not have been flashed
completely, or the booted media is not the expected device.
