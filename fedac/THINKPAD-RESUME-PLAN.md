# ThinkPad Resume Plan: Build + Burn Fedora Notepat USB

Use this after pulling latest `main` on the ThinkPad.

## 1) Pull latest

```bash
cd /home/me/aesthetic-computer
git pull --ff-only origin main
git log -1 --oneline
```

## 2) Confirm USB target

```bash
lsblk -dn -o PATH,SIZE,RM,TRAN,MODEL,TYPE
```

Pick the removable USB disk path (example: `/dev/sdb`).

## 3) Rebuild local bundle + build image + burn USB (foreground)

```bash
cd /home/me/aesthetic-computer
DEV=/dev/sdb
LOG=/home/me/fedac-notepat-build-flash-$(date +%Y%m%d-%H%M%S).log

sudo -n bash fedac/scripts/make-kiosk-piece-usb.sh notepat "$DEV" \
  --bundle /home/me/aesthetic-computer/fedac/out/notepat-local-piece.html \
  --rebuild-bundle \
  --image /home/me/aesthetic-computer/fedac/out/notepat-fedora-wayland-next.img \
  --image-size 6 \
  --work-base /home/me/fedac-work \
  --yes |& tee "$LOG"

echo "log: $LOG"
```

## 4) Background mode (optional)

```bash
cd /home/me/aesthetic-computer
DEV=/dev/sdb
LOG=/home/me/fedac-notepat-build-flash-$(date +%Y%m%d-%H%M%S).log

nohup sudo -n bash fedac/scripts/make-kiosk-piece-usb.sh notepat "$DEV" \
  --bundle /home/me/aesthetic-computer/fedac/out/notepat-local-piece.html \
  --rebuild-bundle \
  --image /home/me/aesthetic-computer/fedac/out/notepat-fedora-wayland-next.img \
  --image-size 6 \
  --work-base /home/me/fedac-work \
  --yes > "$LOG" 2>&1 &

echo "log: $LOG"
tail -f "$LOG"
```

## 5) Success check

Look for these lines near the end of the log:

- `Disk image ready`
- `USB flashed from image`
- `FedAC Kiosk Build Ready`

## 6) Notes (already handled in latest script)

- Kiosk URL no longer forces `device=true` (so prompt HUD/corner label can render).
- If oven is unreachable, bundling falls back to local in-repo bundler.
- `--rebuild-bundle` ensures `fedac/out/notepat-local-piece.html` is fresh before burn.
