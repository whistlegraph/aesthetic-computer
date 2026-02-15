# Offline Boot Configuration

The FFOS build now boots **straight to offline starfield** by default.

## Current Configuration

- **Default:** Boots to `file:///opt/ac/offline-pieces/starfield.html`
- **No internet required** - works completely offline
- **Bundled pieces:** starfield, notepat, roz

## Switching to Different Offline Pieces

Edit `overlays/ffos-user/users/feralfile/.config/systemd/user/aesthetic-kiosk.service`:

```ini
# Boot to offline notepat instead:
Environment=AC_URL=file:///opt/ac/offline-pieces/notepat.html

# Boot to offline roz (KidLisp):
Environment=AC_URL=file:///opt/ac/offline-pieces/roz.html

# Boot to offline starfield (default):
Environment=AC_URL=file:///opt/ac/offline-pieces/starfield.html
```

## Switching to Online Mode

To boot to the online aesthetic.computer instead:

```ini
# Requires internet connection
Environment=AC_URL=https://aesthetic.computer?tv=true&nogap=true&nolabel=true
```

## Adding More Offline Pieces

Edit `build.sh` line 279:

```bash
OFFLINE_PIECES="notepat:piece starfield:piece roz:code wand:piece line:piece"
```

Then rebuild the ISO - the bundle-html API will download and bundle them.

## Runtime Configuration

On a running system, you can:

1. **Reconfigure via TUI:**
   ```bash
   ac-setup
   ```

2. **Switch piece manually:**
   ```bash
   # Edit the service file
   nano ~/.config/systemd/user/aesthetic-kiosk.service

   # Reload and restart
   systemctl --user daemon-reload
   systemctl --user restart aesthetic-kiosk.service
   ```

## Build & Flash

```bash
# Build ISO with offline starfield
bash utilities/ffos-build/build.sh

# Verify & flash to USB
bash utilities/ffos-build/verify-iso.sh \
  utilities/ffos-build/.ffos-cache/out/*.iso \
  /dev/sdX
```

Boot your ThinkPad from USB → straight to starfield, no internet needed ✨
