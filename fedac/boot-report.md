# FedAC Kiosk USB — Boot Architecture Report

## Current Boot Chain

```
BIOS/UEFI POST (~1-3s)
  └─ GRUB (timeout=0, instant)
       └─ Kernel decompresses, KMS/DRM init (~2s) ← BLACK #1
            └─ initrd (dracut + dmsquash-live)
                 ├─ Plymouth PALS spinner starts ← first visible frame
                 └─ mounts EROFS via overlayfs, pivots to real root
                      └─ systemd (real root)
                           ├─ livesys.service → livesys-gnome (schema compile, cleanup)
                           ├─ kiosk-piece-server.service (python HTTP server on :8080)
                           └─ multi-user.target
                                └─ getty@tty1 (autologin liveuser)
                                     └─ .bash_profile → kiosk-session.sh
                                          ├─ wait for :8080 (up to 30s)
                                          └─ mutter --wayland --no-x11 -- firefox --kiosk
                                               ├─ mutter compositor bg visible ← FLASH #1 (teal/gray)
                                               └─ Firefox starts kiosk mode
                                                    ├─ tab bar visible briefly ← FLASH #2
                                                    └─ page loads from :8080
                                                         └─ piece renders ✓
```

---

## Known Issues

### 1. BLACK before Plymouth
**Cause**: Kernel boot + early initrd runs entirely before Plymouth initializes.
`loglevel=1` suppresses kernel text but VT is still blank. Plymouth's `ShowDelay=0`
starts it as soon as possible in the initrd, but KMS init takes 1-2s first.

**Fix (hard)**: Write raw pixels to `/dev/fb0` from a dracut pre-udev hook.
The kernel framebuffer (via DRM/KMS emulation) is available even in text mode.
A hook can fill the screen black + optionally draw the PALS logo before Plymouth
even starts. Requires modifying/rebuilding `pals-initrd.img`.

**Fix (easy, done)**: `vt.handoff=7` ensures Plymouth gets clean VT handoff.
`rd.plymouth=1` starts Plymouth in initrd stage, not after pivot.

---

### 2. COMPOSITOR FLASH (teal/gray before Firefox)
**Cause**: `mutter --wayland --no-x11` shows its default compositor background
(a gray/teal gradient) for 500ms–2s while Firefox is starting up and rendering
its first frame. Not GDM — GDM is masked. This IS mutter.

**Fix (easy)**: Write zeros to `/dev/fb0` in `kiosk-session.sh` before launching
mutter. The framebuffer stays black through compositor init until Firefox paints.

```bash
dd if=/dev/zero of=/dev/fb0 bs=4M 2>/dev/null || true
```

**Fix (better)**: Replace mutter with **cage** — a Wayland compositor built for
kiosks. No background, no shell, just the app. Starts in ~50ms.
`cage -- firefox --kiosk http://127.0.0.1:8080`

**Fix (alternative)**: Use **sway** with a black background and `exec firefox`.
sway is a full wlroots compositor — cleaner startup than mutter for single-app use.

---

### 3. FIREFOX TAB BAR FLASH
**Cause**: Firefox renders one frame with the full browser chrome visible before
`--kiosk` flag suppresses it. Also affects the white flash on first paint.

**Fix**: Pre-create a Firefox profile (`~liveuser/.mozilla/firefox/kiosk/`) with:
- `chrome/userChrome.css` — hides `#TabsToolbar`, `#nav-bar` at CSS level
  (takes effect before first paint, no flash)
- `user.js` — sets `browser.display.background_color=#000000`, disables telemetry
- Pass `--profile /home/liveuser/.mozilla/firefox/kiosk` to Firefox

---

### 4. CONNECTION REFUSED (127.0.0.1:8080)
**Cause**: `kiosk-piece-server.service` is a systemd service in `multi-user.target`.
Getty autologin fires at roughly the same time. Race condition: sometimes
`kiosk-session.sh` starts before the Python server is listening.

**Fix**: Start the piece server inline in `kiosk-session.sh` instead of relying
on the systemd service. The 30s wait loop then handles the ~100ms startup time.

```bash
python3 /usr/local/bin/kiosk-piece-server.py &
```

---

## Alternative Compositors

| Compositor | In Fedora 43 repos | Kiosk suitability | Notes |
|---|---|---|---|
| **mutter** | ✓ (installed) | Medium | GNOME-native, heavy, shows gray bg |
| **cage** | ✗ (not in repos) | **Best** | Purpose-built kiosk, no bg, 50ms start |
| **sway** | unclear | Good | wlroots, black bg config, active |
| **labwc** | unclear | Good | Lightweight wlroots openbox-style |
| **weston** | ✗ (not installed) | Medium | Reference compositor, kiosk plugin |
| **gnome-kiosk** | check repos | Good | Simplified gnome-shell for kiosk |

### Recommended: Build `cage` as a static binary
```
cage v0.2.0 (github.com/nicowillis/cage)
Dependencies: wlroots, wayland, pixman — can be musl-statically linked
Size: ~200KB static binary
Drop into rootfs at /usr/local/bin/cage
```

Alternatively, `dnf install cage` if it lands in Fedora 44.

---

## Faster Boot Strategies

### Already applied
- `loglevel=1` — suppresses kernel messages
- `mitigations=off` — disables CPU security mitigations (~200ms saved)
- `vt.handoff=7` — clean VT handoff to Plymouth
- 15 services masked (abrt, avahi, cups, ModemManager, sssd, etc.)
- GDM eliminated (was starting gnome-shell greeter)

### Not yet applied
- **`rd.udev.log-level=3`** — reduce udev logging noise
- **`systemd.show_status=0`** — suppress status messages
- **Disable SELinux**: `selinux=0` in kernel cmdline — saves ~300ms relabeling
- **Disable NetworkManager**: `rd.neednet=0` — we don't need network
- **Plymouth `--show-splash` immediately** — already ShowDelay=0
- **initrd framebuffer hook** — show PALS logo/black during BLACK #1
- **`zstd` EROFS compression** instead of `lzma` — 3x faster decompression at ~10% larger size

### Boot time breakdown (estimated)
```
BIOS:         1-3s  (hardware dependent, can't improve)
GRUB:         ~0s   (timeout=0)
Kernel+initrd: 3-5s (can improve with zstd EROFS, rd.neednet=0)
systemd:       2-4s (can improve by masking more services)
mutter+FF:     2-4s (can improve with cage + fb0 paint)
Page load:    <1s   (localhost, already fast)
───────────────────
Total:        8-17s (currently), target: 6-10s
```

---

## Roadmap

### ✅ Done (this build)
- [x] GDM eliminated — getty autologin replaces display manager entirely
- [x] 15 slow services masked (abrt, avahi, cups, ModemManager, sssd, thermald...)
- [x] Inline piece server in `kiosk-session.sh` — fixes connection race
- [x] `/dev/fb0` painted black before mutter — eliminates compositor flash
- [x] Firefox kiosk profile with `userChrome.css` — eliminates tab bar flash
- [x] `selinux=0 rd.neednet=0 systemd.show_status=0` in GRUB cmdline
- [x] `loglevel=1 mitigations=off vt.handoff=7` kernel flags

### 🔜 Short Term (next USB build)
- [x] **lz4hc EROFS compression** — switched from `-zlzma` to `-zlz4hc`.
  ~25% larger image (2.0GB → 2.5GB) but 3× faster decompression.
- [ ] **Black GRUB background image** — NOTE: with `timeout=0` GRUB never renders
  the menu/background (boots instantly before gfxterm draws). Only useful if
  timeout ≥ 1. Skipping for now.
- [x] **`rd.udev.log-level=3`** in GRUB cmdline — quieter udev init
- [ ] **Disable more systemd units** — audit `systemctl list-units` on a booted
  live system to find any remaining slow services
- [ ] **Pre-warm Firefox profile** — run Firefox headless once in the rootfs build
  step to generate pre-compiled startup cache files (`startupCache/`, `*shelljit*`)

### 🔧 Medium Term (requires new tooling)
- [x] **`cage` compositor** — installed via `dnf install cage` (available in Fedora 43).
  Zero background, no shell, 50ms start. Replaces mutter entirely.
  ```bash
  exec cage -- firefox --kiosk --no-remote http://127.0.0.1:8080
  ```
- [ ] **`gnome-kiosk` compositor** — simplified GNOME shell for kiosk. Check if
  available: `dnf info gnome-kiosk`. Handles blank background and single-app
  session natively.
- [x] **Piece server as socket-activated unit** — systemd socket activation via
  `kiosk-piece-server.socket`. Port 8080 bound before server process starts.
  Python server updated to accept `LISTEN_FDS` socket from systemd.

### 🌟 Long Term (deeper surgery)
- [x] **initrd framebuffer hook** — appended cpio to `pals-initrd.img` adds a
  systemd unit (`kiosk-fb.service`) that fills `/dev/fb0` with zeros after
  `dracut-pre-trigger.service`. Eliminates BLACK #1 before Plymouth.
  (Uses dracut-systemd unit injection via concatenated cpio, not shell hooks.)
- [ ] **initrd framebuffer PALS logo** — extend the fb0 hook to blit the actual
  PALS logo (pre-converted PNG→raw BGRA) to center of screen instead of solid black. Options:
  - Solid PALS purple fill (trivial — write 4-byte pattern via Python)
  - Full PALS logo (pre-convert PNG to raw BGRA, embed in initrd, blit to center)
  ```bash
  # /usr/lib/dracut/hooks/pre-udev/00-kiosk-fb.sh
  python3 -c "
  import struct
  with open('/dev/fb0','wb') as f:
      # read resolution from /sys/class/graphics/fb0/virtual_size
      sz = open('/sys/class/graphics/fb0/virtual_size').read().strip().split('x')
      px = struct.pack('BBBB',0,0,0,255)  # BGRA black
      f.write(px * int(sz[0]) * int(sz[1]))
  " 2>/dev/null || true
  ```
- [ ] **Custom Plymouth plugin** that transitions to a solid-color fill before
  handing off to the compositor, so there's no color change at all.
- [ ] **Remove Plymouth entirely** (`rd.plymouth=0`, no splash) — go straight
  from kernel to kiosk. Saves ~500ms. Black screen the whole way, then piece.
  Clean aesthetic if the boot is fast enough (target: under 8s total).
- [ ] **Custom minimal initrd** — instead of full dracut, a hand-crafted initrd
  with only dmsquash-live + what's needed. Saves 1-2s of initrd processing.
- [ ] **Piece server in Rust** — replace Python HTTP server with a ~1MB static
  Rust binary. Starts in <10ms vs ~200ms for Python. Also handles larger pieces.
