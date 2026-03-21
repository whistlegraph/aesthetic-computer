# FedAC Kiosk USB — Boot Report

**Date:** 2025-02-24
**USB:** 14.5G (toss piece, EROFS lzma, Fedora 43 live)

---

## Current Status: Black Screen in Firefox

Firefox boots and runs (F10 opens bookmarks, cursor visible), but **all page content is black** — including a plain HTML diagnostic page.

### Root Cause: `firefox.cfg` Color Override

```js
// These two lines together = black text on black background
defaultPref("browser.display.background_color", "#000000");
defaultPref("browser.display.document_color_use", 2);
// document_color_use=2 means "ALWAYS override page colors with user colors"
// We set background to black but never set foreground → defaults to black
// Result: ALL page content invisible (black on black)
```

Browser chrome (bookmarks, toolbar) still renders because it uses its own color scheme — only web page content is affected by `document_color_use`.

### Fix

Remove `document_color_use = 2` (let pages use their own CSS colors). Keep the black background for flash prevention but don't override page foreground colors.

### If Still Black After Fix

Escalation path:
1. Add `LIBGL_ALWAYS_SOFTWARE=1` to kiosk-session.sh (force software GL rendering)
2. Add `gfx.webrender.force-disabled=true` to firefox.cfg (disable WebRender)
3. Simplify firefox.cfg to absolute minimum (rule out autoconfig parsing errors)
4. Enable SSH on kiosk for remote debugging via port 9222

---

## Boot Chain (All Working)

```
UEFI POST
└─ GRUB (timeout=0, no Plymouth)
   └─ Kernel 6.17.1-300.fc43.x86_64 (loglevel=7)
      └─ initrd (dracut + dmsquash-live)
         └─ mounts EROFS (lzma, 2.1GB) via overlayfs
            └─ systemd → multi-user.target
               └─ getty@tty1 autologin → liveuser
                  └─ .bash_profile → kiosk-session.sh
                     └─ cage (Wayland kiosk compositor) ✅
                        └─ Firefox --fullscreen ✅
                           └─ file:///...piece.html → BLACK ❌
```

---

## USB Layout

```
sda1 (400MB FAT32 "BOOT")
├── EFI/BOOT/grub.cfg
└── loader/{linux, initrd}

sda2 (ext4 "FEDAC-LIVE")
├── LiveOS/squashfs.img  (2.1GB EROFS lzma)
└── logs/                (persistent, empty — log services not firing)
```

## Key EROFS Files

| Path | Purpose |
|------|---------|
| `/usr/local/bin/kiosk-session.sh` | cage + Firefox launch |
| `/usr/local/share/kiosk/piece.html` | toss bundle (599KB) |
| `/usr/local/share/kiosk/diag.html` | diagnostic page |
| `/usr/lib64/firefox/distribution/policies.json` | Firefox admin policies |
| `/usr/lib64/firefox/firefox.cfg` | Firefox autoconfig prefs |
| `/usr/lib64/firefox/defaults/pref/autoconfig.js` | Loads firefox.cfg |
| `/etc/systemd/system/getty@tty1.service.d/autologin.conf` | liveuser autologin |
| `/home/liveuser/.bash_profile` | Runs kiosk-session.sh on tty1 |
| `/usr/bin/cage` | Wayland kiosk compositor |

## Current kiosk-session.sh

```bash
#!/bin/bash
export XDG_SESSION_TYPE=wayland
export XDG_RUNTIME_DIR="/run/user/$(id -u)"
export MOZ_ENABLE_WAYLAND=1
exec > /tmp/kiosk.log 2>&1
exec cage -- \
  firefox --fullscreen --no-remote \
  --remote-debugging-port=9222 \
  --profile /home/liveuser/.mozilla/firefox/kiosk \
  file:///usr/local/share/kiosk/piece.html
```

---

## Resolved Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| Boot shuts down | GRUB `gfxmode=auto` + `terminal_output gfxterm` | Switched to `terminal_output console` |
| Plymouth holds VT | Masked `plymouth-quit.service` | Unmasked + drop-in with `--retain-splash` |
| HTTPServer crash | `__new__` bypassed `BaseServer.__init__` | `PreBoundHTTPServer` subclass |
| Firefox loads wrong URL | policies.json hardcoded `http://127.0.0.1:8080` | Changed to `file:///` |
| PALS logo missing | Watermark in rootfs Plymouth, but initrd shows Fedora logo | Needs initrd rebuild (deferred) |
| Log services empty | kiosk-log-dump at shutdown.target — may not fire on crash | Needs early debug service |

---

## Roadmap

### Immediate
- [ ] Fix `document_color_use` black-on-black issue
- [ ] Verify piece.html renders in Firefox on kiosk
- [ ] Enable SSH on kiosk for remote debugging

### Short Term
- [ ] Re-enable Plymouth with PALS logo
- [ ] Pre-warm Firefox profile (startup cache)
- [ ] Fix log dump services (early boot + shutdown)
- [ ] Switch EROFS to zstd compression (faster decompression)

### Long Term
- [ ] initrd framebuffer hook (PALS logo before Plymouth)
- [ ] Custom minimal initrd (strip unused dracut modules)
- [ ] Piece server in Rust (replace Python, <10ms startup)
