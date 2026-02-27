# FedAC Sub-1GB OS Image Feasibility Report

**Date**: 2026-02-27
**Current image size**: ~4GB
**Goal**: Under 1GB (ideally 300-500MB)

---

## Why It's 4GB Right Now

The FedAC kiosk starts with a **Fedora 43 Workstation Live ISO** (~6.4GB uncompressed rootfs) and strips ~2.5GB of bloat (LibreOffice, Java, GNOME apps, locales, docs, etc.). What remains is still ~3.9GB uncompressed because Fedora carries:

| Component | Size | Needed? |
|---|---|---|
| GNOME Shell + Mutter | 200-300MB | No (we use Cage) |
| GDM | 50MB | No (tty1 autologin) |
| Python 3 + site-packages | 150-200MB | Only for piece server |
| dnf/rpm database + tools | 50-80MB | No (read-only image) |
| NetworkManager | 30-40MB | No (offline kiosk) |
| Kernel modules (broad) | 200-400MB | Most unnecessary |
| Remaining firmware | 100-150MB | Can be trimmed further |
| GLib/GTK/GNOME platform libs | 200-300MB | Partially needed |
| SELinux policy + tools | 30-50MB | No (disabled) |
| Perl | 50-80MB | No |
| systemd full stack | 80-100MB | Mostly unnecessary |

**Dead weight after stripping: ~1.2-1.8GB.** Fedora is fundamentally a desktop OS — stripping it down to kiosk size is fighting the distro's design.

With EROFS compression (2-3x ratio), 3.9GB becomes ~1.3-2.0GB compressed. Getting under 1GB from Fedora would require removing so much that you'd effectively be rebuilding the distro from scratch.

---

## The Alternatives

### Alpine Linux + Chromium/Firefox (RECOMMENDED)

**Target: 300-400MB total image.**

Alpine is a musl-based distro designed for containers and embedded use. It's the most proven path for minimal browser kiosks — deployed in hospitals, digital signage, and retail (40+ machine deployments documented as of April 2025).

```
Estimated package breakdown (uncompressed):
  alpine-base + busybox + openrc     ~  40MB
  linux-lts kernel                   ~ 120MB
  firmware (Intel i915 + AMD only)   ~  50MB
  mesa-dri-gallium + mesa-egl        ~  80MB
  chromium                           ~ 250MB
  cage + wlroots                     ~   5MB
  pipewire + wireplumber             ~  30MB
  alsa-lib                           ~  10MB
  one Latin font                     ~   5MB
  dbus + eudev                       ~  15MB
                                     --------
  Total uncompressed:                ~ 605MB
  EROFS compressed (~2.5x):          ~ 240-300MB
```

**Partition layout:**
```
EFI:   32MB  FAT32  (GRUB — no theme, just binary + config)
PIECE: 10MB  ext4   (piece.html + piece-app.html)
LIVE:  ~300MB EROFS  (Alpine rootfs)
TOTAL: ~350MB
```

**That's 10x smaller than current.** And it's not theoretical — people have fit Chromium + X11 Alpine kiosks onto 256MB SD cards.

**Pros:**
- musl libc is dramatically smaller than glibc
- apk package manager is ~1MB (vs dnf's 50MB+)
- Cage, Firefox, Chromium, PipeWire all in Alpine repos
- 3.2-second boot demonstrated on Intel Alder Lake
- Runs from RAM (diskless mode possible)

**Cons:**
- musl can cause subtle issues with some binary blobs (mitigated by using Alpine-native packages)
- Slightly smaller community than Fedora for troubleshooting

**Risk: LOW.** Best documented path for this exact use case.

---

### Debian Minimal (debootstrap)

**Target: 500-600MB total image.**

Start from `debootstrap --variant=minbase`, add only what's needed. Uses glibc (full compatibility), slightly larger.

```
debootstrap minbase          ~ 220MB
chromium + deps              ~ 390MB
cage + wlroots               ~   5MB
pipewire                     ~  30MB
mesa + libdrm                ~  80MB
linux-image kernel           ~ 200MB
firmware (intel + amd)       ~  50MB
                             --------
Total uncompressed:          ~ 975MB
EROFS compressed:            ~ 450-550MB
```

**Total image: ~500-600MB.** Comfortably under 1GB.

**Pros:** Largest package repo. glibc = zero compatibility risk. Extremely stable.
**Cons:** ~2x the size of Alpine. More manual build setup.
**Risk: LOW.**

---

### Buildroot (Custom Embedded Linux)

**Target: 300-500MB but impractical.**

Theoretically optimal — you build exactly what you need. But Chromium is notoriously difficult to cross-compile with Buildroot (12+ hours, 45GB+ build space). Most Buildroot kiosk projects use WPE WebKit instead, which has incomplete Web Audio and WebGL.

**Risk: HIGH.** Not worth the engineering cost unless you switch to WebKit.

---

### Current Fedora With More Stripping

**Target: 1.0-1.5GB (can't reach sub-1GB).**

Could still remove GNOME Shell, Mutter, GDM, Python, Perl, dnf, SELinux, more kernel modules. Saves ~1.0-1.6GB additional, bringing compressed image to ~1.0-1.5GB. Diminishing returns — each removal risks breaking subtle Fedora dependencies.

**Risk: MEDIUM-HIGH.** Won't reach sub-1GB.

---

### Disqualified Options

| Distro | Why Not |
|---|---|
| **ChromeOS Flex** | 6GB+ installed, requires Google Admin Console, can't serve local HTML |
| **Tiny Core** | Browser packages are severely outdated, no WebGL guarantee |
| **Chimera Linux** | Too immature (no stable 1.0 yet), no kiosk community |
| **Void Linux** | No kiosk community, Chromium not in official repos |

---

## GPU Firmware: What's Actually Needed

| Firmware | Installed Size | Notes |
|---|---|---|
| Intel i915 (all gens) | 124MB | Can trim to ~30-40MB for recent gens only |
| AMD amdgpu | 26MB | Already lean |
| AMD radeon (legacy) | 2MB | Optional |
| NVIDIA nouveau | 104MB | Drop entirely unless NVIDIA support needed |

**Trimmed firmware budget: ~60-70MB** (Intel recent + AMD only).

---

## Recommendation

**Switch to Alpine Linux.** Here's why:

1. **10x size reduction** (4GB -> 350MB) is achievable and proven
2. **Faster boot** (3-5 seconds vs current 15-30 seconds)
3. **Simpler to maintain** — `apk add` is cleaner than stripping Fedora
4. **Same browser, same audio, same compositor** (Chromium/Firefox + PipeWire + Cage)
5. **Runs from RAM** — entire rootfs loads into memory for instant file access

### Migration Plan

**Phase 1 (2-3 days):** Build Alpine + Chromium + Cage + PipeWire prototype. Validate piece HTML renders correctly with WebGL + Web Audio on Intel/AMD hardware.

**Phase 2 (1-2 days):** Replace Fedora ISO extraction in `make-kiosk-piece-usb.sh` with Alpine rootfs builder (`apk --root`). Port kiosk overlays (kiosk-session.sh, piece server, Plymouth/splash).

**Phase 3 (1 day):** Optimize — trim firmware, strip kernel modules, tune EROFS compression. Target 300-350MB final image.

### Size Comparison

```
Current (Fedora):    ████████████████████████████████████████  4000MB
Fedora stripped max: ████████████████████████████               1200MB
Debian minimal:      ██████████████████                          550MB
Alpine + Chromium:   ████████                                    350MB
Alpine theoretical:  ██████                                      250MB
```

---

## References

- [Alpine RPi Kiosk (256MB image)](https://github.com/kyoushuu/alpine-rpi-kiosk)
- [Alpine Minimal Kiosk Setup (2024)](https://dev.to/nesterow/setup-minimal-kiosk-environment-with-alpine-linux-27b)
- [Alpine Cage Kiosk (April 2025)](https://giuliomagnifico.blog/post/2025-04-24-minipc-kiosk/)
- [Porteus Kiosk (~300MB)](https://porteus-kiosk.org/)
- [Debian Kiosk Installer](https://github.com/josfaber/debian-kiosk-installer)
- [Cage Kiosk Compositor](https://github.com/cage-kiosk/cage)
- [Fedora Firmware Minimization](https://fedoraproject.org/wiki/Changes/Linux_Firmware_Minimization)
