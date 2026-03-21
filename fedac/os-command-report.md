# `os` Command — Bootable Piece OS Builder

**Status:** Proposal / Roadmap
**Date:** 2025-02-24
**Author:** @jeffrey + Claude

---

## Summary

Build an `os` command into the AC prompt that takes any piece name, injects it into a pre-baked Fedora base image, and produces a downloadable `.iso` file anyone can flash to USB with Balena Etcher.

```
aesthetic.computer/os:notepat
```

Downloads: `notepat-os.iso` (~2.8 GB)
Flash with Etcher → boot → PALS splash → notepat fullscreen.

---

## Current State

Today the kiosk USB build is a manual process:

```bash
sudo bash fedac/scripts/make-kiosk-piece-usb.sh notepat /dev/sda
```

This script:
1. Fetches the piece bundle from oven `/pack-html?piece=notepat`
2. Extracts Fedora Workstation Live ISO (~2.4 GB) into a rootfs (~9 GB uncompressed)
3. Injects kiosk config: cage compositor, Firefox autoconfig, volume keys, Plymouth, systemd units
4. Compresses rootfs to EROFS with LZMA (~2.7 GB)
5. Partitions the USB, writes EFI boot + EROFS
6. Total time: ~15-20 minutes on a fast machine

**Problems with current approach:**
- Requires Linux, root access, and a physical USB drive
- Full rootfs extraction + EROFS compression is slow and disk-hungry (~12 GB temp)
- Not accessible to anyone without CLI skills

---

## Architecture: Pre-Baked Base Image + Piece Injection

### Key Insight

The rootfs extraction, kiosk config injection, and EROFS compression are **identical for every piece**. The only variable is the 2-5 MB piece bundle (`piece.html`). We can pre-bake everything once and store a "hollowed" base image on CDN, then inject the piece at download time.

### Two-Layer Image Format

```
┌──────────────────────────────────┐
│  Layer 1: Base Image (pre-baked) │  ~2.7 GB EROFS, stored on CDN
│  ┌────────────────────────────┐  │
│  │ Fedora 43 rootfs           │  │
│  │ cage, Firefox, PipeWire    │  │
│  │ kiosk-session.sh           │  │
│  │ volume key daemon          │  │
│  │ PALS Plymouth + fb splash  │  │
│  │ /usr/local/share/kiosk/    │  │
│  │   └── piece.html ← PLACEHOLDER │
│  └────────────────────────────┘  │
├──────────────────────────────────┤
│  Layer 2: Piece Bundle           │  ~2-5 MB, generated per request
│  └── piece.html (from oven)     │
└──────────────────────────────────┘
```

### Injection Strategy: Preallocated Slot in EROFS

EROFS is read-only, so we can't modify it in-place. Two approaches:

**Option A — Hybrid ISO with overlay partition**
- Base EROFS has a placeholder `piece.html` (1 byte)
- A small second partition (ext4, 20 MB) holds the real `piece.html`
- `kiosk-session.sh` mounts the overlay partition and symlinks/bind-mounts the piece
- At download time: concatenate base ISO + piece partition
- Pros: Fast (~seconds), base image never rebuilt
- Cons: Slightly more complex boot chain

**Option B — EROFS with preallocated padding (recommended)**
- Build base EROFS with a dummy `piece.html` padded to exactly 8 MB (zeros)
- Record the byte offset of `piece.html` within the EROFS image
- At download time: seek to that offset, overwrite with real piece bundle
- Pros: Single file, simple, no boot chain changes
- Cons: Requires EROFS internals knowledge, fragile if file layout changes

**Option C — Full ISO assembly on server**
- Store pre-built components: EFI partition, kernel, initrd, base EROFS
- At download time: build EROFS from a cached rootfs snapshot + the new piece
- Pros: Clean, no hacks
- Cons: Requires rootfs snapshot on server (~9 GB), EROFS build takes ~5 min

### Recommended: Option A (Hybrid ISO)

Most practical balance of simplicity and speed:

```
fedac-base.iso (pre-baked, ~2.8 GB, stored on DO Spaces CDN):
  Partition 1: EFI (400 MB FAT32) — GRUB + kernel + initrd
  Partition 2: FEDAC-LIVE (ext4) — squashfs.img (EROFS)
  Partition 3: FEDAC-PIECE (ext4, 20 MB) — empty, for piece injection

At download time (oven server):
  1. Stream fedac-base.iso from CDN
  2. Fetch piece bundle from /pack-html
  3. Write piece.html into partition 3
  4. Stream modified ISO to user
```

---

## Implementation Plan

### Phase 1: Pre-Baked Base Image

**Build script changes** (`make-kiosk-piece-usb.sh`):
- Add `--base-image` flag that builds the base without a specific piece
- Uses a minimal placeholder `piece.html` (`<html><body>loading...</body></html>`)
- Adds a third 20 MB ext4 partition (`FEDAC-PIECE`)
- Outputs `fedac-base-{date}.iso` (~2.8 GB)
- Upload to Digital Ocean Spaces CDN

**Boot chain change** (`kiosk-session.sh`):
```bash
# Mount piece overlay if available
PIECE_PART=$(blkid -L FEDAC-PIECE 2>/dev/null || true)
if [ -n "$PIECE_PART" ] && [ -f "$PIECE_PART_MOUNT/piece.html" ]; then
  mount -o ro "$PIECE_PART" /mnt/piece
  ln -sf /mnt/piece/piece.html /usr/local/share/kiosk/piece.html
fi
```

**Automated rebuilds:**
- GitHub Action: rebuild base image weekly or on fedac/ changes
- Store on DO Spaces: `https://assets.aesthetic.computer/os/fedac-base-latest.iso`
- Keep last 3 versions for rollback

### Phase 2: Oven `/os` Endpoint

**New route in `oven/server.mjs`:**

```javascript
app.get('/os', async (req, res) => {
  const piece = req.query.piece || req.query.code;
  if (!piece) return res.status(400).send('Missing piece parameter');

  // 1. Fetch piece bundle
  const bundle = await createJSPieceBundle(piece, ...);
  // or createBundle() for KidLisp

  // 2. Stream base ISO from CDN
  const baseUrl = 'https://assets.aesthetic.computer/os/fedac-base-latest.iso';
  const baseStream = await fetch(baseUrl);

  // 3. Inject piece into partition 3
  // The base ISO has a known layout:
  //   - Partition 3 starts at a fixed offset (recorded at build time)
  //   - Write piece.html to the ext4 filesystem in partition 3

  // 4. Stream to user
  res.setHeader('Content-Type', 'application/octet-stream');
  res.setHeader('Content-Disposition', `attachment; filename="${piece}-os.iso"`);
  // ... stream modified ISO
});
```

**Technical detail — partition injection:**
The oven server needs to write a file into an ext4 partition within the ISO. Options:
- Use `e2fsprogs` (`debugfs`) to inject a file into ext4 without mounting
- Use a pre-formatted ext4 image and just `dd` the piece into a known offset
- Simplest: pre-format a 20 MB ext4 image at build time with a single file slot

### Phase 3: AC Prompt Piece (`disks/os.mjs`)

```javascript
// system/public/aesthetic.computer/disks/os.mjs
function boot({ params, net, jump }) {
  const piece = params[0] || "prompt";
  const url = `${net.apiUrl}/api/os?piece=${encodeURIComponent(piece)}`;
  jump("out:" + url); // triggers download
}

export const desc = "Download a bootable OS image for any piece.";
export { boot };
```

**Netlify redirect** (`netlify.toml`):
```toml
[[redirects]]
  from = "/api/os"
  to = "https://oven.aesthetic.computer/os"
  status = 200
```

### Phase 4: Progress UI

Since ISO assembly takes 10-30 seconds, add a progress page:

```
aesthetic.computer/os:notepat
  ┌──────────────────────────────┐
  │                              │
  │      🎵 notepat OS           │
  │                              │
  │   Building your image...     │
  │   ████████████░░░░░  67%     │
  │                              │
  │   Fetching piece bundle...   │
  │   Assembling ISO...          │
  │                              │
  └──────────────────────────────┘
```

Use SSE (Server-Sent Events) for progress, similar to the existing `track-media-stream` endpoint.

---

## Infrastructure Requirements

| Component | Current | Needed |
|-----------|---------|--------|
| Oven server (Fly.io) | 1 GB RAM, shared CPU | 2 GB RAM, 10 GB ephemeral disk |
| DO Spaces CDN | Assets storage | + base ISO storage (~3 GB) |
| Build runner | Manual (ThinkPad) | GitHub Action or dedicated VM |
| `e2fsprogs` on oven | Not installed | `debugfs` for ext4 injection |

**Cost estimate:**
- DO Spaces: ~$0.02/GB/month storage + $0.01/GB transfer = ~$0.10/month base + $0.03/download
- Fly.io upgrade: ~$5/month for larger VM
- Total: ~$5-10/month for moderate usage

---

## File Sizes

| Component | Size |
|-----------|------|
| Fedora base rootfs (uncompressed) | ~9 GB |
| EROFS image (LZMA compressed) | ~2.7 GB |
| EFI partition (kernel + initrd + GRUB) | ~400 MB |
| Piece partition (ext4) | 20 MB |
| **Total ISO** | **~3.1 GB** |
| Piece bundle (typical) | 2-5 MB |

---

## User Experience

```
# From any browser:
aesthetic.computer/os:notepat

# From AC prompt:
> os notepat

# What happens:
1. Page shows "Building notepat OS..." with progress
2. Oven fetches piece bundle (~2s)
3. Oven streams base ISO + injects piece (~10-30s)
4. Browser downloads notepat-os.iso (~3.1 GB)
5. User opens Balena Etcher, selects ISO, flashes to USB
6. Boot from USB → PALS splash → notepat fullscreen
```

---

## Security Considerations

- ISO downloads should be rate-limited (expensive bandwidth)
- Piece code runs in Firefox sandbox (same as browser)
- No user data on the USB (live system, RAM-only)
- Consider signing ISOs with GPG for verification

---

## Timeline

| Phase | Effort | Dependencies |
|-------|--------|-------------|
| 1. Pre-baked base image | 2-3 days | Current build script works |
| 2. Oven `/os` endpoint | 3-5 days | Base image on CDN |
| 3. AC prompt piece | 1 day | Oven endpoint working |
| 4. Progress UI | 1-2 days | SSE infrastructure exists |
| **Total** | **~1-2 weeks** | |

---

## Future Extensions

- **`os:@handle/piece`** — build OS from published user pieces
- **Custom branding** — per-piece boot splash (piece's preview as Plymouth theme)
- **ARM support** — Raspberry Pi images (aarch64 Fedora base)
- **Minimal base** — strip Fedora to ~1 GB (remove LibreOffice, GNOME apps, etc.)
- **Delta updates** — only download the piece partition for repeat builds
- **Network boot (PXE)** — boot from LAN without USB
