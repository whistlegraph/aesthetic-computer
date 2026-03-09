# fedac auto-updater

Self-healing USB reflash from a live production image, triggered from within the running system.

## Is it possible?

Yes. The Linux kernel + initramfs are fully loaded into RAM within the first few seconds of boot.
After that, the USB's EFI partition is not locked — it can be mounted, written to, and unmounted
freely while the system is running. Overwriting `EFI/BOOT/BOOTX64.EFI` on the live USB is safe
because the running kernel lives entirely in RAM.

This means the device can download a new kernel+initramfs image over WiFi and flash itself,
then reboot — no external computer required.

---

## Architecture

### Hosted artifact

At `releases.aesthetic.computer/os/` publish a signed build on each deploy:

```
native-notepat-latest.vmlinuz   — raw kernel+initramfs EFI image (the bzImage we already produce)
native-notepat-latest.sha256    — checksum for verification
native-notepat-latest.version   — plain text version string, e.g. "2026-03-09.ba8c42d"
```

The build pipeline (`build-and-flash.sh`) uploads these after a successful build.

### Device-side flow (triggered by "os" button)

1. **OS panel opens** (fullscreen, like WiFi panel) — shows current version + checks remote version
2. User taps **"update"** — or it can auto-update silently if version differs
3. `system.fetch("https://releases.aesthetic.computer/os/native-notepat-latest.version")` → compare
4. If newer: `system.fetch` the `.vmlinuz` binary → save to `/tmp/vmlinuz.new` via `system.writeFile`
5. C-side update function (`system_update_flash()`):
   - `mount /dev/sda1 /tmp/efi -t vfat`
   - `cp /tmp/vmlinuz.new /tmp/efi/EFI/BOOT/BOOTX64.EFI`
   - `sync && umount /tmp/efi`
   - `rm /tmp/vmlinuz.new`
6. TTS: "update complete, rebooting in three seconds"
7. `reboot` (via `system.reboot()` → C `system("reboot")`)

### Version tracking

Embed the git hash + build timestamp in the kernel commandline or initramfs
(already done: `AC_GIT_HASH` and `AC_BUILD_TS` macros in the Makefile).
At runtime, `system.version` exposes this string. The OS panel can compare
it against the remote `.version` file.

---

## OS button panel (UI)

Tap "os" in the status bar → opens a fullscreen panel similar to the WiFi panel:

```
┌─────────────────────────────────────────────┐
│  aesthetic.computer / native                │
│                                             │
│  current:   2026-03-09 · ba8c42d           │
│  available: 2026-03-10 · f3a1b2c  ← new   │
│                                             │
│  [ update now ]                             │
│                                             │
│  ████████░░░░░░░░  downloading...  58%      │
│                                             │
│  releases.aesthetic.computer/os/            │
└─────────────────────────────────────────────┘
```

States: idle → checking → up-to-date / update-available → downloading → flashing → rebooting

---

## Implementation steps

### 1. Build pipeline — upload artifacts
- After `build-and-flash.sh` succeeds, `scripts/upload-release.sh`:
  - `curl -T build/vmlinuz "https://releases.aesthetic.computer/os/native-notepat-latest.vmlinuz"`
  - Write `.sha256` and `.version` files alongside it
- Host on Digital Ocean Spaces (CDN, already used for assets)

### 2. C: `system.version` binding (`js-bindings.c`)
- Expose `AC_GIT_HASH` and `AC_BUILD_TS` as `system.version` string

### 3. C: `system.fetchBinary(url, path)` (`js-bindings.c` + `fetch.c`)
- Streaming download via curl subprocess: `curl -L -o <path> <url>`
- Progress reported back via `system.fetchProgress` (0.0–1.0) polled each frame

### 4. C: `system.flashUpdate(path)` (`js-bindings.c`)
- Mount, copy, sync, umount sequence
- Runs in background thread; signals JS when done via `system.flashDone`

### 5. C: `system.reboot()` (`js-bindings.c`)
- `system("reboot")` — already have `system()` access via wifi.c pattern

### 6. JS: OS panel in `notepat.mjs`
- `osUrlVisible` → replace with `osPanelOpen` state machine
- Fetch remote version on open, compare, render panel, handle touch on "update now"
- Progress bar driven by `system.fetchProgress`

---

## Safety

- **Checksum verify**: compare `.sha256` before flashing; abort if mismatch
- **Size sanity**: reject images < 10MB or > 80MB
- **WiFi required**: disable update button when not connected
- **No partial writes**: download fully to `/tmp` first, then flash atomically
- **Reboot delay**: 3-second TTS countdown so user isn't surprised

---

## Auto-update (background, no UI)

Optional future mode: on WiFi connect, silently check version, download in background,
flash and reboot at idle (no notes held, no active voices, past midnight).
Could be gated by a setting in `wifi_creds.json`: `"autoUpdate": true`.

---

## Files to touch

| File | Change |
|------|--------|
| `fedac/native/src/js-bindings.c` | `system.version`, `system.fetchBinary`, `system.flashUpdate`, `system.reboot` |
| `fedac/native/src/ac-native.c` | wire new bindings, background thread for download |
| `fedac/native/pieces/notepat.mjs` | OS panel UI, version check, download/flash flow |
| `fedac/native/scripts/upload-release.sh` | new script: upload artifacts to DO Spaces |
| `fedac/native/scripts/build-and-flash.sh` | call upload-release.sh after successful build |
| `fedac/native/Makefile` | no changes needed |
