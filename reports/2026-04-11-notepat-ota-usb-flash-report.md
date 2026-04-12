# 2026-04-11 — notepat OTA build and USB flash report

Session focus: ship the async DJ USB mount check and Notepat live-percussion
work as a fresh OTA build, then get that exact OTA image onto the USB with
Jeffrey's inscribed credentials and preserved Wi-Fi seeds.

Final release published in oven:

- Release: `dynamic-milksnake`
- Commit: `848d3231a`
- Build stamp: `848d3231a-2026-04-11T21:30`
- Oven job: `fff0bd10-0`

---

## 1. What shipped

This build includes the native change that makes the DJ music-USB probe
non-blocking:

- `system.mountMusic()` now schedules its probe off-thread instead of blocking
  the main JS/render path
- JS callers now consume cached mount state via
  `system.mountMusicMounted` / `system.mountMusicPending`
- This closes the periodic crawl caused by synchronous USB polling during play

Work was committed and pushed as:

- Commit: `848d3231a5c5126cf2df1f731fd0d293d7a62350`
- Message: `native: make dj usb mount check async`

---

## 2. OTA build result

Manual oven trigger was required because the native poller was not picking the
build up automatically at the time of the session.

Published artifacts included:

- `os/releases.json`
- `os/native-notepat-latest.vmlinuz`
- `os/native-notepat-latest.vmlinuz-slim`
- `os/native-notepat-latest.initramfs.cpio.gz`

OTA publication succeeded and `ac-os pull` was able to download and verify the
new release payload.

---

## 3. Standard flash-path failure

The normal `./fedac/native/ac-os pull` flash path did not complete on this host.

Observed failure:

- partitioning completed, but kernel partition-table reread stayed busy
- `mkfs.vfat` on `/dev/sda1` retried repeatedly and failed with
  `Device or resource busy`
- direct host-side reread attempts (`blockdev --rereadpt`, `partx -u`) were
  also blocked

This meant the OTA payload was downloaded correctly, but the standard helper
could not safely move from partitioning into filesystem creation on the live
USB device.

---

## 4. Raw-image fallback

To bypass the host's live partition-reread problem, the USB was flashed through
a raw-disk fallback:

1. Download OTA artifacts with `ac-os pull`
2. Stage `config.json`, `wifi_creds.json`, kernel, initramfs, and EFI payloads
3. Build full `ACBOOT`, `ACEFI`, and `AC-MAC` partition images offline
4. Assemble a complete GPT raw USB image
5. Write that raw image directly to `/dev/sda`
6. Read files back from the physical device by partition offset to verify the
   result

The bulk raw write completed successfully:

- bytes written: `15518924800`
- elapsed: `742.457 s`
- sustained rate at completion: `20.9 MB/s`

---

## 5. Device readback verification

Readback from the physical USB confirmed:

- `config.json` present on the device
- `wifi_creds.json` present on the device
- handle: `jeffrey`
- AC token: present
- Claude token: present
- GitHub PAT: present
- `claudeCreds`: present
- `claudeState`: present
- Wi-Fi networks preserved: `4`

Readback SSIDs:

- `aesthetic.computer`
- `ATT2AWTpcr`
- `GettyLink`
- `Tondo_Guest`

Partition content checks:

- `ACBOOT` contained `EFI/BOOT/BOOTX64.EFI` and `BOOTIA32.EFI`
- `ACEFI` contained `EFI/BOOT/BOOTX64.EFI`, `LOADER.EFI`, and `KERNEL.EFI`

---

## 6. Caveat

Immediately after the raw write, host-side `lsblk` still showed `/dev/sda1-3`
without refreshed filesystem metadata. That appears to be the same host/kernel
partition-refresh limitation that broke the standard flash path.

Important distinction:

- host metadata refresh was stale
- direct readback from the physical USB contents succeeded

So the USB image itself verified cleanly even though the host did not fully
refresh its partition view until replug.

---

## 7. Outcome

The `dynamic-milksnake` OTA build for commit `848d3231a` was successfully
published and successfully written to the USB, with credentials and Wi-Fi data
inscribed and verified from the device itself.
