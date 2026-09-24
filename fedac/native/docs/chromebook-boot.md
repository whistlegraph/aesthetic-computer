# Booting AC OS sticks on stock Chromebooks

Stock Chromebook firmware never boots a UEFI stick. In developer mode, Ctrl+U
looks for a GPT partition of ChromeOS-kernel type holding a kernel packed and
signed by `vbutil_kernel`; the public developer keys are accepted, so no
firmware change (MrChromebox RW_LEGACY or full ROM) is needed. Every AC OS
stick now carries that partition alongside the UEFI layouts, and the same
`vmlinuz` serves all three.

## Layout

| partition | type              | contents                                   |
|-----------|-------------------|--------------------------------------------|
| 1 ACBOOT  | Basic data (FAT32)| BOOTX64.EFI (kernel-direct), initramfs, config.json, wifi_creds.json |
| 3 KERN-A  | ChromeOS kernel   | `vmlinuz.kpart` — vboot-packed bzImage, 64 MB partition |
| 2 ACEFI   | EFI System        | splash → systemd-boot → KERNEL.EFI + initramfs |

UEFI firmware picks ACEFI. Depthcharge (Chromebook firmware) picks KERN-A.
Partition 3 sits between the two on disk; the number is what macOS and vboot
care about. GPT attribute bits follow cgpt semantics: priority 10, tries 5,
successful 1 — the flags recovery media use, so the firmware never counts
the stick down to unbootable.

## How the Chromebook path reaches the real initramfs

Depthcharge passes the kernel no initrd. The kernel therefore embeds a stub
initramfs (`initramfs-stub/init` + static busybox, wired in by
`docker-build.sh` Step 3b through `CONFIG_INITRAMFS_SOURCE`). The stub waits
for the stick, mounts the first FAT partition carrying `initramfs.cpio.gz`
and `config.json`, unpacks the real initramfs into a tmpfs and
`switch_root`s into it. From there boot is identical to the UEFI path,
including the inscription baked into `initramfs.cpio.gz` at flash time.

On UEFI boots the firmware-supplied initrd is unpacked over the embedded
one, so its `/init` replaces the stub and the stub never runs.
`/proc/cmdline` contains `ac.boot=chromeos` only on the depthcharge path;
the kernel's built-in `CONFIG_CMDLINE` supplies everything else (x86 appends
the bootloader's command line to the built-in one).

## Pipeline

- `Dockerfile.builder` installs `vboot-utils` (Fedora) for `vbutil_kernel`
  and `/usr/share/vboot/devkeys`.
- `docker-build.sh` builds the stub cpio, sets `CONFIG_INITRAMFS_SOURCE`,
  and after `bzImage` packs + verifies `vmlinuz.kpart`. A missing tool fails
  the build; `AC_SKIP_KPART=1` opts out loudly.
- `oven/native-builder.mjs` extracts `vmlinuz.kpart` (required unless
  `AC_SKIP_KPART=1`) and `scripts/upload-release.sh` publishes it as
  `native-notepat-latest.vmlinuz.kpart`.
- `ac-os pull` fetches it when the release has one; `scripts/flash-mac.sh`
  adds KERN-A when `vmlinuz.kpart` is in the source dir, dd's the blob, and
  verifies it by reading the partition back. Releases without a kpart still
  flash as two-partition UEFI sticks.

Not yet covered: the Linux `ac-os flash` path and the ISO (`media-layout.sh`).

## On the Chromebook

1. Developer mode (Esc+Refresh+Power, then Ctrl+D at the recovery screen;
   this wipes local ChromeOS data once).
2. Sign in, open a terminal (Ctrl+Alt+T → `shell`, or VT2) and run
   `sudo crossystem dev_boot_usb=1`.
3. Reboot with the stick in. At the "OS verification is OFF" screen press
   Ctrl+U.

If the stick is ignored, the firmware is not seeing a kernel partition it
accepts: check `cgpt show /dev/sdX` from the ChromeOS shell for a KERN-A
entry with type `ChromeOS kernel` and the priority/tries/successful bits.
The stub logs to the console and kmsg as `[ac-stub]`; if it cannot find the
stick it drops to a shell instead of hanging.

First verified target: Lenovo 500e Chromebook 2nd Gen (board `phaser360`,
Octopus / Gemini Lake), 2026-09-24 lane.
