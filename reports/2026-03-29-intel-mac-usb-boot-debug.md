# Intel Mac USB Boot Debug Handoff

Prepared on 2026-03-29 from the local flashing/debugging session in `/workspaces/aesthetic-computer`.

## Goal

Get `fedac/native` USB media to appear and boot reliably on an older Intel MacBook while preserving per-user `config.json` and the recent hybrid-media work.

## Current Repo State

At the time of this note:

- Branch: `main`
- HEAD: `39f4a773c`
- Worktree: clean before adding this report

The current checked-in native changes already include:

- `fedac/native/ac-os`
  - `build_kernel()` now stages built-in firmware from `/usr/lib/firmware` or `/lib/firmware`, including nested paths like `i915/glk_dmc_ver1_04.bin`.
- `fedac/native/initramfs/init`
  - USB mount logic now prefers a FAT partition with `config.json` before falling back to FAT partitions that only have boot files.
- `fedac/native/src/ac-native.c`
  - Display-failure diagnostics now write to `/mnt/ac-init.log` instead of `/mnt/usb/ac-init.log`.
- `fedac/native/scripts/flash-helper-runner.sh`
  - This file is where the Intel-Mac-specific boot-media behavior was being iterated during this session.

## Build Result

A fresh local kernel build completed successfully during this session.

- `fedac/native/build/vmlinuz`
- Size: `282571776`
- SHA256: `09e1f63b92540d12a0f5209b5566894540f9c53e776cedd9aa557a23dabdbb0b`

## What Happened

### 1. Initial hybrid flash was discoverable but wrong on the Mac

The first hybrid-media flash produced 3 visible `EFI Boot` entries on the old Intel MacBook.

Local verification showed why:

- partition 1 (`ACBOOT`) had `config.json`, `EFI/BOOT/BOOTX64.EFI`, and `EFI/BOOT/KERNEL.EFI`
- partition 2 (`ACEFI`) had the same boot tree
- partition 3 (`AC-MAC`) had the same boot tree plus a blessed Mac path

That matched the Mac behavior: 3 generic boot targets.

The user reported:

- all 3 `EFI Boot` entries appeared
- each one crashed/rebooted back into macOS

At the same time, the USB had no runtime logs written back to the writable partition. That strongly suggested the failure was happening before Linux userland mounted the writable FAT partition.

### 2. First attempt to force a single Mac target regressed too far

The next iteration tried to force a single Apple-visible target by:

- removing `BOOTX64.EFI` from the FAT helper partitions
- hiding the FAT partitions in partition metadata
- leaving only a blessed HFS Mac path
- using a direct-kernel `boot.efi` on the HFS partition

This was the version the user tried next, and the old Intel MacBook did not show the stick at all.

That is the key regression.

### 3. The USB then became awkward to rewrite cleanly in-place

After one interrupted rewrite, `/dev/sda1` remained kernel-busy inside the privileged helper environment:

- `mkfs.vfat /dev/sda1` kept returning `Device or resource busy`
- `sda2` and `sda3` were still writable
- `sda1` could not be cleanly reformatted again during this session

Because of that, later updates were done in-place on `sda2` and `sda3` rather than from a perfectly clean full reprovision.

## Current USB State

The most recent local write left the stick in this state:

- GPT still has 3 partitions:
  - `sda1` `ACBOOT`
  - `sda2` `ACEFI`
  - `sda3` `AC-MAC`
- Hybrid MBR currently advertises only the Apple HFS partition:
  - protective entry
  - `0xAF` for the HFS partition
- `sda2` was refreshed and verified to contain:
  - `config.json`
  - `EFI/BOOT/KERNEL.EFI`
  - no `EFI/BOOT/BOOTX64.EFI`
- `sda3` was refreshed with:
  - `System/Library/CoreServices/boot.efi`
  - that file’s SHA256 matched the rebuilt kernel hash above

Important caveat:

- this exact latest USB state was not re-tested by the user on the old MacBook before this report was requested
- the last user-observed result on the old MacBook was still: “did not show up at all”

## Source-Backed Findings

The web check suggests the “hide everything except one blessed HFS file” approach was too improvised and not close enough to the Fedora path.

### Fedora docs

Fedora’s `livecd-iso-to-disk --efi` docs explicitly say that this mode is necessary for most Intel Macs and can create up to 3 partitions:

- main image partition
- EFI helper partition from `/images/efiboot.img`
- HFS+ Mac helper partition from `/images/macboot.img`

Source:

- https://raw.githubusercontent.com/livecd-tools/livecd-tools/master/docs/livecd-iso-to-disk.pod

### Fedora implementation

The `livecd-iso-to-disk.sh` source does not treat the Mac helper partition as “just write one arbitrary `boot.efi` file.” It formats the HFS partition and copies the contents of `macboot.img` into it.

Source:

- https://raw.githubusercontent.com/livecd-tools/livecd-tools/main/tools/livecd-iso-to-disk.sh

### Fedora Mac EFI notes

Fedora’s Intel Mac EFI notes emphasize:

- HFS+ blessing matters
- Apple-visible boot media needs Apple-style metadata, not just a generic GPT ESP

Source:

- https://fedoraproject.org/wiki/User%3APjones/MacCDsForEFI

### Apple boot path

Apple’s booting documentation describes `/System/Library/CoreServices/boot.efi` as the standard bootloader path for the simplest boot configuration.

Source:

- https://developer.apple.com/library/archive/documentation/Darwin/Conceptual/KernelProgramming/booting/booting.html

### HFS blessing tool

`hfs-bless` is explicitly meant to make an HFS+ filesystem bootable for EFI Apple hardware.

Source:

- https://www.mankier.com/1/hfs-bless

## Best Current Reading

The safest reading is:

- the older Intel MacBook is sensitive to the exact Apple helper partition layout
- the first “3 EFI Boot” result showed the stick was at least discoverable
- the later “single visible target” rewrite likely removed or distorted something the old Mac firmware needed for discovery
- Fedora’s documented path is closer to “separate helper images for EFI and Mac” than to the custom hidden-partition approach used in the later local experiment

## Recommended Next Step

Claude should optimize for “shows up and boots” first, not for deduplicating the Option-boot picker.

Recommended order:

1. Move `fedac/native/scripts/flash-helper-runner.sh` back toward a Fedora-style discoverable layout.
2. Stop hiding the helper FAT partition(s) until boot discovery is stable on the old MacBook.
3. Recreate a proper Mac helper partition rather than treating partition 3 as only a hand-written `boot.efi` drop.
4. Once the stick reliably appears and boots, then iterate on reducing duplicate Option-boot entries.

## Practical Notes For Claude

- Do not assume the current USB contents are a clean baseline. `sda1` became sticky/busy after an interrupted rewrite.
- The rebuilt kernel artifact is good and verified.
- The biggest unresolved problem is old-Intel-Mac discovery semantics, not Linux kernel compilation.
- If you need a fully clean USB reprovision, physically replugging the stick may be the fastest way to clear the stale `sda1` busy state before retrying.
