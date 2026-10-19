# CHUWI MiniBook X (N100)

DMI: `CHUWI Innovation And Technology(ShenZhen)co.,Ltd` / `MiniBook X`,
BIOS `ZW.RP11D.26060101.009`. Alder Lake-N (i915 device a7a9), Wi-Fi 6
AX101, touchpad `AMR12174:00 0911:5288` over I2C, `NYM 869B-E112` USB
multitouch.

## Display

A 1200x1920 DSI panel, 50 Hz, mounted sideways in a landscape lid. The
VBT has no orientation and the kernel has no quirk for it.

- **Stripes.** The BIOS lights the panel with PLL cfgcr0 `0x1599b0` /
  cfgcr1 `0x90` (port clock 165919 kHz). i915 derives `0x1555b0` / `0x490`
  (165920), fails fastset, and forces a full modeset that ends in
  `DSI link not ready` and a striped screen. `kernel/patches/0007-dsi-keep-firmware-pll.patch`
  keeps the firmware's PLL. `i915.enable_dc=0` does not help.
- **Rotation.** Pieces draw upright at 1920x1200 and `display_present`
  turns each frame 270°. Override per stick with `/mnt/display-rotate`
  (`0`, `90` or `270`).
- **Density.** Ctrl+= / Ctrl+- is remembered in `/mnt/pixel-scale`; the
  show stick is seeded with `3` (640x400).
- `nomodeset` boots on the firmware framebuffer at 1024x768, unrotated.

## Keyboard

The F-row sends F1–F12 plainly; the printed functions need Fn. AC maps
the plain keys on this machine (`minibook_keys` in `src/ac-native.c`):

| Key | Printed | With Fn (evdev) | AC without Fn |
|-----|---------|-----------------|---------------|
| F1  | display | Super+P         | —             |
| F2  | mute    | KEY_MUTE        | mute          |
| F3  | vol −   | KEY_VOLUMEDOWN  | volume down   |
| F4  | vol +   | KEY_VOLUMEUP    | volume up     |
| F5  | kbd light | handled by the EC | —           |
| F6  | bright − | nothing reaches the OS | brightness down |
| F7  | bright + | nothing reaches the OS | brightness up   |
| F8  | prev    | KEY_PREVIOUSSONG | —            |
| F9  | next    | KEY_NEXTSONG    | —             |
| F10 |         | KEY_PAUSE       | —             |
| F11 |         | KEY_INSERT      | —             |
| F12 |         | KEY_SYSRQ       | —             |

The BIOS can swap Fn behaviour; AC's mapping assumes the default.

## Network and access

Joins `ATT2AWTpcr` (a preset) on its own. AC OS makes a new SSH host key
on every boot — use a throwaway known_hosts. The next boot's systemd-boot
entry can be chosen from a running system by writing the
`LoaderEntryOneShot-4a67b082-0a4c-41cf-b6c7-440b29bb8c4f` efivar
(attributes `07 00 00 00`, then the UTF-16LE entry name, NUL-terminated).

Needs Secure Boot off (unsigned `BOOTX64.EFI`).
