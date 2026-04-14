# Firmware customization assets

Files referenced by [`/install-firmware.sh`](../install-firmware.sh):

- `ac-splash.bmp` — 1366×768 24-bit BMP that replaces the MrChromebox
  rabbit at boot. Must be uncompressed BMP (coreboot's splash decoder
  is minimal); generated from `system/scripts/firmware-splash.mjs`.
- `cbfstool` — statically-linked linux-x86_64 binary used to edit the
  coreboot CBFS of the downloaded base ROM. Upstream build recipe
  lives at https://doc.coreboot.org/util/cbfstool/index.html.

Pipeline:

1. `install-firmware.sh` downloads the board's signed ROM from
   MrChromebox's CDN.
2. Downloads `ac-splash.bmp` + `cbfstool` from here.
3. Swaps the splash and tweaks `etc/boot-menu-wait` inside CBFS.
4. Flashes the composed ROM via `flashrom -p internal -w`.

Both assets are published by `npm run firmware:publish` (TODO).
