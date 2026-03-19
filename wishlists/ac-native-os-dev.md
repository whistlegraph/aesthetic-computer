# AC Native OS Dev Wishlist

## Faster Iteration Loop

### Ethernet PXE Netboot (top priority)
- Two ThinkPads with built-in ethernet (X13, T14, T16, L-series, P-series)
- One cable between them, `ac-os serve` on the dev machine
- Target reboots and grabs the new build over TFTP — no USB ever
- babypat (1.5 KB) downloads in milliseconds, full kernel (89 MB) in ~2s on gigabit
- Need: 2x ethernet-capable laptops, 1x ethernet cable
- Already built: `fedac/babypat/netboot.sh` (dnsmasq DHCP + TFTP server)
- TODO: `ac-os serve` command that wraps netboot for the full kernel

### WiFi Netboot Stub (no ethernet needed)
- Tiny Linux (~8 MB) on USB that never changes
- Boots, connects to WiFi, downloads payload over HTTP, kexec into it
- Dev machine serves builds over WiFi — same edit/rebuild/reboot loop
- Slower than ethernet but works with any laptop
- TODO: build the stub (kernel + wpa_supplicant + curl + kexec, ~8 MB)

### Current Pain Points
- USB stick plug/unplug cycle for every flash
- X1 Nano (current dev host) has no ethernet — can't PXE boot targets from it
- Devcontainer can't access /dev directly — needs SSH to docker host for flashing
- Build artifacts (~89 MB kernel) take time to write to USB

## Hardware Wishlist
- 2x ThinkPad with ethernet (X13 or T14) for twin dev setup
- USB-C ethernet adapters as fallback for ultrabooks ($10 each)
- Ethernet cable (cat6, any length)

## babypat
- Bare-metal UEFI musical keyboard, 1,536 bytes total
- Currently: PC speaker square wave (buzzy, not all laptops have one)
- Want: Intel HDA sine wave synthesis (~15-20 KB extra, real audio output)
- Want: fits in a single QR code on a card (currently 1.5 KB, QR max ~3 KB)
- Stretch: paper-encoded bootable OS on a score card

## ac-os Improvements
- `ac-os serve` — PXE/HTTP server for network boot (replaces `ac-os flash`)
- `ac-os serve --wifi` — WiFi stub mode (HTTP server, target downloads over WiFi)
- `ac-os qemu` — already exists, good for testing without hardware
- Faster kernel builds (incremental, ccache)
