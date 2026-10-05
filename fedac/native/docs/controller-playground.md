# HDMI controller playground

The October 4, 2026 device session runs a closed-lid ThinkPad on HDMI at
1920×1080/60, drawing a 640×360 canvas at 3× nearest-neighbor scale. These
changes are a development checkpoint; no public OTA was built or published.

## Display and drawing

Set `AC_HDMI_ONLY=1 AC_PIXEL_SCALE=3` before launching `ac-native`. HDMI becomes
the primary display, using advertised progressive 1080p60 when available, and
other CRTCs are disabled after a successful modeset. With no connected HDMI
at startup, the primary-display search falls back to the laptop panel.
`AC_PIXEL_SCALE` accepts integers 1–16. HDMI-only mode does not provide live
unplug-to-internal-panel handoff; restart the runtime to select another output.

Native `paste(painting, dx, dy, sx, sy, width, height)` now supports source
rectangles with preserved row stride and alpha compositing. Invalid source
rectangles are ignored. The original three-argument form is unchanged.

`hdmi` displays FPS, source size, actual HDMI mode, scale, and the nearest
filter. Run `sh /mnt/tools/hdmi-monitor.sh` to populate its mode readout from
DRM debugfs. `hdmi-mode-probe.c` is an optional diagnostic preload for older
mirroring runtimes: it restricts modes to advertised progressive choices and
reverts unconfirmed tests after 20 seconds. Do not combine it with HDMI-only
mode. It is not loaded by the normal build.

## Controller and keyboard

The tested USB device is the **8BitDo Arcade Controller for Xbox, 2dc8:202c**.
The device kernel has neither xpad nor loadable modules. The narrowly scoped
`tools/ac-usb-controller.c` reader claims its unbound USB data interface via
usbfs, initializes GIP reports, and publishes atomic state to
`/tmp/ac-controller.json`. It never detaches a kernel driver. A lock prevents
multiple readers; unplugging clears held controls, and discovery retries.
Pieces discard stale state after 1.5 seconds. This is a piece-level bridge,
not a general native gamepad API or support for every Xbox controller.

Build the helpers on Linux for the target architecture:

```sh
cc -O2 -Wall -Wextra -Werror -static tools/ac-usb-controller.c -o ac-usb-controller
cc -O2 -Wall -Wextra -Werror -static tools/ac-keyboard-remote.c -o ac-keyboard-remote
```

Install executable copies under `/mnt/tools/`. Both playground pieces start
`ac-usb-controller` when its heartbeat is missing. Slab launches the keyboard
receiver over authenticated SSH; see [AC keyboard remote](../../../slab/menubar-swift/README.md#ac-keyboard-remote)
for the Command–Option–L shortcut and per-host configuration. Helpers are
installed separately; the image builders do not install these binaries yet.

## Pieces

- `little-platformer`: side-scrolling physics at a fixed 120 Hz simulation
  step. A jumps (hold for height), B shoves, X drops a ball, Y resets, and
  RB/RT runs. Includes coyote time, buffered jumps, object collisions,
  spring pads, falling/respawn, held buttons and FPS. Arrows/Space work too.
- `little-world`: top-down movement and camera scrolling; A hops, B plants,
  RB/RT runs, and stars can be collected.
- `doody-blit`: four animated sprite types leave persistent stamps. C clears,
  arrows change stamp count, Space pauses, and M toggles low sine-wave music.
- `fia-stars`: the “love u fia” starfield.
- `feral-file-now`: a dated October 4 news snapshot with source attribution.

The pieces use `lib/saved-wifi.mjs` to join visible saved networks without
interrupting association/DHCP. Credentials stay in device-local
`/mnt/wifi_creds.json`; none are bundled in these pieces. Image builders and
`dev-push.sh` include the saved-WiFi and platform-physics modules.

On the tested USB, both boot partitions hold copies of the pieces, libraries,
helpers and boot configuration. Its device-local init copies `/mnt/pieces/*.mjs`
and `/mnt/lib/*.mjs` into RAM before launching AC. The platform-physics module
is also in its initramfs overlay. These local boot overrides are separate from
the repository's stock init. If installing onto another existing image,
install the dependencies in its initramfs or equivalent persistent boot hook;
a RAM-only `/lib` upload does not survive reboot.

## Evidence and limits

The running device was inspected at 1080p60 with the laptop panel disabled;
HDMI captures showed 60 FPS, transparent sprites and persistent stamps.
Actual controller presses moved the top-down character and scrolled its camera;
the user confirmed operation. The platformer was inspected on the same TV.
The live native binary uses the device's earlier base plus these display/blit
changes; it is not a full current-HEAD release. USB archive integrity and
piece/helper hashes were checked on both partitions; the final boot overlays
have not been cold-boot tested.

Checks:

```sh
node --test lib/saved-wifi.test.mjs lib/platform-physics.test.mjs
cc -O2 -Wall -Wextra -Werror tests/framebuffer-scale.c src/framebuffer.c -o /tmp/framebuffer-scale
/tmp/framebuffer-scale
# Linux:
cc -O2 -Wall -Wextra -Werror tools/ac-usb-controller-test.c -o /tmp/controller-test
/tmp/controller-test
cc -O2 -Wall -Wextra -Werror tools/ac-keyboard-remote-test.c -o /tmp/keyboard-test
/tmp/keyboard-test
# macOS, from the repository root:
bash slab/menubar-swift/tests/ac-remote-test.sh
```

The device also passed a cropped-paste probe covering source bounds, alpha,
untouched pixels and no automatic full-frame clears in the blit piece.
