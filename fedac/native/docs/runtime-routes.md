# Runtime routes: Nix, the browser, the Mac app

2026-11-24. Where AC OS can run besides its own kernel, and why NixOS is not
the base.

## NixOS was dropped for frame rate

`fedac/nixos/` builds a NixOS USB image (`ac-os` `nix` subcommand, OTA channel
`nix`). Last touched 2026-04-11. Commit `da699209eb` (2026-04-07) deprecated
it for production:

- DRM-direct ac-native ran 27 fps on the ThinkPad 11e; bare metal runs 60.
- Page flip took 24–28 ms instead of 16. Disabling Wi-Fi, mitigations, logind
  and frame sync did not move it. Cause: the generic kernel's i915 stack.
- Boot took ~90 s.

The known fix is the bare-metal kernel `.config` inside Nix, which leaves
NixOS adding little but rollback.

## Nix as the builder: not now

Oven builds take 2–6 min and pass. Of 63 builder commits since April, four
episodes were build-system pain Nix would address:

| Date | Failure | Nix |
|---|---|---|
| 2026-08-11 | Build exited 0 and shipped the previous initramfs (`4c2ab4b026`) | Missing outputs fail |
| 2026-10-13 | Four commits fighting Docker cache pruning | Content-addressed store |
| 2026-04-09 | `ldd` hunting for initramfs libs (`iris_dri.so`) | Closures are computed |
| 2026-04-14, 07-02 | No Fedora `cbfstool`, gcc 15 `-Werror`, kernel.org outage | Pinned toolchain, binary cache; partial |

All four are patched. Revisit when a Fedora bump or a stale artifact next
costs a day.

## Browser

`fedac/native/wasm/` (April) runs the native `prompt.mjs` on a small WASM
raster core with browser shims; live at `/ac-native-wasm/`. A notepat
AudioWorklet version followed 2026-04-27.

- **Runtime in WASM** (extend the prototype): native pieces in a tab, a
  try-before-you-flash page. Tests no kernel or drivers.
- **Emulated kernel**: v86 is 32-bit only and the oven builds x86_64;
  QEMU-wasm is slow, with no GPU. Useful only as a CI boot check.

## Mac app

The app is a WKWebView, so it can load `/ac-native-wasm/` as is.

A native macOS port is the stronger route. `src/` is ~24k lines of C with no
`__APPLE__` code; the Linux coupling sits in three files:

| File | Lines | macOS backend |
|---|---|---|
| `drm-display.c` | 1.4k | Blit framebuffer to a Metal layer or view |
| `input.c` | 1.6k | NSEvent keys and pointer |
| `audio.c` | 5.5k | CoreAudio output; synths unchanged |

Wi-Fi, power and mounts in `ac-native.c` stub out. Native pieces then ship
with the app, so runtime changes need an app release or a downloaded bundle.

Virtualization.framework can boot the real kernel and initramfs, but Apple
Silicon needs an arm64 kernel and ac-native, a second build target.

## Next step

Split display, input, audio and storage into a platform layer with a Linux
backend. The macOS and WASM backends then implement the same boundary.
