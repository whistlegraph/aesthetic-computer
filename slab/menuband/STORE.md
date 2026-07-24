# Menu Band — distribution metadata

Prepared 2026-04-28; refreshed for the 1.6.5 release in July 2026. The direct
build is Developer ID signed, notarized, and distributed as a universal DMG.

## Core copy

| Field | Value |
| --- | --- |
| Name | Menu Band |
| Subtitle / tagline | Built-in macOS instruments, in the menu bar. |
| Category | Music |
| Bundle ID | `computer.aestheticcomputer.menuband` |
| Team ID | `FB5948YR3S` (Jeffrey Scudder) |
| Version | 1.6.5 (build 165) |
| Minimum macOS | 11.0 |
| Architectures | Universal (Apple Silicon + Intel) |
| Signing | Developer ID Application + hardened runtime + notarized |

## Long description

> Menu Band brings the built-in macOS instruments — the ones GarageBand uses — into your menu bar.
>
> A tiny piano sits at the right of your menu bar. Click any key to play. Type letters to play. Send notes to your DAW over MIDI.
>
> Three input modes:
>
> • **Pointer** — mouse-only, two octaves of QWERTY-mapped piano keys.
> • **Notepat** — global keystroke capture, Notepat layout, two octaves.
> • **Ableton** — global keystroke capture, Ableton Live's M-mode QWERTY layout, one octave.
>
> Menu Band publishes a virtual MIDI source named "Menu Band" — pick it as an input in any DAW and the keys you tap (or letters you type) play through your DAW's instruments. A built-in General MIDI synth covers hundreds of patches when no DAW is running.
>
> A political project: accessible music-making is as essential as time, network connectivity, and battery life. Free + open source.

## Short description (≤170 chars, App Store promo line)

> Built-in macOS instruments in your menu bar. Tap, type, or send to your DAW. Three input modes. One virtual MIDI source. Free + open source.

## Keywords (≤100 chars, App Store)

```
music,midi,menubar,garageband,ableton,daw,piano,qwerty,instrument,synth
```

## What's new (1.0)

First release. Three-state input picker (Pointer / Notepat / Ableton) with hover-preview, MIDI loopback self-test, virtual MIDI source for any DAW, instrument picker covering all 128 General MIDI patches.

## URLs (App Store Connect form)

- **Support URL:** `https://aesthetic.computer`
- **Marketing URL:** `https://notepat.com`
- **Privacy Policy URL:** `https://aesthetic.computer/menuband/privacy.html` (live).

## Screenshot checklist

The Mac App Store requires at least one screenshot. For direct download, screenshots are optional but help.

- [ ] Menubar close-up — Menu Band lit on a held chord (accent color).
- [ ] Popover open — Input picker visible, Pointer/Notepat/Ableton hover-preview shown.
- [ ] In Ableton/Logic — virtual MIDI port labelled "Menu Band" in the source list, with a track receiving notes.
- [ ] Optional: instrument picker open showing GM family list.

## Distribution channels

### (a) Direct download — ready today

`./install.sh` then `./notarize.sh` produces a Developer ID-signed, hardened-runtime, notarized, stapled bundle at `~/Applications/Menu Band.app`. Package with `./dmg.sh` and host the DMG behind a download button on notepat.com / aesthetic.computer. Gatekeeper accepts it without warnings.

### (b) Mac App Store — shipping as a sandboxed subset

The App Store target compiles out global event taps, direct process launching,
process-audio capture, room hosting, and other features the sandbox forbids.
Focused typing, built-in instruments, hardware MIDI, DAW MIDI output, recording,
and internet radio remain available.

The standing release process is:

1. Keep direct-only code behind `#if !MAC_APP_STORE`.
2. Generate the Xcode project from `project.yml` and compile the Release target.
3. Archive with App Store signing and export the installer package.
4. Run `fastlane mac meta → shots → upload → ship` exactly once per lane.
5. Verify the signed sandboxed build using `STORE-APP-STORE.md` §5.

This ships as a strict subset of the direct-download version from the same
source tree.

### (c) Both — recommended long-term

Direct download = full version, App Store = sandboxed Pointer-only build for users who only want the menubar piano + DAW MIDI output without the global keystroke capture.

## Build + ship recipe (direct download)

```fish
cd ~/aesthetic-computer/slab/menuband
source ~/aesthetic-computer/aesthetic-computer-vault/apple/app-specific-password.env
./install.sh                # build + sign + install + load LaunchAgent
./notarize.sh               # zip + submit + staple
./dmg.sh                    # package signed DMG
```

Resulting DMG sits at `slab/menuband/Menu-Band-<version>.dmg`. Host on Spaces / notepat.com.
