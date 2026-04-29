# Menu Band — distribution metadata

Prepared 2026-04-28. Notarized + stapled bundle: `~/Applications/Menu Band.app` (1.0, build 1).
Notary submission ID for this build: `7cd9617a-ef01-4962-b4f5-fd6cff249de8`.

## Core copy

| Field | Value |
| --- | --- |
| Name | Menu Band |
| Subtitle / tagline | Built-in macOS instruments, in the menu bar. |
| Category | Music |
| Bundle ID | `computer.aestheticcomputer.menuband` |
| Team ID | `FB5948YR3S` (Jeffrey Scudder) |
| Version | 1.0 (build 1) |
| Minimum macOS | 11.0 |
| Architectures | arm64 (Apple Silicon) |
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
- **Privacy Policy URL:** TODO — needed for App Store; for direct download, optional.

## Screenshot checklist

The Mac App Store requires at least one screenshot. For direct download, screenshots are optional but help.

- [ ] Menubar close-up — Menu Band lit on a held chord (accent color).
- [ ] Popover open — Input picker visible, Pointer/Notepat/Ableton hover-preview shown.
- [ ] In Ableton/Logic — virtual MIDI port labelled "Menu Band" in the source list, with a track receiving notes.
- [ ] Optional: instrument picker open showing GM family list.

## Distribution channels

### (a) Direct download — ready today

`./install.sh` then `./notarize.sh` produces a Developer ID-signed, hardened-runtime, notarized, stapled bundle at `~/Applications/Menu Band.app`. Package with `./dmg.sh` and host the DMG behind a download button on notepat.com / aesthetic.computer. Gatekeeper accepts it without warnings.

### (b) Mac App Store — blocked by sandbox

The current build uses `CGEventTap` (TYPE mode) and Carbon `RegisterEventHotKey` (⌃⌥⌘P), neither of which run under the App Sandbox that the Mac App Store mandates.

To ship a Mac App Store version:

1. Drop TYPE mode and the global hotkey (Pointer-only).
2. Add `com.apple.security.app-sandbox = true` to `MenuBand.entitlements`.
3. Add the App Sandbox's `com.apple.security.network.client` if any net call sneaks in (none today).
4. Re-archive in Xcode (Mac App Store distribution requires the Xcode archive flow, not raw `swift build`).
5. Upload via Xcode Organizer or `xcrun altool`.
6. Submit for review (1–7 days typical).

This would ship as a strict subset of the direct-download version. Maintaining both means a `#if MAC_APP_STORE` flag around the TYPE mode + hotkey.

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
