# OSKIEWAR for Xbox

This is the no-WebView Aesthetic Computer host. It is a C++/CX UWP DirectX
application with native Direct3D 11 rendering, XAudio2 sound,
Windows.Gaming.Input polling, and a QuickJS-ng interpreter compiled into the
package.

The first package boots a bundled JavaScript smoke piece. Press A/B/X/Y to
change the Direct3D clear color and submit an XAudio2 tone. Device Portal debug
output includes `AC_NATIVE_BIOS_READY engine=quickjs-ng piece=smoke`.

Package tiles and splash art are generated from the current Pals image served
by `https://pals.aesthetic.computer`, pinned in `xbox/assets/pals.png`.

The JS environment exposes an allowlisted piece API plus the lifecycle
functions `boot`, `sim`, `paint`, `act`, and `leave`. It has no DOM, WebView,
filesystem, process, Device Portal, arbitrary WinRT, or arbitrary network
globals.

Current bindings are `wipe`, queued `box`, `line`, bitmap `write`, native
`systemWrite`, Segoe MDL2 `systemGlyph`, latest AC `painting`, one-shot `synth`,
MenuBand-derived `drum`, continuous `oscillator` / `oscillatorStop`, `controllers`,
indexed `gamepad(0)` / `gamepad(1)` snapshots (plain `gamepad()` remains Player 1),
`capabilities`, `runtime`, the host-mediated `ac` feed, and bounded structured
`telemetry`. Revision 29 adds bounded `gameSignal(event, player, value, value2)`
OSC output to UDP `51338`; the host fixes the LAN broadcast destination and
pieces never receive a general socket. Revision 25 adds the host-mediated `disc`, `discScan`, `discShow`,
`discPhoto`, and `discCopy` photo-disc surface. The `ac` snapshot polls only declared Aesthetic Computer mood,
clock-chat, and painting endpoints; sandboxed pieces do not receive a general
HTTP primitive. Runtime failures roll back to the last known good piece.

The photo-disc service recursively searches mounted removable volumes for
`.jpg`, `.jpeg`, `.jpe`, `.png`, `.tif`, `.tiff`, and `.pcd`. It keeps WinRT
`StorageFile` objects and paths inside the native host, bounds discovery to
4,096 photos, bounds encoded input to 128 MiB, decodes through Windows Imaging
to an sRGB image no larger than 2,048 pixels per side, and publishes only an
immutable status snapshot to JavaScript. `discPhoto` draws the current decoded
image through the existing scene texture path. `discCopy` makes a flat,
numbered copy of the discovered photos under `LocalState/photo-cd`, where Xbox
Device Portal can retrieve them. `.pcd` is inventoried because Kodak Photo CDs
commonly use it; display still depends on the Xbox Windows image decoder having
a codec for that particular file.

Revision 22 turns the existing `Windows.Devices.Midi` probe into a hot-plug
monophonic instrument. Note On/Off gates a native XAudio2 sine oscillator,
14-bit pitch bend shifts it continuously, CC1 is exposed for modulation, and
CC7 controls level. `runtime()` reports the gate, channel, note, velocity,
pitch bend, last controller/value, port status, and input-to-audio latency.

Revision 23 adds a bounded UDP MIDI inlet on port `51337`. It accepts only the
`ACM1` bridge envelope and only Note On/Off, CC, and pitch-bend messages, then
feeds the exact same native oscillator/event path as a local MIDI port. This is
needed for instruments such as Yamaha reface whose vendor USB-MIDI driver is
not available inside Xbox UWP. On a Mac connected to the same private network:

```bash
swift xbox/tools/midi-bridge.swift <xbox-host> reface
```

The bridge reads CoreMIDI, prints the live event stream, and sends one compact
UDP datagram per musical event. The Xbox HUD changes from
`NETWORK-LISTENING :51337` to `NETWORK: REFACE YC` after the first packet.

Revision 30 caps the DXGI device frame queue at one and records controller-edge
to-present latency as `AC_NATIVE_INPUT_LATENCY`. OSKIEWAR emits OSC addresses
under `/oskiewar/*` on UDP `51338`; `ac-m4l/AC-GameSignals.amxd.json` turns bounded
combat and movement-edge events into MIDI notes. The sender starts a datagram
when `gameSignal` is called, drains its three consecutive reliability copies as
each asynchronous write completes, and reports enqueue-to-`StoreAsync`
completion as `AC_NATIVE_OSKIEWAR_SIGNAL_LATENCY`. This measures the native send
queue, not UDP delivery or Ableton's audio output latency. The Max receiver uses
the sequence field to collapse each consecutive three-copy group and caps its
own queue at 256 datagrams so overload drops signals instead of playing a long
stale backlog.

Revision 31 adds the allowlisted OSKIEWAR fighter profile feed for `@jeffrey`,
`@fifi`, `@oskie`, and `@sat`: current mood, per-character handle colors, and
latest public system-chat text. The sandbox still receives only the immutable
`ac()` snapshot and has no general network access. It also packages the existing
YWFT Processing face and exposes it through bounded `ywftWrite`; `write` remains
the native AC block face and `systemWrite` remains the Xbox system face.

OSKIEWAR also packages the KidLisp `Comic Relief` face and exposes it through
bounded `comicWrite`; its browser, macOS, replay, spectator, and Xbox clients
therefore render the same UI typography without relying on an installed font.

Revision 32 removes the global chromatic channel offset. The point-sampled
scan, one-pixel dither, and vignette remain without separating RGB edges.

Revision 33 exposes bounded `saveReplay` for versioned `.oskiedemo` match
streams. The host accepts one JSON object up to 512 KiB and uploads it to the
fixed AC replay endpoint; pieces receive neither arbitrary networking nor
storage credentials.

Revision 34 starts each OSKIEWAR OSC datagram immediately, preserves the
three-copy reliability group without a queue race, and reports native
enqueue-to-send latency for live Ableton diagnosis.

Revision 35 presents the installed Xbox app as OSKIEWAR while retaining the
stable `AestheticComputer.NativeBios` identity for in-place upgrades. It adds
the OSKIEWAR / NEW GAME boot selector, a two-pad input lab, and live match
spectator publishing.

Revision 13 also exposes bounded `stampPainting` and `blur` primitives to the
trusted host-side KidLisp compiler. See [`../KIDLISP-NATIVE.md`](../KIDLISP-NATIVE.md)
for the supported subset and `$obk` wire-deploy flow.

## Live development from blueberry

`xbox/tools/live.mjs` is the credential-safe control surface for agents and
humans. It reads Device Portal credentials from the Xbox vault on blueberry;
credentials never enter the repository or command output.

```bash
node xbox/tools/live.mjs status
node xbox/tools/live.mjs install xbox/builds/1.0.0.10/NativeBios_1.0.0.10_x64.msix xbox/builds/1.0.0.10/Microsoft.VCLibs.x64.14.00.appx
node xbox/tools/live.mjs deploy xbox/live/controller-probe.js
node xbox/tools/live.mjs deploy xbox/live/native-showcase.js
node xbox/tools/live.mjs deploy xbox/live/photo-disc.js
node xbox/tools/live.mjs deploy-kidlisp '$obk'
node xbox/tools/live.mjs logs 100
```

The `deploy` command publishes sandboxed JavaScript into the installed package's
`LocalState/live-piece.js`, launches the newest installed BIOS revision, and
prints recent telemetry.

After a successful Native BIOS install, the tool removes stale uninstallable AC
development packages from earlier experiments. Cleanup is restricted to our
`CN=AestheticComputerDev` publisher or `AestheticComputer.*` package namespace,
and always preserves `AestheticComputer.NativeBios`; system and unrelated dev
packages are never targeted. `prune` is also available as an explicit command.

## Build

AppVeyor builds `NativeBios.sln` for Release x64 and emits the distinct
`xbox-native-bios-x64` sideload artifact. Assets and its temporary development
certificate are generated by `appveyor.yml`.

## Vendored interpreter

`third_party/quickjs-ng` is QuickJS-ng v0.15.1, pinned from the upstream release
archive with SHA-256:

`c4e813951b7c46845096a948e978c620b11ab4cf5fd622ca09c727ec31f42623`

Only the four core library translation units and their generated headers are
vendored; the CLI and libc bindings are deliberately excluded. QuickJS-ng is
MIT licensed; its unmodified license is included beside the source.
