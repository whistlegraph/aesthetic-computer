# Menu Band

**On the Mac App Store since 2026-07-12 (v1.5.3, $4.99):**
<https://apps.apple.com/us/app/menu-band/id6767311903> — that build is a reduced,
sandboxed subset of this codebase; `STORE-APP-STORE.md` is the record of what it
forks out and what review said. The full version stays at prompt.ac/menuband
(`STORE.md`).

A macOS menubar piano. The status item draws a multi-octave keyboard
inline in the menu bar; clicks/key-presses play through CoreAudio +
optional CoreMIDI; a popover (gear chip → click) exposes the GM
instrument chooser, octave control, layout pickers, shortcut binders,
and a metronome. Paired with a floating "play palette" panel for the
expanded keyboard view + waveform visualizer.

Lives at `slab/menuband/`. Distinct from `slab/menubar-swift/` (the
Claude session menubar; different status item, different process).

## Layout

```
slab/menuband/
├── Package.swift              SwiftPM manifest. No external deps —
│                              uses AppKit + Carbon hotkeys + AVFoundation.
├── install.sh                 Production build. Compiles release →
│                              wraps in .app bundle → signs with
│                              Developer ID → loads launchd agent.
├── Info.plist                 Bundle metadata (LSUIElement = YES).
├── MenuBand.entitlements      Hardened Runtime, no exceptions.
├── computer.aestheticcomputer.menuband.plist.tmpl
│                              launchd template (KeepAlive, RunAtLoad).
├── Sources/MenuBand/
│   ├── main.swift             Entry point. setActivationPolicy(.accessory).
│   ├── AppDelegate.swift      Status item, hotkeys, popover lifecycle,
│   │                          click-away monitors.
│   ├── MenuBandController.swift
│   │                          Audio/MIDI engine + state machine
│   │                          (octave, instrument, key map, etc.).
│   ├── MenuBandPopover.swift  All popover UI (~2000 LOC). Iterated on
│   │                          most often → primary watch-reload target.
│   ├── PianoWaveformWindow/   Floating "play palette" panel.
│   │                          Liquid-glass NSPanel; collapsed strip +
│   │                          expanded keyboard/visualizer.
│   ├── KeyboardIconRenderer.swift
│   │                          Status-item icon drawing (the inline
│   │                          piano keys). Hit-testing for clicks.
│   ├── Localization.swift     EN/ES tables + .didChange notification
│   │                          that triggers full popover rebuild.
│   ├── GarageBandLibrary.swift / GeneralMIDI.swift
│   │                          GM patch metadata + display names.
│   └── (helpers)              Hover, hotkeys, MIDI, synth, shortcuts.
├── bin/
│   ├── dev.sh                 Fast debug build + run (no signing).
│   ├── watch-reload.sh        Auto rebuild + restart on save.
│   ├── instrument-cards.mjs   FLUX → IG-square mnemonic deck builder.
│   └── instrument-card-compose.py
└── assets/
    └── instrument-cards/      Generated 1:1 instrument deck.
```

## Build + run

### Production (signed, launchd-managed)

```bash
./install.sh
```

What it does:
1. `swift build -c release`.
2. Wraps the binary in `~/Applications/Menu Band.app`.
3. Signs with Developer ID Application (or self-signed if absent).
4. Writes `~/Library/LaunchAgents/computer.aestheticcomputer.menuband.plist`.
5. `launchctl load`s the plist (KeepAlive + RunAtLoad).

Logs:
```bash
tail -f /tmp/menuband.err   # stderr
tail -f /tmp/menuband.out   # stdout
```

### Iterate on UI: watch-reload (recommended)

For popover/UI iteration. Save a `.swift` file → automatic rebuild +
relaunch + popover reopens. Cycle is ~2-5 seconds for incremental
builds. State that survives the restart: nothing inside the process,
but the popover auto-reopens via the existing
`computer.aestheticcomputer.menuband.showPopover` distributed
notification, so it *feels* immediate for visual iteration.

```bash
./bin/watch-reload.sh           # watch all Sources/
./bin/watch-reload.sh popover   # narrow to popover + Localization
```

Requires `fswatch` (`brew install fswatch`).

### Iterate on logic: dev.sh (manual restart, faster build)

```bash
./bin/dev.sh
```

What it does:
1. Stops the launchd-managed production daemon.
2. Builds debug (no optimization, faster) and runs the unsigned binary
   directly via `swift run -c debug --scratch-path .build-debug`.
3. No bundle wrapping, no signing, no launchd — just the binary.

After Ctrl-C, run `./install.sh` to put the signed daemon back.
Subsequent re-runs of `dev.sh` rebuild incrementally (~2-5s).

## Why no hot-reload

Tried `johnno1962/HotReloading` and `johnno1962/InjectionLite` (the
SwiftPM-friendly variant) for true in-place dylib injection. Both
fundamentally rely on parsing Xcode's `.xcactivitylog` files to derive
per-file compile commands. SwiftPM emits `.build/debug.yaml` instead,
which the injector doesn't read. The InjectionLite class loads into
the binary fine, but its `LogParser` finds no logs and bails silently.

Workarounds available, none clean:
- Wrap the project in an `.xcodeproj`, build it once via Xcode, then
  let InjectionLite pick up the resulting xcactivitylog. Means
  maintaining a parallel Xcode project alongside Package.swift.
- Write a custom in-process file watcher that runs `swift build` and
  manually loads the resulting object file via `dlopen`. Real work.

For now, `watch-reload.sh` is the iteration path. ~2-5s per cycle
with auto-reopen makes it feel close enough to live-reload for the
popover work that takes most of the iteration time.

## Architectural notes for agents

**Singletons.** `popover` is a single `NSPopover`. The expanded floating
keyboard is a single `PianoWaveformPanel` managed by
`PianoWaveformWindowDelegate`. If you see "two liquid-glass things on
screen", it's the popover + the panel both rendering, not duplicates of
either. `toggleFocusCaptureFromShortcut` (AppDelegate.swift) defers
panel show by one runloop tick when the popover was open so AppKit can
finish tearing it down before the panel materializes.

**Popover behavior.** `popover.behavior = .applicationDefined` because
clicks on the menubar piano keys would count as "outside" the popover
under `.transient` and dismiss it during play. Custom click-away
monitor in `AppDelegate.closePopover` handles dismissal manually.

**Language-change rebuild path.** Posting `Localization.didChange`
triggers `rebuildPopoverForLanguageChange()`, which closes the popover,
runs `installPopoverVC()` to build a fresh VC against the new locale,
and reopens it if it was visible. Same path runs every time
`watch-reload.sh` reposts the showPopover notification after relaunch.

**Hardened Runtime.** Release builds opt into Hardened Runtime *without*
exceptions. CoreMIDI, AVAudioEngine, CGEventTap, and Carbon
`RegisterEventHotKey` all work under default Hardened Runtime. Don't
add exceptions unless you've demonstrated they're required.

## Squawk — voice dictation (⌘⌃⌥`)

A push-to-talk mic that transcribes speech
**on-device** with Apple's `Speech` framework and drops the text into
whatever app is frontmost — built for dictating into a terminal / Claude
Code without leaving the keyboard, but it types into any focused text
field. Lives in `MenuBandSquawk.swift` (self-contained: recognizer +
`CGEvent` unicode text injector), wired from `AppDelegate`.

**Why it's cheap to add here.** Menu Band already owns the whole rig:
the `audio-input` entitlement is in *both* `MenuBand.entitlements` and
`MenuBand-AppStore.entitlements`; `MenuBandSampleVoice`/`MenuBandMicTempo`
already tap the mic `inputNode`; `KeyEventTap` already types into the
focused app; `GlobalHotkey` already registers system shortcuts; and the
About window already persists feature flags via `UserDefaults` checkboxes
(`toggleTapeFeature`/`togglePercussionSplit`). Dictation is those parts
re-composed plus one new framework (`Speech`).

**Design decisions**

- **On-device, no account.** `SFSpeechAudioBufferRecognitionRequest` with
  `requiresOnDeviceRecognition = true` — offline, free, zero backend, in
  keeping with Menu Band's no-server character. (Claude Code's own
  `/voice` is cloud + a Claude.ai account; this is the local path.)
  Needs `NSSpeechRecognitionUsageDescription` (+ the existing mic desc)
  in `Info.plist` and a one-time auth prompt.
- **Separate `AVAudioEngine`.** Dictation runs its own engine + input tap
  so it never fights the sampler's `recordEngine` tap on bus 0.
- **Trigger.** Default `⌘⌃⌥` + `` ` `` (`kVK_ANSI_Grave`), matching the
  ⌘⌃⌥-letter family the other shortcuts use. **Push-to-talk**: hold to
  listen, release to finalize + inject. `GlobalHotkey` gained an
  `onRelease` callback (registers `kEventHotKeyReleased`); `MenuBandSquawk`
  guards a `wantsListening` desired-state so a release that lands mid-auth
  cancels the pending start instead of stranding the mic. The popover 🦜
  cell and fullscreen button stay **click-to-toggle** (a click can't hold).
- **Off by default, gated by an Advanced flag.** About-window checkbox
  writes `MenuBandSquawk.enabledDefaultsKey`; the hotkey only registers
  when it's on. Keeps the mic dormant for users who don't want it.
- **Sandbox reality (DMG-only for typing).** `CGEvent` keystroke
  injection is forbidden in the App Store sandbox — same wall as the
  Notepat/Ableton typing modes — so the *type-into-focused-app* path is
  `#if !MAC_APP_STORE`. The MAS build can still transcribe; it just can't
  type into other apps.

**The mic-cell homogenization.** *Done (popover):* a 🦜 MIC cell at the
LEFT edge of the top row in `InstrumentListView` (`InstrumentMapView.swift`)
— pink, mirroring `sampleRect`'s special-cell pattern (`micRect` +
`isMicHit` + `mouseDown` branch + draw block + `onMicCommit`). It only
appears when the Advanced flag is on (`squawkEnabled`), fills while
`squawkListening`, and toggles dictation. Wiring lives in
`CollapsedPianoWaveformView`: `onMicCommit` posts
`.menuBandSquawkToggleRequested`; AppDelegate toggles and broadcasts
`.menuBandSquawkStateChanged`; the cell also watches
`.menuBandSquawkEnabledChanged` so it shows/hides live. Decoupled by
notification — the grid view never touches the dictation engine.

*Done (fullscreen):* the Expanded view (`ExpandedPianoWaveformView`) draws
its own custom liquid surface — it does NOT reuse `InstrumentListView` — so
Squawk there is a **"Squawk" toggle button** in the mode row (next to
Gamepad / LLMs), `mic` SF Symbol, hidden unless enabled. Same notification
seam: click posts `.menuBandSquawkToggleRequested`; the button's pushed
state follows `.menuBandSquawkStateChanged`; visibility follows
`.menuBandSquawkEnabledChanged`.

*Deliberately NOT built — "Ask / LLM prompt" mode.* The original idea was a
third cell mode that queues the transcript as an LLM prompt. But
`LLMGuideWindow` is a **static copy-paste guide**, not an interactive input
— there is no in-app LLM chat surface to feed. And in practice **Dictate
already *is* the "prompt an LLM" path**: when Claude Code (or any LLM CLI)
is the frontmost app, talking types the prompt straight into it. A separate
Ask mode would either duplicate Dictate or require a whole LLM chat UI. If
that surface ever exists, the seam is ready: swap
`MenuBandSquawk.onFinalText` to route the transcript instead of typing it.

See "Where popover controls ACTUALLY render" in the menuband memory before
adding AppKit controls — many stacks here are built-but-dropped.

## Common tasks

| Task | Command |
| --- | --- |
| Build + reinstall signed app | `./install.sh` |
| Auto rebuild + restart on save | `./bin/watch-reload.sh` |
| Manual debug build + run | `./bin/dev.sh` |
| Tail logs | `tail -f /tmp/menuband.err` |
| Force kill running daemon | `pkill -f "/MenuBand$"` |
| Stop launchd daemon | `launchctl unload ~/Library/LaunchAgents/computer.aestheticcomputer.menuband.plist` |
| Generate instrument cards | `node bin/instrument-cards.mjs` |

## Notation

This file is the source of truth. `README.md` is a symlink to it so
GitHub renders it as the project landing page.
