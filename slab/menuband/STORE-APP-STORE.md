# Menu Band — Mac App Store submission plan

Companion to `STORE.md` (which covers the direct-download DMG). This file is
the App Store-specific plan: what must change in the code, the sandbox audit,
the signing/archive steps, and the runtime checks only a signed build can
answer. Prepared 2026-05-29 against v1.4.1 (build 141).

The App Store build ships as a **reduced, sandboxed subset** of the
direct-download version. The full version stays at prompt.ac/menuband.

---

## 0. Status of prep (done before handoff)

- [x] `MenuBand-AppStore.entitlements` created — app-sandbox + audio-input + network.client.
- [x] `Info.plist` — added `ITSAppUsesNonExemptEncryption=false` (skips the per-submission export question).
- [x] Privacy policy already live at **https://aesthetic.computer/menuband/privacy.html** — use this as the App Store Connect Privacy Policy URL (resolves the STORE.md TODO). It accurately describes the full app; the sandboxed build collects strictly less.
- [x] **`#if MAC_APP_STORE` source fork APPLIED + compile-verified** (see §2). Both
      `swift build` (direct-download) and `swift build -Xswiftc -DMAC_APP_STORE`
      (sandboxed subset) link clean. Files touched: `GlobalHotkey.swift`,
      `MenuBandController.swift` (enableTypeMode), `AppDelegate.swift` (stickies
      start), `MenuBandPopover.swift` (picker), `UpdateChecker.swift`,
      `CrashLogReader.swift`. Decision locked: **Pointer + focused typing**
      (Notepat/Ableton layouts work while Menu Band is frontmost via
      `LocalKeyCapture`; no global typing).
- [ ] Xcode app target + archive — see §3 (needs Jeffrey: Apple ID, certs).
- [ ] Screenshots — see §4 (best captured from the signed sandboxed build).

### Environment prerequisites discovered on this Mac (Jeffrey — do these first)

1. **Activate full Xcode** — `xcode-select` currently points at the Command
   Line Tools, so `xcodebuild`/archiving won't run. Xcode.app IS installed:
   ```
   sudo xcode-select -s /Applications/Xcode.app/Contents/Developer
   ```
2. **Mac App Store signing cert is missing.** The keychain has *Developer ID
   Application* (`FB5948YR3S`, used for the notarized DMG) and an *Apple
   Development* cert — but **no Apple Distribution / 3rd Party Mac cert**, which
   App Store submission needs. Easiest: let Xcode "Automatically manage signing"
   create the Apple Distribution cert + Mac App Store provisioning profile on
   first archive (requires being signed into the Apple ID in Xcode ▸ Settings ▸
   Accounts).
3. **Team-ID check.** Developer ID is team `FB5948YR3S`; the Apple Development
   cert is under `88649MN8MK`. Create the App Store Connect app record and sign
   under **the team that owns the bundle id `computer.aestheticcomputer.menuband`**
   (the FB5948YR3S team, per STORE.md). Pick that team in the Xcode signing pane.

---

## 1. The one hard fact

Mac App Store mandates the **App Sandbox**. Several Menu Band features use APIs
or file paths the sandbox forbids. The App Store build must compile-gate them
out behind `#if MAC_APP_STORE` and ship without them.

---

## 2. Sandbox audit — what to gate, what survives

### Must gate OUT (`#if !MAC_APP_STORE` around the code + its call sites)

| Feature | File(s) | Why it's illegal under sandbox |
| --- | --- | --- |
| Global keystroke capture (Notepat/Ableton **global** typing) | `KeyEventTap.swift` + wiring in `MenuBandController.swift` (~L37, L1167, L1783) | `CGEventTap` is forbidden by the sandbox. |
| System-wide hotkeys (⌃⌥⌘P etc., right-⌘ toggle) | `GlobalHotkey.swift` + 5 instances in `AppDelegate.swift` (L16–20, L874–934) | Carbon `RegisterEventHotKey` is forbidden. |
| Stickies typing bridge | `StickiesBridge.swift` (L13 init, L161–180) | Apple Events to another app + its own event tap. App Review rejects this for an instrument. |
| Cmd-cmd launcher daemon | `Sources/MenuBandLauncher/` (separate target) | Background daemon tapping global keys. Drop the target from the App Store build entirely. |
| Auto-update checker + "New Menu Band Available" banner | `UpdateChecker.swift` + banner UI in `MenuBandPopover.swift`/`AboutWindow.swift` | App Review §2.4.5 / 3.2.2: apps must not steer users to update outside the App Store. The App Store handles updates. |
| Crash-log reader + manual upload | `CrashLogReader.swift`, `CrashViewer.swift` | Reads `~/Library/Logs/DiagnosticReports/` — outside the sandbox container; not readable. |
| GarageBand patch picker (the GM/GarageBand toggle) | `GarageBandLibrary.swift`, `GarageBandPatchView.swift` | Would scan `/Library/Application Support/GarageBand` + `~/Music/Audio Music Apps` (outside the container) — **but this is already dormant code** ("deprecated for now", `GarageBandPatchView` is never instantiated), so it never runs and needs no gate. The **built-in GM synth stays** (see below). Left as-is; revisit only if the picker is ever re-surfaced. |

### Keeps working under sandbox (entitlements already in `MenuBand-AppStore.entitlements`)

| Feature | Note |
| --- | --- |
| Pointer mode — click the menubar piano | Core. No special API. |
| **Typing while Menu Band is focused** (`LocalKeyCapture.swift`) | Its own header calls it "MAS-safe": invisible key panel + local `NSEvent` monitor, no CGEventTap, no Accessibility. This lets the App Store build keep **Notepat + Ableton layouts** as long as Menu Band is the active app — a much better product than Pointer-only. **Recommended to keep** (see §2a). |
| Trackpad pitch-bend gesture | Local indirect-touch via `LocalKeyCapture`'s sensor view. Sandbox-legal. |
| Built-in General MIDI synth | Loads Apple's system soundbank `/System/Library/Components/CoreAudio.component/.../gs_instruments.dls` — the same file GarageBand uses; sandboxed apps may read it. **Must verify on a signed run (§5).** |
| Virtual CoreMIDI source ("Menu Band") + MIDI out to DAWs | CoreMIDI virtual sources are sandbox-legal, no entitlement. |
| Microphone sampler (hold `` ` ``) | `com.apple.security.device.audio-input` + the existing `NSMicrophoneUsageDescription`. |
| MIDI-file drag-and-drop (`MidiDropTargetView`/`MidiFilePlayer`) | Dragging a file in is a user-intent gesture; the sandbox grants temporary read access to dropped files. No entitlement needed. |
| KidLisp TV feed, KPBJ radio stream, web window | Outbound HTTPS only → `com.apple.security.network.client`. |

### Needs a judgment call

- **`AestheticWebWindow` (WKWebView loading aesthetic.computer).** Sandbox-legal
  with network.client, but App Review scrutinizes apps that embed web content —
  if the web window is prominent it can trigger a "this is just a web wrapper"
  rejection. For a menubar instrument with an *optional* web panel it's likely
  fine. Be ready to gate it out if review pushes back.

### 2a. Product decision: keep local-focus typing, or strict Pointer-only?

Because `LocalKeyCapture` already exists and is sandbox-safe, the App Store
build does **not** have to be Pointer-only. Two options:

- **Recommended — Pointer + local-focus typing.** Keep Notepat/Ableton layouts
  active while Menu Band is the frontmost app. Loses only *global* typing
  (playing while another app is focused). Far more useful; the code already
  exists. Bigger fork (must reroute the mode wiring from `KeyEventTap` to
  `LocalKeyCapture` for the App Store build).
- **Simplest — strict Pointer-only.** Gate out all typing; mouse + synth + MIDI
  out only. Smallest, safest, fastest-to-submit fork. Ship local typing as a
  fast-follow update.

---

## 3. Build + sign + archive (Jeffrey — needs Apple ID + certs)

The project is SwiftPM-only with **no `.xcodeproj`**. App Store submission
requires the Xcode archive → Organizer flow with **Apple Distribution** signing
and a **Mac App Store provisioning profile** (not the Developer ID cert used for
notarized direct download).

The Xcode app target is pre-specified in **`project.yml`** (XcodeGen) — it wraps
the same `Sources/MenuBand/*.swift`, flattens `Resources/` + ships
`WaveformShaders.metal` as a raw resource + `AppIcon.icns`, sets the
`MAC_APP_STORE` + `XCODE_APP_TARGET` compilation conditions, points at
`MenuBand-AppStore.entitlements` and the repo `Info.plist`, links IOKit, and
selects automatic signing under team `FB5948YR3S`. The `MenuBandLauncher`
target is intentionally omitted. (`Bundle.module` is supplied for this target by
`MASBundleShim.swift`, guarded so it never collides with SwiftPM's accessor.)

1. `brew install xcodegen` (one-time).
2. `cd slab/menuband && xcodegen generate` → writes `MenuBand.xcodeproj`
   (already git-ignored via `*.xcodeproj/`).
3. `open MenuBand.xcodeproj`.
4. **Xcode ▸ Settings ▸ Accounts** — sign in to the Apple ID that owns the
   `FB5948YR3S` team. With automatic signing on, Xcode creates the **Apple
   Distribution** cert + **Mac App Store** provisioning profile on first
   archive (neither exists in the keychain yet — see §0 prereqs).
5. **Product ▸ Archive**, then **Distribute App ▸ App Store Connect ▸ Upload**.

> **Verify in Xcode** (things only a real build confirms): that the flattened
> resources resolve (`sheet.html`, the fonts, the keymaps PDF), that
> `WaveformShaders.metal` is in Copy Bundle Resources (not Compile Sources),
> and that the build picks up `Info.plist` rather than a generated one. If the
> visualizer renders black, the `.metal` got compiled instead of copied.
>
> Alternative without XcodeGen: **File ▸ New ▸ Project ▸ macOS App**, add the
> sources/resources/entitlements manually, set the same two compilation
> conditions. The `project.yml` is just the same recipe, reproducibly.

---

## 4. App Store Connect record

- Create the app record (bundle id `computer.aestheticcomputer.menuband`, name
  "Menu Band", category Music).
- Metadata is ready in `STORE.md` (description, subtitle, keywords, support +
  marketing URLs). **Privacy Policy URL → https://aesthetic.computer/menuband/privacy.html**.
- **App Privacy nutrition label: "Data Not Collected."** (The app collects
  nothing; the update-check + crash-upload are gated out of this build anyway.)
- **What's New:** rewrite for the subset — see §6.
- ≥1 screenshot required (§4 of STORE.md has the shot list; `bin/app-store-screenshots.mjs` exists).

---

## 5. Runtime checks only a signed sandboxed build can answer

Run these on the archived/signed App Store build before submitting — they
cannot be verified by `swift build` or an unsigned binary:

1. **Does sound play?** Confirm the GM synth loads `gs_instruments.dls` inside
   the sandbox (almost certainly yes — GarageBand does it — but verify). If it
   fails, bundle a fallback `.sf2`/`.dls` in `Resources/` and load from the
   bundle instead.
2. **Virtual MIDI source appears** in a DAW's input list and receives notes.
3. **Mic permission prompt** fires on first `` ` `` and recording works.
4. **Local-focus typing** (if kept) plays notes while Menu Band is frontmost and
   silently stops when another app is focused (no Accessibility prompt).
5. **No sandbox violations** in Console.app (filter: `Sandbox` + process
   `MenuBand`) during a full play session.
6. **MIDI file drag-drop** still reads the dropped file.

---

## 6. What's New (App Store, subset)

> Menu Band for the Mac App Store — the built-in macOS instruments in your menu
> bar. Click the little piano to play, or type to play while Menu Band is
> focused. Publishes a virtual MIDI source so any DAW can receive your notes,
> with a built-in General MIDI synth when no DAW is running. (For system-wide
> typing from any app, the full version is a free download at prompt.ac/menuband.)

---

## 7. Realistic timeline

- **Submitting by Sunday night: realistic** if the fork (§2) goes cleanly and
  the signed build passes §5.
- **Live this weekend: no.** Apple review is 1–7 days.
- Biggest risks: the Xcode-target/provisioning setup (§3, first time), and a
  §5 surprise (synth or MIDI behaving differently sandboxed).
