# Menu Band — Mac App Store submission

Companion to `STORE.md` (which covers the direct-download DMG). This file is
the App Store-specific record: the sandbox audit, what the code forks out, the
signing/archive steps, the runtime checks only a signed build can answer — and
the review log. Written 2026-05-29 against v1.4.1 (build 141) as a plan; kept
since as the standing account of what Apple has said back.

The App Store build ships as a **reduced, sandboxed subset** of the
direct-download version. The full version stays at prompt.ac/menuband.

---

## 0. Where it stands

**PUBLISHED — v1.5.3 (build 154) went live on the Mac App Store 2026-07-12.**

> **https://apps.apple.com/us/app/menu-band/id6767311903**

App Apple ID `6767311903`, SKU `MENUBAND`, bundle id `computer.aesthetic.menuband`,
**$4.99**, Music category, macOS 11.0+, seller "Jeffrey Scudder". Approved on the
third submission; the shipped commit is tagged **`menuband-v1.5.3`**. It was
indexed in App Store search immediately (top hit for "menu band") rather than
taking the usual day.

**IN PROGRESS — v1.5.4 (build 155):** the first patch, from bugs found by
running the actual shipped store build. Not yet submitted. See §8.

### Review log

| # | Submitted | Verdict | Guideline | What Apple objected to | Fix |
|---|---|---|---|---|---|
| 1 | Jul 5 (build 153) | rejected Jul 9 | **5.2.5** + **1.5** | The subtitle carried a trademark ("macOS"), and the support URL didn't lead to a real support page. | `ea903abae` — subtitle → *"Instruments in your menu bar"*; wrote `system/public/menuband/support.html` and pointed the support URL at `menuband.app/support.html`. |
| 2 | Jul 9 (build 153) | rejected Jul 11 | **2.4.5(i)** | The app declared `com.apple.security.device.bluetooth` for features the sandboxed build doesn't ship — an entitlement it never uses. | `8fce4eb70` — dropped the entitlement, **build 154**. The MultipeerConnectivity fleet is `#if !MAC_APP_STORE` anyway, and game controllers ride the high-level GameController framework (no `CBCentralManager` anywhere), so paired controllers still work without it. |
| 3 | Jul 11 (build 154) | **approved Jul 12** | — | — | Released same day. |

**The QuickLook payoff.** macOS will not register a locally-built QuickLook
extension — the `.mbscore` piano-roll preview only loads from a *notarized*
bundle, which is why it never worked on dev installs. The MAS build is notarized,
so this is the first channel where ⌘-Space on an `.mbscore` file actually renders.

**The lesson from round 2, worth keeping:** review reads the entitlements
against what the app *actually does*, not against what the codebase could do.
Every entitlement in `MenuBand-AppStore.entitlements` must be traceable to a
code path that ships **in the MAS build** — a `#if !MAC_APP_STORE` feature is
not a justification. That file now carries a "NOT included, and why" block;
keep it truthful, and prune the entitlement whenever a feature gets gated out.

### Prep (all done)

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
- [x] Xcode app target + archive — see §3. Archived and uploaded; the signing
      prerequisites below are all resolved.
- [x] Screenshots — see §4. Live in `fastlane/screenshots/en-US/`
      (`00-overview`, `01-keymap`, 1440×900).

### Environment prerequisites (resolved — kept as the record of what it took)

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
> with a built-in General MIDI synth when no DAW is running.

---

## 7. What review has actually cost

The original estimate here ("submitting by Sunday night, live in 1–7 days")
was right about the submission and wrong about the shape of the wait. The real
cost has not been the archive or the provisioning — those worked. It's the
round-trip: **each rejection burns 2–4 days**, most of it sitting in the queue
waiting for a reviewer, and the fixes themselves have each taken under an hour.

Observed cadence, for planning the next version:

- **Queue → In Review:** 2–4 days (Jul 5 → Jul 9; Jul 9 → Jul 11).
- **In Review → verdict:** under an hour, both times.
- **Rejection → resubmitted:** same day, both times.

So the thing to optimize is *not being rejected*, not turnaround. Both
rejections were avoidable pre-flight checks, and neither was about the app's
behavior — one was metadata copy, one was an unused entitlement. Before the
next submission, re-read §2's table and the "NOT included, and why" block in
`MenuBand-AppStore.entitlements` against the code that actually ships, and read
every metadata string in `fastlane/metadata/en-US/` for trademarks.

---

## 8. v1.5.4 (build 155) — the first post-launch patch

Five bugs, all found by installing the **actual store build** from the App Store
and using it. Worth stating why that mattered: three of the five exist *only* in
the sandboxed build, so no amount of testing the direct-download app would have
surfaced them. Install from the store before believing the store build works.

| # | Symptom | Root cause | Fix |
|---|---|---|---|
| 1 | **Never comes back after a reboot.** | There was no login-item code *at all*. The DMG build auto-starts from a LaunchAgent written by `install.sh` — a sandboxed app cannot install one, so the MAS build had no mechanism whatsoever. | New `MenuBandLoginItem.swift` (`#if MAC_APP_STORE`): `SMAppService.mainApp`, reconciled on every launch. Defaults **on** (parity with the LaunchAgent), with an "Open at Login" checkbox in About. `SMAppService` is macOS 13+, so it's `#available`-gated and the toggle hides on 11–12. |
| 2 | Instrument-palette down-arrow too small, undiscoverable, no hover. | The "arrow" was never a control: a `▾` **character** appended to the readout button's attributed title at 11pt / 55% alpha. And the button *is* a `HoverLinkButton`, but its four hover colors were never set and it isn't layer-backed — so `applyState(hovered:)` ran and did nothing. Hover was a dead code path. | Chevron up to 15pt and brighter (0.7 → 1.0 on hover). Added `onHoverChange` to `HoverLinkButton`, and a separate rounded `readoutHoverBackdrop` that fades in behind the name — a real hover pill *without* layer-backing the text, which would have rasterized away its 1px Riso shadow. |
| 3 | "Haptics" switch in a bizarre place. | It's **trackpad Force Touch** feedback (`MenuBandHaptics` probes `AppleMultitouchTrackpadHIDEventDriver`), not controller haptics — but it was wedged into the *instrument title row* of the keymap overlay, so foreign there that a constraint width-matched the instrument **number** label to it just to keep the name optically centered. | Switch, label, info button, and the width-match hack all deleted. Haptics is simply always on, gated by `MenuBandHaptics.isAvailable`. `hapticsEnabled` is now a `let … = true`. |
| 4 | Expanded keymap opens at the **top-left** on a fresh install. | `expandedFrame` resolved `savedExpandedOrigin ?? fallbackOrigin ?? centeredOrigin`. On a new install the saved origin is nil, so it fell to `fallbackOrigin` (= the freshly-built panel's origin) and **never reached `centeredOrigin`**. | Opening always centers on the active screen; an in-place refit keeps its position (that distinction is why `expandedFrame` now takes an explicit `origin`). The dragged position is no longer persisted across opens. |
| 5 | **"Tips" opens the menuband.app website.** | `TipsWindow` loads the bundled help HTML and falls back to `menuband.app/support.html` if it can't find it. `install.sh` copies + indexes + signs `MenuBand.help` for the DMG — but **`project.yml` never copied it**, so in the store build the lookup missed and users got the marketing site. Both Info plists already declared `CFBundleHelpBookFolder`/`Name`, pointing at a folder that wasn't there. | `Help/MenuBand.help` added to the MAS resources, plus an `Index Help Book` postBuildScript running `hiutil` on the copied book (verified: it runs before CodeSign, so the index is signed in). `AppDelegate.openTips()` now prefers the **real Help Viewer** and falls back to the in-app window only if Help Viewer doesn't come up — it can silently no-op for an LSUIElement app, and `showHelp` reports nothing back, so we check whether Help Viewer actually launched. The web fallback is gone. |

**Verified:** `swift build` (direct-download) and `xcodebuild -scheme MenuBand
-configuration Release` (MAS) both compile clean, and the built `.app` carries
`1.5.4 (155)` with `Contents/Resources/MenuBand.help` + a generated
`search.helpindex`.

**NOT yet verified** (needs a signed/installed build): that Help Viewer actually
opens for this LSUIElement app, that `SMAppService` registration survives a real
reboot, and the visual result of the hover/centering changes.

**Review watch-item:** #1 makes the app register itself as a login item on first
launch (default on). That's normal for a menu-bar utility and there's a visible
off switch in About, but if review objects, flip the `MBOpenAtLogin` default to
`false` in `MenuBandLoginItem` and let the user opt in.

### Open, diagnosed but NOT fixed (found 2026-07-12 on the shipped build)

Two piano-input bugs, deliberately left out of 1.5.4 rather than patched on a
guess. Both are in `PianoKeyboardView`.

1. **The key hit-area doesn't match the drawn keys — it extends a full keyboard
   above and below.** `rendererPoint(from:)` is the culprit, and it's on
   purpose: with `strictY: false` (the path every mouse handler uses) it accepts
   `y` from `-piano.height` to `piano.height * 2`. So the live hit region is
   three keyboards tall stacked around a one-keyboard drawing. The comment calls
   it "generous ±piano-height slack (fat-finger tolerance for holding a note
   while sliding off the keys)" — the intent is real, the magnitude is not. A
   `strictY` path already exists; the fix is choosing the right tolerance rather
   than writing new code.

2. **Click-and-hold works, but you can't slide to another key.** The note locks
   under the cursor. `mouseDragged` DOES implement glissando (stop the old note,
   start the new one) — but only when the hovered note *differs* from the held
   one. When it's the same, it falls to `updateTapPan(...)` and reinterprets the
   movement as **expression** (pan/velocity), which is what the drag feels like:
   a pan knob instead of a glissando. Whether the note genuinely never changes
   (drag events not reaching the view) or merely doesn't change *near* the press
   point (bug 1's stretched geometry mapping neighbours onto the same note) is
   UNRESOLVED — and the two have different fixes. The deciding experiment: press
   a key and drag several keys sideways at the vertical middle. If the note
   eventually changes, bug 1 is the whole story. If it never does, drag delivery
   is broken and that's separate.

   One unverified suspect for the "never changes" case: `mouseDown` calls
   `window?.makeKey()`, and making the panel key mid-click can disturb AppKit's
   drag tracking. Hypothesis only — do not fix on it without evidence.
