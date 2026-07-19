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

**PUBLISHED — v1.5.4 (build 155) went live on 2026-07-16.** The next patch will
also replace the duplicated 1.5.4 screenshot assets; see the review log below.

**SUBMITTED — v1.5.5 (build 156), 2026-07-18, `WAITING_FOR_REVIEW`.** Lane
order ran as documented (`meta → shots → upload → ship`), but the version
still inherited duplicated screenshot assets (00×2, 01×3 — `shots` even added
a third `01-reel` copy). Fixed via the API before submitting: while the
version is `PREPARE_FOR_SUBMISSION`, `DELETE /v1/appScreenshots/{id}` works —
listed the set's assets with their `sourceFileChecksum`s, kept the first of
each, deleted the rest. `bin/asc.mjs` (new) wraps the JWT + common queries:
`status` / `get <path>` / `sales` / `analytics`.

### Review log

| # | Submitted | Verdict | Guideline | What Apple objected to | Fix |
|---|---|---|---|---|---|
| 1 | Jul 5 (build 153) | rejected Jul 9 | **5.2.5** + **1.5** | The subtitle carried a trademark ("macOS"), and the support URL didn't lead to a real support page. | `ea903abae` — subtitle → *"Instruments in your menu bar"*; wrote `system/public/menuband/support.html` and pointed the support URL at `menuband.app/support.html`. |
| 2 | Jul 9 (build 153) | rejected Jul 11 | **2.4.5(i)** | The app declared `com.apple.security.device.bluetooth` for features the sandboxed build doesn't ship — an entitlement it never uses. | `8fce4eb70` — dropped the entitlement, **build 154**. The MultipeerConnectivity fleet is `#if !MAC_APP_STORE` anyway, and game controllers ride the high-level GameController framework (no `CBCentralManager` anywhere), so paired controllers still work without it. |
| 3 | Jul 11 (build 154) | **approved Jul 12** | — | — | Released same day. |
| 4 | Jul 15 (build 155) | **approved Jul 16** | — | v1.5.4 post-launch fixes and submission hardening. | Released automatically as configured. |
| 5 | Jul 18 (build 156) | pending | — | v1.5.5: menu-bar 3·2·1 count-in, save-anywhere tape export, tape cover art, spoken digits; deduplicated screenshot set. | — |

### Screenshot duplication found after release

The 1.5.4 storefront shows each of its two screenshots twice. App Store Connect
contains four distinct asset records, but each pair has the same filename,
size, and source checksum. Apple refuses screenshot deletion after submission,
including after the version reaches `READY_FOR_SALE`, so this cannot be repaired
in place. Replace the screenshot set on the next version before submitting it.

Cause: both `fastlane mac shots` and `fastlane mac ship` uploaded the screenshot
directory. The Fastfile now gives screenshots a single owner: `shots` replaces
the set once, while `meta` and `ship` both pass `skip_screenshots: true`. For each
new version, run the lanes in this order:

```fish
fastlane mac meta
fastlane mac shots
fastlane mac upload
fastlane mac ship
```

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

- [x] `MenuBand-AppStore.entitlements` created — app-sandbox + audio-input +
      network.client + user-selected read/write for recording export.
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
      (`00-keyboard-menu`, `01-reel`, 1440×900).

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
   under **the team that owns the bundle id `computer.aesthetic.menuband`**
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
| Tape recording export | Store builds return the rendered WAV directly and use `NSSavePanel`; `com.apple.security.files.user-selected.read-write` permits writing only to the destination the user explicitly chooses. The Desktop/DMG and external-ffmpeg paths are compiled out. |
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

- Use the existing app record (bundle id `computer.aesthetic.menuband`, name
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
| 1 | **Never comes back after a reboot.** | There was no login-item code *at all*. The DMG build auto-starts from a LaunchAgent written by `install.sh` — a sandboxed app cannot install one, so the MAS build had no mechanism whatsoever. | New `MenuBandLoginItem.swift` (`#if MAC_APP_STORE`): `SMAppService.mainApp`, reconciled on every launch. Explicitly **opt-in** through the "Open at Login" checkbox in About. `SMAppService` is macOS 13+, so it's `#available`-gated and the toggle hides on 11–12. |
| 2 | Instrument-palette down-arrow too small, undiscoverable, no hover. | The "arrow" was never a control: a `▾` **character** appended to the readout button's attributed title at 11pt / 55% alpha. And the button *is* a `HoverLinkButton`, but its four hover colors were never set and it isn't layer-backed — so `applyState(hovered:)` ran and did nothing. Hover was a dead code path. | Chevron up to 15pt and brighter (0.7 → 1.0 on hover). Added `onHoverChange` to `HoverLinkButton`, and a separate rounded `readoutHoverBackdrop` that fades in behind the name — a real hover pill *without* layer-backing the text, which would have rasterized away its 1px Riso shadow. |
| 3 | "Haptics" switch in a bizarre place. | It's **trackpad Force Touch** feedback (`MenuBandHaptics` probes `AppleMultitouchTrackpadHIDEventDriver`), not controller haptics — but it was wedged into the *instrument title row* of the keymap overlay, so foreign there that a constraint width-matched the instrument **number** label to it just to keep the name optically centered. | Switch, label, info button, and the width-match hack all deleted. Haptics is simply always on, gated by `MenuBandHaptics.isAvailable`. `hapticsEnabled` is now a `let … = true`. |
| 4 | Expanded keymap opens at the **top-left** on a fresh install. | `expandedFrame` resolved `savedExpandedOrigin ?? fallbackOrigin ?? centeredOrigin`. On a new install the saved origin is nil, so it fell to `fallbackOrigin` (= the freshly-built panel's origin) and **never reached `centeredOrigin`**. | Opening always centers on the active screen; an in-place refit keeps its position (that distinction is why `expandedFrame` now takes an explicit `origin`). The dragged position is no longer persisted across opens. |
| 5 | **"Tips" opens the menuband.app website.** | `TipsWindow` loads the bundled help HTML and falls back to `menuband.app/support.html` if it can't find it. `install.sh` copies + indexes + signs `MenuBand.help` for the DMG — but **`project.yml` never copied it**, so in the store build the lookup missed and users got the marketing site. Both Info plists already declared `CFBundleHelpBookFolder`/`Name`, pointing at a folder that wasn't there. | `Help/MenuBand.help` added to the MAS resources, plus an `Index Help Book` postBuildScript running `hiutil` on the copied book (verified: it runs before CodeSign, so the index is signed in). `AppDelegate.openTips()` now prefers the **real Help Viewer** and falls back to the in-app window only if Help Viewer doesn't come up — it can silently no-op for an LSUIElement app, and `showHelp` reports nothing back, so we check whether Help Viewer actually launched. The web fallback is gone. |

**Verified:** `swift build` (direct-download) and `xcodebuild -scheme MenuBand
-configuration Release` (MAS) both compile clean, and the built `.app` carries
`1.5.4 (155)` with `Contents/Resources/MenuBand.help` + a generated
`search.helpindex`. A fresh universal archive and App Store export also succeed;
the exported payload is signed with Apple Distribution and carries only the
expected sandbox, microphone, outbound-network, and user-selected-file
entitlements.

**NOT yet verified** (needs a signed/installed build): that Help Viewer actually
opens for this LSUIElement app, that `SMAppService` registration survives a real
reboot, and the visual result of the hover/centering changes.

**Review hardening:** launch-at-login is off on a fresh install and is registered
only after the user enables the visible checkbox. Tape export is likewise an
explicit user action through `NSSavePanel`; no Store path writes directly to the
Desktop or launches `ffmpeg`, `hdiutil`, or `zip`.

### The slide bug — SOLVED (2026-07-14). It was never in `PianoKeyboardView`.

The entry below used to file two bugs against `PianoKeyboardView`. That was a
misdiagnosis, and it sent every subsequent reader to the wrong file. The
symptom — press a key, drag sideways, the note never changes — reproduces on
the **QWERTY keymap** (`QwertyLayoutView`), not on the piano graphic. The piano
graphic always slid fine. Getting that distinction right *is* the diagnosis:

`QwertyLayoutView.mouseDown` plays a cap by calling `handleLocalKey(...)` — the
**physical-keyboard** path. So the cap's key code lands in `heldNotes`,
indistinguishable from a real keypress, and `keyboardNotesHeld` goes true. That
arms the trackpad pitch-bend, which calls
`CGAssociateMouseAndMouseCursorPosition(0)` and `CGDisplayHideCursor` — it
**decouples the mouse from the cursor and hides it**. The note wasn't locking
under the cursor; the cursor had stopped moving. Of course you couldn't slide.

The guard against exactly this already existed, and said so:

> *"Pitch-bend cursor lifecycle — only engages when the user is playing via the
> KEYBOARD (not mouse taps). Locking the cursor on a mouse-tapped note would
> freeze drag mid-stroke, so we gate on `keyboardNotesHeld`."*

The piano graphic honours that gate because it plays via `startTapNote` (the tap
path). The QWERTY caps defeated it by borrowing the keyboard path.

**Fix:** `handleLocalKey` takes `fromPointer:`, and pointer-held key codes go
into a `pointerHeldKeys` set that `keyboardNotesHeld` excludes — the same shape
as `latchArmedKeys`, which already exists to keep notes that are in `heldNotes`
but not *physically held* from arming the bend. All three cap maps (popover,
collapsed, expanded) pass it. The notes play identically; only the cursor lock
is withheld. Note this means **no trackpad-vs-external-mouse detection is
needed** — the pointer origin is known in our own code.

Trade-off accepted: holding a cap with an external mouse while bending on the
trackpad won't bend. Detectable via `NSEvent` touch data if it ever matters.

### Open, diagnosed but NOT fixed

**The piano key hit-area doesn't match the drawn keys — it extends a full
keyboard above and below.** This one is real and still unfixed.
`PianoKeyboardView.rendererPoint(from:)` is the culprit, and it's on purpose:
with `strictY: false` (the path every mouse handler uses) it accepts `y` from
`-piano.height` to `piano.height * 2`. So the live hit region is three keyboards
tall stacked around a one-keyboard drawing. The comment calls it "generous
±piano-height slack (fat-finger tolerance for holding a note while sliding off
the keys)" — the intent is real, the magnitude is not. A `strictY` path already
exists; the fix is choosing the right tolerance rather than writing new code.

Worth stating plainly: this was **never** the cause of the slide bug, though the
old entry guessed it might be. It can't be. `KeyboardIconRenderer.noteAt()`
picks the note from **x only** — `y` merely selects the black-key band, and has
no upper bound at all — so vertical slack cannot collapse two horizontally
distinct keys onto the same note.
