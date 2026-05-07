# Menu Band App Store Screenshot System

Prepared 2026-05-07 for the Mac App Store submission.

## Reference Pages

- Tailscale: https://apps.apple.com/us/app/tailscale/id1475387142?mt=12
- GarageBand: https://apps.apple.com/us/app/garageband/id682658836?mt=12
- Apple Mac screenshot specs: https://developer.apple.com/help/app-store-connect/reference/app-information/screenshot-specifications/

Apple currently requires Mac app screenshots to be 16:10, using one of these sizes: `1280x800`, `1440x900`, `2560x1600`, or `2880x1800`. For Menu Band, the working master should be `2880x1800`, exported down only if needed. That gives the typography and menubar details room to stay clean after App Store scaling.

## What The References Are Doing

GarageBand's page behaves like a full product screenshot: dense, colorful, and almost entirely inside the app. It trusts the interface to sell the product. The shots are full-bleed application windows with no large marketing text. The appeal is capability: tracks, waveforms, pedals, knobs, loops, drummer controls, and the feeling that a whole studio lives in the app.

Tailscale's Mac page is more relevant to Menu Band. It does not show a big document-style app window. It stages a partial Mac screen, emphasizes the menu bar/system tray context, and places a short tagline to the side. The first screenshot is roughly: cropped laptop/screen on the left, Tailscale menu open from the menu bar, then large bold copy on the right. The tagline copy is blunt and useful, not decorative: "Connect any device, anywhere, securely." The second repeats that system-context composition for sign-in: browser window cropped on the left, large explanatory promise on the right.

Menu Band is technically much closer to Tailscale than GarageBand. It is a menu bar instrument, not a full DAW. But it borrows GarageBand's emotional territory: built-in instruments, colorful notes, MIDI, performance, and playful immediacy. So the best visual direction is Tailscale's layout grammar with GarageBand's musical density inside the actual Menu Band surface.

## Recommended Direction

Use a hybrid screenshot system:

1. Capture or render the real Menu Band UI from Swift/AppKit fixtures.
2. Composite those captures into App Store-safe `2880x1800` canvases with a Tailscale-style tagline column.
3. Keep one or two shots closer to GarageBand's density when showing MIDI/DAW use, but do not make GarageBand the main model.

This gives us truthful UI, repeatable exports, and editorial control over the App Store storytelling. Pure live screenshots are too brittle for a menu bar app. Pure mocks are fast but will drift from the product. The hybrid keeps the app honest and lets the store page feel intentional.

## Capture Options

### Option A: Fully Automated Live App Screenshots

Launch the built app, drive it into specific states, open the menu bar popover, and capture the screen with `screencapture`, `CGWindowListCreateImage`, or an accessibility-driven script.

This is useful for final proof screenshots because it captures the true menu bar, true popover material, true shadows, and any current OS appearance. It is also the most fragile route. Menu bar item placement changes with screen size, notch, other status items, time/date, permissions, first-run state, and global keyboard capture. It can be made to work, but it should not be the only production path.

Best use: final verification pass and occasional "real machine" assets.

### Option B: Swift/AppKit Screenshot Fixtures

Add a small screenshot harness that reuses Menu Band's existing AppKit views and controller state, but renders them in deterministic offscreen windows or `NSImage` contexts. The current code is AppKit-first, with reusable surfaces such as `MenuBandPopoverViewController`, `MenuBandPopoverPanel`, `KeyboardIconRenderer`, `QwertyLayoutView`, `StaffView`, `InstrumentMapView`, `GarageBandPatchView`, `WaveformView`, and `MenuBandController`.

The cleanest implementation would be to add a lightweight capture mode:

```sh
MENUBAND_CAPTURE=popover-pointer swift run MenuBand
MENUBAND_CAPTURE=popover-ableton swift run MenuBand
MENUBAND_CAPTURE=menubar-lit swift run MenuBand
```

In capture mode, the app would avoid global side effects, seed the controller with known state, render the popover or menu bar chip into an offscreen image, and write PNGs into something like `slab/menuband/screenshots/raw/`.

This is the best technical core. It keeps the UI real, can run repeatedly, and can become part of release preparation. It may require small refactors so UI state can be seeded without physically pressing keys or relying on timers.

Best use: canonical raw UI captures for every store screenshot.

### Option C: Separate Mock/Compositor Scripts

Build all App Store screenshots in scripts, likely Node Canvas, Playwright/HTML, or Python/Pillow. These scripts would draw a macOS desktop frame, menubar, Menu Band chip, popover-like panel, and tagline copy.

This is fastest for layout experiments and very good for the Tailscale-style outer composition. It is not ideal for the app UI itself because Menu Band's popover is detailed and actively evolving. Recreating it by hand would age quickly.

Best use: outer App Store canvas, taglines, background, device/screen crop, and export resizing.

## Best System

Build a two-layer pipeline.

Layer 1 is a Swift capture harness. It produces raw, transparent or tightly-cropped PNGs of the actual Menu Band states:

- menu bar chip, idle
- menu bar chip, notes lit
- popover in Pointer mode
- popover in Notepat mode
- popover in Ableton mode
- instrument picker open
- MIDI output/self-test state
- optional DAW integration state, if we can stage it cleanly

Layer 2 is a composition script. It takes those raw captures and places them into polished `2880x1800` App Store frames:

- light background, not a busy gradient
- cropped Mac desktop/screen region on the left
- Menu Band UI large enough to read
- one bold tagline on the right, Tailscale-style
- one short support line under the tagline
- output filenames numbered in App Store order

The compositor should be data-driven. A simple manifest is enough:

```json
[
  {
    "slug": "01-menubar-instrument",
    "raw": "raw/popover-pointer.png",
    "headline": "Built-in instruments, in your menu bar",
    "body": "Tap a tiny piano, switch sounds, and start playing without opening a studio."
  },
  {
    "slug": "02-type-to-play",
    "raw": "raw/popover-notepat.png",
    "headline": "Type letters. Hear notes.",
    "body": "Pointer, Notepat, and Ableton-style keymaps turn your keyboard into an instrument."
  }
]
```

This makes future updates cheap. When the UI changes, regenerate raw captures. When the store story changes, edit the manifest and recompose.

## Proposed Screenshot Set

### 1. Built-in instruments, in your menu bar

Hero shot. Tailscale-style composition. Left side: cropped Mac screen corner with Menu Band visible in the menu bar and the popover open. The popover should show the instrument readout, keyboard/staff area, and a few lit notes. Right side: large headline.

Headline: `Built-in instruments, in your menu bar`

Body: `Tap the tiny piano, pick a sound, and start playing without opening a studio.`

### 2. Type letters. Hear notes.

Show the keymap surface. The QWERTY mapping should be visually prominent, with a few keys/notes active. This is where Menu Band becomes immediately legible as more than a novelty menu icon.

Headline: `Type letters. Hear notes.`

Body: `Pointer, Notepat, and Ableton-style layouts turn your keyboard into an instrument.`

### 3. Send MIDI to any DAW

This is the most GarageBand-adjacent screenshot. Use a DAW or DAW-like context if possible, with Menu Band's virtual MIDI source visible or implied. If using a real third-party DAW screenshot creates review or licensing awkwardness, stage this as Menu Band's MIDI mode/self-test panel plus a clean "virtual MIDI source" indication.

Headline: `Send MIDI to your studio`

Body: `Menu Band publishes a virtual MIDI source for DAWs and instrument plugins.`

### 4. Choose from a full instrument palette

Show the instrument picker or General MIDI patch list. This is the GarageBand emotional bridge: many sounds, fast switching, colorful musical possibility.

Headline: `A pocket orchestra of sounds`

Body: `Jump from piano to strings, brass, synths, drums, and more.`

### 5. Tiny surface, live feedback

Show the menu bar chip lit up, waveform/staff/chord feedback visible, or held-note/chord cards if the current UI state supports it. This reinforces that the tiny menu bar app is alive.

Headline: `Small enough to stay open`

Body: `Notes, chords, levels, and octave changes stay visible while you work.`

## Visual Rules

The primary screenshot style should be calm, mostly white or very light gray, with black/near-black typography. This is the Tailscale lesson: simple system utility, instantly readable, no visual shouting. Menu Band can bring color inside the product surface through notes, waveform, instrument badges, and keyboard highlights.

Avoid a full GarageBand-style wall of controls for the first screenshot. It makes Menu Band look like a DAW, which it is not. Use GarageBand's color and musical credibility as a seasoning, especially in shots 3 and 4.

Taglines should be short enough to read in the App Store carousel. Recommended type scale at `2880x1800`:

- headline: 120-150 px, bold, 2-3 lines max
- body: 54-68 px, regular/medium, 2-3 lines max
- UI capture: large enough that key labels and instrument names survive downscaling

Do not rely on tiny text in the UI as the only explanation. The screenshot should be understandable from the headline alone, with the UI confirming it.

## Technical Plan

### Phase 1: Capture Harness

Add a capture mode to the existing executable or a separate SwiftPM executable target. A separate target is cleaner long-term, but the current `Package.swift` only defines the `MenuBand` executable target, so the lowest-friction start is an environment-gated mode in `main.swift` or `AppDelegate`.

Suggested interface:

```sh
swift run MenuBand --capture popover-pointer --out screenshots/raw/popover-pointer.png
swift run MenuBand --capture popover-ableton --out screenshots/raw/popover-ableton.png
swift run MenuBand --capture instrument-picker --out screenshots/raw/instrument-picker.png
swift run MenuBand --capture menubar-lit --out screenshots/raw/menubar-lit.png
```

Implementation notes:

- register bundled fonts before rendering, as the app already does in `AppDelegate`
- create `MenuBandController` with deterministic defaults
- add a small fixture API for held notes, current instrument, input mode, octave, MIDI mode, and chord candidates
- instantiate `MenuBandPopoverViewController` directly for popover states
- render views with `bitmapImageRepForCachingDisplay(in:)` or a window-backed capture
- avoid starting event taps, hotkeys, timers, network checks, or LaunchAgent behavior in capture mode
- write raw PNGs to `screenshots/raw/`

Some UI is currently private inside controllers, which is good application design but slightly awkward for screenshots. Prefer adding small fixture hooks over exposing whole internals.

### Phase 2: App Store Compositor

Create `bin/app-store-screenshots.mjs` or `bin/app-store-screenshots.py`.

Inputs:

- `screenshots/app-store.json` manifest
- raw PNGs from Swift capture
- optional desktop wallpaper/background asset
- fonts from `Sources/MenuBand/Resources/` or system SF fonts

Outputs:

- `screenshots/app-store/01-menubar-instrument-2880x1800.png`
- `screenshots/app-store/02-type-to-play-2880x1800.png`
- `screenshots/app-store/03-send-midi-2880x1800.png`
- `screenshots/app-store/04-instrument-palette-2880x1800.png`
- `screenshots/app-store/05-live-feedback-2880x1800.png`

The compositor should validate that each output is exactly `2880x1800`, and optionally export `1440x900` previews for quick review.

### Phase 3: Live Verification Capture

Once the generated set looks right, run the real app and take one manual or scripted live screenshot to verify that the generated captures still match reality. This is not the production path; it is the final smell test.

### Live Capture Black-Screen Note

During the first test run on 2026-05-07, `screencapture` initially produced an all-black image after the desktop had been aggressively cleaned by hiding apps, disabling Finder desktop icons, and restarting Finder. A later diagnostic capture with a visible normal window succeeded, so this was not a permanent Screen Recording permission failure. The practical fix is to avoid capturing from an over-hidden/blank Space. Keep a visible neutral backdrop or Finder desktop painted before invoking `screencapture`.

Recommended live-capture setup:

1. leave Finder desktop icons enabled: `defaults write com.apple.finder CreateDesktop -bool true; killall Finder`
2. use a fresh Space or a neutral full-screen backdrop window rather than hiding every visible process
3. launch Menu Band and open the desired popover state
4. run `screencapture -x screenshots/live/<name>.png`
5. immediately inspect the result for non-black pixels before using it as source material

If a black image returns again while a visible window is definitely onscreen, then check System Settings > Privacy & Security > Screen & System Audio Recording and grant access to the terminal/automation host being used for `screencapture`.

## App Store Review Considerations

Screenshots should represent the actual App Store build. This matters because the current `STORE.md` notes that a sandboxed Mac App Store build may need to be Pointer-only unless global key capture and hotkeys are removed or gated. If the submitted App Store build omits Notepat/Ableton global capture, the screenshots should not promise unavailable global typing. In that case, keep "Type letters" only if local focused typing remains available in the App Store build, and avoid claims that imply background keystroke capture.

Similarly, be careful with GarageBand references. The app can say it uses built-in macOS instruments or supports DAW MIDI output, but screenshots should not visually imply that Menu Band is GarageBand or bundled with GarageBand. If using a DAW screenshot, use our own app surface and generic wording unless there is a clear reason to show a specific third-party app.

## Recommendation

Do not choose between automation and mockups. Use automation for the app UI and mock/composition scripts for the store frame. That is the sweet spot for Menu Band.

The first App Store pass should be five Tailscale-inspired screenshots at `2880x1800`: left side real Menu Band UI in a Mac menu bar context, right side bold tagline plus a single support sentence. This fits the product's true category, gives the page a confident system-utility feel, and still lets the musical richness come through where it matters.

The minimum useful implementation is:

1. add a deterministic Swift capture mode for `popover-pointer`, `popover-ableton`, `instrument-picker`, and `menubar-lit`
2. add a manifest-driven compositor script
3. export five final PNGs into `screenshots/app-store/`
4. update `STORE.md` with the final screenshot filenames once generated
