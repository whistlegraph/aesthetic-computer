# Electron per-window volume slider plan

## Goal
Add a global volume slider on the side of each AC desktop window so each window behaves like its own device, controlling overall audio output by routing through bios.mjs and the speaker worklet.

## Research summary (current state)
- The audio graph is created in `bios.mjs` with `speakerGain` and `sfxStreamGain`, then the speaker worklet connects to `speakerGain` and `sfxStreamGain`. The speaker output is currently routed directly to the destination via `speakerGain`. [system/public/aesthetic.computer/bios.mjs](system/public/aesthetic.computer/bios.mjs#L2708-L2725) and [system/public/aesthetic.computer/bios.mjs](system/public/aesthetic.computer/bios.mjs#L3148-L3164)
- The speaker worklet sets a fixed global volume (0.9) on init via `volume.amount.val`, but there is no message handler to change this after init. [system/public/aesthetic.computer/lib/speaker.mjs](system/public/aesthetic.computer/lib/speaker.mjs#L96-L116)
- The global volume helper is a simple scalar in `lib/sound/volume.mjs`. [system/public/aesthetic.computer/lib/sound/volume.mjs](system/public/aesthetic.computer/lib/sound/volume.mjs#L1-L13)
- The 3D flip window (the desktop app shell) lives in `flip-view.html` and already hosts the AC webview plus custom UI overlays, which is the ideal place to add a vertical slider. [ac-electron/renderer/flip-view.html](ac-electron/renderer/flip-view.html#L660-L760)

## Proposed design
1. **UI slider in the desktop window**
   - Add a vertical range input in the right side “edge” overlay of the flip-view UI (similar to the flip tabs).
   - Show a subtle icon + tooltip, and a small readout (e.g. 0–100) when hovered.
   - Keep the slider out of the webview frame so it stays visible during flips.

2. **Renderer → webview communication**
   - On slider change, call `webviewEl.executeJavaScript(...)` to invoke a stable global setter exposed by `bios.mjs` (e.g. `window.AC.setMasterVolume(value)` or `window.acSetMasterVolume(value)`).
   - Also re-send the last slider value on `dom-ready` or after navigation so the webview always matches the slider.

3. **BIOS audio control API**
   - Add a global setter in `bios.mjs` (e.g. `window.AC.setMasterVolume`) that:
     - Updates `speakerGain.gain.value` to clamp 0–1.
     - Sends a message to the speaker worklet to update `volume.amount.val` (add a new `msg.type === "volume"` handler inside `speaker.mjs`).
   - Optional: Keep a cached `masterVolume` in BIOS for use in any HTML5 audio element (streaming audio or background music) so their internal `volume` multiplies by the master value.

4. **Per-window state**
   - Store the slider value in the flip-view renderer (in-memory + `localStorage`) so each window keeps its own value on reload.
   - On window creation, default to 0.9 (matching the current worklet default) and sync to BIOS once the webview is ready.

## Implementation steps
1. **flip-view UI**
   - Add markup for a vertical volume slider and label in `flip-view.html`.
   - Style it to sit on the right side of the window (outside the card border) and remain visible in both flip states.
   - Persist slider state in `localStorage` and call a `syncVolume()` helper on load + on webview navigation.

2. **Renderer → BIOS command**
   - Use `webviewEl.executeJavaScript` in the flip-view script to call the BIOS setter whenever the slider changes.
   - Add `dom-ready` and navigation hooks so the slider re-applies when the webview reloads.

3. **BIOS volume bridge**
   - Add a `masterVolume` variable in `bios.mjs`.
   - Add a public setter on `window.AC` (and a fallback on `window` if needed) that clamps the value, updates `speakerGain.gain.value`, and posts `type: "volume"` to the speaker worklet.

4. **Speaker worklet support**
   - Extend `speaker.mjs` message handling to accept `type: "volume"` and set `volume.amount.val`.

5. **Optional: apply to HTML5 audio elements**
   - Update `playBackgroundMusic` and `stream:play`/`stream:volume` to multiply by `masterVolume` for parity across all audio paths.

## Files to touch
- [ac-electron/renderer/flip-view.html](ac-electron/renderer/flip-view.html) — slider markup, styles, and renderer logic.
- [system/public/aesthetic.computer/bios.mjs](system/public/aesthetic.computer/bios.mjs) — global setter, master gain updates, worklet messaging.
- [system/public/aesthetic.computer/lib/speaker.mjs](system/public/aesthetic.computer/lib/speaker.mjs) — handle `volume` messages.
- [system/public/aesthetic.computer/lib/sound/volume.mjs](system/public/aesthetic.computer/lib/sound/volume.mjs) — no changes expected, reference only.

## Open questions / checks
- Confirm whether the 3D flip view is the only desktop window in active use, or if `development.html`/`production.html` also need the slider for non-3D windows.
- Decide whether master volume should apply to streaming HTML5 audio and background music or only the speaker worklet.

## Success criteria
- Each Electron window shows its own volume slider and keeps its value across reloads.
- Moving the slider immediately changes audio volume for that window’s AC content.
- No console errors in the webview when the slider value is updated before BIOS is ready.