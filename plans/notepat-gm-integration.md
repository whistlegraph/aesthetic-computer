# Notepat GM Integration Plan

## 1. Conflict audit — top-row digits 0–9 in notepat.mjs

I searched all keyboard handlers for digit bindings.

**Result: there are NO existing keyboard handlers for digit keys 0–9 in notepat.mjs.** Confirmed via:
- `grep -nE 'keyboard:down:[0-9]'` — zero hits.
- `grep -nE '"[0-9]"'` — only finds glyph-resolution lookups (typeface lookups for the character "0"), the `buttonOctaves` array (line 1058 — listing octave *numbers* as strings used as octBtn labels, not key bindings), DAW query parsing (line 1468), and HID scancode→key tables (lines 30–31, used by NuPhy WebHID, not the keyboard event bus).

**Adjacent bindings that are nearby on the keyboard but not on digit row:**
- `,` (comma) and `.` (period) — `upperOctaveShift -=/+= 1` (lines 6535, 6539). Keep — not on digit row.
- `-` and `=` — paint overlay toggles (lines 6626–6627). Keep.
- `tab` — cycles `waveIndex` (line 6613–6620). **Repurpose / keep:** still cycles wave but now wave list begins with `"gm"`.
- `space` — metronome toggle (line 7281). Keep.
- `arrowup/down/left/right` — drum pads (lines 7312–7334). Keep.
- `alt` — crash/ride drums (lines 7300–7310). Keep.
- `shift`, `enter`, `backspace`, `escape`, `/`, `\`, `` ` `` — various toggles. Keep.

**Conclusion: digit row is fully free.** No conflicts. All 10 digits (0–9) can be claimed for the GM digit-buffered picker without breaking anything.

The NuPhy HID table at lines 30–31 maps HID scancodes to digit *strings* — that is hardware-side translation, not an `act` handler, and the resulting `keyboard:down:0` … `keyboard:down:9` events flow through the same dispatcher that currently ignores digits. Once we add a digit handler, NuPhy digit presses will pick GM patches just like an OS keyboard. (Worth a one-line comment in the code.)

## 2. State changes

New module-level state to add near line 393 (next to `wave`):

```js
// GM (General MIDI) sound backend — see lib/gm.mjs
let gmReady = false;             // True once gm.loadManifest resolves.
let gmManifest = null;           // Cached manifest from gm.mjs.
let gmProgram = 0;               // Current GM program 0..127 (0 = Acoustic Grand Piano).
let gmPatch = null;              // Currently loaded patch handle from gm.loadPatch.
let gmPatchLoading = false;      // In-flight guard.
let gmDrumKit = null;            // Optional GM drum kit handle.
let gmDigitBuffer = "";          // "0".."999" while user types.
let gmDigitBufferDeadline = 0;   // performance.now() when buffer auto-commits.
const GM_DIGIT_TIMEOUT_MS = 700; // Auto-commit window (matches user spec).
const GM_PROGRAM_NAMES = [/* 128 GM names */]; // For the HUD label.
// Map of active GM voices: voiceId -> noteHandle returned by patch.play(midi).
const gmVoices = new Map();
```

Existing variables changed:

- `wavetypes` (line 380) becomes `["gm", ...legacyWavetypes]`. Keep the legacy names exactly so saved settings still work.
- `STARTING_WAVE` (line 392) → `"gm"`.
- `waveIndex` (line 391) → `0` (still index zero of new array).
- `shortWaveNames` (line 8504) — add `gm: "GM"`.
- `displayWave` formatting (line 8514) — when `wave !== "gm"`, optionally prefix with `"L:"` to make it visually clear the user is in legacy mode (decision needed — see §8).

## 3. Sound dispatch changes

The unified branch point lives in `makeNoteSound(tone, velocity, pan)` at **line 5967**. The current `if/else if/else` ladder distinguishes `stample`/`sample` → `play()`, `composite` → multi-synth, default → single `synth({ type: synthType })`.

Plan: add `wave === "gm"` as the **first** branch. This branch must:

1. Convert notepat's `tone` (a string like `"4C"`, `"5G#"`) to a MIDI note number. Use `soundContext.freq(tone)` to get Hz, then `12 * log2(hz/440) + 69`. Or expose a helper from `lib/note-colors.mjs` if one already does this. (Decision: add a tiny inline `toneToMidi(tone)` helper at module scope; see §8.)
2. Bail to oscillator path (`return synth({ type: "sine", … })`) if `!gmReady || !gmPatch` — graceful fallback while GM is still loading or assets missing.
3. Call `const noteHandle = gmPatch.play(midi, { velocity, pan });`
4. Wrap in a uniform return object with `kill(fade)` → `noteHandle.release()` and a no-op `update()` (or a real `update()` if the gm.mjs API lands one — currently it does not, per the agent's prompt).
5. Track in `gmVoices` keyed by the `voiceId` so panic / clearHeldVoices can reach them.

Drum case: at **line 6146** the existing `wave === "drum"` branch calls `playPercussion(...)`. Add a parallel branch *above* it: `if (wave === "gm" && /* user mapped drum kit */) { gmDrumKit?.play(midiDrumNote); return true; }`. **Decision needed (§8):** simplest cut is to keep `wave === "drum"` doing the existing percussion lib, and let GM stay melodic-only for v1. Drums via GM kit can be a follow-up — the bake agent is producing per-instrument packs anyway and a GM drum kit is patch 128 conceptually.

Pitch bend in `applyPitchBendToNotes` (line 6082) needs a GM branch too — for v1 we can skip live pitch bend on GM voices (just return early when `wave === "gm"`) since the gm.mjs API doesn't expose a per-voice frequency setter. Document this limitation.

## 4. Tab / legacy UX

**`wavetypes` ordering (line 380):**

```js
const wavetypes = [
  "gm",       // 0 — General MIDI (default)
  "sine",     // 1
  "triangle", // 2
  "sawtooth", // 3
  "square",
  "harp",
  "whistle",
  "composite",
  "stample",
  "drum",
];
```

Tab key (line 6613) and waveBtn `push` (line 7383) already cycle `waveIndex = (waveIndex + 1) % wavetypes.length`. No change to that mechanism — they automatically include "gm" as position 0.

**Display label (`buildWaveButton` ~line 8500):** when `wave === "gm"` the button label is `GM:078` (current program shown so the user always knows what's loaded). When in any legacy wave, the label keeps the wave name (`sine`, `tri`, etc.). Optional polish: dim the legacy labels or add a `[L]` prefix to signal they're not the default.

The drum special case at line 1133 comment stays — `wave === "drum"` still routes through `lib/percussion.mjs`.

## 5. Digit-buffered picker — pseudocode

Insertion point: in `act` near the existing tab handler (line 6613), well above the percussion arrow handlers.

```js
// GM patch picker — type a 1-3 digit decimal program number.
// Mirrors menubands behavior with one addition: a 700ms timeout
// auto-commits and clears the buffer if the user pauses, matching
// the spec. Buffer also clears on any non-digit key press, on
// reaching 3 digits, or on a note key being struck.
{
  const digitMatch = e.is("keyboard:down") && /^[0-9]$/.test(e.key) && !e.repeat;
  const now = performance.now();

  // Auto-commit on timeout before processing this event.
  if (gmDigitBuffer && now > gmDigitBufferDeadline) {
    gmDigitBuffer = "";
  }

  if (digitMatch) {
    if (gmDigitBuffer.length >= 3) gmDigitBuffer = "";
    gmDigitBuffer += e.key;
    gmDigitBufferDeadline = now + GM_DIGIT_TIMEOUT_MS;

    const v = parseInt(gmDigitBuffer, 10);
    // Decision: notepat does not have menuband's MIDI-passthrough
    // mode, so "0" / "00" / "000" maps to GM program 0 (Acoustic
    // Grand Piano) instead of toggling a passthrough. Document this
    // divergence from menuband. See §8.
    const program = v === 0 ? 0 : Math.max(0, Math.min(127, v - 1));

    // Switch to GM if the user is currently on a legacy wave, so
    // typing a digit always produces a GM voice. (Optional — could
    // also just queue the program for next time wave=="gm". Pick
    // the auto-switch behavior; it's friendlier.)
    if (wave !== "gm") {
      waveIndex = wavetypes.indexOf("gm");
      wave = "gm";
      buildWaveButton(api);
    }

    setGmProgram(program); // async: loads patch, swaps gmPatch on resolve.
    api.beep(); // Subtle audio confirmation.
    return;
  }

  // Non-digit key: clear the buffer (don't auto-commit anything new
  // — the program was already applied live on each digit).
  if (e.is("keyboard:down") && gmDigitBuffer) {
    gmDigitBuffer = "";
  }
}
```

Helper:

```js
async function setGmProgram(program) {
  gmProgram = program;
  if (gmPatchLoading) return; // Latest call wins via re-check after await.
  gmPatchLoading = true;
  try {
    const next = await gm.loadPatch(program, window.audioContext);
    if (program === gmProgram) {
      gmPatch = next; // Apply only if user hasn't typed past us.
    }
    buildWaveButton(api); // Refresh "GM:078" label.
  } catch (err) {
    console.warn("🎼 GM load failed", program, err);
  } finally {
    gmPatchLoading = false;
  }
}
```

Debug HUD: `gmDigitBuffer` is rendered as `GM:078` while typing in the existing top-bar status row (next to the wave button) so the user sees the partial buffer live. After commit, the label shows the full program name (e.g., `78 Whistle`).

Note key resets buffer: in `startButtonNote` (line 6123) and `startRelayButtonNote` (line ~5780), add `gmDigitBuffer = ""` at the top — mirrors menuband line 1176.

## 6. MIDI input parity

**Recommend yes** — incoming MIDI program-change messages should switch the GM patch.

In `lib/midi.mjs` line 13, the filter `if (command !== NOTE_ON && command !== NOTE_OFF && command !== PITCH_BEND) return;` drops program-change. Add `0xC0` (PROGRAM_CHANGE) to the allow-list and forward it via `acSEND`.

In notepat.mjs `act`'s `midi:keyboard` block (line 7233), after the existing `MIDI_PITCH_BEND` branch, add:

```js
const MIDI_PROGRAM_CHANGE = 0xC0;
if (command === MIDI_PROGRAM_CHANGE) {
  const program = e.data?.[1] ?? 0;
  setGmProgram(Math.max(0, Math.min(127, program)));
  return;
}
```

This keeps the relay/MIDI plumbing in the existing single `midi:keyboard` event channel; no new event type needed.

## 7. Step-by-step implementation order

1. **Add `gm.mjs` import + state (~30 lines).** Top of notepat.mjs near other imports. Add module-level state from §2. *Blocks on:* parallel agent's gm.mjs landing — but the import will simply 404 until then; we can guard with a try/catch around `await import(...)`. For development, a stubbed local gm.mjs that no-ops is sufficient. **Can start immediately.**
2. **Extend `wavetypes` array + `displayWave` map (~10 lines).** Lines 380, 8504. Independent — can land first.
3. **Boot-time GM init (~20 lines).** In `boot()` around line 1457, after `setSoundContext`, call `await gm.loadManifest(window.audioContext)` and `setGmProgram(0)`. Set `gmReady = true`. *Blocks on:* gm.mjs API.
4. **`makeNoteSound` GM branch (~25 lines).** Line 5967. Add the early-exit gm path with fallback. *Blocks on:* gm.mjs play API.
5. **Stop-path / panic / pitch-bend GM handling (~20 lines).** `stopButtonNote`, `clearHeldVoices`, escape panic block (line 6640), `applyPitchBendToNotes` early-return for gm. Independent of gm.mjs once the play path is shaped — uses the returned handle's `release()`.
6. **Digit picker handler (~35 lines).** Insert in `act()` around line 6620 (just after the tab/wave handler so digits can't clobber tab cycling). Add `gmDigitBuffer = ""` resets in `startButtonNote` and `startRelayButtonNote`. Independent.
7. **Wave-button label refresh (~5 lines).** `buildWaveButton` line 8500 — when `wave === "gm"`, label is `GM:NNN`.
8. **MIDI program-change forwarding (~10 lines).** `lib/midi.mjs` (allow 0xC0) + notepat.mjs midi:keyboard branch. Coordinate with the midi.mjs owner — minor edit, low risk.
9. **Bake-output verification (manual).** Once the bake agent publishes to `assets.aesthetic.computer/gm/`, smoke-test by switching to a few patches (1, 25, 78, 128) and a drum kit. *Blocks on:* bake landing.
10. **Optional polish.** GM drum kit routing, sustain/pan parity for GM voices, persisted last-program in `store`.

Total disk-side change ≈ 150–180 net lines added, ~10 lines edited.

## 8. Risks / open questions — decisions needed

1. **0 / 00 / 000 mapping.** Menuband uses these for "MIDI passthrough." Notepat has no equivalent backend slot. **Recommended decision:** map `0`/`00`/`000` to GM program 0 (Acoustic Grand). Document the divergence in a code comment. Alternative: use `0` to *toggle MIDI passthrough* — i.e., temporarily disable GM and let the user's external synth handle voicing. Probably overkill for v1.
2. **Auto-switch wave on digit press?** Plan above auto-switches `wave` to `"gm"` when the user types a digit while in legacy mode. Alternative: queue the program but stay in legacy mode. **Recommended:** auto-switch — matches user intent ("I'm picking an instrument").
3. **gm.mjs API timing.** If notepat ships before `gm.mjs` does, the import will 404 and break boot. Wrap in `try { gmModule = await import("../lib/gm.mjs"); } catch { gmModule = null; }` and treat `gmModule == null` as "fall back to oscillator forever." This lets disk-side ship independently. (Recommended.)
4. **Bake assets missing at runtime.** `gm.loadManifest` will fail on first run if `assets.aesthetic.computer/gm/` is empty. The plan's fallback (`!gmReady || !gmPatch` → oscillator path) handles this, but the user will silently get sine instead of a GM voice. **Recommended:** when GM fails to load, surface a one-time HUD note (`"GM unavailable, using oscillator"`) and keep the wave label as `gm` but parenthesized: `(gm)`.
5. **Pitch bend on GM voices.** gm.mjs API as documented does not expose per-voice frequency setters. v1 plan: skip pitch bend for GM voices (early-return). **Decision needed:** acceptable for v1, or block on gm.mjs adding `noteHandle.setDetune(cents)`?
6. **`wave === "gm" && wave === "drum"` collision.** If the user wants a GM drum kit, today's `wavetypes` entry `"drum"` means percussion-lib drums, not GM kit. v1 plan keeps `"drum"` as legacy percussion-lib and leaves "GM drum kit" as a follow-up. **Decision needed:** add a separate `"gm-drum"` wavetypes entry, or extend program > 127 to mean drum kit?
7. **`tone` → MIDI note conversion.** notepat's `tone` is a string. `soundContext.freq(tone)` returns Hz. `Math.round(12 * Math.log2(hz/440) + 69)` produces the MIDI note. **Decision needed:** put the helper in notepat.mjs locally, or add to `lib/note-colors.mjs` for reuse?
8. **Velocity scaling.** gm.mjs `play(note, { velocity })` semantics not documented (0–127? 0–1?). Assume 0–127 to match menuband. **Coordinate with the gm.mjs author.**
9. **Voice stealing.** If the user holds a chord on GM and the patch's polyphony cap is exceeded, who wins? Probably gm.mjs handles internally; notepat just calls `play` and trusts the player. Document.
10. **`gmDigitBuffer` and NuPhy.** NuPhy delivers digit keys as `keyboard:down:0`–`9` through the same bus, so the picker works for hardware presses too — desirable.

## Critical Files for Implementation

- /Users/jas/aesthetic-computer/system/public/aesthetic.computer/disks/notepat.mjs
- /Users/jas/aesthetic-computer/system/public/aesthetic.computer/lib/gm.mjs
- /Users/jas/aesthetic-computer/system/public/aesthetic.computer/lib/midi.mjs
- /Users/jas/aesthetic-computer/slab/menuband/Sources/MenuBand/MenuBandController.swift
- /Users/jas/aesthetic-computer/system/public/aesthetic.computer/lib/note-colors.mjs
