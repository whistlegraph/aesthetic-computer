# Autopat plan (notepat-derived autoplay piece)

## Research notes
- `notepat.mjs` is a large, single-file piece with internal `boot()` plus UI/audio/input logic, but it **does not export** `boot`/`paint` (no `export` statements). This suggests the disk loader is not consuming ES module exports for notepat, so direct import/reuse is not currently possible.
- notepat already has track-related constants (`TRACK_HEIGHT`, `TRACK_GAP`) and TODOs for showing a melody and looping tracks.
- There are utilities for note parsing and coloring in `../lib/note-colors.mjs` and `../lib/notepat-convert.mjs` that can be reused by a track UI.
- There is no existing “autoplay” or “track engine” abstraction in `notepat.mjs` that can be imported by another disk.

## Goal
Create a new disk `autopat.mjs` that:
- reuses notepat’s sound engine and visuals,
- adds a mini track UI showing upcoming notes,
- auto-plays the track,
- disables user interaction (keyboard/mouse/touch).

## Recommended approach (refactor to share core)
### 1) Extract a shared core module
Create a new shared module, e.g.
- `system/public/aesthetic.computer/disks/common/notepat-core.mjs`

Move reusable parts into this module:
- note parsing + keyboard mapping helpers
- sound engine primitives (`startButtonNote`, `stopButtonNote`, tone/volume/pan helpers)
- UI layout calculators (button layout, mini piano, qwerty minimap)
- render helpers (note colors, mini piano painter, qwerty minimap painter)

Expose a factory API:
```js
export function createNotepatCore({ mode = "interactive", track = null }) {
  return {
    boot, paint, inputHandlers, setTrack, startAutoPlay, stopAutoPlay,
  };
}
```
- `mode: "interactive"` keeps current notepat behavior.
- `mode: "autopat"` disables input and enables autoplay.

### 2) Update notepat to use the core
- `notepat.mjs` becomes a thin wrapper that calls `createNotepatCore({ mode: "interactive" })` and wires through the piece lifecycle.
- Keep the UI/visuals identical to current notepat.

### 3) Implement autopat.mjs
- New disk at `system/public/aesthetic.computer/disks/autopat.mjs`.
- Imports `createNotepatCore`.
- Supplies a track source (see below) and enables autoplay.
- Disables direct input in the core’s handlers for this mode.

### 4) Track data model
Use a normalized event list that the core can render + schedule:
```js
{
  events: [ { note: "c", octave: 4, startMs: 0, durationMs: 120 }, ... ],
  bpm: 77,
  loop: true,
}
```
This lets the UI draw a “window” (current + upcoming notes) and the engine schedule audio.

### 5) Track UI
- A “mini track” strip above the piano/qwerty.
- Render upcoming notes as colored chips at time offsets.
- Use a playhead marker and optional progress ticks.

## Track sources (short term)
- Built-in tracks: `twinkle`, `bach-prelude`.
- The Bach prelude can reuse the MIDI-derived event list from the existing artery test.
- URL param support: `autopat <track>` or `autopat ?track=...` (later).

## Non-interactive mode
- In `autopat` mode, ignore keyboard/mouse/touch down events in the core.
- Keep audio activation with a single tap (browser requirement).
- Allow minimal controls: play/pause/loop toggle buttons only.

## Step-by-step implementation plan
1) Extract core helpers from notepat into `common/notepat-core.mjs`.
2) Update `notepat.mjs` to call the core.
3) Add `autopat.mjs` with a default track (twinkle or bach-prelude).
4) Add mini track rendering to the core paint loop.
5) Gate input for autopat mode.
6) Optional: add `track` param parsing for custom melodies.

## Risks / constraints
- `notepat.mjs` currently isn’t modular; refactor is large but necessary to “import” cleanly.
- Some UI state is tightly coupled; splitting needs care to avoid regressions.
- Autoplay must still require a user gesture to unlock audio.

## Next actions (if approved)
- Start the refactor into `common/notepat-core.mjs` and wire `notepat.mjs` to it.
- Create `autopat.mjs` and verify playback + UI with a baked-in track.
