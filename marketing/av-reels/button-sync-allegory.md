# Button · UTC-Sync · Allegory — the 4 canonical patterns for reel pieces

Every reel piece (self-running A/V, vertical, luscious) must satisfy FOUR principles.
This doc is the single source of truth — copy these patterns exactly.

Read alongside `piece-kit.md` (lifecycle/API/gotchas) and `graphics-study.md` (graphics tricks).

---

## 1. NO `resolution()` PINNING — render at native res

Do **not** call `resolution(...)`. The piece must fill whatever screen it's on at native
resolution. If you need per-pixel-loop performance (metaballs, displacement fields), render
that field into a SMALL offscreen buffer with `page()` and `paste` it back full-screen —
never pin the global resolution.

```js
// boot: make a small buffer for the heavy pixel field
let field;
function boot({ painting, screen }) {
  const dw = 220, dh = Math.round(220 * screen.height / screen.width); // small, aspect-correct
  field = painting(dw, dh, () => {});           // offscreen buffer
}
function paint({ page, screen, paste, ink, wipe }) {
  page(field);                                   // draw heavy per-pixel work into the small buffer
  // ... pixel loop over field.width × field.height ...
  page(screen);                                  // back to the real screen
  paste(field, 0, 0, { width: screen.width, height: screen.height }); // scale up to native
  // ... draw crisp native-res overlays (notes, bursts, core) on top ...
}
```
(If a piece is cheap enough per-pixel, just draw at native res directly — no buffer needed.)

**⚠️ paste-of-buffer caveat:** in the headless capture path, `paste()` of a custom
`painting()`/`page()` buffer sometimes renders BLACK (observed on `molten`; worked on
`lavabath`). If your pasted buffer comes up black in the harness, don't fight it — the reel
captures at **density 3** (logical screen ~360×640 ≈ 230K px, the per-pixel sweet spot), so
just write the field DIRECTLY to `screen.pixels` at native res (no `resolution()`, no buffer).
Draw crisp vector overlays (notes, bursts, core) on top. Verify fps in the harness either way.

---

## 2. THE WHOLE PIECE IS A "BUTTON" — tap / XY drag PUMPS it

It self-runs with zero input, but ANY tap — and dragging across X/Y — POKES/PUMPS the system
like a sonic boost: an instantaneous burst of **sound** + a synced **visual** burst at the
touch point, colored/scaled by X and Y. Repeated taps and different spots accumulate a
decaying "energy" that pumps the whole system harder.

```js
let pump = 0;                 // decaying global energy, 0..~3
let bursts = [];              // visual bursts spawned by taps {x,y,r,life,hue}

function act({ event: e, sound, screen, clock }) {
  if (e.is("touch") || e.is("draw")) {
    const x = e.x / screen.width;               // 0..1
    const y = e.y / screen.height;              // 0..1
    pump = Math.min(3, pump + (e.is("draw") ? 0.12 : 0.9)); // taps punch, drags feed
    bursts.push({ x: e.x, y: e.y, r: 0, life: 1, hue: x * 360 });

    // SONIC BOOST — X→pitch/pan, Y→octave/brightness. Fits the piece's palette.
    const note = ["c","d","e","g","a"][Math.floor(x * 5)] + (2 + Math.floor((1 - y) * 4));
    sound.synth({ tone: note, type: "sine", beats: 0.5, attack: 0.005, decay: 0.6,
      volume: 0.55, pan: x * 2 - 1 });
    sound.synth({ tone: note, type: "triangle", beats: 0.25, attack: 0.002, decay: 0.3,
      volume: 0.25 * (1 - y) });                 // brighter sparkle higher up
  }
}
```
Decay `pump` and advance `bursts` in `sim`; use `pump` to swell everything (radii, brightness,
displacement) and draw `bursts` as expanding rings/flashes in `paint`. Louder/bigger where tapped.

---

## 3. UTC-SYNCED RHYTHM — two instances auto-align (net.time / clock.mjs)

Any piece with a beat MUST derive its beat grid from the shared **UTC-synced clock**, not a
free-running local counter — so two instances opened anywhere lock to the same grid and to
each other. `clock.time()` returns a UTC-synced `Date`; `clock.resync()` fetches the offset
from `/api/clock`.

**Rule:** pick a BPM, compute the ABSOLUTE global beat from epoch ms, schedule note-onsets when
the integer beat advances, index the pattern by `floor(globalBeat) % len`. Because the value is
absolute-from-epoch, every instance computes the identical step at the identical wall-clock ms.

```js
const BPM = 120;
const BEAT_MS = 60000 / BPM;      // ms per beat
const PATTERN = ["a2","c3","e3","g3"]; // your score; length can be any power-friendly count
let lastBeat = -1;
let beatProgress = 0;             // 0..1 within current beat (for smooth visuals)
let simTime = 0;

function boot({ clock }) {
  clock?.resync?.();              // fetch UTC offset (no-op/local fallback offline — fine)
}

// Drive audio from UTC in sim (NOT from beat()) so onsets align across instances.
function sim({ sound: { speaker, synth }, clock }) {
  speaker?.poll();               // MANDATORY for audio-reactive reads
  // ⚠️ clock.time() returns an *Invalid Date* (getTime() → NaN, NOT null) before the
  // UTC offset resolves — and offline (serve-local has no /api/clock) it stays invalid.
  // `?? new Date()` does NOT catch it (it's a Date object). Guard with isFinite or the
  // beat index goes NaN → onBeat fires EVERY sim tick (audio spam + NaN-position visuals).
  let ms = clock?.time?.()?.getTime?.();
  if (!Number.isFinite(ms)) ms = Date.now();   // local fallback; prod uses synced UTC
  const globalBeat = ms / BEAT_MS;
  const idx = Math.floor(globalBeat);
  beatProgress = globalBeat - idx;              // 0..1
  simTime = ms;
  if (idx !== lastBeat) {                        // a new beat crossed → fire it
    lastBeat = idx;
    const step = ((idx % PATTERN.length) + PATTERN.length) % PATTERN.length;
    onBeat(step, idx, synth);                    // schedule this step's notes
  }
}

function onBeat(step, idx, synth) {
  synth({ tone: PATTERN[step], type: "sine", beats: 0.9, attack: 0.005, decay: 0.6, volume: 0.5 });
  // ...bass on even beats, hats, etc. — all keyed off `step`/`idx`.
}
```
Notes:
- **Do NOT also schedule in `beat()`** — that reintroduces local-phase drift. Do it all from UTC in `sim`.
- `sound.bpm()` in boot is optional (only affects the internal metronome/`beat.detected`); the
  UTC scheduler above is authoritative for onsets.
- For a per-step visual note-flash, spawn a visual marker inside `onBeat` (see §4) so every audible
  onset has a visible birth exactly when it sounds.
- Offline (`serve-local.mjs` has no `/api/clock`), `resync` fails silently → falls back to local
  time; rhythm still runs, just not cross-machine. Production has `/api/clock`.

---

## 4. STRONG GRAPHIC↔SONIC ALLEGORY — the visual IS the score (graphic notation)

The visuals must be a LEGIBLE symbolic representation of the audio: what you SEE relates to what
you HEAR. Map sonic dimensions to visual dimensions CONSISTENTLY and make **every audible event
have a synchronized visible counterpart** born at the moment it sounds.

Canonical mapping (adapt per piece, keep it consistent within the piece):
- **PITCH → vertical position** (or hue, or angle). High note = high on screen / bright / small.
- **LOUDNESS / amplitude → size + brightness.** Bigger, brighter = louder.
- **BASS / kick → a big low pulse or shockwave** from the bottom/center.
- **HIGH note → a small bright high mark / spark.**
- **THE BEAT → a clear rhythmic pulse** (the whole field breathes on the grid).
- **TIMBRE → shape/texture** (sine=smooth circle, saw=jagged, noise=grainy spray).

Practically: in `onBeat`, spawn a visual "note" object encoding the note's pitch→y/hue and
velocity→size; animate it in `sim`; draw it in `paint`. Poll live bands (`sound.speaker.frequencies`,
`.amplitudes`, `.beat.detected`) to make the ambient field react too. A viewer should be able to
"read" the music off the screen — a bass hit is unmistakably the big low bloom, an arp note is
unmistakably the little rising spark. Tap bursts (§2) are the same language: a poke = a visible
shock + an audible boom.
