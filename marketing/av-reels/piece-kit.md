# piece-kit.md — Authoring luscious A/V-synced AC pieces for Reels

A self-contained cheatsheet for building striking, audio-visually-**synced**
aesthetic.computer pieces (`.mjs` disks) that capture cleanly as vertical
Instagram Reels. Companion to `marketing/av-reels/README.md` (the capture
pipeline) and `../../system/public/aesthetic.computer/disks/CLAUDE.md`
(the general authoring guide).

**Two target forms — the whole doc is aimed at these:**

- **(A) STANDALONE self-running A/V piece** — autoplays gorgeous audio + tightly
  synced visuals on boot, needs **no** input, and **loops seamlessly**. The
  ideal clean reel: zero dead time, first frame ≈ last frame.
- **(B) ONE-BUTTON / one-parameter instrument** — a single tap (or one knob /
  XY position) drives **both** sound and visuals together.

All source line references are into
`/Users/jas/aesthetic-computer/system/public/aesthetic.computer/lib/`
unless noted. **Signatures below are lifted from real source — trust them.**

---

## 0. The one constraint that shapes everything

The reel capturer (`marketing/av-reels/bin/capture-av.mjs`) records **AC's own
in-page synthesized audio** (it tees `AudioNode.prototype.connect` before boot).
It does **not** overlay music. Two consequences:

1. **Audio must be AC-synthesized** — `sound.synth`, `sound.bubble`,
   `sound.fart`, `sound.play`. External `<audio>`/media won't be captured well.
2. **AC instruments are SILENT without input.** So:
   - **Form (A)** must autostart its own sound — from `boot`, from `beat`, or
     from **URL params** (e.g. `clock:0.5~{square}cegcdfdefgec` plays an
     arpeggio on boot with zero input). This is what makes it a clean reel.
   - **Form (B)** is played by a scripted "performance" (a timed list of
     taps/holds) at capture time — see §7.

Reels are furniture-free: **no** progress bars / timecodes / labels. Keep the
frame full-bleed; IG brings its own chrome.

---

## 1. Lifecycle — what fires, when, and where audio goes

A piece exports lifecycle functions. **Every one receives the same single merged
`$` API object** — the params shown in examples are just what that function
chose to destructure. You may destructure anything from `$`.

```javascript
function boot($)  { /* once, at load — setup + autostart */ }
function sim($)   { /* FIXED 120 Hz timestep, render-independent */ }
function paint($) { /* once per display refresh (≤165fps, display-capped) */ }
function beat($)  { /* once per beat at current BPM — the metronome */ }
function act($)   { /* on every user/device event; $.event.is("...") */ }
function leave($) { /* once, before unload */ }
export { boot, sim, paint, beat, act, leave };
```

| fn | cadence | use it for |
|---|---|---|
| `boot` | once | resolution, `ui.Button`, **autostart** sound/score (A), state init |
| `sim` | **120 Hz fixed** (`loop.mjs:7` `updateFps = 120`) | **`sound.speaker?.poll()`** (mandatory before reading audio), eased/inter-beat progress, particle physics |
| `paint` | display refresh | read audio, `wipe`/trail, draw visuals |
| `beat` | per BPM tick (default **BPM 60**) | **schedule `synth()` notes** — the metronomic heartbeat of generative loops |
| `act` | per event | map pointer→params (shared by audio+visual), create/update/kill sustained voices |

**Where to schedule audio:**
- **Rhythmic / looping notes → `beat`.** It fires exactly on the BPM grid, so
  notes lock to the beat automatically. Set tempo with `sound.bpm(n)` inside it.
  Buffers flush after each beat (`disk.mjs:11919`), so re-issue every tick.
- **Sustained / position-driven voices → `act`** (create) + `sim`/`paint`
  (update the live handle).
- **Autostart on boot → `boot`** (fire a looping `synth` with
  `duration: "🔁"`, or generate a score for `beat` to play).

**Deterministic time a piece can read** (for loops that self-run identically):
- `$.sound.time` — the audio timebase (a seconds float). Pair with `sound.bpm`
  for smooth inter-beat motion: `beatProgress = (time - beatStart) / (60 / bpm)`.
- `$.simCount` / `$.paintCount` — tick counters. ⚠️ **They arrive in the piece as plain
  Numbers, NOT BigInt.** Never write `paintCount % 240n` — mixing a Number with a BigInt
  literal throws `Cannot mix BigInt and other types` and blanks the frame. Use plain
  Number math: `(paintCount % 240) / 240`.
- `$.clock.time()` — network-synced wall clock (`Date`), server-offset-corrected
  (`disk.mjs:2962`). Use for UTC-synced pieces (see `clock.mjs`).
- `$.paintCount` mod N, or `$.num.wave(phase)`, drive purely time-based loops.

**Seamless loop tip:** drive all motion from a phase that wraps —
e.g. `const t = (paintCount % 240) / 240;` (plain Number math) then animate on `t` (0→1),
so frame 0 and frame 240 are identical. Or key everything to `beatCount`
(§5.C) and loop the score.

---

## 2. Graphics API (in `paint`)

Immediate-mode. Coordinates in pixels. `screen.width`, `screen.height`,
`screen.pixels` (RGBA `Uint8ClampedArray`) are re-read live each frame.

### Color formats (accepted by `ink`, `wipe`, any color arg)

`graph.findColor` (`graph.mjs:993`) accepts:

```javascript
ink(255, 0, 0)          // r,g,b   (0–255)
ink(255, 0, 0, 128)     // r,g,b,a
ink(200)                // grayscale
ink(200, 128)           // gray + alpha
ink("red")              // any CSS named color (unknown name → RANDOM color)
ink("#ff0000")          // hex string ("0xff0000" also ok)
ink(0xff0000)           // number > 255 → raw hex
ink([255, 0, 0, 128])   // array form
ink("rainbow")          // cycles ROYGBIV, advances each call
ink("zebra")            // alternates black/white each call
ink("rainbow", 128)     // dynamic color + alpha
ink("erase")            // transparent-erase blend
ink()                   // no args → RANDOM color
```

**Fade (gradient) strings** — `graph.mjs:parseFadeColor 338`:

```
"fade:<colorA>-<colorB>[-<colorC>...]:<direction>[:dirty]"
```
```javascript
ink("fade:red-blue")                  // horizontal (default direction)
ink("fade:yellow-white:vertical")     // vertical
ink("fade:rainbow-black-rainbow")     // multi-stop, animated stops
wipe("fade:navy-black:vertical")      // gradient background
```
Direction defaults `horizontal` (also `vertical`, angles). `neat` (clean bands)
is the DEFAULT; append `:dirty` for dithered. Each stop can be a CSS name,
`rainbow`/`zebra` (animated), or a `cN` palette index. Alpha = 2nd arg.

### Drawing calls

| call | signature / forms | source |
|---|---|---|
| `wipe(...color)` | fill whole buffer (white if no args) — **call first in paint** | `disk.mjs:6351` |
| `ink(...color)` | set current draw color; chainable | `disk.mjs:6341` |
| `line(x0,y0,x1,y1[,thick])` | also `line([x,y],[x,y])`, `line({x0,y0,x1,y1})` | `graph.mjs:3106` |
| `lineAngle(x,y,dist,deg)` | | `graph.mjs:3308` |
| `pline(coords,thick,shader)` | thick polyline, round caps | `graph.mjs:3497` |
| `box(x,y,size)` / `box(x,y,w,h[,mode])` | mode: `"fill"`(default), `"inline"`/`"in"`, `"outline"`/`"out"` (+`:thick`), suffix `*center` centers | `graph.mjs:3716` |
| `circle(x,y,r,filled=false,thick,precision)` | | `graph.mjs:3378` |
| `oval(...)` / `pie(...)` | ellipse / pie slice | `disk.mjs:6530,6532` |
| `plot(x,y)` / `plot({x,y})` | single pixel at ink | `graph.mjs:1639` |
| `point(...)` | point(s), flexible args | `graph.mjs:1725` |
| `poly(coords)` | array of `[x,y]`/`{x,y}`, open polyline — **the oscilloscope call** | `graph.mjs:3472` |
| `shape(...)` | filled polygon | `graph.mjs:4033` |
| `tri(...)` | triangle (gradient/textured variants) | `graph.mjs:4467` |
| `paste(from,x=0,y=0,scale=1)` | painting/buffer or a cached-piece string code | `graph.mjs:2163` |
| `stamp(from,x,y)` | paste centered on x,y | `disk.mjs:6447` |
| `write(text,pos,bg?,bounds?,wrap=true,typeface?)` | `pos`=`{x,y}` or `{center:"x"\|"xy", y?, size?}` | `disk.mjs:5501` |
| `blur(radius=1)` | box blur current buffer | `graph.mjs:7164` |
| `sharpen(strength=1)` | | `graph.mjs:7405` |
| `spin(steps,anchorX?,anchorY?)` | rotate buffer | `graph.mjs:6165` |
| `zoom(level=1,ax=0.5,ay=0.5)` | scale about anchor | `graph.mjs:6597` |
| `scroll(dx=0,dy=0)` | wraps; sub-pixel accumulates | `graph.mjs:5490` |
| `suck(strength=1,cx?,cy?)` | radial pinch warp | `graph.mjs:7012` |
| `contrast(level=1.0)` | 1.0 = no-op | `graph.mjs:1939` |
| `sort()` | pixel-sort by luminance (within mask) | `graph.mjs:7514` |
| `invert()` | 255−RGB | `graph.mjs:2094` |
| `blend(mode)` | set blend mode | `graph.mjs:2752` |
| `pan(x,y)` / `unpan()` | translate subsequent draws (`savepan`/`loadpan` push/pop) | `graph.mjs:1795` |
| `mask(box)` / `unmask()` | clip to `{x,y,width,height}` | `graph.mjs:1826` |
| `page(buffer)` | redirect drawing to another buffer; pass `screen` to restore | `disk.mjs:6311` |
| `flood(x,y,color?)` | flood fill | `graph.mjs:778` |

**Chaining:** most return the ink object, so `ink("red").box(...).line(...)` works.

### The luscious-motion toolkit (steal these)

- **Motion trails** instead of a hard wipe (from `starfield.mjs`):
  ```javascript
  ink(0, 32).box(0, 0, screen.width, screen.height); // translucent veil each frame
  // ...then draw bright things on top → glowing trails
  ```
- **Feedback zoom/spin** (KidLisp-style bloom): don't wipe; `zoom(1.01)` +
  `spin(0.3)` + `blur(1)` each frame recycles the previous frame into a vortex.
- **Full-bleed gradient wash:** `wipe("fade:navy-black:vertical")`.

---

## 3. Num utilities (`$.num`) + helpers

From `num.mjs`, exposed as `$.num` (`disk.mjs:4499`):

```javascript
num.map(v, inMin, inMax, outMin, outMax)   // linear remap
num.clamp(v, low, high)
num.lerp(a, b, t)                          // t is clamped 0..1
num.wave(phase)                            // = sin(phase), -1..1  (the sine helper)
num.dist(x1,y1,x2,y2)  // or dist({x,y},{x,y})
num.radians(deg)  /  num.degrees(rad)
num.randInt(n)                             // 0..n INCLUSIVE
num.randIntRange(low, high)                // low..high INCLUSIVE
num.randInd(arr)                           // random valid index
num.perlin(x, y)
num.within(range, a, b)                    // |a-b| < range
new num.Track(values, cb)                  // .step(0..1): array→index, {from,to}→lerp
```
Color math also on `num`: `saturate`, `desaturate`, `rgbToHsl`, `hslToRgb`,
`shiftRGB`, `rainbow`, `zebra`.

**`wiggle` is top-level `$` (NOT `num`)** — `disk.mjs:4164`:
```javascript
wiggle(n, level = 0.2, speed = 6)  // n oscillated by a shared global angle
```
There is **no `range()`** util. Geometry classes are under `$.geo`
(`Box`, `Grid`, `Circle`, `Race`, `Quantizer`, `linePointsFromAngle`).

---

## 4. Sound / synthesis + AV sync

`$.sound` (assembled `disk.mjs:12821+`). Everything routes through the captured
audio bus, so it lands in reels.

### `sound.synth({...})` — `disk.mjs:12821`

```javascript
sound.synth({
  tone = 440,       // Hz OR musical note string ("C4", "a3") — run through sound.freq
  type = "square",  // waveform, see below
  duration = 0.1,   // seconds; "🔁" → Infinity (sustained loop)
  beats,            // duration in BEATS instead (locks to BPM) — preferred for rhythm
  attack = 0.01,    // rise time
  decay = 0.9,      // fade multiplier
  volume,           // defaults to 1
  pan = 0,          // -1 (left) .. 1 (right)
  generator = null, // custom waveform fn (only for type "custom")
});
```
**Waveform `type` strings:** `"sine"`, `"triangle"`, `"square"`, `"sawtooth"`,
`"custom"`, `"noise-white"`. ⚠️ **Noise goes in `type`, e.g. `synth({ type: "noise-white",
tone: 800, ... })` — passing `"noise-white"` as `tone` throws `Note not found` and (inside
`beat()`) kills ALL audio for that tick.**

**Returns a live handle:**
```javascript
const v = sound.synth({ tone: "c4", duration: "🔁", volume: 0.5 });
v.update({ tone: "e4", volume: 0.3, pan: 0.5 }); // mutate a playing voice live
v.progress(sound.time);                          // 0..1 through its duration
v.kill(0.2);                                      // stop with 0.2s fade
```

### Other generators

```javascript
sound.bubble({ radius, rise, volume = 1, pan = 0 });
//   → handle + .enableSustain() / .disableSustain() (hold-to-sustain, see §5.B)
sound.fart({ pressure = 1, pitch = 60, rasp = 0.5, volume = 1, pan = 0 });
sound.play(sfxId, { volume, pan, loop });        // named samples → handle w/ .kill/.progress
sound.beep(tone = 1200);                          // convenience one-shot
sound.bpm(120);                                   // set tempo (call in beat/boot)
```
Global FX buses (great for a reel "drop"):
```javascript
sound.room.set({ enabled: true, mix: 0.4, feedback: 0.6 }); // reverb
sound.glitch.set({ enabled: true, mix: 0.5, crush: 6, rate: 4000 }); // bitcrush
```

### Locking notes to the beat

Two proven approaches:

1. **`beat()` + `beats:` duration** (rhythmic, self-syncing):
   ```javascript
   function beat({ sound: { bpm, synth } }) {
     bpm(120);
     synth({ tone: "c4", beats: 1, attack: 0.01, decay: 0.5, volume: 0.6 });
   }
   ```
2. **`boot()` autostart from a melody string** — `clock.mjs` parses
   `piece:0.5~{square}cegcdfdefgec` via `lib/melody-parser.mjs`. Just booting
   the URL plays it — zero input. This is the reels README's go-to autoplay.

### Reading LIVE audio for reactive visuals

**Step 1 — poll every `sim` frame** (nothing populates without this):
```javascript
function sim({ sound: { speaker } }) { speaker?.poll(); }
```
**Step 2 — read in `paint`** (populated async on next round-trip). `sound.speaker`
(`disk.mjs:7517`) exposes, per stereo channel:
```javascript
sound.speaker.amplitudes.left   // a single number, 0..~1  (loudness)
sound.speaker.waveforms.left    // array of raw samples    (oscilloscope)
sound.speaker.frequencies.left  // array of 8 BAND objects (spectrum)
sound.speaker.beat              // { detected, strength, timestamp }
```
**The 8 frequency bands** (`speaker.mjs:1029`) — each `{ name, frequency:{min,max},
amplitude (0..0.9, power-scaled), binRange }`:

| name | Hz | name | Hz |
|---|---|---|---|
| `subBass` | 20–100 | `presence` | 2500–5000 |
| `lowMid` | 100–400 | `treble` | 5000–10000 |
| `mid` | 400–1000 | `air` | 10000–16000 |
| `highMid` | 1000–2500 | `ultra` | 16000–20000 |

```javascript
const bands = sound.speaker.frequencies.left || [];
const bass = bands.find((b) => b.name === "subBass")?.amplitude || 0;
if (sound.speaker.beat.detected) ink("fade:yellow-white:vertical"); // flash on beat
```

**Built-in oscilloscope helper** — `sound.paint.waveform(...)` (used by
`tone.mjs`), so you don't hand-roll it:
```javascript
const wf = sound.speaker.waveforms.left;
const scope = Math.min(wf.length, screen.width * 2);
const samples = help.resampleArray(wf, scope);      // fit waveform to screen width
sound.paint.waveform(api, amplitude || 0.1, samples, 0, 0,
                     screen.width, screen.height, [255, 255, 255, 200]);
```

**Microphone** (`sound.microphone`, `disk.mjs:7471`) — connect after a gesture,
`poll()` each frame, read `.amplitude`, `.waveform`, `.pitch`.

---

## 5. Reusable patterns from the best pieces

### Example roster — technique to steal

**Form A (self-running / audio-reactive):**
- `disks/melody.mjs` — `beat()` fires notes per BPM; `sim()` computes inter-beat
  progress; `paint()` scrolls the score continuously. Auto-generates a random
  score if none → **runs forever with zero input.** The Form-A template.
- `disks/clock.mjs` — autoplays a melody string from URL params + UTC sync. The
  reels README's autoplay workhorse.
- `disks/whistle.mjs` — canonical `paintSound(amp, waveform, x,y,w,h)` helper
  (amplitude box + waveform poly).
- `disks/tone.mjs` — uses built-in `sound.paint.waveform` + `duration:"🔁"`
  sustain; distance-from-center → volume.
- `disks/visualizer.mjs` — full spectrum bars + `speaker.beat.detected` flashes.
- `disks/starfield.mjs` — tiny full-bleed motion loop with **trail-fade** (veil
  box instead of wipe).
- `disks/marimbagraph.mjs` — whistlegraph score piece: tap-to-begin then
  self-plays a `.np` score with generative visuals.

**Form B (one-tap / one-knob):**
- `disks/bubble.mjs` — X→pan, Y→pitch, radial→timbre; **same mapping drives
  audio AND visual**; hold-to-sustain via `enableSustain`/`.update`/`disableSustain`.
- `disks/fartflower.mjs` — the tightest one-button: `ui.Button` + eased
  `bloomProgress` in `sim` + sound on press with a delayed pitch sweep.
- `disks/tone.mjs` — whole screen is the button; `"🔁"` loop, distance→volume.
- `disks/kaos-pad-template.mjs` — multi-touch XY pad, one sine voice per pointer.
- `disks/butterflies.mjs` — hold to spawn 1-bit sprite-voices; each tap = sprite + note.

### A) Audio-reactive visualizer (`whistle.mjs`)

```javascript
// poll in sim (mandatory)
function sim({ sound: { speaker } }) { speaker?.poll(); }

// reusable oscilloscope: amplitude box + waveform polyline, positionable
function paintSound({ ink }, amplitude, waveform, x, y, width, height, color) {
  const xStep = width / waveform.length + 2;
  const yMid = y + height / 2, yMax = height / 2;
  ink(color || [255, 128]).box(x + width / 2, yMid, width, amplitude * yMax * 2, "*center");
  ink(255, 0, 0, 128).poly(waveform.map((v, i) => [x + i * xStep, yMid + v * yMax]));
}
```

### B) One-tap instrument — map pointer once, reuse for sound + visual (`bubble.mjs`)

```javascript
function paramsFor(e, { num, screen }) {
  const cx = screen.width / 2, cy = screen.height / 2;
  const maxD = Math.max(cx, cy);
  const d = num.clamp(num.dist(e.x, e.y, cx, cy) / maxD, 0.01, 1);
  return {
    pan:  num.map(e.x, 0, screen.width, -1, 1),      // X → stereo
    rise: num.map(e.y, 0, screen.height, 4.0, 0.2),  // Y → pitch (inverted)
    soundRadius: num.map(d, 0, 1, 3, 25),            // radial → timbre
    d,
  };
}
// touch → create sustaining voice; draw → update live handle; lift → let fade
```

### C) Generative self-running loop — beat + inter-beat progress (`melody.mjs`)

```javascript
let beatCount = 0, beatStart = 0, beatProgress = 0;

function beat({ sound: { bpm, synth, time } }) {
  bpm(120);
  synth({ tone: notes[beatCount], beats: 1, attack: 0.01, decay: 0.6,
          volume: 0.6, pan: -0.5 + beatCount / (notes.length - 1) });
  beatCount = (beatCount + 1) % notes.length; // loop the score forever
  beatStart = time; beatProgress = 0;
}
function sim({ sound: { time, bpm } }) {
  beatProgress = (time - beatStart) / (60 / bpm()) || 0; // 0..1 through current beat
}
function paint($) {
  // key motion to (beatCount + beatProgress) → smooth, not steppy, and LOOPS
}
```

---

## 6. Reel-optimized rules (checklist)

- **Portrait, full-bleed.** Capture is 1080×1920 (`capture-av.mjs`). Fill the
  whole screen; nothing important near the extreme top/bottom (IG UI overlays).
- **Continuous motion, zero dead time.** Something moves every frame from frame 0.
- **Seamless loop.** First frame ≈ last frame — key all motion to a wrapping
  phase (`paintCount % N`) or a looping `beatCount`, so the cut is invisible.
- **Bold, saturated color.** Gradient washes (`fade:`), `rainbow`, high-contrast
  ink. Trails/bloom read better than flat fills.
- **Audio carries.** Real AC synth, rhythmic, present. Aim for a mean volume near
  the proven reels (notepat −6 dB, clock −5 dB; bubble/fartflower ~−18 dB).
- **No furniture.** No on-screen labels/HUD/progress — capture strips them.

### Clean-capture URL params (from `boot.mjs`, allowlist at `boot.mjs:537-538`)

Append to the piece URL for furniture-free capture:

| param | effect | `boot.mjs` |
|---|---|---|
| `nolabel` | hide the corner piece label / HUD; no localStorage persist | `:1116` |
| `nogap` | edge-to-edge, no letterbox gap (adds `nogap` body class) | `:1113` |
| `tv` | disable touch/keyboard input (display mode) | `:1195` |
| `density=N` | pixel density / resolution scale (capture uses ~3) | `:1132` |
| `duration=N` | timed run | allowlist `:538` |
| `zoom=N` | zoom level | allowlist `:538` |

`capture-av.mjs` sets these for you (density default 3). Example manual URL:
`https://aesthetic.computer/mypiece?nolabel&nogap&density=3`.

### Capturing (from `marketing/av-reels/README.md`)

```bash
# self-running (A) — no input needed:
node marketing/av-reels/bin/capture-av.mjs mypiece --duration 10
# autoplay-from-params:
node marketing/av-reels/bin/capture-av.mjs 'clock:0.5~{square}cegcdfdefgec' --duration 10
# one-tap (B) — drive it with a performance:
node marketing/av-reels/bin/capture-av.mjs bubble --perform bubble-taps --duration 10
# then stamp into a 9:16 reel (keeps AC audio):
node marketing/av-reels/bin/stamp-reel.mjs marketing/av-reels/out/mypiece/base-mypiece.mp4 \
  --title mypiece --out ~/Desktop/av-mypiece-reel.mp4
```
Performance action shapes (`t` = ms from start): `{t,type:"down"|"up"|"press",key}`,
`{t,type:"tap",x,y}` (normalized 0..1), `{t,type:"mdown"/"mup",x,y}` (hold),
`{t,type:"type",text}`. Built-ins live in `marketing/av-reels/bin/performances.mjs`.

---

## 7. KidLisp — luscious self-running loops in a few lines

A piece can be `.lisp`. KidLisp (`kidlisp/README.md`) is ideal for Form (A):
tiny, self-running, immediate. Relevant built-ins:

- **Graphics:** `wipe ink line box circle tri plot flood shape`
- **Transforms (feedback bloom):** `zoom scroll spin smoothspin blur contrast
  suck sort pan`
- **Math:** `sin cos random wiggle + - * /`
- **System/time:** `width height frame clock fps`
- **Audio:** `overtone`, `melody`, `mic` (+ 3 more)
- **Colors:** all CSS names + `rainbow`, `zebra`
- **Control:** `def later if once repeat`

Self-running luscious loop (no wipe → feedback trails), audio via `overtone`:
```lisp
(overtone 220)          ; drone a tone (self-runs, loops)
(ink rainbow)
(repeat 40 i
  (circle (wiggle width) (wiggle height) (+ 4 (* i 2))))
(zoom 1.02)             ; feedback bloom — recycles previous frame
(spin 0.5)
(blur 1)
```
Autoplay a melody line: `melody` + a note string. A KidLisp code stored via
`store-kidlisp.mjs` gets a `$code` and is URL-addressable as
`aesthetic.computer/$code` — captures like any piece. Note `clock`'s
`0.5~{square}cegcdfdefgec` melody-string form is the simplest autoplay of all.

---

## 8. Publishing — create → publish → view

### Create

```bash
npm run new mypiece "My luscious A/V loop"
```
Scaffolds `system/public/aesthetic.computer/disks/mypiece.mjs` by copying
`blank.mjs` and substituting `$NAME` / `$TIMESTAMP` / description
(`utilities/generate-new-piece.mjs:46`).

> ⚠️ **Heads-up:** `blank.mjs` is currently the AC-laptop product page, not a
> clean stub. In practice, author from a template that fits your form — copy one
> of the roster pieces (e.g. `starfield.mjs`, `bubble.mjs`, or the skeletons in
> §9) into `disks/mypiece.mjs` rather than relying on the scaffold's body.

### View in dev

```bash
npm run site   # http://localhost:8888/mypiece
```
Hot reload: saving the `.mjs` reflects immediately via the module loader. Use
`channel custom-name` for multi-device testing.

### Publish (two distinct routes)

**Route 1 — user-published piece (from the AC prompt):**
```
publish            # publishes the CURRENT sketch as-is
publish mypiece    # publish with a custom slug
```
`publishPiece` (`prompt.mjs:9516`) uploads `piece-<slug>.mjs`, then jumps to
`@handle/<slug>` (or just `<slug>` if no handle). This is the fastest path for a
one-off — lives at `aesthetic.computer/@handle/mypiece`.

**Route 2 — built-in repo piece (canonical `aesthetic.computer/mypiece`):**
1. Commit `disks/mypiece.mjs` into the repo.
2. `system/public/**` is a **live-served path** → deploy to production:
   ```bash
   fish lith/deploy.fish
   ```
   (Pushing alone does **not** put it in production — per root `CLAUDE.md`
   "compush" rule. `compush` on live-served paths must be followed by the lith
   deploy.)
3. Live at `https://aesthetic.computer/mypiece`. Share/QR: `share mypiece`.

---

## 9. Copy-pasteable skeletons

### (A) Minimal self-running AV-synced loop

Boots, plays a BPM-synced synth line, renders audio-reactive full-bleed visuals,
and loops seamlessly. Zero input required.

```javascript
// starloop, 26.07.11
// Self-running A/V loop: a 4-note bass pattern drives spectrum-reactive rings.

const NOTES = ["c2", "e2", "g2", "a2"]; // one bar, loops forever
let beatCount = 0, beatStart = 0, beatProgress = 0;

function boot({ sound }) {
  sound.bpm(110);
  sound.room?.set?.({ enabled: true, mix: 0.35, feedback: 0.55 }); // lush tail
}

// Schedule the rhythmic line on the metronome — locks audio to the beat grid.
function beat({ sound: { bpm, synth, time } }) {
  bpm(110);
  synth({
    tone: NOTES[beatCount % NOTES.length],
    type: "sawtooth",
    beats: 1, attack: 0.01, decay: 0.6, volume: 0.55,
    pan: -0.4 + (beatCount % NOTES.length) / (NOTES.length - 1) * 0.8,
  });
  synth({ type: "noise-white", tone: 800, beats: 0.2, decay: 0.3, volume: 0.25 }); // hat (noise → type, not tone)
  beatCount = (beatCount + 1) % NOTES.length;
  beatStart = time; beatProgress = 0;
}

// Poll live audio (mandatory) + smooth inter-beat progress.
function sim({ sound: { speaker, time, bpm } }) {
  speaker?.poll();
  beatProgress = (time - beatStart) / (60 / bpm()) || 0; // 0..1 through the beat
}

function paint({ ink, box, circle, poly, screen, sound, num, help, paintCount }) {
  const { width: w, height: h } = screen;
  ink(0, 40).box(0, 0, w, h); // trail veil instead of hard wipe → glowing motion

  // spectrum-reactive rings from the left channel bands
  const bands = sound.speaker?.frequencies?.left || [];
  const bass = bands.find((b) => b.name === "subBass")?.amplitude || 0;
  const air  = bands.find((b) => b.name === "air")?.amplitude || 0;
  const beatPulse = 1 - beatProgress; // bright on the downbeat, fades to next

  const cx = w / 2, cy = h / 2;
  for (let i = 0; i < 6; i++) {
    const r = (i + 1) * (Math.min(w, h) / 14) * (1 + bass * 1.5) + beatPulse * 20;
    ink("rainbow", 120).circle(cx, cy, r, false, 2 + air * 8);
  }

  // audio oscilloscope across the middle (built-in helper)
  const wf = sound.speaker?.waveforms?.left;
  if (wf?.length) {
    const samples = help.resampleArray(wf, Math.min(wf.length, w * 2));
    sound.paint?.waveform?.(
      { ink }, sound.speaker.amplitudes.left || 0.1, samples,
      0, cy - h / 4, w, h / 2, [255, 255, 255, 160],
    );
  }

  // seamless phase-driven orbiter (wraps every 240 frames)
  const t = (paintCount % 240) / 240; // paintCount is a Number — no BigInt `240n`
  const a = t * Math.PI * 2;
  ink("fade:yellow-white").circle(cx + Math.cos(a) * cx * 0.6,
                                  cy + Math.sin(a) * cy * 0.6, 8 + bass * 40, true);
}

export { boot, beat, sim, paint };
```

### (B) One-button instrument — tap → sound + visual

A single tap (or hold) fires a note and blooms a synced visual; the same pointer
position drives both audio params and the graphic.

```javascript
// tapbloom, 26.07.11
// One-tap instrument: tap position sets pan/pitch/color; hold to sustain.

let voice = null;          // live synth handle while held
let bloom = 0;             // eased visual progress 0..1
let last = { x: 0, y: 0, hue: 0 };

function boot() {}

function act({ event: e, num, screen, sound: { synth } }) {
  if (e.is("touch")) {
    const pan  = num.map(e.x, 0, screen.width, -1, 1);
    const tone = num.map(e.y, 0, screen.height, 660, 110); // top = high
    last = { x: e.x, y: e.y, hue: num.map(e.x, 0, screen.width, 0, 359) };
    voice = synth({ tone, type: "triangle", duration: "🔁",
                    attack: 0.02, decay: 0.9, volume: 0.6, pan }); // sustain
  }
  if (e.is("draw") && voice) { // drag re-maps live
    const pan  = num.map(e.x, 0, screen.width, -1, 1);
    const tone = num.map(e.y, 0, screen.height, 660, 110);
    last = { x: e.x, y: e.y, hue: num.map(e.x, 0, screen.width, 0, 359) };
    voice.update({ tone, pan });
  }
  if (e.is("lift") && voice) { voice.kill(0.25); voice = null; }
}

function sim({ num, sound: { speaker } }) {
  speaker?.poll();
  const target = voice ? 1 : 0;
  bloom = num.lerp(bloom, target, 0.12); // eased bloom in/out
}

function paint({ wipe, ink, circle, screen, num, sound }) {
  wipe("fade:black-navy:vertical");
  if (bloom < 0.001) return;

  const amp = sound.speaker?.amplitudes?.left || 0;
  // ⚠️ num.hslToRgb wants hue 0–360, sat/light 0–100, and ALREADY returns 0–255 (don't ×255):
  const [r, g, b] = num.hslToRgb(last.hue, 90, 60);
  const R = Math.min(screen.width, screen.height) * 0.5 * bloom * (1 + amp * 2);

  for (let i = 6; i > 0; i--) { // concentric halo, brightest at center
    ink(r, g, b, 40 * bloom).circle(last.x, last.y, R * (i / 6), true);
  }
  ink(255, 255, 255, 200 * bloom).circle(last.x, last.y, 6 + amp * 30, true);
}

export { boot, act, sim, paint };
```

---

## 10. Quick reference — wiring by handler

| handler | reels job |
|---|---|
| `boot` | resolution, `ui.Button`, **autostart sound/score (A)**, `sound.room`/`glitch` FX, state |
| `beat` | **`sound.bpm(n)` + `synth({beats})`** — schedule rhythmic notes; buffers flush per tick |
| `sim` | **`sound.speaker?.poll()`** (mandatory), inter-beat progress, eased visual state, physics |
| `paint` | read `sound.speaker.{amplitudes,waveforms,frequencies,beat}`; trail-box or `wipe`; full-bleed draw |
| `act` | map `e.x`/`e.y` → params shared by audio+visual; create/`update`/`kill` sustained voices |

Sustain flags: `duration: "🔁"` **or** `beats: Infinity` keep a synth alive;
`.update({...})` mutates it live; `.kill(fadeSecs)` stops it.
