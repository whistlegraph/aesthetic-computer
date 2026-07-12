# Graphics Study — how AC pieces exploit the pixel pipeline

A technique catalog for authoring **luscious, pixel-manipulation-heavy,
audio-visually-synced** self-running reel pieces. Companion to
`piece-kit.md` (the basic authoring kit). This file is organized by
**technique**, not by piece: each entry gives what it does, the real API +
signature (lifted from source), 1–3 example pieces, a copy-pasteable snippet,
and a performance caveat.

Everything here is verified against:
- `system/public/aesthetic.computer/lib/graph.mjs` — the pixel engine
- `system/public/aesthetic.computer/lib/disk.mjs` — how it's exposed to pieces
- `system/public/aesthetic.computer/bios.mjs` — the density/scaling layer

> **All these functions arrive as `paint({ ... })` params.** Destructure what
> you need: `paint({ ink, box, screen, paste, page, zoom, spin, blur, scroll,
> sort, contrast, suck, shear, mask, unmask, pan, unpan, steal, putback,
> painting, pixel, plot, edit, noise16 })`.

---

## 0. The one fact that governs performance: `density` = render scale

This is the most important thing to understand before writing a per-pixel piece.

The reel capture (`bin/capture-av.mjs:56`, `:92`) loads the piece at
`?nolabel&nogap&density=3`. Density is a **supersampling / render-scale** knob
handled in BIOS (`bios.mjs:1500-1513`):

```js
// bios.mjs:1500  (window is 1080×1920 for a reel)
let ratio = density || window.devicePixelRatio;   // density=3
subdivisions = ratio;
width  = floor(window.innerWidth  / subdivisions); // 1080/3 = 360
height = floor(window.innerHeight / subdivisions); // 1920/3 = 640
projectedWidth  = round(width  * density);         // back up to 1080
projectedHeight = round(height * density);         // back up to 1920
```

So at `density=3`, your piece's **logical `screen` is 360×640** (~230K px), and
BIOS nearest-neighbor-scales that framebuffer up to 1080×1920 for the video.

Consequences for a per-pixel loop:

| density | logical `screen` | pixels/frame | look | per-pixel loop cost |
|--------:|:----------------:|-------------:|------|:--------------------|
| 1 | 1080×1920 | ~2.07M | crisp, native | **brutal** — avoid raw loops |
| 2 | 540×960 | ~518K | fine | heavy but doable |
| **3** (reel default) | **360×640** | **~230K** | chunky-pretty | **the sweet spot** |
| 4 | 270×480 | ~130K | pixely | cheap, retro |

**Takeaways:**
- A full-screen `for (y) for (x) screen.pixels[...]` loop is ~230K iterations at
  density 3, ~2M at density 1 — a **9×** difference. Author and test at the same
  density you'll capture at (3), or you'll ship something that stutters.
- You can override per-piece: `boot({ resolution }) { resolution(360, 640) }`
  pins the logical grid regardless of URL density. Or shrink further for cheaper
  loops — `resolution(240, 426)` is still crisp once upscaled.
- Because upscaling is nearest-neighbor, a low logical resolution reads as
  deliberate chunky pixels, not blur. Lean into it.
- `capture-av.mjs` also passes `--density N`; `stamp-reel.mjs:88-93` reads it
  back and applies an equal `--sharpen` so density-4 capture → 4× hard pixels in
  the final MP4.

---

## (a) Raw pixel manipulation via `screen.pixels`

**What it does.** `screen.pixels` is the live framebuffer as a
`Uint8ClampedArray` in RGBA order (4 bytes/pixel, row-major). Index a pixel at
`(x, y)` with `i = (y * width + x) * 4`; write `pixels[i], [i+1], [i+2], [i+3]`.
Clamping to 0–255 is automatic (that's what `Clamped` buys you). This is the
lowest level and the most expressive — anything a shader does, you can do here.

**API.** `paint({ screen: { width, height, pixels } })`. For read-only sampling
of any buffer: `pixel(x, y, buffer?) → [r,g,b,a]` (`graph.mjs:757`). To edit via
callback: `edit((pixels, w, h) => {…})` = `graph.changePixels` (`graph.mjs:752`,
`disk.mjs:6339`).

**Example pieces.**
- `disks/metaballs.mjs` — full-resolution field render + glitch, the canonical
  raw-pixel reference. Note its header comment: *"Use screen.pixels / direct
  pixel array access for any automated drawing."*
- `disks/seashells.mjs` — writes bytebeat patterns per-column and **reads pixels
  back** to modulate its own synthesis (a closed AV feedback loop).
- `disks/butterflies.mjs` — renders a cellular-automata bitmap straight to
  `screen.pixels` with a parallel decay buffer.

**Snippet — the core write, plus a cheap plasma:**
```js
function paint({ screen }) {
  const { width: w, height: h, pixels } = screen;
  const t = performance.now() * 0.001;
  for (let y = 0; y < h; y++) {
    for (let x = 0; x < w; x++) {
      const i = (y * w + x) * 4;
      const v = Math.sin(x * 0.06 + t) + Math.sin(y * 0.05 - t * 1.3)
              + Math.sin((x + y) * 0.04 + t * 0.7);
      pixels[i]     = 128 + 90 * Math.sin(v);          // r
      pixels[i + 1] = 128 + 90 * Math.sin(v + 2.1);    // g
      pixels[i + 2] = 128 + 90 * Math.sin(v + 4.2);    // b
      pixels[i + 3] = 255;                              // a
    }
  }
}
```

**Metaball weighting core** (`disks/metaballs.mjs:73-147`) — inverse-square
falloff accumulated per ball, normalized, then dithered:
```js
let totalWeight = 0, r = 0, g = 0, b = 0;
for (const ball of balls) {
  const dx = x - ball.x, dy = y - ball.y;
  let weight = 1.0 / Math.max(dx*dx + dy*dy, 1);   // inverse-square field
  totalWeight += weight;
  r += ball.color[0] * weight; g += ball.color[1] * weight; b += ball.color[2] * weight;
}
r /= totalWeight; g /= totalWeight; b /= totalWeight;
const dither = ((x + y) % 2) * 0.05;               // ordered 2×2 dither
pixels[i] = r - dither * 20; /* … */ pixels[i+3] = 255;
```

**Performance caveat.** This is the expensive path. At density 3 (~230K px) a
plain arithmetic loop is fine at 30fps; anything with an inner loop over N
objects (metaballs: N balls × 230K px) gets heavy fast — metaballs keeps N≈13.
Rules of thumb:
- Hoist everything invariant out of the inner loop; precompute per-row values.
- Prefer `Math.sin`/`hypot` sparingly; a lookup table or squared-distance
  (skip `sqrt`, like metaballs does) is much cheaper.
- If you must go native-res detail, render at a **coarse logical resolution**
  (see §d) and let upscaling sell it.

---

## (b) Offscreen buffers: `painting()` and `page()`

**What it does.** Two complementary ways to draw somewhere other than the live
screen:

- **`painting(w, h, (api) => {…})`** creates a standalone RGBA buffer and runs a
  fill callback against it, returning `{ pixels, width, height }`. It's a real
  mini-canvas — inside the callback you get the full ink/box/line/paste API with
  its own `screen`. (`disk.mjs:7254`, engine `graph.mjs:673`.) Use it for
  **static assets** (build once in `boot`) or **accumulator layers** (rebuild
  each frame, pasting the previous into the new — persistence).
- **`page(buffer)`** *redirects all subsequent drawing* to `buffer` until you
  `page(screen)` back (`disk.mjs:6311`). Same drawing calls, different target.
  `page(screen)` restores the true framebuffer from a frame-start snapshot.

Both give you a buffer you then composite with `paste()`/`stamp()` (§f).

**Example pieces.**
- `disks/stample.mjs:499-506` — persistent 128×128 KidLisp buffer: each frame it
  makes a new `painting()`, **pastes the previous frame into it** (accumulation),
  renders on top, then keeps it as `kidlispBuffer` for next frame.
- `disks/visualizer.mjs:1140-1148,1248,1626` — an offscreen `tvBarsBuffer` built
  with `painting(screen.width, screen.height, …)`, drawn into via `page(...)`,
  then `paste()`d back over the HUD each frame.
- `disks/digitpain0.mjs` — holds two image buffers and `page()`s between them to
  cross-mutate pixels (see §c).

**Snippet — accumulation buffer (the persistence pattern):**
```js
let acc;                                  // survives across frames
function paint({ painting, paste, ink, circle, screen }) {
  // Rebuild the accumulator: previous frame faded, plus new marks.
  acc = painting(screen.width, screen.height, (p) => {
    if (acc) p.paste(acc);                // carry the last frame forward…
    p.ink(0, 0, 0, 24).box(0, 0, screen.width, screen.height); // …fade it a touch
    p.ink("rainbow", 200).circle(          // …add this frame's fresh mark
      screen.width / 2 + Math.sin(performance.now() * 0.002) * 120,
      screen.height / 2, 18, true);
  });
  paste(acc);                              // show it
}
```

**Snippet — `page()` redirect (build a texture, blit it):**
```js
let tex;
function boot({ painting }) {
  tex = painting(64, 64, (p) => {          // build once
    for (let i = 0; i < 200; i++)
      p.ink("rainbow", 255).plot((Math.random()*64)|0, (Math.random()*64)|0);
  });
}
function paint({ page, screen, ink, box, paste }) {
  // Draw directly onto an offscreen buffer, then bring it back:
  page(tex);                               // redirect
  ink(0, 0, 0, 30).box(0, 0, 64, 64);      // fade the texture in place
  page(screen);                            // ⚠️ ALWAYS restore before compositing
  paste(tex, 0, 0, screen.width / 64);     // scale-blit across the screen
}
```

**Performance caveat.** `painting()` allocates a fresh `ImageData` every call
(`graph.mjs:678`) — rebuilding a full-screen buffer every frame is an allocation
+ a full paste. Fine at density 3, wasteful at density 1. For static assets,
build in `boot` and reuse. **Always `page(screen)` before you `paste`/composite**
— forgetting leaves you drawing into the wrong buffer and corrupts
`screen.pixels`/`width`/`height` (disk.mjs documents this exact footgun at
`:6320`).

---

## (c) Feedback / trails / recycling the framebuffer

The heart of "luscious." Instead of a hard `wipe`, you leave the previous frame
in place and lightly disturb it. Three families:

### c1. Partial-alpha veil (the trail fade)
Draw a translucent full-screen rectangle each frame; old marks decay instead of
snapping off. This is the single most-used reel move.

```js
ink(6, 4, 16, 30).box(0, 0, w, h);   // deep-violet veil, ~12% opacity → glow trails
```
Alpha 20–48 is the usable band: lower = longer smears, higher = snappier. Used
in all three reference pieces — `disks/prism.mjs:116`,
`disks/lull.mjs:106` (with a **gradient** veil, see §g), `disks/emberdrift.mjs:163`.

### c2. Transform-feedback (recycle *and move* the frame)
Combine the veil with a per-frame `zoom`/`spin`/`scroll`/`suck` so the recycled
image also flows — bloom, vortex, drift. `prism` is the canonical example:
```js
// disks/prism.mjs:114-116
zoom?.(1.008 + energy * 0.01, 0.5, 0.5); // gentle outward bloom, stronger on downbeats
spin?.(0.35 + downbeat * 1.2);            // slow rotation, kicks on the beat
ink(6, 4, 16, 30).box(0, 0, w, h);        // then the trail veil over the recycled frame
```
Each frame the *entire previous frame* is scaled + rotated slightly, veiled, and
drawn on. New geometry on top blooms outward into a kaleidoscopic vortex — with
zero explicit particle bookkeeping.

### c3. Read-back feedback (the frame drives the sound/next frame)
Sample `screen.pixels` and feed the statistics back into synthesis or the next
frame's parameters. `disks/seashells.mjs:371-488` samples ~12–20 probe points,
averages their RGB, and derives `timeModulation`/`shiftMod`/`contrast` for its
bytebeat — the picture literally plays itself.

**Additive accumulation** (trails without any veil): write with `min(255, old +
n)` so bright marks pile up and slowly clip — `disks/seashells.mjs:576`
(`screen.pixels[i] = Math.min(255, screen.pixels[i] + 64)`).

**Performance caveat.** c1/c2 are cheap — one `box` + one engine transform.
`zoom`/`spin`/`suck` are **accumulating**: small values below a threshold buffer
up and apply in bursts (`graph.mjs:6597` zoom threshold 0.05, `:7016` suck 0.5),
so `zoom(1.008)` every frame is smooth, not free-running. c3 pays the per-pixel
read cost; sample a handful of probe points, not the whole frame.

---

## (d) Supersampling / resolution / density

Covered in depth in §0. In-piece control:

**`resolution(width, height, gap?)`** — pins the logical framebuffer. Call it in
`boot`. Lower = chunkier pixels + cheaper per-pixel loops; the reel upscales it
nearest-neighbor to 1080×1920.

```js
function boot({ resolution }) {
  resolution(360, 640);        // match density-3 reel; guarantees the loop budget
  // resolution(240, 426);     // chunkier + ~2× cheaper loops, still reads clean
}
```

**Manual supersampling (anti-aliased downsample).** For smooth edges without a
huge live loop, render into an offscreen `painting()` at **2× your logical
screen**, then `paste` it down at scale 0.5. The downscale averages/samples and
softens jaggies — poor-man's SSAA. (This is `stample`'s scaled-`paste` idea,
`disks/stample.mjs:649`, generalized.)
```js
const big = painting(w * 2, h * 2, (p) => drawScene(p));  // render at 2×
paste(big, 0, 0, 0.5);                                     // composite down at ½
```

Pieces that call `resolution(...)`: `disks/digitpain0.mjs` (`resolution(1000,
1250)` — fixed 4:5), `disks/keep.mjs`, `disks/melody.mjs`, `disks/noise.mjs`.
`disks/wipppps.mjs` skips it and instead does **adaptive block rendering** — big
blocks when audio is quiet/zoomed-out, per-pixel only where detail matters
(`renderFractalBlocks`) — a great pattern when a full per-pixel loop is too dear.

**Performance caveat.** Rendering at 2× to downsample is 4× the pixels of your
logical screen — only worth it if edge quality matters more than the loop budget.
At the reel's density 3 that's 720×1280 into the offscreen buffer; fine for
vector scenes, heavy for per-pixel ones.

---

## (e) Whole-frame transforms: blur, zoom, spin, scroll, contrast, sort, suck, shear

These operate on the **entire current framebuffer** (or the active `mask`
region, §f) in one call — the engine does the pixel loop for you in optimized
(often GPU-accelerated) code. This is how you get expensive-looking pixel FX
without writing the loop. All are exposed as `paint` params.

| fn | signature (`graph.mjs`) | what it does | notes |
|----|------------------------|--------------|-------|
| `blur(strength, quality?)` | `:7164` | separable Gaussian | `strength` accumulates (threshold 0.5); GPU path if available. `quality`: `"low"`/`"medium"`/`"high"`. |
| `zoom(level, ax=.5, ay=.5)` | `:6597` | scale about anchor (0–1) | small deltas accumulate (thr 0.05); `zoom(1.01)` → slow bloom, `zoom(0.99)` → inward suck |
| `spin(steps, ax?, ay?)` | `:6165` | rotate framebuffer | `steps` in degrees-ish; small values give smooth drift |
| `scroll(dx, dy)` | `:5490` | wrap-around pixel shift | fractional amounts accumulate; wraps at edges → seamless conveyor |
| `suck(strength, cx?, cy?)` | `:7012` | radial pull/push | `+` sucks toward center, `−` blows outward |
| `shear(sx, sy)` | `:7639` | KidPix-style skew | accumulates; great glitch/lean |
| `contrast(level=1)` | `:1939` | push/pull around mid-gray | `>1` crunch, `<1` wash; GPU path |
| `brightness(adj)` | `:2022` | add/subtract luma | |
| `invert()` | `:2094` | negate RGB | strobe-y on the beat |
| `sort()` | `:7514` | **pixel-sort** current region by luminance | dark→light; scope it with `mask` (§f) or it sorts the whole frame into a gradient |
| `sharpen(strength)` | `:7405` | unsharp | counter to blur |

**Example pieces.** `disks/prism.mjs` (zoom+spin+blur feedback),
`disks/wipppps.mjs` (blur, contrast on audio), `disks/keep.mjs` /
`disks/wand.mjs` (zoom, spin, contrast, steal), `disks/morpho.mjs` (a
hand-rolled pixel-sorter, instructive counterpoint to the built-in `sort()`).

**Snippet — audio-reactive frame FX stack:**
```js
function paint({ ink, box, screen, sound, zoom, spin, blur, contrast }) {
  const { width: w, height: h } = screen;
  const amp  = sound.speaker?.amplitudes?.left || 0;
  const bass = (sound.speaker?.frequencies?.left || [])
                 .find((b) => b.name === "subBass")?.amplitude || 0;

  zoom(1.006 + bass * 0.02);          // breathe with the low end
  spin(0.3 + amp * 2);                // rotate harder when loud
  ink(8, 6, 20, 34).box(0, 0, w, h);  // trail veil (must follow transforms)
  // … draw new geometry here …
  if (bass > 0.5) blur(1);            // bloom on kicks
  contrast(1.02 + amp * 0.06);        // keep it punchy
}
```

**Snippet — masked pixel-sort ribbon (glitch band):**
```js
// Sort only a horizontal band → a smeared "data-mosh" stripe.
const band = { x: 0, y: (h * 0.4) | 0, width: w, height: (h * 0.2) | 0 };
mask(band); sort(); unmask();
```

**Performance caveat.** These are the *cheap* way to do full-frame pixel FX —
the loop is native/GPU. `blur`/`zoom`/`spin`/`suck`/`scroll` **accumulate
sub-threshold deltas**, so feeding tiny values every frame is smooth and doesn't
thrash. `sort()` is O(n log n) over the region and allocates an array of every
pixel (`graph.mjs:7531-7554`) — mask it to a band for both speed and looks.
Stacking many per frame is fine; stacking `sort()` on the full frame every frame
is the one to watch.

---

## (f) Compositing: `paste`, `stamp`, `mask`/`unmask`, `pan`/`unpan`, `steal`/`putback`

**`paste(from, x=0, y=0, scale=1, blit=false)`** (`graph.mjs:2163`,
`disk.mjs:6432`) — blit a buffer (or `painting()`, or a cached bitmap by
`"$code"` string) onto the current target. `scale` can be a number **or a
transform object** `{ scale, angle, width, height, anchor, crop }` — so `paste`
also rotates, resizes, and crops:
```js
paste(buf, x, y, { scale: 1.4, angle: 30 });   // rotate + scale
paste(buf, x, y, { width: w, height: h });     // stretch to fit a zone
paste("$bmpcode", 0, 0);                        // paste a cached bitmap by code
```

**`stamp(from, x, y, scale?, angle?)`** (`graph.mjs:2698`) — like `paste` but
**centered** on `(x, y)`. Ideal for sprites/particles that spin about their own
middle. `disk.mjs:6447` also accepts `{ center:"x", bottom:N }` layout objects.

**`mask(box)` / `unmask()`** (`graph.mjs:1826`) — clip *all* subsequent drawing
**and every §e transform** to `box = {x,y,width,height}`. This is how you localize
blur/sort/scroll/zoom to a region. Set it, draw/transform, clear it.

**`pan(x, y)` / `unpan()`** (`graph.mjs:1795`) — translate the drawing origin;
everything (including `paste` dest) is offset until `unpan()`. Cheap camera
shake / parallax.

**`steal(x, y, w, h)` → `putback(x, y, scale=1)`** (`graph.mjs:7624`,`:7630`) —
copy a screen region into a stashed buffer, then stamp it back elsewhere
(optionally scaled). Instant mirror/echo/kaleidoscope tiles with no manual
buffer handling. Used in `disks/keep.mjs`, `disks/sister.mjs`,
`disks/painting.mjs`.

**Example pieces.** `disks/chat.mjs` / `disks/mug.mjs` (heavy `paste`),
`disks/visualizer.mjs` (`page`+`paste` layer), `disks/stample.mjs` (scaled
`paste` to fit zones).

**Snippet — steal/putback echo tiles + camera shake:**
```js
function paint({ steal, putback, pan, unpan, screen, sound }) {
  const { width: w, height: h } = screen;
  const kick = sound.speaker?.beat?.detected ? 6 : 0;
  pan((Math.random()*2-1)*kick, (Math.random()*2-1)*kick); // shake on beat
  // … draw the scene …
  unpan();
  steal(0, 0, w, h / 2);          // grab the top half
  putback(0, h / 2, 1);           // mirror it into the bottom → instant symmetry
}
```

**Performance caveat.** `paste`/`stamp` are optimized (integer-scale fast path,
`graph.mjs:2214`) — tiny scales (`|scale|<0.01`) are skipped, and `stamp` frame-
skips when FPS<5 (`:2715`). `steal` allocates a buffer per call — steal once,
`putback` many. Masking a transform to a small box is *faster* than the full
frame, so `mask` is a performance tool as well as an aesthetic one.

---

## (g) Gradient & fade fills

**What it does.** `ink("fade:colorA-colorB[-colorC]:direction", alpha)` paints
with a **gradient** instead of a flat color — usable in `box`, `wipe`, `poly`,
etc. Directions include `vertical`, `horizontal`, and angle/positional variants
(`graph.mjs:1310` `evaluateFadeDirection`, `:288` `parseLocalFade`). Special
keywords `rainbow` and `zebra` are legal color stops, and `cN` color indices
work too.

```js
ink("fade:midnightblue-rebeccapurple:vertical", 46).box(0, 0, w, h);
ink("fade:yellow-white:vertical").box(x, 0, barW, h);   // 1-frame flash
ink("fade:red-orange-yellow:horizontal").poly(pts);     // 3-stop gradient
```

**Example pieces.** `disks/lull.mjs:106` uses a gradient **as its trail veil** —
so the whole feedback field is tinted along a dawn ramp:
```js
// disks/lull.mjs:106 — gradient veil = colored trails, not gray ones
ink("fade:midnightblue-rebeccapurple:vertical", 46).box(0, 0, w, h);
```
`disks/visualizer.mjs:1529` flashes new timeline bars with
`ink("fade:yellow-white:vertical")`.

**Manual gradient (full control).** For a moving/animated ramp, compute per-row
color yourself and `box` scanlines, or write `screen.pixels` directly — see
`disks/lull.mjs`'s `hslToRgb` ring coloring and `disks/emberdrift.mjs`'s
`hue2rgb` helper (`num.hslToRgb(hueDeg, sat0-100, light0-100) → [r,g,b] 0-255`;
note it wants **degrees + 0–100**, and already returns 0–255 — don't re-multiply,
that's the classic white-out bug, `emberdrift.mjs:150-155`).

**Performance caveat.** `fade:` fills are engine-side and cheap — prefer them
over hand-rolled per-pixel gradients unless you need per-frame animation of the
stops. A `fade:` veil costs the same as a flat veil but reads far richer.

---

## Recipes for luscious reels

Combined technique stacks. Each is a drop-in `paint` skeleton; wire the audio
reads (`sound.speaker.frequencies/amplitudes/beat`) as in `prism`/`lull`.

### R1 — Supersampled feedback bloom
Offscreen 2× render → downscale for smooth edges, over a zoom+spin feedback base.
Combines §b + §c2 + §d.
```js
let acc;
function paint({ painting, paste, zoom, spin, blur, ink, box, screen, sound }) {
  const { width: w, height: h } = screen;
  const bass = (sound.speaker?.frequencies?.left||[]).find(b=>b.name==="subBass")?.amplitude||0;

  // 1) recycle previous frame, flow it outward
  zoom(1.008 + bass * 0.02); spin(0.4);
  ink(6, 4, 16, 30).box(0, 0, w, h);

  // 2) render fresh geometry at 2× into an offscreen buffer (crisp edges)
  const big = painting(w * 2, h * 2, (p) => {
    p.ink("rainbow", 220).circle(w + Math.sin(performance.now()*0.002)*w*0.5, h, 40 + bass*120, true);
  });
  paste(big, 0, 0, 0.5);       // 3) downsample-composite (SSAA)
  if (bass > 0.5) blur(1);     // 4) bloom on kicks
}
```

### R2 — Per-pixel liquid displacement
Read the framebuffer and re-sample each pixel from a sine-warped source → the
image "flows." Pure §a; keep the logical resolution low (§d).
```js
function boot({ resolution }) { resolution(300, 533); }  // cheap loop budget
let prev;
function paint({ screen }) {
  const { width: w, height: h, pixels } = screen;
  if (!prev || prev.length !== pixels.length) prev = new Uint8ClampedArray(pixels);
  else prev.set(pixels);                    // snapshot last frame
  const t = performance.now() * 0.001;
  for (let y = 0; y < h; y++) {
    const oy = (Math.sin(y * 0.05 + t) * 4) | 0;
    for (let x = 0; x < w; x++) {
      const ox = (Math.sin(y * 0.08 + t * 1.3) * 5) | 0;   // horizontal wobble
      const sx = Math.min(w-1, Math.max(0, x + ox));
      const sy = Math.min(h-1, Math.max(0, y + oy));
      const si = (sy * w + sx) * 4, di = (y * w + x) * 4;
      pixels[di]   = prev[si]   * 0.96 + 4;   // displace + gentle fade → trails
      pixels[di+1] = prev[si+1] * 0.96;
      pixels[di+2] = prev[si+2] * 0.97 + 8;
      pixels[di+3] = 255;
    }
  }
  // seed a bright source somewhere so there's something to smear:
  // ink("rainbow",255).circle(...) after the loop.
}
```
(Displacement/RGB-shift reference: `disks/metaballs.mjs:340-434` — per-row
horizontal shift + per-channel offset = chromatic glitch.)

### R3 — Chromatic buffer trails (RGB-split feedback)
Scroll the three color channels by different amounts each frame → the trail
smears into red/green/blue ghosts. §c + §a.
```js
function paint({ screen, ink, box, scroll }) {
  const { width: w, height: h, pixels } = screen;
  // channel-split by copying shifted rows (cheap: only 2 of 3 channels move)
  for (let y = 0; y < h; y++) {
    for (let x = w - 1; x >= 2; x--) {
      const i = (y * w + x) * 4;
      pixels[i]   = pixels[i - 2*4];        // red lags 2px  → left ghost
      pixels[i+2] = pixels[i + 0];          // blue as-is
    }
  }
  scroll(0, -1);                            // whole frame drifts up → vertical smear
  ink(0, 0, 0, 18).box(0, 0, w, h);         // slow fade so ghosts don't saturate
  // … draw new colored marks on top …
}
```

### R4 — Kaleidoscope via steal/putback + spin
Mirror-tile the frame and rotate the whole thing → endless symmetry with almost
no code. §e + §f. (This is what `prism` approximates with 8-fold geometry;
`steal`/`putback` gets you there from *any* source.)
```js
function paint({ steal, putback, spin, ink, box, screen, sound }) {
  const { width: w, height: h } = screen;
  const amp = sound.speaker?.amplitudes?.left || 0;
  spin(0.5 + amp * 3);                       // rotate the accumulated frame
  ink(4, 2, 12, 26).box(0, 0, w, h);         // trail veil
  // … draw a single asymmetric wedge of new content …
  steal(0, 0, w / 2, h);                     // grab left half
  putback(w / 2, 0, 1);                      // mirror into right half → bilateral symmetry
  steal(0, 0, w, h / 2);                     // grab top half
  putback(0, h / 2, 1);                      // mirror into bottom → 4-fold
}
```

---

## Quick reference — signatures (from source)

```
screen.pixels        Uint8ClampedArray, RGBA, i = (y*width + x)*4      disk.mjs:7370
pixel(x,y,buf?)      → [r,g,b,a] read                                  graph.mjs:757
edit(fn)             fn(pixels, width, height)                          graph.mjs:752
painting(w,h,fn)     → {pixels,width,height} offscreen buffer           disk.mjs:7254
page(buf)            redirect drawing to buf; page(screen) restores     disk.mjs:6311
paste(from,x,y,scale|xform,blit?)  blit buffer/"$code"/painting         graph.mjs:2163
stamp(from,x,y,scale?,angle?)      centered paste                       graph.mjs:2698
steal(x,y,w,h) / putback(x,y,scale=1)  grab & stamp a region           graph.mjs:7624
mask(box) / unmask()  clip drawing + transforms to {x,y,width,height}   graph.mjs:1826
pan(x,y) / unpan()    translate origin                                  graph.mjs:1795
blur(strength,quality?)   separable gaussian (accumulates)              graph.mjs:7164
zoom(level,ax=.5,ay=.5)   scale about anchor (accumulates <0.1)         graph.mjs:6597
spin(steps,ax?,ay?)       rotate framebuffer                           graph.mjs:6165
scroll(dx,dy)             wrap-around shift (accumulates)               graph.mjs:5490
suck(strength,cx?,cy?)    radial pull(+)/push(-)                       graph.mjs:7012
shear(sx,sy)              skew (accumulates)                           graph.mjs:7639
contrast(level=1) / brightness(adj) / invert()   tone ops              graph.mjs:1939
sort()                    luminance pixel-sort of region/mask          graph.mjs:7514
sharpen(strength)         unsharp                                      graph.mjs:7405
ink("fade:a-b:dir", alpha)  gradient fill (also rainbow/zebra/cN)      graph.mjs:288
noise16 / noise16Aesthetic / noise16Sotce / noiseTinted(tint,amt,sat)  graph.mjs:5337+
resolution(w,h,gap?)      pin logical framebuffer (in boot)            disk.mjs:1106
```

## Underused / high-leverage moves (author's shortlist)

1. **`density`/`resolution` as the loop-budget dial** — nobody thinks of it, but
   it's the difference between a smooth per-pixel reel and a slideshow. Author at
   density 3 (360×640) and lean into the chunk.
2. **`sort()` masked to a band** — one line, instant Kim-Asendorf data-mosh
   stripe; almost no reel piece uses it.
3. **`steal`/`putback`** — free mirror/kaleidoscope tiling with zero buffer
   bookkeeping; far simpler than the hand-rolled 8-fold geometry in `prism`.
4. **`fade:` gradient *as the trail veil*** (`lull`) — colors your entire
   feedback field along a ramp for the same cost as a gray veil.
5. **Transform-feedback (`zoom`+`spin`+veil)** — the cheapest path to
   "expensive-looking" motion; the whole previous frame is your particle system.
6. **`paste` at 2× → downscale** — poor-man's anti-aliasing that keeps edges
   silky without a native-res per-pixel loop.
```
