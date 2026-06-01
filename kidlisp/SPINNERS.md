# Spinners — Variable-Spin KidLisp Catalog

A 256-piece design space of `(wipe fade:<palette>:<motion-expr>)` kidlisp
pieces. Each piece is a one-line fade gradient whose direction is animated
by an arbitrary expression over the `frame` counter.

The original seed was the minted piece **$wwi** = `(wipe fade:red-rainbow)`
listed at 10 XTZ on keeps.kidlisp.com. The catalog below cross-multiplies
16 palettes × 16 motion expressions for 256 distinct minable variations.

---

## Tools built this session

All under `kidlisp/tools/`. Each is self-contained and runs against a local
dev server (`npm run site`, port 8888) or production (`https://aesthetic.computer`).

### `gen-spinners.mjs`

Emits the full catalog of 256 spinner sources. Pure data, no IO.

```
node kidlisp/tools/gen-spinners.mjs                     # plain sources, one per line
node kidlisp/tools/gen-spinners.mjs --json              # full JSON with palette + motion
node kidlisp/tools/gen-spinners.mjs --catalog           # human-readable index
node kidlisp/tools/gen-spinners.mjs --out sources.txt   # write to file
```

### `preview-anim.mjs`

Renders a single kidlisp source as mp4 (or animated webp). Uses puppeteer +
Chrome's `Page.startScreencast` for real-time capture at AC's actual ~60fps
paint rate. Output format chosen by extension: `.mp4` → libx264 h264, `.webp`
→ img2webp.

```
node kidlisp/tools/preview-anim.mjs '(wipe fade:red-rainbow:frame)' \
  --out ~/Desktop/spin.mp4 \
  --duration 4000 --size 320x240 --scale 2 --quality 95
```

Flags: `--size` (AC virtual resolution), `--scale` (output pixel multiplier,
ffmpeg nearest-neighbor upscales), `--duration`, `--fps`, `--quality`,
`--boot`, `--settle`, `--keep`.

The rig does a 2-pass warmup: pass 1 navigates and populates AC's
IndexedDB BDF glyph cache (so the QR overlay's MatrixChunky8 label has
real glyphs available). Pass 2 reloads and captures from t=0 with the
warm cache.

### `preview-grid.mjs`

Renders N sources as static thumbnails in a single grid PNG.

```
node kidlisp/tools/gen-spinners.mjs --out /tmp/srcs.txt
node kidlisp/tools/preview-grid.mjs --sources /tmp/srcs.txt \
  --out ~/Desktop/spinners-256.png \
  --cols 16 --cell 96x72 --boot 1500 --settle 1000 --label
```

Has crash recovery — if Chrome dies mid-render, the rig auto-relaunches
the browser and continues with a black tile in the failed slot.

### `preview-grid-anim.mjs`

Renders N sources as animated cells composed into a single grid mp4 via
per-frame sharp composites + ffmpeg encode.

```
node kidlisp/tools/preview-grid-anim.mjs --sources /tmp/srcs.txt \
  --out ~/Desktop/spinners-anim-256.mp4 \
  --cols 16 --cell 96x72 --duration 4000 --fps 15 \
  --boot 2000 --settle 1500
```

Uses `Page.startScreencast` with `everyNthFrame` to downsample from AC's
~60fps paint rate to the requested output fps — one persistent CDP
connection per cell instead of N screenshot requests, drastically friendlier
to the server.

---

## AC fix shipped this session

**`system/public/aesthetic.computer/lib/disk.mjs` lines 15554 and 15954** —
QR overlay's `isLoaded` glyph check.

The BDF font system returns an animated "loading placeholder" glyph for
chars whose Proxy access hits an unloaded entry. That placeholder has
`resolution`, `pixels`, and `advance` fields set, so the QR overlay's
existing check (`glyph && (glyph.resolution || glyph.pixels || glyph.commands)`)
treated it as "loaded" — caching a label rendered with the placeholder
pattern instead of the real `$xxx` code. Added `!glyph.isPlaceholder` to
both check sites so the QR cache properly waits for real glyphs.

```js
// before
const isRealGlyph = glyph && glyph !== null &&
  (glyph.dwidth || glyph.advance !== undefined || glyph.resolution);
// after
const isRealGlyph = glyph && glyph !== null && !glyph.isPlaceholder &&
  (glyph.dwidth || glyph.advance !== undefined || glyph.resolution);
```

---

## The design space

### 16 palettes

| name | fade string |
|---|---|
| red-rainbow | `red-rainbow` |
| orange-rainbow | `orange-rainbow` |
| yellow-rainbow | `yellow-rainbow` |
| green-rainbow | `green-rainbow` |
| red-band | `black-red-black` |
| orange-band | `black-orange-black` |
| yellow-band | `black-yellow-black` |
| green-band | `black-green-black` |
| blue-band | `black-blue-black` |
| indigo-band | `black-indigo-black` |
| violet-band | `black-violet-black` |
| sunset | `black-orange-red-purple-black` |
| ocean | `navy-blue-cyan-teal-navy` |
| fire | `black-red-yellow-red-black` |
| ice | `white-cyan-blue-indigo-black` |
| vapor | `pink-cyan-purple-cyan-pink` |

(Plus 10 more in the generator: `midnight`, `lava`, `aurora`, `sakura`,
`candy`, `jungle`, `autumn`, `cosmos`, `purple-red`, `roygbiv`,
`roygbiv-reverse` — only the first 16 fit into 256 with 16 motions each.)

### 16 motion expressions

The angle param after the second `:` in `fade:colors:<expr>` is an
s-expression evaluated each paint with `frame` as the AC frame counter.

| name | expression |
|---|---|
| spin | `frame` |
| fast-spin | `(* frame 5)` |
| slow-spin | `(/ frame 3)` |
| swing | `(+ frame (* (sin (* frame 0.05)) 60))` |
| wobble | `(+ frame (* (sin frame) 50))` |
| accelerate | `(* frame frame 0.001)` |
| decelerate | `(* (sqrt frame) 30)` |
| bounce | `(abs (* (sin (* frame 0.03)) 360))` |
| pendulum | `(* (sin (* frame 0.02)) 180)` |
| pulse | `(+ frame (* (sin (* frame 0.1)) 30))` |
| jitter | `(+ frame (* (sin (* frame 2)) 20))` |
| reverse | `(- 0 frame)` |
| chaos | `(+ frame (* (sin (* frame 0.07)) (cos (* frame 0.13)) 200))` |
| meditative | `(* (sin (* frame 0.01)) 90)` |
| exp-sin | `(* frame (sin (* frame 0.05)))` |
| double | `(+ frame (* (sin (* frame 0.1)) (sin (* frame 0.03)) 100))` |

Variable spin works because the kidlisp evaluator was patched earlier to
re-glue tokenizer-split `fade:colors:` strings with their following
s-expression arg. See the `args.length >= 2` re-glue pass in
`system/public/aesthetic.computer/lib/kidlisp.mjs` around line 9890.

---

## Reproducing the artifacts

```
# Static 256-cell contact sheet (~25 min against local, ~30 min against prod)
node kidlisp/tools/gen-spinners.mjs --out /tmp/srcs.txt
node kidlisp/tools/preview-grid.mjs --sources /tmp/srcs.txt \
  --out ~/Desktop/spinners-256.png --cols 16 --cell 96x72 \
  --boot 1500 --settle 1000 --label

# Animated 256-cell grid mp4 (~30 min against prod with screencast)
node kidlisp/tools/preview-grid-anim.mjs --sources /tmp/srcs.txt \
  --out ~/Desktop/spinners-anim-256.mp4 --base https://aesthetic.computer \
  --cols 16 --cell 96x72 --duration 4000 --fps 15 \
  --boot 2000 --settle 1500

# Individual mp4 of a single piece (~15s per render)
node kidlisp/tools/preview-anim.mjs \
  '(wipe fade:black-purple-red-purple-black:(+ frame (* (sin (* frame 0.05)) 60)))' \
  --out ~/Desktop/spin.mp4 --duration 4000 --size 320x240 --scale 2 --quality 95
```

---

## Full catalog — 256 sources

### red-rainbow (red-rainbow)

| # | motion | source |
|---|---|---|
| 000 | spin | `(wipe fade:red-rainbow:frame)` |
| 001 | fast-spin | `(wipe fade:red-rainbow:(* frame 5))` |
| 002 | slow-spin | `(wipe fade:red-rainbow:(/ frame 3))` |
| 003 | swing | `(wipe fade:red-rainbow:(+ frame (* (sin (* frame 0.05)) 60)))` |
| 004 | wobble | `(wipe fade:red-rainbow:(+ frame (* (sin frame) 50)))` |
| 005 | accelerate | `(wipe fade:red-rainbow:(* frame frame 0.001))` |
| 006 | decelerate | `(wipe fade:red-rainbow:(* (sqrt frame) 30))` |
| 007 | bounce | `(wipe fade:red-rainbow:(abs (* (sin (* frame 0.03)) 360)))` |
| 008 | pendulum | `(wipe fade:red-rainbow:(* (sin (* frame 0.02)) 180))` |
| 009 | pulse | `(wipe fade:red-rainbow:(+ frame (* (sin (* frame 0.1)) 30)))` |
| 010 | jitter | `(wipe fade:red-rainbow:(+ frame (* (sin (* frame 2)) 20)))` |
| 011 | reverse | `(wipe fade:red-rainbow:(- 0 frame))` |
| 012 | chaos | `(wipe fade:red-rainbow:(+ frame (* (sin (* frame 0.07)) (cos (* frame 0.13)) 200)))` |
| 013 | meditative | `(wipe fade:red-rainbow:(* (sin (* frame 0.01)) 90))` |
| 014 | exp-sin | `(wipe fade:red-rainbow:(* frame (sin (* frame 0.05))))` |
| 015 | double | `(wipe fade:red-rainbow:(+ frame (* (sin (* frame 0.1)) (sin (* frame 0.03)) 100)))` |

### orange-rainbow (orange-rainbow)

| # | motion | source |
|---|---|---|
| 016 | spin | `(wipe fade:orange-rainbow:frame)` |
| 017 | fast-spin | `(wipe fade:orange-rainbow:(* frame 5))` |
| 018 | slow-spin | `(wipe fade:orange-rainbow:(/ frame 3))` |
| 019 | swing | `(wipe fade:orange-rainbow:(+ frame (* (sin (* frame 0.05)) 60)))` |
| 020 | wobble | `(wipe fade:orange-rainbow:(+ frame (* (sin frame) 50)))` |
| 021 | accelerate | `(wipe fade:orange-rainbow:(* frame frame 0.001))` |
| 022 | decelerate | `(wipe fade:orange-rainbow:(* (sqrt frame) 30))` |
| 023 | bounce | `(wipe fade:orange-rainbow:(abs (* (sin (* frame 0.03)) 360)))` |
| 024 | pendulum | `(wipe fade:orange-rainbow:(* (sin (* frame 0.02)) 180))` |
| 025 | pulse | `(wipe fade:orange-rainbow:(+ frame (* (sin (* frame 0.1)) 30)))` |
| 026 | jitter | `(wipe fade:orange-rainbow:(+ frame (* (sin (* frame 2)) 20)))` |
| 027 | reverse | `(wipe fade:orange-rainbow:(- 0 frame))` |
| 028 | chaos | `(wipe fade:orange-rainbow:(+ frame (* (sin (* frame 0.07)) (cos (* frame 0.13)) 200)))` |
| 029 | meditative | `(wipe fade:orange-rainbow:(* (sin (* frame 0.01)) 90))` |
| 030 | exp-sin | `(wipe fade:orange-rainbow:(* frame (sin (* frame 0.05))))` |
| 031 | double | `(wipe fade:orange-rainbow:(+ frame (* (sin (* frame 0.1)) (sin (* frame 0.03)) 100)))` |

### yellow-rainbow (yellow-rainbow)

| # | motion | source |
|---|---|---|
| 032 | spin | `(wipe fade:yellow-rainbow:frame)` |
| 033 | fast-spin | `(wipe fade:yellow-rainbow:(* frame 5))` |
| 034 | slow-spin | `(wipe fade:yellow-rainbow:(/ frame 3))` |
| 035 | swing | `(wipe fade:yellow-rainbow:(+ frame (* (sin (* frame 0.05)) 60)))` |
| 036 | wobble | `(wipe fade:yellow-rainbow:(+ frame (* (sin frame) 50)))` |
| 037 | accelerate | `(wipe fade:yellow-rainbow:(* frame frame 0.001))` |
| 038 | decelerate | `(wipe fade:yellow-rainbow:(* (sqrt frame) 30))` |
| 039 | bounce | `(wipe fade:yellow-rainbow:(abs (* (sin (* frame 0.03)) 360)))` |
| 040 | pendulum | `(wipe fade:yellow-rainbow:(* (sin (* frame 0.02)) 180))` |
| 041 | pulse | `(wipe fade:yellow-rainbow:(+ frame (* (sin (* frame 0.1)) 30)))` |
| 042 | jitter | `(wipe fade:yellow-rainbow:(+ frame (* (sin (* frame 2)) 20)))` |
| 043 | reverse | `(wipe fade:yellow-rainbow:(- 0 frame))` |
| 044 | chaos | `(wipe fade:yellow-rainbow:(+ frame (* (sin (* frame 0.07)) (cos (* frame 0.13)) 200)))` |
| 045 | meditative | `(wipe fade:yellow-rainbow:(* (sin (* frame 0.01)) 90))` |
| 046 | exp-sin | `(wipe fade:yellow-rainbow:(* frame (sin (* frame 0.05))))` |
| 047 | double | `(wipe fade:yellow-rainbow:(+ frame (* (sin (* frame 0.1)) (sin (* frame 0.03)) 100)))` |

### green-rainbow (green-rainbow)

| # | motion | source |
|---|---|---|
| 048 | spin | `(wipe fade:green-rainbow:frame)` |
| 049 | fast-spin | `(wipe fade:green-rainbow:(* frame 5))` |
| 050 | slow-spin | `(wipe fade:green-rainbow:(/ frame 3))` |
| 051 | swing | `(wipe fade:green-rainbow:(+ frame (* (sin (* frame 0.05)) 60)))` |
| 052 | wobble | `(wipe fade:green-rainbow:(+ frame (* (sin frame) 50)))` |
| 053 | accelerate | `(wipe fade:green-rainbow:(* frame frame 0.001))` |
| 054 | decelerate | `(wipe fade:green-rainbow:(* (sqrt frame) 30))` |
| 055 | bounce | `(wipe fade:green-rainbow:(abs (* (sin (* frame 0.03)) 360)))` |
| 056 | pendulum | `(wipe fade:green-rainbow:(* (sin (* frame 0.02)) 180))` |
| 057 | pulse | `(wipe fade:green-rainbow:(+ frame (* (sin (* frame 0.1)) 30)))` |
| 058 | jitter | `(wipe fade:green-rainbow:(+ frame (* (sin (* frame 2)) 20)))` |
| 059 | reverse | `(wipe fade:green-rainbow:(- 0 frame))` |
| 060 | chaos | `(wipe fade:green-rainbow:(+ frame (* (sin (* frame 0.07)) (cos (* frame 0.13)) 200)))` |
| 061 | meditative | `(wipe fade:green-rainbow:(* (sin (* frame 0.01)) 90))` |
| 062 | exp-sin | `(wipe fade:green-rainbow:(* frame (sin (* frame 0.05))))` |
| 063 | double | `(wipe fade:green-rainbow:(+ frame (* (sin (* frame 0.1)) (sin (* frame 0.03)) 100)))` |

### red-band (black-red-black)

| # | motion | source |
|---|---|---|
| 064 | spin | `(wipe fade:black-red-black:frame)` |
| 065 | fast-spin | `(wipe fade:black-red-black:(* frame 5))` |
| 066 | slow-spin | `(wipe fade:black-red-black:(/ frame 3))` |
| 067 | swing | `(wipe fade:black-red-black:(+ frame (* (sin (* frame 0.05)) 60)))` |
| 068 | wobble | `(wipe fade:black-red-black:(+ frame (* (sin frame) 50)))` |
| 069 | accelerate | `(wipe fade:black-red-black:(* frame frame 0.001))` |
| 070 | decelerate | `(wipe fade:black-red-black:(* (sqrt frame) 30))` |
| 071 | bounce | `(wipe fade:black-red-black:(abs (* (sin (* frame 0.03)) 360)))` |
| 072 | pendulum | `(wipe fade:black-red-black:(* (sin (* frame 0.02)) 180))` |
| 073 | pulse | `(wipe fade:black-red-black:(+ frame (* (sin (* frame 0.1)) 30)))` |
| 074 | jitter | `(wipe fade:black-red-black:(+ frame (* (sin (* frame 2)) 20)))` |
| 075 | reverse | `(wipe fade:black-red-black:(- 0 frame))` |
| 076 | chaos | `(wipe fade:black-red-black:(+ frame (* (sin (* frame 0.07)) (cos (* frame 0.13)) 200)))` |
| 077 | meditative | `(wipe fade:black-red-black:(* (sin (* frame 0.01)) 90))` |
| 078 | exp-sin | `(wipe fade:black-red-black:(* frame (sin (* frame 0.05))))` |
| 079 | double | `(wipe fade:black-red-black:(+ frame (* (sin (* frame 0.1)) (sin (* frame 0.03)) 100)))` |

### orange-band (black-orange-black)

| # | motion | source |
|---|---|---|
| 080 | spin | `(wipe fade:black-orange-black:frame)` |
| 081 | fast-spin | `(wipe fade:black-orange-black:(* frame 5))` |
| 082 | slow-spin | `(wipe fade:black-orange-black:(/ frame 3))` |
| 083 | swing | `(wipe fade:black-orange-black:(+ frame (* (sin (* frame 0.05)) 60)))` |
| 084 | wobble | `(wipe fade:black-orange-black:(+ frame (* (sin frame) 50)))` |
| 085 | accelerate | `(wipe fade:black-orange-black:(* frame frame 0.001))` |
| 086 | decelerate | `(wipe fade:black-orange-black:(* (sqrt frame) 30))` |
| 087 | bounce | `(wipe fade:black-orange-black:(abs (* (sin (* frame 0.03)) 360)))` |
| 088 | pendulum | `(wipe fade:black-orange-black:(* (sin (* frame 0.02)) 180))` |
| 089 | pulse | `(wipe fade:black-orange-black:(+ frame (* (sin (* frame 0.1)) 30)))` |
| 090 | jitter | `(wipe fade:black-orange-black:(+ frame (* (sin (* frame 2)) 20)))` |
| 091 | reverse | `(wipe fade:black-orange-black:(- 0 frame))` |
| 092 | chaos | `(wipe fade:black-orange-black:(+ frame (* (sin (* frame 0.07)) (cos (* frame 0.13)) 200)))` |
| 093 | meditative | `(wipe fade:black-orange-black:(* (sin (* frame 0.01)) 90))` |
| 094 | exp-sin | `(wipe fade:black-orange-black:(* frame (sin (* frame 0.05))))` |
| 095 | double | `(wipe fade:black-orange-black:(+ frame (* (sin (* frame 0.1)) (sin (* frame 0.03)) 100)))` |

### yellow-band (black-yellow-black)

| # | motion | source |
|---|---|---|
| 096 | spin | `(wipe fade:black-yellow-black:frame)` |
| 097 | fast-spin | `(wipe fade:black-yellow-black:(* frame 5))` |
| 098 | slow-spin | `(wipe fade:black-yellow-black:(/ frame 3))` |
| 099 | swing | `(wipe fade:black-yellow-black:(+ frame (* (sin (* frame 0.05)) 60)))` |
| 100 | wobble | `(wipe fade:black-yellow-black:(+ frame (* (sin frame) 50)))` |
| 101 | accelerate | `(wipe fade:black-yellow-black:(* frame frame 0.001))` |
| 102 | decelerate | `(wipe fade:black-yellow-black:(* (sqrt frame) 30))` |
| 103 | bounce | `(wipe fade:black-yellow-black:(abs (* (sin (* frame 0.03)) 360)))` |
| 104 | pendulum | `(wipe fade:black-yellow-black:(* (sin (* frame 0.02)) 180))` |
| 105 | pulse | `(wipe fade:black-yellow-black:(+ frame (* (sin (* frame 0.1)) 30)))` |
| 106 | jitter | `(wipe fade:black-yellow-black:(+ frame (* (sin (* frame 2)) 20)))` |
| 107 | reverse | `(wipe fade:black-yellow-black:(- 0 frame))` |
| 108 | chaos | `(wipe fade:black-yellow-black:(+ frame (* (sin (* frame 0.07)) (cos (* frame 0.13)) 200)))` |
| 109 | meditative | `(wipe fade:black-yellow-black:(* (sin (* frame 0.01)) 90))` |
| 110 | exp-sin | `(wipe fade:black-yellow-black:(* frame (sin (* frame 0.05))))` |
| 111 | double | `(wipe fade:black-yellow-black:(+ frame (* (sin (* frame 0.1)) (sin (* frame 0.03)) 100)))` |

### green-band (black-green-black)

| # | motion | source |
|---|---|---|
| 112 | spin | `(wipe fade:black-green-black:frame)` |
| 113 | fast-spin | `(wipe fade:black-green-black:(* frame 5))` |
| 114 | slow-spin | `(wipe fade:black-green-black:(/ frame 3))` |
| 115 | swing | `(wipe fade:black-green-black:(+ frame (* (sin (* frame 0.05)) 60)))` |
| 116 | wobble | `(wipe fade:black-green-black:(+ frame (* (sin frame) 50)))` |
| 117 | accelerate | `(wipe fade:black-green-black:(* frame frame 0.001))` |
| 118 | decelerate | `(wipe fade:black-green-black:(* (sqrt frame) 30))` |
| 119 | bounce | `(wipe fade:black-green-black:(abs (* (sin (* frame 0.03)) 360)))` |
| 120 | pendulum | `(wipe fade:black-green-black:(* (sin (* frame 0.02)) 180))` |
| 121 | pulse | `(wipe fade:black-green-black:(+ frame (* (sin (* frame 0.1)) 30)))` |
| 122 | jitter | `(wipe fade:black-green-black:(+ frame (* (sin (* frame 2)) 20)))` |
| 123 | reverse | `(wipe fade:black-green-black:(- 0 frame))` |
| 124 | chaos | `(wipe fade:black-green-black:(+ frame (* (sin (* frame 0.07)) (cos (* frame 0.13)) 200)))` |
| 125 | meditative | `(wipe fade:black-green-black:(* (sin (* frame 0.01)) 90))` |
| 126 | exp-sin | `(wipe fade:black-green-black:(* frame (sin (* frame 0.05))))` |
| 127 | double | `(wipe fade:black-green-black:(+ frame (* (sin (* frame 0.1)) (sin (* frame 0.03)) 100)))` |

### blue-band (black-blue-black)

| # | motion | source |
|---|---|---|
| 128 | spin | `(wipe fade:black-blue-black:frame)` |
| 129 | fast-spin | `(wipe fade:black-blue-black:(* frame 5))` |
| 130 | slow-spin | `(wipe fade:black-blue-black:(/ frame 3))` |
| 131 | swing | `(wipe fade:black-blue-black:(+ frame (* (sin (* frame 0.05)) 60)))` |
| 132 | wobble | `(wipe fade:black-blue-black:(+ frame (* (sin frame) 50)))` |
| 133 | accelerate | `(wipe fade:black-blue-black:(* frame frame 0.001))` |
| 134 | decelerate | `(wipe fade:black-blue-black:(* (sqrt frame) 30))` |
| 135 | bounce | `(wipe fade:black-blue-black:(abs (* (sin (* frame 0.03)) 360)))` |
| 136 | pendulum | `(wipe fade:black-blue-black:(* (sin (* frame 0.02)) 180))` |
| 137 | pulse | `(wipe fade:black-blue-black:(+ frame (* (sin (* frame 0.1)) 30)))` |
| 138 | jitter | `(wipe fade:black-blue-black:(+ frame (* (sin (* frame 2)) 20)))` |
| 139 | reverse | `(wipe fade:black-blue-black:(- 0 frame))` |
| 140 | chaos | `(wipe fade:black-blue-black:(+ frame (* (sin (* frame 0.07)) (cos (* frame 0.13)) 200)))` |
| 141 | meditative | `(wipe fade:black-blue-black:(* (sin (* frame 0.01)) 90))` |
| 142 | exp-sin | `(wipe fade:black-blue-black:(* frame (sin (* frame 0.05))))` |
| 143 | double | `(wipe fade:black-blue-black:(+ frame (* (sin (* frame 0.1)) (sin (* frame 0.03)) 100)))` |

### indigo-band (black-indigo-black)

| # | motion | source |
|---|---|---|
| 144 | spin | `(wipe fade:black-indigo-black:frame)` |
| 145 | fast-spin | `(wipe fade:black-indigo-black:(* frame 5))` |
| 146 | slow-spin | `(wipe fade:black-indigo-black:(/ frame 3))` |
| 147 | swing | `(wipe fade:black-indigo-black:(+ frame (* (sin (* frame 0.05)) 60)))` |
| 148 | wobble | `(wipe fade:black-indigo-black:(+ frame (* (sin frame) 50)))` |
| 149 | accelerate | `(wipe fade:black-indigo-black:(* frame frame 0.001))` |
| 150 | decelerate | `(wipe fade:black-indigo-black:(* (sqrt frame) 30))` |
| 151 | bounce | `(wipe fade:black-indigo-black:(abs (* (sin (* frame 0.03)) 360)))` |
| 152 | pendulum | `(wipe fade:black-indigo-black:(* (sin (* frame 0.02)) 180))` |
| 153 | pulse | `(wipe fade:black-indigo-black:(+ frame (* (sin (* frame 0.1)) 30)))` |
| 154 | jitter | `(wipe fade:black-indigo-black:(+ frame (* (sin (* frame 2)) 20)))` |
| 155 | reverse | `(wipe fade:black-indigo-black:(- 0 frame))` |
| 156 | chaos | `(wipe fade:black-indigo-black:(+ frame (* (sin (* frame 0.07)) (cos (* frame 0.13)) 200)))` |
| 157 | meditative | `(wipe fade:black-indigo-black:(* (sin (* frame 0.01)) 90))` |
| 158 | exp-sin | `(wipe fade:black-indigo-black:(* frame (sin (* frame 0.05))))` |
| 159 | double | `(wipe fade:black-indigo-black:(+ frame (* (sin (* frame 0.1)) (sin (* frame 0.03)) 100)))` |

### violet-band (black-violet-black)

| # | motion | source |
|---|---|---|
| 160 | spin | `(wipe fade:black-violet-black:frame)` |
| 161 | fast-spin | `(wipe fade:black-violet-black:(* frame 5))` |
| 162 | slow-spin | `(wipe fade:black-violet-black:(/ frame 3))` |
| 163 | swing | `(wipe fade:black-violet-black:(+ frame (* (sin (* frame 0.05)) 60)))` |
| 164 | wobble | `(wipe fade:black-violet-black:(+ frame (* (sin frame) 50)))` |
| 165 | accelerate | `(wipe fade:black-violet-black:(* frame frame 0.001))` |
| 166 | decelerate | `(wipe fade:black-violet-black:(* (sqrt frame) 30))` |
| 167 | bounce | `(wipe fade:black-violet-black:(abs (* (sin (* frame 0.03)) 360)))` |
| 168 | pendulum | `(wipe fade:black-violet-black:(* (sin (* frame 0.02)) 180))` |
| 169 | pulse | `(wipe fade:black-violet-black:(+ frame (* (sin (* frame 0.1)) 30)))` |
| 170 | jitter | `(wipe fade:black-violet-black:(+ frame (* (sin (* frame 2)) 20)))` |
| 171 | reverse | `(wipe fade:black-violet-black:(- 0 frame))` |
| 172 | chaos | `(wipe fade:black-violet-black:(+ frame (* (sin (* frame 0.07)) (cos (* frame 0.13)) 200)))` |
| 173 | meditative | `(wipe fade:black-violet-black:(* (sin (* frame 0.01)) 90))` |
| 174 | exp-sin | `(wipe fade:black-violet-black:(* frame (sin (* frame 0.05))))` |
| 175 | double | `(wipe fade:black-violet-black:(+ frame (* (sin (* frame 0.1)) (sin (* frame 0.03)) 100)))` |

### sunset (black-orange-red-purple-black)

| # | motion | source |
|---|---|---|
| 176 | spin | `(wipe fade:black-orange-red-purple-black:frame)` |
| 177 | fast-spin | `(wipe fade:black-orange-red-purple-black:(* frame 5))` |
| 178 | slow-spin | `(wipe fade:black-orange-red-purple-black:(/ frame 3))` |
| 179 | swing | `(wipe fade:black-orange-red-purple-black:(+ frame (* (sin (* frame 0.05)) 60)))` |
| 180 | wobble | `(wipe fade:black-orange-red-purple-black:(+ frame (* (sin frame) 50)))` |
| 181 | accelerate | `(wipe fade:black-orange-red-purple-black:(* frame frame 0.001))` |
| 182 | decelerate | `(wipe fade:black-orange-red-purple-black:(* (sqrt frame) 30))` |
| 183 | bounce | `(wipe fade:black-orange-red-purple-black:(abs (* (sin (* frame 0.03)) 360)))` |
| 184 | pendulum | `(wipe fade:black-orange-red-purple-black:(* (sin (* frame 0.02)) 180))` |
| 185 | pulse | `(wipe fade:black-orange-red-purple-black:(+ frame (* (sin (* frame 0.1)) 30)))` |
| 186 | jitter | `(wipe fade:black-orange-red-purple-black:(+ frame (* (sin (* frame 2)) 20)))` |
| 187 | reverse | `(wipe fade:black-orange-red-purple-black:(- 0 frame))` |
| 188 | chaos | `(wipe fade:black-orange-red-purple-black:(+ frame (* (sin (* frame 0.07)) (cos (* frame 0.13)) 200)))` |
| 189 | meditative | `(wipe fade:black-orange-red-purple-black:(* (sin (* frame 0.01)) 90))` |
| 190 | exp-sin | `(wipe fade:black-orange-red-purple-black:(* frame (sin (* frame 0.05))))` |
| 191 | double | `(wipe fade:black-orange-red-purple-black:(+ frame (* (sin (* frame 0.1)) (sin (* frame 0.03)) 100)))` |

### ocean (navy-blue-cyan-teal-navy)

| # | motion | source |
|---|---|---|
| 192 | spin | `(wipe fade:navy-blue-cyan-teal-navy:frame)` |
| 193 | fast-spin | `(wipe fade:navy-blue-cyan-teal-navy:(* frame 5))` |
| 194 | slow-spin | `(wipe fade:navy-blue-cyan-teal-navy:(/ frame 3))` |
| 195 | swing | `(wipe fade:navy-blue-cyan-teal-navy:(+ frame (* (sin (* frame 0.05)) 60)))` |
| 196 | wobble | `(wipe fade:navy-blue-cyan-teal-navy:(+ frame (* (sin frame) 50)))` |
| 197 | accelerate | `(wipe fade:navy-blue-cyan-teal-navy:(* frame frame 0.001))` |
| 198 | decelerate | `(wipe fade:navy-blue-cyan-teal-navy:(* (sqrt frame) 30))` |
| 199 | bounce | `(wipe fade:navy-blue-cyan-teal-navy:(abs (* (sin (* frame 0.03)) 360)))` |
| 200 | pendulum | `(wipe fade:navy-blue-cyan-teal-navy:(* (sin (* frame 0.02)) 180))` |
| 201 | pulse | `(wipe fade:navy-blue-cyan-teal-navy:(+ frame (* (sin (* frame 0.1)) 30)))` |
| 202 | jitter | `(wipe fade:navy-blue-cyan-teal-navy:(+ frame (* (sin (* frame 2)) 20)))` |
| 203 | reverse | `(wipe fade:navy-blue-cyan-teal-navy:(- 0 frame))` |
| 204 | chaos | `(wipe fade:navy-blue-cyan-teal-navy:(+ frame (* (sin (* frame 0.07)) (cos (* frame 0.13)) 200)))` |
| 205 | meditative | `(wipe fade:navy-blue-cyan-teal-navy:(* (sin (* frame 0.01)) 90))` |
| 206 | exp-sin | `(wipe fade:navy-blue-cyan-teal-navy:(* frame (sin (* frame 0.05))))` |
| 207 | double | `(wipe fade:navy-blue-cyan-teal-navy:(+ frame (* (sin (* frame 0.1)) (sin (* frame 0.03)) 100)))` |

### fire (black-red-yellow-red-black)

| # | motion | source |
|---|---|---|
| 208 | spin | `(wipe fade:black-red-yellow-red-black:frame)` |
| 209 | fast-spin | `(wipe fade:black-red-yellow-red-black:(* frame 5))` |
| 210 | slow-spin | `(wipe fade:black-red-yellow-red-black:(/ frame 3))` |
| 211 | swing | `(wipe fade:black-red-yellow-red-black:(+ frame (* (sin (* frame 0.05)) 60)))` |
| 212 | wobble | `(wipe fade:black-red-yellow-red-black:(+ frame (* (sin frame) 50)))` |
| 213 | accelerate | `(wipe fade:black-red-yellow-red-black:(* frame frame 0.001))` |
| 214 | decelerate | `(wipe fade:black-red-yellow-red-black:(* (sqrt frame) 30))` |
| 215 | bounce | `(wipe fade:black-red-yellow-red-black:(abs (* (sin (* frame 0.03)) 360)))` |
| 216 | pendulum | `(wipe fade:black-red-yellow-red-black:(* (sin (* frame 0.02)) 180))` |
| 217 | pulse | `(wipe fade:black-red-yellow-red-black:(+ frame (* (sin (* frame 0.1)) 30)))` |
| 218 | jitter | `(wipe fade:black-red-yellow-red-black:(+ frame (* (sin (* frame 2)) 20)))` |
| 219 | reverse | `(wipe fade:black-red-yellow-red-black:(- 0 frame))` |
| 220 | chaos | `(wipe fade:black-red-yellow-red-black:(+ frame (* (sin (* frame 0.07)) (cos (* frame 0.13)) 200)))` |
| 221 | meditative | `(wipe fade:black-red-yellow-red-black:(* (sin (* frame 0.01)) 90))` |
| 222 | exp-sin | `(wipe fade:black-red-yellow-red-black:(* frame (sin (* frame 0.05))))` |
| 223 | double | `(wipe fade:black-red-yellow-red-black:(+ frame (* (sin (* frame 0.1)) (sin (* frame 0.03)) 100)))` |

### ice (white-cyan-blue-indigo-black)

| # | motion | source |
|---|---|---|
| 224 | spin | `(wipe fade:white-cyan-blue-indigo-black:frame)` |
| 225 | fast-spin | `(wipe fade:white-cyan-blue-indigo-black:(* frame 5))` |
| 226 | slow-spin | `(wipe fade:white-cyan-blue-indigo-black:(/ frame 3))` |
| 227 | swing | `(wipe fade:white-cyan-blue-indigo-black:(+ frame (* (sin (* frame 0.05)) 60)))` |
| 228 | wobble | `(wipe fade:white-cyan-blue-indigo-black:(+ frame (* (sin frame) 50)))` |
| 229 | accelerate | `(wipe fade:white-cyan-blue-indigo-black:(* frame frame 0.001))` |
| 230 | decelerate | `(wipe fade:white-cyan-blue-indigo-black:(* (sqrt frame) 30))` |
| 231 | bounce | `(wipe fade:white-cyan-blue-indigo-black:(abs (* (sin (* frame 0.03)) 360)))` |
| 232 | pendulum | `(wipe fade:white-cyan-blue-indigo-black:(* (sin (* frame 0.02)) 180))` |
| 233 | pulse | `(wipe fade:white-cyan-blue-indigo-black:(+ frame (* (sin (* frame 0.1)) 30)))` |
| 234 | jitter | `(wipe fade:white-cyan-blue-indigo-black:(+ frame (* (sin (* frame 2)) 20)))` |
| 235 | reverse | `(wipe fade:white-cyan-blue-indigo-black:(- 0 frame))` |
| 236 | chaos | `(wipe fade:white-cyan-blue-indigo-black:(+ frame (* (sin (* frame 0.07)) (cos (* frame 0.13)) 200)))` |
| 237 | meditative | `(wipe fade:white-cyan-blue-indigo-black:(* (sin (* frame 0.01)) 90))` |
| 238 | exp-sin | `(wipe fade:white-cyan-blue-indigo-black:(* frame (sin (* frame 0.05))))` |
| 239 | double | `(wipe fade:white-cyan-blue-indigo-black:(+ frame (* (sin (* frame 0.1)) (sin (* frame 0.03)) 100)))` |

### vapor (pink-cyan-purple-cyan-pink)

| # | motion | source |
|---|---|---|
| 240 | spin | `(wipe fade:pink-cyan-purple-cyan-pink:frame)` |
| 241 | fast-spin | `(wipe fade:pink-cyan-purple-cyan-pink:(* frame 5))` |
| 242 | slow-spin | `(wipe fade:pink-cyan-purple-cyan-pink:(/ frame 3))` |
| 243 | swing | `(wipe fade:pink-cyan-purple-cyan-pink:(+ frame (* (sin (* frame 0.05)) 60)))` |
| 244 | wobble | `(wipe fade:pink-cyan-purple-cyan-pink:(+ frame (* (sin frame) 50)))` |
| 245 | accelerate | `(wipe fade:pink-cyan-purple-cyan-pink:(* frame frame 0.001))` |
| 246 | decelerate | `(wipe fade:pink-cyan-purple-cyan-pink:(* (sqrt frame) 30))` |
| 247 | bounce | `(wipe fade:pink-cyan-purple-cyan-pink:(abs (* (sin (* frame 0.03)) 360)))` |
| 248 | pendulum | `(wipe fade:pink-cyan-purple-cyan-pink:(* (sin (* frame 0.02)) 180))` |
| 249 | pulse | `(wipe fade:pink-cyan-purple-cyan-pink:(+ frame (* (sin (* frame 0.1)) 30)))` |
| 250 | jitter | `(wipe fade:pink-cyan-purple-cyan-pink:(+ frame (* (sin (* frame 2)) 20)))` |
| 251 | reverse | `(wipe fade:pink-cyan-purple-cyan-pink:(- 0 frame))` |
| 252 | chaos | `(wipe fade:pink-cyan-purple-cyan-pink:(+ frame (* (sin (* frame 0.07)) (cos (* frame 0.13)) 200)))` |
| 253 | meditative | `(wipe fade:pink-cyan-purple-cyan-pink:(* (sin (* frame 0.01)) 90))` |
| 254 | exp-sin | `(wipe fade:pink-cyan-purple-cyan-pink:(* frame (sin (* frame 0.05))))` |
| 255 | double | `(wipe fade:pink-cyan-purple-cyan-pink:(+ frame (* (sin (* frame 0.1)) (sin (* frame 0.03)) 100)))` |
