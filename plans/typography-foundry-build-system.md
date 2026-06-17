# Typography Foundry Build System

A repeatable, in-repo pipeline for turning Aesthetic Computer's homegrown
bitmap/vector typefaces into distributable font files (TTF / OTF / WOFF /
WOFF2), from a single source of truth, with proper OFL licensing.

Status: proposal. Author target: AC foundry tooling.

---

## 0. What we have today (audited)

| Face | Source | Geometry | Glyphs | Notes |
|---|---|---|---|---|
| **font_1** | Per-glyph JSON at `system/public/aesthetic.computer/disks/drawings/font_1/{numbers,uppercase,lowercase,symbols}/<char> - <date>.json` | **Vector line strokes** on a 6×10 grid: `{ "resolution":[6,10], "commands":[{"name":"line","args":[x1,y1,x2,y2]},{"name":"point","args":[x,y]}] }` | ~99 (10 numbers, 28 upper, 29 lower, 32 symbols) | AC's signature face. Monospace (`proportional:false`, 6×10). |
| **MatrixChunky8** | `MatrixChunky8.bdf` (assets server, served via `/api/bdf-glyph`) | 8px proportional bitmap | full | Per-glyph advance widths declared in `fonts.mjs` `advances{}`. |
| **unifont** | `unifont-16.0.03.bdf` | 16×8 bitmap | huge | **GPL — not ours to relicense.** Ship as-is / link, do not re-foundry. |
| **microtype** | 3×5 legacy JSON | tiny bitmap | small | Legacy; out of scope for distribution. |

Key facts that shape the design:

- **The char-map already exists.** `system/public/aesthetic.computer/disks/common/fonts.mjs`
  exports `font_1 = { glyphHeight:10, glyphWidth:6, proportional:false, 0:"numbers/0 - 2021...", ..., "@":"symbols/at - 2022...", ... }`.
  This is the authoritative mapping from a Unicode character to a
  date-stamped glyph file. **We do not need to invent a char-map — we reuse
  this one.** That solves deliverable #5's "date-stamped filenames need a
  char map" concern outright: it's a data file, already maintained, already
  the runtime source of truth.
- **font_1 is centerline strokes, not pixels.** Commands are `line`
  (4 args: x1,y1,x2,y2) and `point` (2 args). The grid origin is top-left,
  y grows downward (canvas convention). The runtime (`type.mjs`) draws these
  strokes directly at 1px weight. So the "native" reading of font_1 is a
  1-unit-wide stroked path, *not* a filled bitmap.
- **Existing web fonts live at** `system/public/type/webfonts/` (currently
  Berkeley Mono + YWFT Processing, each shipped as `.woff/.woff2` plus a
  `.css` `@font-face`). This is exactly where our generated AC faces should
  land, with matching CSS.
- **Runtime BDF path:** `type.mjs` fetches MatrixChunky8/unifont glyphs from
  `/api/bdf-glyph` (`system/netlify/functions/`) and caches them in
  IndexedDB. The BDF is the runtime source of truth for those faces.

---

## 1. Pipeline options compared

### 1a. font_1 line-commands → outline TTF/OTF

The central design question: **font_1 strokes have no width.** A TTF/OTF
glyph is a *filled* closed outline. So we must turn 1D centerlines into 2D
filled shapes. Two interpretations:

**Interpretation A — Expand-stroke (vector, smooth).**
Treat each `line` as a centerline and give it a round/square nib of width
*w* (e.g. 1 grid unit). The result is a clean filled outline that scales
crisply — a true vector face. `point` becomes a filled dot (a degenerate
stroke / small square). This honors font_1's *actual* nature (it was drawn
as strokes).

**Interpretation B — Pixel interpretation (blocky, bitmap-faithful).**
Rasterize the strokes onto the 6×10 grid (each covered cell = one filled
square), then emit one square contour per "on" pixel. This produces a
pixel-font look that matches how font_1 renders on screen at 1× today
(blocky 1px strokes). Closer to the on-screen runtime appearance, but loses
the "it's actually a vector" property and bloats contour count.

**Recommendation for font_1: Interpretation A (expand-stroke).** font_1 is
genuinely a stroked face; expanding strokes gives a scalable face that looks
right at display sizes and prints. Offer a **`--pixel` build variant** later
if a blocky cut is wanted. Square caps + miter joins on a 1-unit nib will
reproduce the on-grid look at integer sizes while still scaling cleanly.

Tool fit for 1a:

| Tool | Stroke→outline | Make font from scratch | TTF/OTF out | Verdict for font_1 |
|---|---|---|---|---|
| **FontForge (Python)** | **Yes** — `glyph.stroke("circular", width, ...)` / `"square"` cap, miter join; converts centerlines to filled outlines, with `removeoverlap()` to merge. | Yes — `fontforge.font()`, `createChar()`, `glyphPen()`. | `font.generate("X.ttf"/"X.otf")` | **Best fit.** Does the whole job in one dependency: draw centerlines with the glyph pen, `stroke()` them, `removeOverlap()`, set metadata, generate. ([stroke docs](https://fontforge.org/docs/techref/stroke.html), [python api](https://fontforge.org/docs/scripting/python/fontforge.html)) |
| **opentype.js** | No stroke-expand. We'd hand-roll offsetting (compute parallel edges + joins + caps) ourselves. | Yes — `new opentype.Glyph({path})`, `new opentype.Font(...)`, `font.download()`. | TTF (glyf/CFF). | Pure-JS, no Python/Java. But we'd own the geometry math for stroking, which is the hard part. Good fallback if we want zero native deps. ([README](https://github.com/opentypejs/opentype.js/blob/master/README.md)) |
| **fonttools + ufo2ft** | No stroke-expand (it compiles, doesn't draw). | We author UFO `.glif` outlines, then compile. | `compileOTF`/`compileTTF`. | Excellent *compiler*, not a *drawer*. Pair it with something that expands strokes (FontForge, or our own offsetter writing UFO). ([ufo2ft](https://github.com/googlefonts/ufo2ft)) |
| **harfbuzz** | No. It's a *shaping/layout* engine (runtime). | No. | No (it consumes fonts). | Not a build tool. Irrelevant to authoring; only matters if we cared about shaping our output. |

So for font_1 the realistic choices are **FontForge** (built-in stroke
expand, one tool) or **opentype.js + our own offsetter** (no native deps,
more code). FontForge wins on correctness/effort.

### 1b. BDF bitmap (MatrixChunky8) → TTF/OTF

Here the source *is* pixels, so we want each "on" pixel → a unit square,
merged into outlines, with metrics matching the pixel grid.

| Tool | Fit |
|---|---|
| **bits'n'picas** | **Best fit.** Purpose-built bitmap→font tool. CLI: `java -jar BitsNPicas.jar convertbitmap -f ttf -o MatrixChunky8.ttf MatrixChunky8.bdf`. Reads BDF directly, preserves proportional DWIDTH advances, can emit TTF/OTF/OTB (bitmap-in-OpenType). Pixel-perfect at the design ppem. Java dependency. ([repo/README](https://github.com/kreativekorp/bitsnpicas/blob/master/README.md)) |
| **FontForge** | Also capable — import BDF as a bitmap strike, autotrace/`import` then generate; or keep the bitmap strike embedded. More fiddly than bits'n'picas for pure bitmap→outline. Useful if we already have FontForge in the toolchain (we will, for font_1). |
| **bdf2ttf (koron) / pyftsubset / fonttools** | `bdf2ttf` embeds bitmaps as-is (no outline) — niche. fonttools won't trace a BDF by itself. |
| **opentype.js** | Could do it (one square contour per pixel) but we'd re-implement BDF parsing + pixel merge. |

**Recommendation for 1b:** Since MatrixChunky8's runtime truth is the BDF and
we want a pixel-perfect outline cut, **bits'n'picas `convertbitmap`** is the
cleanest. If we want to avoid a second runtime (Java) and already have
FontForge, FontForge's BDF import → generate is acceptable.

### Format conversion (shared, both faces)

TTF/OTF → WOFF/WOFF2 is the same regardless of source:

- **WOFF2 (and WOFF):** `fonttools` —
  `fonttools ttLib.woff2 compress X.ttf -o X.woff2`, or in Python
  `f = TTFont('X.ttf'); f.flavor='woff2'; f.save('X.woff2')`. Requires the
  `brotli` package. WOFF uses `flavor='woff'`.
  ([woff2 docs](https://fonttools.readthedocs.io/en/stable/ttLib/woff2.html))
- This is identical to how Google's gftools ships web fonts; fonttools is the
  industry-standard converter and is already a tiny, pip-installable dep.

---

## 2. Recommended pipeline

**One source of truth per face → one intermediate (UFO) → all formats.**

```
                         (char-map: fonts.mjs font_1 / MatrixChunky8 advances)
                                          │
   font_1 JSON strokes ──┐                │
                         ├──> build-ufo ──┴──> AC<Face>.ufo ──> ufo2ft ──> .ttf + .otf ──> fonttools ──> .woff + .woff2 + .css
   MatrixChunky8.bdf  ───┘   (per face)                                                   (flavor)
```

### Why UFO as the standardized intermediate

- UFO is the de-facto open source-format; `ufo2ft.compileTTF` /
  `compileOTF` produce both flavors from one UFO, and `fonttools` then makes
  WOFF/WOFF2. ([ufo2ft](https://github.com/googlefonts/ufo2ft),
  [ufoLib2](https://github.com/fonttools/ufoLib2))
- It is plain text on disk (`.glif` XML + plists) — diffable, reviewable,
  carries metadata (`fontinfo.plist`: family, copyright, license URL,
  ascender/descender, unitsPerEm) and `features.fea` (kerning/spacing).
- It decouples *drawing* from *compiling*: we can swap the front end
  (FontForge stroke-expand, our own offsetter, or bits'n'picas output) while
  the compile/convert tail stays constant.

### Build steps

**Stage A — Source → UFO (the only face-specific code).**

- **font_1** (`build/font_1_to_ufo.mjs` or `.py`): read `fonts.mjs` `font_1`
  map → for each `char → "subdir/<name> - <date>"`, load the JSON, and for
  each `line`/`point` command emit a contour. Two front-end strategies:
  1. **FontForge route (recommended):** in a FontForge Python script, for
     each glyph open `glyph.glyphPen()`, `moveTo`/`lineTo` the centerlines as
     open contours, call `glyph.stroke("circular", W, cap="square", join="miter")`
     (or `"square"` nib) to fill them, `glyph.removeOverlap()`, set
     `glyph.width` (monospace advance = 6 units scaled). Coordinate transform:
     grid (0..6, 0..10, y-down) → font units (em=1000 or 600), **flip Y**
     (font y-up), place baseline so the 10-unit cell sits with descender
     room. Then `font.generate("AC_Font1-Regular.ttf")` and `.otf`. FontForge
     can also write UFO (`font.save(... .ufo)`) if we want the UFO artifact.
  2. **Pure-JS route (no native deps):** our own stroke offsetter emits UFO
     `.glif` via `ufoLib`/hand-written XML, then `ufo2ft`. More code; keep as
     fallback.
- **MatrixChunky8** (`build/bdf_to_ufo`): run `bits'n'picas convertbitmap`
  straight to `.ttf`/`.otf` (it doesn't need the UFO hop), **or** trace the
  BDF into a UFO for a uniform tail. Pixel squares, advances from BDF DWIDTH
  (already mirrored in `fonts.mjs` `advances{}` for cross-checking).

**Stage B — UFO → TTF + OTF.**
`compileTTF(ufo).save("X.ttf")` and `compileOTF(ufo).save("X.otf")`
(`ufo2ft`). (If using FontForge `generate()` or bits'n'picas directly,
TTF/OTF come straight out and Stage B is a no-op for that face.)

**Stage C — TTF → WOFF + WOFF2.**
For each `.ttf`: `TTFont(ttf); f.flavor='woff2'; f.save('.woff2')` and again
with `'woff'`. (`fonttools` + `brotli`.)

**Stage D — Emit `@font-face` CSS** matching the existing
`system/public/type/webfonts/*.css` convention (one `.css` per face listing
woff2 then woff `src`).

### Metadata / license / reserved font name (set in Stage A → carried through)

Set on the UFO `fontinfo.plist` (or FontForge `font.*` fields), which become
the OpenType `name` table:

- `familyName`: `AC Font One` ; `styleName`: `Regular`
- `copyright`: `Copyright YEAR Aesthetic Computer / Jeffrey Scudder. ...`
- `openTypeNameLicense`: full OFL text (or short notice + URL)
- `openTypeNameLicenseURL`: `https://openfontlicense.org/`
- `openTypeNameDesigner`, `openTypeNameManufacturer`: AC
- `unitsPerEm`: 1000 (vector) ; `ascender`/`descender`/`capHeight`/`xHeight`
  derived from the 6×10 grid
- **Reserved Font Name** declared in the bundled `OFL.txt` header (see §4).

---

## 3. Repo integration

### Location

Create a top-level **`foundry/`** tooldir (matches CLAUDE.md's "foundry
tooldir" hint and keeps build tooling out of the runtime tree):

```
foundry/
  README.md
  package.json            # local deps if any JS front-end is used
  requirements.txt        # fonttools, ufo2ft, ufoLib2, brotli  (pip)
  bin/BitsNPicas.jar      # vendored, or fetched on first run
  sources/
    font_1/               # symlink/notes pointing at the canonical
                          #   disks/drawings/font_1 + fonts.mjs map
    MatrixChunky8.bdf     # canonical copy (or fetched from assets)
  build/
    font_1_to_ufo.py      # FontForge script (stroke-expand)
    bdf_to_font.sh        # bits'n'picas convertbitmap wrapper
    compile.py            # ufo2ft compileTTF/compileOTF
    webfonts.py           # fonttools woff/woff2 + @font-face CSS
  dist/                   # build outputs (gitignored)
  metadata/
    OFL.txt
    FONTLOG.txt
```

### Single source of truth (no drift)

- **The char-map is `disks/common/fonts.mjs` (`font_1` object) — period.**
  The build script *imports/parses that file* to discover glyph→file
  mappings. It must never hard-code a second copy. Adding a glyph = add the
  JSON + one line in `fonts.mjs`, and both the runtime *and* the next font
  build pick it up. This is the mechanism that keeps the web font and the
  in-runtime bitmap in sync from one source.
- **MatrixChunky8's truth is the BDF** (runtime fetches via `/api/bdf-glyph`).
  The build consumes the same BDF, so the distributed font and the runtime
  glyphs come from one file. `fonts.mjs` `advances{}` is a cross-check.

### Outputs slot into existing web-font dir

Stage D writes to **`system/public/type/webfonts/`** alongside Berkeley Mono /
YWFT, e.g.:

```
ac-font-one.css
AcFontOne-Regular.woff2
AcFontOne-Regular.woff
AcFontOne-Regular.ttf      # optional: keep ttf/otf for download/dist
AcFontOne-Regular.otf
matrix-chunky8.css
MatrixChunky8-Regular.woff2 / .woff / .ttf
```

Runtime `type.mjs` is **unchanged** — it keeps rendering from JSON/BDF as it
does now. The web fonts are a *distribution artifact* of the same sources, so
on-canvas rendering and the downloadable/CSS font cannot diverge.

### npm target

Add to root `package.json`:

```json
"scripts": {
  "fonts:build": "python foundry/build/run_all.py",
  "fonts:build:font1": "fontforge -script foundry/build/font_1_to_ufo.py && python foundry/build/compile.py font_1 && python foundry/build/webfonts.py font_1",
  "fonts:build:matrix": "bash foundry/build/bdf_to_font.sh && python foundry/build/webfonts.py matrix"
}
```

`run_all.py` orchestrates A→D for every face and copies finals into
`system/public/type/webfonts/`. Tooling deps install via
`pip install -r foundry/requirements.txt` (fonttools, ufo2ft, ufoLib2,
brotli) plus system `fontforge` and a JRE for bits'n'picas — documented in
`foundry/README.md`. A doctor check (`toolchain/doctor.mjs` CHECKS array) can
verify `fontforge`, `python -c "import ufo2ft, brotli"`, and the jar are
present.

---

## 4. Licensing AC's own faces (OFL)

Release **font_1** and **MatrixChunky8** under **SIL Open Font License 1.1**
with a Reserved Font Name. (unifont stays GPL — leave it alone; microtype out
of scope.)

What that needs (per the OFL FAQ / openfontlicense.org):

1. **`OFL.txt`** — the verbatim license text with our header filled in:
   ```
   Copyright (c) 2026 Aesthetic Computer (https://aesthetic.computer),
   with Reserved Font Name "AC Font One".
   Copyright (c) 2026 Aesthetic Computer,
   with Reserved Font Name "Matrix Chunky 8".
   ```
   The Reserved Font Name(s) go after the copyright line(s) at the top of
   `OFL.txt`. ([RFN guidance](https://openfontlicense.org/ofl-reserved-font-names/),
   [OFL-FAQ](https://openfontlicense.org/ofl-faq/))
2. **`FONTLOG.txt`** — recommended (not required): font description, change
   log, acknowledgements (Jeffrey Scudder / contributors). ([best practices](https://silnrsi.github.io/FDBP/en-US/Copyright_and_Licensing.html))
3. **In-file metadata** (carried via UFO `fontinfo` → `name` table, §2): set
   `openTypeNameLicense` to the OFL notice, `openTypeNameLicenseURL` to
   `https://openfontlicense.org/`, and `copyright` with the RFN statement.
4. **Reserved Font Name discipline:** the *menu/family* name shipped must be
   the RFN (e.g. "AC Font One"); any third-party modified derivative must
   rename. Pick public-facing RFNs distinct from the internal codenames
   (`font_1` is a fine *internal* id; "AC Font One" or a chosen brand name is
   the *public* RFN).

Bundle `OFL.txt` + `FONTLOG.txt` in `foundry/metadata/` and ship copies in
any download package next to the `.ttf/.otf`.

---

## 5. Risks, gotchas, effort

**Stroke weight & look (font_1).** Choosing nib width *W* and cap/join
defines the entire personality. Square cap + miter join at W=1 grid unit
reproduces the on-screen blocky strokes while scaling cleanly; round nib
softens it. Needs a visual review pass and probably a tunable constant.
`point` commands must become real filled dots (small square/circle), not
zero-area paths. `removeOverlap()` is mandatory after stroking or overlapping
strokes leave holes/winding errors.

**Coordinate / baseline mapping.** Source grid is y-down, top-left origin;
fonts are y-up with a baseline. Get the flip + baseline placement right or
glyphs sit too high/low and descenders (g, j, p, q, y) clip. The 6×10 cell
needs a chosen baseline row and descender allowance; verify against the
runtime's `baseline`/`y`-offset hints in `fonts.mjs`.

**Hinting.** Bitmap-derived (MatrixChunky8) outlines are pixel-aligned by
construction and only look right at the design ppem (and integer multiples);
off-size rendering blurs. Options: ship OTB/embedded-bitmap for crispness at
target sizes, or accept that the web-font cut is for scalable display use and
keep the runtime BDF path for pixel-exact small text. font_1 vector outlines
need at minimum autohinting (`fontforge` autoHint, or `ttfautohint` as a
later step) for clean small rendering; full manual TT hinting is out of
scope. opentype.js cannot emit TT hinting at all
([opentype.js #380](https://github.com/opentypejs/opentype.js/issues/380)).

**Kerning / spacing.** font_1 is monospace (advance = 6 units) — trivial, no
kerning needed initially. MatrixChunky8 is proportional; advances come from
BDF DWIDTH (and mirrored `fonts.mjs` `advances{}`). Optional kerning pairs
later via a `features.fea` in the UFO.

**Date-stamped filenames → char-map.** **Already solved**: the
`fonts.mjs` `font_1` object is the authoritative char→file map and the build
reads it directly. The only maintenance rule: every new glyph must be
registered there (it already must be, for the runtime). No separate
char-map to invent or keep in sync.

**Coverage gaps.** font_1 has ~99 glyphs; it lacks space metrics, many
accented/non-Latin chars (runtime falls back to BDF `6x10`). The distributed
font will only cover what's in the map — set a sensible `.notdef` and a
space advance, and document the limited charset.

**Toolchain deps.** Adds Python (fonttools/ufo2ft/ufoLib2/brotli), system
`fontforge`, and a JRE (bits'n'picas). All free/open, all scriptable
headless. Document + doctor-check them.

### Effort estimate (rough)

| Task | Effort |
|---|---|
| `foundry/` scaffold, deps, README, doctor check | 0.5 day |
| font_1 → UFO/TTF FontForge stroke-expand script (coord flip, baseline, stroke tuning, removeOverlap, monospace metrics) + visual review | 1.5–2.5 days |
| MatrixChunky8 via bits'n'picas + advance verification | 0.5 day |
| ufo2ft compile + fonttools woff/woff2 + CSS emit + npm targets | 0.5 day |
| OFL.txt / FONTLOG / name-table metadata wiring | 0.25 day |
| Buffer: hinting/autohint pass, on-canvas vs web-font visual parity check | 0.5–1 day |
| **Total** | **~3.5–5 days** for both faces, fully repeatable. |

---

## Sources

- FontForge Python API — https://fontforge.org/docs/scripting/python/fontforge.html
- FontForge Expand Stroke facility — https://fontforge.org/docs/techref/stroke.html
- ufo2ft (UFO → TTF/OTF) — https://github.com/googlefonts/ufo2ft
- ufoLib2 — https://github.com/fonttools/ufoLib2
- fonttools WOFF2 — https://fonttools.readthedocs.io/en/stable/ttLib/woff2.html
- bits'n'picas (bitmap→font) — https://github.com/kreativekorp/bitsnpicas/blob/master/README.md
- opentype.js — https://github.com/opentypejs/opentype.js/blob/master/README.md ; hinting limitation https://github.com/opentypejs/opentype.js/issues/380
- SIL OFL FAQ — https://openfontlicense.org/ofl-faq/
- Reserved Font Names — https://openfontlicense.org/ofl-reserved-font-names/
- Font Dev Best Practices (licensing) — https://silnrsi.github.io/FDBP/en-US/Copyright_and_Licensing.html
