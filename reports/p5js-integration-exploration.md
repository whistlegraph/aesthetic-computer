# Adding p5.js Support to Aesthetic Computer

*Exploration — not a decision. 2026-05-27.*

## What's being asked

Today an AC piece is a `.mjs` (ES module exporting `boot`/`paint`/`act`/`sim`) or a `.lisp` (KidLisp source). The runtime — `disk.mjs` — lives inside a Web Worker and talks to the main thread (`bios.mjs`) through a pixel-buffer protocol. The proposal is: support a third extension, `.js`, that **defaults to a p5.js sketch** rather than the native AC API.

The goal is **not** to wrap AC pieces in p5 — `.mjs` stays the canonical native form. The goal is to make AC a comfortable host for the thousands of existing p5 sketches floating around (classroom code, Coding Train clones, OpenProcessing exports), and to let people fluent in p5 publish without learning the AC API first.

## The hard question first: can p5.js run in a Worker?

Short answer: **not out of the box, but yes with a constrained shim.**

p5.js was written for the main thread. The default sketch lifecycle calls `createCanvas()`, which creates an HTML `<canvas>` and attaches it to the DOM. It reads `window.innerWidth`, hooks DOM events (`mousePressed`, `keyTyped`), and exposes DOM helper APIs (`createDiv`, `createSlider`, `loadJSON` via XHR fallback). None of that exists in a Worker.

What *does* work:

- **2D rendering via `OffscreenCanvas`** — Chrome/Edge/Firefox/Safari 16.4+ all support `OffscreenCanvas.getContext("2d")` inside Workers. Most of p5's `p5.Renderer2D` calls map directly onto a 2D context.
- **WebGL via `OffscreenCanvas`** — same story; `p5.RendererGL` is a thinner shim and would port more easily than people expect.
- **Instance mode** (`new p5(sketch, container)`) — instance mode already isolates p5 from globals, which is the foothold needed to swap out its DOM assumptions.

What does **not** work in a Worker without surgery:

- DOM helpers (`createDiv`, `createButton`, `createSlider`) — these have no analog and should throw clear "unsupported in AC" errors rather than silently fail.
- Direct event hooks (`mousePressed` etc.) — must be marshalled from the main thread; AC already does this for native pieces, so the pattern exists.
- `loadFont` via FontFace (Worker FontFace support is partial) — AC's `Typeface` system can stand in.
- `preload()`-style XHR — must be re-routed through `fetch` (p5 already uses `fetch` internally on modern versions, but third-party addons assume XHR).

The realistic verdict: a **patched p5** (call it `p5-ac.js`) that monkey-patches `createCanvas` to use the worker's OffscreenCanvas, stubs out DOM helpers with clear errors, and forwards events from main-thread postMessage is feasible. Estimate: ~500–800 lines of shim plus a vendored copy of p5 with maybe a dozen call sites patched.

## Four architectural options

### Option A — Main-thread bridge

p5 runs on the main thread (in `bios.mjs`'s context), AC's worker stays untouched. A `.js` piece posts state across the worker boundary in both directions: AC events in, frame-completion signals out. p5 owns its own canvas, drawn underneath or alongside AC's compositor.

- **Pros:** Maximum p5 compatibility — every addon, every classroom example works. Smallest surgery to AC.
- **Cons:** Breaks AC's "everything in a worker" invariant. Two canvases to composite. Main-thread jank affects AC chrome. Sandboxing/restricted-API story gets harder. Recording/preview tooling needs a second source.

### Option B — Worker mode with patched p5 + OffscreenCanvas (recommended foundation)

Vendor p5 as `lib/p5-ac.mjs`. Patch the renderer constructor to accept an OffscreenCanvas from `disk.mjs`. Stub or redirect DOM helpers. Forward DOM events from `bios.mjs` (which AC already does for native pieces). Map p5's frame ticks onto AC's `paint`/`sim` calls.

- **Pros:** Preserves AC's worker model. One compositor. Recording/preview/QR-share all work because pixels still come out of disk.mjs. Restricted-API story unchanged.
- **Cons:** ~10–20% of p5 sketches use unsupported DOM helpers and will fail. Maintenance: pulling future p5 updates means re-applying patches (mitigated by keeping the patch set tiny + a CI test that builds the patched bundle).

### Option C — API translation layer (no p5 inside AC)

Detect that a piece imports/uses p5-style globals (`ellipse`, `rect`, `noStroke`, `pmouseX`) and re-implement those names against the AC API. No p5 code shipped.

- **Pros:** No vendor maintenance. Smallest bundle.
- **Cons:** You will spend years chasing edge cases. p5 sketches rely on specific easing of e.g. `colorMode(HSB, 360, 100, 100)` semantics, `push()`/`pop()` matrix stack details, `RandomGaussian()` distribution. A "p5-ish" runtime that quietly disagrees with real p5 is a worse experience than no p5 at all.

### Option D — iframe-embedded sketches

`.js` pieces load into a sandboxed iframe with real p5. AC chrome wraps the iframe.

- **Pros:** Trivial to implement. Maximum compatibility.
- **Cons:** Two coordinate systems, two input pipelines, broken recording/preview, broken HUD overlays, broken `jump`/`leave` plumbing. Defeats most of what AC's runtime gives you. Acceptable as a **fallback for sketches that break under Option B**, not a primary mode.

## Recommended path

**Option B with Option D as a graceful fallback.**

Ship `p5-ac.mjs` as the worker-hosted patched build. If a piece throws on the unsupported DOM helpers, transparently re-host it in a sandboxed iframe (Option D) with a one-line HUD note: *"this piece uses DOM helpers — running in compatibility mode."* That keeps the front door clean and the back door open.

## Touchpoints in the stack

A `.js` piece needs to be a first-class citizen across the boot pipeline. The places it changes:

### 1. Piece resolution — `lib/disk.mjs`

Around line 3046 there's already a "try `.mjs`, then `.lua`, then `.lisp`" cascade. Add `.js` between `.mjs` and `.lua`. The dispatcher then needs a new branch: if the loaded source is `.js`, instantiate the p5-ac runner instead of `import()`-ing it as an ES module. (We can still `import()` it as a module — p5 sketches that use top-level `function setup() {}` will need a thin wrapper that captures those globals into an instance-mode closure.)

### 2. KidLisp-style detection helpers — `lib/disk.mjs` ~2100, ~2339

`endsWith('.lisp')` checks appear in several places to gate HUD behaviour, kidlisp-specific overlays, and source-share. Mirror those for `.js` where it makes sense: a `isP5Piece(path)` helper alongside `isKidlispSource`.

### 3. Runtime adapter — new file `lib/p5-ac.mjs`

Vendored p5 + patch set. Exports a single `runP5Piece(source, api)` that:

- Builds an `OffscreenCanvas` sized to AC's screen buffer.
- Constructs `new p5(sketchFn, fakeContainer)` where `fakeContainer` is a stub object.
- Maps `setup` → AC `boot`, `draw` → AC `paint`, mouse/key events → AC `act`.
- Returns the OffscreenCanvas' pixel buffer to AC's compositor each frame via `transferToImageBitmap()` (zero-copy on Chrome).

### 4. Worker bootstrap — `bios.mjs` ~3914

The worker spawn does not change; what changes is the *module* the worker loads when the piece is `.js`. Today disk.mjs is the worker entry; it stays the entry. p5-ac just becomes another module disk.mjs lazy-imports.

### 5. Event forwarding — `bios.mjs`

AC already forwards pointer/keyboard events to the worker. p5-ac translates them into p5's expected event shape (`mouseX`, `mouseY`, `pmouseX`, `pmouseY`, `mouseIsPressed`, `keyCode`, `key`). No new wire — only a new translation function inside the worker.

### 6. Source viewer / `source` command — `lib/parse.mjs` + prompt

`source piece` and the `download blank template` flow assume `.mjs`. Add `.js` to the dispatch — and add a `blank.js` template that's a minimal p5 sketch with AC-compatible imports.

### 7. Publishing — `system/netlify/functions/publish*.mjs`

Whatever path validates extensions when users `publish` needs `.js` allowed alongside `.mjs` and `.lisp`. MIME-type and Content-Disposition for served pieces should be `application/javascript` (it already is for `.mjs` — same headers apply).

### 8. URL routing / lith Express

Currently `aesthetic.computer/foo` looks for `foo.mjs` then `foo.lisp` in the disks directory and on user S3 paths. Extend to also probe `foo.js`. Be careful with ordering: `.mjs` wins ties so existing pieces don't get shadowed.

### 9. Module loader / hot reload — `module-loader.mjs`

The WebSocket loader watches `disks/*.mjs` and `disks/*.lisp`. Add the `.js` glob. The reload path is identical — disk.mjs throws away the old piece and re-invokes the new source.

### 10. Recording / preview / QR — `lib/frame-capture.mjs`, preview pipelines

Because p5-ac draws into an OffscreenCanvas that AC then composites, recording works for free. No changes here. *This is the main reason Option B beats Option A.*

### 11. Restricted-API / shop pieces — `lib/restricted-api.mjs`

If a `.js` piece runs under restrictions (paid/shop pieces), we need to decide whether the p5 globals leak past the restriction wall. Probably yes — the restriction is about AC's `net`, `store`, etc., not about drawing primitives. Audit the existing restricted-API matrix and add `.js` rows.

### 12. `npm run new` — template generator

`scripts/new.mjs` (or wherever the blank-template logic lives) defaults to `.mjs`. Add a `--p5` flag (or a separate `npm run new-p5`) that emits a `blank.js`.

### 13. Documentation

- `CLAUDE.md` — piece-structure section gains a "p5 mode" subsection.
- `system/public/aesthetic.computer/disks/help/` — add p5 quickstart.
- A worked example piece: `disks/p5-example.js`, a 30-line bouncing ball with comments showing what's supported and what isn't.

## File-extension semantics

| Extension | Source type     | Runtime                          | Default API style              |
| --------- | --------------- | -------------------------------- | ------------------------------ |
| `.mjs`    | ES module       | disk.mjs native                  | AC native (`boot`/`paint`/...) |
| `.lisp`   | KidLisp source  | kidlisp.mjs evaluator            | KidLisp s-exprs                |
| `.js`     | Plain JS or ESM | p5-ac.mjs (OffscreenCanvas)      | p5 globals + AC interop hooks  |

Loading a `.js` piece always boots p5-ac, even if the source doesn't use p5 functions. That's the contract — "if you want native AC, write `.mjs`." Avoid clever auto-detection (Option C's failure mode).

## AC ↔ p5 interop

A p5-ac piece should still get an `api` object — the same one `.mjs` pieces receive — so it can use `jump()`, `net.pieces()`, `store`, `sound`, etc. Two ways to expose it:

- **`window.ac`** equivalent: inject a global `ac` inside the sketch closure, so `ac.jump("painting")` works alongside p5 calls.
- **Second arg to sketchFn**: `new p5((p, ac) => { p.setup = () => { ac.jump("painting"); }; })` — cleaner but non-idiomatic for people coming from OpenProcessing.

Probably do both — global `ac` for ease, second arg for purists.

## Worker constraints worth noting

- `OffscreenCanvas.getContext("webgl2")` is fine in workers, but **antialiasing context-attribute is honored inconsistently across browsers**; expect to manually handle smoothing.
- Worker fonts: `FontFace` is in workers as of 2023 in Chrome/Firefox/Safari, but `document.fonts` is not. p5's `loadFont` ultimately uses opentype.js for the glyph data — fine in a worker — but the font registration path is what breaks. Route through AC's `Typeface`.
- `console.log` from a worker reaches DevTools but not `bios.mjs`'s console UI; p5's helpful warnings will need to be piped through AC's existing log relay.
- `Math.random()` is fine, `crypto.getRandomValues` is fine; `Date.now()` is fine. Nothing exotic in p5 needs main-thread context.

## Managing the dependency

p5.js releases roughly quarterly. Strategy:

- **Pin a version** (e.g. p5 v1.10.0) and vendor the patched copy at `lib/dep/p5/p5-ac.mjs`.
- Keep the patch set in `lib/dep/p5/patches/*.patch` — small, auditable diffs against upstream.
- A `bin/update-p5.mjs` script downloads new upstream, re-applies patches, runs a smoke-test suite (10–20 canonical sketches: bouncing ball, particle system, 3D box, agar, flocking, perlin landscape, video filter, etc.) and reports breakages.
- Don't auto-track upstream. p5 sometimes ships breaking changes minor-version; we control the cadence.

## Open questions

- **WebGL 1 vs 2** — p5's `WEBGL` mode targets WebGL1 by default. Worth deciding whether p5-ac silently upgrades to WebGL2 (better, but breaks some shaders) or matches p5 exactly.
- **Sound** — p5.sound is a separate addon and is *heavily* DOM/AudioContext-coupled. Probably scope it out for v1 and route sketches to AC's `sound` API for audio. Document this clearly.
- **`loadImage` / `loadJSON`** — these are async and p5 has `preload()` semantics that block `setup()`. AC has no preload phase. We'll need to either implement a tiny preload scheduler in p5-ac, or document that preload moves into an async `boot`.
- **Editor integration** — does the in-browser editor (`prompts`) understand `.js` syntax-highlighting and autocomplete for p5 globals? Decent default: ship a curated `p5-globals.d.ts`-style list for the autocomplete layer.
- **Naming collisions** — a sketch named `noise.js` already shadows the p5 `noise()` function in some loaders. Worth a lint warning.
- **Versioning the runtime** — pieces from 2027 may rely on patched-p5 v1, pieces from 2029 on v2. Keep the loader checking a per-piece pragma (`// @p5-ac v1`) so old sketches stay reproducible.

## Suggested rollout

1. **Spike** — vendor p5 v1.10.0 into a sandbox; get one OffscreenCanvas-backed sketch (rotating cube + mouse follow) running in the worker, fully isolated from disk.mjs. ~1 day.
2. **Wire** — integrate `p5-ac.mjs` into disk.mjs's piece dispatcher behind a `.js` extension. ~2 days.
3. **Event marshalling + AC interop** — pointer/key events in, `ac.jump` out. ~1 day.
4. **Compat fallback (Option D)** — sandboxed iframe path for failing sketches. ~2 days.
5. **Template + `npm run new-p5`** — ~half day.
6. **Smoke-test corpus + CI** — 15-sketch suite + screenshot diffing. ~2 days.
7. **Docs + announce** — README section, blog/post, port a Coding Train classic as the demo piece.

Realistically: **8–10 focused days** to a v1 that handles most sketches well and degrades gracefully on the rest.

## Bottom line

This is *additive*, not invasive. AC's native runtime stays canonical; p5 mode is an alternative front door. The worker model is preserved (Option B), recording/preview/share work for free, and the maintenance surface is small (one vendored library + a thin shim). The biggest risk is the long tail of p5 addons and DOM-helper sketches — handle those with a transparent iframe fallback rather than trying to be 100% compatible inside the worker.

If/when this ships, the side effect is that AC becomes the easiest place on the web to publish a p5 sketch with a real URL, real preview, real recording, and a real social layer attached — without giving up the AC piece model.
