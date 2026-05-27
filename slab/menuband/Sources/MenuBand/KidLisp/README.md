# KidLisp (Swift port, Menuband)

Native Swift implementation of KidLisp. Linked into the MenuBand executable
but designed to be independent of AppKit (Foundation only) so it can render
headlessly from the CLI for conformance testing.

**Decree conformance:** `KidLisp Decree '26: Core` — phase 1 corpus only.
See `kidlisp/SCORE.md` for the full runtime registry.

## Files

| File | Role |
|---|---|
| `KLColor.swift` | Pixel color + 147-entry CSS color table |
| `KLLexer.swift` | Tokenizer (paren/comma/newline/ident/number) |
| `KLExpr.swift` | AST: number, symbol, list, bareForm, atom, program |
| `KLParser.swift` | Tokens → AST. Top-level statements separate on `,` and `\n` |
| `KLRNG.swift` | Seedable PCG32 for deterministic conformance frames |
| `KLFramebuffer.swift` | CPU RGBA8 buffer + `wipe`/`line` (Bresenham)/`blur` (separable box) + PPM writer |
| `KLEvaluator.swift` | Tree-walker. Heads: `wipe`, `ink`, `line`, `blur` |
| `KLRuntime.swift` | Top-level facade: `KLRuntime.render(source:width:height:)` |
| `KLCLI.swift` | Headless `--kidlisp-render` flag handler |

## Headless renderer

```sh
swift build
./.build/debug/MenuBand --kidlisp-render '<source>' <width> <height> <out.ppm> [seed]
```

Examples (phase 1 corpus):

```sh
./.build/debug/MenuBand --kidlisp-render 'blue' 256 256 /tmp/4xa.ppm
./.build/debug/MenuBand --kidlisp-render '(wipe blue)' 256 256 /tmp/wib.ppm
./.build/debug/MenuBand --kidlisp-render 'purple, ink, line, blur 5' 256 256 /tmp/bop.ppm
```

## Status

| Phase | Corpus | Heads | Status |
|---|---|---|---|
| 1 | `$bop` · `$wib` · `$4xa` | wipe, ink, line, blur, bare-color-wipe | ✅ done |
| 2 | `$pie` · `$39i` | `(fps N)`, timing tokens, magic vars (w/h/frame), scroll, flood, circle, zoom, contrast | not started |
| 3 | `$roz` · `$ceo` | `fade:` gradients, `:frame` animation, spin, coat, `?`/`...` | claims `RBP-26` when done |
| 4 | `$cow` | `$code` embed (recursive eval + source fetch) | not started |
| 5 | `$4bb` | `bake`/`burn` offscreen pages | not started |

When phase 3 lands, we can publish `KidLisp Decree '26: Core + Render + RBP-26`.

## Not yet ported from kidlisp.mjs

- Audio API (`amp`, `mic`, `melody`, `overtone`) — Decree Audio level
- 3D primitives (`cube`, `form`, `trans`)
- Text rendering (`write`, font tables)
- All transformation effects beyond `blur` — `scroll`/`spin`/`zoom`/`contrast`/`suck`
- `bake`/`burn` offscreen page model (Decree §6 Render)
- Embed resolution (`$code` → fetch + recursive render)
- The `?` and `...` choice/cycle operators

These land in phases 2–5.
