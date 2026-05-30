# KidLisp — Score

The KidLisp "score": one place to track the language spec, the reference implementation, every conforming runtime in the monorepo, and the conformance corpus they're all graded against.

> KidLisp is *a language*, not *a file*. `system/public/aesthetic.computer/lib/kidlisp.mjs` is the **reference implementation**; the **specification** is `KidLisp Decree '26`. Any runtime — JS, Common Lisp, Swift, WASM, Game Boy ROM — claims a conformance level against the Decree and runs the same corpus.

## 1. Specification

| Document | Path | Role |
|---|---|---|
| Decree '26 | `kidlisp/docs/core/kidlisp-decree-26.md` | Normative ABI + conformance levels |
| Decree (latest pointer) | `kidlisp/docs/core/kidlisp-decree.md` | Always points at the current stable decree |
| Language reference | `kidlisp/docs/core/language-reference.md` | Syntax + constructs |
| Complete API map | `kidlisp/COMPLETE_API_MAP.md` | All 118 built-ins, grouped |

**Conformance levels** (Decree '26 §3):
- `Core` — parser, evaluator, lifecycle ABI, host shape
- `Render` — offscreen buffers, `page`/`paste`, alpha compositing
- `Audio` — `amp`/`mic` globals + ranges

**Named profiles:**
- `RBP-26` — `$roz` Baseline Profile (Decree '26 §13.1). Minimum surface to run `$roz` correctly: `ink`/`line`/`circle`/`scroll`/`spin`/`zoom`/`contrast`/`?`/`1s...`/`2s...`/`0.5s`/`fade:…`, magic vars `w`/`h`/`w/2`/`h/2`.

Canonical claim format: `KidLisp Decree '26: Core + Render` (extend with `+ Audio` and/or `+ RBP-26` as supported).

## 2. Reference Implementation

| Path | Surface |
|---|---|
| `system/public/aesthetic.computer/lib/kidlisp.mjs` | The canonical evaluator — JS, runs in browser + Node + JSC |
| `system/netlify/functions/store-kidlisp.mjs` | Source storage / `$code` resolution / hit counting |
| `kidlisp/tools/` | `api-summary.mjs`, `source-tree.mjs`, etc. |

The reference impl is **load-bearing for deployment** (web runtime, service worker cache, `disk.mjs` imports, session server). Its path is frozen; do not move it.

## 3. Runtime Registry

Every implementation in the monorepo, with claimed conformance level and current status. **Update this row when you change a runtime's surface.**

| Runtime | Path | Decree claim | Status | Notes |
|---|---|---|---|---|
| **JS** (reference) | `system/public/aesthetic.computer/lib/kidlisp.mjs` | `'26: Core + Render + Audio` | shipping | Canonical; spec defers to behavior here when ambiguous |
| **Common Lisp** (AC Native) | `fedac/native/cl/kidlisp-*.lisp` | `'26: Core + Render` (target) | in progress | Tree-walker, DRM/KMS framebuffer; replacing QuickJS path |
| **Swift** (Menuband) | `slab/menuband/` | `'26: Core + Render` (planned) | not started | This document's motivating port; Metal blit + CPU framebuffer |
| **WASM** | `kidlisp-wasm/` | unclaimed | experimental | Compiler approach (`compiler.mjs`) |
| **Playdate** | `kidlisp-playdate/` | unclaimed | experimental | C runtime for Panic Playdate |
| **Game Boy** | `kidlisp-gameboy/` | unclaimed | experimental | GBDK C + asm |
| **N64** | `kidlisp-n64/` | unclaimed | experimental | Bare-metal asm exploration |
| **CLI** | `kidlisp-cli/` | host-runner | shipping | Public `kidlisp` CLI |
| **Sidecar** | `kidlisp-sidecar/` | host-service | shipping | Clojure service |
| **VS Code syntax** | `vscode-extension/kidlisp-syntax.ts` | tooling | shipping | Editor highlighting only |
| **kidlisp.com** | `kidlisp.com/` | site | shipping | Landing page |
| **Knowledge base** | `kidlisp-knowledge/` | docs | shipping | LLM-oriented documentation aggregator |
| **Analysis tools** | `kidlisp-tools/`, `kidlisp/tools/` | tooling | shipping | Probe + source-tree (two locations — see Open Questions) |

## 4. Conformance Corpus

The corpus is **the top KidLisp pieces by live hit count**, pulled from production. Any new runtime is expected to render these pixel-comparably against the reference implementation.

Refresh command: `curl -s "https://aesthetic.computer/api/store-kidlisp?recent=true&limit=10&sort=hits"`.

### Top 10 (refreshed 2026-05-25)

| # | Code | Hits | Chars | Source / feature footprint |
|---|------|-----:|------:|---|
| 1 | `$bop` | 14,708 | 25 | `purple, ink, line, blur 5` — bare-color wipe, bare commands, blur |
| 2 | `$pie` | 10,363 | 153 | `(fps 24)`, timing wipe, magic vars, `scroll frame frame` |
| 3 | `$roz` |  9,106 | 239 | `fade:` gradient, `1s...`/`2s...`/`0.5s`, `?`, spin/zoom/contrast/scroll/circle → **RBP-26 reference** |
| 4 | `$4xa` |  8,330 |   4 | `blue` — bare color = implicit wipe |
| 5 | `$ceo` |  8,167 |  82 | `coat fade:…:frame` animated gradient + zoom |
| 6 | `$cow` |  7,819 |  52 | `($39i 0 0 w h 128)` `($r2f …)` — `$code` embeds (recursive eval) |
| 7 | `$4bb` |  5,629 | 215 | `bake`/`burn`, `ink … erase`, scroll vectors, blur |
| 8 | `$wib` |  5,424 |  11 | `(wipe blue)` |
| 9 | `$39i` |  5,332 | 275 | flood, circle, timed zoom/blur/contrast, multi-statement |
| 10 | `$nsh` |  5,326 |   7 | `kidlisp` (bare identifier) |

### Suggested phasing for a new port

1. **$bop · $wib · $4xa** — bare color → wipe, bare commands, `(wipe color)`, `blur N`
2. **$pie · $39i** — `(fps N)`, timing tokens, magic vars (`w`/`h`/`width`/`height`/`frame`), `scroll`, `flood`, `circle`, `zoom`, `contrast`
3. **$roz · $ceo** — `fade:` gradients with `:frame` animation, `spin`, `coat`, `?`/`...` cycle → **claims `RBP-26`**
4. **$cow** — `$code` embed (recursive sub-region eval + network/cache fetch)
5. **$4bb** — `bake`/`burn` (offscreen page semantics, Decree '26 §6)

A port hitting phase 3 can publish `KidLisp Decree '26: Core + Render + RBP-26`.

## 5. Adding a New Runtime

1. Pick a path. Sibling-of-monorepo (`kidlisp-foo/`) for hardware/platform ports; embedded inside a host app (`slab/menuband/`, `fedac/native/cl/`) for ports tied to a specific runtime.
2. Add a row to §3 above with `claim = unclaimed`, `status = not started`.
3. Build through the §4 phases. After each phase, render the corpus and diff against reference frames.
4. When all `RBP-26` tokens render correctly, update the row to `'26: Core + Render + RBP-26`.
5. Conformance test artifacts (frame PNGs, diff reports) should land under `kidlisp/conformance/<runtime>/`.

## 6. Open Questions

- **`kidlisp/tools/` vs `kidlisp-tools/`** — two tool homes; the sibling should probably absorb the sub-dir or vice versa. Not urgent, but document the decision when made.
- **Physical reorg** — should the reference impl be re-homed under `kidlisp/` and re-exported from `system/public/aesthetic.computer/lib/`? Conceptually cleaner, but the blast radius (service worker cache keys, bundler resolution, WebSocket module loader prefetch, every `disk.mjs` import path) hasn't earned its cost. **Decision: keep `kidlisp.mjs` where it is; `kidlisp/` is the *house* (spec, corpus, docs, registry), not the *runtime location*.**
- **`KDL-26` test suite** — the Decree §10 reserves this name for the formal conformance suite. Land the corpus in §4 here, then graduate it to `KDL-26` once stable.
- **Sibling consolidation** — `kidlisp-knowledge/` and `kidlisp/docs/` overlap; resolve before they drift further.

## 7. Pointers

- Decree '26: [`docs/core/kidlisp-decree-26.md`](docs/core/kidlisp-decree-26.md)
- Language reference: [`docs/core/language-reference.md`](docs/core/language-reference.md)
- API map: [`COMPLETE_API_MAP.md`](COMPLETE_API_MAP.md)
- Directory map: [`STRUCTURE.md`](STRUCTURE.md)
- Top-level project score: [`../SCORE.md`](../SCORE.md)
