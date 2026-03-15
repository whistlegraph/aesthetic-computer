# KidLisp Cards — Status Report & Improvement Plan

## What We Have

### LaTeX → SVG Pipeline (`/kidlisp.com/cards/`)

**4 cards exist** (from 118 possible functions):
- `drawing-line` — `(line x1 y1 x2 y2)`
- `drawing-box` — `(box x y w h)`
- `colors-ink` — `(ink color [alpha])`
- `colors-wipe` — `(wipe [color])`

**Build system:** `build.sh` compiles `.tex` → PDF → SVG via `pdflatex` + `pdf2svg`.

**Emacs integration:** `kidlisp-cards.el` with keybindings for creating/building/previewing cards.

### Book UI in kidlisp.com Editor

The cards show up in a "book stack" inside the reference panel:
- 3D flip animation (front = SVG card, back = card-back.svg)
- Spread view (grid layout)
- Stack physics (offset/scale/rotation for depth)
- Only renders SVG cards for `aesthetic-computer` platform (other platforms use DOM-based cards)

### Card Back

A minimal card-back.svg (3.2KB) with a generic pattern.

---

## What's Dated / Broken

### 1. Wrong fonts — doesn't match kidlisp.com

The LaTeX cards use `\sffamily` (Computer Modern Sans) and `\ttfamily` (Computer Modern Mono). These don't match kidlisp.com's design at all.

**kidlisp.com uses:**
- `'Berkeley Mono Variable'` / `'Noto Sans Mono'` — code
- `'YWFTProcessing-Regular'` / `'YWFTProcessing-Bold'` — display/titles
- `'Comic Relief'` — fun/body text

**Fix:** Embed these webfonts in the SVGs (or switch the whole pipeline from LaTeX to HTML/CSS → SVG, which would make font matching trivial and eliminate the LaTeX dependency entirely).

### 2. Wrong syntax highlighting colors

The LaTeX cards use their own color palette:
- `klfunction: #FF6B6B` (coral)
- `klnumber: #4ECDC4` (cyan)
- `klstring: #FFE66D` (yellow)
- `klparen: #888888` (gray)

**kidlisp.com dark mode uses:**
- Functions: `#61afef` (blue)
- Numbers: `#d19a66` (orange)
- Strings: `#e5c07b` (warm yellow)
- Operators: `#98c379` (green)
- Variables: `#e06c75` (red)
- Comments: `#6a6a6a`

These need to match so the cards feel like they belong on the site.

### 3. Only 4 of 118 functions covered

The 12 categories in KidLisp, with suggested priority cards:

| Category | Functions | Cards | Priority additions |
|----------|-----------|-------|-------------------|
| Drawing | 17 | 2 (line, box) | `circle`, `tri`, `plot`, `shape`, `fill`/`outline` |
| Colors | 2 | 2 (ink, wipe) | done for now |
| Transform | 12 | 0 | `scroll`, `zoom`, `spin`, `blur`, `sort` |
| Media | 7 | 0 | `paste`, `write`, `tape` |
| Audio | 6 | 0 | `mic`, `amplitude`, `speaker`, `melody` |
| 3D | 8 | 0 | `cube`, `form`, `trans` |
| Control | ~10 | 0 | `def`, `later`, `if`, `repeat`, `once` |
| Math | ~12 | 0 | `random`/`?`, `wiggle`, `sin`/`cos` |
| Timing | ~4 | 0 | `1s`, `0.5s...`, `0.1s!` |
| System | ~5 | 0 | `width`/`w`, `frame`/`f`, `fps` |
| Navigation | ~3 | 0 | `$codeId`, `jump` |
| Misc | ~5 | 0 | `...` (cycle), `choose`, `bunch` |

**Minimum viable deck:** ~25 cards covering the most-used functions (could be informed by the live popularity data from `/api/store-kidlisp?stats=functions`).

### 4. Examples are too simple / not from real pieces

Current card examples are synthetic:
- `(line 0 0 width height)` — technically correct but boring
- `(box 10 10 100 50)` — doesn't show what makes KidLisp fun

**Better approach:** Pull real code snippets from top community pieces (the data is already available via `topHitsData` in the learn tab). Show the function in context, in a pattern someone actually used.

### 5. LaTeX pipeline is heavyweight

Requires `texlive`, `pdf2svg`, a full TeX installation. Building a card is slow (~2-3 sec each). Hard to iterate on design. Doesn't support the project's webfonts natively.

### 6. Card back is generic

The card-back.svg is a plain pattern. Should feature the KidLisp logo, the `( )` motif, or a generative pattern that matches the site identity.

---

## Proposed Improvements

### Phase 1: Switch from LaTeX to HTML/CSS → SVG

**Why:** Eliminates font mismatch entirely. Uses the same CSS variables, webfonts, and syntax highlighting as kidlisp.com. Cards become trivial to build — just an HTML template rendered to SVG via Puppeteer or even served as inline HTML.

**New pipeline:**
1. Card data lives in a JSON/JS array (function name, category, description, example code, diagram type)
2. An HTML template uses kidlisp.com's fonts, colors, and `highlightKL()` syntax highlighter
3. Build script uses Puppeteer (already available via oven) to screenshot to SVG/PNG
4. Or: skip pre-rendering entirely — render cards as live DOM in the book UI

**Card template zones (same layout, better rendering):**
```
┌─────────────────────────┐
│     CATEGORY (muted)    │  ← YWFTProcessing-Regular
│      FUNCTION NAME      │  ← YWFTProcessing-Bold, large
│    one-line description │  ← Comic Relief
│  ┌───────────────────┐  │
│  │   visual diagram   │  │  ← canvas/SVG illustration
│  └───────────────────┘  │
│  ┌───────────────────┐  │
│  │  (fn arg1 arg2)   │  │  ← Berkeley Mono, syntax-highlighted
│  │  (fn in context)  │  │     with kidlisp.com colors
│  └───────────────────┘  │
└─────────────────────────┘
```

### Phase 2: Expand to 25+ cards using real examples

Source the "Example" code block from actual community pieces:
1. Query popularity data for each function
2. Find the top-viewed piece that uses it cleanly
3. Extract a minimal snippet showing the function in real context

**Priority functions for first 25 cards:**
`wipe`, `ink`, `line`, `box`, `circle`, `tri`, `plot`, `fill`, `outline`,
`repeat`, `def`, `later`, `if`, `once`, `?`/`random`, `wiggle`,
`sin`, `scroll`, `zoom`, `spin`, `blur`, `paste`, `write`, `mic`, `amplitude`

### Phase 3: Deck design & polish

- **Card back:** Generative pattern using the `( )` motif in YWFTProcessing, dark bg matching kidlisp.com's `#111`
- **Category color bands:** Each category gets a subtle accent color on the card edge (Drawing = blue, Transform = orange, Audio = green, etc.)
- **Rarity indicator:** Functions used in <5% of pieces could get a "rare" badge — gamifies exploration
- **Card numbering:** `4/118` style — shows completeness and invites collection

### Phase 4: Interactive cards (skip pre-rendering)

Instead of static SVG files, render cards as live DOM elements:
- The code block could be **runnable** — tap to see the output
- Syntax highlighting via the existing `highlightKL()` function
- Cards could show a **live animated thumbnail** (the same oven WebP we use for list items)
- Eliminates the build step entirely — card data is just JSON

---

## Decision: LaTeX vs HTML/CSS vs Live DOM

| Approach | Fonts | Colors | Build time | Iterability | Interactivity |
|----------|-------|--------|-----------|-------------|---------------|
| LaTeX → SVG | Wrong (CM) | Wrong (custom) | Slow (~2s/card) | Hard | None |
| HTML → SVG (Puppeteer) | Correct | Correct | Medium (~1s/card) | Good | None |
| **Live DOM** | **Correct** | **Correct** | **None** | **Instant** | **Full** |

**Recommendation:** Move to live DOM rendering. The card data (function, category, description, example) stays in a JSON structure. The book UI already renders DOM-based cards for non-AC platforms — extend that with the kidlisp.com font/color treatment and syntax highlighting. Keep the LaTeX pipeline as a print/export option if physical cards are ever wanted.

---

## Quick Wins (can do now)

1. **Update `svgCards` color palette** to match kidlisp.com dark mode vars
2. **Add 5 more LaTeX cards** for the most popular functions (`circle`, `repeat`, `def`, `scroll`, `blur`)
3. **Improve existing examples** — replace `(line 0 0 width height)` with a real snippet from a popular piece
4. **Build a card data JSON** with all 118 functions — even before rendering, this becomes the source of truth for both the Learn tab reference and the card system
