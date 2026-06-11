# Ren'Py → AC: adapting the visual-novel idea to the piece stack

*2026.06.10 — research report. Grounding: `disks/CLAUDE.md` (lifecycle), `disks/besospesos.mjs` (first consumer), `lib/nom.mjs` (shared-engine pattern), `disks/marimbaba.mjs` + `lib/pop.mjs` (manifest-driven wrapper), `HAND.md` (gradient: libs are the instrument, leaves stay small).*

---

## 1. A short tour of Ren'Py's architecture

Ren'Py is three layers that are worth keeping mentally separate, because we only want one and a half of them.

### 1a. The script layer (the part worth stealing)

Ren'Py's script language is a flat sequence of statements grouped under **labels**, with a tiny control-flow vocabulary:

```renpy
define s = Character('Sylvie', color="#c8ffc8")
default book = False

label start:
    s "Hello!"
    menu:
        "Take the book":
            $ book = True
            jump library
        "Leave" if not shy:
            jump exit
label library:
    if book:
        "You chose the book path."
    return
```

The whole narrative model is:

- **say** — `"narration"` or `character "dialogue"`. One statement = one click. This is the engine's heartbeat: execution *parks* on a say until the player advances.
- **characters** — `define s = Character(...)` binds a short name to a display name + styling, so dialogue lines stay terse.
- **labels** — named entry points. `jump` is a goto (no stack); `call`/`return` push/pop a call stack so shared scenes can be reused. Labels can take parameters; `return` can carry a value (`_return`).
- **menus** — a list of caption → block pairs. Choices take an optional `if` guard (hidden when false) and a `set` clause (hide already-picked choices). Blocks set flags (`$ drank_tea = True`) and usually end in `jump`.
- **variables/flags** — `default` declares saveable state; `$` runs a line of Python; `if/else` branches on it.

**The minimal viable subset** — what every Ren'Py game actually uses — is: characters, labels, say, menu (with guards + flag effects), flags, if, jump. Call/return is the first optional extra; everything else (screen language, ATL, transforms, Python escape hatch) is elaboration.

### 1b. The presentation layer (mostly skip)

A retained scene graph: `scene bg meadow` clears a layer, `show sylvie green smile at left` adds/replaces a tagged sprite (tag + attribute system, so `show sylvie sad` swaps the expression on the existing sprite), `hide` removes, `with dissolve` transitions between interaction snapshots. Layers (master/transient/screens/overlay), zorder, ATL (a mini-language for animating transforms), side images next to the dialogue box, and a "screen language" for building UI declaratively.

This is all in tension with AC: our graphics are **immediate-mode** — `paint` redraws everything each frame, there is no retained displayable tree, and pieces like besospesos already draw parametric pixel portraits with `ink().box()` instead of sprite PNGs. The *concepts* that survive translation are: a stage with named slots (who's on screen, where, with what mood), a dialogue box with a typewriter, and a choice list. The retained-graph machinery does not.

### 1c. Persistence (two tiers, both map cleanly)

- **Saves**: a snapshot of (current statement, displayed images, music, all `default`-declared + mutated variables). Rollback is automatic checkpointing of that snapshot at every interaction, with custom "revertable" list/dict/set types so mutation is undoable.
- **Persistent**: a separate `persistent.*` namespace that survives across playthroughs — gallery unlocks, endings seen, settings — merged by recency when synced across machines.

The save = `{label, index, flags}` insight is the key one: because execution parks on statement boundaries, a save is just a program counter plus a flag bag. Tiny, JSON-able, perfect for `store`.

---

## 2. What the JS adaptations kept and dropped

- **Monogatari** (closest cousin) — kept Ren'Py's *statement-stream* model but encoded it as a JS object: `script({ 'Start': [ 'y:happy Hi! I am Yui.', {'Choice': …}, 'jump End' ] })` — labels are keys, statements are strings with a micro-syntax (`'y:happy text'`) or objects for structured actions. Characters defined separately via `characters({...})`. Conditionals are inline JS functions reading a storage object. Lesson: **a JS data structure is a perfectly good Ren'Py script**; the string micro-syntax keeps dialogue lines as terse as Ren'Py's. It dropped ATL and the screen language entirely; presentation is CSS.
- **RenJS** — YAML scripts on top of PhaserJS. Kept labels/choices/scene/show/music; dropped Python and the screen language; pitched at "writers, not programmers." Lesson: a declarative file works, but YAML bought them a parser dependency and a second syntax without buying expressiveness — guards and effects get awkward strings.
- **Ink / inkjs** — the other pole: a *presentation-agnostic* narrative engine. Knots/stitches (≈labels), diverts (≈jump), choices, variables, weave/gather to merge branches. The runtime hands the host app text + current choices and nothing else; the host owns all rendering. Lesson: **the cleanest seam is "story runtime below, presentation above"** — exactly the seam between a `lib/vn.mjs` script-runner and AC's `paint`.

The composite lesson for AC: take Monogatari's "script is data" encoding, Ink's runtime/presentation seam, and Ren'Py's statement-parking execution model. Take nobody's renderer.

---

## 3. Proposed design: `lib/vn.mjs`

A shared engine in the `nom.mjs` mold: module-level state, a `reset()` that re-zeros everything (the engine is a session singleton shared across vn pieces — same bleed-prevention comment nom carries), and the five lifecycle exports a wrapper forwards to. Pieces become thin wrappers that pass a **script object** at boot, marimbaba-style.

### 3a. The script format (JS object literal)

```js
// disks/besospesos.mjs — the whole piece is a script + a 12-line wrapper.
import * as vn from "../lib/vn.mjs";

const script = {
  meta: { title: "besospesos", desc: "a ceo dating sim…" },
  flags: { besos: 0, pesos: 0 },              // default-declared state (saveable)
  meters: [                                    // HUD meters, drawn by the engine
    { flag: "besos", icon: "heart", col: [255, 90, 140], at: "left" },
    { flag: "pesos", icon: "$", col: [140, 230, 140], at: "right" },
  ],
  cast: {
    sam: { name: "sam altman", face: { skin: […], hair: […], style: "wavy", … } },
    elon: { name: "elon musk", face: { … } },
  },
  start: "title",
  labels: {
    title: [
      { titleCard: ["besos", "pesos"], sub: "a ceo dating sim" },
      { jump: "select" },
    ],
    select: [
      { menu: "who needs you today?", choices: [
        { label: "sam altman — his facetime keeps freezing",
          if: (f) => !f.dated_sam, jump: "sam1" },
        { label: "elon musk — his smart house locked him out",
          if: (f) => !f.dated_elon, jump: "elon1" },
        { if: (f) => f.dated_sam && f.dated_elon, jump: "fin", auto: true },
      ]},
    ],
    sam1: [
      { show: "sam", mood: 0 },
      ["sam", "my facetime to shanghai keeps freezing. the board call is in four minutes."],
      { menu: { choices: [
        { label: "four minutes is plenty. let me ping it.",
          set: { besos: +2 }, say: "he watches you type. 'you're so composed.'" },
        { label: "i charge rush rates. $800.",
          set: { besos: -1, pesos: +800 }, say: "he wires it without blinking." },
      ]}},
      // …two more questions…
      { if: (f) => f.sam_besos >= 5, then: "sam_kiss", else: "sam_invoice" },
    ],
    fin: [ /* tally screen as a step type, or a custom paint hook */ ],
  },
};

function boot($) { return vn.boot($, script); }
function paint($) { return vn.paint($); }
function sim($) { return vn.sim($); }
function act($) { return vn.act($); }
function leave($) { return vn.leave($); }
function meta() { return vn.meta(script); }
export { boot, paint, sim, act, leave, meta };
```

Step vocabulary — the Ren'Py minimal subset, one JS shape each:

| Ren'Py | vn.mjs step | notes |
|---|---|---|
| `s "text"` | `["sam", "text"]` or bare `"narration"` | array = dialogue, string = narration; parks until advance |
| `label x:` | key in `labels` | |
| `jump x` | `{ jump: "x" }` | program counter reassignment |
| `call x` / `return` | `{ call: "x" }` / `{ return: true }` | tiny array as the stack; v2 if besospesos doesn't need it (it doesn't) |
| `menu:` + `if` guard + `$ flag` | `{ menu, choices: [{ label, if, set, say, jump }] }` | `set` is a delta bag (`{besos:+2}`), `say` is the reply beat, guard hides |
| `default` / `$ x = v` | `flags` defaults + `set` deltas + `{ do: (f) => … }` escape hatch | |
| `if/else` | `{ if: pred, then: label, else: label }` | predicates are plain functions over the flag bag |
| `show sylvie smile at left` | `{ show: "sam", mood: 1, at: "center" }` | mood drives the parametric portrait, not sprite swaps |
| `scene bg` | `{ scene: { wipe: [26,12,28] } }` | a wipe color / backdrop drawer, not an image |
| `with dissolve` | skip (v1) | a frame-counted crossfade later if ever wanted |

### 3b. How it maps onto boot/paint/sim/act

The engine is a **statement-parking interpreter**: a program counter `pc = { label, index, stack: [] }` plus a `phase` ("typing" | "parked" | "choosing"), exactly the shape besospesos already has ad hoc (`state`/`q`/`phase`/`typed`).

- **boot($, script)** — `reset()`, store the script, seed `flags` from defaults, set `pc` to `script.start`, then `run()` until the first parking step (a say or menu). Optionally `store.retrieve` a save and resume.
- **sim()** — the feel loop, lifted verbatim from besospesos: `typed += speed` typewriter, scheduled-melody drain (`melody.filter`-style blip queue), floaters. No story logic lives here.
- **paint($)** — immediate-mode redraw of the current parked state: backdrop wipe, the shown cast member (parametric portrait via the `face` spec — `drawCEO` generalizes into the engine's default portrait painter, with a script-level `drawPortrait` override hook for pieces that want their own), name, meters HUD, word-wrapped typewriter line, and — when phase is "choosing" — the stacked outline-box choice buttons. Rebuilds `layout.hits` each frame for `act` to hit-test, the besospesos/nom idiom.
- **act($)** — advance/choose only: tap completes the typewriter, then advances `pc`; arrows + enter / number keys / touch pick choices; escape = script-defined bail (`onBail` hook → besospesos's "walking out counts as a dud"). Every act that moves the `pc` calls `run()` — which executes non-parking steps (jump, set, if, show, scene) eagerly until it hits the next say/menu. That eager-run loop is the entire interpreter; it should be ~30 lines.

`run()` parking on says/menus is what makes saves trivial and keeps `paint` pure: between interactions, nothing in the story moves.

### 3c. Persistence → `store`

Two tiers, mirroring Ren'Py exactly, on the existing API (`store[key] = …; store.persist(key, method)` / `await store.retrieve(key, method)`):

- **Save slot** (Ren'Py save file): `store["vn:besospesos:save"] = { pc, flags, shown }`, persisted with `"local"` (it's tiny JSON — localStorage is fine; `"local:db"` if a script ever carries blobs). Written automatically at every menu choice — choices are the natural checkpoint, the analogue of Ren'Py snapshotting at interaction boundaries. `leave()` persists too. boot offers resume if a save exists.
- **Persistent** (Ren'Py `persistent.*`): `store["vn:besospesos:seen"] = { endings: {…}, choicesPicked: {…} }` — survives resets, powers "new game+" guards (`if: (f, p) => p.endings.rich` for content that only unlocks on a second run) and Ren'Py's `menu set` behavior (graying choices you've picked in any playthrough).

Because the save is `{pc, flags}` and predicates are pure functions of flags, there's no pickling problem, no revertable-collection machinery — the whole reason Ren'Py's save system is complicated is that it snapshots arbitrary Python heap; ours snapshots a JSON bag by construction.

---

## 4. Script format recommendation

**Recommendation: JS object literals in the piece file (option A), with the structure kept JSON-clean except for predicate functions.** This is the nom/besospesos lineage, not the pop/marimbaba one, and the difference is principled:

| | A. JS object in the piece | B. JSON manifest (`disks/vn/x.json` + fetch) | C. KidLisp DSL |
|---|---|---|---|
| guards/conditions | plain closures over flags — full power, zero parser | strings to mini-eval (`"besos>=5"`) — invents a worse language | natural `(if (>= besos 5) …)` |
| authoring/loop-generation | best — one file, the loop already writes these (besospesos was generated this way) | okay, but two files per piece and a fetch in boot | needs new special forms in a 15.6k-line evaluator mid-rehandify |
| hot reload | piece save = reload, free | manifest fetch can cache stale | free |
| who it serves | us + the loop | external tooling that doesn't exist yet | KidLisp authors, eventually |
| precedent | nom games, besospesos | pop tracks (where the data really is pure data: notes, colors) | — |

pop.mjs earned its JSON because a track manifest is *genuinely* declarative — no conditionals, no predicates. A VN script is not: guards, branch predicates, and flag math want a real language, and the JSON versions of those (`"if": "besos>=5 && !dated_sam"`) mean writing an expression parser — building a worse JS inside JSON. Skip it. If a piece's script grows big, it can move to a sibling `disks/vn/besospesos.data.mjs` and be imported — still option A, just split.

**KidLisp is the right v2, not the v1.** A `(label …)` / `(say …)` / `(menu …)` form-set riding the existing evaluator would make VN authoring available to `$code` pieces and the prompt — a genuinely AC-native move (Ren'Py's screenplay syntax is arguably *more* lisp-shaped than JS-shaped). But it means touching `kidlisp.mjs` (rehandify target #3, under-tested) and designing parking semantics inside an evaluator built for per-frame re-evaluation — execution that *waits* is a new concept there. The clean path: build `lib/vn.mjs` against JS step objects first; the step vocabulary above is trivially expressible as S-exprs later, and a thin `(vn …)` reader can compile KidLisp forms into the same step objects without the engine knowing. Design the step schema now as the stable IR; let formats compile to it (this is exactly Ink's compiler/runtime split).

---

## 5. Don't-build list

- **Rollback.** Ren'Py's flagship feature and its biggest complexity tax (revertable collections, per-interaction heap snapshots). Our pieces are 5-minute arcade-VNs, not 20-hour reads; "esc bails the date" is the AC-native undo. If ever wanted: saving `{pc, flags}` at each choice into a ring buffer gives back-button rollback in 10 lines — so deferring costs nothing.
- **Screen language.** A declarative UI toolkit inside the engine. AC already *is* the UI toolkit — `paint` + `layout.hits` + `ui.Button`. Custom screens are just steps with a `paint` hook.
- **ATL / transforms / layers / zorder.** Retained-mode animation language on a retained scene graph we don't have. Mood-parameterized portraits + `sim`-driven floaters/pulses already read better on a 6px-font pixel screen than tweened sprite slides. A `{ at: "left"|"center"|"right" }` slot on `show` covers two-character scenes; that's the whole positioning system.
- **Side images, say-attributes, image tag/attribute algebra.** Collapses to `mood` on the parametric portrait.
- **Skip/auto-forward.** Skip-seen-text requires per-statement seen-tracking for a feature that matters in 40k-word novels. Tap-to-complete-typewriter (already in besospesos) is the right amount of impatience support. Auto mode is a timer trivially added later if a piece wants kiosk playback.
- **NVL mode, history/backlog, self-voicing, translations.** Real Ren'Py features, zero AC pieces asking.
- **Asset pipeline** (image folders → names, archives). AC VNs are no-asset by conviction — parametric faces, bitmap hearts, synth blips. This is the identity of the form here, not a limitation.
- **A custom script *file format* / parser of any kind.** No `.vn` text format, no YAML (RenJS's parser bought them little). JS objects now, KidLisp forms later — both already have parsers.

## 6. Sizing and the gradient

Per HAND.md the engine is instrument-tier: human-owned, legible, one head. Everything in §3b exists in besospesos today in ad-hoc form (~250 of its 609 lines are engine: typewriter, advance gating, choice buttons, hit-test, melody scheduler, floaters, wrap, portrait painter). Hoisted and generalized, `lib/vn.mjs` should land around 400–500 lines — a third of nom. besospesos then becomes the marimbaba shape: a script object (~200 lines, almost all of it the writing — which is the point) plus the 8-line forwarding wrapper. The second consumer (any new date-able cast, a mystery, a choose-your-own anything) is a loop-generated leaf: paste a script, ship a piece.
