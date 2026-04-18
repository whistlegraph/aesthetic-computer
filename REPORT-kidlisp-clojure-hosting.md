# Hosting KidLisp on Clojure / ClojureScript

Date: 2026-04-17
Author: Claude (for @jeffrey)
Scope: Feasibility assessment of moving the KidLisp runtime from hand-rolled JS to a Clojure/ClojureScript host, plus the state of Clojure → WASM as of early 2026.

## TL;DR

**Not recommended as a wholesale migration.** ClojureScript is a natural Lisp host in principle, but KidLisp's specific surface area — bespoke reader syntax, tight coupling to the Aesthetic Computer Disk API, and an expectation of ~instant boot inside an already-large web runtime — makes a rewrite costly while delivering modest linguistic upside. The most defensible path, if Clojure appeals for other reasons, is a compile-time target (KidLisp AST → ClojureScript forms) rather than a runtime port. **Clojure on WASM** in 2026 is still pre-production across the board; nothing on that track is ready to replace a shipping JS runtime.

---

## 1. What KidLisp actually is today

- `system/public/aesthetic.computer/lib/kidlisp.mjs` — **~15,400 lines** of hand-written ES modules: reader, evaluator, effect dispatch, and timing DSL all in one.
- 118 built-in functions across 12 categories (drawing, audio, math, control, animation, data, etc.).
- Extra ports already exist outside the browser runtime:
  - `kidlisp-gameboy/` — KidLisp → GBDK/C compiler for Game Boy.
  - `kidlisp-n64/` — bare-metal / libdragon experiments.
- Integrated with the JS-native **Disk API** (`lib/disk.mjs`) via destructured function bags (`{ wipe, ink, line, ... }`) — effects are immediate-mode calls into canvas/WebGL/audio graph.
- Non-standard Lisp features that matter for any host decision:
  - Timing literals: `1s`, `2s...`, `0.5s!`.
  - Cached-code references: `$abc123`, `(embed $code ...)`.
  - Handle/timestamp references: `@user/123456`.
  - Unquoted URLs in arg position: `(paste https://example.com/a.png x y)`.
  - Dynamic color atoms: `rainbow`, `zebra`, `c0..c150`.
  - Reader-visible dashes are **subtraction**, not identifier chars.

These extensions mean KidLisp is only *approximately* a Lisp at the reader level. Any host — Clojure included — would need a custom reader, so "it's already a Lisp" is a weaker argument than it first appears.

## 2. Why ClojureScript looks tempting

- **Homoiconicity.** KidLisp ASTs map cleanly onto `clojure.core` sequences and symbols.
- **Macros.** Timing (`1s`, `2s...`), `(once ...)`, `(later ...)` all read like macro fodder — ClojureScript macros would give them a first-class home instead of special-cased evaluator branches.
- **Persistent data structures** for free, plus proper recur/trampolines for the repeat/bunch loops.
- **REPL-driven iteration.** shadow-cljs + reagent-style hot reload is culturally aligned with "tweak a number, keep the canvas" — which is exactly what `REPORT-kidlisp-realtime-state.md` says KidLisp still doesn't do well.
- **Compiler infrastructure.** Google Closure's advanced optimizations, dead-code elimination, and source maps are mature.
- **Shared language across hosts.** In principle one ClojureScript codebase could target browser (cljs), native (jank/GraalVM), and JVM simultaneously — appealing for the Game Boy / N64 / OS targets already in the repo.

## 3. Why it's the wrong move for KidLisp specifically

### 3.1 Bundle and boot cost

- A minimal self-hosted ClojureScript runtime (for runtime eval of user code — which KidLisp *must* do) brings in `cljs.js` + the analyzer + the reader: **~1–2 MB gzipped** in practice, even after advanced optimizations, because you cannot DCE a runtime evaluator.
- Scittle / SCI is smaller (~300–500 KB gzipped) but is an *interpreter* with different perf characteristics than the present kidlisp evaluator.
- The current kidlisp.mjs ships as part of Disk; its footprint is already accounted for and tree-shakes against the rest of the runtime. A CLJS host would add weight *on top of* disk.mjs (572 KB) rather than replacing anything JS-shaped.
- Aesthetic Computer is mobile-first; a cold-start regression of even 500 ms on low-end devices would be felt immediately.

### 3.2 Reader is not reusable

- Clojure's reader cannot parse `2s...`, `0.5s!`, `$abc123`, `@user/123456`, or bare URLs.
- You'd still write a custom reader. At that point, "ClojureScript is a Lisp" buys you **data structures and macros** but not parsing.
- Worse: you must teach editor/LSP/formatter tooling that these literals exist, or give them up. The existing `kidlisp-reference.mjs` is already a docs-first contract; splitting it across Clojure reader + custom reader risks drift.

### 3.3 Effect boundary friction

- KidLisp calls are side effects on a JS graphics/audio API designed around destructuring (`{ wipe, ink, paste }`). ClojureScript interop with that shape is verbose (`(.wipe api)` / `(js/api.wipe)`) unless you wrap everything, and then you're maintaining two APIs.
- The Disk API changes often (see `disk.mjs` churn). Every change becomes a double-edit: JS definition + CLJS wrapper.

### 3.4 Ecosystem/ops mismatch

- The rest of AC is `.mjs`: boot, bios, disk, session server, netlify functions, lith deploy. Introducing a ClojureScript build chain (shadow-cljs, deps.edn, JVM on the build box) fights the current fish-based single-toolchain ethos.
- lith (DO VPS) deploys pull from the tangled knot and run Node. Adding a JVM-class dep to the deploy pipeline is a real cost.

### 3.5 Existing external ports regress

- `kidlisp-gameboy` compiles KidLisp → C for GBDK. A ClojureScript-hosted evaluator doesn't help this path (still need a bespoke compiler).
- `kidlisp-n64` is assembly-adjacent. Same story.
- If anything, the Game Boy and N64 work suggests KidLisp's *semantic model* is the stable asset and the **evaluator language is incidental** — which argues for keeping the evaluator where its neighbors live (JS in the browser, C on GB, asm on N64), not centralizing on Clojure.

## 4. State of Clojure → WASM (early 2026)

Nothing in this space is production-ready for replacing a shipping web runtime. Summary of the tracks:

### 4.1 jank (LLVM-native Clojure)

- Native Clojure dialect by Jeaye Wilkerson, targets LLVM IR → native binaries.
- LLVM's `wasm32` backend is mature, so in principle jank can emit WASM. In practice, the Clojure runtime (persistent collections, keywords, vars, multimethods) ships as a C++ runtime library that has to be built for the target; the WASM build path has been "experimental / pre-alpha" through 2025 and into early 2026.
- **Not a credible host for a browser interpreter today.** Would also need AOT — jank doesn't give you in-browser `eval` out of the box.

### 4.2 GraalVM native-image → WASM

- Oracle's GraalVM has an experimental WebAssembly backend (`native-image --tool:wasm` / Truffle-on-WASM variants). Works for tiny programs; Clojure pulls in a large JVM surface and hits `UnsupportedFeatureError` on real-world code regularly.
- Babashka (GraalVM + SCI) demonstrates Clojure can AOT to a native binary, but Babashka's own maintainers have not committed to WASM as a shipping target.
- **Use case fit:** poor for a browser runtime. Size would dwarf the current evaluator.

### 4.3 SCI / Scittle (pure JS, not WASM)

- Small Clojure Interpreter by @borkdude. Runs in the browser today as plain JS (~300–500 KB gz).
- **Not a WASM port** — it's JS. Often miscategorized in WASM discussions.
- Could theoretically be embedded inside a QuickJS-WASM sandbox, but that's stacking interpreters and helps nobody.

### 4.4 ClojureScript via JS → WASM glue (e.g. Javy, Spin, Kotlin/JS-WASM paths)

- Tools like Javy (Shopify) and Spin/Wasmtime embed JS engines inside WASM. You *can* run ClojureScript-compiled JS inside a WASM-embedded JS engine. That doubles the interpreter layers.
- Useful for serverless edge; irrelevant for AC's browser runtime.

### 4.5 Ferret (Clojure → C++)

- Ferret AOT-compiles a Clojure subset to C++. C++ → WASM via Emscripten is routine, so Ferret → WASM is feasible for small, statically-knowable programs.
- Doesn't support `eval`, which is the KidLisp core requirement.

### 4.6 Summary table

| Track | Supports runtime `eval` | Production-ready for browser | Rough browser size |
|---|---|---|---|
| SCI / Scittle (JS) | Yes | Yes (not WASM) | 300–500 KB gz |
| Self-hosted ClojureScript (JS) | Yes | Yes (not WASM) | 1–2 MB gz |
| jank → WASM | No (AOT) | No, pre-alpha | Unknown, likely multi-MB |
| GraalVM native-image → WASM | Partial | No, experimental | Large |
| Ferret → C++ → WASM | No (AOT) | Niche only | Small but feature-limited |
| Javy/QuickJS-WASM hosting CLJS | Yes (via nested JS) | Shipping for edge, not browser | 2–5 MB |

**Bottom line:** if the reason to move to Clojure is to eventually run KidLisp *as WASM*, wait. In 2026 the realistic browser deployment of Clojure is still JavaScript (via CLJS or SCI), not WebAssembly.

## 5. Concrete paths forward (ordered by cost)

1. **Do nothing to the host, improve the current evaluator.**
   The single biggest pain point identified in `REPORT-kidlisp-realtime-state.md` is the *full-reset-on-edit* behavior. That is a state-management issue, not a language-host issue. Fix it in `kidlisp.mjs`.

2. **Adopt Clojure *data* without adopting the Clojure *runtime*.**
   Keep the JS evaluator; borrow ideas (persistent vectors via a tiny Immer-like lib, `recur`-style trampolines). Cheap, keeps the bundle lean.

3. **Move the KidLisp compiler (not the interpreter) to Clojure.**
   The Game Boy and N64 ports are essentially compilers. If you want a unified compilation story, a JVM-hosted Clojure compiler that emits C / asm / JS makes sense there, because it runs at dev time, not in the user's browser. This is the **highest-value** place Clojure would land in AC.

4. **Rewrite the browser evaluator in ClojureScript.**
   Only if KidLisp stops being primarily a browser artifact *and* you commit to shadow-cljs-in-the-deploy-pipeline. Rough order: 6–12 eng-weeks to reach parity, assuming you keep a custom reader and a CLJS-shaped Disk API wrapper. Meaningful perf work on top.

5. **Bet on Clojure WASM.**
   Premature in early 2026. Revisit when jank's WASM target ships stable builds and has `eval` (or when an ahead-of-time model becomes acceptable for KidLisp).

## 6. Recommendation

Keep the runtime in JavaScript. Invest Clojure/ClojureScript effort, if any, in the **compiler tier** (the place where KidLisp becomes Game Boy C, N64 asm, or native binaries) — not the browser interpreter. Do not block on Clojure WASM; the 2026 state of that ecosystem is not where a shipping creative-computing runtime should live.

## Appendix: what would change my mind

- A production-grade `sci` or `scittle` release that is clearly under 150 KB gzipped with a usable macro system.
- A decision to rebuild Aesthetic Computer's client around a JVM/GraalVM deploy story for other reasons (e.g. collapsing session-server + site onto a single JVM runtime), at which point CLJS becomes a natural front-end complement.
- jank reaching a 1.0 with a shipping WASM target and in-browser `eval`.
- KidLisp growing a module system / macro layer that genuinely outstrips what the hand-rolled evaluator can express — at that point a host language with real macros starts paying for itself.
