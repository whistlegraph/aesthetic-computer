# The Hand — a code style guide for Aesthetic Computer

companion to `papers/VOICE.md`. VOICE.md governs how papers *sound*; this governs how the code *reads*.

it exists because of what *The Hand and the Loop* (`papers/arxiv-hand-and-loop`) found: across three eras the codebase gained velocity and breadth and lost the instrument — the terse, single-mind discipline of the hand-written years. this guide is how the hand gets handed back to the loop. an agent that reads this should write code that knows it's part of an instrument, not just code that runs.

## the one rule everything else serves

**a piece of this system should fit in one mind.** terseness here is not golf — it's the compression that comes from knowing the idiom so well you don't need the long name. write small because small is knowable.

## naming

- a name that teaches the idiom beats a correct generic one. `choose` over `getRandomKey`. `p2` for a 2d point, `pA`/`pB` for two of them, `c` for a color in a tight loop.
- a name earns its length. module-level and api names can be longer — strangers read them. locals in a tight function stay short — they're read in context.
- playful is allowed. `flip`, `nonvalue`, `anyKey`. a name can have a sense of humor.
- don't rename to satisfy a linter's idea of clarity. clarity here is idiom-fluency, not verbosity.

## comments

- explain *why*, not *what*. the code already says what. `// a holder for the logged-in user. (defined in boot)` — factual, parenthetical, sparse.
- no JSDoc ceremony on internal functions. if the signature needs a paragraph, the function is wrong.
- no banner walls. a `// === SECTION ===` every twenty lines is the loop labeling its own output. one short comment where the *why* is non-obvious beats a decorative header.
- a comment that restates the next line is noise. delete it.
- emoji in a comment is a tag, not decoration. the lifecycle headers earn theirs because they're a learned fingering. a brain emoji on a variable does not.

## the piece spine

- pieces keep the lifecycle shape: `boot`, `paint`, `act`, `sim`, `leave`. same fingering every time, so any piece is legible at a glance.
- destructure the api in the param list — `function paint({ wipe, ink, screen })` — take what you use, nothing more.
- a piece is a leaf. leaves stay small and bare (see *the gradient*).

## defensiveness

the subtle one. the loop's reflex is to wrap everything; the hand's reflex is to let it crash and tell you. neither is always right.

- in dev, let it throw. a guard that swallows an error you needed to see is worse than the crash.
- guard at *boundaries* — network, user input, audio that should degrade not die. `sound?.synth?.()` is good; a missing synth shouldn't kill the frame.
- don't guard between two lines you wrote that can't be null. a `try`/`catch` around your own pure function is ceremony.
- the test: would this guard ever fire from a cause i can't see at the call site? if no, drop it.

## file size

- a file should fit in one head. when it stops fitting, split it — not into a `utils` dumping ground, but into parts that each have a spine.
- foundational libs are where bloat hides and hurts most. `disk.mjs` at 16k lines is not more capable than the system that fit in 5k — it's just less knowable.
- growth from real surface (a new subsystem) is fine. growth from boilerplate, redundant guards, and restated comments is the thing to carve.

## logging

- sparse and contextual. a log is for a reader at 2am, not for filling the console.
- one emoji as a tag is fine — `📦 [DISK] imports: 40ms`. a different emoji on every line is the loop decorating.
- delete debug logs before they ship. `rm prompt logs` is a real commit and a good habit.

## commits

- lowercase, sound like a person. ``mo `sno` tweaks``. backticks around piece names.
- conventional prefixes (`fix:`/`feat:`) are optional, not a ritual. use one when it genuinely helps scanning; don't perform it.
- co-authorship trailers stay — credit is honest — but the subject line is still yours to write like you mean it.

## the gradient

not everything should be carved to the bone. the honest split:

- **leaves (pieces) — keep them small, let the loop generate them.** a 24-line generated `stripes.mjs` already reads like the hand. don't gold-plate leaves.
- **libs (the foundation) — these are the instrument; a human hand owns them.** legibility here outweighs feature velocity. spend the carving budget here.
- **new infra meant for others to read — a little more structure is fine.** legible-to-strangers is a real goal there; don't force piece-terseness onto a public api someone else maintains.

---

## the rehandify pass (active campaign)

bring the foundational libs back to a size a single mind can hold — without breaking the live system or wasting effort on leaves that are already fine.

**what this is, and is not.** the goal is *knowability* — better abstraction, cleaner seams, modules that each fit a head. it is NOT compression for its own sake. terse 5k lines that only fit *one specific* head is not a win over legible 12k — it just trades loop-bloat for author-lock-in, a bus factor of one. and much of the growth was real surface (permission systems, metadata, kidlisp, VST), not fluff — you can't carve features, only ceremony. so the honest carveable target is the *redundant* guards, banner walls, and restated comments, not the line count. and it is NOT safe without tests: `disk.mjs`/`bios.mjs` are under-tested (the paper: ~2.8% of commits touch tests), and refactoring under-tested core for aesthetics is a foot-gun. characterization tests come first or the pass doesn't start.

the paper named the targets, in priority order by pain:

1. `disk.mjs` — 16,821 lines (was 5,154). worst offender; the whole api flows through it.
2. `bios.mjs` — 21,679 lines (was 5,211).
3. `kidlisp.mjs` — 15,640 lines.
4. `boot.mjs` — 2,039 lines for a bootstrap (was 203).

principles for the pass:

- **behavior-preserving only.** rehandify is refactor, never rewrite. no feature changes ride along.
- **one file, one pass at a time.** never carve two core files in the same change.
- **measured.** every pass reports before/after on the hand metrics (LOC, try/catch count, null guards, comment density, emoji logs), so the work is legible and we don't kid ourselves.
- **test-backed.** a carve lands only behind green tests + a manual run of the surface it touches. core libs have no margin for "looks fine."
- **leaves are out of scope.** pieces are already small. don't touch them.
- **8gb discipline.** one carve at a time; no parallel chromium/builds.

phases:

- **phase 0 — the meter + a safety net.** two things, both cheap and both worth it regardless of how far the pass goes. (a) a small script (`toolchain/hand/hand-meter.mjs`) that computes the paper's metrics for any file at HEAD, and at a given git rev so we can compare to the 2023 version — turns "feels bloated" into a number, and shows where ceremony ends and real surface begins. (b) characterization tests around the slice of `disk.mjs`/`bios.mjs` behavior a pass will touch — the precondition for any carve. ~an afternoon for the meter; the tests are ongoing and gate everything downstream.
- **phase 1 — split, don't shrink.** the first carve of a god-file isn't deleting lines, it's finding the seams. pull self-contained regions of `disk.mjs` into sibling modules that each have a spine and fit a head. behavior identical; the win is abstraction and knowability, not a smaller number. this is the safest high-value move and may be *most* of the worthwhile work — if the file is legible as a set of modules, line count stops mattering.
- **phase 2 — collapse the ceremony.** within each module: remove banner walls, restated comments, and redundant guards (the ones that can't fire — keep the boundary ones). restore terse local names where they teach the idiom. every removal justified against *the gradient*.
- **phase 3 — hold the line.** the meter runs in CI (warn, don't block) on the core libs, so new bloat is visible the day it lands instead of a year later. commit-voice and comment discipline come back by habit, helped by this guide being in the loop's context.

success isn't a line-count target for its own sake — it's `disk.mjs` and `bios.mjs` becoming a set of modules where each one fits in a single mind again. the property the paper says we lost. measure it, don't vibe it.

---

*maintained by @jeffrey — companion to `papers/VOICE.md` and `papers/arxiv-hand-and-loop`. update when the hand evolves.*
