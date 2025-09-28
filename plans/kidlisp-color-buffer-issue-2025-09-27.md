# KidLisp Color Buffer Persistence Issue (2025-09-27)

## Summary

- **Problem:**
  - In `headless.mjs`, color words (e.g., `black`, `white`, `salmon`, `gray`) are implemented as hardcoded functions that clear the buffer every frame or in a non-dynamic way.
  - This does not match KidLisp semantics, where a color as the first atom should act as a persistent buffer wipe (equivalent to `(once (wipe color))`).
  - The logic should be dynamic, handle any color, and only wipe the buffer once if the color is the first atom.
  - Ideally, the color/background logic should be reused from `kidlisp.mjs` to avoid duplication and ensure consistency.

- **Progress So Far:**
  - Identified that the current logic is wrong and inconsistent with KidLisp rules.
  - Proposed to refactor `headless.mjs` to import and use the color/background logic from `kidlisp.mjs`.
  - Plan to remove hardcoded color functions and replace with a dynamic handler.
  - Need to ensure buffer wipe only happens once per render, when a color is the first atom.

- **Next Steps:**
  1. Refactor `headless.mjs` to reuse KidLisp color logic.
  2. Remove hardcoded color functions.
  3. Implement dynamic, once-per-render buffer wipe for color words.

## TODO (2025-09-28)

- [x] Wire `headless.mjs` into the KidLisp color/background helpers so any valid color token triggers the correct wipe.
- [x] Remove the hardcoded `black`/`white`/`salmon`/`gray` API shims or route them through the shared implementation without redundant clears.
- [x] Track first-atom color wipes per render to guarantee persistence while still allowing manual wipes later in the frame.
- [x] Add a regression exercise (test or script) that renders a KidLisp program with a leading color atom and verifies subsequent frames preserve the backdrop.

---

*Logged for future reference. Ready to start fresh with a new agent as requested.*
