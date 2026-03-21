# Study: $roz Function Profile for KidLisp Decree '26

**Date:** 2026-03-09  
**Scope:** Inventory the exact functions/tokens used by `$roz` and encode them into `KidLisp Decree '26`.

---

## Source Snapshot

Lookup:
- `GET https://aesthetic.computer/api/store-kidlisp?code=roz`

Snapshot metadata at analysis time:
- `handle`: `@jeffrey`
- `when`: `2025-08-25T05:49:46.495Z`
- `hits`: `6258`

Snapshot source:
```lisp
fade:red-blue-black-blue-red
ink (? rainbow white 0) (1s... 24 64)
line w/2 0 w/2 h
(spin (2s... -1.125 1.125)) (zoom 1.1)
(0.5s (contrast 1.05))
(scroll (? -0.1 0 0.1) (? -0.1 0 0.1))
ink (? cyan yellow magenta) 8
circle w/2 h/2 (? 2 4 8)
```

---

## AST-Extracted Inventory

AST heads/tokens observed:
- `fade:red-blue-black-blue-red`
- `ink`
- `?`
- `1s...`
- `line`
- `spin`
- `2s...`
- `zoom`
- `0.5s`
- `contrast`
- `scroll`
- `circle`

Symbol atoms observed:
- `w`, `h`
- `w/2`, `h/2`

Color tokens observed:
- `red`, `blue`, `black`, `white`
- `rainbow`
- `cyan`, `yellow`, `magenta`

---

## Semantics Required by $roz

`$roz` requires, at minimum:
- choice operator semantics: `?`
- time expression semantics:
  - repeating timed form (`1s...`, `2s...`)
  - non-repeating timed form (`0.5s`)
- transform/effect semantics:
  - `spin`
  - `zoom`
  - `contrast`
  - `scroll`
- primitive rendering semantics:
  - `line`
  - `circle`
  - `ink`
- dimension aliases and division shorthand:
  - `w`, `h`
  - `w/2`, `h/2`
- fade shorthand token parsing:
  - `fade:<color>-<color>-...`
- dynamic color generation:
  - `rainbow`

---

## Decree Integration Recommendation

Add a decree profile:
- **`RBP-26`** = `$roz Baseline Profile (KidLisp Decree '26)`

Conformance statement:
- Runtime MAY claim `KidLisp Decree '26: RBP-26` only if all required semantics above pass profile tests.

Benefits:
- Gives native bring-up a concrete target before full language parity.
- Establishes a recognizable AC reference profile tied to a high-use piece (`$roz`).
- Lets hosts be honest about partial support while still shipping meaningful compatibility.

---

## Native Bring-Up Checklist for $roz

1. Parse and evaluate `$roz` source without fallback/stub behavior.  
2. Verify `?` emits varied values for color and geometry choices.  
3. Verify `1s...` and `2s...` repeating cadence behavior.  
4. Verify `0.5s` delayed single-trigger behavior.  
5. Verify `spin + zoom + contrast + scroll` affect the same composited frame path.  
6. Verify `w/2` and `h/2` resolve against current screen dimensions.  
7. Verify fade shorthand resolves to visible gradient-style color behavior.  
8. Verify color tokens and `rainbow` map consistently across hosts.

