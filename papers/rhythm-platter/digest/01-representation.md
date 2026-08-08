# 01 · Representation, necklaces, bracelets

**Shelf:** toussaint · **Sources:** `toussaint-2005-bridges`, `toussaint-2005-jcdcg`, `toussaint-2010-cgta`, `keith-1991`

## The cycle

A rhythm is `k` onsets placed on `n` equally-spaced pulses of a cycle. Four
interchangeable encodings, all of which the tooling needs:

| Encoding | Example (tresillo) | Use |
|---|---|---|
| Box / TUBS | `x..x..x.` | human reading, docs |
| Binary | `10010010` | canonical forms, string ops |
| Onset set | `[0, 3, 6]` | scheduling, geometry |
| Inter-onset intervals (IOI) | `(3, 3, 2)` | evenness, chronotonic distance |

Geometrically: `n` points equally spaced on a circle, `k` of them marked. The
marked points are the vertices of an inscribed polygon. Every property in this
platter is a property of that polygon.

The IOI sequence is cyclic and must sum to `n` — the last interval wraps from the
final onset back to the first. Off-by-one here is the most common bug in
hand-rolled rhythm code, and it silently produces a pattern one pulse short.

## Necklaces and bracelets

- A **necklace** is an equivalence class of rhythms under **rotation** (cyclic
  shift). Son clave started on its third onset is the same necklace as son clave.
- A **bracelet** is an equivalence class under **rotation or reflection** (the
  dihedral group). A rhythm and its reversal are the same bracelet.

This is the platter's most load-bearing idea and the one most often skipped in
code. Toussaint's catalogue **prints one arbitrary rotation per entry**, and many
traditional rhythms are explicitly named as *a rotation of* the Euclidean form
("the actual Bossa-Nova rhythm usually starts on the third onset"). Comparing raw
strings therefore reports false negatives across half the catalogue.

**Canonical representative.** The lexicographically least rotation of the binary
string. Two rhythms are the same necklace iff their representatives are equal.
For bracelets, take the least over rotations of both the string and its reversal.
This is the only correct equality test.

## Counting

The number of binary necklaces of length `n` with `k` ones:

```
N(n, k) = (1/n) · Σ_{d | gcd(n,k)} φ(d) · C(n/d, k/d)
```

Bracelets require Burnside over the dihedral group and the closed form splits on
the parity of `n`. **Implement bracelets by orbit enumeration, not by a formula**
— the enumeration is cheap at the sizes music uses (`n ≤ 32`) and the closed form
is easy to get subtly wrong.

## Why it matters musically

Rotation is the safest compositional lever there is: every property in this
platter except off-beatness is rotation-invariant. You can precess a pattern
indefinitely and it never becomes less even, less deep, or less odd — only its
phase relationship to its neighbours changes. `pop/minitek/c/hypnotek.c` already
exploits exactly this.

Reflection is the untested one. It preserves every listed property too, but a
rhythm and its retrograde plainly do not sound the same. That gap — a formal
equivalence with no perceptual counterpart — is what the `pop/bracelet/` lane
exists to probe. See [08](08-spatial-appendix.md).

## tools

- `to_onsets(box|binary) -> int[]`, `to_box`, `to_binary`, `to_ioi`, `from_ioi`
  — with a hard assertion that IOIs sum to `n`.
- `rotate(rhythm, r)` — cyclic shift, positive = later.
- `reflect(rhythm, axis = 0)` — reverse the cycle about a chosen pulse.
- `necklace_canonical(rhythm) -> string`, `bracelet_canonical(rhythm) -> string`.
- `same_necklace(a, b) -> bool`, `same_bracelet(a, b) -> bool` — the equality
  tests every other function should route through.
- `necklace_count(n, k)`, `bracelet_count(n, k)` (enumeration-based).
- `enumerate_necklaces(n, k) -> string[]` — the search space for a generator.
