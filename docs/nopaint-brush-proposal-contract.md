# No Paint compatible brush proposal contract

No Paint is a constrained piece/brush surfer. A compatible AC brush owns the
parameter schema, seeded score generation, and proposal rendering used by the
surfer. The conductor owns selection, No/Paint decisions, persistence, and the
accepted-painting composite.

Compatibility is explicit and allowlisted. Exporting an ordinary `meta()` or
using `system = "nopaint"` does not automatically make a stateful brush safe to
run as a proposal.

## Version 1 contract

`line.mjs` is the first implementation and exports `nopaintProposal`:

- `version`: contract version.
- `slug` and `label`: stable identity and human HUD label.
- `compatible: true`: explicit opt-in.
- `parameters`: machine-readable bounds and types.
- `generate({ random, width, height, base })`: returns a frozen, deterministic
  score containing explicit params, colon modifiers, geometry, and duration.
- `render(api, score, frame)`: renders only into the active proposal buffer.

The frozen score records the invocation that produced the pixels:

```js
{
  brush: {
    slug: "line",
    params: [r, g, b, alpha],
    colon: [thickness],
    parameters: { thickness, alpha, pointCount, durationFrames }
  },
  points: [{ x, y }, ...]
}
```

## Safety invariants

1. Selection comes only from the conductor's static compatibility allowlist.
2. Generation uses the session's seeded PRNG; ambient randomness is forbidden.
3. Every generated coordinate and parameter stays within the brush-owned
   schema.
4. Rendering cannot persist, broadcast, bake, or mutate the accepted painting.
5. No clears the proposal buffer. Paint alone composites and persists it.
6. Stateful legacy brushes require an adapter or a brush-owned proposal export;
   `meta()` prose is not executable capability metadata.

The existing Robo synthetic-pen pipeline remains the migration path for older
pointer-driven brushes. It must be isolated per proposal and must never invoke
their normal bake/broadcast path before Paint.
