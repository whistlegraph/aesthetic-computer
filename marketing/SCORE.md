# marketing/ — SCORE

Strategy + index for AC promo/marketing image & film work. Pipeline structure
and gen commands live in `README.md`; this file tracks *why* and *what's reusable*.

## Cast library — `cast/CAST.md`

**Relevance:** Across pop covers, marketing campaigns, grant films, and paper
covers we keep re-casting the same figures — jeffrey, the felt **pixsies**, the
**NELA Computer Club** crowd, the drawn **CEOs** (Gates, Zuckerberg). Re-deriving
each character's look per production wastes effort and drifts the brand. The
cast library is the single source of truth for character appearance + the
cross-production motifs (PALS laptop, Sailor Pro Gear pens, yellow bear emblem,
LED-under-felt glow, one-screen laptop geometry). Drop a character's block
straight into a gpt-image / `gen-promo` / `gen-felt` prompt for consistency.

**Use it when:** writing any new illustration/felt prompt that includes a
recurring AC character; onboarding a new production into the AC visual universe;
deciding which ensemble fits a scene. **Keep it updated** as new characters are
cast — it only stays useful if it stays current.

Consumers so far: `grants/restless-egg-2026/video/gen-felt.mjs` (felt ensemble),
`marketing/bin/gen-promo.mjs` (campaigns), pop cover prompts.

## Tooling index
- `bin/gen-promo.mjs` — gpt-image-2 campaign entrypoint (jeffrey refs on by default)
- `bin/capture-ac-platter.mjs` + `bin/build-ac-platter.py` — real AC screen captures → contact-sheet platter (laptop-screen refs for gens)
- `bin/capture-ac-native.mjs` — real ac-native WASM UI frames
- `lib/jeffrey-refs.mjs` — shared identity photos
- `cast/CAST.md` — recurring character bible
