# conformance — the KidLisp pixel oracle

Renders the corpus (the top pieces by live hits, `corpus.json`) from a checkout
and compares it against another, so a KidLisp change is judged by what it draws.
This is the gate every agent PR runs; its sheets go in the PR body.

```bash
node kidlisp/conformance/oracle.mjs check                 # this checkout vs origin/main (~8 min, exit 1 = broke)
node kidlisp/conformance/oracle.mjs check --against HEAD  # vs your last commit
node kidlisp/conformance/oracle.mjs refresh --top 40      # re-pull the corpus from prod
node kidlisp/conformance/oracle.mjs render --out DIR [--repo PATH | --base URL] [--only bop,pie] [--at 1500,4000] [--video]
node kidlisp/conformance/oracle.mjs compare BEFORE AFTER --out DIR [--noise AGAIN]
```

`check` boots `lith/server.mjs` for both trees (no env or db needed; `$code`
lookups are answered from prod by the oracle), renders the base twice and the
change once, and writes `before/`, `noise/`, `after/` and `diff/` (with
`compare.md` and a before/after sheet per flagged piece).

## verdicts

| verdict | meaning | fails? |
|---|---|---|
| `same` | within the piece's own noise | no |
| `drift` | moved a little past its noise | no, sheet for the eye |
| `changed` | moved a lot; may be a chaotic piece being itself | no, sheet for the eye |
| `blank` | went flat and far from before | **yes** |
| `gone` | a steady piece became a different picture | **yes** |
| `error` | a console error the base didn't throw | **yes** |

## why it isn't pixel-exact (yet)

KidLisp seeds `?` from `Date.now()` and runs in a worker the harness can't
clock, so the same code draws different frames run to run. The oracle judges by
64-bin colour histograms against each piece's measured self-noise. A
deterministic mode (seeded `?`, frame-stepped time, behind a URL flag) would
make this exact, and is the first roadmap item it wants.

## gotchas

- `kidlisp.mjs` is bundled into the disk worker. An edit renders nothing new
  until `cd system && npm run build:disk-worker`, and the bundle + manifest
  must be committed with it.
- Clocks start at `window.acBOOTED`, not navigation: boot swings by seconds
  under load. Each piece gets its own browser context (shared service worker
  breaks parallel boots) and background throttling is off.
- Local network noise (no db, no session server) is filtered out of errors.
- Proven on 2026-09-23: identical trees pass (38 same, 2 chaotic flagged);
  a tree with `wipe` stubbed fails (4 blank, 1 gone).
