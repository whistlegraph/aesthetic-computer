# Portraits / Jeffrey

Image-generation pipeline for canonical jeffrey portraits. Consumes the POI manifest at [`papers/jeffrey-platter/manifest.json`](../../papers/jeffrey-platter/manifest.json) and the `assets.aesthetic.computer/jeffreys/` CDN buckets cataloged there.

> The platter is the index (read-only pointers). This directory is the workshop (training, generation, sample outputs). The visual analogue of [`recap/`](../../recap/) for `jeffrey-pvc` voice — same canonical-self pattern, applied to faces instead of audio.

## Status

Scaffolded 2026-04-28. The corpus fetcher is wired; everything downstream of "have a local corpus" is TBD.

## Pipeline (planned)

```
papers/jeffrey-platter/manifest.json
       │
       ▼  bin/fetch-corpus.mjs
corpus/<bucket>/<filename>            (gitignored — downloaded from CDN)
corpus/index.json                     (manifest + fetch metadata)
       │
       ▼  TBD: bin/face-crops.mjs     (apply manifest POI face boxes to crop training tiles)
crops/face/*.jpg                      (gitignored — N×N face tiles for LoRA/IP-Adapter)
       │
       ▼  TBD: training (LoRA / DreamBooth / IP-Adapter ref set — pick one)
weights/jeffrey-{lora|ipadapter}.safetensors
       │
       ▼  TBD: bin/generate.mjs       (sample portraits given a prompt)
out/<run-id>/                         (gitignored — generated samples)
```

## Commands

### Pull canonical corpus (CDN → local)

```bash
node bin/fetch-corpus.mjs              # all three buckets (shoot + masters + candids)
node bin/fetch-corpus.mjs --shoot      # 55 AV-shoot headshots only
node bin/fetch-corpus.mjs --masters    # 38 HEIC/JPEG iPhone originals (best for training)
node bin/fetch-corpus.mjs --candids    # 38 JPG derivatives
node bin/fetch-corpus.mjs --force      # re-download even if local copies exist
```

The fetcher reads `papers/jeffrey-platter/manifest.json` directly — there is no separate config. To add a new image to the corpus, add it to the manifest and re-run.

### Bulk-archive Instagram

Two paths to bootstrap an instaloader session — pick whichever your account allows:

```fish
# Path A (recommended): import session from a logged-in browser
#   - Works around 2FA-flagged accounts where the password API path
#     silently strips the auth cookie (this is what bit @whistlegraph)
#   - One Keychain prompt on first Chrome run, then silent thereafter
bin/ig-import-cookies.py chrome whistlegraph
bin/ig-import-cookies.py firefox aesthetic.computer  # or chrome / brave / arc / edge / safari

# Path B (fallback): login with password
#   - Only works on accounts without 2FA / checkpoints
IG_PASSWORD='...' bin/ig-login.py whistlegraph

# Then archive the full timeline (de-dupes via --fast-update on re-runs)
bin/ig-archive.fish whistlegraph
bin/ig-archive.fish aesthetic.computer
```

Sessions persist at `aesthetic-computer-vault/silo/instaloader-sessions/<account>` (the only piece that stays in vault — it's an authenticated cookie). Archives land at `portraits/jeffrey/ig-archive/<account>/` (gitignored). Re-running `ig-archive.fish` only fetches new posts (by shortcode); safe to cron.

**On account safety**: Instagram's anti-bot detection has flagged @whistlegraph before (see [reports/instagram-api-migration-2026-03-29.md](../../reports/instagram-api-migration-2026-03-29.md)). The cookie-import path is safer than password-flow because it piggybacks on a session Instagram has already accepted in your browser — there is no fresh login event for the anti-bot system to evaluate.

## Social graph / art-world map

A second layer on top of the faces platter: from an account's 1st-degree
Instagram graph, **map the contemporary art gallery world it sits in** —
galleries, museums, institutions, artist-run/project spaces, art media &
fairs, and *the people who run or work at them* (gallerists, directors,
curators, staff). Art-world nodes are prioritized; personal fans are
demoted to a faint background. Outputs live at
`portraits/jeffrey/social/<account>/` (gitignored — authenticated personal
social data, same treatment as `ig-archive/`).

Six stages — only stages 1 & 3 touch Instagram, and both stop dead on the
first IG throttle rather than hammering (the enumeration that soft-locked
@whistlegraph before) while self-healing across local network drops:

```bash
# 1. fetch the cheap node lists (followers + following), one paged pass.
#    Resumable, streams to disk; re-run after a cooldown to continue.
bin/ig-social-graph.py whistlegraph

# 2. classify offline — taxonomy + a Phase-3 work order. Zero network.
node bin/ig-artworld-classify.mjs whistlegraph

# 3. enrich an explicit, audited art-world target list with Instagram's own
#    category + bio (→ who runs/works at what). --from-list keeps the target
#    set reviewable; hard --cap, slow, resumable, throttle-stopped.
node bin/ig-artworld-classify.mjs whistlegraph    # writes candidates.json
#    (build enrich-targets.txt = art-world + verified/signal, zero pure fans)
bin/ig-social-enrich.py whistlegraph --from-list enrich-targets.txt \
    --cap 600 --sleep-floor 5

# 4. corroborate against Wikidata/Wikipedia — catches art-world identity that
#    NEITHER the heuristic NOR IG's category reveal (empty-bio / plain-name
#    handles). Web-only (zero IG load), key-less, cached, resumable. NOT
#    Google/DDG: those bot-challenge (HTTP 202) within a few queries.
node bin/ig-web-corroborate.mjs whistlegraph

# 5. assemble graph.json (typed nodes + edges). Zero network.
node bin/ig-social-edges.mjs whistlegraph

# 6. the deliverable: literate REPORT.md + sortable artworld.csv + a
#    REPORT.tex compiled to REPORT.pdf with xelatex (repo papers TeX, no
#    pandoc). Tables/prose, not a chart. Zero network, re-runnable.
node bin/ig-social-report.mjs whistlegraph

# 6.5 VALIDATION GATE — fails loud (exit 1) on LaTeX escaping artifacts,
#      count mismatches, structural breakage, or xelatex fatal errors.
#      Run pre-compile; --post also checks the log + PDF. This exists
#      because broken "\textbackslash{}textbf" output shipped once unseen.
node bin/ig-social-validate.mjs whistlegraph            # pre-gate (blocks compile)
( cd social/whistlegraph && PATH=/Library/TeX/texbin:$PATH \
    xelatex -interaction=nonstopmode REPORT.tex && xelatex -interaction=nonstopmode REPORT.tex )
node bin/ig-social-validate.mjs whistlegraph --post     # post-gate (log+pdf)
```

### Asking relational vectors

`bin/ig-social-query.mjs <acct> <vector> [filters]` queries the assembled
graph (zero network): `mutuals` (following ∩ followers), `idols`
(following \ followers), `fans`, `list`, `affiliations [h]`,
`also-follows <h>`, `who-follows <h>`, `node <h>`; filters `--art --type
--relation --min-reach --limit --csv`. `mutuals` needs the followers pull
(`ig-social-graph.py <acct> --only=followers`). `also-follows`/`who-follows
<h>` need a **capped, opt-in third-party** pull:
`ig-social-graph.py <acct> --target <h> --cap N` → writes
`social/<acct>/targets/<h>.followers.jsonl`. **Never run two IG
enumerations concurrently** (a third-party scrape while the followers pull
is live is how the flagged account gets banned) — queue them.

Stage 6 also emits **`ONEPAGER.tex` → `ONEPAGER.pdf`**, the headline
deliverable: a one-page colour-coded *broadside* — masthead, lede, the
three-lens overlap panel (heuristic ∩ IG-category ∩ Wikidata, the spine =
triple-confirmed), the art world by category (top names, two-column), and
who-runs-what. The validator hard-fails the broadside unless it is **exactly
one page** and artifact-clean. The exhaustive 529-row line-by-line stays in
`artworld.csv` and the multi-page `REPORT.pdf`.

A node counts as art-world if **heuristic OR IG-category OR Wikidata**
confirms (`confirmed_by` records which); affiliations ("who runs/works at
what") merge IG-bio @mentions with Wikidata employer/member-of. **The
deliverable is the report, not a chart** — `REPORT.md` (literate summary +
ranked per-category line-by-line tables), `artworld.csv` (exhaustive,
sortable), `REPORT.pdf` (landscape longtables via xelatex). `graph.svg` is
still emitted by stage 5 as a rough data-image but was explicitly deprioritized
(generic radial bubble scatter — not analytically useful). The accounts
already pulled into `ig-archive/` (`kadistkadist`, `moca`,
`printedmatter_artbookfairs`, `petra_cortright`…) seed the classifier; extend
`SEED_TYPES` in `ig-artworld-classify.mjs` as the map sharpens.

## Open decisions

- **Training approach** — face LoRA (~20–60 face tiles, fine-tunes the model) vs IP-Adapter / FaceID (5–10 reference images, no training, identity injected at inference time) vs hybrid. Different tradeoffs on faithfulness, generation speed, base-model lock-in. Pick before scripting `bin/face-crops.mjs` since the crop format depends on it.
- **Base model** — Flux.1-dev, SDXL, SD 3.5, or something else. Affects which adapter formats apply.
- **Compute** — local on jas's mac (MPS), runpod/lambda (cloud GPU), or fal/replicate (managed). Manifest + corpus are portable; only `weights/` and `out/` are environment-specific.
- **Identity-preserving prompt suffix** — a stable text fragment that describes jeffrey's appearance for use as a generation hint. Adjacent to but distinct from voice clone metadata.
- **Consent / use rails** — even though jeffrey is the operator and subject, downstream pipelines that *publish* generated portraits should have an explicit allowlist of where they can render him.

## Why here, not under `papers/`

`papers/jeffrey-platter/` is **index-only** — pointers to where canonical material lives. This directory holds the actual machinery (scripts, weights, samples) that consumes the index. Same separation as `papers/whistlegraph-platter/` (index) ↔ `system/public/aesthetic.computer/disks/whistlegraph.mjs` (the practice surface that uses it).

## See also

- [papers/jeffrey-platter/README.md](../../papers/jeffrey-platter/README.md) — canonical platter index
- [papers/jeffrey-platter/manifest.json](../../papers/jeffrey-platter/manifest.json) — POI manifest (source of truth)
- [recap/SCORE.md](../../recap/SCORE.md) — voice analogue (`jeffrey-pvc` PVC pipeline)
- [reports/instagram-api-migration-2026-03-29.md](../../reports/instagram-api-migration-2026-03-29.md) — IG ingestion gating
