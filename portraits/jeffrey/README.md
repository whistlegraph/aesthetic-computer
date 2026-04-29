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
