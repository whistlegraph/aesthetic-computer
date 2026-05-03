# Jeffrey Platter

An index of canonical photographic references for **Jeffrey Alan Scudder** — face, body, hands — across the repo, the assets CDN, the vault, and the social silos. A sub-platter within the [papers platter](../SCORE.md), parallel to [whistlegraph-platter](../whistlegraph-platter/).

> The platter exists so that any pipeline (CV layout, image generation, press kit, lecture slide, video composite) can resolve "a photograph of jeffrey" to a concrete URL or file path with known POIs, focal points, and provenance — instead of reaching for whatever's nearby.

This is **index-only**: pointers to where canonical images live. Masters live on the assets CDN and in the vault. No binary duplication into `papers/`.

---

## 1. In this repo

### Pieces (capture surfaces)

These are the AC pieces that produce new photographic frames of jeffrey when he uses them:

- [system/public/aesthetic.computer/disks/selfie.mjs](../../system/public/aesthetic.computer/disks/selfie.mjs) — *Selfie* — decorated photographic selfie palette (designed with @mollysoda)
- [system/public/aesthetic.computer/disks/snap.mjs](../../system/public/aesthetic.computer/disks/snap.mjs) — *Snap* — still photo capture, saves to painting
- [system/public/aesthetic.computer/disks/cap.mjs](../../system/public/aesthetic.computer/disks/cap.mjs) — *Cap* — video capture (caps/tapes), portrait-native
- [system/public/aesthetic.computer/disks/camera.mjs](../../system/public/aesthetic.computer/disks/camera.mjs) — base camera piece
- [system/public/aesthetic.computer/disks/phand.mjs](../../system/public/aesthetic.computer/disks/phand.mjs) — *Phand* — peter-hand palette (hand reference frames)

### POI manifest (single source of truth)

- **[manifest.json](manifest.json)** — POI manifest for both CDN buckets (8 headshots + 38 candids). Each item carries `focal: [x%, y%]`, `pois: [{t: f|b|h, box}]`, and `aspect`. POIs originally detected via OpenCV DNN + Haar cascades.
- **[sync.mjs](sync.mjs)** — copies `manifest.json` to consumer-served paths. Run after editing the manifest. Currently writes one target: `system/public/give.aesthetic.computer/jeffreys-manifest.json`.

### Consumers (where canonical jeffrey-images are already used)

- [system/public/justanothersystem.org/cv.html](../../system/public/justanothersystem.org/cv.html#L485-L488) — bio portrait slideshow with Ken Burns; pulls `jeffery-av--07/06/10.jpg` from the `shoot/` bucket. OG/Twitter card image is `jeffery-av--07.jpg`. *Hardcoded URLs; doesn't read the manifest yet (TBD).*
- [system/public/give.aesthetic.computer/index.html](../../system/public/give.aesthetic.computer/index.html) — full Jeffreys Ken Burns canvas slideshow. Fetches `./jeffreys-manifest.json` at runtime via `loadJeffreysManifest()` and merges into the slideshow's image index (`buildImageIndex()`). Originally inlined the same data as JS object literals; lifted to JSON 2026-04-28.
- [recap/](../../recap/) — recap pipeline uses the **`jeffrey-pvc` Professional Voice Clone** via `/api/say`. Audio analogue of canonical-jeffrey: not visual but lives in the same canonical-self category. See [recap/SCORE.md](../../recap/SCORE.md).

### Adjacent textual / video material

- [papers/cv/cv.tex](../cv/cv.tex), [papers/cv/cv.pdf](../cv/cv.pdf) — formal CV (text + dates)
- [grants/lacma-2026/](../../grants/lacma-2026/) — LACMA pitch package; uses jeffrey-pvc narration + 6 photos from `jeffreys/jpg/` (per `SESSION-LOG.md` line 195)
- TBD: lecture recordings cataloged under [papers/lectures/](../lectures/) — cross-reference any with on-camera jeffrey

---

## 2. External (assets CDN + sites)

### Hosted dashboard — `papers.aesthetic.computer/platter/jeffrey/`

Public sortable/filterable gallery of the 457 confirmed-jeffrey IG selfies from `@whistlegraph` (face-matched against the AV shoot, described per-image by GPT-4o). Lives under the new `/platter/` namespace alongside future per-platter dashboards:

- **Dashboard:** https://papers.aesthetic.computer/platter/jeffrey/ (static `index.html`, listed on the [platter index](https://papers.aesthetic.computer/platter/) as "Jeffrey's Instagram")
- **Source HTML:** [system/public/papers.aesthetic.computer/platter/jeffrey/index.html](../../system/public/papers.aesthetic.computer/platter/jeffrey/index.html)
- **Manifest JSON:** [system/public/papers.aesthetic.computer/platter/jeffrey/manifest.json](../../system/public/papers.aesthetic.computer/platter/jeffrey/manifest.json) (slim per-row schema with CDN thumb URL)
- **Thumbnails:** `assets.aesthetic.computer/jeffreys/whistlegraph/<shortcode>.jpg` (256px, ~30 KB each, ~5 MB total)
- **Build script:** [`portraits/jeffrey/bin/build-thumbnails.mjs`](../../portraits/jeffrey/bin/build-thumbnails.mjs) — reads `portraits/jeffrey/curated/jeffrey-described.jsonl`, generates thumbs into `system/public/assets/jeffreys/whistlegraph/`, and rewrites `manifest.json`.
- **Refresh:** `node portraits/jeffrey/bin/build-thumbnails.mjs && npm run assets:sync:up`

Page is **public** — the source images were already public on `instagram.com/whistlegraph/`, and the GPT-4o descriptive layer is treated as part of the public AC research record.

### `assets.aesthetic.computer/jeffreys/`

Hosted on Digital Ocean Spaces. Sync via `npm run assets:sync:down` / `npm run assets:sync:up` (see [CLAUDE.md](../../CLAUDE.md)).

#### `jeffreys/shoot/` — Professional AV photoshoot (face-focused headshots)

**55 headshots** named `jeffery-av--01.jpg` through `jeffery-av--55.jpg`, uploaded 2026-01-05. Bucket audited 2026-04-28 via `aws s3 ls`; all 55 are present and cataloged in [manifest.json](manifest.json) under `buckets.shoot.items`.

**Resolution tiers** (sizes vary 10x within the sequence — uploads were sized for different consumers):

| Tier     | Range          | Size        | Use                                            |
| -------- | -------------- | ----------- | ---------------------------------------------- |
| `master` | `--01..--10`   | 20–26 MB    | Training crops, print, archival                |
| `mid`    | `--11..--35`   | 1.7–5.3 MB  | Slideshows, web reference                      |
| `web`    | `--36..--55`   | 220–340 KB  | Thumbnails / smallest backgrounds              |

Per-tier annotation lives in each item's `tier` and `size` fields. All headshots share uniform POI framing: `aspect: 0.667`, `focal: [50, 35]`, `pois: [{t:"f", box:[30,15,40,40]}]` — face-centered portrait. The framing is a placeholder; a re-run of OpenCV face detection on `master`-tier frames would yield true bounding boxes.

Note the spelling: filenames use `jeffery-` (one r), not `jeffrey-`.

**Currently consumed**:
- give.aesthetic.computer Ken Burns slideshow — re-enabled 2026-04-28; merges all 55 into `allImagesData` (was previously commented out — only 8 cataloged then)
- justanothersystem.org cv.html — hardcoded URLs for `--06/07/10` (OG card uses `--07`)

#### `jeffreys/<NAME>.{heic,HEIC,jpeg,JPEG}` — iPhone master files

**38 masters** sit at the top of the `jeffreys/` prefix (not in a subdirectory), 1:1 with `jpg/` candids. These are the iPhone-original HEIC/JPEG files — the JPGs in `candids/` are derivatives. Cataloged in [manifest.json](manifest.json) under `buckets.masters.items` with `candid_key` cross-references and per-file size.

Use this bucket for **training crops** — masters carry roughly 2× the byte-budget of their JPG derivatives and preserve color depth that JPEG re-encoding lossily compresses. Don't surface masters in slideshows (HEIC has spotty browser support); use the `candids/` JPGs for those.

Mixed extensions in the bucket: 32 are `heic`/`HEIC`, 6 are `jpeg`/`JPEG`. Manifest keys preserve the actual extension on the CDN.

#### `jeffreys/jpg/` — Candids (JPG derivatives of the masters)

38 enumerated candids (`IMG_NNNN.jpg` + `FullSizeRender`) with hand-tuned focal points and POI bounding boxes. Full data lives in [manifest.json](manifest.json) under `buckets.candids.items`. Includes:

`FullSizeRender`, `IMG_0260`, `IMG_0675`, `IMG_0686`, `IMG_0688`, `IMG_0798`, `IMG_1111`, `IMG_1577`, `IMG_1616`, `IMG_1737`, `IMG_1809`, `IMG_2124`, `IMG_2208`, `IMG_2280`, `IMG_2498`, `IMG_2630`, `IMG_2658`, `IMG_2668`, `IMG_2905`, `IMG_2913`, `IMG_3017`, `IMG_3234`, `IMG_4281`, `IMG_4312`, `IMG_4606`, `IMG_4894`, `IMG_4997`, `IMG_5043`, `IMG_5050`, `IMG_5272`, `IMG_5644`, `IMG_6342`, `IMG_6367`, `IMG_6435`, `IMG_8080`, `IMG_8188`, `IMG_8989`, `IMG_9795`.

Bucket audited 2026-04-28 — exactly 38 entries, manifest is complete, no orphans, no uncataloged files. Each candid has a `master` field pointing at its HEIC/JPEG counterpart in `jeffreys/`; prefer the master for training, use the JPG for slideshows and embeds.

POI types: `f` = face, `b` = body, `h` = hand. Aspects vary (0.562 / 0.563 / 0.667 / 0.75 / 0.8 / 1.333). `IMG_2124` and `IMG_2658` are referenced by name in [give.aesthetic.computer/index.html](../../system/public/give.aesthetic.computer/index.html) as the splash/background plates.

TBD: locate the script that generated the existing focal/POI values (OpenCV DNN + Haar cascades, per the give-page comment) and check it in to `portraits/jeffrey/bin/`. The manifest carries the *output* of that pipeline; the pipeline itself isn't in the repo yet.

#### `screenshots/images/` — First-person POV "selfies" (90 environmental webp captures)

90 webp screenshots of jeffrey's working environment — laptop screens, hands on the keyboard, desk surfaces, ambient development moments. Captured live while making AC. Treated as canonical **first-person POV reference imagery** for any pipeline that wants a POV-from-jeffrey look (vs the third-person headshots in `shoot/`). Mostly portrait orientation (`aspect: 0.75`), each item has a `focal` coordinate and `pois` boxes (`t: "s"` = scene region).

URL pattern: `https://assets.aesthetic.computer/screenshots/images/<name>.webp`. Cataloged in [manifest.json](manifest.json) under `buckets.screenshots.items` (imported from `give.aesthetic.computer/index.html` 2026-05-02). Currently consumed by the give Ken Burns slideshow (merged into `allImagesData`) and available to image-gen pipelines via `--refs`.

#### `jeffreys/gens/` — Generated images (gpt-image-2 + platter-grounded identity)

Output bucket for any pipeline that synthesizes a new jeffrey-image conditioned on the platter refs (typically `SHOOT_REFS` + `SELFIE_REFS` from [`portraits/jeffrey/bin/generate-neo.py`](../../portraits/jeffrey/bin/generate-neo.py)). One PNG per successful gen, dated, never overwritten.

**Filename convention:** `<context>-<segment>-<YYYY-MM-DDTHHMM>.png` — e.g. `recap-jeffrey-24h-02_menuband-2026-04-29T1630.png`. `context` says where the gen came from (e.g. `recap-jeffrey-24h`); `segment` is whatever locally-meaningful subdivision the producer cares about; the timestamp keeps regens distinct so we never lose a previous take.

**Producers:**
- [`recap/bin/jeffrey-photos.mjs`](../../recap/bin/jeffrey-photos.mjs) — auto-archives every gen here as part of the recap pipeline. Tone defaults to real+goofy candid (per memory).
- [`portraits/jeffrey/bin/generate-neo.py`](../../portraits/jeffrey/bin/generate-neo.py) — currently writes one-offs to `~/Desktop`; promote to this bucket when a take is worth keeping.

**Manifest entry shape** (under `buckets.gens.items[<filename>]`):

```json
{
  "model": "gpt-image-2",
  "size": "1024x1536",
  "quality": "high",
  "refs": ["portraits/jeffrey/corpus/shoot/jeffery-av--07.jpg", "..."],
  "context": "recap-jeffrey-24h",
  "segment": "02_menuband",
  "generated": "2026-04-29T16:30:00.000Z",
  "bytes": 2520266,
  "prompt": "<full gpt-image-2 prompt — preserved for provenance>"
}
```

The full prompt is kept inline so a year from now we can answer "what was the metaphor that produced this take?" without cross-referencing the source audience config (which has likely drifted by then).

**Push to CDN:** `npm run assets:sync:up` after a batch of gens. The bucket is under the standard `system/public/assets/*` gitignore — git tracks the manifest entry, not the PNG bytes.

### Social silos (canonical public faces)

- **Instagram (jeffrey solo, post-2023)** — https://www.instagram.com/whistlegraph/
- **Instagram (AC)** — https://www.instagram.com/aesthetic.computer/ — silo target account
- **TikTok @whistlegraph** — https://www.tiktok.com/@whistlegraph (~2.7M peak)
- **YouTube** — https://www.youtube.com/channel/UCZ_5AuCebRbm9t9_Y7SrckQ
- **X / Twitter @whistlegraph** — https://x.com/whistlegraph
- TBD: jas.life — closed-source / private; never surfaced through this index. Route any jas.life-sourced material through the vault, not the public CDN.

### Institutional pages carrying jeffrey portraits

- **KADIST artist page** — https://kadist.org/people/jeffrey-alan-scudder/ — TBD: confirm portrait usage and licensing
- **Schneider Museum of Art** — https://sma.sou.edu/whistlegraph/ — TBD: pull headshots used in event listings
- **Rhizome / New Museum** *First Look* — TBD: 2022-05-14 debut, may carry press portraits
- **Feral File** — https://feralfile.com/artists/Whistlegraph — TBD: artist page may carry a headshot

---

## 3. Local bulk archive (gitignored)

Bulk image data lives at [`portraits/jeffrey/`](../../portraits/jeffrey/) under gitignored subdirectories — these aren't secrets (vault is for SECRET_FILES only), just bulky regenerable cache:

- **[`portraits/jeffrey/ig-archive/<account>/`](../../portraits/jeffrey/ig-archive/)** — Instagram bulk dumps. As of 2026-04-29: `whistlegraph/` ≈ 4.1 GB, 6,104 jpgs spanning 2014-08-23 → 2026-04-28 (posts + highlights + active stories). Pulled via `portraits/jeffrey/bin/ig-archive.fish`.
- **[`portraits/jeffrey/curated/`](../../portraits/jeffrey/curated/)** — outputs of the face-match → vision-describe pipeline. `jeffrey-match.jsonl` (insightface identity scores), `jeffrey-described.jsonl` (GPT-4o scene-graph metadata), `thumbnails/` (384px), `index.html` (static browser).
- **TBD: `portraits/jeffrey/selfies/`** — selfie corpus (raw output from `selfie.mjs`/`snap.mjs`/`cap.mjs` runs); promote the keepers to `assets/jeffreys/jpg/` after curation.
- **TBD: `portraits/jeffrey/press/`** — press headshots and high-res stills sent to institutions.
- **TBD: `portraits/jeffrey/shoot-raw/`** — uncompressed masters from the AV photoshoot (only web-sized JPGs are on the CDN today).

The only jeffrey-related thing that lives in the vault is the **instaloader session cookie** at `aesthetic-computer-vault/silo/instaloader-sessions/whistlegraph` — that one IS a credential (authenticated IG session) and stays vault-side.

---

## 4. Instagram archive ingestion (currently BLOCKED)

The silo Instagram bridge is the path to bulk-importing jeffrey's solo + AC photo archives into the platter.

- **Bridge code**: [silo/server.mjs](../../silo/server.mjs) on the silo VPS, port 3003
- **Migration report**: [reports/instagram-api-migration-2026-03-29.md](../../reports/instagram-api-migration-2026-03-29.md)
- **Status (2026-03-29)**: login rejected on `instagram-private-api`, `@i7m/instagram-cli`, and `subzeroid/instagrapi-rest`. Errors range from `IgLoginBadPasswordError` to "IP added to the blacklist". Migration to `instagrapi 2.3.0` (Python, actively maintained) sketched but not landed.
- **Block resolution path**: bypassed 2026-04-29 — instaloader cookie-import (Chrome) succeeded; bulk archive of @whistlegraph (4.1 GB, 6,104 frames, 2014–2026) lives at [`portraits/jeffrey/ig-archive/whistlegraph/`](../../portraits/jeffrey/ig-archive/). Silo bridge migration is independent and still pending — only needed if the silo dashboard needs live IG profile/feed queries.

When ingestion comes online, image filenames should map IG shortcodes to date-prefixed names (`YYYY-MM-DD-<shortcode>.jpg`) so timeline queries are easy. POI re-detection is a separate post-ingest pass.

---

## 5. For canonical image generation (the goal)

The motivating use case for this platter is to feed image-generation pipelines a stable, well-annotated reference set of jeffrey so generated portraits stay on-model.

**Pipeline location: [`portraits/jeffrey/`](../../portraits/jeffrey/)** at the monorepo root. It consumes [manifest.json](manifest.json) and the CDN buckets; it is the agreed home for training scripts, LoRA artifacts, IP-Adapter ref sets, and any sample outputs.

**Currently in place**

- A two-bucket archive (`shoot/` headshots + `jpg/` candids) hosted on the CDN
- Per-image POI metadata (face/body/hand bounding boxes + focal point + aspect) for 46 images in [manifest.json](manifest.json). Lifted out of `give.aesthetic.computer/index.html` 2026-04-28.
- Voice analogue (`jeffrey-pvc` PVC) already wired into `/api/say` — establishes the pattern of canonical-self assets being callable as a service

**Gaps to close**

- TBD: agree on what "canonical" means for generation:
  - Face LoRA / DreamBooth training corpus — needs ≥20 high-quality face crops with diverse lighting/angles. The 8 `shoot/` headshots are too uniform on their own (all `focal: [50, 35]`); needs candids mixed in.
  - IP-Adapter / FaceID reference set — needs a smaller curated set of 5–10 very clean references.
  - Identity-preserving prompt suffix — needs a stable text description of jeffrey's appearance, derived from photos. Adjacent to but distinct from voice clone metadata.
- TBD: licensing & consent rails — even though jeffrey is the subject and operator, downstream pipelines that publish generated portraits should have an explicit allowlist of where they're allowed to render him.

**Adjacent infrastructure to study**

- [arxiv-penrose/](../arxiv-penrose/) — Penrose pipeline for AC illustrations. Not for portraits, but the "diagrams from data" pattern (declarative spec → rendered image) is a useful analogue.
- [recap/bin/scout.mjs](../../recap/bin/scout.mjs) — content-query resolver. Could be extended with a `{ jeffrey: "shoot" | "candid" | "any", count: N }` query that returns N canonical jeffrey URLs for use in slide compositions.

---

## 6. Timeline (rough spine)

- **2026-01-03 to 2026-01-05** — candids and HEIC masters uploaded to `jeffreys/` and `jeffreys/jpg/` (38 files each); AV shoot uploaded to `jeffreys/shoot/` (55 frames) on 2026-01-05
- **2026-01–04** — POI manifest authored by hand inside `give.aesthetic.computer/index.html`; consumed by `cv.html` (justanothersystem.org) and the Ken Burns slideshow
- **2026-03-29** — silo IG migration attempted, blocked at login layer ([report](../../reports/instagram-api-migration-2026-03-29.md))
- **2026-04-28** — platter index created; POI manifest lifted from `give.aesthetic.computer/index.html` to standalone [manifest.json](manifest.json); give-page now fetches the synced copy at runtime; bucket audit added `masters/` bucket and `tier`/`size`/`master` annotations; headshots merge re-enabled in give-page; `portraits/jeffrey/` scaffolded; silo `instagrapi.service` staged
- **TBD: candid corpus origin** — `IMG_NNNN.jpg` filenames are iPhone-default; the masters carry EXIF dates that should be parsed and added to the manifest as `taken_at`

---

## How to extend this index

1. When new material appears (a new shoot, a new selfie session keeper, a press photo placement, an ingested IG batch) — add a line under the matching section with a link or asset path.
2. Use `TBD:` prefix for known-missing items so they're greppable.
3. Don't check binaries into `papers/`. Masters live on the CDN or in the vault; this file points at them.
4. When the POI manifest gets extracted from `give.aesthetic.computer/index.html` into a standalone JSON, link it here and update §2 to reflect the new source of truth.
5. If a sibling `manifest.json` or `pois/` directory grows, note it in §1 (consumers) so other pipelines can find it.
