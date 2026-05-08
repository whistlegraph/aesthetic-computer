# marketing/

Central home for AC promo / marketing image generation.

```
marketing/
  bin/
    gen-promo.mjs            # generic OpenAI gpt-image-2 entrypoint
    capture-ac-native.mjs    # puppeteer screenshots of ac-native WASM (prompt + notepat)
  lib/
    jeffrey-refs.mjs         # shared SHOOT + SELFIE refs for jeffrey identity
  campaigns/                 # in-repo campaigns (when not staging on Desktop)
  captures/                  # local puppeteer captures (gitignored)
```

prompts and processes are tracked. outputs (`campaigns/*/gens/`,
`captures/`) are gitignored — gens are large and regen-able from
`cover-prompt.txt` plus the refs.

`gen-promo.mjs` reads a campaign directory anywhere on disk — repo or
Desktop — so a campaign can stage on `~/Desktop/<slug>/` and graduate
into `marketing/campaigns/<slug>/` once it's ready to commit.

A campaign dir holds:

```
<campaign>/
  brand-brief.md         # source-of-truth concept (human-readable)
  cover-prompt.txt       # the prompt body (required by gen-promo)
  refs/                  # optional outside-world reference images
  gens/                  # output PNGs (gen-promo writes here)
  WORKLIST.md            # the plan + open questions (human-readable)
```

## Quickstart

```bash
# generate v1 from a Desktop staging dir (jeffrey refs on by default)
node marketing/bin/gen-promo.mjs ~/Desktop/nela-now-instant

# landscape variant
node marketing/bin/gen-promo.mjs ~/Desktop/nela-now-instant \
  --size 1536x1024 --variant v2

# prompt-only (no jeffrey identity refs — for emblem-style work)
node marketing/bin/gen-promo.mjs ./marketing/campaigns/foo --no-jeffrey
```

## Sizes

`gpt-image-2` accepts `1024x1024`, `1024x1536` (portrait), `1536x1024`
(landscape). Default is portrait.

## Desktop mirror

When the campaign dir lives under `~/Desktop`, every successful gen also
gets a flat copy at `~/Desktop/<campaign>-<variant>.png` so the image is
immediately browsable from the Mac desktop without diving into the folder.

Override the mirror target with `--mirror <dir>`, or disable with
`--no-mirror`.

## Capturing real ac-native UI for refs

`bin/capture-ac-native.mjs` opens `system/public/ac-native-wasm/{index,notepat}.html`
in headless Chrome via puppeteer, focuses the canvas, types `notepat`
at the prompt, and writes two PNGs — `ac-native-prompt.png` and
`ac-native-notepat.png`. these are real ac-native frames, not mockups.

```bash
node marketing/bin/capture-ac-native.mjs                       # writes to marketing/captures/
node marketing/bin/capture-ac-native.mjs --out ~/Desktop/foo/refs   # straight into a campaign refs dir
```

drop the captures into a campaign's `refs/` and gen-promo will pass
them to gpt-image-2 alongside the jeffrey identity refs — the model
renders the on-screen content keyed to the real ui.

## Style guide

prompt prose follows [`papers/VOICE.md`](../papers/VOICE.md) — lowercase,
fragments, em-dashes, first-person where natural. no academic exposition.

## Sibling pipelines

- `papers/bin/gen-cover.mjs` — paper-cover emblems on cream paper. No
  jeffrey refs; prompt-only `/v1/images/generations`.
- `recap/bin/gen-photos.mjs` and `recap/bin/jeffrey-photos.mjs` — recap
  segment photos driven by audience configs. Use the same SHOOT + SELFIE
  refs that `lib/jeffrey-refs.mjs` exports here.

A future cleanup can migrate the recap scripts to import from
`marketing/lib/jeffrey-refs.mjs` instead of duplicating the arrays.
