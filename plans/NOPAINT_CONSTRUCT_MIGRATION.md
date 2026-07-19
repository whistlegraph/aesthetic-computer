# No Paint: Construct Recovery and Aesthetic Computer Migration

## Source recovered

- Dropbox source: `/Apps/Construct 3/No Paint/No Paint.c3p`
- Dropbox modified: `2021-10-16T21:52:49Z`
- Size: `7,444,479` bytes
- SHA-256: `1550856d224332eca2615736ae4526a404d235e9d9464e75d8058654d02aa7e2`
- Recovery date: 2026-07-17
- The archive passes `unzip -t` and was inspected read-only outside the repo.

The `.c3p` is a ZIP archive containing the complete editable Construct project,
not merely an exported build. Keep the Dropbox original immutable and make any
Construct experiments on a copy.

## Why current Construct will not open it

The project embeds its missing addon at:

`addons/plugin/StraniAnelli_InjectCSS_v2.c3addon`

It also embeds the two custom effects it needs:

- `addons/effect/skymen_RGBSplit.c3addon`
- `addons/effect/Mikal_MirrorY.c3addon`

The CSS addon is present; current Construct rejects it because it uses legacy
SDK v1. Construct r449 LTS is the final line that supports SDK v1 and should be
used to open and export the preserved project.

The CSS addon is not part of No Paint's painting engine. It is referenced only
twice in `eventSheets/splash.json`:

1. Load the bundled `background.css`.
2. Add the class `mobile` to `body, html`.

The entire stylesheet is one rule which changes the mobile page background to
`rgb(235, 235, 235)`. This can be removed or replaced with ordinary host CSS.

## Recovered project anatomy

- 45 event sheets plus their UI-state files
- 82 layouts plus their UI-state files
- 205 object types
- 1,197 images
- 128 sounds and 2 music tracks
- 3 fonts
- 2 JavaScript files
- 3 bundled addons
- 35 unique weighted brush/filter choices

The project is significantly more than the current AC `nopaint.mjs` prototype.
Besides the painting loop it contains prompt commands, playlists, camera input,
parameter hints, save/load, music, stamps contributed under painter handles,
and painter-story/profile layouts.

### Recovered proposal vocabulary

Native/generative brushes:

`Softy`, `Build`, `Bubbles`, `Banner`, `Dark Window`, `Wafer`, `Walker`,
`Aura`, `Box`, `Triangle`, `Line`, `Ellipse`, `Breathe`, `Vignette`,
`Caterpillar`, `Frame`, `Grid Worm`, `Stamp`, and `Rainbow`.

Painting transforms and filters:

`Wipe`, `Blur`, `Sharpen`, `Mirror`, `Noise`, `Flip`, `Saturate`, `Invert`,
`Scroll`, `Contrast`, `Zoom`, `Recurse`, `Quicksand`, `Spin`, `Light Bump`,
and `Turn`.

`Stamp` has the highest proposal weight (`2`), followed by `Softy` (`1.5`).
`Walker` is deliberately rare (`0.2`). The picker also contains a second Box
entry, which raises Box's aggregate probability.

## Original state machine

No Paint's essential behavior is small even though its Construct UI is large:

```text
pick weighted brush/filter + parameters
                 |
                 v
       animate a live proposal
          /             \
       No                 Paint
       |                    |
discard proposal       freeze time
       |               commit into Painting
       |               store painting/date
       \____________________/
                 |
             pick next
```

The recovered `No()` function kills playlist timers, restores time scale, and
returns to the Index layout without committing. `Paint()` freezes time, sets
the `Painting` flag, waits for the brush's `"painted"` signal, stores the
result, clears the temporary canvas region, restores time, and returns to the
Index. This is the conceptual core to port; Construct layouts are an
implementation detail.

## What Aesthetic Computer already provides

`system/public/aesthetic.computer/systems/nopaint.mjs` already has the hard
platform layer needed for the remake:

- a separate proposal buffer over the persistent painting
- present/bake behavior
- brush transforms and gesture recording
- pan and zoom
- undo paintings
- brush parameter and color parsing
- robot/synthetic input
- a native-compatible nopaint implementation

The current `disks/nopaint.mjs` only imports `rect` and emits random drag boxes.
It should become the conductor for the mature nopaint system rather than grow a
second painting engine.

Existing AC brushes give the first porting set:

| Construct concept | AC starting point |
| --- | --- |
| Box | `rect` |
| Ellipse | `oval` |
| Line | `line` |
| Wipe | `wipe` |
| Camera | `camera` |
| Stamp | `paste`, `icon`, and painting assets |
| Paint/commit | nopaint `bake`/`present` path |
| No/discard | clear/recreate the proposal buffer |
| Prompt commands | AC prompt and colon parameters |

## Migration sequence

### 0. Preserve

1. Keep the canonical `.c3p` in Dropbox unchanged.
2. Record its hash and provenance (above).
3. Open a copy in Construct r449 LTS.
4. Export a static web build as a playable reference.
5. Preserve that build at a stable classic URL, not as the new implementation.

### 1. Make the native loop real

Replace the one-brush timer in `disks/nopaint.mjs` with explicit states:

- `choosing`
- `proposing`
- `committing`
- `discarding`
- `paused`

Start with `rect`, `oval`, `line`, `wipe`, and `camera`. Preserve the original
weighted picker. Expose two unmistakable actions: No and Paint. Keyboard,
pointer, touch, voice, and robot controls should all call those same actions.

### 2. Port authored brushes

Port one event sheet at a time as a normal AC piece. Treat the Construct event
sheet as a behavioral score, not source code to translate mechanically. Begin
with visually distinctive, low-dependency brushes: Softy, Bubbles, Grid Worm,
Walker, Banner, and Wafer.

### 3. Recover the social artwork

The 1,197 images and painter profiles are culturally central. Build a manifest
that retains original filename, Construct object/layout, painter handle,
dimensions, and source archive hash. Bring stamps into AC's existing publishing
and handle system instead of hard-coding them into a new monolith.

### 4. Restore the language

Map old playlists and brush parameter strings onto the AC prompt. A historical
playlist can become a sequence of URL-addressable AC commands. This is where
the predecessor becomes visibly continuous with Aesthetic Computer rather than
merely reskinned.

### 5. Present the lineage

Use `nopaint.art` as the canonical public home. It already has history and is
the strongest name for the work. Suggested routing:

- `nopaint.art` — the new native work and its short explanation
- `nopaint.art/classic` — preserved 2021 Construct build
- `aesthetic.computer/nopaint` — the same live native piece inside AC
- `nopaint.art/story` — 2016 to 2020/21 to Aesthetic Computer to the present

The award submission can frame the work as **No Paint, 2016–present**: a long
collaboration between a person, a proposing machine, and a community of brush
and stamp makers. The Construct recovery is evidence of lineage; the native AC
remake is the current artwork.

## First shippable milestone

A useful first milestone is intentionally narrow:

1. Native AC No Paint proposes `rect`, `oval`, `line`, `wipe`, or `camera`.
2. No discards the current buffer immediately.
3. Paint commits it and proposes the next operation.
4. The proposal weights are deterministic from a session seed.
5. A painting can be saved/shared through AC.
6. `nopaint.art` launches this piece while `/classic` preserves the old build.

That is enough to recover the original artwork's agency and make a credible
award submission. The remaining brushes, profiles, sounds, and playlists can
then return incrementally without blocking the relaunch.

## 2026-07-19 — hosting swap + archive findings

Step 0 is materially complete and the domain now lives on AC infrastructure.

- **Hosting moved Vercel → lith.** `system/public/nopaint.art/` holds the
  complete 2021 export mirrored from the live site (165 files; all 130
  audio, 32 spritesheets, runtime, fonts, icons). Caddy serves it at both
  the root and `/classic` with a dedicated ACME site block (this host is
  direct-DNS on DigitalOcean, not Cloudflare-proxied like the rest of
  lith). `www` 301s to the apex. DNS A records flipped via doctl
  (76.76.21.21 → 209.38.133.33).
- **`.c3p` is local**: `~/Documents/nopaint-recovery/` holds the Dropbox
  original (SHA-256 verified against the hash above) plus a full
  extraction — so brush ports need neither Dropbox nor Construct.
- **The painting archive is far larger than the project file suggests.**
  DO Spaces bucket `pix.nopaint.art` (sfo3, CDN at
  `https://pix.nopaint.art/<id>.png`) holds **32,184 paintings / 3.9 GB**,
  256×288 px, uploaded 2021 (15,407) → 2022 (8,016) → 2023 (5,449) →
  2024 (2,112) → 2025 (1,200). People kept playing for four years after
  "Summer 2021."
- **Saving is currently broken.** Uploads stop in 2025; the game's save
  server was `server.nopaint.art` → `glitch.edgeapp.net`, and Glitch is
  gone. Restoring save (lith endpoint → Spaces) is now part of the plan.
- **No painter attribution in the archive**: PNGs carry no tEXt/S3
  metadata; handle association (if recorded) lived in the dead Glitch
  app. Check for a Glitch project export before assuming it is lost.

### Gallery (extends step 3) — SHIPPED 2026-07-19

`nopaint.art/gallery` is live: an endless grid over all 32,184
paintings, driven by `gallery/manifest.json` (generated from the bucket
listing: id, date, size; regenerate after save is restored), images
straight off the pix CDN, with by-year filters and a shuffle view
(default). Pixelar face + the #ebebeb grey, matching the game. Painter-
handle overlays arrive later if the Glitch data surfaces. The 1,197
in-project stamp/profile images remain a separate, curated set from
the extracted `.c3p`.
