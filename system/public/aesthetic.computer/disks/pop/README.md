# /pop track manifests

Each released `pop/` single gets a piece at `disks/<slug>.mjs` (≈ 25 lines)
that delegates to `lib/pop.mjs`, plus a JSON manifest in this folder. Same
pattern as `chat.mjs` ↔ `laer-klokken.mjs` — all the player code lives in the
shared lib, the wrapper just names a manifest.

The player is a **slideshow with a camera**: the active section's illy fills
the screen at any aspect ratio, cropped around the section's `focus` point,
and the camera eases onto that point over the section. A live waveform, a
colour-arc progress bar that keeps the loudness of what has played, and a
timecode run along the bottom. Tap = play/pause; drag the bar (or use the
arrow keys) to scrub. Frames are cached (`paint` returns `false`) and the
backdrop is pre-scaled once per section/resize into a buffer a little larger
than the screen, so every frame is an unscaled blit. The waveform asks for
about 30 repaints a second while audio plays; paused, the piece is still.
`<slug> <code-or-number>` (params or colon) starts playback at a section.

Two hard-won constraints (violating either stalls the page on long tracks):
- **Audio streams** over the `stream:*` HTML-audio channel (like `dj.mjs`),
  never `sound.play` — the worklet sample path decodes the whole track and
  clones ~130MB of PCM for a 6-minute song. Wrappers must forward the
  `receive` export so `stream:time-data` etc. reach the player.
- **Illys load on demand** (current section + next, older ones evicted) —
  preloading 34 × 1024² illys fronts ~140MB of decoded pixels at boot.

## Manifest schema

```json
{
  "slug": "marimbaba",
  "title": "marimbaba",
  "artist": "Aesthetic Dot Computer",
  "album": "pixsies",
  "bpm": 56,
  "key": "F major",
  "meter": "3/4",
  "duration": 83.5714,
  "released": "2026-05-22",
  "status": "released",
  "audio":  "/assets/pop/<slug>.mp3",
  "cover":  "/assets/pop/<slug>.jpg",
  "sections": [
    { "name": "<section name>", "t": 0, "illy": "/assets/pop/<slug>/sec-0.jpg" },
    ...
  ],
  "links": { "spotify": "https://open.spotify.com/track/..." },
  "credits": "<short attribution line>"
}
```

- `released` — the ISO date the single went out (DistroKid release date, or
  the day it went live on AC). `pop.aesthetic.computer` sorts by it, newest
  first; `pop/RELEASES.md` is where the date comes from.
- `status` — `released` (in stores via DistroKid) or `ac` (self-hosted on AC
  only, e.g. `sailor-song`). Defaults to `released`.
- `links` — `spotify`, `apple`, `youtube` as recorded in `pop/RELEASES.md`.
  The catalog page shows whichever are present.
- A manifest with `"sections": []` and no `disks/<slug>.mjs` wrapper is
  **catalog-only** (`americomputadora`, `momabobasheep`): it plays on
  `pop.aesthetic.computer`, in `juke` and the iPhone app, but has no
  slideshow piece, so the catalog's `piece` is `null`.
- `audio` / `cover` — CDN-served (`assets.aesthetic.computer/pop/...`) or
  dev-served (`/assets/pop/...`). Both work because `system/public/assets/`
  syncs to the CDN.
- `sections[].t` — start-second of each section. Pulled from the track's
  `out/<slug>.struct.json` so it matches the audio bake exactly.
- `sections[].focus` — optional `[x, y]`, each 0..1 across the illy: the
  point the camera keeps in frame and settles on (faces, the instrument).
  Centre when absent; a top-level `focus` sets the default for every section.
  Portrait art on a wide screen shows about a quarter of its height, so set
  `y` where the subject is.
- `sections[].illy` — per-section illustration JPG (1024² is plenty). For
  `marimbaba` these came from `pop/marimba/out/marimbaba-p-sec-*.png` via
  `magick … -resize '1024x1024>' -quality 82`.

## Adding a new track (recipe)

1. **Stage section illys.** From the campaign gens, pick the locked variant
   per section and write to `system/public/assets/pop/<slug>/sec-<n>.jpg`.
2. **Write the manifest.** Copy `marimbaba.json`, edit slug/title/sections.
   Pull section `t` values from `<slug>.struct.json`.
3. **Drop the wrapper.** Copy `disks/marimbaba.mjs` to `disks/<slug>.mjs`,
   change the `MANIFEST_URL` and fallback meta string. Keep the `receive`
   forward — stream playback events arrive through it.
4. **Sync CDN** (when ready to publish): `npm run assets:sync:up` + the
   `doctl compute cdn flush` dance from `[[project_cdn_overwrite_stale]]`.
5. **Visit** `aesthetic.computer/<slug>` to confirm — tap to play.
6. **Publish the catalog:** `node pop/bin/publish-release-records.mjs`
   (rewrites `system/public/pop.aesthetic.computer/releases/catalog.json` +
   `feed.xml`) then `node pop/bin/catalog-art.mjs <slug>` (thumbs/, art/,
   and the `pop-og.jpg` montage when run without a slug). Deploy lith.

A future `pop/bin/publish-illys.mjs <slug>` should automate steps 1–2 by
reading `out/<slug>.struct.json` + the campaign gens dir.

## Released wrappers wired

- `marimbaba` ✓
- `helpabeach` ✓
- `trancenwaltz` ✓
- `trancepenta` ✓
- `fluttabap360` ✓
- `femrag++` ✓ (also `femrag-plusplus`; manifest slug is the punctuation-free
  one, since it names the asset paths. Its section stills are twelve crops of
  the cover photograph rather than an illy campaign —
  `pop/maytrax/bin/stage-femrag-plusplus-assets.sh` cuts them. A disk cannot
  `import` a sibling disk to share a wrapper: the loader hands modules a
  non-hierarchical base and `./other.mjs` fails to resolve, so the two names
  are two full wrappers over one manifest.)
- `sailor-song` ✓ (sage oc's cover, artist `sage oc`. Its section stills are
  frames of the relit performance video, cropped above the caption row.)

`pop.aesthetic.computer` (also served at `aesthetic.computer/pop`) lists the
catalog newest-first with an in-page jukebox; `juke` is the same catalog as a
piece. The AC iPhone app plays the whole catalog natively: `pop` (or `pop <slug>`)
at the prompt opens `apple/aesthetic.computer/PopPlayer.swift`, which reads
`pop.aesthetic.computer/releases/catalog.json`. A track reaches it once
`node pop/bin/publish-release-records.mjs` has been re-run with its manifest.
