# /pop track manifests

Each released `pop/` single gets a piece at `disks/<slug>.mjs` (≈ 25 lines)
that delegates to `lib/pop.mjs`, plus a JSON manifest in this folder. Same
pattern as `chat.mjs` ↔ `laer-klokken.mjs` — all the player code lives in the
shared lib, the wrapper just names a manifest.

The player is a **static slideshow**: the active section's illy fills the
screen, a per-section colour-arc progress bar + timecode run along the
bottom, and the only gesture is tap = play/pause. Frames are cached
(`paint` returns `false`) and the backdrop is pre-scaled once per
section/resize, so the piece idles at roughly one repaint per second.
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

- `audio` / `cover` — CDN-served (`assets.aesthetic.computer/pop/...`) or
  dev-served (`/assets/pop/...`). Both work because `system/public/assets/`
  syncs to the CDN.
- `sections[].t` — start-second of each section. Pulled from the track's
  `out/<slug>.struct.json` so it matches the audio bake exactly.
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

A future `pop/bin/publish-illys.mjs <slug>` should automate steps 1–2 by
reading `out/<slug>.struct.json` + the campaign gens dir.

## Released wrappers wired

- `marimbaba` ✓
- `helpabeach` ✓
- `trancenwaltz` ✓
- `trancepenta` ✓
- `fluttabap360` ✓
