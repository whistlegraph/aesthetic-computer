# chillwave

audio-only chillwave / ambient tracks built bottom-up from AC instruments. one AC vision per track, compressed to a slow melodic theme over a noise-bed of waves and filter sweeps.

opens on **explore** as the first sub-format — mysterious, exploratory chillwave with literal ocean waves, slow filter sweeps, and bubble textures. future sub-formats (downtempo, drone, vaporwave proper) land alongside if and when they earn a finished track.

## format spec — explore

- **length**: ~1:30 (sketch edit). canonical chillwave is 4–6 minutes; we compress while keeping the slow-burn arc intact
- **tempo**: 70 BPM (range 60–80), often felt in half-time so the perceived pulse is ~35
- **key**: minor pentatonic — A minor / E minor / D minor by default
- **structure**: tide-in (8) → drift 1 (16) → swell (8) → drift 2 (16) → tide-out (8) bars
- **bed**:
  - **ocean bed** — pink noise low-passed by a ~0.12 Hz LFO (~8 s wave period) with a slow tremolo
  - **filter sweeps** — white noise low-passed by a 0.04–0.08 Hz LFO sweeping 200 Hz → 6 kHz over ~12–25 s
  - **bubbles** — short descending sine blips (1500 → 400 Hz, ~80–150 ms), sparse, random pan
  - **pad** — soft sine pad (fundamental + octave + fifth), slow attack, long release
  - **bells** — sinebells melody from the .np score, sparse, far back in the mix
- **vocal**: optional — jeffrey-pvc whispered fragments only (not sung, not rapped). spoken-word reverb-soaked. instrumental tracks are valid by default
- **output**: single mp3 per track in `out/<slug>.mp3`. no video

## source → track

each track corresponds to one AC vision or moment from the platter, same as big-pictures and dance — but compressed to a *slow melodic theme* set into an ambient noise-bed.

```
<idea / paper / moment>
  → pop/chillwave/<slug>.np            (notepat score: melody + section markers)
  → pop/chillwave/<slug>.txt           (optional spoken-word fragments)
  → pop/chillwave/out/<slug>.mp3       (mix)
```

the `.np` file carries the **bell melody** as the primary content — every note is a placeholder syllable (`_`) unless a vocal fragment is being scored.

## arrangement notation

same as the dance lane — section markers as comments tell the bed builder when to bring waves in, when to open the filter, when to drop the bubbles:

```
# tide-in 8 [waves-fade-in, no-bells]
A4:_*4 E4:_*4

# drift 1 16
A4:_*2 C5:_*2 E5:_*2 D5:_*2 ...

# swell 8 [filter-sweep-open, bubbles-dense]
...
```

bracket flags pass through to `bin/render.mjs` to gate which bed layers fire on each section. unflagged sections inherit the lane defaults.

## pipeline

`bin/render.mjs` — single-command renderer. mirrors `melody-bells.mjs` but layers the ocean bed, filter sweeps, bubbles, and pad alongside the bell line.

```
read .np score
  → render bells (sinebells, slow + sparse)
  → render pad   (sinepower pad voice, slow attack)
  → render waves (pink noise + LFO LP)
  → render sweeps (white noise + slow LFO LP)
  → render bubbles (descending sine blips)
  → mix + peak normalize → mp3
```

cached per step by content hash so reruns cost $0 unless inputs change.

## tracks

- **explobeach** — slow A-minor pentatonic wander, lots of waves, lots of sweeps, lots of bubbles. mysterious exploration

---

*maintained by @jeffrey*
