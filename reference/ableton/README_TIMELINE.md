# Ableton Timeline Data Extraction Guide

This directory contains `analyze-ableton.mjs` used to parse a large extracted Ableton Live XML (unzipped `.als`) and produce:

- `ableton-report.json` structured data
- `ABLETON_TIMELINE_GUIDE.md` human-friendly summary (run with `--markdown`)

## Quick Run

```
cd reference
node analyze-ableton.mjs \
  --project ../system/public/assets/wipppps/zzzZWAP_extracted.xml \
  --blank ./live-12-blank.xml \
  --out ableton-report.json --markdown

## Run Simulation (ncurses-style)

After generating `report.json` and `notes.json` (use `--out report.json --notes-out notes.json` flags when analyzing):

```
cd reference
node analyze-ableton.mjs \
  --project ../system/public/assets/wipppps/zzzZWAP_extracted.xml \
  --blank ./live-12-blank.xml \
  --out report.json --notes-out notes.json

node simulate-ableton.mjs --auto --rate 2 --fps 20
```

Flags:
- `--auto`                 looks for `./notes.json` and `./report.json`
- `--rate <mult>`          playback speed multiplier (beats progress faster)
- `--fps <n>`              UI refresh rate (default 20)
- `--filter-pitch X`       only simulate/render notes of pitch X
- `--filter-pitches a,b`   multi pitch filter
- `--aggregate-window <b>` rolling window in beats for density bars (default 4)
- `--hat-pitches a,b`      mark these pitches as target/"hi-hat" group in stats & stream
- `--show-stream`          enable scrolling pitch stream line
- `--stream-width <n>`     width of stream (default 60)
- `--start <beat>`         start from beat (default 0)
- `--end <beat>`           stop at beat (default inferred end)
- `--sandcastle`           accumulate bottom histogram of bursts over song (with `--sand-height <rows>`)
- `--burst-line`           append a scrolling line of per-burst glyphs
- `--sparklines`           per-track micro history (use `--spark-width <n>`)
- `--bpm <n>`              override tempo
- `--minimal-line`         ultra-minimal continuous glyph stream (legend then raw glyphs only)
- `--minimal-pitch`        (with minimal-line) show pitch-class letters instead of track dots when not matched by group / hat
- `--omni-line`            pure event firehose: every timed thing (ALL notes unfiltered, locators, clip start `[`, clip end `]`, tempo change `t`) as a left-to-right stream
- `--no-beat-marks`        (with minimal-line or omni-line) suppress periodic `|` beat markers
- `--groups 'Name:p1,p2:Glyph;Name2:p3:Glyph2'` define pitch groups (order = precedence)
- `--snapshot-csv file.csv` periodic snapshot rows (see also `--snapshot-interval-beats <b>`) 
- `--groups-grid`          show per-group (or hats / fallback tracks) aggregated beat-intensity lanes
- `--grid-csv file.csv`    export per-beat group counts (columns: beat,<group labels...>)

Controls: press `q` to quit early.

Aggregation & Colors:
- Each track line shows a colored bar whose fill intensity reflects note density over the last `aggregate-window` beats.
- Colors transition green -> yellow -> red as density increases.
- A burst column of `*` shows notes fired this frame.
- Optional pitch stream shows recent notes as pitch-class glyphs (C c D d E F f G g A a B) with hat/target pitches highlighted as `H` in magenta.

### Minimal Line Mode & Pitch Groups

If you want the leanest possible visualization (a single growing line), use:

```
node simulate-ableton.mjs --auto --bpm 144 --minimal-line --minimal-pitch \
  --groups 'Kicks:36,48:K;Snares:38,50:S;Hats:42,44,46:h'
```

Legend prints once, then each note emits one glyph:

- Group match (in the order you list them) -> its colored single-character glyph.
- Else hat pitch (from `--hat-pitches`) -> `H` (magenta).
- Else (if `--minimal-pitch`) pitch-class colored letter (C c D d E F f G g A a B).
- Else fallback colored track dot.

Beat markers: `|` every 4 beats, bold `|` every 16 beats.

Fish shell note: semicolons are command separators in `fish`; wrap the entire groups string in quotes (single or double) OR escape semicolons: `Kicks:36,48:K\;Snares:38,50:S`.

Examples (fish):

```
node simulate-ableton.mjs --auto --minimal-line --minimal-pitch \
  --groups 'K:36,48:K;S:38,50:S;H:42,44,46:h' \
  --hat-pitches 60,72
```

Real‑time full song (assumes `bpm 144`):

```
node simulate-ableton.mjs --auto --bpm 144 --minimal-line --minimal-pitch \
  --groups 'K:36,48:K;S:38,50:S;H:42,44,46:h'
```

### Sandcastle & Burst Line

### Omni Line Mode (Everything)

If you want absolutely every timed event without any filtering, use `--omni-line`.

Emitted glyphs:

- Track note: colored dot (per track hash)
- Locator: magenta `|`
- Clip start: green `[`  Clip end: red `]`
- Tempo change: yellow `t`

Unlike `--minimal-line`, pitch/hat/group precedence is ignored; all notes share the per-track dot encoding for maximal raw density. Beat markers are shown unless you add `--no-beat-marks`.

Example:

```
node simulate-ableton.mjs --auto --omni-line --rate 8 --bpm 140 --no-beat-marks
```

Combine with `--no-beat-marks` for an uninterrupted glyph torrent.

- `--sandcastle` builds a vertical colored accumulation (green→yellow→red) per timeline column.
- `--burst-line` gives a compact scrolling composite of instantaneous per-track bursts (glyph size encodes intensity).

### CSV Snapshots

Use `--snapshot-csv snapshots.csv --snapshot-interval-beats 4` to export periodic aggregate counts.

Columns include global counts plus per track totals (and per track hat counts if hat-pitches provided).

### Groups Grid

`--groups-grid` renders one lane per defined pitch group (or a single Hat lane if only `--hat-pitches`, else top tracks fallback). Each column = one beat. Intensity tiers:

- base glyph (your group glyph) = 1 hit
- ░ 2–3 hits
- ▒ 4–7 hits
- ▓ 8–15 hits
- █ 16+ hits

Beat markers row ("Beats") shows `|` every 4 beats (bold every 16). A dim live cell at the right previews the in‑progress beat before it finalizes. Use `--grid-csv grid_counts.csv` to export raw per-beat counts for downstream analysis.
```

## Core Data Shapes

Timeline entry:
```json
{
  "index": 0,
  "trackId": "63",
  "name": "(Clip Name)",
  "times": { "CurrentStart": 0, "CurrentEnd": 8, "LoopStart": 0, "LoopEnd": 8 },
  "noteCount": 42
}
```

Clip (internal representation):
```json
{
  "type": "AudioClip|MidiClip",
  "trackId": "63",
  "name": "",
  "times": {"CurrentStart":0},
  "notes": [{"time":0,"duration":0.25,"velocity":100}],
  "rawAttrs": { /* original XML attributes for the clip element */ }
}
```

Locator:
```json
{"id":"3","time":32,"name":"DROP"}
```

Warp Marker:
```json
{"clipIndex":5,"beatTime":0,"secTime":0}
```

## Visualization Hints

- Horizontal axis: choose beats (from WarpMarkers & clip times) or absolute seconds (convert using tempo map if expanded later).
- Track lanes: one row per track (ordered as in `trackSummaries`).
- Clip bars: span `CurrentStart` to `CurrentEnd`; loop overlay using `LoopStart`/`LoopEnd` if those differ.
- MIDI notes: relative to clip start; render rectangles at `time` with width `duration`, height from pitch (pitch not yet captured—next enhancement would parse KeyTrack -> pitch attribute).
- Audio warp markers: small vertical tick marks at `beatTime` within the clip bar.
- Locators: global vertical lines across all tracks at `locator.time`.
- Color: use `track.color` (integer Ableton palette index) to tint lane.

## Next Enhancements (TODO)

1. Capture MIDI pitch (need to parse enclosing KeyTrack / NoteNumber attribute if present).
2. Extract tempo automation explicitly (currently grabbing raw Tempo Value occurrences; build a sorted deduplicated list with beat positions).
3. Handle Scene names & launch order.
4. Map group track hierarchy (`TrackGroupId`).
5. Export a slimmer JSON specifically for front-end consumption.
6. Add CLI flag `--notes-max` to limit note capture for huge sets.
7. Support writing separate files: `timeline.json`, `tracks.json`, `notes.json`.

## Implementation Notes

- Streaming SAX parse (`saxes`) so large 200k+ element files process quickly (< 1s here).
- Heuristics detect clips by element names ending in `Clip` (excluding `ClipSlot`).
- Timing attributes captured when inside a clip for known keys: CurrentStart, CurrentEnd, LoopStart, LoopEnd, etc.
- MIDI notes appear as `<MidiNoteEvent ... />`; captured directly with time/duration/velocity.
- Warp markers inside `<WarpMarkers>` list; we store a sample (all collected, but only sample surfaced now).
- Locators nested under LiveSet/Locators/Locators.

---
Generated helper; modify as you extend parsing.
