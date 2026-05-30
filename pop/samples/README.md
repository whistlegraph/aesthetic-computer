# pop/samples — third-party reference audio + global sample index

This is where downloaded reference tracks (YouTube, Bandcamp rips, etc.)
are sliced into per-onset WAV chops for use across the `pop/` lanes.

It is the *reference* counterpart to lane-owned sample libraries like
`pop/hellsine/samples/`, which hold AC's own recorded sounds. Sample
**audio** here is third-party and stays local — only the **metadata**
(per-source `manifest.json` + global `INDEX.json` + `onsets.json`) is
committed, so anyone with the repo can rebuild the chops from the URL.

## layout

```
pop/samples/
  INDEX.json                       ← tracked · aggregates every source
  README.md                        ← this file
  <source-slug>/
    manifest.json                  ← tracked · slice recipe + chop list
    onsets.json                    ← tracked · librosa onset times (cheap)
    source.m4a                     ← gitignored · yt-dlp download
    source.wav                     ← gitignored · 48k mono for cutting
    chops/
      chop-001-0428ms.wav          ← gitignored · padded zero-index = ms length
      chop-002-1192ms.wav
      …
```

## adding a source

```bash
node pop/bin/sample-from-youtube.mjs <url> \
  [--slug my-name]        # override slug (default: from YT title)
  [--min-ms 250]          # minimum chop length
  [--max-ms 6000]         # maximum chop length (clamps long tails)
  [--max-chops 64]        # cap total chops, keep loudest
  [--start 12 --end 180]  # trim head/tail seconds from source
  [--note "for hippyhayzard verse 2"]
  [--redo]                # recut even if chops/ exists
```

Pipeline: `yt-dlp` → `ffmpeg` (48k mono PCM) →
`pop/bin/detect_onsets.py` (librosa) → onset-aligned WAV chops with
3 ms declick fades + per-chop peak normalization → manifest +
global-index update.

## reproducing chops on a fresh checkout

```bash
git pull
node pop/bin/sample-from-youtube.mjs <url-from-INDEX.json>
# or pass --slug to keep the existing directory name
```

`yt-dlp` is the only thing the recipe truly needs; `onsets.json` is
kept around so the cutter is fully deterministic without re-running
librosa if you only want to retune `--min-ms` / `--max-ms` (the cutter
re-runs onsets every time today, but the data is committed for audit
and downstream tools).

## BBC Sound Effects (sound-effects.bbcrewind.co.uk)

A second source: the BBC Sound Effects archive (30k+ effects). Fetched
via `pop/bin/bbc-fetch.mjs` (lib: `pop/lib/bbc-rewind.mjs`) — no
credentials needed, it hits the same public search + media endpoints the
website's app uses, and writes full-quality WAVs named by BBC id
(`<id>.wav`, e.g. `pachinko-bbc/07022449.wav`).

```bash
# search only — print results so you can pick ids:
node pop/bin/bbc-fetch.mjs --query "pachinko" --list

# search + download the top N into pop/samples/<slug>/:
node pop/bin/bbc-fetch.mjs --query "pachinko" --count 6 --slug pachinko-bbc

# download specific ids directly:
node pop/bin/bbc-fetch.mjs --slug pachinko-bbc --id 07022449,07032210
```

Each fetch writes a tracked `manifest.json` (id + description + the
RemArc licence stamp) and drops a `.gitignore` so the WAV audio stays
local, then upserts the global `INDEX.json`.

> ⚠ **Licence — RemArc, non-commercial only.** The BBC archive is free
> for personal / educational / research use and **non-commercial live
> performance** (e.g. the AC-native `dj` piece at a free/art set). It is
> **not** cleared for commercial release — a BBC sample must never be
> baked into a DistroKid release master. Every fetched sample is tagged
> `license: "remarc-noncommercial"`, `commercialUse: false`. For
> commercial use the BBC licenses the same library via Pro Sound Effects.
> Terms: <https://sound-effects.bbcrewind.co.uk/licensing>

## why third-party audio stays out of git

Same posture as `pop/references/README.md`: third-party copyrighted
material does not belong in a public github repo. The manifest +
URL + slicing parameters are scaffolding (fair-use-ish for a private
research notebook); the actual mp3/wav is not.
