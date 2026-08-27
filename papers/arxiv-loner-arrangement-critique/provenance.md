# Provenance — Three Kinds of Time

Snapshot date: 2026-08-27, America/New_York.

## Primary evidence

- `pop/loner/out/lonerclub-v4pid-release.wav`
  - modified 2026-08-27 10:41:54 EDT
  - SHA-256 `cfaed16556b46b3eb375ad7d132882321436e5c16906c1cb4cd48decd6bec7b0`
- `pop/loner/out/lonerclub-v4pid-release.mp3`
  - modified 2026-08-27 10:41:56 EDT
  - SHA-256 `4be1dd066b3331142dc7e30be9b36931f1146504129cb5bdc08549cd72f3f93b`
- `pop/loner/viz/wordclock.json` — per-word vocal and drawing-gesture clock.
- `pop/loner/vox4/.chart.json` — regulated 122 BPM note chart.
- `pop/loner/README.md` — work, take, tuning, and version history.
- `pop/RELEASES.md` — release-lane record.
- `pop/loner/c/lonerremix.c` and `pop/loner/c/cut-wax.sh` — tracked score and mastering law.
- `neo:vod` (`df296e24-513a-488b-b96b-31cb958c1bda`) — active arrangement-session decisions. The raw transcript was inspected locally and is not bundled.

The analysed WAV/MP3 and raw session transcript are excluded from the source bundle. They contain copyrighted performance audio and private process material; their hashes and the minimal decisions required for audit are recorded here.

## Platter consulted

- `papers/SCORE.md` and the rendered public Platter index.
- `papers/rhythm-platter/README.md`.
- `papers/rhythm-platter/digest/07-perception-groove.md`.
- `papers/rhythm-platter/digest/10-timbre-space.md`.
- `papers/rhythm-platter/digest/10a-timbre-space-in-ac.md`.
- `papers/arxiv-whistlegraph/whistlegraph.tex` and `references.bib`.
- `papers/arxiv-sampling/sampling.tex` and `references.bib`.
- `papers/arxiv-comp-strats/comp-strats.tex` and `references.bib`.

The rhythm platter marks London, Butler, Iyer, and Danielsen as unchecked. This paper therefore does not cite those records. Wessel (1979) is marked checked against the local scan.

## Reproduction

Run:

```sh
pop/.venv/bin/python papers/arxiv-loner-arrangement-critique/analysis.py
```

The script reads the local release-candidate WAV and writes:

- `analysis-windows.csv`
- `section-metrics.csv`
- `figures/arrangement-evidence.png`

The EBU R128 master values were measured separately with FFmpeg's `ebur128=peak=true` filter.
