# single-study-toolkit

Static analysis for finished singles — study a track **from the outside in**,
the way a listener meets it: the mastered object first, then its shape in
time, then who is playing when, then what the notes are.

Registered in the pop menu (`lib/menu.mjs`) as `analysis.single-study` and
`analysis.study-compare`; the critique-bench posture lives in `SCORE.md`
under *Shared tooling — single study*.

| layer | name | what it measures |
|---|---|---|
| L0 | master | LUFS, LRA, crest, true peak, stereo image, spectral tilt |
| L1 | structure | tempo, beat grid, self-similarity, section letters |
| L2 | arrangement | six-band energy over time, harmonic/percussive, onsets |
| L3 | harmony | chroma, global + per-section key, dominant-voice pitch |

## use

```fish
cd pop
.venv/bin/python study/study.py path/to/track.mp3 \
    --out study/out/track-slug --title "One Step" --artist oskie
```

Outputs land in `--out`: `report.json`, `REPORT.md`, and four figures
(`fig-structure`, `fig-ssm`, `fig-arrangement`, `fig-chroma`).

Compare several studied tracks:

```fish
.venv/bin/python study/compare.py study/out/*/report.json \
    --out study/out/comparison
```

That writes `COMPARISON.md` plus section-timeline, band-balance, and
loudness-small-multiple figures.

## honesty notes

- Loudness range and true peak are **approximations** (RMS-window LRA,
  4× oversampled peak) — good for comparison, not for mastering QC.
- Section letters are repetition classes **within one track**; the same
  letter on two different tracks means nothing.
- Key/melody estimates run on the harmonic component of the full mix;
  treat them as evidence, not truth.
- A 128 kbps source rolls off ≈16 kHz — ignore the `air` band verdict
  on streaming rips.

Deps live in `pop/.venv`: librosa, scipy, soundfile, matplotlib,
pyloudnorm. First run of a study takes ~1–3 min per track (pyin is the
slow part).
