# Offloaded media

Gitignored render output moved off neo to the render node **poorslice**
(`ssh poorslice`, user `aesthetic`). Destination root `~/offload/neo/`
mirrors the path under `/Users/jas/aesthetic-computer/`. Each batch was
copied with `rsync -a --partial`, verified with
`rsync -a -c -n -i` (empty output = byte-identical) and matching file
counts, then removed locally.

Fetch anything back from the repo root with

```sh
rsync -a --partial poorslice:~/offload/neo/<path>/ <path>/
```

## 2026-10-04

Trigger: root disk at 1.7 GB free, renders failing. Sources, stems,
samples, take recordings and anything git tracks were left in place.

| local path (under aesthetic-computer/) | poorslice path (under ~/offload/neo/) | size | files |
| --- | --- | --- | --- |
| pop/afternoon-study/out | pop/afternoon-study/out/ | 10G | 789 |
| pop/sailor-song/out — `sailor-song-v<N>.mp3` and `sailor-song-v<N>.events.json` for N < 115 (v17–v114, incl. lettered variants like v20k, v31b) | pop/sailor-song/out/ | 743M | 236 |
| pop/eightgigabytes/out | pop/eightgigabytes/out/ | 929M | 358 |
| pop/loner/out | pop/loner/out/ | 133M | 31 |
| pop/cult/out | pop/cult/out/ | 161M | 29 |
| pop/cult/c/out | pop/cult/c/out/ | 233M | 4 |
| pop/factory/out | pop/factory/out/ | 33M | 5 |
| pop/blackboard/out | pop/blackboard/out/ | 3.2M | 15 |
| pop/marimba/out | pop/marimba/out/ | 9.6M | 7 |
| pop/menuband/out | pop/menuband/out/ | 2.0M | 2 |
| pop/americomputadora/variations | pop/americomputadora/variations/ | 206M | 1802 |
| pop/americomputadora/c/out | pop/americomputadora/c/out/ | 217M | 6 |

Kept locally in pop/sailor-song/out: v115–v120 everything, all `.flac`
and `-master.wav` (not in scope), `sailor-song-v103-perf.mp4` (video
base), `sailor-song-v119-relight.mp4`, `sailor-song-v118-final.mp3`,
v17/v18 `-vox-click.mp3`, and anything under an hour old (v120 was
rendering during the move). pop/sailor-song/src untouched.

Left alone on purpose: pop/samples, pop/physical/masters (press
masters), pop/americomputadora/{sources,utterances,snapped} (snapped is
the render's input library), pop/nullabye/reel, pop/menuband/raytracer,
pop/marimba/lullabies (git tracked).

Caches cleared (not offloaded): `uv cache clean` 1.2 GiB,
`npm cache clean --force`, `brew cleanup --prune=all -s`.
