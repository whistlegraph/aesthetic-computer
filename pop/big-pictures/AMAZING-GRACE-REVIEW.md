# Amazing grace — listening review

Artist: **Aesthetic Dot Computer**. User approved the choir-v2 playback and authorized DistroKid release on September 23, 2026. DistroKid confirmed submission; the release is processing. [HyperFollow](https://distrokid.com/hyperfollow/aestheticdotcomputer/amazing-grace).

The September 23 arrangement repeats the verse after the plagal turnaround.
The second pass starts at 64.286 seconds. Independently rendered inner harmonies
and octave voices enter progressively; the highest layer sustains “see” over the
amen. The dry lead stays centered. Backing voices carry most of the flange and
room reflections. Scored FM blips answer phrases, and stereo bursts mark transitions.

The vocal method comes from `pop/cult/bin/sing.py`, also used for `wannadash`:
stretch vowels while preserving consonants, replace pitch from a score, verify
the resulting notes. The hymn uses legato joins and gentler mastering than the
percussive `wannadash` arrangement. All layers use existing Jeffrey PVC sources;
these are synthesized harmonies, not new recordings.

## Reproduce on Blueberry

From the repository, with the existing archived vocal takes, Python environment,
and source renders restored:

```sh
export PATH="$PWD/toolchain/shims:$HOME/.local/bin:/opt/homebrew/bin:$PATH"
export AG_OUT="$PWD/pop/big-pictures/out"
OUT="$AG_OUT/amazing-grace-smooth-2026-09-23" SMOOTH_VOX=1 \
  bash pop/big-pictures/bin/bake-amazing.sh
bash pop/big-pictures/bin/bake-amazing-harmony.sh
pop/.venv/bin/python pop/big-pictures/bin/clarify-amazing.py \
  "$AG_OUT/amazing-grace-smooth-2026-09-23" \
  "$AG_OUT/amazing-grace-clear-lead-2026-09-23"
pop/.venv/bin/python pop/big-pictures/bin/hybrid-amazing.py \
  "$AG_OUT/amazing-grace-harmonized-2026-09-23" \
  "$AG_OUT/amazing-grace-clear-lead-2026-09-23" \
  "$AG_OUT/amazing-grace-hybrid-lead-2026-09-23"
OUT="$AG_OUT/amazing-grace-choir-hybrid-2026-09-23" \
LEAD_OVERRIDE="$AG_OUT/amazing-grace-hybrid-lead-2026-09-23/lead-centered.wav" \
CHOIR_FORWARD=0 bash pop/big-pictures/bin/bake-amazing-choir.sh
```

Each stage writes a separate `out/` folder. `SOURCE`, `OUT`, and `LEAD_OVERRIDE`
select a source/candidate without replacing earlier versions. The final pass uses
tone/glue, measured static gain, an oversampled limiter, and native 24-bit/48 kHz
delivery. `master-audit.tsv` includes mono/phone translation and decoded AAC.

## Diction check

```sh
pop/.venv/bin/python pop/big-pictures/bin/hear-amazing.py \
  path/to/master.flac recap/models/ggml-small.en.bin path/to/whisper-check.json
```

This follows `grants/culturehub-la-2026/macneopolitan/bin/hear.mjs`: decode each
line with local OpenAI Whisper, then compare the raw transcript to the words.
No lyric prompt or forced alignment is supplied. Reports retain raw transcripts,
word-error counts, and the audio hash. Familiar lyrics can be inferred by ASR;
this is a diagnostic, not proof of human intelligibility.

The first choir mix scored 21/52 word edits (40.4%). Alternate source takes are
auditioned separately; a better total does not justify worsening a particular
line. The existing “now I'm found” take differs from the written “now am found”
and contributes an insertion under the literal scoring rule.

Latest hybrid preview: `amazing-grace-choir-v2.mp3` / `.flac`, 127.571 seconds,
−11.6 LUFS / −2.5 dBTP. `hybrid-amazing.py` selects alternate word indices
2/20/21 (how/was/blind) while retaining the original second line. Feed its
`lead-centered.wav` through `LEAD_OVERRIDE` into `bake-amazing-choir.sh` with a
fresh `OUT`; `CHOIR_CACHE` can point to the first choir render to reuse octave
resynthesis. Blind Whisper improves to 17/52 edits (32.7%); the second line and
“was blind” remain unclear to the recognizer. The user approved this exact listening candidate. The later CHOIR_FORWARD render was not played and is not the release master.

Local review files are in `~/Documents/Shelf/amazing-grace-REVIEW-2026-09-23/`
on Frisbee. The DistroKid packet now contains the approved choir-v2 master and close photo crop. The public AC audio has not been replaced.
