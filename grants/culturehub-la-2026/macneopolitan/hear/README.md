# hear/ — can the words be heard?

One JSON per evaluation run of `bin/hear.mjs`: every sung line rendered
offline by Menu Band's own singer (`singrender`), transcribed by Whisper, and
scored against its lyric as word error rate. These are the measurements the
singing-core tuning rests on, kept so a later change can be compared against
them rather than re-argued.

- `baseline-s.json` — the cast as jeffrey chose it (neo = Noelle Enhanced,
  blueberry = Allison Enhanced), small.en judge, 8 dialogs, 421 words: **18.5%**
  sung against a **3.8%** spoken-source ceiling.
- `v-*.json` — the same scores in another voice. `v-samantha` is **7.8%**:
  the voice turned out to be a bigger lever than any core parameter.
- everything else — one core knob or combination, named for it
  (`gap40`, `sus5m`, `cgain16`, `cmix05`, `vib06`, `loop1`, `sus5b`, `combo*`).
  `loop1` (127.6%) and `sus5b` (24.0%) are recorded failures; leave them here
  so nobody spends the afternoon rediscovering them.
- `align-audit.json` — per-voice accuracy of the synthesizer's word onsets,
  which the core trusts to slice audio onto notes.

Compare two runs with `node bin/hear.mjs --compare A B`; rescore them all
after a change to the scorer with `--rescore`. The WAVs themselves are bulk
and live in `$TMPDIR/mnp-hear/<tag>/` on whichever machine rendered them.
