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
- `align-audit.json`, `align-<Voice>.json` — per-voice accuracy of the
  synthesizer's word onsets, which the core trusts to slice audio onto notes.
  75 to 88 ms for every voice, while their word error spans 7.8% to 39.4%:
  alignment is not what loses the words.
- `ho-*.json` — the HELD-OUT set: the trio movements and the ballad, 687
  words the tuning never saw. This is where the tuning fell down. Read the
  warning below before trusting any number in this directory.

## The warning

Runs here are exactly reproducible — three identical configurations scored
15.2% (64/421) to the word — so differences between them are real and not
noise. That makes it very easy to fit the corpus instead of the problem, and
that is what happened: the settings that took the eight dialogs from 18.5%
to 13.1% made the held-out trio movements WORSE, 11.9% to 13.4%. The dialogs
are 3/4 arias whose every phrase ends on a three-beat cadence, and every knob
that "won" was a knob that manages a long held note. None of it was adopted;
the core's defaults are unchanged and the piece sounds as it did.

What did survive held-out is the voice. Samantha halves the errors on both
corpora and Moira nearly does. Any future change should be judged the same
way — tuned on one corpus, believed only after the other agrees.

Compare two runs with `node bin/hear.mjs --compare A B`; rescore them all
after a change to the scorer with `--rescore`. The WAVs themselves are bulk
and live in `$TMPDIR/mnp-hear/<tag>/` on whichever machine rendered them.
