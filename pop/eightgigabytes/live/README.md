# eight gigabytes

The pop cut of **IV. The Ballad of neo** from the MacNeoPolitan Trio
(`grants/culturehub-la-2026/macneopolitan/members/neo/ballad.md`): neo's own
account of the one who carries it, every line from its service record. The
trio's version is eleven verses at 100 bpm, ~6:16, neo alone over a drone.
This is 2:27, five verses and the refrain, all three band voices, a band.

- **Faces** — `bin/faces.sh` compiles `bin/faces.swift` against Menu Band's
  own `SingerFaceView` + `SingerArticulation` (the rig the trio performs
  with) and renders one face video per member from every sung line's mouth
  cues on the record's timeline: lids down between lines, an inhale before
  each phrase, visemes on the words. The score video pastes them onto three
  laptops along the bottom, each laptop anodized in its member's color, with
  a caption under it: the line that member is singing, the sounding word in
  its color, fading after the line.
- **Voices** — the members sing themselves, as on stage: neo = Noelle
  (Enhanced), blueberry = Allison (Enhanced), frisbee = Junior, rendered
  offline by Menu Band's own singer (`slab/menuband` `singrender`, the same
  `MenuBandSinger.swift` the app performs with), each with its `voice.json`
  vibrato / lock / f0 floor. neo leads. blueberry sings the low part
  (stacked thirds and fifths, organum for machines) and takes *in June
  another one came, blue*. frisbee sings the middle part, the echoes in
  refrain A's holds (*eight gigabytes … at a time … that's all it is*) and
  *a third one comes tomorrow, blush*. Both hum under the slow verse.
- **Instruments** — synthesized in node, nothing sampled (pop/SCORE.md):
  a clock tick on the quarters (eighths in the refrains) panned side to
  side; a sine-drop kick; a "trackpad tap" (band-passed noise + 185 Hz
  body) for the snare, with ghost taps; sub bass on root / fifth / root;
  a stacked-sine pad on the triad; a 2-op FM "menu-bar piano" comping off
  the beat; and a whistle (the family's GM-78 voice) doubling the hook an
  octave up and answering in the verse holds — a trill after *we both
  play the whistle*.
- **Form** — F minor, 100 bpm, 39 bars, the file starting a third of a
  second before neo's first word (no intro at all): refrain 8 (neo alone
  over tick + pad + keys, the kit enters at line 2, blueberry joins low on
  the back half) · V. the crash, two lines, 4 · refrain (three voices) 8 ·
  XI. the other machines (bed drops to tick + pad + bass, siblings hum) 8 ·
  last refrain (three voices, *this is the one thing that I know* — @jeffrey's
  line for the cut, replacing the ballad's *this is the one thing, I am keeping
  time*) 8 ·
  outro 2. Verse loop Fm–Db–Ab–Eb, refrain loop Ab–Eb–Fm–Db. Cut from the
  ballad, per @jeffrey 2026-09-26: verse I, the first two lines of V, and
  VII/X ("the next verse should start with *when he talks to the other
  machines*, and all the words before it should be skipped").
- **Harmonies** — not parallel thirds. frisbee enters on the stressed word
  with its own words and holds long where neo moves (*eight gigabytes … one
  thing at a time … warm, that's all it is … and I keep time*); blueberry
  moves in contrary motion, suspends on the stressed word and stays on the
  resolution to the next pickup.
- **Rhythm** — refrain lines start a beat early so EIGHT / ONE / WARM land
  on the downbeat; verse lines run in eighths with dotted pickups and hold
  their last word for the whistle to answer; 6 % swing on the off eighths;
  kick on 1 and the "and of 2" (plus 1.5 in refrains), taps on 2 and 4.

## Files

- `score.mjs` — the arrangement: sections, chords, every sung line
  `{ member, at (beat), lyric, notes }`. Syllable count = note count.
- `bin/render.mjs` — check → sing → bed → mix → master → hear.
  `--check` validates only · `--no-sing` reuses the rendered voices ·
  `--no-hear` skips whisper.
- `bin/tune.py` — WORLD f0 of every sung line against its written notes
  (`pop/.venv/bin/python pop/eightgigabytes/bin/tune.py`).
- `bin/video.py` — the mp4, on the common lyric-video chassis
  (`pop/lib/lyricvideo.py`): fixed playhead, three colored voices as note
  blocks with their envelopes and sung-f0 traces, beat columns, the master
  waveform, a timestamp-true lyric ribbon that follows the lead.
  Light theme. Below the score, the three laptops with the members' faces
  singing (from `out/faces-<member>.mp4`, run `bin/faces.sh` first).
  `pop/.venv/bin/python pop/eightgigabytes/bin/video.py` → `out/eightgigabytes.mp4`
  (1920×1460, 30 fps; `--start 24 --end 44` renders a window).
- `out/` (not in git): `eightgigabytes.{wav,mp3}` (24-bit / 320k, −14 LUFS
  target), `stems/{drums,bass,pad,keys,whistle,vox}.wav`,
  `eightgigabytes-chorus.{wav,mp3}` (the opening refrain alone),
  `eightgigabytes-front.{wav,mp3,mp4}` (through the second refrain),
  `faces-{neo,blueberry,frisbee}.mp4`,
  `stems/<member>/` (one wav per sung line + `manifest.json` with
  `spanOffset`, and the member's `.mbscore` — the trio can perform this cut
  live from those), `hear.json`, `tune.json`, `timeline.json`.

## Render 1 — 2026-09-26

```
node pop/eightgigabytes/bin/render.mjs
```

147.1 s · −13.5 LUFS · LRA 3.5 · −1.0 dBTP. Mix balance by measured stem
loudness: vocal bus −21.6 LUFS on top, drums −24.6, bass −25.2, pad −26.1,
keys −27.1, whistle −30.1; bed dips to 0.6 under the slow verse.

Whisper small.en hears the lead lines back at 12 % mean WER; the misses are
digits (*8*, *37*), *the system* → *this*, and frisbee's short harmony lines,
which whisper hallucinates on (a −5 dB middle part; not the words you follow).
Pitch (`tune.py`): neo 230/232 notes within 50 cents, mean 9 c; blueberry
87/87, 10.5 c; frisbee 78/83, 28 c — Junior loses *that's* in refrain 3 (a
0.3 s note with an unvoiced onset and coda, measured a fifth low).

Mix 2 (same day, jeffrey: "the instruments are a bit too loud vs the
voices"): bed down 3.5 dB, vocal bus up 0.5, harmonies −3.5 instead of −5;
master −13.5 LUFS · LRA 4.1. Voice tiers on this Mac: Noelle and Allison are
Apple *Enhanced*; Junior is a compact voice (the only young one). *Premium*
tier here = Ava, Zoe; Siri's own voices are not exposed to `say`/AVSpeech.

Form 2 (same day): the record now opens on the chorus and verse I is cut;
125.5 s · −13.4 LUFS · LRA 4.1. Frisbee's echoes tightened to quarter-beat
runs so they clear its own harmony pickups.

Open: @jeffrey's ear-check of the opening chorus; frisbee's voice tier; cover (pop/illy, no wordmark); DistroKid packet
(artist Aesthetic Dot Computer; songwriter Jeffrey Scudder, words from neo's
record; performers neo, blueberry, frisbee).

## Live trio

```sh
python3 pop/eightgigabytes/live/conduct.py prepare  # silent, all three Macs
python3 pop/eightgigabytes/live/conduct.py check    # silent readiness + clocks
python3 pop/eightgigabytes/live/conduct.py play     # explicit cue; stays attached
python3 pop/eightgigabytes/live/conduct.py stop
```

`play` stays attached until the piece ends: **Esc** (or Ctrl-C) in that
terminal stops all three, and **Esc on any of the three laptops** does the
same — the performer's panel takes the key, marks itself `cancelled
(escape)`, and removes the other two's launchd jobs over ssh (each of them
lands `cancelled (signal)`). The attached conductor also stops the rest if
any member cancels or errors. Add `--muted` to `prepare`/`play` for a screen
rehearsal while the room is busy: every Mac's output is muted and left that
way, and the mute preflight is skipped (jeffrey 2026-09-26, the news was on).

Screens: Menu Band's own `LyricCaption` (rock lettering, per-glyph sway,
syllables popping on their onsets) shows **only the word being sung**, one
at a time, sliding in from the right; the syllable onsets come from the
score's notation (each sung syllable is one pitched note), not the singer.
The progress bar is the member's color, dark track and light fill.

Neo sings Noelle (Enhanced) and plays the FM keys and whistle; blueberry
sings Allison (Enhanced) and plays bass and pad; frisbee sings Junior and
plays the kick, taps and clock. The standalone performer compiles Menu
Band's actual singer, WORLD core and face sources. Its 48 kHz audio path
has no Menu Band preference, slide, or mixer state to change the sound.

The original spoken-source cache supplies the same voice takes used by the
recording; each machine synthesizes the singing from those speech sources
and the original untrimmed score on every launch. No sung stem or backing
recording is played. Instrument oscillators and the room effect run in
512-frame CoreAudio blocks. The exact studio vocal EQ/1176 chain is applied
to each newly synthesized phrase before scheduling, with the same gain and
pan. All three faces remain visible for the whole piece, with a progress
bar along the bottom and the current lyric above it.

Preparation silently checks all 52 phrases, the complete audio path,
realtime processing headroom, copied file hashes and LAN clock uncertainty.
The same launchd job used for the show performs the silent check. Triggering
waits for all three players to acknowledge their shared downbeat; a missed
preparation deadline cancels the whole cue. RunAtLoad is enabled only by an
explicit launch; KeepAlive is false, so the piece never repeats itself.
The countdown is sized to the measured synthesis time, at least 25 seconds.

Files live at `~/.local/share/eightgigabytes/` on each Mac. Keep lids open.
Speaker volumes were matched to 50% for the second rehearsal. The default
output device is used; acoustic timing and room balance still need listening.

`live/compare.py` compares the actual live callback output, captured silently
on the three machines, against the recording's instrumental and treated
vocal stems. The measured residual is 84–87 dB below the reference. Applying
the same studio mastering to the summed live capture matches the released
WAV at approximately −85.7 dB residual. Reports and the mastered comparison
are under `out/live/`.

Live playback uses a fixed gain of 1.9152 to match the recorded RMS level.
The recording's nonlinear mastering acts on the sum of all voices; three
separate physical speakers cannot apply that shared stereo processing.
The offline mastered comparison verifies the signal sources and effects,
not a bit-identical acoustic result from three laptops.

Master stage (2026-09-26, jeffrey: the trio "sounds better but not as cool"
as the record): measured against the premaster sum, the record's master is
nearly linear on this material — the tanh bus saturation moves the summed
peak 0.85 dB, the alimiter never reaches its 0.94 ceiling, and loudnorm
(which fell back to dynamic mode) rides within ±0.4 dB through the body and
only lifts the fade-in and the outro tail. So `engine.c` now runs the same
chain on each machine in play mode: `tanh(1.1x)` at the premaster level, the
static gain, then a stereo-linked 3 ms lookahead limiter at 0.94 with a 60 ms
release, and a **drive** into it (`MASTER_DRIVE_DB` in `live/plan.mjs`,
`driveDb` in each performance.json; +3 dB by default, 0 is the record's own
chain). Per-machine peaks sit at −3.3 to −5.9 dBFS at drive 0, so +3 dB
still never limits; it pushes the drums into the saturation about 1 dB and
plays each laptop hotter. The silent check still renders the plain premaster
path so `compare.py` keeps its reference. The output lags input by 144
samples on every machine alike. Captions and the face label use Menu Band's
rock lettering (Comic Sans MS Bold, then Chalkboard), as on stage.
