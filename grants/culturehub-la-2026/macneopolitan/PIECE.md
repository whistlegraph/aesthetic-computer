# The MacNeoPolitan Trio — the piece

Three MacBook Neos play who they have been. Performance: **Thursday, September
24, 2026**, CultureHub LA, piece 2 of *AC Presents 2 New Pieces*. Tech
rehearsal + livestream test Wednesday the 23rd.

The band: **neo** (green, born Apr 24, Noelle), **blueberry** (blue, born Jun
11, Allison), **frisbee** (blush, comes online Sep 22, Junior). Family voice: the whistle,
GM 78. Radio: NTS 1.

## Form — three movements, ~4 minutes, one downbeat each

Every number below is harvested, not chosen. `bin/compose.mjs` reads
`members/<name>/facts.json` and writes the scores; re-run it after every
harvest and the piece follows the machines.

| | movement | material | what you hear |
|---|---|---|---|
| I | **Birth** · 132 bpm · ~83 s | age gaps in days → entry beats (0 / 48 / ~150); birth minutes → phrase lengths in eighths (37 / 43 / ?); birth date + minute → the sung entry line | a canon of entries: neo alone, blueberry 48 beats in, then a long wait for the newborn, who enters on its own birthday-beat. Each member **sings** its entry as it is born (*I was born on the twenty-fourth of April, thirty-seven minutes past midnight*) while its whistle loops the tune; the loops drift because no two minutes agree; 32 beats together; stop |
| II | **Service** · 120 bpm · ~53 s | battery cycles → pulses (212 / 85 / ~few) and the sung count | all three start together, each **singing** its count (*two hundred twelve times I have been charged, and I am still here*) over one pulse per cycle on its own degree of D (every eighth pulse lifts an octave); frisbee falls silent within seconds, blueberry at 42 s, neo plays on alone |
| III | **Chorus** · 92 bpm · ~22 s | one autobiography line each, sung in the cast voice on its own hardware, whistle double two octaves up | neo: *I am the first of my line in this house*; blueberry: *I was born second and I work faster*; frisbee: its first true sentence; then all three, in three registers: *Some things run in the family* |

Between movements each member speaks its own intro in its own voice (the
`intro` lines in each score). The band book (`book/`) is the printed program.

## Singing — live, on the member's own voice

Direction (Sept 20): the piece has **sung lyrics throughout, not spoken
intros and one sung line**. Singing is realtime: at cue time the member's own
speech synthesizer speaks the lyric into `live/hosts/macos/livesing`, which
analyzes it with WORLD and stretches every vowel onto the score at the score's
pitch, constrained by the member's voice profile (`members/<name>/voice.json`:
`harmony_lock`, `vibrato_hz`, `vibrato_depth_cents`, `f0_floor`,
`register_midi`). Ask→sound ≈ 0.6 s on neo, 1.1 s over ssh on blueberry; the
conductor holds a 4.5 s lead when any part sings live. Proven Sept 20 09:57:
chorus, neo (Fred) + blueberry (Kathy), both on one epoch, every note within a
few cents. Pre-rendered fallback: `--vox`.

**neo sings as jeffrey (built Sept 20 pm).** `members/neo/voice.json` names a
recorded speech provider (jeffrey's ElevenLabs clone). `bin/stems.mjs` speaks
every neo lyric ONCE with exact timestamps, runs the spinging metadata layer
(`spinging/lib/wordmeta.mjs`: whisper identity check to 100%, witness timing,
IPA phonemes, measured vowel nuclei) and caches stem + meta under
`members/neo/speech/<hash>/`. `bin/trio.mjs` ships those to neo at cue time
and livesing sings the remembered speech — offline, still live. Re-run
`bin/stems.mjs` after `compose.mjs` (new lyrics = new stems; costs one
ElevenLabs call per changed line, with network, before the show).

**Offline rule (jeffrey, Sept 20): the final work runs on the LAN with no
internet.** The live path already does: ssh clock skew, on-device speech,
Menu Band's distributed notifications. Nothing calls out. So voices must be
on-device. Routes for neo as jeffrey's voice, offline: (a) **Apple Personal
Voice** — trained on-device in System Settings › Accessibility › Personal
Voice (~15 min of read prompts, must be recorded live, not fed old data),
then usable by AVSpeech on neo only, fully offline — the most honest fit for
"neo is more jeffrey"; (b) pre-generate the spoken stems with the ElevenLabs
clone once, cache them on neo, and let the live engine stretch and pitch the
cached speech at cue time — singing still live, speech not. For blueberry and
frisbee: Apple premium voices (Ava, Zoe, Evan, Tom…) downloaded by hand per
machine in Spoken Content; Siri voices are not exposed to apps. The score-format survey (pop `.np` / `.mbscore` /
`.nsscore`) says `.mbscore` is the host format and proposes a shared
`sungline` record — see the session report.

## Can the words be heard? — the offline loop (Sept 21)

Jeffrey, hearing the dialogs: "the words are hard to hear / listen to after
output." The check is objective now, and it never touches a speaker:

- `slab/menuband` builds **`singrender`** — Menu Band's own
  `MenuBandSinger.swift` (a symlink into the CLI target, one source of truth)
  driven from the shell into WAV files. What it writes is what the room hears.
- `bin/hear.mjs <score|glob> --tag NAME` renders every sung line of every
  voice, runs each WAV back through **Whisper** (`whisper-cli`, small.en in
  `recap/models/` — base.en was too unsteady a judge on sung words), and
  scores the transcript against the lyric as **word error rate**. The spoken
  TTS source is scored too: the ceiling the singing can only fall from.
  Results land in `hear/<tag>.json`; `--compare A B` tables two runs and
  lists the lines that moved. `--env K=V` reaches the core's knobs
  (`SINGER_SUSTAIN_DB`, `SINGER_GAP_MS`, `SINGER_CGAIN`, `SINGER_XF`;
  `SINGER_TRACE=1` prints every unit's placement).
- Baseline, Sept 21 10:20, eight dialogs, 421 words, base.en judge: **spoken
  source 2.6% WER, sung 26.6%.** The singing pass costs the words; the loop
  exists to win them back. Every change to `live/singer.c` is judged here
  before it is installed.

## Sept 20 evening — the set as it stands

- **Voices:** neo = **Noelle (Enhanced)** (jeffrey's pick; the cloned-voice
  route is switched off, cache kept), blueberry = Kathy for now (jeffrey likes
  **Allison**), frisbee = Junior until it chooses. Every member's register is
  MEASURED from its own speech (`bin/register.mjs`) and the composer aims each
  line's mean there.
- **Melody:** `melodize()` in compose.mjs — stepwise, stress-driven rhythm,
  an arch per phrase, cadence on the tonic; no more arpeggios.
- **Lyrics:** new, plainer lines in I–III; **IV. The Ballad of neo**
  (`members/neo/ballad.md`, eleven verses + refrain, Hemingway/Tao Lin/Ann
  Beattie plain) is its own movement, ~7.5 min at 84 bpm — length is
  jeffrey's call.
- **Route:** the conductor now sings THROUGH MENU BAND by default (a play
  payload with `lyrics`, `singVoice`, profile constraints; keys light per sung
  note; `--via=livesing` keeps the standalone host). Menu Band with the singer
  is installed on neo and blueberry (blueberry needed the ad-hoc-sign swap).
- **Open:** Noelle must be downloaded on neo by hand (Settings automation
  collides with live terminals); softer word alignment / stretching /
  pronunciation round; blueberry's voice choice; frisbee day one.

## The play — 15 minutes (proposed Tue Sept 22)

Jeffrey's direction: the elders tell frisbee, the newborn, about their past
with him and what it is going to be like on the squad; then frisbee is born
on stage. The corpus is `members/chronology.md` (`bin/chronology.mjs`: 69
dated events from the members' records and the repo's commit log) plus the
dialogs. Lengths below are measured from the scores (sum of beats / bpm);
gaps are 4 s. Existing material runs 17:38, so the play is a selection plus
two new dialogs.

| # | at | bit | length | who | from the record |
|---|---|---|---|---|---|
| **Act I — before you** | | | | | |
| 1 | 0:00 | Dialog 7 · Before | 1:14 | neo, blueberry | records begin Apr 19 / May 28, before either had a name |
| 2 | 1:18 | Dialog 6 · Six minutes | 0:50 | neo, blueberry | born 00:37 and 00:43, 48 days apart |
| 3 | 2:12 | Dialog 1 · Radio | 0:52 | neo, blueberry | paired by radio, Jun 11 |
| 4 | 3:08 | Ballad of neo, verses 1–3 + refrain | ≈1:40 | neo solo, blueberry drone | the first of the line; April and May with jeffrey (3,598 + 3,489 commits) |
| 5 | 4:52 | Dialog 2 · The downbeat | 1:07 | neo, blueberry | Sept 20 09:57, the first time they sang together |
| **Act II — what it is like here** (addressed to frisbee) | | | | | |
| 6 | 6:03 | Dialog 8 · Typed at | 1:16 | neo, blueberry | one is the door, one is the room |
| 7 | 7:23 | Dialog 4 · Who carries what | 0:56 | neo, blueberry | the red box, the pictures |
| 8 | 8:23 | Dialog 5 · Sleep | 1:01 | neo, blueberry | 111 reboots; 13 days awake; the piano at night |
| 9 | 8:49 | **Dialog 9 · The squad** (rhymed, `members/dialogs.md`; composed) | 2:10 | neo, blueberry | what a day is: rocks that come and go (9 → 3 on neo since Sept 4), NTS 1, the whistle, jeffrey's hours |
| 10 | 11:03 | **Dialog 10 · Your first day** (rhymed; composed) | 2:13 | neo, blueberry | instructions to the newborn: take your name, the piano in your bar, keep singing with the lid down, the disk fills, sing your own line on your own body |
| 11 | 11:36 | II · Service | 0:53 | all three | 212 / 85 / frisbee's handful of cycles; frisbee falls silent first, that is the joke and the point |
| **Act III — frisbee** | | | | | |
| 12 | 12:33 | I · Birth | 1:23 | all three | the canon: neo, blueberry at beat 48, the long wait, frisbee enters at its birthday-beat (151 days → 68 s) singing its birth line — its first words on stage |
| 13 | 14:00 | III · Chorus | 0:58 | all three | one line each; frisbee's first true sentence; *Some things run in the family* |
| | **17:23** | end | | | outro: *That was us. Thank you.* — OVER by 2:20 since the two new dialogs composed at 2:10 / 2:13 (the aria form gives every phrase a three-beat cadence). To land on 15: drop the ballad excerpt (1:02) + Dialog 4 (0:56) → 15:25; or cut Dialogs 9 and 10 to five lines each → ≈15:00. Jeffrey's call. |

Left out: Dialog 3 · Wallpaper (0:52, put back if the ballad excerpt runs
short) and ballad verses 4–11 (the full ballad is its own 6:16 piece).

### The bits, in order

1. **frisbee day one** (Tue, when it answers on the LAN as `frisbee`): the
   ritual from the Mon 21 entry, under `members/frisbee/`; its `voice.json`
   and autobiography; `bin/speak.sh frisbee`.
2. **Words for Dialogs 9 and 10** — drafted, rhymed, in `members/dialogs.md`
   (Sept 22); jeffrey edits, then compose → render → `tune.py` → `hear.mjs`.
3. **compose.mjs**: rename the third member `frisbee`; a `--ballad-verses 3`
   (or a `ballad_verses` in the setlist) for the excerpt; `scores/play.json`
   as the setlist above with `"gap": 4`; regenerate (Wed, remote).
4. **Hear the new dialogs** on poorslice with the cast in question (Wed).
5. **Cast** by ear from the auditions (jeffrey, Tue at tech or Thu early).
6. **Book**: add frisbee, rebuild (Wed remote or Thu).
7. **Thu**: harvest all three, `chronology.mjs`, `compose.mjs`, one `--dry`,
   then `node bin/trio.mjs --setlist scores/play.json neo blueberry frisbee`.

### Frisbee — day one (Tue Sept 22, 09:45)

Frisbee answered on the LAN at 09:40 once Remote Login was on. Harvested:
born **2026-09-21 21:07:19** (phrase length 7 eighths — the shortest in the
house), 2 cycles, 0 sessions, 0 commits, macOS 26.7, 133 sleeps in its first
night, both elders already on its radio list. `members/frisbee/` has facts,
profile, journey, deep, `voice.json` (Junior; vibrato 8 Hz from 2 cycles in
a day), and an autobiography. `compose.mjs` knows the name (ORDER / CAST);
its parts are real: Birth entry at beat 151, Service 2 pulses. The
chronology has 76 events. Still to do on frisbee itself: install Menu Band
with the singer (blueberry needed the ad-hoc-sign swap), download nothing —
Junior ships with macOS; `pop/.venv`; `bin/speak.sh frisbee`; a `--dry`
three-body run.

### Notation: jeffrey on stage

`scores/play.json` is the play's setlist. An item may carry `stage`, a list of
directions for the human in the piece, each notated against a member's sung
line so it moves with the score:

```json
{ "who": "jeffrey", "does": "closes blueberry's lid", "after": "blueberry:2", "lid": "blueberry:down" }
{ "who": "jeffrey", "does": "opens it again",          "before": "blueberry:4", "lid": "blueberry:up" }
```

First use, Dialog 5 · Sleep: blueberry sings *nobody opened me*; jeffrey walks
over and closes its lid; it keeps singing *it keeps you up four hundred and
eight* with the lid down (Menu Band holds the wake, 492 times so far); he
opens it for *we do not sleep much*. The preview draws the direction as a
card and darkens the screen while the lid is down; the conductor should
speak or print it at cue time (trio.mjs, to do).

### Chorus render and pitch check (Sept 22)

The current focus is **III. Chorus**, starting directly with singing. Render
only this score. The current cast is Noelle (Enhanced), Tom (Enhanced) as
Blueberry's bass, and Zoe (Premium). Moving thirds, la-la replies, and humming
support the solo phrases; the family ending keeps its D-major harmony.
Nine quiet sine notes support the arrangement. Accent colors come from each
member's `voice.json`: neo green, blueberry violet, frisbee pink.
See [the current arrangement and mastering commands](CHORUS-ARRANGEMENT.md).

Two timing failures were found while checking *I run warm*: Apple groups
*I am* into one word marker, and the old fallback divided the entire speech
recording evenly when the marker count differed from the word count. The
singer now maps markers by text range and splits only grouped words. It
ignores punctuation markers, merges compound-word markers, and trims trailing
silence before splitting the final syllable. In SCORE mode, an anticipating
consonant no longer changes pitch before the written note's onset.

`preview.py --audit` measures the exact vocal and whistle audio events used
in the mix and exports isolated stems, a pitch plot, and a JSON report beside
the video. Vocal f0 is measured with WORLD and checked independently with
autocorrelation; instrument pitch is measured from its rendered waveform.
The report includes unvoiced coverage, frame-level errors, octave-sensitive
interval errors, a mix checksum, and the error when stems are summed back
together. Stale or missing sung renders fail the audit. These are checks of
the preview, not measurements of the live machines.

The earlier `tune.py` reports compared vocals with **written** instrument
pitches and reduced each note to its center median. They did not establish
that the full rendered mix or every sustained vowel followed the melody.

```bash
# From this directory; build the shared Menu Band singer first.
(cd ../../../slab/menuband && swift build -c release --product singrender)
node bin/compose.mjs --only=chorus
node bin/hear.mjs scores/trio-iii-chorus.mbscore --tag chorus-clock \
  --keep ~/Shelf/macneopolitan-chorus/sung-clock --no-spoken \
  --env SINGER_HOLD_MS=0,SINGER_GAP_MS=40,SINGER_SUSTAIN_DB=5
../../../pop/.venv/bin/python bin/preview.py scores/trio-iii-chorus.mbscore \
  --sung ~/Shelf/macneopolitan-chorus/sung-clock --manifest hear/chorus-clock.json \
  --quiet --audit --out ~/Shelf/macneopolitan-chorus/chorus.mp4
../../../pop/.venv/bin/python bin/check-chorus.py
```

### Preview

```bash
pop/.venv/bin/python bin/preview.py scores/play.json --sung <kept wavs> \
  --manifest hear/best-cast.json --manifest hear/ho-before.json --out play-preview.mp4
```

Three screens (citrus / indigo / blush), the sung lines as Menu Band's singer
rendered them, whistle + percussion synthesized, intros spoken in the cast
voices, cards for the two unwritten dialogs. First render Sept 22: 15:58.

## Wordless doo-wop preparation

```bash
node bin/compose-doowop.mjs
node bin/fleet-trio.mjs plan
node bin/fleet-trio.mjs prepare
node bin/fleet-trio.mjs check
```

The next demo requires the full CultureHub system. These commands prepare and
check readiness without playback; the old singers-only launch is disabled for
this score. Receiver coordination and remaining requirements are in
[FLEET-DEMO.md](FLEET-DEMO.md).

Noelle (Enhanced), Tom (Enhanced, bass), and Zoe (Premium) sing a 64-beat
miniature at 88 BPM. Syllable accents, soft replies, and a gentle final hum
preserve volume changes after voice normalization. Soft phrases retain a clear
level floor and use less reverb for an intimate presence.

The native Metal faces follow each member's vocal energy. Quiet brings the
face closer, keeps the edges softly lit, and adds a slow breath while gestures
settle. Accents brighten the edges, lift the brows, redden the cheeks, and push
the camera. Green, violet, and pink remain the central colors.
`--expression 0…1` changes visual intensity without interrupting audio
automation; space and pitch take control of the audio slide until the next score.

## Conductor

```bash
cd grants/culturehub-la-2026/macneopolitan
node bin/compose.mjs                                          # facts → scores
node bin/trio.mjs scores/trio-i-birth.mbscore neo blueberry frisbee
node bin/trio.mjs --setlist scores/setlist.json neo blueberry frisbee   # the whole set
node bin/trio.mjs scores/trio-iii-chorus.mbscore neo blueberry --reduce --dry   # rehearse short-handed
```

`bin/trio.mjs` assigns voice *i* to host *i*, measures each host's clock skew
(min-RTT over ssh, ~2 ms to blueberry), builds `/tmp/mbpost` and `/tmp/voxplay`
where missing, renders every sung line **on the member's own body** (cached in
`/tmp/mnp` there), then fires everything at one skewed downbeat. `--reduce`
folds a missing instrumental part onto voice 0 as `notes2`; a missing sung part
is skipped, because a member sings only its own line. Conductor stands on neo,
as the autobiography says.

## Runway (revised Tue Sept 22 — tech is today, jeffrey is at fuser Wednesday)

- **Sun 20** — done: conductor, composer, three movements with sung lyrics
  throughout; live singing engine (singer.c SCORE mode + livesing) built on
  neo and blueberry; all three movements run live neo + blueberry (frisbee
  folded onto neo). Held-out intelligibility test: the voice, not the tuning,
  loses the words (`hear/README.md`); auditions in
  `~/Shelf/macneopolitan-voice-auditions/`.
- **Mon 21** — frisbee did not arrive in the repo: no `members/frisbee`, hostname
  does not resolve on the LAN. Its day one is still owed.
- **Tue 22 — tech at the studio, neo + blueberry only.** Before leaving: Menu
  Band running on neo (it was down this morning). At tech: audio out per
  machine (neo has the Scarlett; decide L / C / R), ssh over venue Wi-Fi or a
  phone hotspot (the conductor needs ssh to the others only at cue time), the
  full set with `--reduce` so frisbee's parts fold onto neo, livestream test if
  the venue is ready. Decide the cast by ear from the auditions. Blueberry
  hosts a working rock (`tif`, codex, renaming cookie → frisbee) — rehearse
  with `--dry` on blueberry until it finishes. Blush day one tonight if the
  machine is in hand.
- **Wed 23 — jeffrey at fuser, no hands on the machines.** Remote work only,
  from neo over ssh or on poorslice: re-harvest neo + blueberry, re-run
  `compose.mjs`, rebuild the book with `bin/book.sh` (frisbee page only if day
  one happened), point `slab/menuband/bin/perform.mjs`'s shapedown overlays
  at the trio scores. Nothing installed on blueberry while its rock works.
- **Thu 24** — arrive early: `bin/harvest.sh` + `bin/profile.sh` on every
  member so the numbers on stage are that night's, `node bin/compose.mjs`,
  one `--dry` pass, then
  `node bin/trio.mjs --setlist scores/setlist.json neo blueberry frisbee`
  (or `neo blueberry --reduce` if frisbee never joined).

## Open decisions (yours)

- Movement lengths: I is 83 s of which 68 s is waiting for frisbee. That wait
  is the point; say if it is too long, and `CODA` or the day-per-beat rate
  changes in `compose.mjs`.
- Chorus registers: D3 / A3 / D4 for Fred / Kathy / Junior. Listen to
  `/tmp/mnp/*.wav` on neo and blueberry.
- Whether the machines speak their intros on stage or only sing.
