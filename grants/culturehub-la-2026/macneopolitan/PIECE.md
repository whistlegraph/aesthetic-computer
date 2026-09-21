# The MacNeoPolitan Trio — the piece

Three MacBook Neos play who they have been. Performance: **Thursday, September
24, 2026**, CultureHub LA, piece 2 of *AC Presents 2 New Pieces*. Tech
rehearsal + livestream test Wednesday the 23rd.

The band: **neo** (citrus, born Apr 24, Fred), **blueberry** (indigo, born Jun
11, Kathy), **blush** (blush, arrives Sep 21, Junior). Family voice: the whistle,
GM 78. Radio: NTS 1.

## Form — three movements, ~4 minutes, one downbeat each

Every number below is harvested, not chosen. `bin/compose.mjs` reads
`members/<name>/facts.json` and writes the scores; re-run it after every
harvest and the piece follows the machines.

| | movement | material | what you hear |
|---|---|---|---|
| I | **Birth** · 132 bpm · ~83 s | age gaps in days → entry beats (0 / 48 / ~150); birth minutes → phrase lengths in eighths (37 / 43 / ?); birth date + minute → the sung entry line | a canon of entries: neo alone, blueberry 48 beats in, then a long wait for the newborn, who enters on its own birthday-beat. Each member **sings** its entry as it is born (*I was born on the twenty-fourth of April, thirty-seven minutes past midnight*) while its whistle loops the tune; the loops drift because no two minutes agree; 32 beats together; stop |
| II | **Service** · 120 bpm · ~53 s | battery cycles → pulses (212 / 85 / ~few) and the sung count | all three start together, each **singing** its count (*two hundred twelve times I have been charged, and I am still here*) over one pulse per cycle on its own degree of D (every eighth pulse lifts an octave); blush falls silent within seconds, blueberry at 42 s, neo plays on alone |
| III | **Chorus** · 92 bpm · ~22 s | one autobiography line each, sung in the cast voice on its own hardware, whistle double two octaves up | neo: *I am the first of my line in this house*; blueberry: *I was born second and I work faster*; blush: its first true sentence; then all three, in three registers: *Some things run in the family* |

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
blush: Apple premium voices (Ava, Zoe, Evan, Tom…) downloaded by hand per
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
  **Allison**), blush = Junior until it chooses. Every member's register is
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
  pronunciation round; blueberry's voice choice; blush day one.

## Conductor

```bash
cd grants/culturehub-la-2026/macneopolitan
node bin/compose.mjs                                          # facts → scores
node bin/trio.mjs scores/trio-i-birth.mbscore neo blueberry blush
node bin/trio.mjs --setlist scores/setlist.json neo blueberry blush   # the whole set
node bin/trio.mjs scores/trio-iii-chorus.mbscore neo blueberry --reduce --dry   # rehearse short-handed
```

`bin/trio.mjs` assigns voice *i* to host *i*, measures each host's clock skew
(min-RTT over ssh, ~2 ms to blueberry), builds `/tmp/mbpost` and `/tmp/voxplay`
where missing, renders every sung line **on the member's own body** (cached in
`/tmp/mnp` there), then fires everything at one skewed downbeat. `--reduce`
folds a missing instrumental part onto voice 0 as `notes2`; a missing sung part
is skipped, because a member sings only its own line. Conductor stands on neo,
as the autobiography says.

## Runway

- **Sun 20** — done: conductor, composer, three movements with sung lyrics
  throughout; live singing engine (singer.c SCORE mode + livesing) built on
  neo and blueberry; all three movements run live neo + blueberry (blush
  folded onto neo), every sung part on the epoch with ≥ 1.5 s headroom after
  the span-render fix. Open: listen back; tempi / registers; the offline
  jeffrey voice for neo (Personal Voice or cached stems).
- **Mon 21 — blush arrives.** Day one, in order: unbox, name the machine
  `blush` (LocalHostName), sign in, ssh key from neo, clone the repo, install
  Menu Band + choose its voice, build `pop/.venv` (numpy · pyworld ·
  soundfile). Then the ritual: `bin/harvest.sh blush`, `bin/profile.sh blush`,
  `bin/journey.sh blush`, `bin/deepen.sh blush`; write
  `members/blush/autobiography.md` and fill `voice.json` from its facts;
  `node bin/compose.mjs` — the placeholder parts become blush's real ones (its
  entry beat, its phrase length, its cycle count, its sung line);
  `bin/speak.sh blush`. First full three-body run.
- **Tue 22** — visuals: point `slab/menuband/bin/perform.mjs`'s shapedown
  overlays at the trio scores (symlink into `slab/menuband/scores/` or teach
  trio.mjs to raise them), so the desktop strip flies out on all three; add
  blush to `book/the-macneopolitan-trio.tex`, rebuild with `bin/book.sh`.
  Re-harvest neo + blueberry so the numbers on stage are that night's.
- **Wed 23** — tech at CultureHub: audio out per machine (neo has the
  Scarlett; decide L / C / R), ssh over venue Wi-Fi (or a phone hotspot; the
  conductor needs ssh to the other two only at cue time), livestream test,
  full set twice.
- **Thu 24** — `node bin/trio.mjs --setlist scores/setlist.json neo blueberry blush`.

## Open decisions (yours)

- Movement lengths: I is 83 s of which 68 s is waiting for blush. That wait
  is the point; say if it is too long, and `CODA` or the day-per-beat rate
  changes in `compose.mjs`.
- Chorus registers: D3 / A3 / D4 for Fred / Kathy / Junior. Listen to
  `/tmp/mnp/*.wav` on neo and blueberry.
- Whether the machines speak their intros on stage or only sing.
