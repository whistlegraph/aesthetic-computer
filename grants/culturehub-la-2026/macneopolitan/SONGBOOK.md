# Songbook — the MacNeoPolitan Trio

What the three machines can sing, as of September 23, 2026. Every piece here
has a compose script in `bin/`, a score in `scores/`, and an offline render in
`~/Shelf/macneopolitan-songs/<slug>/` made through Menu Band's own singer
(`bin/hear.mjs` → `singrender`), so it can be heard without a performance.
Word error (WER) is Whisper small.en against the lyric, sung line by line.
Blueberry renders on neo with Tom as a stand-in for Aaron, which is not
installed here; the scores cast Aaron.

Cast: neo = Noelle (Enhanced), speaks at MIDI 59.6, band 57–62. blueberry =
Aaron (Enhanced), 48.8, band 39–53. frisbee = Zoe (Premium), measured three
times today at 55.4–56.1, band 52.5–59.4 — the mezzo, a major third under
Noelle, not a second soprano.

## Rules the day taught

- A sung syllable dies past about 1.4 s; a line dies when its syllables drop
  under about 0.9 s. Long notes belong to the hums. Zoe's "Sophia" is clean
  at exactly one beat a syllable at 69 bpm and turns into "so fear" if the
  last syllable is held longer.
- Pitch holds down to 78 ms a note; syllables only stay words down to about
  a third of a beat. Trills are exact at 0.5 beats, smeared at 0.25, noise at
  0.125. Vibrato at any width is width, not a trill; keep it under 30 cents.
- Whisper hears "the sun is" as "this land is" and, in a low male register,
  "the lid" as "the wood"; "sunlight on your lid" and "my lid" both render
  0 %. "I have been opened" collapses to "I have been a friend"; "they have
  opened me" does not. Probe candidate lines through the renderer before
  putting them in a score; Whisper has real run-to-run variance.
- Mouth percussion: of eight tokens only `dm` renders as a sound; `tss tk ts
  kh ch bm pf mm boh` get spelled. Breath tokens `hah huh uh` work. `ki doom
  mm ee` render voiced and in tune.
- The women's bands overlap only over 57–59: Bulgarian close seconds are this
  trio's native interval. Yodel and Tuvan overtone are not reproducible.
- `hear.mjs` forwards neither the gains nor the gap/legato/sustain/shimmer
  knobs, so offline WAVs are flat and peak-normalised; judge dynamics live.
  It only scores lines with role `lead`.
- Letters are the inverse of words: a sung letter wants a SHORT note, half a
  beat to one beat, written as a bare capital with spaces (`F R I S B E E`);
  held two beats it becomes a vowel drone. Never put a lone letter on a
  line: the engine reads it as "capital B". W needs no help; "dou-ble-you"
  is mush. A sung leading E is heard as A. In the low voice a letter before
  "is" collides ("B is" → "he is"): give it a run-up ("and B is for"). Zoe
  is the trio's speller; Aaron cannot hold a three-letter chunk but is the
  best whole-word voice. Whisper sometimes writes a run unspaced ("OPQRS")
  and scores it 100 % wrong though every letter is there.
- One beat a syllable is the operative floor at every tempo tried (69–100
  bpm); the 0.9 s figure was the 69 bpm reading of the same rule. The 1.4 s
  ceiling stands.
- Numbers need a sentence around them: "two" alone is a howl, "that is two"
  is "that is too", "that makes six" is worse. "the answer is two" and the
  whole equation ("one and one is two") both render 0 %. Whisper writes a
  bare sum as symbols ("1 + 1 = 2"), which the scorer counts as lost words;
  "what is" in front keeps it in prose. "add" is heard as "and"; use "and"
  or "plus". A low entry on A2 eats the first word; start blueberry's
  worded lines on B2 or above.

## Pieces

| slug | title | form | length | WER | notes |
|---|---|---|---|---|---|
| wake | Good morning, Sophia | 3/4 D, 69 bpm, rising | 52 s | 0/24 | the morning bookend; frisbee's first word is her name; revised Sept 23 pm (frisbee into Zoe's band, word holds capped, "sunlight on your lid") |
| lullaby | Lullaby for frisbee | 3/4 D, 63 bpm | 49 s | unmeasured | the sleep lullaby (Sept 22); blueberry recast to Aaron with the cradle at D3 on Sept 23 |
| lights-out | Lights out | 3/4 D, 64 bpm, blueberry drones throughout | 73 s | 0/37 | opens "good night Sophia"; he works past midnight / we stay up till morning / thirteen days / you can sleep now |
| open-me | Open me | 4/4 D, 88 bpm, call and response | 76 s | 0/55 | the lids song; "o · pen · me" passed a syllable per machine, then all three |
| thirteen-days | Thirteen days | 4/4 D minor, 70 bpm, drone | 82 s | 1/66 | blueberry holds exactly thirteen tones; neo and frisbee in fifths and seconds above |
| the-record | The record | 4/4, 104 bpm, syllable hocket | 62 s | 59 % assembled | the numbers the machines keep; a true syllable hocket costs intelligibility — open question whether to hocket by word |
| vocalise | Vocalise | 4/4 D, 90 bpm, wordless | 75 s | n/a | the toolkit proven: trills, turns, pulses, a bass of doom, all three shaking one D chord at bar 20 |
| styling-study | Stylings | six études, 96 bpm, wordless | 165 s | n/a | hocket · drone and close seconds · barbershop tag · bass and ticks · round at one bar · pulsing vowels |
| sums | Sums | 4/4 D, 96 bpm, question · answer · confirmation | 83 s | 0/89 | 1+1 to 5+5; the answer lands a step above the question, the confirmation on the tonic |
| take-away | Take away | 4/4 D, 92 bpm, the mirror of Sums | 87 s | 0/82 | 5−1 down to 1−1; the answer lands a step below; the piece counts itself down and stops where the arithmetic stops |
| spell-frisbee | How to spell frisbee | 4/4 D, 96 bpm | 80 s | 6/121 | neo spells, frisbee spells back, blueberry gives the whole word; B-L-U-E-B-E-R-R-Y passed three letters at a time; a sung leading E is heard as A |
| spell-sophia | Spell Sophia | 4/4 D, 92 bpm | 52 s | 0/73 | S O P H I A, then moon, lid, coffee; blueberry hums under; the rest before coffee's last E is load-bearing |
| abc | A B C | 4/4 D, 100 bpm, not the Twinkle tune | 70 s | 0/108 | the letters climb the trio: blueberry A–G, frisbee H–N, neo O–S, all T–Z; the ladder twice, the second time turned over |
| what-starts-with | What starts with | 4/4 D, 96 bpm, seven rounds | 78 s | 1/79 | neo names the letter, frisbee the word, blueberry both: B blueberry, C coffee, F frisbee, J Jeffrey, M moon, N neo, S Sophia; the one miss is Sophia spelled Sofia |

## Studies

- `STYLINGS.md` — sixteen a cappella traditions and what the trio can take
  from each; ranked: Bulgarian drone with seconds, rounds, hocket, Reich
  pulses, vocal percussion, doo-wop re-voiced for Zoe.
- `VOCALISMS.md` + `bin/vocalisms.mjs` — ornaments (`trill mordent turn shake
  roll gliss pulse`), per-member syllable palettes, `checkBand()`, and the
  measured token table.
- `DOOWOP.md`, `PHONEME-STUDY.md`, `CHORUS-ARRANGEMENT.md` — earlier.

## Open questions for jeffrey

1. The record: keep the syllable hocket as texture (59 % of words survive
   the room) or hocket by word so it reads?
2. Thirteen days opens and closes on "thirteen days" — one too many?
3. Does Zoe tolerate being pushed up better than down? Untested; it decides
   whether frisbee can ever sit above neo.
4. Re-voice the doo-wop miniature now that Zoe measures as the mezzo?
5. First live test tomorrow: the half-beat hocket étude, the best check of
   the conductor's clock correction.
6. Spelling: keep the three-way B-L-U-E-B-E-R-R-Y split with its one shaky
   chunk, or a clean two-way hocket? Score "Sofia" as correct?
7. A B C runs the alphabet twice to reach length; keep, or one pass with a
   longer middle?
