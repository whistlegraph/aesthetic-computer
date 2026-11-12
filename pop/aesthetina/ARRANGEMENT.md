# aesthetina — the arrangement

**Spelling is composition.** Every layer is a word typed at some speed, octave
or offset. Melody, harmony (three-letter chord words), bass (the hook slowed
down), hats (Euclidean keystrokes) and choir (letter names sung on their own
pitches) all come out of notepat's key map
(`disks/notepat.mjs` `NOTE_TO_KEYBOARD_KEY`). The score is the text.

128 BPM · 80 bars · 2:30 · A pedal. `bin/render.mjs` is this document as sound.

## Form

Phrases run 8/8/12/8/16/4/16/8. The stillness falls at 0.36 and the drop at
0.71. Each phrase changes one lane.

| bars  | section   | what happens |
|-------|-----------|--------------|
| 0–8   | boot      | hook typed as switch only; one more letter gains breath each bar |
| 8–16  | lift-off  | spacebar kick (fill into it); hats E(7,16); toots; no bass yet |
| 16–28 | A1        | sub A1 enters; hook every 2 bars; counter on the hook's complement; mid vessel plays the hook at 4× augmentation |
| 28–36 | stillness | drums + bass out; A major ("tea") held; voice sings a e i o u on the aeiou pitches |
| 36–44 | B1        | chord words every 2 bars: tea · daw · rib · ace; konnakol scat |
| 44–52 | B2        | chord words every bar; E(7,12) bembe clicks against the 4/4 |
| 52–56 | prepare   | kick + bass out; typing rolls climb the tuplet ladder 1→8 per beat |
| 56–72 | drop      | everything: prolation canon, "vespertine" second typist, letter-name choir, E(9,16) hats; from bar 64 ace · fad · big · tea |
| 72–80 | outro     | the opening at half density; "adieu" typed; one spacebar alone at the end |

## Typed material (notepat pitches)

| word | notes | role |
|------|-------|------|
| aesthetina | A4 E4 D♯4 C♯5 C5 E4 C♯5 D5 B5 A4 | hook; its 2+3 gap pattern is the clave |
| aesthetic computer | — | counter, on the hook's complement steps |
| tea · ace · daw · fad · rib · big | A · Am · D · Dm · G♯° · G | chord words, strummed |
| vespertine | C♯4 E4 D♯4 A♯5 E4 G♯4 C♯5 D5 B5 E4 | second typist in the drop (heterophony) |
| aeiou | A4 E4 D5 G♯5 F♯5 | stillness vowel melody |
| adieu | A4 D4 D5 E4 F♯5 | outro, lands open |

Harmony: B goes major→minor (tea daw rib ace); the drop mirrors it (ace fad big
tea), a Picardy return onto the hook's C♯.

Prolation canon (Pärt, *Cantus*): lead 1×, mid vessel 2× an octave down, bass
vessel 4× two octaves down. The sub never leaves A1.

## Rhythm

- hats: downstrokes E(7,16), upstrokes on its complement, so every 16th is struck once. The drop moves to E(9,16).
- claps: four or five keys mashed at once, on beats 2 and 4. The brief suggested E(5,8) here; the render keeps 2 and 4.
- kick: four on the floor; the last bar of each 8 in A1 is E(3,8).
- B2: E(7,12) on key clicks, 3:4 against the kick.
- scat: konnakol 5+3+3+5 — *ta din gi na TOM · ta ki ta · ta ki ta · ta din gi na TOM*.

## Voice

Toots in A1 and the drop. Letter-name choir in the drop (*ay ee ess tee aych
ee tee eye en ay*, each on its letter's pitch). Chord words sung as one
syllable on the triad, three copies spread 0/28/56 ms. Konnakol in B. Vowels
in the stillness. "ay dee eye ee yoo" in the outro. Dub delay 0.93 beat on the
toots, dotted-eighth echo on the chord words.

## Sources

`papers/rhythm-platter/` (timelines.json; digest 02, 06, 09) ·
`papers/laptop-platter/` · `papers/bass-platter/digest/01-dub-bass.md`,
`02-wub-bass.md` · `papers/chamber-platter/digest/01, 03, 04` ·
`papers/arxiv-comp-strats/comp-strats.tex` ·
`papers/arxiv-laptop-performance/laptop-performance.tex`
