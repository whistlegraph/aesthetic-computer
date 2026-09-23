# A cappella stylings for three laptops

Unaccompanied vocal traditions, read for what three synthesized voices can take
from them. The trio is not a choir: three monophonic singers with hard pitch
lock, one syllable per note, no breath, no register break. That rules some out
and makes others easier than they are for people. See also `DOOWOP.md`,
`PHONEME-STUDY.md` and `CHORUS-ARRANGEMENT.md`; the proof score is
`scores/trio-styling-study.mbscore`, from `bin/compose-styling-study.mjs`.

## The cast, measured

| member | voice | speaks (MIDI) | band | usable sung | color |
|---|---|---|---|---|---|
| neo | Noelle (Enhanced) | 59.6 (C4) | 57–62 | 54–66 | `#8FD13F` |
| blueberry | Aaron (Enhanced) | 48.8 | 39–53 | 43–55 | `#5A57D3` |
| frisbee | Zoe (Premium) | **55.4 (G3)** | **52.3–59.3** | 50–62 | `#F2A7B9` |

Zoe was unmeasured before today: sixty words of plain prose through
`say -v "Zoe (Premium)"`, then `pyworld.harvest` on the 22.05 kHz WAV — 2322
voiced frames of 2669, median 200.3 Hz = MIDI 55.38, p10 52.33, p90 59.26, sd
2.80. She is not a second soprano. She sits a major third below Noelle and a
minor seventh above Aaron, so the group is soprano, mezzo, baritone with the
women's bands overlapping only over 57–59. Close seconds are native here.

## Measured limits

Rendered offline through `singrender` at 96 bpm with `singLock: 1`, then read
back with `pyworld.harvest` against the scored grid and whisper-small.en.

| note length | at 96 bpm | notes rendered | in tune (<50¢) | syllables heard |
|---|---|---|---|---|
| 0.5 beat | 312 ms | 8/8 | 8/8 | 8/8, 0% WER |
| 0.375 beat | 234 ms | 12/12 | 12/12 | 12/12, 0% WER |
| 0.25 beat | 156 ms | 16/16 | 16/16 | 11/16, 31% WER |
| 0.1875 beat | 117 ms | 16/16 | 16/16 | none — one gesture |
| 0.125 beat | 78 ms | 16/16 | 16/16 | none — a trill |

Two floors, far apart. **The pitch floor is below 78 ms:** at every duration
`notesUsed` equalled `noteCount`, and each 78 ms slot's median pitch landed
within 5 cents of the score, so the trio can trill and hocket faster than a
person can. **The syllable floor is 0.375 beat, 234 ms:** below it whisper
hears eleven of sixteen `doo`s, then a smear it labels "*evil laughter*".

**Unvoiced onsets break the lock.** `ta` at 78 ms held pitch in 1 slot of 16,
median error 2.14 semitones, recovering to 14/16 at 156 ms. Voiced onsets are
immune — `doo`, `dm`, `doom`, `ki` each 16/16 at 78 ms. Anything under ~200 ms
must begin on a voiced consonant or a vowel.

**Timing is sample-accurate.** Each line's WAV carries a lead-in of 0 to 1.03 s
before its first note, but `spanOffset` plus that lead-in equals the scored beat
every time. Hocket works offline; three host clocks are a live question.
**No portamento:** one syllable, one note, one pitch, so a glide becomes an
audible staircase. **New tokens:** `ki` and `doom` clean at 78 ms.

**Plosive instability is density, not the token.** In a run of sixteen ticks
`tk` held 7/16 and `ta` 1/16; the same `ts` and `tk` spaced a beat apart in the
study held 10/10 in tune, median error 0.11 semitones. Isolated ticks are
pitched, crowded ticks are noise, and either can be what the part wants.

**`lineGains` and `noteGains` do not reach the offline render.** `hear.mjs`
sends `singrender` only notes, lyrics, voice, vibrato, lock and f0 floor; every
line returns normalized to peak 0.800 with a flat envelope, and a scored swell
of [.30 .52 .78 1 1 .78 .52 .30] measured 0.87 1.00 0.85 0.83 0.92 0.83 0.97
0.34. Judge any gain-built device through `bin/preview.py` or live.

**The study itself:** 43 lines across six études, all rendered, no retries,
`notesUsed` equal to `noteCount`, every scored note in tune to under 50 cents.
## The traditions

### Doo-wop backing

Nonsense syllables carry a progression under a lead over a walking bass, chosen
for attack and vowel colour. Built already (`DOOWOP.md`) — but that miniature
wrote Zoe beside Noelle; at 55.4 she should hold the inner third, freeing
Noelle to sit on top and Aaron to walk lower. **Recipe.** Aaron `shoo boom ba
dum dum` on roots and fifths 43–50; Zoe inner on `ooh`/`wah` 54–59; Noelle
`doo wah` 59–66; swap the upper two second time.
**Risk.** Swing as 2/3 + 1/3 at 96 bpm puts the short syllable at 208 ms.

### Barbershop

Four-part close harmony in root position on the barbershop seventh, tuned until
a fourth ringing tone appears; a tag holds one voice on a long "post" while the
others swipe between chord tones on one vowel. The ring needs just intonation
and four voices, so there is none here; the post is what this engine does best,
the swipe a glide it cannot make. **Recipe.** Noelle posts `ooh` at 62 eight
beats while Aaron steps 50→48→47→45 and Zoe 57→56→54→53, landing on 50/54/62.
**Risk.** The swipe re-articulates each step. Call it a stepped cadence.

### Bulgarian women's choir

Bright chest resonance, drones, and seconds held until they beat; the shopska
attack is a glottal shout on a downbeat. Best fit in the survey — two bands
overlapping by three semitones over a bass who can drone forever — and equal
temperament helps, since a locked second beats steadily instead of wandering.
**Recipe.** Aaron drones D at 50 on `ooh`; Zoe 57 and Noelle 59 on `ah`, walking
59/57 → 61/59 → 62/60 and crossing at the phrase end. **Risk.** No shopska: the
shout needs a transient the engine will not make. Hold the pair at 0.32 gain.

### Sacred Harp / shape-note

Four-part congregational singing from shape-note books, open and unblended,
full of the parallel fifths later theory forbade; a fuging tune enters the
voices one at a time before they converge. Open fifths are easy and sound
right, because unblended is what this engine is. **Recipe.** No thirds — Aaron 43,
Zoe 55, Noelle 62 in parallel; for the fuge, Aaron, then Zoe a bar later, then
Noelle. **Risk.** One vibrato rate makes an organ stop. Split 4.5/3.5/5 Hz.

### Meredith Monk and Björk's *Medúlla*

Monk built a vocabulary of non-lexical gestures — clicks, ululations, keening,
syllables invented for a character; *Medúlla* gave voice every job an
arrangement usually gives instruments, breath and throat noise included. With a
small phoneme set and no breath, what transfers is the structural half: a
wordless language used consistently enough to read as one. **Recipe.** Six
tokens, two per member — Noelle `loo`/`ah`, Zoe `ki`/`ooh`, Aaron `dm`/`doom` —
then swap once, late. **Risk.** Without breath it is a patch, not a body.

### Inuit katajjaq (throat games)

Two women stand face to face trading short voiced and unvoiced patterns on
alternating inhale and exhale until one laughs, the patterns offset rather than
identical. The interlock is reproducible and the breath is not, so half the
sound is gone by construction — but the 78 ms pitch floor lets the trio
interlock faster than the game is played. **Recipe.** Zoe and Noelle alternate
0.25-beat notes offset by one, Zoe `ki`, Noelle `hmm`; Aaron `dm` at 45 every
other beat; no cadence, stop abruptly. **Risk.** Plosives would cost the pitch.

### Yodel

Rapid alternation of chest and head register with the break made audible and
rhythmic — the yodel is the timbre change, not the interval. **Not
reproducible:** these voices have one register. A neural voice shifted past its
band does not flip into falsetto, it croaks; measured on blueberry on 23
September, where Tom a fifth below its speaking pitch ribbited. **Recipe.** None; if the interval is wanted for itself, give it to Noelle
between 54 and 66 and call it a leap. **Risk.** Otherwise it croaks.

### Tuvan overtone singing

A drone sung with the tract shaped so one harmonic rises far above the rest,
producing a whistling melody over a fixed fundamental. **Not reproducible:** it
needs independent control of formant trajectory over a held f0, and the
pipeline shifts a recorded utterance to a target pitch while preserving
formants rather than steering them. No filter bank reaches the score.
**Recipe.** Aaron holds 45 (A2, 110 Hz) under a very quiet Noelle at 69 (A4).
**Risk.** Calling that overtone singing would be a lie.

### Ligeti, *Lux Aeterna* — micropolyphony

Sixteen voices in canons so dense, at such close intervals and staggered
entries, that no line is audible and only the moving cluster is. Three voices
cannot make a cluster, but they can run the mechanism small: one figure at
three speeds and three offsets until the ear stops following any of them. Hard
lock and no vibrato make it more static than the original, which is the
interesting part. **Recipe.** One eight-note figure, Noelle at 0.375 beat, Zoe
0.4375, Aaron 0.5, gain 0.30. **Risk.** One vowel at three rates detunes.

### Reich, *Music for 18 Musicians* — pulsing vowels

Two voices breathe a chord in and out over a held pulse, entering from nothing
and leaving the same way, so it appears without an attack and the phrase length
is set by breath. Reproducible in shape, not mechanism: `noteGains` writes the
swell as short repeated notes under a rising then falling envelope, and on a
sustained vowel the re-articulation passes as a pulse. **Recipe.** D major,
Aaron 50, Zoe 57, Noelle 62, eight 0.5-beat `ooh`s each at gains
[.3,.5,.75,1,1,.75,.5,.3], staggered. **Risk.** Inaudible in an offline render.

### Bobby McFerrin — one voice, bass and melody

A single singer alternates a chest bass note with a melody above, fast enough
that the ear hears two parts. The trick is unnecessary with three voices; what
transfers is the voicing — a short struck bass with a wide gap above it — and
the fact that one member alternating 43 and 55 at 0.375 beat splits into two
audible lines alone. **Recipe.** Aaron alone four bars on 45 (`doom`) and 55
(`bah`) at the half beat, then Noelle takes the melody and Aaron keeps the low
note. **Risk.** 55 is where Aaron's croak lives. Cap him at 52 if it appears.

### Pentatonix-style vocal percussion

A beatboxer covers the kit with mouth sounds: a closed kick, a lip or tongue
burst for the snare, a clipped sibilant for the hat, unpitched and outside the
harmony. This is the one place the engine's weakness is the feature — unvoiced
onsets lose the lock when crowded (`tk` 7/16, `ta` 1/16 in a dense run), which
is what an unpitched sound is. **Recipe.** Zoe takes the kit, `ts` offbeat and `tk` on the
back half at 0.25 beat, 55–57, gain 0.30; Aaron kicks `doom` at 45 on 1 and 3;
Noelle rides `bah` above. **Risk.** Slowed, they recover vowel colour and words.

### Medieval hocket and organum

Organum doubles chant at a fixed interval in parallel; hocket cuts one melodic
line between voices so each sings fragments and the line exists only in the
sum. Both fit: organum needs no blend, hocket needs only timing, which the
offline render has to the sample. The catch is that the bands span an octave
and a half, so a hocketed melody is necessarily compound. **Recipe.** Eight
notes at 0.5 beat rotating Noelle → Zoe → Aaron: 62,
57, 50, 64, 59, 52, 62, 57 on `doo`. **Risk.** Live, a bad clock shows here.

### Rounds and canons

One tune, every voice, each entering a fixed distance behind the last, written
so every vertical combination consonates. The easiest thing here: the only
constraint is a tune transposable into three bands nearly two octaves apart, so
keep its span inside a fifth. Octaves keep the canon exact; a fourth or fifth
makes it a canon at the fifth and keeps everyone centred. **Recipe.** A
four-bar tune in D spanning 57–64 on `loo`, Noelle first, Zoe a bar later down
a fifth, Aaron a bar after down an octave. **Risk.** Check every vertical pair.

### Call and response

A leader sings a phrase and a group answers, repeating or completing it; the
answer is shorter and rhythmically fixed while the call varies. The register
spread helps — a call from Noelle answered by Zoe and Aaron an octave and a
fifth below reads as two different bodies — and the answer can be a real
two-voice chord instead of a unison. **Recipe.** Noelle calls two bars, 59–64,
on `loo`; Zoe (54) and Aaron (50) answer one bar on `bah` at 0.375 beat.
**Risk.** Two answering one swamp the caller: 0.34 each against a 0.50 call.

### Lining out

A precentor intones a hymn line alone and the congregation sings it back slowly
and heterophonically, each ornamenting differently — a wide, slow,
unsynchronized unison. The precentor is trivial; the congregation is not, since
heterophony needs many voices differing slightly and three locked voices
differing slightly just sound wrong. The usable half is the form: a terse solo
answered by a slow, thick, much lower repeat. **Recipe.** Noelle intones six
notes at 0.375 beat; all three repeat them at 1.5 beats, spread by octave.
**Risk.** Three straight long octaves read as a chord, not a crowd.

## Try these first

Ranked by how much of each tradition survives the pipeline, and how much the
result sounds like these machines rather than a failed choir.

1. **Bulgarian close seconds over a drone** — the 57–59 overlap makes it native.
2. **Rounds and canons** — exact and cheap; the octave spread marks the entries.
3. **Medieval hocket** — sample-accurate; the octave displacement is its sound.
4. **Reich-style pulsing vowels** — but hear the swell live, not offline.
5. **Vocal percussion** — the unvoiced-onset pitch failure is a gift.
6. **Doo-wop revoiced for Zoe's measured register** — put her in the middle.
7. **Sacred Harp open fifths and fuging entries** — wants vibrato spread.
8. **Call and response** — a balance problem more than a style problem.
9. **Barbershop post and stepped cadence** — the post is excellent, the swipe a
   compromise. Keep the name honest.
10. **Ligeti-style phase canon** — a different piece from *Lux Aeterna*.
11. **Monk / Medúlla vocabulary** — structure only; a patch, not a body.
12. **Katajjaq interlock** — half the tradition, the breath, is unavailable.
13. **McFerrin bass-and-melody** — redundant, and near Aaron's croak.
14. **Lining out** — form yes, heterophony no.
15. **Yodel** — not reproducible. One register per voice.
16. **Tuvan overtone** — not reproducible. No formant control in the score.
