# 01 · Tonal forms, 10 to 16 minutes: timing rules for a score generator

Shelf: form · Sources: `bach-1720-chaconne`, `bach-c1710-passacaglia`, `part-1977-fratres`, `part-1978-spiegel`, `reich-1987-electric-counterpoint`, `reich-1973-pieces-of-wood`, `adams-1978-shaker-loops`, `adams-1973-christian-zeal`, `eastman-1973-stay-on-it` (see `sources-01.json`)

Companion to `grants/culturehub-la-2026/chamber-studies/01-tonal-forms.md`. That file argues; this one states constraints for a `/pop` generator. Every number is a restatement of a fact read in the cited source; no source text is reproduced.

## Unit

A 4/4 bar at 112 BPM is 2.143 s; 16 bars are 34.3 s (arithmetic). A generator stepping every 16 bars produces a switch every 34 s. None of the works below holds one phrase length for a whole piece.

## Facts

Chaconne: 64 four-bar variations; sections of 33, 19, 12 variations in minor, major, minor (`corilon-chaconne`). Major section is variations 34 to 52; intensity peak at 23 to 30, maximum at 28; variation 63 restates variation 1; rhythmic and harmonic complexity move inversely (`bach-1720-chaconne`).

Passacaglia: eight-bar ground, theme plus 20 variations, fugue without pause (`bach-c1710-passacaglia`).

Fratres: nine rotations separated by a two-bar 6/4 percussion pattern; bars of 7/4, 9/4, 11/4; A-E drone (`part-1977-fratres`). Six-bar theme steps away from a centre for three bars and reverses; dynamics arch, peak in the central variation; ends on quiet tolling bars (`parlance-fratres`). About nine minutes (`sfs-fratres`).

Spiegel im Spiegel: F major, about 10 minutes; phrase begins as two notes and adds one per phrase; each ascent answered by a descending mirror to A (`part-1978-spiegel`, `ue-spiegel`).

Electric Counterpoint: 14:46, three movements without pause; canon built one voice at a time, then resultant patterns, then strummed chords; slow movement halves tempo and changes key; finale alternates E minor and C minor, 3/2 and 12/8, accelerating until basses fade and E minor 12/8 resolves (`reich-1987-electric-counterpoint`, `hyperion-electric-counterpoint`). First movement 6:51 (`apple-metheny-electric-counterpoint`).

Music for Pieces of Wood: sections in 6/4, 4/4, 3/4 built by substituting beats for rests (`reich-1973-pieces-of-wood`); about 58 pattern shifts in ten minutes; recordings 11 to 15 minutes (`aso-pieces-of-wood`, `hyperion-pieces-of-wood`).

Shaker Loops: movements at 0:00, 8:51, 15:25, 22:45 on the cited recording; second movement is the stillness; climax is a unison push-pull late in the third; fourth is coda (`adams-1978-shaker-loops`).

Christian Zeal and Activity: 8 to 10 minutes; one hymn phrase per minute; voices displaced so chords blur (`adams-1973-christian-zeal`, `ircam-christian-zeal`).

Stay On It: about 25 minutes; sections repeat ad lib on cue, freedom widens (`eastman-1973-stay-on-it`); each riff return lands as an arrival (`spectrum-stay-on-it`).

## Rules

R1 phrase length from {8, 12, 16, 24} bars; never three equal in a row. Fratres 7/9/11, Pieces of Wood 6/4/3.

R2 phrase departs the tonic by n steps and mirrors home; n increments per phrase, resets per section; arrival tone holds one bar. Fratres reversal, Spiegel growth.

R3 every fourth phrase cadences on tonic-triad tones, 2-bar decay, 1 bar silent; voices enter only there. Chaconne's four-bar cycle.

R4 when chord rate doubles, halve attack density. Chaconne.

R5 climax at 0.71 of duration after an unbroken 90 s crescendo; the next 30 s are the second-quietest passage. Shaker Loops (0.80 then coda), Chaconne (0.36 to 0.47 then clearing).

R6 stillness at 0.36: two voices, one chord, 8 bars, no attacks. Hymning Slews, Christian Zeal.

R7 three join types per boundary: 4-bar elision, 1-bar rest plus 2-beat pickup, 2-bar general pause. Chaconne, Fratres, Passacaglia.

R8 register and density never change in the same phrase; register steps a third per phrase for nine phrases. Fratres.

R9 caller seat n, responder n+3 after 2 beats, answer inverted; caller order 1, 4, 2, 5, 3, 6. Spiegel.

R10 percussion enters only after a rest-substitution fill on beats 3 and 4; fill periods 8 and 32 bars. Pieces of Wood.

R11 key lift prepared by alternating tonic and target at 8, 8, 4, 4, 2, 2, 1, 1 bars; reversed to return. Electric Counterpoint, Chaconne.

R12 ending restates the opening at half density, decay doubling per phrase, final tone 8 bars, one release cue. Chaconne 63, Fratres close, Pieces of Wood stop.

## Debts

Fratres arch and duration come from program notes, not the score. Electric Counterpoint movements 2 and 3 are not individually timed. Shaker Loops timestamps are one recording. Passacaglia duration unconfirmed.
