# Wattajetta stone form

The canonical source is the original `wattajetta.mp3` section beginning around
1:51: the bar-64 stone drop. Its identity is locked to these ingredients:

- a continuous tempo climb from roughly 127 BPM to 138 BPM;
- sustained sub plus short offbeat sine gallops;
- dense, swung water-bloop percussion;
- granite FEM glass runs and stone bowls;
- the original 0.85 crunch and global platter accelerando;
- mostly tiny FEM strikes during the first 45 seconds, crossed by five explicit
  10–14 second glass-body blooms, then opening afterward;
- roughly half-density FEM runs and half as many bowls during that sparse
  opening; kick and sine floor remain continuous;
- no inherited scratch-buffer or platter-drag gestures.
- eager hats, dragged backbeats, swung shakers, skip-double fills, reverse
  snare intakes, and 32nd-note rushes on a separate seeded millisecond clock;
- stone glass, handbell, tubular, bowl, and one restrained church geometry.
- four chordal stone gates that widen and wobble as the form hardens;
- no white-noise splash at 0:00; the kick and sine floor introduce the track.
- one through-composed pentatonic bell line whose melodic cursor never resets
  at bar boundaries; asymmetric turns prevent eight-note loop repetition.

The two-minute form is four seamless mutations of that vocabulary. Density and
stereo placement evolve, but the four-on-floor and offbeat sine gallop never
stop. There are no midsection breakdowns and no alternate mix variants.

```bash
node pop/wattajetta/bin/render-wattajetta.mjs --stone-study
```

The output is `out/wattajetta-stone-canonical.mp3`. The FEM cache in
`out/.wattajetta-bell-cache-v1/` keeps the deterministic rebuild quick; the
revised low-loss glass model uses `-fem2` cache keys so older materials remain
warm without masking the new physical response.

The opening title utterance is removed. `--stone-club` keeps the same form and
adds a firmer kick, continuous 2-and-4 backbeat, offbeat hats, and deeper
sidechain movement. It writes `out/wattajetta-stone-club-audition.mp3` so the
accepted canonical render remains available for A/B review.

The club bell line is played like a two-mallet percussionist on a granite
lithophone: the right hand leads the beat-aligned eighths while the left
answers a hair late and softer, even-bar downbeats take flams, the tubular
mutation dead-strokes its left-hand diddles, and accents pick up octave-below
double-stops from a ladder extended one octave down (the play never reaches
above E5). Ornaments are mutually exclusive per strike and budgeted at two a
bar. Rolls cycle three shapes — crescendo tremolo, two-mallet dyad, and the
accelerating rush-gliss — and never repeat one twice in a row; seam pickups
evolve (ascending sextuplet into 18, a descending turn into 36, the full
two-octave gliss into 54) with smaller surprise rushes at 27, 45, and 62.
Slow figures keep millisecond humanization; fast figures (rolls, rushes,
glisses) are grid-perfect — at 32nd-note speed, machine placement reads as
intent. Every figure aims at the recorded bar leads of the through-composed
cursor and reads only the seeded groove clock, so the melody never re-rolls.
The line breathes through two incommensurate waves (13- and 7.3-bar periods):
valleys lay out and let single strikes ring 2.5–6.5 s; crests tighten, swing
harder, and sprout sixteenth fills. A redundancy sweep before mixing merges
any unchoked same-note near-unisons and the audit fails the render if one
survives.

The form carries classic song structure on top of the four mutations —
intro 0–8, verse 8–18, chorus 18–26, verse 26–36, chorus 36–44, bridge 44–54,
final chorus 54–66, outro 66–72 — expressed by scaling the breathing wave, so
every lane inherits the arc. Choruses carry one composed two-bar hook in
call-and-response with the walking line (the last chorus doubles it an octave
below); the bridge submerges the whole line underwater and belongs to the
ukulele.

A nylon Karplus-Strong ukulele is the cut's one stringed voice — no samples —
tuned an octave below a real uke with near-undamped 10× tails, so strums
overlay into a slow harp wash. Open strums follow the bowl roots two bars at
a time, choked "chnk" skanks ride the crest offbeats, a bright open E rings
each seam's resolution for ~20 s, and Fibonaccian arpeggiations fill the
valleys and the bridge: the Fibonacci word picks pluck-or-rest on the eighth
grid while intervals walk the 1-1-2-3-5 cycle, reflecting at the ladder edges.
The uke shares the bell stem.

The percussion grid is mathematically alive above the sacred 4/4 floor:
shakers play Euclidean necklaces E(k,16) with k climbing 5→7→9→11 across the
mutations, rotated by 5 (coprime to 16) each bar with accents where a
counter-rotating E(3,16) coincides; a {3,4,5}-coprime polymeter family (a rim
tick every 5 sixteenths, a wood tick every 3 from bar 36) only rephases every
15 bars; and Fibonacci-word ghost snares tap the off-sixteenths with density
1/φ² — self-similar, never periodic.

Its spatial mix follows the Special Sign rule: one fixed listener, moving
source bodies, centered kick/sub, and an antisymmetric wet return that cancels
exactly in mono. Four exact-turn spin windows and an elastic baseline wobble
move the non-kick field, at roughly 60% of the first cut's depth — the
full-strength spin read as seasickness on the master. A shared macro envelope gives the intro a slow reveal,
steps down at mutation valleys, crests late, and retreats across the final 15
seconds; the gentler bus compressor preserves that arc.

The club cut also throws macOS's local Empty Trash sound into the three 18-bar
mutations at progressively slower rates. Export its four aligned, true-summing
48 kHz/24-bit premaster stems with:

```bash
node pop/wattajetta/bin/render-wattajetta.mjs --stone-club --stems
```

They land in `out/wattajetta-stone-club-stems/`: kick, water engine/percussion,
stone bells, and Empty Trash FX. Every stem carries the shared narrative
envelope; non-kick stems carry their part of the mono-safe spatial return. The
renderer rejects incomplete stereo bell-cache pairs before mixing so a low-disk
render cannot introduce non-finite samples.
