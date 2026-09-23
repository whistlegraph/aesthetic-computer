# 05 · Hearing inside the ring: what an audience among the laptops can and cannot place

Shelf: spatial, psychoacoustics · Sources: `sources-05.json`

Companion to `grants/culturehub-la-2026/chamber-studies/05-inside-the-ring.md`, which applies this to *Note(s)pat(ial) Native* chapter by chapter. Digest 02 phrased placement as music (hop, orbit, antiphony, tutti). This entry asks what the ear actually gets from a laptop speaker in a pentagon when the listener is not at the center. Every number is a restatement of the cited source; no text is reproduced.

## Unit

Pentagon 5 to 7 m across, seats 2.5 to 3.5 m from the center, neighbours 72° apart from the center. A listener 1 m from one laptop and 5 m from another hears the far one 11.7 ms later (343 m/s) and about 14 dB quieter by inverse square (20·log10 5). Nobody but the performer is at the center; the numbers below are for that off-center listener. A laptop speaker rolls off under roughly 150 to 200 Hz; a ThinkPad X1 Carbon measures its 100 to 315 Hz band 11% under its own median at a 76.8 dB maximum (`notebookcheck-x1c9`, `soundtest-laptop-bass`). The runtime's scheduler steps at 25 ms in the dry run; an event shorter than a frame may never be voiced (`notespatial-native-check.mjs`).

## Facts

**Precedence.** Two arrivals of one sound fuse to the first when the lag is under 1 to 5 ms for clicks and up to about 40 ms for complex sounds such as speech or piano; the later arrival becomes a separate echo above about 50 ms (`wallach-1949-precedence`, `litovsky-1999-precedence`, `wikipedia-precedence`). Under 2 ms the image sits between the two sources (summing localization); that is the whole basis of amplitude panning, and it needs the listener equidistant from both speakers to within about 0.7 m.

**Placing a tone in a room.** With an attack transient a sound is localized regardless of reverberation time; without one, a 500 Hz sine is placed at chance and a 5 kHz sine only somewhat better (`hartmann-1983-rooms`). The steady state of a sine gives no usable direction in a room; an onset lets the precedence effect work, and the effect fades as the onset is stretched from impulsive toward 5 s (`rakerd-hartmann-1986-onset`). Interaural time carries direction below about 1.5 kHz and interaural level above it (`macpherson-middlebrooks-2002-duplex`).

**Front and back.** Front-back confusions come from the cone of confusion and nearly vanish when the listener may move their head (`wightman-kistler-1999-front-back`). Lateral phantom sources between two speakers are unstable even for a centered listener, which is why Theile and Plenge proposed real speakers at ±30°, ±90°, ±150° instead (`theile-plenge-1977-lateral`).

**Motion.** Minimum audible movement angle is about 5° for a source ahead and over 30° at ±90°; it grows fourfold from 15°/s to 90°/s and rises sharply once a moving sound lasts under 100 to 150 ms (`grantham-1986-mama`). A smooth rotation is followed up to about 2.8 rotations per second for noise, less for harmonic tones, and best for low fundamentals (`feron-2010-rotation`).

**Masking.** Separating a target from a competing sound by 90° buys normal-hearing adults about 6 to 10 dB of intelligibility (`litovsky-srm-children`, `bronkhorst-2000-cocktail`). Brant's practical rule is the same in a composer's words: spatial separation disentangles dense, contrasting lines and does nothing for unified material (`brant-1967-space`, `bates-spatial-music`).

**Distance.** Judged distance grows as roughly the 0.54 power of physical distance; loudness is the main cue for an unfamiliar source (`zahorik-2005-distance`). A laptop at 3 m therefore sounds under twice as far as one at 1 m, not three times.

**Pitch below the speaker.** The pitch of a low complex tone is carried by its third to fifth harmonics, so a root the speaker cannot pass is still heard at its root if those harmonics are present (`ritsma-1967-dominance`).

## Rules

R1 A root under 200 Hz on a laptop carries its octave at 0.5 and its twelfth at 0.28 of the fundamental's gain, same attack, shorter tails. A kick is a click, a 150 Hz thump, the 78 Hz body and the body's second harmonic. Sources under 150 Hz never move (digest 04, rule 10).

R2 Anything that must be *placed* has an attack of 10 ms or less and a partial above 1.5 kHz: an octave on top of any sine under 750 Hz. Pads (600 ms attack) are wash and never carry a location. Every click or tick lasts at least 30 ms, or the scheduler may drop it.

R3 In a ring the audience sits inside, a melodic line hops: each note is pinned whole to one seat. Glide (equal-power crossfade between neighbours) only for wash and noise. An off-center listener hears a glide as a jump at a moment set by where they sit; a hop puts the jump on the beat for everyone, and one laptop per note cannot flam against another under clock skew.

R4 A tracked voice stays two seats (144°) or more from its accompaniment at every instant; when the tune hops, the walk hops to the pair across from it.

R5 A hopped runner dwells 200 ms or more per seat; at tempi where an eighth is shorter, each seat takes two eighths. Rotations of tones stay under about 0.5 rotations per second to read as travel; faster is spin, used once as cadence (digest 02, R7).

R6 Simultaneity budget: a tutti of plucks reads as one event while the laptops agree within about 40 ms, and as one *point* only within 5 ms. Crystal drift of 20 to 50 ppm puts six free-running clocks 15 to 39 ms apart after 13 minutes; measure at the tech, and re-cue by chapter once drift passes 20 ms.

R7 Rear seats carry broadband material (snare, ticks, high taps) so behind reads as behind; the program note invites listeners to turn their heads.

R8 Center level is set from the far seat, not the performer's ear: for a listener 1 m from a ring laptop and r m from the center, equal loudness needs the center 20·log10(r) dB hotter (about 9.5 dB at 3 m). During center-only passages longer than one phrase the performer walks toward the rear pair.

R9 Approach and recession are not done by level across a 3 m radius (distance compresses); they are done by handing a voice from a ring seat to the center or back, which is the seat-to-hand gesture the form already uses.

## Debts

The onset-duration thresholds of `rakerd-hartmann-1986-onset` were not read from the paper; only the abstract's claim is used. The 6 to 10 dB masking figure is quoted from a secondary summary. `ritsma-1967-dominance` is cited from working knowledge; its abstract was not retrieved. Laptop roll-off is from one review measurement and a consumer test page, not from the performance machines; measure a seat with pink noise at the tech.

tools: `residueLow(midi, g)` → the three-event stack of R1; `placeable(event)` → attack ≤ 10 ms and any partial > 1.5 kHz; `hopLane(orbit, t)` → the seat nearest an orbit angle at onset (R3); `separation(seatA, seatB)` → degrees between two ring seats; `skewBudget(ppm, seconds)` → milliseconds apart; `centerTrim(rMeters)` → 20·log10(r).
