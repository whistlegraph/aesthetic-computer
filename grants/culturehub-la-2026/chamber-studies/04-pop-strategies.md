# Chamber study 04: what /pop already does, applied to six laptops

Grid for *Note(s)pat(ial) Native*: 112 BPM, beat 0.536 s, bar 2.14 s, 8-bar phrase 17.1 s. Sections: Appear 0:00, Ring 1:44, Echoes 3:09, Climb 5:09, Lift 7:27, Return 9:44, Vanish 12:01, end 13:08 (`grants/culturehub-la-2026/NOTESPATIAL-ARRANGEMENT.md:17-25`).

## 1. Structural habits

Lengths. The mill compresses to 1:30 (`pop/SCORE.md:41,55`). wannadash shipped at 1:54 after a 3:28 cut lost its third hook in a row and a fourth chorus, seams on downbeats with 10 ms fades (`pop/cult/MIX-NOTES-2026-09-01.md:180`, `pop/RELEASES.md:110-112`).

Builds are one curve, not stacked switches. wannadash's `polyAmt(bar)` is one piecewise-linear 0 to 1 over 120 bars; five cross-rhythm layers switch on as it crosses 0.10, 0.28, 0.40, 0.62, 0.78 (`pop/cult/README.md:53-81`). The same figure descends fs4, d4, b3, fs3 at bars 0/48/64/84 (`:125-131`) while its bus rides 1.00 to 0.83 at 0.3 dB per 8 s (`:142-150`). hypnotek builds from phase alone, precession 0/1/3/0/5/0 step-rotations per phrase, kick never rotating (`pop/minitek/thesis/hypnotek.md`, section 3.3). hellsine's drop is a gain envelope 0.75 to 1.55 at 110.77 s, tempo x1.06 from the same instant, +2 semitones (`pop/RELEASES.md:972-985`).

Joins. No drops, risers or crashes in cult (`pop/cult/README.md:87-88, 562-565`); acts add or remove one element; the turn is the kick and beeps stopping (`:389-390`). Its markers are eight `ELASTIC_EXPLOSIONS` at bars 29/40/48/64/76/92/104/108.5, spring 0.46 to 2.45 Hz, damping 0.34 to 1.85, attack 20 to 400 ms, drums held to a `keepTime` floor of 0.15 to 0.45 (`pop/cult/bin/render10.mjs:1376-1386`).

Endings. Beeps outlast the music and a hang-up click lands after silence (`pop/cult/README.md:305`); Special Sign brings the spatial world to rest at unchanged tempo, no fade, integrating a squared ease-out over the last 6 s (`spatial-sineabye.c:133-139`); bracelet lands when rotation stops on the balanced set (`pop/bracelet/README.md:60`).

Envelopes. hellsine voice: partials 1:1, 2:.5, 3:.34, 4:.16, 5:.12, 6:.06, vibrato 5.2 Hz at 0.6 % ramped over 120 ms (`pop/hellsine/c/hellsine.c:158,174-175,187-188`); kick 250 to 45 Hz over 35 ms then tanh (`pop/hellsine/README.md:40`). sineabye gong: modes 1, 1.41, 1.98, 2.91, 4.07, gains 1, .52, .34, .2, .1, lengths 8.5, 7.2, 6.1, 4.8, 3.5 s, attack 18 ms plus 6 ms per mode (`pop/nullabye/bin/render-sineabye.mjs:103-109`).
## 2. Spatial and warble techniques, and their six-seat form

Each is discrete copies offset by milliseconds and cents, reproducible with per-event pitch, gain, attack and decay.

a. Crossover wiggle (wannadash). Three performers on one pitch 28 ms apart; vibrato 4.3/5.9/3.4 Hz; depth by act 4/10/7/16/22/14/9/5 cents, ramped in over 0.42 to 0.50 s; drift +0.42w and −0.5w cents (`render10.mjs:1637-1641, 1652-1700, 1180-1183`). Six seats: the pitch on three adjacent seats at 0/28/56 ms; warble as a second event at the same seat detuned so the beat equals the vibrato rate (4.3 Hz at A4 is 17 cents), attack 0.45 s so beating arrives after the onset.

b. Choir sway. 62/80/98 cents at 0.16/0.22/0.28 Hz, 1.5 s ramp (`:1993-1996`). Six seats: overlapping events every quarter period at the sine's current offset, 1 s attack.

c. Doppler run. `far` 1 to 0 to 1 per cycle; gain x(1 − 0.74 far), delay x(1 + 1.8 far), pitch ±26 cents; three runners at phase 0.05 + 0.28i (`:1156-1163, 1993`). Six seats: the center laptop is the microphone; a ring event decays while a center event attacks at +26 cents, then the opposite seat at −26.

d. Staircase pan. Three copies a third of a lap apart, raised-cosine gain silent at both edges, constant sum, so a word slides one way forever; direction flips per bar (`:1241-1282`; `MIX-NOTES-2026-09-01.md:90`). Six seats: an orbiting lane with three copies 120° apart, each gated by that window.

e. Orchestra tour. Voice buses to 0.36, side to 60 %, music takes one turn over 9.4 bars (`:3654-3666`).

f. Rotation ribbon (Special Sign). Opening 96 turns over 4 bars, quartic ease from about 30 rps to 0, fusing into a hum (`spatial-sineabye.c:222-227`); super-spin 8 turns from 58.5 s over 16 s, quintic ease, sin²-windowed wobble ±0.31 rad (`:39-42, 230-236`); integer-turn bursts return to the incoming orientation (`:146-152`); echo and air trail the listener by 2.2 and 5.5 s (`:272`). Six seats: an orbit is a hop sequence with dwell period/5; dwells under 0.2 s fuse (`pop/lib/necklace-space.mjs:52-79`).

g. Bracelet. Rotation is precession, reflection a seat mirror; kick and sub under 150 Hz never move; ring voices need a broadband chiff to localise; only a chiral ring with an off-axis centroid mirrors audibly (`pop/bracelet/README.md:39-48, 81-87`; `necklace-space.mjs:143-147`).

h. Chorus (loner). Octave double at +6/−7 cents, 28/41 ms late (`pop/loner/bin/render3.mjs:16-18, 397-399`). Six seats: the note at the source seat's two neighbours, 28 and 41 ms late.

i. Tail warble (hellsine). 11 Hz at 1.4 % or 0.6 % on the last 45 % of a note (`hellsine.c:1592-1601`). Six seats: a second event 24 cents off from 55 % of the note.

## 3. Why wannadash translated

It is the release nearest the house target and loses only 4.1 dB on the phone proxy (`pop/MASTERING.md:40,52`). The reasons are compositional: equal-power dry pans with a band-limited antisymmetric side return, mono fold −0.12 dB (`pop/cult/README.md:642-646`); a unison of three real voices 28 ms apart, not a detune (`:556-558`); a wiggle that is local, ramped in, and grows with the story (`:322-329`); a 4/4 that never moves under the polyrhythm (`:83-88`). Every spatial effect is a small time or pitch offset between discrete copies, so six speakers with per-event control lose nothing.

## 4. Recommendations

1. Appear, 1:20 to 1:44: extend the walk to twelve laps ending at 0.12 s per seat, hold two fused laps, stop dead on seat 1 (f).
2. Ring, 2:18: crossover wiggle at 4 cents on the theme's held notes, ramp 0.5 s (a). Depth ladder: Echoes 10, Climb 16, Lift 22, Return 9, Vanish 0.
3. Echoes, 3:09: echo seat is the seat the theme held 2.2 s earlier; the tap answer trails 5.5 s (f), replacing the fixed plan at `compose-notespatial-native.mjs:161`.
4. Echoes pads, 3:43: sway chains, ±62/80/98 cents at 0.16/0.22/0.28 Hz, one overlapping event per quarter period (b).
5. Climb, 6:17: top line as a staircase, three copies 120° apart, direction flipping each bar (d).
6. Climb to Lift, 7:27: one blast, every orbit offset displaced one seat and springing back at 0.92 Hz, damping 0.58, 4.8 s (`render10.mjs:1384`); kick stays front (g).
7. Lift, 7:27: theme as a three-seat unison 0/28/56 ms, outer copies drifting apart by 0.42w and 0.5w cents (a).
8. Lift, 8:01: answer as three Doppler runners between ring and center, ±26 cents, phases 0.05 + 0.28i (c).
9. Lift peak, 8:35 to 9:00: eight-turn super-spin on theme and hats, dwell 2.14 s down to 0.2 s and back on a quintic ease, one early or late hop per turn as wobble (f).
10. Return, 9:44 to 10:03: orchestra tour, center to 0.36, ring assignments rotate one seat every 4 s for one lap (e).
11. Return, 10:35: mirror the walk, seats 2 and 5, 3 and 4 swapped, two bars on, two flipped; the FRONT [4,0,1] / BACK [2,3] map is chiral about the front axis, so the flip reads (g).
12. Vanish, 12:01: dwells of the backwards lap grow by the integrated squared ease over the last 6 s; 2.6 s after the last C, one tap from seat 4, the hang-up (`spatial-sineabye.c:133-139`; `pop/cult/README.md:305`).
