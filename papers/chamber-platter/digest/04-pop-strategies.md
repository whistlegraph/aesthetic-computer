# 04 · Spatial and warble strategies in /pop, restated for six seats

**Shelf:** spatial, warble, form · **Sources:** repo files, cited by path and line. Restatement only. Applied in `grants/culturehub-la-2026/chamber-studies/04-pop-strategies.md`.

## Form facts

- Tracks run about 1:30 (`pop/SCORE.md:41,55`). wannadash went from 3:28 to 1:54 by removing repeated statements, seams on downbeats with 10 ms fades (`pop/cult/MIX-NOTES-2026-09-01.md:180`; `pop/RELEASES.md:110-112`).
- One curve drives a build: `polyAmt` rises 0 to 1 over 120 bars, layers entering at 0.10/0.28/0.40/0.62/0.78 (`pop/cult/README.md:53-81`); a figure descends fs4 to fs3 in four stages while its bus rides 1.00 to 0.83 at 0.3 dB per 8 s (`:125-150`).
- Phase alone builds: precession 0/1/3/0/5/0 step-rotations per phrase, kick fixed (`pop/minitek/thesis/hypnotek.md`, section 3.3).
- A peak is one instant: gain 0.75 to 1.55, tempo x1.06, +2 semitones (`pop/RELEASES.md:972-985`).
- Joins are impacts, not risers: springs 0.46 to 2.45 Hz, damping 0.34 to 1.85, attack 20 to 400 ms (`pop/cult/bin/render10.mjs:1376-1386`).
- Endings: one click after the music (`pop/cult/README.md:305`); motion integrated to rest over 6 s at fixed tempo (`pop/nullabye/c/spatial-sineabye.c:133-139`).

## Envelope facts

- Sine voice partials 1:1, 2:.5, 3:.34, 4:.16, 5:.12, 6:.06; vibrato 5.2 Hz at 0.6 %, 120 ms ramp (`pop/hellsine/c/hellsine.c:174-175,187-188`).
- Gong modes 1, 1.41, 1.98, 2.91, 4.07; gains 1, .52, .34, .2, .1; attack 18 ms plus 6 ms per mode (`pop/nullabye/bin/render-sineabye.mjs:103-109`).

## Spatial and warble facts

- Unison: three voices on one pitch 28 ms apart (`render10.mjs:1652-1700`). Chorus: octave double at +6/−7 cents, 28/41 ms late (`pop/loner/bin/render3.mjs:16-18`).
- Wiggle: vibrato 4.3/5.9/3.4 Hz; depth per section 4/10/7/16/22/14/9/5 cents; ramp 0.42 to 0.50 s; drift +0.42w and −0.5w (`render10.mjs:1637-1641,1652-1700`).
- Sway: 62/80/98 cents at 0.16/0.22/0.28 Hz, 1.5 s ramp (`:1993-1996`).
- Run: gain x(1 − 0.74 far), delay x(1 + 1.8 far), pitch ±26 cents, runner phases 0.05 + 0.28i (`:1156-1163,1993`).
- Staircase: three copies a third of a lap apart, raised-cosine window silent at the edges (`:1241-1282`).
- Tour: voices to 0.36, accompaniment one turn over 9.4 bars (`:3654-3666`).
- Rotation: 96 turns over 4 bars, quartic ease from about 30 rps to 0 (`spatial-sineabye.c:222-227`); 8 turns over 16 s, quintic ease, wobble ±0.31 rad in a sin² window (`:39-42,230-236`); integer-turn bursts return to orientation (`:146-152`).
- Ring law: step i at 2πi/n; rotation is precession, reflection a mirror; sources under 150 Hz never move; a tone needs a broadband chiff; only a chiral ring with an off-axis centroid mirrors audibly (`pop/bracelet/README.md:39-48,81-87`). Pulse floor 100 ms, precedence 5 ms (`pop/lib/necklace-space.mjs:52-79`).
- wannadash loses 4.1 dB on the phone proxy, mono fold −0.12 dB (`pop/MASTERING.md:40,52`; `pop/cult/README.md:642-646`).

## Rules for a six-seat score generator

Events carry pitch, gain, attack and decay; five ring seats plus a center.

1. Warble: two events at one seat, Δf equal to the beat rate (17 cents at A4 gives 4.3 Hz), the second with 0.45 s attack; depth on a per-section ladder ending at zero.
2. Unison: one pitch on three adjacent seats at 0/28/56 ms; outer copies drift 0.4 to 0.5 of the depth in opposite directions.
3. Chorus: the note on the two neighbouring seats, 28 and 41 ms late, +6/−7 cents, gain 0.22.
4. Sway: overlapping events every quarter period at the sine's current offset, 1 s attack.
5. Run: a ring event decays while a center event attacks at +26 cents, leaving at −26 to the opposite seat; runners at phases 0.05 + 0.28i.
6. Orbit: hop sequence, dwell period/5; below 0.2 s the ring fuses; integer laps with quintic-eased dwell return a lane to its seat.
7. Staircase: three copies 120° apart, each windowed to silence at the wrap seat.
8. Tour: ring assignments advance one seat per fifth of the period while the center drops to 0.36.
9. Mirror: swap seats 2 and 5, 3 and 4, only for an assignment chiral about the front axis.
10. Impact: displace every orbit offset one seat, spring back at 0.5 to 2.4 Hz, damping 0.3 to 1.9; kick and sub never move; ring tones open with a short noise event.
11. Ending: grow the final dwells by an integrated squared ease over 6 s, then one event in silence.

tools: `hopSequence(period, laps, ease)`, `warblePair(seat, hz, beatHz, attack)`, `unisonTriple(seats, offsetsMs, driftCents)`, `staircase(lane, copies)`, `mirror(assignment)`, `impact(offsets, hz, damping)`, `windDown(dwells, seconds)`.
