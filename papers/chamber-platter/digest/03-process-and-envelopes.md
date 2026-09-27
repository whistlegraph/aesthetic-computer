# 03 · Process form and additive envelopes

**Shelf:** process-form, envelopes · **Sources:** `../sources-03.json`, ids in brackets. Restatement only. Applied to one piece in `grants/culturehub-la-2026/chamber-studies/03-process-and-envelopes.md`.

## Phrase hierarchy in process music

Every process ensemble studied changes one parameter of one lane per phrase and saves large changes for rare section boundaries. Reich floors each four to five minute section on one chord, moves harmony only at section ends, keeps the mallet pulse constant, swells wind and voice chords at breath length, and restates the opening eleven-chord cycle at the close [reich-m18]. *Six Pianos* builds parts by substituting notes for rests one at a time over one 8-beat pattern, with three sections on one seven-note collection and sudden centre changes [reich-six-pianos]. *Electric Counterpoint* assembles an 8-voice canon one entry at a time [reich-electric-counterpoint]. *In C* repeats each of 53 patterns for 45 to 90 s, keeps players within two or three patterns of each other, over a constant eighth-note pulse on high Cs [riley-in-c]. *Phrygian Gates* has 14 subsections that each alter one thing (figuration, register, pulse speed, amplitude) and switches mode abruptly at gates [adams-phrygian-gates]. *Hoketus* seats two identical groups at the stage extremities and never lets them sound together [andriessen-hoketus]. Eastman buries material rather than removing it; each section contains all earlier ones [eastman-gay-guerrilla]. *Cantus* runs a descending scale in prolation canon, each entry an octave lower at half speed [part-cantus]. Dance arrangement: something small every 8 bars, bigger every 16, sections every 32; a breakdown removes kick and bass; a riser or roll fills the last 2 to 4 bars before a drop [ghost-rule-of-32] [quadrophone-arranging].

Rules for a score generator:

1. Three tiers: one lane, one parameter per phrase; one lane in or out per double phrase; section change per quadruple phrase. Never two changes in one phrase.
2. Introduce a pattern by substitution (one note per phrase), not whole.
3. Prepare a peak by removing pulse and bass one phrase early, then land key, pulse and theme on one downbeat.
4. Overlap registers: the new one enters as a half-speed canon while the old persists two phrases at reduced gain.
5. Hocket across seats for a bounded span (odd seats beats 1 and 3, even 2 and 4), then release into overlap.
6. End by restating the opening; exit lanes with a one-phrase linear fade on a downbeat.

## Envelopes from sines with linear attack and decay

Risset's bell uses 11 partials at frequency ratios 0.56, 0.56 (+1 Hz), 0.92, 0.92 (+1.7 Hz), 1.19, 1.7, 2.0, 2.74, 3.0, 3.76, 4.07; relative gains 1, 0.67, 1, 1.8, 2.67, 1.67, 1.46, 1.33, 1.33, 1, 1.33; duration ratios 1, 0.9, 0.65, 0.55, 0.325, 0.35, 0.25, 0.2, 0.15, 0.1, 0.075 [puckette-risset-bell] [arkadyan-risset-sc]. The transferable law: each partial owns its decay, and higher partials die first. Karplus-Strong plucks obey it through the loop filter [wikipedia-karplus-strong]. Marimba bars are tuned 1:3.92:9.24 and xylophone 1:3:6.16 [euphonics-marimba]; marimba decay runs about 2 s at low C to 20 ms at C7, and vibraphone bars ring 30 s low to 6 s high [instrumentalist-vibraphone]. Perceived attack under 10 ms reads as struck or plucked, 50 to 100 ms as bowed [wikipedia-perceptual-attack]; 150 to 400 ms breathed and over 1 s pad are AC working values, unsourced. A synth kick is a sine falling from about 150 to 48 Hz with 1 ms attack and 300 to 450 ms decay [dev-sample-free-drums]; Risset's drum pairs inharmonic partials with narrow-band noise that decays alongside them [audacity-risset-drum].

Recipes. T is note length; partial duration is a fraction of T; attack and decay are linear; every partial costs one voice.

| Voice | Partials (ratio:gain:duration/T) | Attack | T |
|---|---|---|---|
| Plucked melody | 1:1:1.0, 2:0.5:0.5, 3:0.25:0.3, 4:0.12:0.18; noise burst gain 0.1, 8 ms | 3 ms fundamental, 2 ms partials 2 to 4 | 0.9 x inter-onset interval |
| Struck bell | 1:1:1.0, 2:0.6:0.55, 3:0.4:0.3, 4.07:0.25:0.12, 0.56:0.3:0.9 | 5 ms fundamental, 2 ms others | 1.5 to 2.5 s |
| Soft pad | 1 (triangle):0.7, 1.003 (sine):0.5, 2 (sine):0.2; all duration 1.0 | 1.5 to 4.3 s | attack plus equal decay |
| Tap / kick / snare / hat | tap: sine 200 to 90 Hz over 30 ms, decay 120 ms, gain 0.5, noise 10 ms at 0.15. kick: sine 150 to 48 Hz over 50 ms, decay 400 ms, noise 15 ms at 0.3. snare: sines 180 Hz and 330 Hz decay 120 ms, noise decay 200 ms. hat: noise decay 40 ms | 0 to 1 ms | as given |

Marimba variant: 1:1:1.0, 3.92:0.35:0.25, 9.24:0.1:0.08, attack 2 ms, T 0.6 s at C3 to 0.05 s at C6 [euphonics-marimba] [instrumentalist-vibraphone].

In code: `pop/lib/bell.mjs` is the FEM modal engine, not additive; these recipes are the sine-stack path for capped runtimes with linear envelopes only (fedac native, 32 voices). Count partials as voices per machine.
