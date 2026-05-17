# melodyne study — does our autotune match?

research note. how melodyne corrects pitch vs. how `autotune.py` /
`pitchsnap_world.py` / `voicepitch.py` do it here, and where ours is
weaker.

## how melodyne works

- **note objects, not frames.** melodyne analyses the take into "blobs"
  — discrete note objects on a piano-roll — each with a centre pitch
  plus the *micro* pitch movement inside it (attack scoop, vibrato,
  drift) kept as an offset on top of the note. you correct the note's
  centre; the expressive movement rides along, scaled.
- **probabilistic detection.** every detection is graded by likelihood;
  a Note Assignment control trades how many candidate notes are shown
  vs. used. pitch detection is explicitly "not an exact science."
- **fundamental vs overtones separated per note**, which is what lets
  DNA reach individual notes inside polyphony (chords, guitar).
- **formant preservation.** transposing a note up auto-shifts the
  formants back down by the same amount → no "Mickey Mouse." Formant
  Up/Down sliders set how much correction (0 = let it chipmunk, 100% =
  full timbre lock), independently per direction.

## how ours works

WORLD vocoder pipeline (Morise et al.):

```
harvest      → f0 candidates
stonemask    → f0 refinement
cheaptrick   → spectral envelope   ← this IS the formants / voice identity
d4c          → aperiodicity        ← breath / consonants
REPLACE f0   → (snap to scale, or a target melody)
synthesize   → voice, new pitch, SAME envelope
```

`autotune.py` snaps every voiced frame toward the nearest scale note
(strength 0..1, short glide). `pitchsnap_world.py` replaces f0 with a
fixed target melody. `voicepitch.py` medians f0 per ~0.28 s segment for
the pitch-shadow.

## does it match?

**On the thing that matters most — pitch corrected, timbre intact —
yes, ours matches Melodyne in kind.** Melodyne shifts formants opposite
to the transposition to keep them fixed; WORLD keeps the spectral
envelope (`cheaptrick`) constant by construction and only swaps the
excitation f0. Different mechanism, *same result*: the voice stays her,
the pitch moves. Our `+ d4c` aperiodicity path also preserves breath /
consonant texture, which is the part naive pitch-shifters smear.

**Where ours is weaker than Melodyne:**

1. **Frame-snap, not note-aware.** we quantise every frame; Melodyne
   corrects a note's *centre* and lets the vibrato/scoop ride on top.
   at high `strength` ours can iron out expressive drift → "harder",
   less musical. (we already have per-segment medians in
   `voicepitch.py` — the building block for note-aware correction.)
2. **Mono only.** `harvest` is monophonic f0. fine for a solo vocal
   (our case); no DNA-style polyphony.
3. **No formant *control*.** ours locks timbre but can't deliberately
   shift formants for character the way Melodyne's Up/Down sliders do
   (WORLD can warp the `cheaptrick` envelope — we just don't expose it).

## to close the gap (cheap wins, not done yet)

- **note-segment then correct per note**: group voiced frames into notes
  (pitch-stable runs), correct each note's median toward the scale,
  re-add the original intra-note deviation × a "preserve expression"
  factor. ≈ Melodyne's behaviour, ~30 lines on top of `autotune.py`.
- **`--formant` knob**: scale the `cheaptrick` envelope along frequency
  before synth → deliberate formant shift (brighter/darker, or
  chipmunk-on-purpose) like Melodyne's sliders.
- **`--preserve-vibrato`**: low-pass the *correction* (not the f0) so
  fast pitch movement passes through uncorrected.

verdict: for a monophonic sung take, our WORLD path is the same family
as Melodyne and timbre-faithful; it's a *hard* tuner where Melodyne is a
*musical* one. the note-segment pass is the one upgrade worth doing if
solafiya's vocal wants to sound less corrected.
