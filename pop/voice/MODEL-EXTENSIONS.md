# model extensions — how the platter can improve pink trombone itself

PHYSIOLOGY.md uses the platter to *constrain* PT's parameter search. this file goes one step further: the same anthropometric data could *extend* PT's model, not just bound it. that's a research direction, not a deliverable for the harness — but it's worth marking now so we don't accidentally rederive it later, and it's the natural shape of a paper out of this lane.

> "the laptop orchestra was a beautiful idea that reached almost no one" — `papers/arxiv-plork/plork.tex` §1
>
> pink trombone is the same shape: a beautiful physical model that is one anatomical specimen wide. every voice that comes out of it is the same person.

## what PT currently is

neil thapen's PT (2017) is a **single-speaker** physical model:

- 44 tract segments, each a constant 0.4 cm → tract length pinned at ~17.5 cm
- one hard-coded neutral-tract area function (close to Maeda 1990 generic adult-male)
- tongue index/diameter as the user's articulation interface (~Maeda's tongue-body PCA collapsed into 2D)
- glottal source from Liljencrants-Fant (LF) with a tension knob
- nasal port branch with constant scaling
- lip diameter as a single end-of-tube area parameter

it sounds remarkable for ~500 lines of JS. but it is *one* mouth.

## extensions the platter enables

### 1. variable tract length, properly

PT exposes `tract.n` (segment count) but the JS UI never varies it. with the Lammert+Narayanan VTL estimate from the platter we can:

- set `tract.n` to a per-speaker integer that targets the measured VTL at the existing 0.4 cm/segment quantum, *or*
- vary the per-segment length (`tract.dx`) and keep `n` fixed.

both should be acoustically equivalent at low frequencies; the difference matters at the higher formants. the question worth answering — and it isn't currently in the PT literature — is which formulation is more robust for an *adapted* tract whose length differs by 5–10% from the default.

### 2. personalized neutral-tract area function

PT ships with a single area function (close to schwa /ə/). every articulation is a deviation from that one neutral pose. the personalization opportunity:

> the platter gives us closed-mouth neutral-pose photos. for each, we read the lip aperture, the mandible angle, and (with DECA / EMOCA) the 3D head shape. this anchors **one end** of the area function — the lip end — to jeffrey-specific values. the *interior* of the tract still uses the generic curve, but the end conditions are jeffrey's.

acoustically: this should shift F1/F2 of jeffrey's neutral pose by 50–150 Hz versus PT's default. measurable from the schwa recording in CORPUS.md.

### 3. mandible kinematics from real jaw rotation

PT's "lip diameter" knob secretly controls a small constellation of segments (the front of the tract). with the platter's open-mouth candids vs. closed-mouth neutrals, we can fit jeffrey's actual mandible rotation range and translate that into a *coupled* update of multiple PT segments — front-tract opening, lip protrusion, lower-incisor position — instead of one slider.

this is closer to Maeda's original tongue-jaw PCA, but personalized. measurable: open-mouth /ɑː/ vs closed-mouth /uː/ formant shifts should track better against the real jeffrey-pvc recordings than against PT's defaults.

### 4. nasal cavity scale from external nostril measurement

PT's nasal branch has a constant-scaled area function. nostril-width and inter-alar distance correlate with nasal-cavity volume (Sahin-Yilmaz et al. 2008, Otolaryngology). a one-parameter scale on PT's nasal branch, derived from the platter, should improve nasal-vowel quality (/m n ŋ/ recordings in CORPUS.md become the test set).

### 5. F0 envelope from the corpus, not from a knob

PT's glottis is parameterized by tension and noise. jeffrey's actual F0 distribution comes out of the alignment-timestamped corpus *for free* — every recorded clip already carries jeffrey-pvc's pitch contour. we can:

- estimate jeffrey's neutral F0 (likely 100–130 Hz adult male)
- estimate his vibrato envelope (probably tiny in the speech corpus, larger if a singing corpus ever lands)
- use those as the priors on PT's F0 modulation, not a hand-tuned LF

### 6. cross-speaker generalization

if this works for jeffrey, the same pipeline can be run on any speaker who has both a voice clone and a small photo set. that's a contribution back to PT's user community — a "speaker adaptation" pipeline.

## what this would *not* be

- **not a paper claiming PT is broken.** it isn't. PT is a celebrated educational artifact and a remarkably accurate generic adult-male tract. the extensions are personalization, not correction.
- **not a face-to-voice deepfake.** the goal is voice synthesis whose anatomical parameters are *consistent* with the visible face, not whose acoustic fingerprint matches a target. the acoustic fingerprint comes from the jeffrey-pvc corpus, which jeffrey controls.
- **not a clinical tool.** medical use cases (post-surgery prosody planning, articulator training for speech therapy) need MRI ground truth. this lane is artistic and research-grade.

## paper outline (if/when it earns one)

```
title: Personalized Pink Trombone — Anatomically-Grounded Vocal Tract
       Synthesis from Photographs and Speech

§1  Introduction — PT as universal-adult model, the personalization gap
§2  Related work — Maeda 1990, Story 1996 area functions, Lammert 2015
                   VTL regression, Fitch-Giedd 1999, voice cloning state
§3  Anthropometric pipeline — mediapipe → measurements → VTL
§4  Acoustic fitting — CMA-ES on PT params, MFCC distance, anatomical bounds
§5  Model extensions — variable-VTL, personalized end-conditions, mandible
§6  Evaluation — held-out phonemes, A/B vs default-PT, formant accuracy
§7  AC-native deployment — C/WASM port, runtime budget, integration with
                            the recap and big-pictures lanes
§8  Limitations — external-only measurements, no MRI ground truth, single
                  speaker, plosive quality
§9  Future work — multi-speaker corpus, singing extension, real-time
                  inversion (image-to-voice with no acoustic target)

cite: Thapen 2017 (PT itself), Maeda 1990, Story et al 1996, Fitch+Giedd
      1999, Lammert+Narayanan 2015, Sahin-Yilmaz et al 2008
       (nasal anatomy), DECA/EMOCA for 3D face fit if used.
```

this fits the **arxiv-ac** lane in `papers/SCORE.md` as a sibling to the
existing AC-native papers. it doesn't fit the big-pictures rap lane —
the rap version is just the chorus: *"PT was one mouth; the platter made
it ours."*

## status

- 2026-05-04 — outline only. no extensions implemented.
- gating: ship the harness first (PHYSIOLOGY + fit), then evaluate which
  extensions produce audible/measurable gains, then write the paper if
  any of them do.
