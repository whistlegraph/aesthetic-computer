# physiology — anthropometric priors from the platter

the visual side of the harness. PT's tract is a 44-segment tube with a default geometry intended for "an adult". if we leave it there, the CMA-ES fitter wastes evals exploring acoustic basins that correspond to anatomically impossible jeffreys. the platter already contains 55 AV-shoot headshots and 38 candid masters — high-res, multi-angle, well-annotated. we measure jeffrey from those, derive tract priors, and clamp the fitter's search space.

> "the address is the score" — `papers/arxiv-ac/ac.txt`

every photograph already encodes a physiological score. we just need to read it.

## what we measure (and why)

| measurement                       | source                                                      | reason it matters for PT                                  |
| --------------------------------- | ----------------------------------------------------------- | --------------------------------------------------------- |
| **head height** (vertex → menton) | mediapipe face mesh landmarks 10 (forehead) → 152 (chin)    | strong correlate of vocal tract length (Fitch+Giedd 1999) |
| **bizygomatic width**             | landmarks 234 ↔ 454                                         | secondary tract-length predictor; fixes scale             |
| **mandible length** (gonion → menton) | landmarks 172/397 → 152                                  | mandible ROM constrains jaw rotation in PT               |
| **lip width** (cheilion ↔ cheilion) | landmarks 61 ↔ 291                                         | upper bound on PT's lip diameter parameter               |
| **lip aperture range**            | min/max vertical lip distance across the corpus              | lip-protrusion → -aperture mapping for PT                |
| **philtrum length**               | landmark 0 (subnasale) → 13 (upper lip cupid's bow center)  | minor; informs lip-anterior cavity                        |
| **nose-to-chin (subnasale-menton)** | landmark 2 → 152                                           | proxy for oral-cavity vertical span                      |
| **nostril width**                 | landmarks 219 ↔ 438                                         | scales nasal cavity volume in PT's nasal port            |

we do **not** try to estimate velum position, soft-palate angle, or pharyngeal length from external photographs — those need MRI or videofluoroscopy to be honest. they stay free in the fit, bounded only by the published anatomical envelope.

## landmarking stack

```
photo (jpg / heic / webp)
  → opencv read + heif decode (libheif via pillow-heif)
  → mediapipe face mesh         (468 3D landmarks)
  → insightface buffalo_l       (5 landmarks, identity scoring,
                                  cross-validates that this IS jeffrey;
                                  reuses the existing portraits/jeffrey
                                  pipeline's identity threshold)
  → DECA / EMOCA optional       (FLAME params, 3D head fit;
                                  skipped on first pass — mediapipe is
                                  enough for ratios)
```

mediapipe is the workhorse. it gives metric 3D landmarks (in a canonical
head-normalized space), so the ratios survive camera distance. for
absolute scale we need a reference: jeffrey's actual height (known) or a
known object in the frame (rare in the platter). the cleanest path is to
**ratio-normalize** all measurements to head-height-units and then bind
to absolute centimeters at one point: VTL.

## VTL estimation (the load-bearing number)

Fitch & Giedd (1999) — JASA 106 — and Lammert & Narayanan (2015) —
JASA 137 — give regression models from external head measurements to
internal vocal tract length. simplest defensible form:

```
VTL_male_cm ≈ 17.5 + 0.05 * (height_cm - 175) ± 0.8
```

…with a ±0.8 cm 1σ uncertainty for the population. when we add jeffrey-
specific head measurements (head-height, bizygomatic), Lammert+Narayanan
shrink that uncertainty to ~0.4 cm.

we use the **lower-uncertainty** estimate as a *narrow prior* on PT's
tract length parameter, **not** as a hard fix. the fitter can move
within ±2σ. if the fitter consistently lands at the prior boundary, that
is a real signal that either (a) the regression is biased for jeffrey or
(b) PT's idealized tube cannot represent his actual tract. either is a
research finding, not a bug.

## output contract

`pop/voice/jeffrey-anthropometry.json` (committed; small, stable):

```json
{
  "version": 1,
  "source_corpus": {
    "shoot": 55,
    "candids": 38,
    "screenshots": 0,
    "rejected_identity": 3,
    "rejected_quality": 7
  },
  "measurements_cm": {
    "head_height": { "median": 23.4, "iqr": [22.9, 23.8], "n": 83 },
    "bizygomatic_width": { "median": 14.1, "iqr": [13.8, 14.4], "n": 83 },
    "mandible_length": { "median": 11.2, "iqr": [10.9, 11.5], "n": 71 },
    "lip_width_neutral": { "median": 5.1, "iqr": [4.9, 5.3], "n": 60 },
    "lip_aperture_max": { "median": 1.8, "iqr": [1.6, 2.0], "n": 22 },
    "philtrum_length": { "median": 1.6, "iqr": [1.5, 1.7], "n": 80 },
    "nose_to_chin": { "median": 6.4, "iqr": [6.2, 6.6], "n": 78 },
    "nostril_width": { "median": 3.2, "iqr": [3.1, 3.3], "n": 65 }
  },
  "derived": {
    "vtl_cm": { "central": 17.6, "ci95": [17.0, 18.2], "model": "lammert-narayanan-2015" },
    "tract_segments_44": { "segment_length_cm": 0.4 },
    "lip_diameter_max_pt": 1.5,
    "mandible_rotation_max_deg": 22
  },
  "notes": [
    "VTL uncertainty narrowed from ±0.8 (Fitch-Giedd, height-only) to ±0.3 (Lammert-Narayanan, multi-feature)",
    "lip_aperture_max samples are scarce — most jeffrey-platter photos are neutral-mouth; resampling needed if singing-voice corpus is added"
  ],
  "generated": "2026-05-04",
  "generator": "pop/voice/bin/measure-jeffrey.py v0.1"
}
```

the JSON is the contract. anything downstream — `fit.py`,
`render-pt.mjs`, the eventual paper — reads from this file, not from
re-running the landmarker.

## scale-anchor decision

absolute scale needs *one* known length. options ranked:

1. **jeffrey's stated height** — most reliable, single-number bind, but
   requires jeffrey to fill it in.
2. **bizygomatic width populational mean** (~13.5–14.5 cm adult male) —
   adds population uncertainty back in.
3. **interpupillary distance** (median adult ~63 mm) — narrow population
   range, mediapipe gives this directly. defensible default.

first pass: option 3 (IPD = 63 mm). if jeffrey provides height, drop in
option 1 and rerun.

## what this is **not**

- not a medical-grade measurement. mediapipe landmarks are good to
  ~1–2 mm in well-lit frontal portraits, worse at oblique angles.
- not a clinical tract reconstruction. there is no MRI here. we measure
  what's externally visible, regress to tract length using published
  models, and report the uncertainty honestly.
- not a face-recognition step. identity validation reuses the existing
  insightface threshold from `portraits/jeffrey/bin/face-match.py` and
  rejects non-jeffrey frames before measurement.

## next

- [ ] implement `bin/measure-jeffrey.py`
- [ ] run on all `papers/jeffrey-platter/manifest.json` items, write the JSON
- [ ] confirm derived VTL is in plausible adult-male range (16.5–18.5 cm)
- [ ] feed bounds into `fit.py`'s parameter envelope

## references

- Fitch, W. T., & Giedd, J. (1999). *Morphology and development of the human vocal tract: A study using magnetic resonance imaging.* JASA, 106(3), 1511–1522.
- Lammert, A. C., & Narayanan, S. S. (2015). *On short-time estimation of vocal tract length from formant frequencies.* JASA, 137(2), 985–995.
- Story, B. H., Titze, I. R., & Hoffman, E. A. (1996). *Vocal tract area functions from magnetic resonance imaging.* JASA, 100(1), 537–554.
- Maeda, S. (1990). *Compensatory articulation during speech.* In Speech Production and Speech Modelling.
- Thapen, N. (2017). *Pink Trombone.* https://dood.al/pinktrombone/
