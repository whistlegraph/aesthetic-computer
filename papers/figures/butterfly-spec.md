# Whistlegraph Butterfly — Formal Form Specification

A formal English description of the four-stroke "Butterfly Cosplayer" emblem, derived from the canonical graphic score `system/public/assets/whistlegraph/butterfly-cosplayer/butterfly-cosplayer-score.png` (composed by Jeffrey Alan Scudder, December 29, 2019).

This spec exists so image-generation prompts can describe the butterfly precisely without needing the score image as a reference, and so any rendering pipeline can verify a generated butterfly against a written form.

---

## 1. Overall Gestalt

A small **four-petal-wing butterfly**, drawn as a single contiguous outline (no fill), in a plain dark ballpoint or felt-tip line on cream paper. Reads at a glance like a child's hand-drawn marker doodle of a butterfly: charmingly imperfect, NOT geometric, NOT symmetric to ruler precision, NOT painterly.

The butterfly is **front-on** (wings spread flat to the viewer), vertically centered around its body axis, and **slightly taller than wide** (approx. 1.0 wide × 1.1 tall, roughly square but with a hint of vertical reach from the antennae).

It carries **NO color, NO fill, NO shading, NO ornament** in its emblem form — only the closed black outline and (optionally) a small body-line and antennae.

---

## 2. The Four Strokes (Wings)

The wing structure is a **four-petal-cloverleaf rendered as four overlapping rounded lobes**, NOT four separate ovals. Each lobe is a soft humped arc that loops back into the next lobe through the body. Reading clockwise from the upper-left:

1. **Upper-left wing.** A rounded petal-like lobe that begins at the body, swells out and up to the upper-left, peaks, and curves back down to the body. The peak is at roughly the 10–11 o'clock position. Top edge slightly higher than the upper-right peak (asymmetry intentional — it's hand-drawn).
2. **Upper-right wing.** Mirror of stroke 1 toward the upper-right, peaking at roughly 1–2 o'clock. Top edge slightly lower than stroke 1's peak.
3. **Lower-right wing.** A rounded petal-like lobe to the lower-right, peaking at roughly 4–5 o'clock. Slightly smaller and rounder than the upper wings — like the hindwings on a swallowtail without the tails.
4. **Lower-left wing.** Mirror of stroke 3 to the lower-left, peaking at roughly 7–8 o'clock.

Each lobe is a **single confident curve** (not a stack of strokes). The four lobes meet at a **central pinch** where the body sits — a small hourglass-narrow waist where opposing wing pairs touch each other.

The upper wings are slightly larger and rounder than the lower wings. The whole wing assembly resembles a **four-leaf clover** more than a real butterfly's wing geometry.

---

## 3. Body and Head

A small vertical body line runs along the central axis from the bottom of the wing assembly to just above the upper wing tips. Two parts:

- **Thorax/abdomen.** A short vertical rectangle (slightly taller than wide), sitting in the center of the four-wing pinch. Sides are roughly straight, top and bottom are gently curved.
- **Head.** A tiny circular or dot-like nub at the top of the body, just above the wing pinch.

The body is drawn with the same line weight as the wings — no thickening, no fill.

---

## 4. Antennae

**Two single-stroke antennae** rise from the head, one to the upper-left and one to the upper-right. Each antenna is:

- A thin curved line (no thickening),
- Roughly the height of the wing assembly itself (long — they reach above the wings),
- Curving outward away from the central axis like a soft "C" or a pair of opposing question-mark hooks,
- Ending in a **tiny ball/dot** at the tip (the antenna club).

The antennae are NOT straight, NOT branched, NOT feathery — just two confident curving strokes with a dot at each tip.

---

## 5. Line Quality

- **Single confident line.** The whole butterfly reads as drawn in ONE sitting, with the line varying only slightly in thickness from natural pen pressure.
- **Slight imperfection.** Curves are not mathematically smooth; lobes are not identical; the symmetry is approximate, NOT mirror-perfect.
- **No hatching, no fill, no cross-contour, no shading.** Pure outline.
- **Color.** Plain dark ink (black or near-black) on cream paper. When used as an inscribed emblem on a colored object (e.g. on the back of a chartreuse laptop lid), the line stays dark against the surface, NOT recolored.

---

## 6. Scale and Placement (when used as a lid emblem)

When the butterfly appears in place of a manufacturer's logo on the back of jeffrey's chartreuse-green MacBook Neo lid:

- Centered on the lid, both horizontally and vertically,
- Occupies roughly the **central 35–40%** of the lid's height,
- Drawn at the same line weight as a permanent-marker inscription (~2–3px equivalent at the rendered scale),
- Sits ON the chartreuse surface, NOT cut out of it (the green of the lid shows through any open shape; the butterfly is a black line drawing, not a backlit cutout).

---

## 7. What it must NOT look like

- NOT a realistic butterfly silhouette (no scalloped wing edges, no eye-spots, no veins),
- NOT a corporate butterfly logo (no gradients, no geometric mirror-symmetry, no bevels),
- NOT a silhouette/fill (the wings are open outlines, the cream/chartreuse background shows through),
- NOT a single stroke with a flat plane (the four lobes are clearly four humps, not one ellipse),
- NOT an emoji-style butterfly (no tilted 3/4 perspective, no decorative dots inside wings),
- NOT a Monarch / Swallowtail with tails or pointed wings — the lobes are ROUND.

---

## 8. Compact prompt fragment

For inclusion verbatim in image-generation prompts:

> **The butterfly emblem is a small hand-drawn FOUR-STROKE WHISTLEGRAPH BUTTERFLY in confident dark ink: four rounded petal-like wing-lobes meeting at a central pinched waist (a four-leaf-clover gestalt — two slightly larger upper lobes peaking near 10–11 and 1–2 o'clock, two slightly smaller lower lobes peaking near 4–5 and 7–8 o'clock), a short rectangular thorax-and-abdomen body sitting in the central pinch with a small dot for a head above, and TWO long curving single-stroke antennae rising up-and-outward from the head and ending in tiny ball-dots. Pure outline only — no fill, no shading, no color, no veins, no eye-spots. A child's confident felt-tip butterfly, charmingly asymmetric, NOT a corporate logo, NOT a realistic Monarch, NOT a silhouette.**
