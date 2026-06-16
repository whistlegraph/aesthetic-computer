# Restless Egg — Concept Film · NARRATIVE v1
### "Play Again" — a 4-beat illustrated story (jeffrey-as-character, /pop style)

A concept film, not an explainer. **jeffrey is an illustrated character** (pop
pipeline: painterly panels, often 3/4-back or silhouette — faces fail Seedance
moderation). Each beat is one illustration **in res** → **Seedance 2 image-to-
video** (fal, image-to-video) → a ~5s clip. Cuts > morphs. Bed = notepat playing
itself; jeffrey-pvc VO can ride on top (trim of the existing 49s take).

Arc: **painter → instrument → commons → invitation.** Real AC lore throughout.

---

### BEAT 1 — THE PAINTER  *(origin · ~5s)*
**Illustration:** a cluttered LA studio at night, green-terminal glow + warm
desk lamp. jeffrey from 3/4-behind at the desk — a painter's posture, brush in
hand — but the "canvas" is a laptop screen of light. A quiet stack of dead,
shut laptops beside him (the e-waste). Citrus-neo palette, painterly.
**Lore:** a painter (Ringling/Yale) who spent a decade deciding the computer is
paint — No Paint, then Aesthetic Computer.
**Seedance motion:** slow push-in; the brushstroke on screen ignites into a
ribbon of glowing generative code.
**VO:** "I'm a painter who spent the last decade building instruments instead of paintings."

### BEAT 2 — THE AWAKENING  *(the instrument · ~5s)*
**Illustration:** his hands flip open a salvaged ThinkPad and slot a USB stick;
the screen blooms a vivid grid of **notepat note-tiles**, a bright waveform
leaping across the top, warm light + color spilling out of the machine into the
dark room. The fifty-dollar laptop becoming an instrument.
**Lore/UI:** AC Native boots bare-metal in ~7s; notepat's playable tiles; a real
128-voice synth under it.
**Seedance motion:** the boot-bloom — tiles igniting in sequence, light flooding outward.
**VO:** "It boots a salvaged laptop straight into an instrument — no operating system underneath it."

### BEAT 3 — THE COMMONS  *(the world · ~5s)*
**Illustration:** pull way back — the one glowing screen becomes **many**: a dark
map/constellation of thousands of little screens and @handles, each blinking on
with a tiny distinct **KidLisp** program (a galaxy of generative art). People
playing together, threads of light connecting them.
**Lore:** ~18,000 makers, 16,000+ programs — a real commons, not an audience.
**Seedance motion:** zoom-out into the galaxy of creative screens, lights blinking alive.
**VO:** "It grows out of Aesthetic Computer — where eighteen thousand people have made sixteen thousand little programs."

### BEAT 4 — THE INVITATION  *(the ask · ~5s)*
**Illustration:** back to one refurbished laptop, alone on a warm softly-spotlit
stage, glowing like an instrument waiting to be played. jeffrey's silhouette
sets it down and steps back into the dark. Hopeful, reverent.
**Lore/wordmark:** Aesthetic Inc. — instruments you play, not tools that make
you efficient.
**Seedance motion:** slow settle, the glow steadies, vignette closes.
**VO:** "I want to make computing something you play again. That's what I'd do with Restless Egg."

---

## Production plan
1. **Illustrate** the 4 key-frames in res (gpt-image, pop illustration style — one
   cohesive painterly look, jeffrey 3/4-back / silhouette, AC palette).
2. **Animate** each with **Seedance 2 image-to-video** (fal harness, ~5s, 16:9).
3. **Assemble**: 4 clips, hard cuts, notepat bed + trimmed jeffrey-pvc VO,
   captions. ~20–25s concept film (or stretch beats to land ~30s).
4. **boardwizard** (Swift app) manages this narrative — beats, their key-frame
   image, motion prompt, generated clip, VO line, order. (Evolution of the
   ShotWizard build — narrative-oriented, native, no HTML.)

## Notes
- Keep jeffrey faceless/3-4-back per pop moderation rule.
- Each beat is visually DISTINCT (studio → machine bloom → galaxy → stage) —
  fixes the "every panel is the same laptop" problem.
- This supersedes the talking-head 7-shot board for the *concept-film* cut.
