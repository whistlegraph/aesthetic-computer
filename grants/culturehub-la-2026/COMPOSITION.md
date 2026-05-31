# Composition Sketch — *REPL*

> Backing musical concretion for the CultureHub LA application
> (`DRAFT.md`). A sketch, not a finished score: the point of the
> residency is to compose this for real. Everything below is the
> starting plan, written to be revised in Week 1.

**Working title of the piece:** *REPL*
**Duration:** ~12 minutes, continuous (loop-capable)
**Forces:** one performer (Jeffrey) on **notepat**, with **sinebells**,
**chord**, and **beat** as the rest of the stack; **KidLisp** drives
macro-timing cues and the projected graphic score.
**Center:** D — dorian inflected, with a late +2-semitone lift to E and
a warm D resolution.

---

## Why this form

A program's life is **Read → Eval → Print → Loop**. That is also a
classical statement → development → restatement → coda. Using the REPL
as the musical form is not a gimmick: it makes the piece *legible as a
score*, because the form is the same shape as the source that produces
it. The listener is handed a short motif to memorize (Read), hears it
computed on (Eval), hears it returned plainly (Print), and the piece is
built to loop (Loop). The take-away card teaches exactly this.

**The cell** (the "function" that gets called): a 4-note motif
**D – F – A – G** — minor third up, third up, step down; the natural-6
(G over a D-minor field) gives the dorian color. Memorizable on first
hearing; everything in *Eval* is a transformation of it.

---

## Instrument roles

| Instrument | Role in the piece |
|---|---|
| **notepat** | The live, expressive voice — states the cell, carries melody + counterpoint, played by hand. The variable layer. |
| **sinebells** | Sustained sine-bell halo / drone — the resonant "memory" of the piece; long decays hold harmonic context under everything. |
| **chord** | Functional harmony — block voicings that define each region; mostly sequenced, triggered live at section seams. |
| **beat** | Rhythmic substrate — sine-derived percussion (the hellsine method, so the palette stays all-AC); absent in *Read*, the engine of *Eval*, gone again by *Print*. |
| **KidLisp** | Macro clock + section cues + the projected graphic score. The KidLisp source *is* the score document (side B of the card). |

---

## Form (continuous; timings approximate)

**0. BOOT** — `0:00–0:45`
One sinebell on **D**, swelling from silence; the room tunes to it. No
pulse. (The "load.")

**1. READ** — `0:45–3:30`
The cell is **stated once, plainly**, slow and rubato, notepat solo over
the D sinebell drone. Stated a second time, harmonized by a single
sustained **Dm9** from chord. No beat. The listener is given the
material to hold. Settles toward an implied ~72 bpm.

**2. EVAL** — `3:30–7:30`
The computation. **beat** enters (sine-kick, half-time feel ~128 bpm);
**chord** moves: `Dm – B♭ – C – Gm` modal turns, with a bright feint to
**F major** as a "return value." notepat fragments the cell —
inversion, augmentation, stretto against sinebells; density compounds
like recursion. Climax `~6:45`: a **+2-semitone lift to E**, full
stack, the cell restated forte. This is the only place all four
instruments play at once.

**3. PRINT** — `7:30–10:30`
Texture clears to "output." The cell returns **whole and harmonized** —
chord now consonant, a Picardy turn toward **D major** under a lyrical
notepat line, sinebells haloing. This is the legible version — the one
engraved on the card. Slow, warm, conclusive.

**4. LOOP** — `10:30–12:00`
Short, loop-friendly coda. The opening D sinebell returns; the cell
reduces to its **first two notes (D – F)**, thinning. The piece is
written so this tail can splice straight back to BOOT — performed once
as an ending, or actually looped live. Ends by decaying to the single
D, or by looping.

---

## The score as a document (the take-away card)

One single-sheet card, engraved through the papermill
(`cards-convert.mjs` / arXiv pipeline):

- **Side A — graphic score.** A horizontal time-strip in the AC
  menubar-piano-roll visual language: four colored lanes (notepat /
  sinebells / chord / beat), the four REPL phases labeled across the
  top, and **the cell drawn as a recurring glyph** every time it is
  "called" — so the eye can track the recursion. Whistlegraph is the
  precedent (`papers/arxiv-whistlegraph/`).
- **Side B — the source.** The actual KidLisp/AC listing that produces
  the structural backbone of the piece. Runnable: typing it at the AC
  prompt performs a reference rendering. Reading the card teaches the
  piece; running it plays it.

This is the literal claim of the project: the instrument, the
performance, and the score are one material.

---

## Performance practice

notepat is **played live** — the expressive line, the rubato in *Read*,
the climax phrasing in *Eval*. chord/beat/sinebells run on the KidLisp
macro clock but are **triggered and modulated live** at the section
seams (tempo of the *Eval* entry, the timing of the E lift, the length
of the *Loop*), so the public night is a performance, not a playback.
Solo is the default; one invited LA co-performer (a second notepat or a
voice doubling the cell in *Print*) is a Week-1 decision, not a promise.

---

## Realization in the repo (bottom-up, deterministic reference)

- Composed in the **`pop/` method**: notepat `.np` data + `score-pitch`
  + fixed `bpm`; harmony hand-shaped, never end-to-end generated.
- **KidLisp** owns structure + the projected score; the same source is
  side B of the card.
- A **seeded deterministic bake** (cf. `pop/hellsine/bin/bake.mjs`)
  renders a fixed reference recording for documentation, while the live
  performance stays the variable, expressive version.
- Built and rehearsed in the CultureHub studio for the signal +
  projection path.

---

## Honest risks (what Week 1 is for)

1. **Engraving legibility** — turning an executable piece into a static
   sheet a stranger can both *read* and *re-run* has not been done in AC
   before. This is the actual research; budgeted to fail openly.
2. **Live coordination of four layers** — keeping the live notepat
   expressive while cueing chord/beat/sinebells by hand needs real
   rehearsal time (that is what the room buys).
3. **Form length** — 12 minutes is a target; *Eval* may want to be
   shorter. The composition is locked by end of Week 1, the score by
   mid-Week 2, the performance on the final day.
