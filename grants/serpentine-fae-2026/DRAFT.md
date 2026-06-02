# Serpentine — Future Art Ecosystems R&D Fellowship: Art × Convergence — Application Draft

> **Deadline:** 2026-06-07, midnight BST
> **Award:** £10,000 + travel & accommodation for the London weekends
> **Format:** Hybrid, six months (Sep 2026 – Mar 2027)
> **Apply (Airtable form):** https://airtable.com/appB9uhkcEZ0NUpbQ/pagB33PwCFgLTUTww/form
>
> **Status: form-ready.** Answers below are mapped to the five things FAE
> says a strong application must do (see RESEARCH.md). The Airtable form is
> login-gated and could not be read headlessly — **jeffrey must open it to
> confirm the exact field order / any word caps and to upload media.** These
> answers are written tight and self-limiting; paste and trim only if a
> field enforces a shorter cap.

---

## Working title

**A Voice of My Own — building an artist-owned voice synth inside Aesthetic Computer**

---

## 1 · Practice & context
*(FAE: "clearly describe the applicant's practice and context")*

I am a painter and toolmaker. For six years I have been building **Aesthetic
Computer** — a mobile-first runtime, operating system, and social network for
creative computing — as an alternative to the platforms most artists currently
rent from. It is one system end to end: a kernel that boots refurbished laptops
straight into it, a piece API artists program against, **KidLisp** (a minimal
Lisp for generative art that ships inside it), a publication channel
(papers.aesthetic.computer), and a daily public output. Roughly thirty papers,
an open codebase, and a small community (biweekly NELA Computer Club demos at
Plot.Place, Chinatown LA) come out of it. My tools *No Paint* and *notepat* have
each reached the front page of Hacker News; my work is in the collections of
KADIST (San Francisco) and SMK (Copenhagen). I am currently Artist in Residence
at UCLA's Social Software studio. The throughline is simple: I build the
instruments I make my own work with, in public, and hand them to other people.

---

## 2 · Research question / line of inquiry
*(FAE: "articulate a compelling research question, concern, or line of inquiry")*

**What does it mean, in 2027, for an artist to own the throat they are sung
through?**

There is one unfinished room in Aesthetic Computer: my own voice. Today I sing
through a *hosted* clone of myself — an ElevenLabs model, `jeffrey-pvc` — inside
a pipeline where every sung word is sliced, re-pitched, elongated, and
harmonised in code. The composition already happens in my code; the *voice
itself* is still rented from a server I don't control. The inquiry is whether I
can walk that last hop out of the cloud: fit a **Pink Trombone** physical
vocal-tract model to my own clone, on a tiny training corpus, and ship the
result as a small C voice that runs natively inside AC's kernel with no internet
at all. The question is not technical trivia — it is the most personal case of
the larger one this whole practice asks: which parts of making art should an
artist *own outright* versus rent from a convergent AI service, and what
specifically becomes possible when you own the throat, the runtime, and the
publication channel together.

---

## 3 · Connection to Art × Convergence
*(FAE: "explain connection to the Art × Convergence theme")*

Convergence asks how AI's growing capacity to pursue goals, model environments,
and act in the world reshapes cultural systems. Most artists meet that question
from the outside — as users of hosted creative-AI services, or as critics of
them. I want to meet it from inside, by building the artist-owned infrastructure
for the most intimate of those services: voice. That reframes "convergence" from
something that happens *to* artists into something an artist can *own a piece
of*. Aesthetic Computer is already the long-form argument that artists can hold
their own runtime, kernel, social layer, and papermill rather than renting them;
the voice synth is that argument carried into the body. This is an infrastructure
question as much as an aesthetic one — what public value is created when the
substrate under art × tech is artist-run rather than platform-rented — which is
the layer FAE's published research has taken more seriously than anyone.

---

## 4 · Why this fellowship suits my development
*(FAE: "show why the Fellowship suits your development")*

**Documented process is the deliverable, and documenting in public is already
what AC does** — source, papers, daily recaps, dossiers on every adjacent
organisation. The fellowship doesn't ask me to do something foreign; it gives me
six months and a serious room to do a specific, hard sub-thread with the care it
deserves, instead of squeezing it between everything else. It is also the rare
programme that funds *the practice of building infrastructure* rather than
*artworks that use it* — which is the right shape for what AC actually is. And
the panel makes the work self-explaining from two sides at once: **Holly
Herndon** has pushed voice-cloning, ownership and identity hard from the music
side, so the vocal spine needs no translation; **Mariana Mazzucato, Venkatesh
Rao and Lila Ibrahim** make the artist-owned-infrastructure and public-value
framing legible to people who think about systems and economics for a living.
Six months is a realistic window to get the tract-fit harness from research toy
to a usable AC instrument and the first works made *for* it.

**What the six months produces:**
- *Months 1–2.* Finish the pop/ vocal pipeline as a stable, documented,
  artist-facing tool; fix the open syllable-to-note alignment drift; publish
  the recipe format.
- *Months 2–4.* Pink Trombone fitting in earnest — build the corpus, write a
  perceptual-loss fitter, port the best parameter trajectory into a small C
  runtime that loads on AC's kernel.
- *Months 4–6.* Make work *for* the new voice — short songs, sung essays, a
  piece that reads one of my papers aloud in my own throat with no internet.
  Output: a paper, a public reel, and the binary.

**On risk (stated plainly):** fitting Pink Trombone to a clone of myself with
perceptual loss is research, not engineering — it may not converge cleanly. The
fallback is honest and useful: a much richer per-word post-production pipeline
plus a published dossier of what failed in the fit — itself a contribution to
the field's shared knowledge of *how far you can carry one voice with how
little*.

---

## 5 · Support, dialogue & conditions I need
*(FAE: "identify needed support / dialogue / conditions")*

- **Time and a deadline-bearing room.** The £10k and the six-month cadence buy
  focus on one thread that otherwise stays perpetually "next."
- **Dialogue across the cohort and panel** — specifically on (a) voice cloning,
  ownership and identity (the Herndon lineage) and (b) what "public value" and
  artist-owned infrastructure mean when an individual artist, not an institution,
  holds the stack (Mazzucato / Rao).
- **A critical audience for the documentation** as it's made, not only at the
  end — AC publishes continuously and benefits from response in the open.
- **Practical:** confirmation of the three London weekend dates (Sep / Nov /
  Mar) so I can hold them against the UCLA Social Software residency and the
  NELA Computer Club rhythm. Travelling from Los Angeles; the covered travel +
  accommodation makes three trips tractable.

---

## Short bio (~100 words, if a separate field)

Jeffrey Alan Scudder (b. 1989) is a painter and toolmaker based in Los Angeles.
He builds instruments and tools for other artists across performance painting,
software, and teaching. He is the author of **Aesthetic Computer**, a
mobile-first runtime and social network for creative computing, and **KidLisp**,
a minimal Lisp for generative art that ships inside it. Yale MFA (2013), Ringling
BFA (2011). Currently Artist in Residence at UCLA Social Software. His tools *No
Paint* and *notepat* have each reached the front page of Hacker News. Work in the
collections of KADIST (San Francisco) and SMK (Copenhagen).

---

## Links to include

- Aesthetic Computer — https://aesthetic.computer
- Bio / CV — https://justanothersystem.org/cv
- Papers — https://papers.aesthetic.computer
- KidLisp — (repo / docs link)
- A pop/ track showing the current jeffrey-pvc + per-word pipeline — (URL)

---

## Media samples (to upload / link)

1. **pop/ vocal pipeline reel** — ~90 sec: current jeffrey-pvc + per-word
   post-production in action, with the `.edit.json` recipe shown alongside.
2. **Pink Trombone harness sketch** — ~60 sec: current fitting state on the
   123 / abc corpus, however rough.
3. **AC Native OS demo** — ~60 sec: a refurbished ThinkPad cold-booting from
   USB straight into the AC kernel (where the new voice will live).
4. **A paper PDF** — `papers/arxiv-notepat/notepat.pdf` (or `arxiv-os/os.pdf`)
   as a written companion piece.

---

## Submission checklist

- [ ] **jeffrey:** open the Airtable form, confirm exact fields + any word caps,
      map these answers onto them (reorder/trim only if a field is shorter)
- [ ] Pull the 3 short media reels (pop pipeline, PT sketch, Native OS boot)
- [ ] Confirm the three London-weekend windows vs. UCLA residency + NELA rhythm
- [ ] Final read for the single spine — voice, *not* AestheticAnts too
      (see `feedback_proposals_single_spine.md`)
- [ ] Submit before 2026-06-07, midnight BST
