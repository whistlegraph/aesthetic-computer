# SSRC Just Tech Fellowship 2027 — Application DRAFT v0

**Applicant:** Jeffrey Alan Scudder (Aesthetic Computer / Studio Zollo) · US-based
**Deadline:** 2026-06-28 23:59 EST · **Award:** up to $60,000 unrestricted, 1 yr
**Status:** v0 — written long, NOT yet trimmed to the word caps. See
`RESEARCH.md` for the exact component limits and the portal checklist.

> **Spine (one line):** Aesthetic Computer is a digital community commons —
> a creative-computing operating system that reclaims the personal computer
> as something a public can own, build on, and belong to, running on the
> hardware the platform industry has thrown away.
>
> **Society-facing question:** Who owns the personal computer? When a handful
> of platforms decide what a computer is for — and an upgrade cycle strands
> hundreds of millions of working machines as e-waste — what would it take to
> re-found personal computing as a commons: owned in common, legible to its
> users, and hospitable to a culture rather than an audience?

---

## A. Personal statement (cap: ≤1,000 words written, or ≤5-min video)

I make software that tries to give the personal computer back to the people
using it. For three and a half years I have built and operated **Aesthetic
Computer**, a creative-computing platform and small online society that, as of
this writing, carries **2,907 registered handles**, **402 community-authored
pieces**, and a continuous public record of **20,109 commits** across roughly
twenty contributors. It is not a product with users. It is closer to a place
with residents — people who have a name there, a mood that becomes the site's
banner, a chat that follows them into any program, and the standing to publish
their own programs into shared address space.

I came to this from teaching and from art. I have watched what happens to
creative people when the tools they depend on are rented rather than owned:
the work is hostage to a subscription, the data to a platform, the very
definition of "what this computer does" to a company whose incentives are not
theirs. I have also watched the inverse — what a person becomes when a computer
is finally legible to them, when they can read the whole instrument and change
it. Aesthetic Computer is my attempt to build that second condition at the
scale of a community, and to keep it honest by running the entire thing in the
open: self-hosted, source-readable, local-first, with no advertising and no
tracking infrastructure anywhere in its seventy-odd database collections.

The technology-and-society stakes became concrete to me in the last year, when
Windows 10's end-of-life stranded an estimated **240 million** working x86_64
laptops and the world generated roughly **62 million tonnes** of e-waste. These
are not abstractions; they are a planetary population of perfectly good
machines declared obsolete by a consumer OS. So I built one that doesn't
declare them obsolete. **AC Native** is a bare-metal creative operating system
that boots from a USB stick as the first process on the machine — no systemd,
no shell, no consumer userland — and has a creative environment running in
about seven seconds, with sound, graphics, input, and network. The per-seat
cost lands near fifty dollars. A salvaged ThinkPad becomes a full instrument
again.

My practice has always insisted that everyone can be a programmer, and that the
barrier is design, not aptitude. So the language at the center of AC,
**KidLisp**, is deliberately tiny — 118 built-ins — and addressable: a few
lines of it earn a short `$code` that anyone can run, share by QR, or remix.
The house code-style guide I wrote for the project states the value plainly: "a
piece of this system should fit in one mind." That is an accessibility claim
and a governance claim at once. A commons can only be self-governed if its
members can actually read it.

I am applying to Just Tech because the question I am working on is not a
technical one, it is a public one — *who gets to own and shape the computer* —
and because the program funds exactly the kind of practitioner I am: not a
researcher writing about technology from the outside, and not a technologist
chasing novelty, but someone building a working alternative and tending the
community that lives inside it. Lauren Lee McCarthy's p5.js work as a prior
fellow tells me this lane is real. The fellowship year would let me do the part
I cannot do alone: turn AC Native from a working prototype into a **public
device library** — a lending fleet of reflashed laptops, with Flash Days and
family workshops — so that "you can own your computer again" stops being an
argument and becomes something a person in a room can hold in their hands.

What I would bring to the cohort is a builder's account of these questions:
concrete, shipped, measurable, and unromantic about its own limits. AC today
has a strong central steward — me — and a real but small core of contributors;
part of the honest work of this year is widening that, and a cohort of people
thinking hard about technology, equity, and the public is precisely the room I
want to do it in.

*(~560 words — room to grow toward the 1,000 cap, or to cut for a video.)*

---

## B. Work proposal (cap: ≤3,000 words written, or ≤10 slides)

### B1. Central concept / research question

The project is **Aesthetic Computer as a digital commons, and AC Native as the
public device library that lets a community own it.** The animating question:
*Can personal computing be re-founded as a commons — owned in common, legible
to its members, and run on hardware the market has discarded — and what does it
take to operate one in public?* The fellowship year turns the existing working
software into a **circulating public library of computers**: a lending fleet of
salvaged laptops flashed with AC Native, paired with Flash Days, family
workshops, and an open onboarding path into the existing online community.

### B2. How the project engages technology substantively

This is not a critique delivered from outside the machine; it is a working
counter-model built down to the kernel. Concretely, and verifiably in the
public repository:

- **A bare-metal creative OS.** AC Native boots from USB via UEFI as PID 1 on
  a custom Linux 6.14.2 kernel with an embedded initramfs — no systemd, no
  shell, no consumer userland — and reaches a running creative piece in ~7.3
  seconds. Graphics via DRM/KMS, input via raw evdev, audio via ALSA at 192
  kHz with 32-voice polyphony, plus WiFi, camera, and on-device text-to-speech.
  Each build embeds its own git hash and a spoken build name, so every machine
  can tell you exactly what it is running. (`fedac/native/PROGRESS.md`,
  `Makefile`.)
- **A full instrument on the metal.** A 3,870-line C synthesizer implements all
  128 General MIDI voices through physical modeling — digital waveguides for
  strings and brass, modal synthesis for bells — so a fifty-dollar laptop is a
  serious musical instrument, not a toy. (`fedac/native/src/gm_synth.c`.)
- **A commons of authorship.** Members publish `.mjs` and `.lisp` programs into
  shared, URL-addressable space (`@handle/piece-name`); KidLisp snippets earn
  short `$codes` cached for anyone to run or remix. (`store-piece.mjs`,
  `store-kidlisp.mjs`.)
- **A real social fabric.** Durable, moderated, multi-channel chat that overlays
  any program; a "moods" microblog where a member's status becomes the site's
  message-of-the-day; hearts; a community news board with posts, comments, and
  votes; and a two-tier presence signal ("here" vs "online"). (`chat-manager.mjs`,
  `mood.mjs`, `chat.mjs`.)
- **Anti-extractive by construction.** Self-hosted on its own stack and its own
  git server; local-first, with an encrypted on-device memory store and remote
  sync off by default; a working "erase and forget me" endpoint; identity that
  dual-writes to the open ATProto/Bluesky social web rather than a walled
  garden. No ads, no trackers.

### B3. Perspective, method, distinguishing approach

My method is **build-and-operate in public.** I do not study a community from
the outside; I run one, and I publish the whole apparatus — code, voice,
governance, and finances — as I go. Three commitments distinguish the work:

1. **Knowability as a civic property.** "A piece of this system should fit in
   one mind." Software a public is meant to own must be software that public can
   read. The small-language, small-pieces design is the political claim made
   technical.
2. **Salvage as a stance.** Building forward on discarded hardware is both an
   environmental position and an accessibility one — it drives the per-seat cost
   to where a library, a school, or a family can actually reach it (~$50 raw,
   ~$128/seat refurbished-and-flashed).
3. **A place, not a platform.** The design goal is belonging — a handle, a
   mood, a published piece — not engagement metrics. The architecture has no
   surface for surveillance because the goal was never attention.

### B4. The fellowship year — scope and what advances

A realistic one-year arc for the unrestricted award:

- **Q1 — The Library, v1.** Build out a lending fleet of ~10–15 reflashed
  ThinkPad laptops; harden the AC Native OTA/flash pipeline for non-expert
  stewards; write the lending and care documentation.
- **Q2 — Flash Days.** Run public Flash Days and family workshops (LA first,
  via existing partners) where people flash a salvaged laptop and leave able to
  write their first KidLisp piece. Document accessibility outcomes honestly.
- **Q3 — Widening the commons.** Lower the contributor barrier: onboarding
  docs, a "your first piece" path, and governance writing about how moderation,
  handles, and publishing actually work — so stewardship can spread beyond one
  person.
- **Q4 — Public account.** Publish a plain-language field report (the project
  already runs a "papermill" that presses its own thinking into papers and
  cards) on what it costs and what it takes to operate a computing commons, so
  others can copy it.

What "meaningfully advances this year": the device library goes from prototype
to a standing, lendable program; the online community gains a real on-ramp for
new authors and co-stewards; and the whole model is documented well enough to
be reproduced elsewhere.

### B5. Anticipated challenges and mitigation

- **Single-steward risk.** AC today has one dominant maintainer. *Mitigation:*
  the year's explicit deliverable is widening contributorship and writing the
  governance down; the cohort is the venue for pressure-testing it.
- **Hardware heterogeneity.** Salvaged laptops vary. *Mitigation:* standardize
  on a small set of known-good models (ThinkPad 11e Yoga) for the fleet while
  keeping the OS general; publish a compatibility list.
- **Sustainability beyond the grant.** *Mitigation:* the unrestricted award
  funds the library build-out; ongoing costs are deliberately low by design
  (self-hosted, salvage hardware), and the field report is aimed at making the
  model portable rather than dependent on me.
- **Scale of community care.** Moderation and safety scale with people.
  *Mitigation:* the moderation layer already exists (mutes, profanity
  filtering); the year formalizes the social norms around it.

### B6. Awareness of related work / field context

The project sits among, and distinguishes itself from, several lineages:
laptop-orchestra and live-coding practice (PLOrk, TOPLAP/Sonic Pi) — but at
two orders of magnitude lower per-seat cost and as a standing community rather
than an ensemble; accessible creative-coding tools (Processing, **p5.js** —
whose creator is a prior Just Tech fellow) — but extended downward to the
operating system itself; the right-to-repair and e-waste movements — but
answering "what do you *do* with the salvaged machine" with a full creative
environment; and the small-internet / IndieWeb / ATProto commons — which AC
joins by dual-writing identity to the open social web rather than enclosing it.

### B7. Public contribution and dissemination

- **The device library + Flash Days** put the argument in public hands.
- **Open everything:** source, the `papers/` research platter, and the cards
  are public and free; the field report is written to be copied.
- **The existing community** (2,907 handles, an active chat/moods/news commons)
  is the standing audience and co-author, not a hypothetical one.
- **Cohort collaboration:** AC is a hospitable substrate for other fellows'
  work — anyone can publish a piece into it — making the optional collaborative
  seed funding a natural fit.

*(Trim toward 3,000 words, or convert to a ≤10-slide deck for submission.)*

---

## C. Two work samples (each needs a brief description)

Pick two. Candidates, strongest first:

1. **AC Native — bare-metal creative OS (demo video + repo).** A salvaged
   laptop booting from USB into a full creative environment in ~7 seconds.
   *Description angle:* the technology-and-society thesis made physical — owning
   the computer down to PID 1, on hardware the market discarded.
2. **The community commons (chat + moods + a published `@handle/piece`).** A
   short capture of the living social fabric: chat-over-any-piece, a mood
   becoming the site banner, a member's published program at its own URL.
   *Description angle:* a place, not a platform — belonging without surveillance.
3. **A KidLisp `$code` piece.** A few lines producing generative art, shareable
   by QR. *Description angle:* accessibility as a civic property — the whole
   instrument fits in one mind.

## D. Résumé / CV (≤2pp) — assemble separately

Pull from existing exhibition/teaching record (KADIST, SMK, CalArts) + the AC
operating record. Reuse the LACMA/Serpentine portfolio material in
`grants/lacma-2026/` and `grants/serpentine-fae-2026/portfolio/`.

---

## Numbers ledger (refresh at submission)
- Years operating: ~3.5 (first commit 2022-12-23) · Commits: 20,109 · Contributors: ~20
- Registered handles: **2,907** (live prod count API)
- Pieces: **383 `.mjs` + 19 `.lisp`** = 402 · KidLisp built-ins: **118**
- AC Native: boots PID 1 in ~7.3s · kernel 6.14.2 · ALSA 192 kHz / 32-voice ·
  `gm_synth.c` 3,870 lines / 128 GM voices · ~$50 raw, ~$128/seat refurbished
- Context: ~240M Win10-EOL laptops stranded · ~62M tonnes e-waste/yr
