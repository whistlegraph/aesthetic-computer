# Work Proposal — SSRC Just Tech Fellowship 2027
### Jeffrey Alan Scudder · final (cap: ≤3,000 words written, or ≤10-slide deck)

**Project:** *The Public Computer* — Aesthetic Computer as a digital community
commons, and AC Native as the public device library that lets a community own
it.

---

## Central concept and research question

Personal computing is being quietly re-constituted, from two directions at once.
The demand for AI compute is pulling everything toward the data center, turning
the machine on your desk into a thin client — a rented window onto someone
else's computer. At the same time, the consumer upgrade cycle is declaring
hundreds of millions of working machines obsolete: Windows 10's end-of-life
alone stranded an estimated **240 million** x86_64 laptops, against a backdrop of
roughly **62 million tonnes** of e-waste a year. The personal computer — the most
basic site of creative and civic freedom — is becoming something you neither own
nor can read all the way down.

My question is the counter-move: **Can personal computing be re-founded as a
commons — owned in common, legible to its members, and run on hardware the
market has discarded — and what does it actually take to operate one in public?**

The fellowship year turns an existing, working system into a concrete answer: a
**public device library** of salvaged laptops flashed with a creative operating
system, paired with public Flash Days, family workshops, and an open on-ramp
into a community that already exists.

## How the project engages technology substantively

This is not a critique delivered from outside the machine. It is a working
counter-model built down to the kernel, already running, and fully public in the
repository.

- **A bare-metal creative OS.** *AC Native* boots from USB via UEFI as the first
  process (PID 1) on a custom Linux 6.14.2 kernel with an embedded initramfs —
  no systemd, no shell, no consumer userland — and reaches a running creative
  piece in about 7.3 seconds. Graphics via DRM/KMS, input via raw evdev, audio
  via ALSA at 192 kHz with 32-voice polyphony, plus WiFi, camera, and on-device
  text-to-speech. Each build compiles in its own git hash and a spoken build
  name, so any machine can tell you exactly what it is running and verify itself.
- **A full instrument on the metal.** A 3,870-line C synthesizer implements all
  128 General MIDI voices through physical modeling — digital waveguides for
  strings and brass, modal synthesis for bells — so a fifty-dollar laptop is a
  serious musical instrument, not a toy.
- **A commons of authorship.** Members publish JavaScript (`.mjs`) and KidLisp
  (`.lisp`) programs into shared, URL-addressable space (`@handle/piece-name`);
  KidLisp snippets earn short `$codes` cached for anyone to run or remix. The
  language is intentionally minimal — 118 built-ins — so the barrier to becoming
  an author is design, not aptitude.
- **A real social fabric.** Durable, moderated, multi-channel chat that overlays
  any program; a "moods" microblog where a member's status becomes the site's
  message-of-the-day; reactions; a community news board with posts, comments,
  and votes; and a two-tier presence signal ("here" vs "online"). Identity
  dual-writes to the open ATProto/Bluesky social web rather than a walled garden.
- **Anti-extractive by construction.** Self-hosted on its own stack and its own
  git server; local-first, with an encrypted on-device memory store and remote
  sync off by default; a working "erase and forget me" endpoint; and no
  advertising or tracking infrastructure anywhere across its ~70 database
  collections. There is no surface for surveillance because the goal was never
  attention.

The platform is not a proposal: it has run continuously for roughly three and a
half years, across 20,109 public commits and ~20 contributors, and today carries
2,907 handles and 402 community pieces.

## Perspective, method, and distinguishing approach

My method is **build-and-operate in public.** I don't study a community from the
outside; I run one, and I publish the entire apparatus — code, voice, governance,
and finances — as I go, including a "papermill" that presses the project's own
thinking into freely available papers and printable cards. Three commitments
distinguish the work from its neighbors:

1. **Knowability as a civic property.** Software a public is meant to own must be
   software that public can read. The small-language, small-pieces design ("a
   piece of this system should fit in one mind") is a political claim made
   technical: self-governance requires legibility.
2. **Salvage as a stance.** Building forward on discarded hardware is both an
   environmental position and an accessibility one — it drives the per-seat cost
   to where a library, a school, or a family can actually reach it (~$50 raw;
   ~$128 per seat refurbished and flashed). This is roughly two orders of
   magnitude below the Princeton Laptop Orchestra's per-laptop cost, which kept
   that powerful idea locked inside wealthy universities.
3. **A place, not a platform.** The design goal is belonging — a handle, a mood,
   a published piece — not engagement metrics. The architecture reflects that
   all the way down.

My background as a painter and a teacher (UCLA, Parsons, Southern Oregon; and
currently Author in Residence at UCLA's Social Software studio with Casey Reas
and former Just Tech fellow Lauren Lee McCarthy) is the method's other half: the
work is judged by whether a person can pick up the instrument and make something
that is unmistakably theirs.

## The fellowship year — scope and what advances

A realistic one-year arc for the unrestricted award, paced around public events:

- **Q1 — The Library, v1.** Build out a lending fleet of ~10–15 reflashed
  ThinkPad laptops; harden the AC Native flash/over-the-air-update pipeline for
  non-expert stewards; write the lending, care, and safety documentation that
  lets the fleet leave my hands.
- **Q2 — Flash Days.** Run public Flash Days and family workshops in Los Angeles,
  via existing partners (NELA Computer Club; the UCLA studio), where people flash
  a salvaged laptop and leave able to write their first KidLisp piece. Document
  accessibility outcomes honestly — who it works for, where it doesn't.
- **Q3 — Widening the commons.** Lower the barrier to authorship and
  co-stewardship: an onboarding path, a "your first piece" track, and plain
  governance writing about how moderation, handles, and publishing actually
  function — so stewardship can spread beyond a single maintainer.
- **Q4 — Public account.** Publish a plain-language field report on what it costs
  and what it takes to operate a computing commons, written to be copied, plus
  the standing references for the OS and the language.

What "meaningfully advances this year": the device library goes from prototype
to a standing, lendable program; the online community gains a real on-ramp for
new authors and co-stewards; and the whole model is documented well enough to be
reproduced elsewhere. The deliverables are tangible — a fleet, a series of public
events, and a written method — not a research finding alone.

## Anticipated challenges and mitigation

- **Single-steward risk.** AC today has one dominant maintainer; a commons that
  depends on one person is fragile. *Mitigation:* widening contributorship and
  writing the governance down are explicit Q3 deliverables, and the cohort is the
  venue for pressure-testing them.
- **Hardware heterogeneity.** Salvaged laptops vary widely. *Mitigation:*
  standardize the lending fleet on a small set of known-good models (ThinkPad 11e
  Yoga) while keeping the OS general, and publish a compatibility list so others
  can extend it.
- **Local AI on low-end hardware.** A live research thread — letting language
  models into the OS as local, owned faculties rather than data-center landlords
  — may simply not fit on a fifty-dollar machine. *Mitigation:* treat the limit
  itself as a publishable finding (how far a personal computer carries on its own
  before it must call out), so the risk produces knowledge either way.
- **Sustainability beyond the grant.** *Mitigation:* the model is deliberately
  low-cost (self-hosted, salvage hardware); the unrestricted award funds the
  build-out, and the field report is aimed at portability rather than dependence
  on me or on the grant.
- **Community care at scale.** Moderation scales with people. *Mitigation:* the
  moderation layer already exists (mutes, filtering); the year formalizes the
  norms around it rather than inventing them under pressure.

## Awareness of related work and field context

The project sits among, and distinguishes itself from, several lineages.
**Laptop-orchestra and live-coding practice** (the Princeton Laptop Orchestra,
TOPLAP, Sonic Pi) — but at two orders of magnitude lower per-seat cost and as a
standing community rather than an ensemble. **Accessible creative-coding tools**
(Processing, and p5.js, whose creator is a prior Just Tech fellow and a current
collaborator) — but extended downward to the operating system itself, not only
the sketch. **Right-to-repair and e-waste movements** — but answering "what do
you actually *do* with the salvaged machine" with a full creative environment
rather than a recycling logistics. And the **small-internet / IndieWeb / ATProto
commons** — which AC joins by dual-writing identity to the open social web rather
than enclosing it. The distinctive contribution is holding the *whole stack*, from
kernel to community, in one legible, commonly-owned system.

## Public contribution and dissemination

- **The device library and Flash Days** put the argument directly in public
  hands — people leave owning a working computer and able to program it.
- **Everything is open and free:** the source, the `papers/` research platter,
  and the printable cards; the field report is written explicitly to be copied
  and reused by libraries, schools, and other communities.
- **The existing community** (2,907 handles; an active chat / moods / news
  commons) is the standing audience and co-author — not a hypothetical one.
- **Cohort collaboration:** because anyone can publish a piece into Aesthetic
  Computer, it is a hospitable substrate for other fellows' work, making the
  optional collaborative seed funding a natural fit.

The aim is not a better app. It is a working demonstration, repeatable by others,
that the personal computer can still be a public thing — owned in common, read
all the way down, and built on the machines the market threw away.

*(~1,180 words — well under the 3,000 cap; room to expand a Q-by-Q budget
narrative or a deeper related-work section if the portal allows, or to compress
into a ≤10-slide deck.)*
