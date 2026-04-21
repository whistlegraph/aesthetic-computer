# LACMA Art + Technology Lab 2026 — Application (Submittable copy-paste source)

> **Deadline:** April 22, 2026, 11:59 PM PST
> **Submit at:** https://lacma.submittable.com/submit/348727/2026-art-technology-lab-grants
> **Questions:** lab@lacma.org
>
> This markdown file is the canonical source for form fields. The authoritative narrative PDF is `lacma-2026.pdf` (compiled from `lacma-2026.tex`). Keep them in sync.

---

## Project Name

Aesthetic Computer: The Unfinished Instrument

## Three Descriptive Words

instrument, language, network

## One-Sentence Project Description

Aesthetic Computer is a bare-metal creative computing system — custom hardware, a handmade programming language, and a social network — that reimagines the personal computer as a live musical instrument for art.

---

## Full Project Description (500 words max)

_[499 words — right at the cap]_

Aesthetic Computer (AC) is a creative computing platform built from first principles. It consists of three interlocking layers: a bare-metal operating system that boots directly into art software, a custom programming language called KidLisp for generative art, and a social network where anyone can publish and share interactive programs called "pieces."

The core provocation is simple: what happens when you strip away the consumer operating system — the notifications, the app stores, the surveillance — and build a computer that does nothing but help you make things?

**AC Native** is our answer in hardware. It is a Linux kernel that boots directly into art software on x86_64 UEFI laptops, running a custom C runtime as PID 1 — no desktop, no window manager, no browser. The system renders graphics through DRM without a compositor, reads input from raw evdev streams, and synthesizes audio sample-by-sample through ALSA at 192 kHz with 32-voice polyphony. A built-in `code` command drops into a native terminal running Anthropic's Claude Code — making AC Native the only bare-metal creative OS we know of with an AI coding partner built in. The default piece is *notepat*, an 8,466-line polyphonic instrument with eight waveforms, room reverb, sample recording, and USB + UDP MIDI; twenty other pieces ship alongside it.

**KidLisp** is a minimal Lisp dialect designed specifically for generative art. With 118 built-in functions across 12 categories, it provides an accessible entry point for non-programmers while remaining expressive enough for complex compositions. Over 16,000 KidLisp programs have been written on the platform. KidLisp programs can be minted as on-chain "keeps" on Tezos, establishing provenance without requiring artists to understand blockchain infrastructure.

**The Network** ties it together. Aesthetic Computer hosts 371 built-in pieces and 265 user-published pieces across 2,800+ registered handles. Every piece is URL-addressable and instantly shareable via QR code. The platform supports real-time multiplayer through WebSocket and UDP channels — people can draw, compose, and play together.

During the grant period, we propose to develop AC Native from a working prototype into a distributable creative instrument and public installation:

1. **Portable Instruments** — Produce USB-bootable AC Native drives preloaded with curated pieces that visitors and workshop participants can take home and boot on their own laptops.
2. **KidLisp Workshops** — Hands-on sessions where participants write KidLisp programs that run on AC Native hardware in real time, experiencing the full loop from code to sound and image with no intermediary.
3. **Public Installation** — Multiple AC Native stations at LACMA where visitors encounter creative computing as a direct, embodied experience — more like sitting down at a piano than opening an app.
4. **Open Documentation** — Publish the complete build pipeline, hardware compatibility guide, and workshop curriculum so other artists and institutions can replicate the system.

This project is not about building a product. It is about demonstrating that the personal computer can still be a site of artistic invention — that the instrument is not yet finished being designed.

---

## Artist Bio (short)

Jeffrey Alan Scudder (b. 1989, Assonet, MA) is an artist based in Los Angeles, working across stretched canvas, custom software, and live performance. Yale School of Art MFA (2013). He is the creator of Aesthetic Computer, Whistlegraph, and No Paint; his open-source tools *No Paint* (2020) and *notepat* (2024) each reached the front page of Hacker News. Work is held in the collections of KADIST (San Francisco) and SMK — National Gallery of Denmark. He is currently Author in Residence at UCLA, working with Casey Reas, and hosts biweekly NELA Computer Club demos at Plot.Place in Chinatown, Los Angeles.

## Artist CV

See the full CV appended to `lacma-2026.pdf` (pp. 4–5) and the canonical CV at `/papers/cv/cv.pdf`. Top-line credentials:

- **Education:** Yale MFA (2013), Ringling BFA (2011), AICAD NYSP Residency (2010)
- **Collections:** KADIST Foundation (San Francisco), SMK — National Gallery of Denmark
- **Current Residency:** Author in Residence, UCLA Social Software (Casey Reas), 2026
- **Teaching:** UCLA DMA (2016, 2024, 2026), Southern Oregon University (2019), Parsons (2013–2016)
- **Recent Exhibitions:** 47th Venice Family Clinic Art Exhibition (2026), Turbo Cheap inaugural (2025), Ten Whistlegraphs at Feral File (2022)
- **Selected Lectures:** New Museum NYC (2022), Korea HCI keynote (2020), India HCI keynote (2019), 35c3 Chaos Communication Congress (2018), bitforms gallery in conversation with Casey Reas (2018)
- **Press:** Rhizome Artist Profile (2017), Schlosspost RDP Manifesto (2017), Artsy on Microsoft Paint (2017), Dirt on Whistlegraph (2023), Hacker News front page ×2 (No Paint 2020, notepat 2024)
- **Software:** Aesthetic Computer (2021–), AC Native (2025–), KidLisp (2024–, 16,000+ programs), notepat (2024–, 8,466 lines), No Paint (2016–)

ORCID: 0009-0007-4460-4913

---

## Artistic Merit Statement (100 words max)

_[~95 words]_

Aesthetic Computer treats the computer itself as an unfinished instrument — a site for ongoing artistic invention rather than a fixed consumer product. By building from bare metal (custom kernel, framebuffer rendering, sample-level audio synthesis), we recover the directness that early personal computing promised but commercial platforms abandoned. The work sits at the intersection of software art, instrument design, and language design: KidLisp is simultaneously a tool and a medium, and AC Native transforms commodity laptops into dedicated creative instruments. The artistic claim is that how we build computers is itself a creative act with cultural consequences.

---

## Technology and Culture Dialogue Statement (100 words max)

_[~98 words]_

Consumer operating systems have become attention-extraction machines — optimized for engagement metrics, not creative agency. AC Native offers a counter-model: a computer that boots directly into a single piece of art software and does nothing else. This is not nostalgia for early computing but a forward-looking argument that the personal computer's design is a cultural question, not a settled technical one. KidLisp extends this argument to programming itself — demonstrating that a language can be designed for artistic expression rather than industrial production. The 16,000+ programs written in KidLisp suggest this resonates beyond our own practice.

---

## Public Engagement Plan (100 words max)

_[~95 words]_

We propose three forms of public engagement. First, hands-on KidLisp workshops at LACMA where participants write generative art programs that run on AC Native hardware — no prior coding experience required. Second, an installation of multiple AC Native stations where visitors experience creative computing as a direct, instrument-like interaction. Third, open "build days" where we assemble USB drives and document the process publicly, inviting visitors into the making of the system itself. All curricula, documentation, and software will be published openly for other artists and institutions to adopt.

---

## Implementation Plan

| Phase | Timeline | Milestones |
|-------|----------|------------|
| **1. Hardware Refinement** | Fall 2026 | Expand AC Native hardware compatibility; develop multi-piece boot menu; optimize for 5+ laptop models commonly available at institutions |
| **2. Workshop Development** | Winter 2026–27 | Design KidLisp workshop curriculum (3 difficulty levels); test with pilot group; produce printed reference cards |
| **3. Installation Design** | Spring 2027 | Design multi-station installation layout; produce 20+ bootable USB drives; develop kiosk-mode security for public use |
| **4. Public Programs** | Summer–Fall 2027 | Deliver 4–6 workshops at LACMA; install and maintain public AC Native stations; collect participant feedback |
| **5. Documentation & Release** | Winter 2027–28 | Publish complete build guide, workshop curriculum, and hardware compatibility list; present at LACMA Biennial Symposium (2027) |

---

## Budget (Itemized)

| Item | Cost |
|------|------|
| Artist fee (24 months) | $20,000 |
| Installation laptops (5 × $400 refurbished) | $2,000 |
| USB drives, cables, peripherals | $500 |
| Studio hardware (dev machines, displays) | $4,000 |
| NuPhy analog keyboards for installation (5 × $120) | $600 |
| Workshop materials (printed guides, reference cards) | $1,000 |
| Installation fabrication (furniture, mounts) | $2,500 |
| Server + compute infrastructure (hosting, CDN, CI/CD) | $3,500 |
| Platform development (contract engineering) | $3,000 |
| Documentation production (video, photography) | $2,000 |
| Contingency (10%) | $3,900 |
| **Total Requested** | **$43,000** |

$7,000 of headroom remains under the $50,000 cap.

---

## Other Funding Sources

- Open-source sponsorship via GitHub Sponsors and Liberapay (supplements ongoing infrastructure costs)
- Personal / studio funds (primary ongoing support)

---

## Images / Schematics (up to 5, JPEG)

Upload these 5 JPEGs from `grants/lacma-2026/jpegs/submit/`:

1. **platform-screenshot.jpg** — Aesthetic Computer running on mobile and desktop. Platform hosts 600+ interactive pieces across 2,800+ registered users.
2. **kidlisp-featured.jpg** — *$roz* by @jeffrey, a KidLisp generative piece with 6,000+ plays.
3. **card-gallery.jpg** — Four KidLisp pieces as printable cards ($berz, $24m, $duv, $kl1), produced for Casey Reas & Laurent Bourgault's "Social Software" course at UCLA (2026). Screenshot + source code + QR code per card, monochrome, 2.75″ × 4.75″.
4. **card-berz.jpg** — The $berz card at full size. Six lines of KidLisp produce a recursive wire-tangle that spins, zooms, and blurs each frame.
5. **hardware-yoga.jpg** — Target hardware: a Lenovo Yoga convertible running AC Native from USB.

---

## Optional Video (under 5 min, hyperlinked)

See `video-script.md` in this directory. Filming tomorrow.

_[TODO: record + upload unlisted to YouTube/Vimeo + paste URL here before submit]_

---

## Submission Checklist

- [x] Project name + three words + one-sentence description
- [x] Full project description (~510w — trim if form hard-caps at 500)
- [x] Artist bio
- [x] Three 100-word statements (merit / culture / engagement)
- [x] Implementation plan / timeline
- [x] Budget with milestones
- [x] Funding sources
- [x] PDF attached (lacma-2026.pdf)
- [x] 5 JPEGs prepared (in `jpegs/submit/`)
- [ ] Video recorded + uploaded + linked
- [ ] Submittable account created / logged in
- [ ] Form fields pasted + saved as draft
- [ ] Final review (word counts, typos)
- [ ] Submit before April 22, 2026, 11:59 PM PST
