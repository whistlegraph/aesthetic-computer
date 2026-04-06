# LACMA Art + Technology Lab 2026 — Application Draft

> **Deadline:** April 22, 2026, 11:59 PM PST
> **Submit at:** https://lacma.submittable.com/submit/348727/2026-art-technology-lab-grants
> **Questions:** lab@lacma.org

---

## Project Name

Aesthetic Computer: Bare Metal Instruments

## Three Descriptive Words

instrument, language, network

## One-Sentence Project Description

Aesthetic Computer is a bare-metal creative computing system — custom hardware, a handmade programming language, and a social network — that reimagines the personal computer as a live musical instrument for art.

---

## Full Project Description (500 words max)

_[~495 words]_

Aesthetic Computer (AC) is a creative computing platform built from first principles. It consists of three interlocking layers: a bare-metal operating system that boots directly into art software, a custom programming language called KidLisp for generative art, and a social network where anyone can publish and share interactive programs called "pieces."

The core provocation is simple: what happens when you strip away the consumer operating system — the notifications, the app stores, the surveillance — and build a computer that does nothing but help you make things?

**AC Native** is our answer in hardware. It is a Linux kernel that boots from a USB stick on any x86 laptop in under two seconds, running a custom C runtime as PID 1 — no desktop, no window manager, no browser. The system renders directly to the framebuffer, reads input from raw device events, and synthesizes audio sample-by-sample through ALSA at 192kHz. The result is a zero-latency creative instrument: a 7,800-line musical composition tool called *notepat* currently ships as the default piece, turning any laptop into a polyphonic synthesizer with room reverb, waveform selection, and time-of-day-responsive visuals.

**KidLisp** is a minimal Lisp dialect we designed specifically for generative art. With 118 built-in functions across 12 categories, it provides an accessible entry point for non-programmers while remaining expressive enough for complex compositions. Over 16,000 KidLisp programs have been written on the platform to date. KidLisp programs can be minted as on-chain "keeps" on Tezos, establishing provenance without requiring artists to understand blockchain infrastructure.

**The Network** ties it together. Aesthetic Computer hosts 359 built-in pieces and 265 user-published pieces across 2,800+ registered handles. Every piece is URL-addressable and instantly shareable via QR code. The platform supports real-time multiplayer through WebSocket and UDP channels — people can draw, compose, and play together.

During the grant period, we propose to develop AC Native from a working prototype into a distributable creative instrument and public installation. Specifically:

1. **Portable Instruments** — Produce a set of USB-bootable AC Native drives preloaded with curated pieces (music, drawing, generative art) that visitors and workshop participants can take home and boot on their own laptops.

2. **KidLisp Workshop Series** — Develop and deliver hands-on workshops where participants write KidLisp programs that run on AC Native hardware in real time, experiencing the full loop from code to sound and image with no intermediary.

3. **Public Installation** — Design an installation of multiple AC Native stations at LACMA where visitors encounter creative computing as a direct, embodied experience — more like sitting down at a piano than opening an app.

4. **Open Documentation** — Publish the complete build pipeline, hardware compatibility guide, and workshop curriculum so other artists and institutions can replicate and extend the system.

This project is not about building a product. It is about demonstrating that the personal computer can still be a site of artistic invention — that the instrument is not yet finished being designed.

---

## Artist / Collective Bio

_[Update with Jeffrey's full bio/CV — below is a placeholder draft]_

Jeffrey Scudder is an artist and software developer whose practice centers on creative computing, interface design, and digital culture. He is the creator of Aesthetic Computer, an open-source platform for interactive art, and its predecessor No Paint (Hacker News front page, 2020). His work explores the computer as an expressive instrument rather than a productivity tool.

Scudder has exhibited and presented work internationally. His musical instrument *notepat* reached the front page of Hacker News in 2024. He maintains an active creative computing community of 2,800+ users and has developed KidLisp, a custom programming language for generative art. His work is published under an open-source license and is migrating to decentralized infrastructure via AT Protocol.

ORCID: 0009-0007-4460-4913

_[TODO: Add exhibition history, education, institutional affiliations, press]_

---

## Artistic Merit Statement (100 words max)

_[~95 words]_

Aesthetic Computer treats the computer itself as an unfinished instrument — a site for ongoing artistic invention rather than a fixed consumer product. By building from bare metal (custom kernel, framebuffer rendering, sample-level audio synthesis), we recover the directness that early personal computing promised but commercial platforms abandoned. The work exists at the intersection of software art, instrument design, and language design: KidLisp is simultaneously a tool and a medium, and AC Native transforms commodity laptops into dedicated creative instruments. The artistic claim is that how we build computers is itself a creative act with cultural consequences.

---

## Technology and Culture Dialogue Statement (100 words max)

_[~98 words]_

Consumer operating systems have become attention-extraction machines — optimized for engagement metrics, not creative agency. AC Native offers a counter-model: a computer that boots in two seconds, runs one piece of art software, and does nothing else. This is not nostalgia for early computing but a forward-looking argument that the personal computer's design is a cultural question, not a settled technical one. KidLisp extends this argument to programming itself — demonstrating that a language can be designed for artistic expression rather than industrial production. The 16,000+ programs written in KidLisp suggest this resonates beyond our own practice.

---

## Public Engagement Plan (100 words max)

_[~97 words]_

We propose three forms of public engagement during the development period. First, a series of hands-on KidLisp workshops at LACMA where participants write generative art programs that run on AC Native hardware — no prior coding experience required. Second, an installation of multiple AC Native stations where visitors experience creative computing as a direct, instrument-like interaction. Third, open "build days" where we assemble USB drives and document the process publicly, inviting visitors into the making of the system itself. All workshop curricula, build documentation, and software will be published openly for other artists and institutions to adopt.

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
| Hardware — laptops for installation (5 × $400 refurbished) | $2,000 |
| Hardware — USB drives, cables, peripherals | $500 |
| Workshop materials (printed guides, reference cards) | $1,000 |
| Travel to LA (4 trips × $1,200) | $4,800 |
| Accommodation during LA visits (30 nights × $150) | $4,500 |
| NuPhy analog keyboards for installation (5 × $120) | $600 |
| Fabrication — installation furniture/mounts | $2,500 |
| Software infrastructure (hosting, CDN, domain) | $1,200 |
| Documentation production (video, photography) | $2,000 |
| Contingency (10%) | $3,900 |
| **Total** | **$43,000** |

_[Adjust amounts based on actual needs — this leaves $7,000 headroom under the $50,000 cap]_

---

## Other Funding Sources

_[List any other grants, sponsorships, or institutional support — or note "None" if this is the sole funding source]_

- Open-source sponsorship via GitHub Sponsors and Liberapay (covers ongoing server costs)
- _[Add others if applicable]_

---

## Images / Schematics (up to 5, JPEG)

_[TODO: Prepare and attach]_

1. **AC Native booting on a laptop** — photo of bare-metal boot sequence, USB stick visible
2. **notepat running** — screenshot of the musical instrument with key labels, status bar, time-of-day tint
3. **KidLisp generative art** — grid of KidLisp program outputs showing range of visual expression
4. **Aesthetic Computer web platform** — screenshot showing piece navigation, prompt interface, social features
5. **Installation concept sketch** — diagram of proposed multi-station LACMA installation layout

---

## Optional Video (under 5 min, hyperlinked)

_[TODO: Record a short demo showing AC Native booting from USB, playing notepat, and writing a KidLisp program. Upload and link here.]_

---

## Notes for Jeffrey

- **Deadline is April 22** — 16 days from now
- Bio section needs your real CV/exhibition history
- Budget is a starting draft — adjust the artist fee and travel based on your actual needs
- The 5 images are critical — strong visuals of AC Native and notepat will carry the application
- A short video demo would be very compelling given the "instrument" framing
- Anthropic is listed as a LACMA partner — worth noting your use of Claude Code in the development process if you think that strengthens the connection
- The "safe-to-fail" framing is a gift — lean into the experimental, prototype nature of the work
