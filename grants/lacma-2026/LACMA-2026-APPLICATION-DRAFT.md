# LACMA Art + Technology Lab 2026 · Application (Submittable copy-paste source)

> **Deadline:** April 22, 2026, 11:59 PM PST
> **Submit at:** https://lacma.submittable.com/submit/348727/2026-art-technology-lab-grants
> **Questions:** lab@lacma.org
>
> This markdown file is the canonical source for form fields. The authoritative narrative PDF is `lacma-2026.pdf` (compiled from `lacma-2026.tex`). Keep them in sync.

---

## Project Name

Aesthetic.Computer: Personal Computers Are Not Done Yet

## Three Descriptive Words

instrument, library, network

## One-Sentence Project Description

Aesthetic.Computer is a creative computing system and public device library (custom hardware, a handmade programming language, a social network, and a lending fleet of laptops for flashing custom creative OSes) that reimagines the personal computer as a live musical instrument for art.

---

## Full Project Description (500 words max)

Personal computers have not been very personal. For forty years the form has been shaped by the companies that sold them: operating systems built to sell attention, software gatekept by app stores. The 1980s personal computing scene promised a computer that belonged to you, that you could program, that could do anything. Platform consolidation cut it short. Aesthetic.Computer bets a second personal computing scene is starting, and with tools this powerful in everyone's hands it will go wider *and* deeper than the first. Wider because anyone can publish. Deeper because anyone can write a language, modify a kernel, or put an AI coding partner to work on a single piece.

Aesthetic.Computer has three interlocking layers (an operating system, a custom programming language, and a social network) and a public face: **the Library**. The whole stack is free and open source on GitHub.

**There has never been a better time to develop new software for old hardware.** Windows 10 end-of-life has stranded roughly 240 million x86_64 laptops; 62 million tonnes of e-waste pile up each year. Strip away the consumer OS and those machines become a planetary population of half-built instruments waiting for a kernel.

**The operating system** boots a laptop directly into Aesthetic.Computer. A Linux boot runs a custom C runtime as PID 1 on x86_64 UEFI laptops, with no desktop, compositor, or browser. Graphics via DRM, input via raw evdev, audio via ALSA at 192 kHz, 32-voice polyphony. Per-seat cost lands near $50, two orders of magnitude below Princeton's PLOrk laptop-orchestra model. A built-in `code` command drops into a terminal running Anthropic's Claude Code. The default piece is *notepat*, an 8,466-line polyphonic instrument.

**KidLisp** is a minimal Lisp for generative art. 118 functions, 17,000+ programs already on the platform. Programs can be minted on Tezos without artists touching blockchain infrastructure.

**The Network** hosts 371 built-in pieces, 265 user-published, 2,800+ registered handles. The website has been in active development since 2021 (19,000+ commits across 5 years); running as the whole operating system on a laptop is new, with the kernel prototype landing February 2026.

**The Library** is what makes the stack civic. A lending fleet of AC Blank laptops (refurbished ThinkPad 11e Yoga Gen 6, flashed with Aesthetic.Computer at $128/seat) circulates through Flash Days, workshops, and Family Play afternoons at LACMA. Members join a public waitlist. Aesthetic.Computer is the flagship; the library openly welcomes artists flashing their own custom creative operating systems onto library hardware, joining a tradition of artist-run device libraries.

During the grant period we grow the library and run public events:

1. **Flash Days.** Cohort artists and library members flash AC Blanks together and take them home.
2. **KidLisp Workshops.** Hands-on sessions writing programs that run on Aesthetic.Computer in real time.
3. **Family Play.** Drop-in kiosk hours where visitors of any age play the library's fleet together.
4. **Open Documentation.** Library curriculum, waitlist software, and build pipeline, published openly.

The personal computer is a civic instrument, and a new scene is just beginning.

---

## Artist Bio (short)

Jeffrey Alan Scudder is an artist, educator, and technologist based in Los Angeles. He loves building instruments and tools for other artists to use, and keeps a live and active practice across performance painting, software writing, and teaching. He holds an MFA from the Yale School of Art (2013) and a BFA from Ringling College of Art and Design (2011). He has taught at UCLA, Parsons School of Design, and Southern Oregon University, and is currently Author in Residence at UCLA Social Software with Casey Reas and Lauren Lee McCarthy. He is the creator of Aesthetic.Computer, Whistlegraph, and No Paint, and his open-source tools *No Paint* and *notepat* each reached the front page of Hacker News. His work is held in the collections of KADIST (San Francisco) and SMK, the National Gallery of Denmark. He hosts biweekly NELA Computer Club demos at Plot.Place in Chinatown, Los Angeles.

## Artist CV

See the full CV appended to `lacma-2026.pdf` (pp. 4–5) and the canonical CV at `/papers/cv/cv.pdf`. Top-line credentials:

- **Education:** Yale MFA (2013), Ringling BFA (2011), AICAD NYSP Residency (2010)
- **Collections:** KADIST Foundation (San Francisco), SMK National Gallery of Denmark
- **Current Residency:** Author in Residence, UCLA Social Software (Casey Reas), 2026
- **Teaching:** UCLA DMA (2016, 2024, 2026), Southern Oregon University (2019), Parsons (2013–2016)
- **Recent Exhibitions:** 47th Venice Family Clinic Art Exhibition (2026), Turbo Cheap inaugural (2025), Ten Whistlegraphs at Feral File (2022)
- **Selected Lectures:** New Museum NYC (2022), Korea HCI keynote (2020), India HCI keynote (2019), 35c3 Chaos Communication Congress (2018), bitforms gallery in conversation with Casey Reas (2018)
- **Press:** Rhizome Artist Profile (2017), Schlosspost RDP Manifesto (2017), Artsy on Microsoft Paint (2017), Dirt on Whistlegraph (2023), Hacker News front page ×2 (No Paint 2020, notepat 2024)
- **Software:** Aesthetic.Computer (2021–), Aesthetic.Computer (2025–), KidLisp (2024–, 16,000+ programs), notepat (2024–, 8,466 lines), No Paint (2016–)

ORCID: 0009-0007-4460-4913

---

## Artistic Merit Statement (100 words max)

_[~95 words]_

Aesthetic.Computer treats the computer itself as an unfinished instrument, a site for ongoing artistic invention rather than a fixed consumer product. By building from the whole operating system (custom kernel, framebuffer rendering, sample-level audio synthesis), we recover the directness that early personal computing promised but commercial platforms abandoned. The work sits at the intersection of software art, instrument design, and language design: KidLisp is simultaneously a tool and a medium, and Aesthetic.Computer transforms commodity laptops into dedicated creative instruments. The artistic claim is that how we build computers is itself a creative act with cultural consequences.

---

## Technology and Culture Dialogue Statement (100 words max)

_[~98 words]_

Consumer operating systems have become attention-extraction machines, optimized for engagement metrics rather than creative agency. Aesthetic.Computer offers a counter-model: a computer that boots directly into art software and grows alongside the artist. Over five years of active development, Aesthetic.Computer's community has written 17,000+ KidLisp programs, made 4,400+ paintings, shared 2,900+ moods, and exchanged 19,000+ chat messages across 2,800+ registered handles. Aesthetic.Computer carries that social layer with it: chat, multiplayer, notepat, KidLisp runtime. The personal computer's design is a cultural question, not a settled technical one, and people show up when given the room.

---

## Public Engagement Plan (100 words max)

_[~95 words]_

Public engagement happens through the AC Device Library. First, Flash Days where cohort artists and library members flash AC Blank laptops together and take them home. Second, KidLisp workshops where participants write generative art programs that run live on Aesthetic.Computer. Third, Family Play afternoons where visitors of any age borrow and play the library's fleet together. A public waitlist keeps the fleet moving. The entire software stack is free and open source on GitHub, and the library openly supports artists flashing their own custom creative OSes onto library hardware.

---

## Implementation Plan

This proposal aligns to the Lab's new biennial calendar: a working-prototype milestone at the **2027 Symposium** and a completed public premiere at the **2028 Demo Day**. The cohort structure (3–5 recipients plus invitational projects) is well-suited to this work, since Aesthetic.Computer's USB-bootable format means other cohort artists can bring their own pieces to the system and cross-pollinate at the Symposium without waiting for Demo Day. Two recent Lab projects sit directly in our neighborhood: Casey Reas's 2023 *METAVASARELY and An Empty Room* (generative systems) and Lauren Lee McCarthy's 2022 *Auto* (public/social interface design). Reas and McCarthy co-teach UCLA's Social Software course, which Jeffrey is an Author in Residence in during this application year; their class is where the KidLisp cards shown in Fig. 3 were first printed and circulated.

| Phase | Timing | Milestones |
|-------|--------|------------|
| **1. Library Build-Out + Curriculum** | Fall 2026 – Spring 2027 | Establish the AC Device Library (12 AC Blank laptops, lending-fleet infrastructure, public waitlist software), expand Aesthetic.Computer compatibility to 5+ laptop models, design the 3-level KidLisp workshop curriculum, and produce printed reference cards. |
| **2. First Library Cohort + Pilots** | Spring – Summer 2027 | Open the library waitlist; run 2 pilot Flash Days at LACMA; first public borrowing cycle; assemble a prototype multi-station installation; publish v0 of the open-source build guide. |
| **3. 2027 Symposium · "We boot the cohort."** | Fall 2027 | **At Symposium, every laptop in the room becomes an Aesthetic.Computer instrument from a single USB stick. Cohort artists and library members flash AC Blanks together and take them home.** Public KidLisp workshop. Talk / in-conversation on generative computing alongside the 2023 cohort. |
| **4. Library Scaling + Extended Workshops** | Winter 2027 – Summer 2028 | Kiosk-mode hardening, library curriculum v1 (EN + ES), 4 additional workshops, returns-and-repair cycle for the fleet, complete documentation of the waitlist and lending system. |
| **5. 2028 Demo Day · "We play the room."** | Fall 2028 | **A multi-station Aesthetic.Computer installation premieres on the LACMA floor. Family Play afternoons open the library fleet to visitors of any age. v1.0 of the open-source build pipeline ships alongside so any institution can run its own room.** |

---

## Budget (Itemized)

| Item | Cost |
|------|------|
| Artist fee (24 months, Fall 2026 – Fall 2028) | $22,000 |
| Studio hardware (dev machines, displays) | $3,500 |
| AC Device Library fleet (12 AC Blank laptops × $128 flashed with Aesthetic.Computer, incl. cases + shipping + returns) | $2,000 |
| Mini Rig portable speakers for installation (5 × $120) | $600 |
| USB drives, cables, peripherals | $500 |
| Installation fabrication (furniture, mounts) | $2,500 |
| Workshop materials (printed guides, KidLisp reference cards) | $1,200 |
| **2027 Symposium "boot the cohort"** (cohort demo USB kit, on-site workshop station, travel) | $2,500 |
| **2028 Demo Day "play the room"** (multi-station install setup, public-program support, v1.0 release) | $3,000 |
| **Paid event helpers** (workshop TAs, install + take-down at Symposium and Demo Day) | $3,000 |
| Server + compute infrastructure (hosting, CDN, CI/CD) | $3,500 |
| Documentation production (video, photography, translation) | $2,000 |
| Contingency (10%) | $3,700 |
| **Total Requested** | **$50,000** |

The 10% contingency line handles budget slip; paid helpers are a direct expense budgeted for the cohort's two public events.

---

## Other Funding Sources

- Open-source sponsorship via GitHub Sponsors and Liberapay (supplements ongoing infrastructure costs)
- Personal / studio funds (primary ongoing support)

---

## Images / Schematics (up to 5, JPEG)

Upload these 5 JPEGs from `grants/lacma-2026/jpegs/submit/`:

1. **platform-screenshot.jpg** · Aesthetic.Computer running on mobile and desktop. Platform hosts 600+ interactive pieces across 2,800+ registered users.
2. **kidlisp-featured.jpg** · *$roz* by @jeffrey, a KidLisp generative piece with 6,000+ plays.
3. **card-gallery.jpg** · Four KidLisp pieces as printable cards ($berz, $24m, $duv, $kl1), produced for Casey Reas & Lauren Lee McCarthy's "Social Software" course at UCLA (2026). Screenshot + source code + QR code per card, monochrome, 2.75″ × 4.75″.
4. **card-berz.jpg** · The $berz card at full size. Six lines of KidLisp produce a recursive wire-tangle that spins, zooms, and blurs each frame.
5. **hardware-yoga.jpg** · Target hardware: a Lenovo Yoga convertible running Aesthetic.Computer from USB.

---

## Optional Video (under 5 min, hyperlinked)

See `video-script.md` in this directory. Filming tomorrow.

_[TODO: record + upload unlisted to YouTube/Vimeo + paste URL here before submit]_

---

## Submission Checklist

- [x] Project name + three words + one-sentence description
- [x] Full project description (500w · at cap)
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
