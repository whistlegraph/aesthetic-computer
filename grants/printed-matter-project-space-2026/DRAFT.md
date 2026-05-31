# NY Art Book Fair — Project Space — Application Draft

> **Deadline:** 2026-05-08
> **Stipend:** $3,500 (per JS — confirm in form)
> **Submit:** Project Space proposal Google Form (link in NYABF FAQ)
>
> *This draft assumes the Project Space format gives 1 booth-or-installation
> footprint + a presentation slot during the fair. Trim once the actual
> form is open.*

---

## Project Title

**The AC Papermill, Live**

(Working title; alternates: "Programs As Cards," "The URL As Edition,"
"Aesthetic Computer Press.")

---

## One-Sentence Description

A live publishing booth at the NY Art Book Fair where visitors write
KidLisp programs that print themselves as single-sheet cards in real
time, demonstrating the live tie between code, web hosting, and the
printed page that sits at the center of the Aesthetic Computer
publishing practice.

---

## What The Project Space Will Be (~200 words)

A small footprint inside the fair: one table, one wall, one networked
printer, two laptops. The setup runs continuously through the fair
hours.

- **Laptop A** boots into Aesthetic Computer's KidLisp environment.
  Visitors are invited to write a short program at the keyboard. Every
  state of every program gets a permanent URL. (KidLisp.com hosts
  17,000+ existing user-written programs at this address scheme.)
- **Laptop B** is a print-on-demand station. As soon as a visitor
  finishes a piece they like, it's rendered as a single-sheet
  index-card composition (screenshot + source + permanent QR-coded URL,
  monochrome, 2.75″ × 4.75″) and printed on the spot. The visitor
  takes their card.
- **Wall display** of the existing AC papers: the AC Reader, twelve
  arxiv-format papers, the dis-order pamphlet, a stack of pre-printed
  KidLisp reference cards from the UCLA Social Software course.
  Everything free to take, restocked from a small inventory.
- **Periodic "press runs"** — at scheduled times each day, the
  artist will compile that day's KidLisp programs into a small
  newsprint zine (16 pp.), printed in a short edition (50–100), free
  to take.

The booth runs **continuously and unattended-friendly**. The artist
will be present for the press-run moments and for visitor
conversations; otherwise visitors operate the publishing themselves.

---

## Why It Belongs At NYABF

The fair celebrates the printed thing. The Project Space format exists
for presentations that frame a publication or printed-matter practice
rather than just selling it from a table. Aesthetic Computer's
publishing practice is unusual: the printed cards and the live software
are the same artifact, the URL is the citation, the QR code is the
delivery. The booth makes that visible by *running* the press at the
fair.

The piece also extends a five-year publishing arc that has produced:

- **The AC Reader** (`papers/reader.pdf`) — a single-volume bound
  collection of all current AC papers
- **The arxiv series** — 28 individual papers across `papers/arxiv-*/`
- **The cards** — single-sheet KidLisp pieces, designed to fit the
  hand and the wall (`papers/arxiv-kidlisp-cards/`)
- **The dis-order pamphlet** — recent printed pamphlet, 8.5×11 bifold
  (`gigs/dis-order-pamphlet-8.5x11-folded/`)

The booth is the live edge of that practice — the part where the
publishing happens in front of the reader.

---

## What Visitors Take Home

- Their own KidLisp card (printed during their visit)
- A permanent URL that runs their program forever
- An AC Reader copy (one per visitor while supply lasts)
- A dis-order pamphlet
- Optional: stack of curated KidLisp reference cards (from the UCLA
  Social Software course set)

---

## Budget Sketch (against the $3,500 stipend)

| Item | $ |
|---|---|
| Travel (LA ↔ NYC, four days) | 700 |
| Lodging (four nights, modest) | 600 |
| Equipment shipping (laptops + printer + cards) | 350 |
| Print supplies (cardstock, newsprint zine paper, ink) for daily press runs across the fair | 800 |
| Pre-printed inventory (AC Readers, dis-order pamphlets, KidLisp reference cards) | 600 |
| Per diem / meals (four days) | 250 |
| Contingency (≈6%) | 200 |
| **Total** | **3,500** |

If the stipend is confirmed at a different amount, the inventory and
press-run line items scale up or down first.

---

## Logistics

| Need | Provided by |
|---|---|
| Booth footprint inside the fair | Printed Matter |
| Networked thermal or laser printer | Artist (ships from LA studio) |
| Two laptops + USB drives + cables | Artist |
| Wall display strips / clips for cards | Artist |
| Pre-printed inventory for the wall + giveaway | Artist |
| Power + wifi at the booth | Printed Matter |
| Optional: small audio for the periodic press-run announcements | Artist |

**Staffing:** Artist runs the booth solo for the duration of the fair.
If selected and budget permits, may bring one helper for the busiest
two days.

---

## Identity / Imprint

**Apply as:** "Aesthetic Computer (Jeffrey Alan Scudder)"

The booth functions as an imprint, not a solo-artist showcase. The
imprint identity makes the social network and the open-source layer
legible to the fair audience.

(This is one of the open questions — discuss with Fia / collaborators
before submitting.)

---

## Short Bio (~100 words)

Jeffrey Alan Scudder is an artist, educator, and technologist based in
Los Angeles. He builds instruments and tools for other artists, and
keeps a live practice across performance painting, software writing,
and teaching. Yale MFA (2013), Ringling BFA (2011). Author in
Residence at UCLA Social Software with Casey Reas and Lauren Lee
McCarthy. Creator of Aesthetic Computer, Whistlegraph, and No Paint.
His open-source tools *No Paint* and *notepat* each reached the front
page of Hacker News. Work in the collections of KADIST (San Francisco)
and SMK (Copenhagen). Hosts biweekly NELA Computer Club demos at
Plot.Place, Chinatown LA.

---

## Media Samples

1. Photo of the existing AC Reader (PDF cover)
2. Photo of the existing KidLisp cards in use at UCLA Social Software
3. Photo of the dis-order pamphlet
4. Short video of a visitor writing a KidLisp program → seeing it
   become a permanent URL → printing the card
5. Inventory shot of the four publication formats side-by-side

---

## Source Materials Repurposed Here

- `papers/SCORE.md` — the papermill-as-mission frame
- `papers/cards-convert.mjs` — the actual code that turns arxiv
  source into single-sheet card layout
- `papers/arxiv-kidlisp-cards/` — example cards
- `papers/reader.pdf`, `papers/reader.tex` — the AC Reader
- `gigs/dis-order-pamphlet-8.5x11-folded/` — recent printed pamphlet
- `system/public/kidlisp.com/` — the hosting layer that already runs
  the URL side of this

---

## Submission Checklist

- [ ] Open the Google Form, capture exact fields + word caps + stipend
      amount
- [ ] Decide imprint vs. solo-artist application identity
- [ ] Source a thermal or laser printer suitable for booth use
- [ ] Pre-print enough Reader / pamphlet / card inventory before fair
- [ ] Confirm fair travel dates against other LA / NYC commitments
      (SFPC visit, Oracle Egg residency window if accepted)
- [ ] Pull the visitor-flow video
- [ ] Submit before 2026-05-08
