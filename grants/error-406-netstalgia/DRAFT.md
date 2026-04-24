# Error 406 [Netstalgia] Not Acceptable — Application Draft

> **Deadline:** 2026-05-04, 23:59 CEST
> **Deliverable:** browser-runnable online exhibition piece + terms of use, due 2026-10-30
> **Ask:** **€5,500** (middle-upper of the €1.5–7k range — see budget)
> **Submit:** https://error417.expectation.fail/406/netstalgia-not-acceptable/open-call
>
> *This draft is amount-flexible and section-flexible until we have the
> actual form fields in hand. Word counts below are first-cut targets.*

---

## Project Title

**The URL Tradition** — a browser-runnable counter-platform piece

(Working title. Optional alternates: "Personal Computers Are Not Done Yet,"
"What An Address Was Supposed To Be," "Permanent Forks.")

## One-Sentence Description

A live web piece where every keystroke makes a new URL — a working
demonstration that the address bar can still be a creative interface, not
a tracking surface, written into Aesthetic Computer's existing public
hosting layer of 17,000+ user programs.

---

## Project Description (~400 words target)

The web's original promise was that every program, every painting, every
in-progress thought could have a permanent address that anyone could
visit, fork, and continue. Forty years of platform consolidation turned
the URL into a tracking primitive. **Netstalgia is the wrong response.**
Wishing for the early web — the personal homepage, the GeoCities ring —
fixes nothing, because the missing infrastructure was never the
aesthetic. It was the *contract*: every artifact addressable, every
artifact remixable, no platform between artist and audience.

**Aesthetic Computer** has been quietly rebuilding that contract since
2021. KidLisp.com hosts 17,000+ user-written generative-art programs,
each at a permanent URL of the form `kidlisp.com/$xxx`. Anyone can fork
any program by appending it to any other URL. There is no app store, no
moderation queue, no algorithm. The hosting layer is free and the source
is on GitHub.

**The proposed piece — *The URL Tradition* — makes that contract visible.**
A single browser-runnable artwork where:

1. **Every keystroke is publishable.** As the visitor writes a KidLisp
   program live in the page, every state of the program is auto-minted as
   a permanent URL. No save button. No login. The URL bar is the
   manuscript.
2. **The address bar is the studio.** The piece lets visitors visit any
   prior visitor's URL, fork it, mix it with any other prior URL by
   concatenation, and watch the result run live.
3. **Inherited memory is a visible substrate, not a frame.** The piece
   surfaces the URL graph itself — what fed into this state, what comes
   after — so that "remix" stops being a metaphor and becomes the
   mechanic.

The piece will run for the duration of the Error 406 online exhibition
(end of 2026 onward) at a dedicated URL, with all visitor-generated state
preserved as part of the work. After the exhibition, the piece continues
at the same URL — as a permanent counter-platform demonstration, not an
expiring artifact.

**Why this fits Netstalgia Not Acceptable.** This is not a recreation of
the early web. It's a working argument that the protocols, the
addressing scheme, and the hosting layer the web actually offered — if
you decline the platform — already deliver what we wish nostalgia could.
The piece proposes (and implements) the URL as the unit of artistic
authorship: not a citation, not a share, but the work itself.

---

## Artist Statement (~150 words target)

I make instruments and tools for other artists to use. Aesthetic Computer
is the central project — a creative computing system, a custom
programming language (KidLisp), a social network, and a lending fleet of
laptops flashed with a custom kernel. I have been arguing for years
that the personal computer is not finished, that the web's original
contract was real, and that the way to defend it is to keep building
it. The URL Tradition piece is the most concentrated expression of
that argument I can fit into a browser. It exists because every other
form of the argument — the operating system, the laptop fleet, the
hardware — is harder to ship to a global online exhibition than a
single URL. That, itself, is the point.

---

## Fit With the Call's Themes

| Call theme | How the piece engages it |
|---|---|
| **Protocols, networks, shared infrastructures** | The piece IS a working protocol stack — KidLisp + the AC hosting layer + permanent URLs. Source on GitHub. |
| **Governance, moderation, sustainability within decentralization** | No moderation queue; every program is its own permanent URL; hosting paid by open-source sponsorship + studio funds, not VC. |
| **Expanding user agency on restrictive platforms** | The piece is itself the alternative platform; visitors leave with permanent URLs they own. |
| **Migration toward existing alternatives** | Built atop infrastructure that already exists and works (17,000+ programs, 2,800+ handles, 5 years of uptime). |
| **Critical interrogation of inherited memories and power structures** | The piece argues against nostalgia explicitly — its companion essay (see deliverables) frames the URL Tradition as a contract, not a memory. |

---

## Deliverables

1. **The piece itself** — a browser-runnable URL (`error406.aesthetic.computer`
   or similar), live for the duration of the exhibition and after.
2. **Companion essay (~1,500 words)** — published as both a permanent URL
   and a printable single-sheet KidLisp card, expanding "The URL
   Tradition" argument for the exhibition catalog.
3. **Terms of use** — short, readable, in plain English. No tracking, no
   accounts, all visitor-generated content preserved at permanent URLs
   under a permissive license.
4. **Source code** — on GitHub (already public for the full AC stack).
5. **Documentation reel** — short video of the piece in use, for the
   exhibition front-matter.

---

## Risk and Process (Selection criterion #5)

This piece is **deliberately under-finished at the moment of submission**
and will be co-developed with the visitor traffic during the exhibition
window. The risk is that visitors don't make anything interesting; the
process commitment is that the piece will publish a periodic "URL of the
week" curated edit *during* the exhibition, with the artist
acknowledging that some weeks will be empty. Failure becomes part of the
work. This is consistent with the call's stated welcome of "high-risk,
process-oriented projects."

---

## Budget (€5,500 ask)

| Item | € |
|---|---|
| Artist time: development + curation through exhibition window (≈12 weeks) | 3,000 |
| Hosting + bandwidth + URL preservation infrastructure (Cloudflare, MongoDB, Digital Ocean Spaces) | 600 |
| Companion essay writing + design + print run (100 cards, distributed in EU + LA) | 500 |
| Documentation reel (production + editing) | 400 |
| Translations of the piece UI + essay into ES, DA, ZH | 600 |
| Contingency (8%) | 400 |
| **Total** | **5,500** |

---

## Short Bio (~100 words)

Jeffrey Alan Scudder is an artist, educator, and technologist based in
Los Angeles. He builds instruments and tools for other artists, and
keeps a live practice across performance painting, software writing, and
teaching. Yale MFA (2013), Ringling BFA (2011). Author in Residence at
UCLA Social Software with Casey Reas and Lauren Lee McCarthy. Creator of
Aesthetic Computer, Whistlegraph, and No Paint. His open-source tools
*No Paint* and *notepat* each reached the front page of Hacker News.
Work in the collections of KADIST (San Francisco) and SMK (Copenhagen).
Hosts biweekly NELA Computer Club demos at Plot.Place in Chinatown, LA.

## Long Bio / CV

See `papers/cv/cv.pdf` (canonical AC CV).

---

## Source Materials Repurposed Here

- `papers/arxiv-url-tradition/url-tradition.tex` — primary intellectual
  source for "The URL Tradition" framing
- `papers/arxiv-sustainability/sustainability.tex` — "Who Pays for
  Creative Tools?" — funding-model argument
- `papers/arxiv-network-audit/network-audit.tex` — current state of the
  17,000-program hosting layer
- `system/public/kidlisp.com/` — the existing hosting platform the
  piece extends
- `papers/arxiv-kidlisp-cards/` — card format for companion essay print

---

## Submission Checklist

- [ ] Open the actual application form, capture exact fields + word caps
- [ ] Trim each section to the form's actual word count
- [ ] Decide on final title (currently "The URL Tradition")
- [ ] Confirm €5,500 ask vs. higher (€7k) or lower (€4k) based on form
      questions about scope vs. honorarium
- [ ] Pull 3–5 work-sample images (existing KidLisp pieces as preview)
- [ ] Record + upload short documentation video
- [ ] Confirm exhibition URL slug (`error406.aesthetic.computer`?)
- [ ] Final review for word counts, typos, and ES/DA/ZH translation note
- [ ] Submit before 2026-05-04, 23:59 CEST
