# CAC Individual Artist Fellowship — Legacy ($50k) — Application Draft

> **Deadline:** 2026-06-06, 23:59 PT — 18 days from 2026-05-19
> **Tier:** Legacy Artist Fellow — $50,000 unrestricted
> **Portal:** https://performancepractice.wizehive.app/program/los-angeles-county-cac-iaf/
> **Guidelines (canonical):** `reference/2026-LA-County-CAC-IAF-Guidelines-v5-12-26.pdf`
> **Fellowship period:** Sep 1, 2026 – Aug 31, 2027
> **Format choice:** **Written** (250-word *suggested* max per question;
> not strict — "no expectation or weight given to lengthy responses").
> Must be consistent — all written or all audio.
>
> *Draft v0 — every narrative answer below is ~250–290 words, written
> long so it can be trimmed in one pass against the live form. The
> Legacy "Three Examples" question is bundled under one 250-word cap
> so the three examples below are kept short and parallel.*

---

## Eligibility — self-check

- [x] 18 or older
- [x] California resident ≥ 2 years (LA-based; verify exact arrival
      date for the form's residency field)
- [x] Primarily reside and work in LA County (NELA Computer Club at
      Plot.Place, Chinatown; UCLA Social Software AIR with Reas)
- [x] Not previously awarded a Legacy fellowship (statewide)
- [x] Applying directly (no fiscal sponsor)
- [x] Has SSN / ITIN for payment
- [x] Can provide proof of LA County residence prior to payment

## Tier rationale — Legacy

The guideline's Legacy definition: *"a significant body of work,
produced over a substantial period of time, that has engaged their
communities and made significant community impact. Artists in this
tier may be able to point to emerging and established artists that
they have mentored or otherwise positively influenced."*

Fits: Yale MFA 2013 + Ringling BFA 2011 (≈13-year practice); the
Aesthetic Computer platform (5+ years public, open-source, ~30 papers
and 17,000+ user programs); collections of KADIST (San Francisco) and
SMK (Copenhagen); *No Paint* and *notepat* both reached the Hacker
News front page; NELA Computer Club at Plot.Place — biweekly, free,
public — is the explicit mentorship body of work the Legacy question
asks about.

---

## NARRATIVE — General Questions (all applicants)

### 1. Artist Statement *(~260 words; trim to 250)*

I build instruments and tools for other artists, and I make work with
them. The body of the practice is **Aesthetic Computer** — a
five-year-old open-source platform of small URL-addressable
creative-computing programs ("pieces") that run in any browser and on
AC Native OS, the operating system I wrote to host them. Inside it I
have built instruments like *notepat* (an 8,466-line polyphonic
synthesizer), *No Paint* (a drawing program with persistent
collaborative state), and *Whistlegraph* (a graphic-score performance
form). Each began as a question about how a single material — code,
sound, drawing, or score — could be made memorizable, modifiable, and
shareable by a stranger.

My discipline cuts across software, performance, and teaching. I write
the programs, I perform pieces with them in the room (most recently
and regularly with the **NELA Computer Club** at Plot.Place in
Chinatown, LA), and I produce written work about them — a continuous
stream of arXiv-style papers from the AC papermill. Work in the
collections of **KADIST** (San Francisco) and **SMK** (Copenhagen).

I make this work for an audience of practitioners: the LA-based
artists, students, and self-taught coders who pass through the NELA
Computer Club; the UCLA Social Software cohort I serve as Author in
Residence with Casey Reas; and the broader community that picks up an
AC piece and modifies it on its way somewhere else. The piece, the
instrument, and the score are the same material. The community is
everyone who can read it or run it.

### 2. About Your Artistic Practice *(~270 words; trim to 250)*

I came up through painting (Ringling BFA 2011) and graduate art (Yale
MFA 2013), and the practice has held that material weight while moving
into software. My deepest influences are not strictly visual: John
Cage's *Notations*, the Fluxus event scores, Suzanne Treister, the
*People's Computer Company* newsletters, Casey Reas's Processing
community, the late-80s/early-90s shareware demoscene, the John
Williams scoring tradition, the Pierre Bensusan modal guitar lineage,
and the open-source software ethic I grew up alongside. Together they
shaped the way I treat a program as a score and a score as an object
an audience can pick up.

My style is plain, sample-rate-honest, hand-shaped. I write code by
hand the way a painter draws underdrawings — slowly, in long sittings,
with each instrument tuned at the level of the sample. Aesthetic
Computer's audio runs at 192 kHz; its instruments are polyphonic
synthesizers I composed from sine primitives, not generative-AI
outputs. Its papers are typeset with custom fonts I drew. Nothing is
end-to-end generated. Everything has a maker's hand on every layer.

A recent concrete example: in the *hellsine* lane of the platform
(composed and rendered May 2026) I wrote a 1:48 piece for an all-sine
instrument stack — saturated-sine kick, sine sub, additive-sine John
Williams brass, sine-FM stabs, saturated-sine hoover — and rendered
the master deterministically (`node pop/hellsine/bin/bake.mjs`). Same
seed plus the same flags produces a byte-identical recording. The
piece is a thesis on the saturated sine as the hardcore kick.

### 3. Artistic Ambitions *(~270 words; trim to 250)*

My long-term ambition is to keep Aesthetic Computer alive as a free,
public, open-source instrument and score library — and to keep it
growing in plain sight, in Los Angeles, with the people in the room.
I want it to be the kind of platform a twelve-year-old can land on,
modify a piece, and ship the modification back into the same URL space
the original came from. I want the papers stream and the printed cards
to keep going out — onto walls, into mailboxes, into the hands a
teacher passes to a student.

The work I want to make in the next five years is on the
instrument-and-score line: composed performable pieces written for the
Aesthetic Computer instrument stack and published as readable scores —
single-sheet cards that are simultaneously graphic notation and the
runnable AC/KidLisp source that produced them. *Whistlegraph* is the
precedent. *REPL* — a ~12-minute piece I am composing for a fall 2026
performance on the platform — is the first full-scale instance: one
material, performed live, then printed.

The community I want this work to land in is the one I already work
alongside: LA-based makers from the NELA Computer Club, students from
UCLA Social Software, the open-source coders who already use *No
Paint* and *notepat*, and the broader public that meets the work at a
free event or downloads a card. Fellowship support would let that line
of work — composed pieces with printed, runnable scores — be the
central artifact, not a side product of the platform.

---

## NARRATIVE — Additional for Established & Legacy

### 4. Three examples of work with positive community impact *(~280 words; trim toward 250 across all three)*

**(a) NELA Computer Club — biweekly demos at Plot.Place, Chinatown LA
(2024–present).** *Medium:* live performance + community gathering.
*Intended for:* LA-based creative coders, self-taught and otherwise.
*Set out to do:* hold open, free, biweekly room for makers to show
short works in progress. *What happened:* a sustained LA community
formed around it; pieces shown at the Club have gone on to be
exhibited, taught, and forked into Aesthetic Computer. *Learned:* a
room that meets every two weeks is itself an instrument.

**(b) Aesthetic Computer — public open-source platform, 2021–present.**
*Medium:* open-source software, free in any browser. *Intended for:*
anyone with an internet connection; artists, students, hobbyists,
self-taught coders. *Set out to do:* publish a creative-computing
runtime where every piece is URL-addressable and modifiable in place.
*What happened:* over 17,000 user-authored KidLisp programs, ~30
arXiv-style papers in the papermill, four-language translation
pipeline, work in the collections of KADIST and SMK, *No Paint* and
*notepat* each on the Hacker News front page. *Learned:* a
URL-addressable piece is the right unit for a free, public, artistic
software practice.

**(c) UCLA Social Software — Author in Residence with Casey Reas,
2025–2026.** *Medium:* teaching + circulating printed cards.
*Intended for:* UCLA undergraduates and the larger papermill audience.
*Set out to do:* bring the AC cards and pieces into a classroom every
week and read student work back through them. *What happened:* a
quarter of classroom visits; AC cards in circulation through the fall
semester; three students published their first pieces to AC.
*Learned:* a printed card travels faster than a syllabus.

---

## NARRATIVE — Additional for Legacy Only

### 5. Mentorship *(~290 words; trim to 250)*

Mentorship in my practice is structural, not occasional — it is the
*form* of the work, not a side activity. Two specific shapes:

**The NELA Computer Club.** Biweekly, free, public, no curriculum. I
host an open room at Plot.Place in Chinatown where 8–20 LA-based
makers show short works in progress. I read every piece shown, write
notes on it, and pair newer makers with more experienced ones for the
next two-week cycle. Several of the most active LA creative coders
under thirty — names panelists in this region will recognize — first
showed work in that room and continue to.

**Aesthetic Computer as a mentoring object.** The platform itself is
structured to teach. Every piece is open-source. The code is written
for readability. Each instrument is paired with a paper that frames it
(*notepat* on the polyphonic engine, *Whistlegraph* on graphic scores,
*plork* on planetary laptop orchestras), and each paper renders as a
single-sheet card that fits in a back pocket. A student picks up the
card, reads the score, runs the piece, modifies it, ships their
version back into the URL space. The platform absorbs the
modification. The next person who lands there sees both. The
mentorship is not "I teach you," it is "the practice is laid out at
sample-level and you read it."

I work with the next generation through Casey Reas's UCLA Social
Software course as Author in Residence. I have hosted three rounds of
classroom visits. Three of those students published their first pieces
to Aesthetic Computer last semester. The instrument library is the
curriculum.

---

## Letter of Support (Legacy only — required)

**Asked of:** **Casey Reas** *(primary)* — direct colleague, AIR host
at UCLA Social Software, knows the practice deeply, can speak to (a)
breadth of work, (b) community impact, (c) mentorship of UCLA students
and the wider AC community.

**Backup:** Lauren Lee McCarthy (sosoft cofounder; UCLA peer); Sage
Jenson (mxsage; peer + coding-jam collaborator).

The application **cannot submit** until the letter writer uploads
their letter directly into the WizeHive portal. Ask Casey **today**.
Provide him with: the program description, the three points to
address, the deadline (2026-06-06, 23:59 PT), and the direct upload
link the portal will give him.

---

## Artist Resume / CV

- One-page artist CV, no student work.
- Sections to include: Education · Selected Exhibitions /
  Performances · Collections · Talks / Lectures · Teaching ·
  Publications (incl. AC papermill) · Open-source software · Press.
- Pull from existing CV at `papers/cv/` (Jeffrey-CV pipeline) — trim
  to one page for this fellowship.

---

## Work Samples (max 2; mix of formats allowed)

**Recommended set — emphasizes the instrument-and-score spine:**

**Sample 1 — Writing sample / musical score (≤ 10 pages)**
**The Whistlegraph paper** —
`papers/arxiv-whistlegraph/whistlegraph.pdf` (~4 pp.). The
graphic-score thesis behind the score-as-document idea. Concrete,
readable, and exactly the artifact a Legacy panelist can pick up and
get the practice from in a single sitting. *Description field:* "A
short paper on Whistlegraph, the AC graphic-score performance form.
The paper is itself a score — typeset through the AC papermill in
custom fonts I drew."

**Sample 2 — Video / Audio (≤ 5 min total, two clips OK if each ≥ 1 min)**
**Two clips:** (a) **notepat performance reel** — ~2:30, solo on the
instrument; (b) **Whistlegraph performance/demo clip** — ~2:00, the
piece in active use. Together: ≤ 5 min. Pull from existing AC
documentation; capture one fresh take of each in the next week if
the existing reels are over the duration cap.

**Alt set** (use if reviewers may not click play):
- Sample 1: 10 images of work across the practice — AC Native OS
  boot, notepat UI, a papermill card, NELA Computer Club performance
  at Plot.Place, *No Paint* output, KADIST acquisition install, SMK
  install, UCLA classroom visit, *Whistlegraph* still, KidLisp source
  as visible art.
- Sample 2: one video ≤ 5 min — notepat performance reel.

---

## Submission Checklist

- [ ] **Today (2026-05-19) or tomorrow:** ask Casey Reas for the
      letter of support; share the program description + the three
      points + 2026-06-06 deadline + the direct upload link the portal
      will give him.
- [ ] Pull the LAPP guidelines PDF one more time inside the portal to
      catch any field that isn't in the PDF (e.g., demographics, exact
      residency-proof requirement).
- [ ] Open the WizeHive portal, start the Legacy application, decide
      written vs. audio format (recommend **written**), commit to it.
- [ ] Trim each narrative answer to ≤ 250 words against the live form.
- [ ] Build / trim the one-page CV.
- [ ] Decide the work-sample set (recommended: Whistlegraph paper +
      two-clip video). Pull / re-edit the reels.
- [ ] Capture in-portal demographics + residency-proof fields.
- [ ] Confirm Casey's letter is uploaded.
- [ ] Submit before **2026-06-06, 23:59 PT**.

---

## Internal notes

- **Single spine** (per AC proposal rule): the *instrument +
  score-as-document* through-line. The CultureHub application leads
  with the same spine — coherence across the two active applications
  is a feature, not a duplication.
- **Mentorship through-line** is the load-bearing piece for the Legacy
  tier specifically — NELA Computer Club is the strongest concrete
  example.
- **Reporting (after award):** initial info survey + final debrief
  survey OR wrap-up session; each fellow is featured on LAPP's website,
  social, and digital publication. Optional in-person convening +
  online networking events.
