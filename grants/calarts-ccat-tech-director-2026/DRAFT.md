# CalArts CCAT — Technical Director — Application Draft

> **Posting:** https://www.paycomonline.net/v4/ats/web.php/portal/CAF0FE2BC5A458C654822EC0474C8E21/jobs/154720
> **Salary:** $100,000–$125,000/yr · full-time · fully on-site (Valencia, CA)
> **Funder:** CHANEL Culture Fund (grant-funded; continuation tied to fund availability)
> **Submitted via:** Paycom ATS portal (no named recipient surfaced — addressed to CCAT search committee unless one is found)
>
> *Draft only. Not yet submitted. Trim each section once the actual
> Paycom form fields and word caps are open.*

---

## Cover Letter (one page · ~450 words)

**To the CCAT search committee,**

I'm writing to apply for the Technical Director role at BB6. I think
this is a job I am already doing — just at a different address.

For the last five years I have been the technical director of
**Aesthetic Computer** (`aesthetic.computer`), a creative-computing
platform that runs as a working public artwork. AC is, at the
infrastructure layer, exactly the kind of thing CCAT is asking
someone to keep alive: a Linux monolith on DigitalOcean (the **lith**
server, deployed via a fish script + systemd, fronted by Caddy); a
**GPU build farm** I call **oven** that runs OTA video-generation
pipelines (whisper, FLUX, ffmpeg, Docker, with cron-driven tmp_pack
cleanup so the disk doesn't fill); a real-time-rendering runtime in
the browser; a multiplayer relay with a UDP/WebSocket dual-channel
pattern; a session server, an at-protocol bridge, a Cloudflare Worker
feed; and **AC Native OS**, a bare-metal Linux build whose kernel
embeds the git hash at compile time and ships via an oven-driven OTA.
I run the equipment, the licensing, the deploys, the renames, the
post-incident clean-ups. I write the onboarding materials. I host
the demo nights.

The reason I want this specific job is that BB6 is the closest thing
I have seen to a **physical version of what I already maintain
remotely** — a rotating room of artists, researchers, fellows, and
students who need their tools to *just work* so they can think about
the work, not the wires. That is the whole posture of my practice. I
host **NELA Computer Club**, a biweekly demo night at Plot.Place in
Chinatown LA where artists show works-in-progress on each other's
machines. I am the Author in Residence at **UCLA Social Software**,
hosted by Casey Reas (a working relationship that goes back to a
public conversation we had at bitforms gallery in 2018, eight years
before this AIR). I have been a **Technical Curator at GIPHY**
(2017), a contractor at Linked by Air (2013–2014), and a teacher
since 2013 at Parsons (Adjunct, 2013–2016), Yale (RDP Lecture
+ Workshop, 2017), UCLA Digital Media Arts (Visiting Professor 2016
/ Summer Section: Interactivity 2024), Southern Oregon University
(Assistant Professor of Emerging Digital Practices, 2019), and
UCLA Social Software (currently). I just published a paper called
*"CalArts, Callouts, and Papers: Art School as Operating System"* —
the thesis is that an art school is at its best when its infra-
structure is pedagogy, when the studio is a runtime that the
students learn to read. BB6, as described in the posting, is that
runtime.

I would bring: hands-on Linux/GPU/Docker administration; working
familiarity with real-time rendering, projection, and immersive
pipelines (and a willingness to ramp fast on the specific named
tools — Resolume, MadMapper, Disguise, the LED-volume stack); a
genuinely scrappy approach to budget-aware equipment expansion; and
**thirteen years** of being patient with users at every skill
level — DMA undergrads at UCLA, MFA students at Parsons (Adjunct,
2013–2016) and Yale, the rotating GIPHY production-tooling cohort
I supported as **Technical Curator** (2017), and the working
artists who pass through NELA Computer Club at Plot.Place each
week.

I would be on-site every day, by the door, with the espresso on.

— **Jeffrey Alan Scudder**
&nbsp; aesthetic.computer · @jeffrey · Los Angeles, CA

---

## Resume / CV

Use `papers/cv/cv.pdf` (HTML source of truth at `justanothersystem.org/cv`).
Confirmed against `papers/cv/cv.tex`:

- ✓ Yale School of Art MFA 2013
- ✓ Ringling College of Art + Design BFA 2011
- ✓ UCLA Social Software AIR (Casey Reas) 2026
- ✓ Technical Curator, GIPHY 2017 *(directly relevant prior role; surface in cover letter)*
- ✓ Linked by Air 2013–2014
- ✓ Adjunct Parsons 2013–2016
- ✓ Visiting Professor, UCLA DMA 2016
- ✓ UCLA DMA Summer Section: Interactivity 2024
- ✓ Assistant Professor of Emerging Digital Practices, Southern Oregon University 2019
- ✓ KADIST collection (per CV press entry, 2023; URL: `kadist.org/work/rdp-98-jas-17-4-16-17-03`)
- ✓ SMK / National Gallery of Denmark collection (per `justanothersystem.org/cv.html` bio paragraph; **not yet** in `cv.tex` — add as a CV refresh task)
- ✓ Casey Reas + Jeffrey in conversation, bitforms gallery, NYC, 2018 (pre-existing relationship before AIR)
- ✓ 2018 RDP tour with Goodiepal & Pals across Hamburg, Oslo, Louisiana Museum,
    KUNSTHAL AARHUS, Mayhem Copenhagen, West Germany Berlin, etc.

Refresh tasks before submitting:

- [ ] Add SMK collection line item to `cv.tex` Press / Collections section (the HTML bio has it; the LaTeX CV does not)
- [ ] Add 2026 entries: NELA Computer Club host, AC Native OS, oven, lith
  (decide which deserve a CV line vs. portfolio link)
- [ ] Re-run `tectonic -X compile cv.tex` after edits

---

## Statement on Technical Direction (~250 words · if asked)

> *Format the form actually requires is unknown — Paycom listings
> sometimes ask for a short philosophy statement. If the form has a
> "why this role / how do you approach technical direction" prompt,
> use this.*

I think a Technical Director's job is to make the equipment
disappear. The point is the work that happens in the room, not the
gear that enables it. The closer the studio gets to feeling like a
musical instrument that anyone in CCAT's rotation can pick up and
play, the better the technical direction is.

That posture is non-negotiable for me. I have built and run a public
creative-computing platform for five years on the same posture: the
infrastructure exists to serve creative inquiry, not the other way
around. When that line is blurred — when the lab starts to feel like
it belongs to its operator — the room shrinks. I work hard to keep it
the size of the rotating cohort.

Practically: I document the way someone unfamiliar with the room
will need it documented, not the way I'd remember it. I label things.
I keep the equipment inventory current and visible. I run a single
canonical onboarding script (mine for AC is a literal `init` skill).
I default to in-person help over a Slack thread. I pursue grants,
loans, and in-kind partnerships as a normal part of the job — my own
fleet of compute is half donated, half bartered. And I treat campus
IT and facilities as collaborators, not obstacles, because the lab is
inside the school, not next to it.

---

## References

- **Casey Reas** — Author in Residence host, UCLA Social Software
  · co-creator of Processing
  · *[email — confirm before listing]*
- **Sage Jenson** (mxsage) — peer-technical reference; GPU-simulation /
  projection artist
  · exhibited at Lyon's Fête des Lumières, the Barbican Centre, Sundance
  · author of `growth`, `realbloom`, `36 Points` (Feral File series)
  · recent collaboration with Neri Oxman
  · recurring coding-jam collaborator with jeffrey since at least Dec 2022
    (Catenary splines, MuJoCo, three.js IK, OffscreenCanvas — shared via iMessage)
  · contact: `sagejenson@icloud.com` · [github.com/mxsage](https://github.com/mxsage)
  · *[confirm consent before listing — likely yes]*
- **[Third reference — TBD]** — possibilities:
  - Plot.Place / NELA Computer Club host (working-relationship reference)
  - KADIST San Francisco curator (collection placement contact)
  - SMK Copenhagen curator (collection placement contact)
  - Yale MFA faculty (formal/academic reference)

---

## Portfolio / Work Samples

If the form asks for links or samples:

1. **aesthetic.computer** — the live platform itself; click any
   piece in the prompt
2. **papers.aesthetic.computer** — the papers + Reader (twelve
   arxiv-format papers, the dis-order pamphlet, KidLisp reference cards)
3. **`papers/arxiv-calarts/calarts.pdf`** — *"CalArts, Callouts, and
   Papers: Art School as Operating System"* (the relevant paper)
   ⚠️ **Fact-check flag:** the paper currently contains the line
   *"The author attended CalArts (MFA Art, 2012)"* (arxiv-calarts/calarts.tex
   line 167). The CV says **Yale MFA 2013** and there's no CalArts
   entry. Before submitting this application, **edit the paper** —
   either remove that sentence, or rewrite it to position CalArts as
   a *cultural ancestor* of AC rather than a literal alma mater
   ("AC inherits CalArts's operating system" stays; "the author
   attended" goes). A CCAT search committee following the link would
   read it as a misrepresentation.
4. **`papers/arxiv-ac/ac.pdf`** — the Aesthetic Computer overview paper
5. **NELA Computer Club** — Plot.Place, Chinatown LA, biweekly · short
   doc reel of one night
6. **`slab/menuband/`** — the menubar instrument (Swift, ships v0.9.1)
7. **`fedac/native/`** — AC Native OS bare-metal Linux build
8. **`oven/` deploy notes (vault)** — sysadmin reality for the GPU build farm

---

## Logistics — to confirm before submitting

- [ ] Reread the actual Paycom application form for required fields
      and caps
- [ ] Refresh `papers/cv/cv.pdf` (2026 entries)
- [ ] Confirm references list with the named people
- [ ] Decide commute vs. relocation framing with Fia (NELA → Valencia
      is doable but is daily-not-occasional)
- [ ] Confirm CCAT exact name + Executive Director name + Director of
      ML name + Director of Moving Image name (search before
      submitting; address letter directly if found)
- [ ] Attach the CalArts paper PDF as a portfolio item if the form
      allows file uploads beyond CV
- [ ] Submit via the Paycom portal
- [ ] Log a SUBMITTED row in `papers/SUBMISSIONS.md` with date +
      confirmation #
- [ ] Update the deadlines page once submitted (status: Active → Submitted)

---

## Submission Checklist (final)

- [ ] Cover letter trimmed to fit form (likely 250–500 words)
- [ ] CV updated and uploaded
- [ ] Three references confirmed and listed with consent
- [ ] Optional statement on technical direction included if a field exists
- [ ] Portfolio links pasted into the relevant field(s)
- [ ] EEO questionnaire completed
- [ ] Submitted; confirmation # captured
- [ ] `SUBMISSIONS.md` updated
- [ ] `deadlines/index.html` row updated to SUBMITTED
