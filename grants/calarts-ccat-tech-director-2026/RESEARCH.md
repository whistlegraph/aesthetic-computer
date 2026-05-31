# CalArts CCAT — Technical Director — Research

> **Posting:** https://www.paycomonline.net/v4/ats/web.php/portal/CAF0FE2BC5A458C654822EC0474C8E21/jobs/154720
> **Title:** Technical Director, CCAT — BB6 studio + lab
> **Location:** Santa Clarita, CA 91355 (CalArts campus, Valencia)
> **Salary:** $100,000 – $125,000 / year
> **Start date listed:** 2026-04-13 (already past as of 2026-05-07; treat as "ASAP, rolling")
> **Schedule:** full-time, fully on-site (no remote/hybrid)
> **Funding:** grant-funded by the **CHANEL Culture Fund** — continued employment contingent on funding availability

## What the role actually is

Operational backbone of the **BB6 studio and lab** at CCAT (Center for
Critical Art and Technology / CalArts' new arts-and-tech research lab —
**[verify exact name]**). Not a curator, not a research lead. The
person who:

1. Keeps every machine in BB6 reliable for a rotating cast of artists,
   fellows, students, researchers.
2. Walks a first-year student through an unfamiliar tool at 2pm and
   diagnoses a hardware failure at 9am.
3. Owns equipment inventory, lab safety, software licensing, and the
   liaison relationship to **CalArts IT, facilities, and campus safety**.
4. Manages CCAT's **GPU compute** (ML training / research workloads) in
   coordination with the **Director of Machine Learning**.
5. Supports virtual production, projection mapping, XR — calibration,
   warping/blending, headset configs, real-time rendering pipelines.
6. Owns equipment strategy: gap analysis, wish list, grants /
   in-kind / loans / vendor relationships, annual budget recs to the
   Executive Director.
7. Designs, installs, strikes, and documents technical setups for
   public-facing CCAT exhibitions and performances.

Day-to-day coordination with: **Directors of ML and Moving Image, and a
Project Manager.**

## Why this is a near-perfect jeffrey shape

The job description maps almost line-by-line onto AC's working
infrastructure and what jeffrey already does daily:

| What CCAT wants | What jeffrey already runs |
|---|---|
| Linux/Unix sysadmin, GPU computing, Docker, networked storage | **lith** monolith on DigitalOcean (Express + Caddy + systemd), **oven** GPU build farm, AC Native OS bare-metal builds |
| HPC / GPU cluster admin | oven runs OTA video-gen pipelines (whisper, FLUX, ffmpeg), tmp_pack disk-fill cleanup crons; pop/ + recap render farms |
| Projection mapping — Resolume / MadMapper / Disguise / etc | AC pieces ship a real-time renderer; Whistlegraph and notepat have been performed under projection; ready to ramp on the named tools |
| Virtual production — real-time rendering, camera tracking, LED/projection volume | AC's runtime IS a real-time renderer; UE5 builder env in vault for `false.work` already wired |
| XR — VR/AR/MR headset config + spatial computing | Done on AC pieces with pen/hand input; ready to ramp on hardware-specific configs |
| Equipment inventory, scheduling, lab access policies | Plot.Place / NELA Computer Club is a small working version of this |
| Onboarding materials, reference guides, patient user support | KidLisp Reference, AC Repo Archaeology paper, UCLA Social Software course materials, NELA biweekly demo nights |
| Scrappy, budget-aware equipment expansion | AC ran 5 years on tiny budgets; built the oven, lith, session-server, AC Native OS in-house |
| Genuine support orientation | NELA Computer Club is exactly this — host other artists' work on shared machines |
| Cross-institutional collaboration with IT/facilities/students/faculty | UCLA Social Software AIR hosted by Casey Reas (Lauren Lee McCarthy is a Social Software cofounder, not the AIR host); Plot.Place / NELA Computer Club; Yale MFA / Ringling alumni network. KADIST + SMK are *collectors* (placement of work), not working collaborators — keep separate. |
| Curiosity about emerging tools / AI-adjacent creative tech | Aesthetic Computer is a public, ongoing research practice in this exact space |

## The arxiv-calarts paper is the warm-start

`papers/arxiv-calarts/calarts.tex` already exists — *"CalArts, Callouts,
and Papers: Art School as Operating System."* It frames the school as a
creative-computing OS where infrastructure is pedagogy. That's the same
thesis CCAT is enacting with BB6. The application can quote / reference
this paper directly as evidence that jeffrey has *already* been
thinking about CalArts as a research-infrastructure problem.

## Why this is a single-spine fit (per feedback memory)

Per `feedback_proposals_single_spine.md`: pick **ONE** AC subsystem to
lead with. For a Technical Director role the spine is unambiguous:

> **AC's lived infrastructure practice** — the lith monolith, the oven
> GPU build farm, AC Native OS on bare-metal Linux, and the NELA
> Computer Club support culture — *all run by jeffrey, daily, as
> technical director of a working creative-computing platform.*

Everything else (KidLisp, Whistlegraph, the papermill, the Reader) gets
mentioned as evidence the infrastructure serves real artists. None of
it gets pitched as the project. The pitch is: *I am already doing this
job, at scale, in public.*

## Key constraints from the posting

- **On-site every day in Valencia.** Not negotiable. Currently jeffrey
  is in NELA / Chinatown LA. Valencia is ~45 min on the 5/14 — workable
  but worth confirming with Fia before committing.
- **CHANEL Culture Fund is the funding source.** Worth a 30-second
  reference in the cover letter — shows research before applying.
- **Reports into Directors of ML + Moving Image + a Project Manager.**
  Means the application should signal comfort with peer-reporting (not
  needing to be the headline), which jeffrey already practices at NELA
  and at UCLA Social Software.
- **Start date listed 2026-04-13** is already past — likely the role is
  open and rolling. The first-mover advantage of a fast, sharp
  application is real.

## Who to address

**[verify]** — the posting doesn't name a hiring manager. Common Paycom
flow is to upload via the portal with no named recipient. Cover letter
can address "CCAT Search Committee" or "Hiring Committee, CCAT BB6."
If a name surfaces (LinkedIn search for CCAT executive director,
director of ML, director of moving image), prefer the named address.

## What we know about CCAT (to verify before submitting)

- CCAT = Center for Critical Art and Technology, CalArts initiative —
  **[verify exact name]**
- BB6 is the studio/lab building/room — **[verify location on campus]**
- **CHANEL Culture Fund** is the named funder — public partnership,
  worth a sentence acknowledging
- Senior Curator + Executive Director are mentioned — search for who
  these are before submitting
- Directors of Machine Learning and Moving Image — search for names

## Application materials likely required

Paycom ATS standard fields — **[verify when starting the application]**:

- [ ] Resume / CV (use `papers/cv/cv.pdf`, refresh dates)
- [ ] Cover letter — written below in DRAFT.md
- [ ] References (3 — Casey Reas as AIR host; **Sage Jenson / mxsage** as peer-technical reference for GPU/projection work; third TBD — possibilities: Lauren Lee McCarthy as Social Software cofounder, Goodiepal, KADIST/SMK contact, Plot.Place, Yale MFA faculty)
- [ ] Portfolio / work samples — pointer to aesthetic.computer + papers.aesthetic.computer
- [ ] Equal opportunity questionnaire (standard)
- [ ] Possibly: a short statement on lab safety / equipment-management philosophy

## Open questions

- Is the role currently filled and the posting just stale? (If
  start-date 04/13 has passed and the listing is still up, the role
  almost certainly hasn't been filled.)
- Salary band $100–125k — at the top of CalArts' typical staff
  scale. Worth showing the application is taking the rate seriously
  (i.e., relocation + on-site commute justified).
- Does the role include any creative latitude, or is it pure ops? The
  description leans heavily ops with curiosity-as-disposition. Pitch
  must respect that — do *not* turn the cover letter into "and here is
  my own art project I'd like to make at BB6." That's the wrong
  framing. The *job* is the project.

## Sources

- [Job posting (Paycom)](https://www.paycomonline.net/v4/ats/web.php/portal/CAF0FE2BC5A458C654822EC0474C8E21/jobs/154720)
- `papers/arxiv-calarts/calarts.tex` — existing CalArts paper
- `papers/cv/cv.pdf` — current CV
- AC infra references:
  - `lith/` + `lith/deploy.fish` — monolith deploy
  - oven (DigitalOcean VPS + systemd) — `aesthetic-computer-vault/oven/`
  - `fedac/native/` + `ac-os` CLI — bare-metal Linux work
  - `session-server/` — geckos.io UDP relay
  - `slab/menuband/` — Swift menubar app
  - `pop/` and `recap/` — GPU video-gen pipelines

## Next steps

1. Open the actual Paycom application form — capture exact fields and
   any caps.
2. Refresh `papers/cv/cv.pdf` with 2026 dates if needed.
3. Confirm references list.
4. Decide commute / relocation framing with Fia before submitting.
5. Submit via Paycom portal.
