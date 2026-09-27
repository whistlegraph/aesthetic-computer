# Eyebeam AGENCY Virtual Residency, Fall 2026: Application Draft

> **Deadline:** Sunday 2026-10-11, 11:59 PM Eastern (8:59 PM Pacific). 18 days from 2026-09-23.
> **Program:** Eyebeam Virtual Residency (Fall 2026), season theme AGENCY. Six US-based artists outside NYC. Online, 12 weeks, Nov 4 2026 - Feb 12 2027, minimum 3-4 hrs/week. **$2,000 stipend** + weekly cohort sessions + alumni mentorship + technical consultation + end-of-year publication.
> **Apply:** Submittable, free, from https://eyebeam.org/articles/agency-virtual-residency. Info session Mon Sept 28, 1 PM ET.
> **DRAFT ONLY. Never submit from this file.**
>
> *Every narrative answer below is written at or under its published cap
> (150 / 300 / 150 / 200 / 200 / 200) so it drops straight into the form.
> The form cannot be edited after submission. Facts are canonical; anything
> marked TODO is Jeffrey's call.*

---

## Eligibility, self-check

- [x] US-based, outside New York City: Los Angeles.
- [ ] **Will remain US-based Nov 4 2026 - Feb 12 2027.** TODO: settle against the Spain thread and Matadero (Jan-Jul 2027). Cannot hold both.
- [x] Applying as an individual.
- [x] Not enrolled full-time; not full-time faculty. (UCLA Author in Residence is not a faculty post.)
- [ ] **Not participating in or committed to another residency during the term.** TODO: email programs@eyebeam.org about the UCLA Author-in-Residence title before Sept 28. Work//Shift (if awarded) is a fellowship, likely fine; confirm.
- [x] No "years of practice" cap in this cycle. Answer the field honestly.

---

## 1. Applicant Information

- **Name:** Jeffrey Alan Scudder (artist name: @jeffrey)
- **Mailing address:** TODO (Los Angeles; use the address on the CultureHub W-9, not Aesthetic Inc.)
- **Pronouns (optional):** TODO
- **Date of birth:** TODO (born 1989, Assonet, MA)
- **Timezone:** Pacific (America/Los_Angeles)
- **Known conflicts during the residency period (optional):** TODO. Candidates to disclose: the Work//Shift convening Jan 29-30, 2027 if awarded; holiday travel. Disclose Matadero only if it is still live at submission.
- **How many years have you been practicing?** 15 (professionally since the 2011 BFA; first residency 2010). TODO confirm the number Jeffrey wants to stand behind.
- **CV or Resume:** export `papers/cv/cv.tex` to PDF.
- **Portfolio link 1:** https://aesthetic.computer
- **Portfolio link 2:** https://prompt.ac/@jeffrey (or the papers platter, https://papers.aesthetic.computer). TODO pick.

### Short Bio (Limit: 150 words; draft is 130)

Jeffrey Alan Scudder is an artist who builds instruments and tools for other
people, based in Los Angeles. For five years he has made Aesthetic Computer,
an open-source public space of tiny programs that run at a URL you can type,
read, and change, with a small Lisp called KidLisp at its core. Out of it came
notepat, a polyphonic synthesizer played live; No Paint, a collaborative
drawing program; and Whistlegraph, a graphic-score performance form. He hosts
the free, biweekly NELA Computer Club in Chinatown, Los Angeles, and is Author
in Residence at UCLA Social Software with Casey Reas. His work is in the
collections of KADIST (San Francisco) and SMK (Copenhagen). Yale MFA 2013,
Ringling BFA 2011. He is interested in computers people can see all the way
into.

---

## 2. Video Introduction (1-2 minutes, .mov or .mp4)

Not a pitch. Three beats, one take, natural voice, quiet room, camera at
arm's length. Script as prompts, not lines:

1. **Who I am.** "I'm Jeffrey. I'm an artist in Los Angeles and I build
   instruments, mostly software, sometimes out of old laptops."
2. **What the practice is.** "For five years I've been building Aesthetic
   Computer, a public computer that lives at a URL. You type a word and get
   a small program back you can read and change. People have made about
   seventeen thousand of them. I make music instruments in it, drawing
   tools, and a tiny Lisp for kids."
3. **What I'm curious about right now.** "Lately an AI agent can sit in
   that space alongside people, under a handle, and I'm curious what it
   means for a room to see exactly what the machine did and to be able to
   say no to it. That's what I want to work on."

TODO: record the week of Sept 28. Play it back once before uploading.

---

## 3. Project Proposal

### 3.1 Describe the project and its stage (Limit: 300 words; draft is 298)

**Who Is Typing** is a prototype for making machine agency visible inside a
shared computer, and revocable by the people sharing it.

Aesthetic Computer is a public, open-source space of small programs at URLs.
People sign in with a handle and make pieces together in rooms. This year I added something new: a language-model agent can be
attached to a handle's room, and every screen signed in as that handle
lights a small mark, a crab, that says an agent is present. That is where
the project stands. The mark exists. Almost nothing behind it does.

Three things are missing, and they are the residency:

1. **Attribution.** When a person and an agent share a handle, which strokes,
   notes, and lines were the machine's? Every action an agent takes in a
   room should be marked as the agent's, readable by anyone in it.
2. **The grant.** A room should be able to say what the agent may touch: this
   canvas but not that one, the synth but not the chat. Given by the people
   present, taken back the same way.
3. **The rehearsal.** I run a free biweekly computer club in Chinatown, Los
   Angeles. I want to bring the agent to the club with the mark and the
   grant switched on and watch what a room of strangers does with it: play
   with it, play against it, write KidLisp with it, revoke it.

Stage: early. The presence layer is shipped and running; attribution and
the grant are on paper. Twelve weeks at three to four hours a week is enough
to prototype both, run three rehearsals, and write up what the room decided.
The result is a working demonstration, a short
paper, and a stack of pieces made by people and a machine that says which
was which.

### 3.2 How does the project relate to AGENCY, and why is it timely? (Limit: 150 words; draft is 146)

The call asks who acts and who is acted upon. Agent software answers that
question by hiding it: the agent acts, the interface smooths the seam, and
you can no longer tell what you did from what was done for you. Who Is
Typing puts the seam back. It is agency once it no longer belongs to a single
actor, made concrete: one handle, two hands, and a public record of whose. The grant is collective governance at the smallest scale that still
means something, a room of six people deciding what a machine may touch and
changing their minds. The rehearsal is mischief on purpose. And it is done
inside a free tool, not a platform. The timing is not abstract. Agents are
being wired into every editor and canvas this year with no mark and no
revoke. I want a working counterexample small enough for anyone to read.

---

## 4. Short Answer Questions (200 words each)

### 4.1 Three things you hope to accomplish by the end of the residency (draft is 167)

1. **Attribution, working.** By week four, any piece made in a shared room
   can show, live and afterward, which actions were a person's and which
   were the agent's. A practical milestone with a hard test: hand the piece
   to a stranger and ask them who did what.
2. **The grant, tested in public.** By week eight, a room can give an agent
   access to specific things and take it back, and I will have run at least
   three open sessions at NELA Computer Club where people actually use the
   revoke. The conceptual question underneath: what do people withhold
   first, and why?
3. **A readable account.** By the end, a short paper in the Aesthetic
   Computer papermill and a printed card for Eyebeam's publication that a
   non-programmer can follow: what we built, what the room granted, what it
   took back, and the pieces that came out, marked. If the honest finding is
   that people do not want the agent in the room at all, that is the paper.

### 4.2 What you hope to bring to the group, and gain from it (draft is 199)

I bring a working public computer the cohort can type into. Aesthetic
Computer runs in any browser and every piece has a URL, so when we argue
about agency in a session, we can run the experiment on the spot rather than
describe it. I also bring the habit of working in the open: five years of
papers, a free biweekly club, and the reflex to hand a tool to a stranger and
watch. I am comfortable being the person who says what is broken.

What I want back is the part I cannot make alone. My instinct is to build
first and think second, and the thinking here matters more than usual. I
want peers who work on bodies, labor, and collective life to tell me where
"a mark and a revoke" is too thin an idea of agency, and to test the
prototype against practices that are not software. I want alumni mentors
who have put artist-made infrastructure into the world, since that is what
Eyebeam has done before. And I want the discipline of weekly critique. I
have been working mostly alone in a fast-moving lane, and a room that slows
me down would be useful.

### 4.3 Do you need specific tools or support? (draft is 152)

Not much, and I want to be honest about that. The software runs on
infrastructure I already maintain, and I am my own developer, so there is no
production ask. What would help:

- **A critic who works on governance or AI policy** for one session in the
  middle, to push on the grant model before I harden it.
- **An introduction or two** into Eyebeam's alumni network of artists who
  build tools others use (the openFrameworks lineage is the obvious one),
  for the mentorship slot.
- **Guidance on the publication contribution**: format, length, and whether
  a printed card that is also runnable source fits. I would rather make the
  contribution a real object than a PDF.
- **A little inference budget** if Eyebeam has any, for the public
  rehearsals. Otherwise I will cover it.

Everything else I can do from Los Angeles with a laptop, a room, and a
Wednesday night.

---

## 5. Work Samples (two; at least one completed)

**Sample A, completed: *Whistlegraph presents* at CultureHub LA, Sept 24, 2026.**
Video. The recording of *Note(s)pat(ial) Native* (six salvaged laptops on AC
Native, notepat, spatialized) and *The MacNeoPolitan Trio* (three MacBook
Neos on Menu Band). **Specify a 2-minute segment**: TODO pick the passage
where sound bodies move between machines; state the timestamps in the form.
Host unlisted on YouTube or Vimeo; must stay live through ~Jan 9, 2027.
Shows execution and the "no single actor" idea in a finished work.

**Sample B, work in progress: *Who Is Typing*, presence layer.**
Either a ≤3-page PDF (screenshots of the linked-agent mark lighting across
surfaces, a KidLisp piece co-written with the agent, one paragraph of
context) or a short screen capture with a named 2-minute segment. TODO:
capture at a NELA Computer Club night if one falls before Oct 11.

Alternate for B if the agent capture is too thin: a No Paint or KidLisp
collaborative session at NELA (people, no agent), as the baseline the
project starts from.

---

## 6. Demographics (optional)

Jeffrey's choice. Withheld from the external panel either way.

---

## Submission checklist

- [ ] **Decide Matadero vs. Eyebeam** (term overlap and the "remain US-based" rule).
- [ ] Email programs@eyebeam.org re: UCLA Author-in-Residence title. Before Sept 28.
- [ ] Register + attend the Zoom info session, Mon Sept 28, 10 AM PT. Capture: session day/time, holiday break, tax withholding, publication format.
- [ ] Open the Submittable form; confirm every field matches this draft; note any character limits.
- [ ] Record the 1-2 min video introduction (.mp4).
- [ ] Export CV PDF from `papers/cv/cv.tex`.
- [ ] Cut and host Sample A; write the 2-minute timestamps.
- [ ] Make Sample B.
- [ ] Fill 1. Applicant Information (address, DOB, pronouns, conflicts, years).
- [ ] Paste bio, proposal (2 answers), short answers (3), portfolio links.
- [ ] Final read: first person, plain, no vendor names, no em-dashes.
- [ ] **Submit before Sunday 2026-10-11, 11:59 PM ET.** Jeffrey hits send.

---

## Internal notes

- **Single spine:** the marked, revocable agent in a shared public computer.
  Not the whole platform, not the laptop orchestra. The CultureHub piece is
  the completed sample and the bio source, per Fía's "similar to CH."
- **Why this over the laptop ensemble:** the AGENCY call names AI agents in
  three of four bullets, the Virtual track wants early-stage prototyping,
  and the presence layer is real and unfinished. The ensemble is finished
  and physical.
- **Jeffrey is not sold on this spine (2026-09-23).** Alternatives, each
  still answering "who acts, who is acted upon":
  1. **KidLisp, two hands on one line.** A child's language where a machine
     can also write. The residency builds the marked co-author: every
     generated form is visibly the machine's, and a kid can strip it back
     out. Early stage, fits the theme, keeps the club rehearsals.
  2. **Whistlegraph, the score that performs you.** A drawn gesture becomes
     notation that later plays the drawer. Agency passes from hand to
     recording to whoever replays it. Strong sample material, less AI.
  3. **The laptop ensemble (CultureHub).** Twelve notepats, one room, who is
     leading. Finished and physical, so it reads as documentation rather
     than a residency project. Weakest fit for the virtual track.
  4. **AC Native on cheap hardware.** A public creative OS on secondhand
     machines: agency as ownership of the device that runs you. Fits the
     "acted upon" half of the theme, but the term is short for OS work.
  Pick one, then 3.1 and 3.2 get rewritten; bio, CV, and Sample A stay.
- **Vendor names:** the proposal says "a language-model agent," never a
  product name. The MCP tool that does this is `toolchain/mcp/acin-mcp.mjs`.
- **Do not** claim AC Native runs on Apple hardware; the Neos run Menu Band.
- **Rubric line 1** (timely AGENCY take) is carried by 3.2 and the last
  paragraph of 3.1. **Line 2** (execution) by Sample A and the CV. **Line 3**
  (cohort) by 4.2 and NELA.
- Reuse for the interview (Oct 26-28, 30 min): the three gaps, the club, and
  the honest finding clause ("if people don't want it in the room, that's
  the paper").
