# NLnet — Call Research & Fit Assessment

> Compiled 2026-06-01 from live pages. The brief named the *NGI Zero
> Commons Fund*, but that fund's **thirteenth and final call closed
> June 1, 2026** (verbatim from the page: *"The thirteenth and final
> call of NGI Zero Commons Fund closed on June 1st 2026… No more new
> applications are sought for this fund, but we have other programmes
> running."*). Several **other NLnet programmes share the same
> deadline — August 1, 2026, 12:00 CEST**. The right vehicle for this
> project is **Restack**. Everything below routes the application there.

---

## The fund to apply to: NLnet — Restack

- **Fund:** Restack (an NLnet / NGI-lineage open-call programme)
- **URL:** https://nlnet.nl/restack/ — submit via https://nlnet.nl/propose/
- **Deadline:** **2026-08-01, 12:00 CEST (noon)** — same cadence the
  brief expected (1st of even months). 61 days out from 2026-06-01.
- **Amount range:** **€5,000 – €50,000** per project, *"with the
  possibility to scale them up significantly if there is proven
  potential."* (Identical band to the Commons Fund the brief named.)
- **Programme budget:** €7M through 2030.
- **Scope (verbatim themes):** *"technology building blocks for an
  open internet stack, from hardware to applications"*; replacing
  *"'dark stack' components only available as volatile proprietary
  services"*; *"from libre chips to middleware without a vendor
  lock-in, from productivity tools to 'local-first' infrastructure"*;
  *"open to new ideas and disruptive technologies on every layer, but
  also want to nurture and scale existing technologies… practical,
  real-world impact."*

### Eligibility — confirmed company-eligible
- From the NGI0/Commons eligibility page (Restack inherits the same
  posture): *"There are no categorical exclusions of persons who may
  not receive support."* Companies, for-profits, individuals, and
  nonprofits may all apply. → **Aesthetic Inc (US C-corp) is eligible.**
- **Geography caveat:** EU residents and Horizon-Europe-associated
  countries get **priority when proposals are of equal quality**.
  Non-EU applicants remain eligible if they show *"a clear European
  dimension."* The US applicant must make the European/global-commons
  angle explicit. (See DRAFT "Ecosystem" field.)
- Minors may apply without parental consent at submission (not
  relevant here).

### Licensing requirement — confirmed FLOSS
- *"Project results shall always become available under a recognised
  free or open source license"* so *"anyone can read and validate the
  source code."*
- **AC repo license today: ISC** (`package.json` → `"license": "ISC"`).
  ISC is an OSI-approved, FSF-recognized permissive license — it
  **already satisfies the requirement.** No relicensing needed. Name
  ISC; optionally note new spec/docs deliverables can ship under
  CC-BY-4.0 / CC0 and the language grammar under ISC to match the runtime.
- Also valued: open standards / interoperability, right to reuse and
  repair, standards compliance.

### Eligible activities (broad)
Open-source development, research, **standardization work**,
**documentation**, security audits, and *"other activities relevant to
robust software development and deployment practices."* → A spec +
language-server + conformance-test deliverable set is squarely fundable.

---

## The exact proposal form (https://nlnet.nl/propose/)

Captured live, in order. There are **few fields and no word limits** —
NLnet wants concrete, technical, plain answers, not essays.

**Contact section**
1. Your name
2. Email address
3. Phone number
4. Organisation
5. Country

**Call selection**
6. *Please select a call* — dropdown. Options seen: *Restack · NGI
   TALER · NGI Fediversity · CodeSupply · ELFA · Research & Higher
   Education Technology Fund · Open Call · Other*. → **Select "Restack".**

**General project information**
7. **Proposal name** — project title.
8. **Website / wiki** — project web presence.
9. **Abstract** — *"Can you explain the whole project and its expected
   outcome(s)."* (No length cap; this is the heart of the form.)
10. **Prior experience** — *"Have you been involved with projects or
    organisations relevant to this project before? And if so, can you
    tell us a bit about your contributions?"*

**Requested support**
11. **Requested Amount** (€) — *"Explain what the requested budget will
    be used for? Does the project have other funding sources, both past
    and present? A breakdown in the main tasks with associated effort
    is appreciated. Make rates explicit."* → This is where the
    **milestone budget table** goes.
12. **Budget comparison** — *"Compare your own project with existing or
    historical efforts."*  (i.e. comparison-to-existing-FOSS field.)
13. **Technical challenges** — *"What are significant technical
    challenges you expect to solve during the project, if any?"*
14. **Ecosystem engagement** — *"Describe the ecosystem of the project,
    and how you will engage with relevant actors and promote the
    outcomes?"*

**Attachments**
15. Attachments — HTML / PDF / ODF / plain text, ≤ 50 MB total.

**Generative-AI disclosure** (mandatory)
16. *"Did you use generative AI in writing this proposal?"* (dropdown)
17. If yes: which model, what for, **the dates of prompts, the prompts
    themselves, and the unedited output** pasted in.
18. Optional: prompt-files upload (≤ 50 MB).

**Privacy / submission**
19. Privacy-statement acknowledgment checkbox.
20. "Send me a copy" checkbox.
21. Optional PGP pubkey.

> **GenAI disclosure note — IMPORTANT.** This draft is a Claude-assisted
> scaffold. If any of this text survives into the submitted proposal,
> jeffrey must answer field 16 truthfully and paste the prompts +
> output, OR rewrite the narrative entirely in his own hand and answer
> "no." NLnet takes this seriously. Cleanest path: jeffrey rewrites the
> Abstract/Challenges in his own words before submitting.

### How milestone budgeting works
NLnet funds against **concrete deliverables/tasks, each with euro
amount and effort**, paid on completion — not a lump salary. The
Requested-Amount field wants: tasks → effort → rate → sum. "Make rates
explicit." Keep deliverables independently verifiable (a spec
document, a passing test suite, a released package) so each can be
signed off. Budget is for **work on the open deliverables**, framed as
engineering/research effort — never as artist stipend or career support.

---

## Fit assessment

**Spine chosen (single, fundable):** *KidLisp + the URL-addressable
"piece" protocol as open internet-commons infrastructure* — an open,
specified creative-computing language plus an open runtime/protocol for
programs you can **read, fork, and share by URL**, as a commons
alternative to app-store / platform-locked computing. The funded work
is the **specification, reference tooling (parser/grammar, language
server, conformance test suite), and documentation** that turn a
working-but-undocumented language into reusable public infrastructure.

**Why this fits Restack well (high):**
- Restack explicitly funds *"productivity tools"*, *"local-first
  infrastructure"*, and replacing proprietary *"dark stack"* services.
  URL-addressable programs that run in any browser and on an open OS,
  without an app store, are exactly a dark-stack replacement.
- Eligible activities name **standardization** and **documentation** —
  the precise shape of this deliverable set.
- Licensing already compliant (ISC).
- "Nurture and scale existing technologies that are still
  future-proof" — AC is 5+ years live with 17,000+ user programs; this
  is a scale-up of working tech, not a speculative new build.

**Honest caveats / risks:**
1. **"Creative computing" can read as niche / art-career funding.**
   NLnet funds *internet technology and commons*, not artists. Mitigate
   by leading with the **language + protocol as general-purpose
   infrastructure**: the deliverable is a spec and tooling any developer
   can use, not paintings. Keep "art" as a motivating example, never the
   ask. No mention of fellowships, exhibitions, or collections in the
   budget rationale.
2. **Non-EU applicant.** US C-corp gets lower geographic priority.
   Mitigate with a genuine European/global dimension: open spec usable
   anywhere, browser-native (no platform gatekeeper), explicit
   engagement with EU FOSS/creative-coding communities (Processing/p5.js
   lineage, Fediverse-adjacent publishing, SMK Copenhagen as a European
   touchpoint — as a *collections* fact, not a partner).
3. **GenAI disclosure** (see box above) — must be handled honestly.
4. **Final-Commons-Fund confusion.** Do **not** select the Commons Fund
   in the dropdown; it's closed. Select **Restack**.
5. **Budget must read as engineering effort with explicit rates**, not
   a round number. The table in DRAFT does this; jeffrey should sanity-
   check the day-rate against what he'd actually bill.

**Fit-confidence: HIGH** for Restack as the vehicle and the
language/protocol spine; MEDIUM on the geographic-priority disadvantage,
which framing can soften but not erase.

---

## Key dates & facts
- Deadline: **2026-08-01, 12:00 CEST**.
- Ask target: **€30,000–€50,000** (draft lands at **€48,000**).
- Applicant: **Aesthetic Inc**, US C-corp, **EIN 33-1483385**.
- Lead: **@jeffrey / Jeffrey Alan Scudder**, ORCID 0009-0007-4460-4913.
- License: **ISC** (existing) + CC-BY-4.0/CC0 for new docs/spec.
- Form: 4 narrative fields (Abstract, Prior experience, Requested
  Amount/budget, Budget comparison, Technical challenges, Ecosystem) +
  contact + GenAI disclosure. No word limits.

## TODO / unverified
- [ ] **Confirm Restack is the best of the open funds** vs. CodeSupply
      or a generic "Open Call" — Restack reads strongest from its scope
      text; quickly skim CodeSupply before committing.
- [ ] Confirm Restack does not impose an EU-only hard requirement
      buried in its own Guide for Applicants (eligibility text quoted
      here is the NGI0 family's; Restack inherits it but verify on the
      Restack Guide for Applicants PDF).
- [ ] Pull the Restack-specific "Guide for Applicants" + FAQ before
      submitting (form instructs applicants to read them).
- [ ] Decide GenAI-disclosure path (rewrite-in-own-hand vs. disclose).
- [ ] Confirm jeffrey's real day-rate for the budget table.
