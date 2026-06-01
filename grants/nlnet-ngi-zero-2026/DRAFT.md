# NLnet Restack — Application Draft

> **Fund:** NLnet — **Restack** (NOT the Commons Fund — that closed
> June 1, 2026). Same €5k–€50k band, same FLOSS rules.
> **Deadline:** 2026-08-01, 12:00 CEST.
> **Form:** https://nlnet.nl/propose/ — short, no word limits, plain
> and technical. Fields filled top-to-bottom below.
> **Applicant:** Aesthetic Inc (US C-corp, EIN 33-1483385).
> **Draft v0 — engineering register. Lead with the language + protocol
> as infrastructure; art is the motivating example, never the ask.**

---

## Eligibility — self-check

- [x] No categorical exclusion of applicant type — companies eligible
- [x] Applicant is a legal entity: **Aesthetic Inc**, US C-corp,
      EIN 33-1483385
- [x] Project results ship under a recognised FOSS license — repo is
      **ISC** today; new spec/docs under CC-BY-4.0 / CC0
- [x] Open standards / interoperability — deliverable IS a spec
- [x] Eligible activities: development + **standardization** +
      **documentation** + test suites
- [x] Requested amount in €5,000–€50,000 band (draft = **€48,000**)
- [ ] **Geographic caveat:** US applicant; EU/Horizon-associated get
      priority on equal quality. Must show a *clear European /
      global-commons dimension* (handled in Ecosystem field)
- [ ] **GenAI disclosure** must be answered honestly (field 16) — see
      RESEARCH.md box; rewrite-in-own-hand recommended before submit
- [ ] Select call = **Restack** in the dropdown

---

## CONTACT

- **Your name:** Jeffrey Alan Scudder (ORCID 0009-0007-4460-4913)
- **Email:** mail@aesthetic.computer
- **Phone:** *(TODO — jeffrey)*
- **Organisation:** Aesthetic Inc (US C-corp, EIN 33-1483385)
- **Country:** United States *(with a clear European/global dimension —
  see Ecosystem)*
- **Please select a call:** **Restack**

---

## 7. Proposal name

**KidLisp — an open language and URL-addressable runtime for
read-fork-share computing**

*(Working alternates: "KidLisp & the Piece Protocol: a commons runtime
for browser-native programs"; pick the clearer one against the live
form.)*

---

## 8. Website / wiki

- https://aesthetic.computer — the live runtime
- https://kidlisp.aesthetic.computer *(or the kidlisp/ docs path; TODO
  confirm public docs URL)*
- Source: the Aesthetic Computer repository (ISC-licensed)

---

## 9. Abstract — "explain the whole project and its expected outcome(s)"

Most computing today is locked behind app stores and platforms. You
cannot read the program you are using, you cannot fork it, and you
cannot hand it to someone else by saying where it is. KidLisp and the
"piece" model are a working counter-example — and this project turns
that working software into reusable public infrastructure.

KidLisp is a minimal Lisp dialect for generative, interactive programs.
It has 118 built-in functions and a single open-source evaluator
(~15,000 lines of JavaScript) that runs in any browser with no install,
no account, and no app store. A program — a "piece" — is addressed by
its URL. You read it, you fork it, you publish your fork to a new URL,
and the next person finds both. Over **17,000 user-authored KidLisp
programs** already exist this way. The runtime also runs on AC Native
OS, an open operating system written to host these pieces directly —
so the same readable program runs browser-native and on bare metal.

Today the language works but is **under-specified**: there is one
reference implementation, no formal grammar, no language server, no
conformance tests, and incomplete documentation. That is the gap
between "an interesting project" and "infrastructure other people can
build on." This project closes it.

Deliverables: (1) a **written language specification** for KidLisp —
grammar, evaluation model, the full built-in surface; (2) a
**standalone, dependency-light reference parser** extracted from the
runtime, with a **conformance test suite** any implementation can run
against; (3) a **language server (LSP)** so KidLisp gets diagnostics,
completion, and hover in standard editors; (4) a documented,
versioned **"piece protocol"** — how a URL resolves to source, how a
fork relates to its origin, how storage and retrieval work — published
so anyone can host a compatible runtime; (5) **documentation and a
"write your own runtime" guide.**

Expected outcome: KidLisp becomes a language anyone can implement,
tool, teach, and host — not a single-vendor feature. A small,
readable, forkable, URL-native programming environment that belongs to
the commons instead of a platform.

---

## 10. Prior experience

I am the author of **Aesthetic Computer (AC)**, a 5+ year open-source
creative-computing platform, and of **KidLisp**, the language this
proposal is about. I designed and wrote the KidLisp evaluator
(`system/public/aesthetic.computer/lib/kidlisp.mjs`, ~15,000 lines),
its storage/retrieval API (`store-kidlisp.mjs`), and the analysis
tooling around it (`kidlisp/tools/`). I also wrote **AC Native OS**,
the operating system that hosts pieces outside the browser, and the
runtime (`bios.mjs`, `disk.mjs`) that exposes the piece API.

AC is genuinely in public use: 17,000+ user-authored KidLisp programs,
a continuous stream of ~30 arXiv-style technical papers documenting its
subsystems, and a four-language documentation pipeline. Two pieces —
*No Paint* (collaborative drawing with persistent state) and *notepat*
(a polyphonic synthesizer) — reached the Hacker News front page. The
work is held in the collections of KADIST (San Francisco) and SMK
(Copenhagen).

Background: Yale MFA (2013), Ringling BFA (2011); artist-in-residence
at UCLA Social Software (host: Casey Reas, creator of Processing); I
run the NELA Computer Club, a free biweekly public creative-computing
meetup in Los Angeles. I have shipped, maintained, and documented this
codebase in the open for five years. The work proposed here is
extracting and specifying a language I already built and operate — not
starting from zero.

---

## 11. Requested Amount — €48,000 — budget & milestones

Budget is engineering and standardization effort on the open
deliverables above. **Rate: €600/day** (a working open-source
engineering day rate); **80 days total → €48,000.** No other funding
covers this specification/tooling work; AC's general development is
self-funded by Aesthetic Inc. Each milestone is an independently
verifiable artifact (a published document, a passing test suite, a
released package) so it can be signed off on completion.

| # | Milestone / deliverable | Effort | Amount |
|---|--------------------------|--------|--------|
| M1 | **KidLisp language specification, v1.0** — written spec covering lexical grammar, evaluation model, scoping/timing semantics, and the complete 118-function built-in surface. Published as HTML + PDF under CC-BY-4.0. | 15 days | €9,000 |
| M2 | **Standalone reference parser + grammar** — parser extracted from the runtime into a dependency-light package (grammar file + tokenizer + AST), released to a public package registry under ISC, decoupled from the AC browser environment. | 15 days | €9,000 |
| M3 | **Conformance test suite** — an open, runnable battery of programs + expected results that any KidLisp implementation can be checked against; CI-runnable; documents each built-in's contract. | 12 days | €7,200 |
| M4 | **KidLisp Language Server (LSP)** — diagnostics, completion, hover, and go-to-definition for KidLisp in any LSP-capable editor (VS Code, Emacs, Neovim), released under ISC. | 18 days | €10,800 |
| M5 | **Piece-protocol specification** — documented, versioned protocol for URL→source resolution, fork↔origin lineage, and storage/retrieval, so third parties can host a compatible runtime. Published under CC-BY-4.0. | 12 days | €7,200 |
| M6 | **Documentation + "write your own runtime" guide** — developer docs, a worked guide to implementing a minimal compatible KidLisp host, and migration notes. Published under CC-BY-4.0. | 8 days | €4,800 |
| | **Total** | **80 days** | **€48,000** |

*Other funding sources: none for this work. AC's ongoing operation is
funded by Aesthetic Inc. This grant funds only the open specification,
reference tooling, and documentation listed above.*

*(If a tighter ask is wanted, the €30k floor = M1+M2+M3+M5; M4 and M6
are the scalable extension.)*

---

## 12. Budget comparison — "compare with existing or historical efforts"

The closest relatives are creative-coding environments and small
embeddable Lisps, and KidLisp's commons angle is what's missing in
each:

- **Processing / p5.js** (Casey Reas, Lauren McCarthy et al.) — the
  reference for open creative coding, and a direct lineage influence.
  But a p5.js sketch is a file you download and host yourself; there is
  no built-in URL-addressable, forkable program identity, and no
  minimal embeddable *language* — it's a JS library. KidLisp adds a
  small specified language and a piece protocol on top of the same open
  ethic.
- **Scratch / Snap!** — block languages for learners, but tied to a
  hosting platform and not a text language you can spec and re-implement
  cheaply. KidLisp is plain text, URL-native, and small enough to
  re-implement.
- **Embeddable Lisps (Fennel, Janet, small Schemes)** — excellent
  small languages, but general-purpose and not built around
  browser-native generative programs or URL-addressable sharing.
  KidLisp's contribution is the *runtime/protocol* pairing, not just
  the interpreter.
- **TIC-80 / PICO-8 fantasy consoles** — closest in spirit
  (small, shareable programs), but PICO-8 is proprietary and cartridge-
  based, not URL-addressable or fully FOSS. KidLisp is the open,
  web-native, no-install version of that idea.

Historically, the thing being reclaimed is the **shareware /
view-source web** — when you could read the program you were running.
App stores and minified bundles ended that. This project is a small,
concrete way to get "read it, fork it, share it by URL" back, with a
language spec underneath so it's durable.

The €5k–€50k of effort here is **extraction and specification of
already-working software**, which is why the budget is days of
engineering, not a multi-year build.

---

## 13. Technical challenges

- **Decoupling the parser from the runtime.** The evaluator is
  currently entangled with the AC browser environment (graphics, audio,
  the piece API). Extracting a clean, dependency-light parser/AST that
  is faithful to the reference behavior — without quietly changing
  semantics — is the central engineering risk.
- **Specifying timing and scoping precisely.** KidLisp programs are
  temporal (they run every frame) and have generative-art-specific
  evaluation semantics. Writing a spec that pins down evaluation order,
  state, and timing tightly enough for a second implementation to match
  is hard, and the conformance suite is what proves we got it right.
- **A conformance suite for visual/temporal output.** Many built-ins
  produce graphics or sound. Defining deterministic, checkable
  contracts (rather than "it looks right") for those is non-trivial;
  the suite will need a deterministic-evaluation mode and stable,
  comparable outputs.
- **Protocol versioning + fork lineage.** The piece protocol must
  define how a fork references its origin and how URL resolution and
  storage interoperate, in a way a third-party host can implement
  without the AC backend. Getting the versioning right so old pieces
  keep resolving is the durability concern.
- **LSP over a small dynamic language.** Useful completion/diagnostics
  for a dynamically-typed Lisp with 118 built-ins means good static
  knowledge of the built-in surface — which the spec (M1) feeds
  directly.

---

## 14. Ecosystem engagement — and the European / global dimension

**Who this is for:** anyone who wants small, readable, forkable
programs without a platform gatekeeper — creative coders, educators,
self-taught programmers, and people building local-first or
browser-native tools. The spec + conformance suite mean **second and
third implementations become possible**, which is the point: the
language stops being one company's feature.

**European / global-commons dimension** (NLnet geographic note): the
deliverables are browser-native and platform-independent — they work
anywhere, under no app store, which is precisely the "reclaim the
public nature of the internet" goal. The lineage is European-rooted
open creative coding (Processing's community, the demoscene). The work
is held in a European public collection (SMK, Copenhagen). I will
engage the Processing/p5.js and broader FOSS creative-coding
communities directly, publish the spec and tooling openly, and make the
runtime trivially self-hostable so European institutions and educators
can run their own instances with no dependency on a US service.

**How outcomes get promoted:** publish the spec + guides openly
(CC-BY-4.0); release parser, LSP, and test suite to public registries
(ISC); document everything through the existing AC papermill (~30
arXiv-style papers already public); demo at the NELA Computer Club and
in the UCLA Social Software residency; and announce to the
creative-coding and small-languages communities. The 17,000+ existing
KidLisp programs are an immediate corpus to validate the spec and
conformance suite against.

---

## 15. Attachments (prepare)

- [ ] One-page **technical overview PDF** (architecture: evaluator →
      parser extraction → spec → LSP → protocol).
- [ ] Link sheet: repo (ISC), live runtime, papermill, a few
      representative public KidLisp pieces.
- [ ] Optional: the KidLisp README as plain text.

## 16–18. GenAI disclosure

- **MUST answer honestly.** This draft was Claude-assisted. Either (a)
  rewrite the Abstract / Technical Challenges in your own words and
  answer "no," or (b) answer "yes" and paste the prompt dates, prompts,
  and unedited output. Recommend (a). See RESEARCH.md.

## 19–21. Privacy / submit

- [ ] Acknowledge privacy statement.
- [ ] "Send me a copy."
- [ ] Optional PGP pubkey.

---

## Submission checklist

- [ ] Skim CodeSupply + Restack Guide-for-Applicants to confirm Restack
      is the best of the open funds and has no EU-only hard rule.
- [ ] Set jeffrey's real day-rate; re-sum the table if it changes.
- [ ] Confirm public KidLisp docs URL for field 8.
- [ ] Fill phone number.
- [ ] Resolve GenAI disclosure (rewrite recommended).
- [ ] Decide €30k vs €48k ask.
- [ ] Build the one-page technical overview PDF attachment.
- [ ] Submit before **2026-08-01, 12:00 CEST**.

---

## Internal notes

- **Single spine:** KidLisp-as-specified-language + the piece protocol.
  Everything else (AC broadly, notepat, No Paint, papers) is context or
  evidence of capability — never part of the ask.
- **Register:** engineering/infrastructure, not art-career. No
  fellowships, exhibitions, or "support my practice" framing anywhere
  in the budget rationale. Art is the motivating example only.
- **Coherence with other live applications** (CAC-IAF, CultureHub):
  those lead with the *instrument + score* spine for arts funders; this
  one deliberately leads with the *language + protocol* spine for a tech
  funder. Same platform, correct framing per venue.
