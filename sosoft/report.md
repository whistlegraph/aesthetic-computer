# SO SOFT — Social Software Cycle Proposal

## Proposal for DESMA 596/199: Score for Social Software
### Jeffrey Scudder / @jeffrey / Aesthetic Computer

---

## 1. The Score

**Title:** *Aesthetic Computer as Social Instrument*

**Core idea:** Develop Aesthetic Computer's existing social infrastructure into a
legible, playable *score* — a set of rules, rituals, and interfaces that a small
community performs together over the 10-week cycle.

AC already has the bones of social software built into its runtime:

- **@handles** — unique identity with per-character color customization
- **chat** — real-time group messaging with hearts/reactions
- **moods** — ephemeral status updates (mirrored to Bluesky via ATProto)
- **paintings** — collaborative pixel art gallery tied to user profiles
- **KidLisp** — a minimal Lisp dialect anyone can use to publish generative art
- **pieces** — URL-addressable programs anyone can write and publish at `@handle/piece-name`
- **multiplayer sessions** — WebSocket rooms that any piece can use for real-time collaboration
- **profiles** — live scorecards showing each user's creative output across all media types

What's missing is not technology — it's *choreography*. The platform has 351
built-in pieces, ~2800 registered handles, and 16k+ chat messages, but the
social dynamics are still largely emergent and undirected. The cycle offers a
chance to compose deliberate social scores *on top of* the existing runtime.

### Proposed Scores to Develop

**Score A: "Daily Painting Circle"**
A structured daily practice where cycle members each create and publish one
painting per day using AC's pixel tools (`new`, `rect`, `line`, `smear`, `fill`,
`shape`). Paintings are published via `done` and visible on each member's
profile. The group reviews the day's paintings together in weekly meetings.
This is a simple, repeatable ritual that teaches the platform's creative tools
through committed practice.

**Score B: "Piece Exchange"**
Each member writes and publishes one AC piece (a small interactive program) per
week, shared at their `@handle/piece-name`. The group plays each other's pieces
and discusses them. Members can fork each other's work via `source @handle/piece`
and publish remixes. This teaches the programming API through peer learning and
creates a growing library of social artifacts.

**Score C: "KidLisp Jam"**
Weekly sessions where members collectively write KidLisp programs — AC's
built-in Lisp dialect for generative art. KidLisp has 118 built-in functions
and programs can be stored and shared instantly. The constraint of a minimal
language (no external libraries, immediate visual output) creates a level
playing field between experienced programmers and beginners.

**Score D: "Chat as Performance"**
Using AC's real-time chat as a performance medium. Structured chat sessions with
rules — e.g., "only respond with piece names," "conversation through shared
paintings," "one word per message." The chat system supports custom fonts,
handle colors, and hearts, making it already suited to expressive constraint-based
communication.

### What I Would Build During the Cycle

1. **A `sosoft` piece** — a dedicated AC piece (`aesthetic.computer/sosoft`) that
   serves as the cohort's home base: a dashboard showing all members' recent
   activity, paintings, published pieces, and moods in one view.

2. **Score templates** — simple markdown + code templates that define the rules
   for each social score, publishable as AC pieces themselves.

3. **Multiplayer scoring pieces** — new pieces that use AC's session server for
   real-time collaborative drawing, KidLisp editing, or structured turn-taking.

4. **Documentation** — a "Write a Score" guide parallel to AC's existing
   "Write a Piece" guide, teaching others to compose their own social software
   scores on the platform.

---

## 2. Intended Users / Audience / Community

**Primary:** The cycle's own members (MFA and BA students in DESMA 596/199).
The scores are designed to be performed by a small group (5–12 people) who
commit to regular participation.

**Secondary:** AC's existing community (~2800 registered handles). Anything
built during the cycle would be immediately live on the public platform.
Existing AC users could discover and join the scores organically.

**Tertiary:** Creative computing educators and students elsewhere. The "Write a
Score" documentation would be a reusable framework for anyone running a creative
computing workshop or class using AC as infrastructure.

---

## 3. Why This Cycle

Aesthetic Computer has been in development since 2021, growing from No Paint
(2020, discussed on Hacker News) into a full runtime with 351 pieces, a Lisp
dialect, multiplayer, chat, and a handle system. The technical infrastructure
is mature. What it needs now is *social composition* — deliberate experiments
in how people use this system together.

Casey's framing of "scores for social software" maps directly onto how I already
think about AC. The SCORE.md file in the repository literally uses the metaphor
of a musical score to organize the project. AC's interface is designed to
function like a musical instrument — users discover memorizable paths, build
literacy through play, and eventually improvise and compose.

This cycle offers:

- **A committed cohort** to test social features that need real human
  participation to evaluate (you can't A/B test a conversation)
- **Critical feedback** from Casey and peers on the social design, not just the
  technical architecture
- **A structured timeframe** (10 weeks) that creates urgency and rhythm —
  exactly what a score needs to be performed
- **Cross-pollination** with other participants' social software projects,
  creating a richer discourse than working in isolation

I'm also interested in this cycle as a way to develop AC's role as
*educational infrastructure* — something I can bring to my own teaching practice
and share with other educators.

---

## 4. Practice Description

I'm Jeffrey Scudder (@jeffrey), an artist, educator, and software developer.
I direct Aesthetic Computer (https://aesthetic.computer), an open-source
creative computing platform and social network.

My practice centers on building software as a medium for art and education.
Before AC, I created No Paint (nopaint.art, 2020), a pixel art tool that was
discussed on Hacker News and used by a community of non-technical artists who
learned computing through contributing to the software they loved.

AC extends this into a full platform: a mobile-first runtime where anyone can
write, publish, and share interactive programs. It includes KidLisp (a Lisp
dialect for generative art), real-time multiplayer, a chat system, and a social
handle system. The codebase is open source with ~78 API endpoints and has been
in continuous development for 4+ years.

I teach creative computing and have used AC as infrastructure in courses and
workshops. My interest in "social software" comes directly from watching
non-technical users learn computation through social participation in software
communities — first in No Paint, now in AC.

**Links:**
- Aesthetic Computer: https://aesthetic.computer
- GitHub: https://github.com/whistlegraph/aesthetic-computer
- No Paint: https://nopaint.art
- Notepat on HN: https://news.ycombinator.com/item?id=41526754

---

## Technical Stack for PDF Generation

- **Content source:** This report.md
- **PDF generation:** Puppeteer (already in project dependencies) rendering a
  styled HTML template to PDF
- **Script:** `sosoft/generate-pdf.mjs` — converts the HTML to a print-quality
  PDF with proper typography
- **Output:** `sosoft/proposal.pdf`

To generate: `node sosoft/generate-pdf.mjs`
