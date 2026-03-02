# Aesthetic Computer: Open Development

## A Platform Submitted for Dialogue
### Score for Social Software — Cycle 2 Proposal
### Jeffrey Scudder / @jeffrey / DESMA 596/199 / March 2026

---

## The Score

I'm submitting Aesthetic Computer (AC) — the open-source creative computing
platform I've been building since 2021 — as a social software project in active
development. I'm not proposing to build a new thing for this cycle. I'm bringing
the thing I'm already building into the room for dialogue, feedback, and critical
exchange as I develop it alongside its community.

AC is a mobile-first runtime and social network where users write, publish, and
share small interactive programs called *pieces*. It has real-time chat, @handles
with custom colors, ephemeral status updates (moods mirrored to Bluesky via
ATProto), a pixel painting system, a built-in Lisp dialect (KidLisp), multiplayer
WebSocket sessions, and user profiles that track creative output. The codebase is
open source with 4+ years of continuous development.

The score: I develop AC in the open over 10 weeks, sharing what I'm working on,
what decisions I'm facing, and what the community is doing. The cohort engages as
users and as critics — trying the tools, reading the design choices, and giving
me feedback I can't get from inside the project.

## What I'm Looking For

- What feels inviting and what feels opaque when you first encounter the platform?
- Which social features sustain participation vs. which are technically impressive
  but socially inert?
- How does the "instrument" metaphor land for people who aren't already invested?
- What would you want to do on AC that you currently can't?
- Where does the design accidentally exclude the people it claims to welcome?

## What the Cohort Gets

- **A live codebase** — open source, documented, changing week to week
- **The dev process** — I work with Claude Code, Cursor, VS Code, and maintain a
  living SCORE.md; the process is as legible as the product
- **Platform accounts** — everyone gets an @handle, can paint, chat, set moods,
  write KidLisp, publish pieces
- **Real community data** — 2,800 handles, 18k chat messages, 4,400 paintings,
  16k KidLisp programs

## Why This Cycle

Casey's framing of "scores for social software" maps directly onto how I think
about AC. The platform's SCORE.md uses the metaphor of a musical score to
organize the project. The interface is designed like an instrument — users
discover memorizable paths, build literacy through play, and eventually
improvise. But I've been composing alone. This cycle is a chance to compose in
conversation.

The technical infrastructure is mature. The question now is about social design:
how do the features I've built actually shape the way people relate to each other
and to creative computing? That's best answered through dialogue with people
thinking critically about social software.

## Practice

I'm an artist, educator, and software developer. Before AC, I created No Paint
(2020), a pixel art tool whose community taught me how people learn computing
through social participation. AC extends that into a full platform: anyone can
write, publish, and share interactive programs at a URL. I teach creative
computing and have used AC as infrastructure in courses and workshops. I'm
bringing this not as a finished project but as an ongoing practice I want to
develop through critical exchange.

**Links:**
- Aesthetic Computer: https://aesthetic.computer
- GitHub: https://github.com/whistlegraph/aesthetic-computer
- No Paint: https://nopaint.art
- Notepat on HN: https://news.ycombinator.com/item?id=41526754

---

## Appendix A: The Network (March 2, 2026)

| Metric | Count |
|--------|-------|
| @handles | 2,798 |
| Chat messages | 18,016 |
| Paintings | 4,392 |
| Moods | 2,900 |
| KidLisp programs | 16,174 |
| Published pieces | 265 |
| Clocks | 333 |
| Tapes | 102 |
| Boot events | 93,122 |

**Who makes things:** 1,067 have painted · 997 have posted moods ·
59 have written KidLisp · 19 have published pieces

## Appendix B: Technical Stack

- **Frontend:** ES Modules, Canvas 2D + WebGL2, WebSocket hot-reload, 351 pieces
- **System Server:** Netlify (~85 API endpoints, edge functions, Auth0, Stripe)
- **Session Server:** Fastify + Geckos.io, Jamsocket containers, Redis
- **Oven Service:** Express.js + FFmpeg, tape → MP4, screenshots
- **Workers:** Cloudflare (feed, grab, KV, Durable Objects)
- **Data:** MongoDB Atlas, Redis, DigitalOcean Spaces (S3 CDN), ATProto PDS
- **Language:** KidLisp — 118 built-in functions, 12 categories
- **Dev Tools:** Claude Code, Cursor, VS Code, esbuild, Jasmine, Vitest, Docker

## Appendix C: Sitemap

22 domains · ~355 disk routes · ~85 API endpoints · ~120 redirects

## PDF Generation

- **Source:** sosoft/proposal.html
- **Script:** `node sosoft/generate-pdf.mjs`
- **Output:** sosoft/proposal.pdf
