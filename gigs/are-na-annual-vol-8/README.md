# Are.na Annual Vol. 8 — pitch

Pitch package for Are.na's Annual Vol. 8, themed **"Score."**

- **Status:** **Rejected 2026-05-08** by Meg Miller (editorial director, are.na). Channel + public pitch page stay live; track for Vol. 9.
- **Call:** [Open Call for Pitches — Are.na Annual Vol. 8](https://www.are.na/editorial/open-call-for-pitches-for-the-are-na-annual-vol-8)
- **Submission form:** https://aredotna.notion.site/3178a0f816d9815abdf3cb1624bb9e88
- **Deadline:** Monday, April 20, 2026 — 11:59pm EST
- **Honorarium:** $200 (published pieces, book releases December 2026)
- **Submitted channel:** [Self-Teaching Scores](https://www.are.na/aesthetic-computer/self-teaching-scores) — 68 blocks

## Pitch (tightened, ~170 words)

> **Channel:** *Self-Teaching Scores* — whistlegraphs alongside Cardew's *Treatise*, Cage's *Fontana Mix*, shape-note hymnals, Fluxus event scores, skateboard lines.
>
> I want to write about whistlegraph, a drawing form I invented in 2019 where every mark is a sung syllable. Between 2019 and 2023 it reached 2.6 million TikTok followers with no paid promotion and no trend-jacking. The distribution model was the form itself: a score legible enough that watching, learning, and performing collapse into a single gesture.
>
> The essay moves through three registers. As **art**, whistlegraph sits downstream of Cardew and Cage but refuses interpretation in favor of one-to-one legibility. As **content**, it proves a drawing can carry the viral mechanics of a dance challenge. As **interface design**, it became the founding principle of aesthetic.computer — every piece a self-documenting score, every URL a memorizable performance.
>
> What I want to work out: why *reproducibility*, not novelty, is the real score of a form — and what it would mean to design more things this way, objects whose instructions and performance are the same object.

## Thesis in one line

**The score teaches you how to play it.** Every mark is a sung syllable; watching, learning, and performing collapse into a single gesture.

## Why this angle

The strongest candidate from the AC monorepo for the "score" prompt. Already-extant material that supports the essay:

- [papers/arxiv-whistlegraph/whistlegraph.tex](../../papers/arxiv-whistlegraph/whistlegraph.tex) — the triad *art / content / interface* is already argued.
- `disks/whistlegraph.mjs` / `whistlegraph-composer.mjs` — the practice site.
- 2019 → 2023 → Rhizome/New Museum → Feral File → aesthetic.computer — five paragraphs of narrative spine.

## Channel construction

The channel is organized bottom-up (scroll reading order) in ten sections:

1. **Viral / social kin** — TikTok dances, memes, pictograms.
2. **Instructional / craft** — knitting, origami, sewing, IKEA, LEGO, recipes, tea ceremony.
3. **Body / movement notation** — Labanotation, Eshkol–Wachman, Benesh, kata, football plays.
4. **Sport as score** — skateboard lines, Z-Boys, surf breaks, yardage books, parkour.
5. **Vernacular / folk notation** — shape note, Sacred Harp, tablature, neumes, jianpu, gongche, sargam, gahu.
6. **Fluxus & event scores** — Yoko Ono, George Brecht, Dick Higgins, Alison Knowles, La Monte Young anthology.
7. **20th-century graphic scores (the canon)** — Cardew, Cage, Earle Brown, Feldman, Wolff, Ligeti/Wehinger, Xenakis, Oliveros, Lucier, Riley, Young.
8. **Computational / card-sized kin** — AC prompt, Notepat, Bitsy, PICO-8, Dwitter, demoscene, Inform 7.
9. **Framing text blocks** — three one-liners that land near the top.
10. **Whistlegraph** — lands on top: TikTok channel, aesthetic.computer page, Feral File editions, Rhizome/New Museum commission, closing text.

Full per-block list with descriptions: [channel-blocks.md](channel-blocks.md).

## Reproducing the channel

Two scripts, both auth via `ARENA_TOKEN` env var. A personal access token is created at https://dev.are.na/oauth/applications → any app → "Access Token." The OAuth code-exchange path used to originally mint this token is described in [reference_arena.md](../../../../../../.claude/projects/-Users-jas-aesthetic-computer/memory/reference_arena.md) (local auto-memory, not in the repo).

- [seed-channel.mjs](seed-channel.mjs) — posts 68 blocks to `self-teaching-scores` in reverse reading order (so whistlegraph lands on top).
- [set-descriptions.mjs](set-descriptions.mjs) — walks the channel and PUTs a per-block description from the lookup map.

```sh
ARENA_TOKEN=... node seed-channel.mjs
ARENA_TOKEN=... node set-descriptions.mjs
```

## Submission checklist

- [x] Channel live and public (68 blocks, all links annotated).
- [x] Channel description set (via web UI — API doesn't persist it).
- [x] Other personal channels set to private so the submission reads as a focused profile.
- [x] Credentials stashed in `aesthetic-computer-vault/.env` (encrypt with `fish vault-tool.fish lock`).
- [ ] Notion submission form filled: pitch paragraphs + channel URL.

## API notes (gotchas from building this)

- v2 `/me` returns **410 Gone**. v2 channel/block endpoints still work.
- OAuth **Client ID** ≠ Personal Access Token. A Client ID will read public data but 401 on writes. You need the "Access Token" shown on the app page at dev.are.na/oauth/applications, *or* do a full `code` → `/oauth/token` exchange.
- `PUT /v2/channels/:slug` accepts the `description` field with a 200 response but doesn't persist it — set via the web UI.
- Free-tier channels are capped at 200 blocks. 68 fits comfortably.
