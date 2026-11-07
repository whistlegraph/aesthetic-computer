# nmsearch

A search engine whose whole identity is its boundary. Type `bats` and you get
bats inside new media: the net art, the media-archaeology papers, the festival
works, the mailing-list threads, the Monoskop page. Never the animal.

nmsearch is the platter's front door for the field, the way [`sleuth/`](../sleuth/)
is its back door for a research pass. Sleuth tells an agent where to look.
nmsearch lets a person look.

## The field is a list

General engines decide relevance after the fact with ranking. nmsearch decides
it up front with membership: a hit is in the field because its *source* is in
the field. The source list is [`field.json`](field.json). Growing the engine
means growing the list, and every entry carries the same four facts the sleuth
asks for: where it lives, how it is searched, what it covers, and what evidence
a hit should keep.

Sources come in three tiers:

| Tier | What it means | Examples |
|---|---|---|
| **live** | A public API answers a browser directly, so the page fans the query out and shows hits inline | Wikipedia (category-scoped), Wikidata (genre-scoped), Monoskop, OpenAlex (Leonardo journals), Zenodo (NIME), Internet Archive (Rhizome DC, XFR STN, UbuWeb) |
| **ramp** | The archive searches well but will not answer cross-origin, so the page hands the query over with one click | Rhizome, ArtBase, ADA, ISEA archives, ELMCIP, Media Art Net, ZKM, Ars Electronica, transmediale, nettime |
| **local** | Our own platter: papers, sub-platters, hosted PDFs, mediascholar | this repository |

A ramp becomes live when a lith-side proxy for it is written. A live source
becomes indexed when we start keeping its records ourselves. That is the whole
roadmap, and it is a loop, not a ladder.

## Four roles

The field has no shared front door. Rhizome has ArtBase, Monoskop has its wiki,
Wikipedia has categories that stop at the first hop, and every festival keeps
its own archive. AC is in a position to sit between them, and the position has
four jobs:

1. **Validator.** A hit is only a lead until someone opens the primary page and
   records a stable identifier. nmsearch inherits the sleuth rule: results show
   the canonical URL, the source's own ID, and the date seen. The engine should
   be able to say *this exists and here is the page that proves it.*
2. **Intaker.** A door for things that are not yet anywhere: a work, a person,
   an event, a text. Intake records provenance first and publishes second.
3. **Archive.** Snapshot plus checksum for what we validate, so a dead link is
   a lead and not a loss. Internet Archive is the first archive, not the only.
4. **Exit ramp.** The canonical long-term home of a fact is Wikidata, then
   Wikipedia, then Rhizome ArtBase and Monoskop. nmsearch routes outward. When
   a validated record has no Wikidata item, that is the intake queue's output:
   a QID to create, with sources attached.

The spine is Wikidata. Every validated entity gets or is matched to a QID, and
the QID is what lets Rhizome, Monoskop and Wikipedia agree they are talking
about the same bat.

## Pages

- `papers.aesthetic.computer/nmsearch/` — the engine. One input. Three lanes:
  **platter** (local, instant), **field** (live fan-out, grouped by source),
  **ramps** (one-click handoff to every in-field archive).
- `?q=bats` is the permalink.

## Build

```
node papers/nmsearch/build.mjs
```

Reads `field.json` and the local platter (the papers table in `../SCORE.md`,
sub-platter READMEs, hosted PDFs) and writes `field.json` plus `local.json`
into `system/public/papers.aesthetic.computer/nmsearch/`. Run it whenever a
source is added or a paper lands. Deploy rides the normal lith deploy.

## Not yet

- Intake form (role 2) and the QID queue it feeds.
- lith proxies for ramps that have APIs but no CORS (ArtBase is a Wikibase;
  ELMCIP is Drupal with JSON views; ADA and ISEA are HTML only).
- Our own index: fetch and store records from live sources on a schedule, so
  nmsearch keeps working when a source does not.
- Category depth on Wikipedia: `incategory` stops at direct members. A
  PetScan-style deep category walk belongs in the indexer, not the page.
- OpenAlex calls share one anonymous daily budget per IP. An API key goes in
  the lith proxy, never in the page.

## Opened

2026-11-06, from a conversation that began "could be a great tool."
