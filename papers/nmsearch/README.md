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

The AC Platter is searched locally. External sources use one of three routes.

| Route | What a search returns |
|---|---|
| Indexed | Rhizome ArtBase artwork titles, artist credits, dates and ArtBase IDs from a February 2022 snapshot |
| Live | Inline results from Monoskop, ISEA Archives, Ars Electronica Festival 2024, Wikipedia, Wikidata, OpenAlex's Leonardo journals, Zenodo's NIME community and Internet Archive |
| Archive link | The query opens at another source such as ELMCIP, Rhizome editorial or ZKM |

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

## Search

Open [nmsearch](https://papers.aesthetic.computer/nmsearch/) or share a query such as [bats](https://papers.aesthetic.computer/nmsearch/?q=bats). Results arrive by source. Use the source selector to focus on one collection. **Source status** lists empty searches and unavailable services. **Search other archives** opens external search pages.

## Build

```sh
node papers/nmsearch/build.mjs
```

The build reads the published papers table, the links and reading lists in `platter.html`, the score's sub-platters, Sleuth and hosted PDFs. It writes the searchable metadata to `system/public/papers.aesthetic.computer/nmsearch/`. The regular `node papers/sync-platter.mjs` refresh also runs this build.

To reproduce the ArtBase snapshot, run `python3 papers/nmsearch/import-artbase.py`. The importer keeps titles, creators, years and ArtBase IDs from the pinned *Net Art Revolution?* artwork table. Each result links to its Rhizome record. The snapshot dates to February 14, 2022. Data preparation by Margherita Donelli, Chiara Catizone and Laurent Fintoni. [Source, credits and licensing](https://github.com/laurentfintoni/Net-Art-Rev/tree/abb03fde210df5da4ca473287d05f01f968af67d)

## Not yet

- Intake form (role 2) and the QID queue it feeds.
- A current ArtBase feed beyond the 2022 research snapshot.
- Fetch and store records from live sources on a schedule, so
  nmsearch keeps working when a source does not.
- Category depth on Wikipedia: `incategory` stops at direct members. A
  PetScan-style deep category walk belongs in the indexer, not the page.
- OpenAlex calls share one anonymous daily budget per IP. An API key goes in
  the lith proxy, never in the page.

## Opened

2026-11-06, from a conversation that began "could be a great tool."
