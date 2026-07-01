# CRM — Linked Open Data for Aesthetic Computer

**Status:** ✅ Stage 1 LIVE at https://data.aesthetic.computer (deployed
2026-06-29; **open to all public handles**, In-Copyright by default; `@jeffrey`
& `@fifi` upgraded to CC-BY-4.0) · **Owner:** @jeffrey · **Drafted:** 2026-06-28

Live & verified: landing recent-records feed (all public contributors),
`/@{handle}` (Person), `/painting/{code}` (image URLs resolve 200),
`/mood/{handle}/{rkey}`, `/.well-known/void`, content negotiation (JSON-LD vs.
HTML), and opt-out exclusion. Cloudflare `data` A record (→ 209.38.133.33,
proxied) created via vault token.

**Stage 1 ships in these files:**
- `system/backend/linked-art.mjs` — pure Linked Art serializers + license/rights registry (CC + rightsstatements.org InC).
- `system/netlify/functions/crm.mjs` — endpoint: path routing, `rightsFor` (open-by-default/In-Copyright, opt-out, CC upgrade), content negotiation, recent-records landing, VoID, 501 SPARQL stub.
- `system/tests/linked-art.test.mjs` — `node --test system/tests/linked-art.test.mjs` (7 passing).
- `lith/server.mjs` — host rewrite `data.aesthetic.computer/* → /api/crm/*`.
- `lith/Caddyfile` — `@data` host block → lith.

**Rights controls** (set by hand in `@handles` until a self-serve toggle lands):
- opt out: `{$set:{linkedData:{enabled:false}}}` → excluded entirely.
- CC upgrade: `{$set:{linkedData:{enabled:true, license:'CC-BY-4.0', optInAt:new Date()}}}`.
- default (no flag) → In Copyright. In dev, `?preview=CC-BY-4.0` forces a license for validation.

Expose AC users' work as **dereferenceable linked data** so cultural-heritage
researchers (Getty, museums, the American Art Collaborative) can discover,
cite, and federate over it — e.g. a Getty researcher querying *"all digital
paintings produced by @sat in 2026"* against AC's endpoint joined with their
own collection.

This is the **research/archival sibling of the Bluesky mirror**. Bluesky
broadcasts moods to a social firehose; this broadcasts *all* opted-in work to
the semantic web. Same architecture (opt-in allowlist, dual-channel, stable
external identity), different audience and ontology.

---

## 1. Precedent in the repo — the Bluesky pattern

`system/backend/bluesky-mirror.mjs` is the template we are deliberately copying:

- **Opt-in allowlist** — only handles in `secrets.bluesky.mirrorHandles` broadcast.
- **Dual-channel** — a mood lives in Mongo *and* gets a stable external
  identity (`atproto.rkey`, `bluesky.uri`) plus a public permalink
  (`https://aesthetic.computer/moods~{handle}~{rkey}` — `bluesky-mirror.mjs:80`).
- **Pull-back** — engagement is re-fetched from a public API
  (`bluesky-engagement.mjs`).

CRM keeps all three properties. The "external identity" becomes an RDF URI;
the "broadcast target" becomes a SPARQL store + JSON-LD content negotiation.

---

## 2. Strategic decision: target **Linked Art**, a profile of CIDOC CRM

CIDOC CRM (ISO 21127) is the *ontology*. But Getty and peer institutions do
not ingest raw CRM RDF in practice — they consume **[Linked Art](https://linked.art)**,
a JSON-LD profile of CIDOC CRM with fixed property names (`produced_by`,
`carried_out_by`, `classified_as`) and Getty AAT vocabulary terms.

> **Decision:** emit **Linked Art JSON-LD** as the primary representation, with
> the **CIDOC CRM** classes underneath. A SPARQL endpoint exposes the same
> triples. "Through CIDOC CRM" is satisfied by Linked Art; raw-CRM purists can
> still query the SPARQL store.

Why this matters: Linked Art is what their pipelines actually parse, and it
gives us an off-the-shelf `@context`, validators, and example consumers.

---

## 3. URI strategy — a dedicated identity domain: `data.aesthetic.computer`

**Decision:** canonical linked-data identifiers live on **`data.aesthetic.computer`**,
mirroring Getty's own `data.getty.edu` and Europeana's `data.europeana.eu`. A
dedicated domain is the cultural-heritage convention precisely because the
identifier must resolve *forever* while the public app (`aesthetic.computer`)
keeps changing. Each identity URI carries a `subject_of`/`equivalent` link back
to the human-facing page on the apex, so the two never drift.

| Entity | Canonical identity URI | Links to (human page) | Source of truth |
|---|---|---|---|
| Person | `https://data.aesthetic.computer/@sat` | `aesthetic.computer/@sat` | `@handles`, `users` |
| Painting | `https://data.aesthetic.computer/painting/{code}` | `aesthetic.computer/painting/{code}` | `paintings` (image at `/media/@sat/painting/{slug}.png`) |
| Piece (KidLisp/.mjs) | `https://data.aesthetic.computer/piece/{code}` | `aesthetic.computer/{$code}` | `pieces` / `kidlisp` |
| Mood | `https://data.aesthetic.computer/mood/{handle}/{rkey}` | `aesthetic.computer/moods~{handle}~{rkey}` | `moods` (`atproto.rkey`) |

Dereferencing rule on `data.aesthetic.computer/{path}`: default (or
`Accept: application/ld+json`, `?format=jsonld`) → **Linked Art JSON-LD**;
`Accept: text/html` → a small human-readable view that also `<link rel="seeAlso">`s
the apex page. `data.aesthetic.computer/sparql` and `/.well-known/void` complete
the hub.

> The JSON-LD examples in §4 still show `aesthetic.computer/...` ids for
> readability — read them as `data.aesthetic.computer/...` per this table.

---

## 4. Entity mapping — CIDOC CRM classes + Linked Art shape

All four entities attribute back to the **Person** node, which is the hub.

### 4.1 Person — `@sat`  → `E21 Person`

```json
{
  "@context": "https://linked.art/ns/v1/linked-art.json",
  "id": "https://aesthetic.computer/@sat",
  "type": "Person",
  "_label": "@sat",
  "identified_by": [
    { "type": "Name", "content": "@sat",
      "classified_as": [{ "id": "http://vocab.getty.edu/aat/300404670", "_label": "primary name" }] }
  ],
  "subject_of": [
    { "type": "LinguisticObject", "_label": "AC profile",
      "digitally_carried_by": [{ "type": "DigitalObject",
        "access_point": [{ "id": "https://aesthetic.computer/@sat" }] }] }
  ]
}
```
- Latest mood can ride along as a `subject_of` LinguisticObject.
- **No email, Auth0 sub, or `verifications` data is ever emitted.** Only the
  public handle and color identity.

### 4.2 Painting → `E22 Human-Made Object` / `D1 Digital Object`

Production event `E12 Production` carries the authorship + time-span.

```json
{
  "@context": "https://linked.art/ns/v1/linked-art.json",
  "id": "https://aesthetic.computer/painting/Abc123",
  "type": "DigitalObject",
  "_label": "painting Abc123 by @sat",
  "classified_as": [
    { "id": "http://vocab.getty.edu/aat/300033973", "_label": "paintings (visual works)" },
    { "id": "http://vocab.getty.edu/aat/300312038", "_label": "digital images" }
  ],
  "produced_by": {
    "type": "Production",
    "carried_out_by": [{ "id": "https://aesthetic.computer/@sat", "type": "Person" }],
    "timespan": { "type": "TimeSpan", "begin_of_the_begin": "2026-03-15T00:00:00Z" },
    "took_place_at": [{ "id": "https://aesthetic.computer", "type": "Place", "_label": "Aesthetic Computer" }]
  },
  "subject_to": [{ "type": "Right",
    "classified_as": [{ "id": "https://creativecommons.org/licenses/by/4.0/", "_label": "CC BY 4.0" }] }],
  "digitally_shown_by": [{ "type": "DigitalObject", "format": "image/png",
    "access_point": [{ "id": "https://aesthetic.computer/media/@sat/painting/Abc123.png" }] }]
}
```
Source fields: `paintings.code`, `.slug`, `.when`, `.user` (→ resolved to handle).

### 4.3 Piece (KidLisp / .mjs) → `E73 Information Object` + `E65 Creation`

Generative-art provenance — a novel, valuable contribution to the CRM graph.
The *source code itself* is a `LinguisticObject`; the running program is the
`DigitalObject`. Hash gives content-addressed identity.

```json
{
  "@context": "https://linked.art/ns/v1/linked-art.json",
  "id": "https://aesthetic.computer/$cow",
  "type": "DigitalObject",
  "_label": "KidLisp piece $cow by @sat",
  "classified_as": [
    { "id": "http://vocab.getty.edu/aat/300265727", "_label": "software" },
    { "id": "http://vocab.getty.edu/aat/300047090", "_label": "generative art" }
  ],
  "created_by": {
    "type": "Creation",
    "carried_out_by": [{ "id": "https://aesthetic.computer/@sat", "type": "Person" }],
    "timespan": { "type": "TimeSpan", "begin_of_the_begin": "2026-03-15T00:00:00Z" }
  },
  "identified_by": [{ "type": "Identifier", "content": "<sha256>",
    "classified_as": [{ "id": "http://vocab.getty.edu/aat/300435704", "_label": "checksum" }] }],
  "subject_to": [{ "type": "Right",
    "classified_as": [{ "id": "https://creativecommons.org/publicdomain/zero/1.0/", "_label": "CC0" }] }],
  "carries": [{ "type": "LinguisticObject", "content": "(wipe blue) (ink red) ...",
    "classified_as": [{ "id": "http://vocab.getty.edu/aat/300028676", "_label": "source code" }] }]
}
```
Source fields: `pieces.code`, `.source`, `.hash`, `.when`, `.user`, `.trustLevel`
(only `trusted` pieces broadcast — natural spam gate).

### 4.4 Mood → `E33 Linguistic Object` + `E65 Creation`

Easiest map; already has `atproto.rkey` permalink and a Bluesky cross-ref we
can surface as `equivalent`/`subject_of`.

```json
{
  "@context": "https://linked.art/ns/v1/linked-art.json",
  "id": "https://aesthetic.computer/moods~sat~3k...",
  "type": "LinguisticObject",
  "_label": "mood by @sat",
  "classified_as": [{ "id": "http://vocab.getty.edu/aat/300026032", "_label": "statements" }],
  "content": "feeling generative today",
  "language": [{ "id": "http://vocab.getty.edu/aat/300388277", "_label": "English" }],
  "created_by": {
    "type": "Creation",
    "carried_out_by": [{ "id": "https://aesthetic.computer/@sat", "type": "Person" }],
    "timespan": { "type": "TimeSpan", "begin_of_the_begin": "2026-03-15T00:00:00Z" }
  },
  "subject_of": [{ "type": "DigitalObject",
    "access_point": [{ "id": "at://did:plc:.../app.bsky.feed.post/..." }] }]
}
```
Source fields: `moods.mood`, `.when`, `.user`, `.atproto.rkey`, `.bluesky.uri`.
Skip `deleted:true`.

---

## 5. Rights — open records by default, In Copyright (the museum model)

The work shown here is *already public* on aesthetic.computer; a linked-data
record is a catalog entry describing it (authorship, date, type, image link) —
exactly what museums publish for living artists' in-copyright work. So the model
is **open by default**, not an opt-in allowlist:

- Every public handle's records are live, marked **In Copyright**
  (`http://rightsstatements.org/vocab/InC/1.0/` — the Europeana/DPLA standard):
  the record is open + citable, but copyright and the right to sell stay with
  the artist. This is what Getty pipelines need (a rights statement) without us
  granting reuse rights on anyone's behalf.
- `@handles.linkedData = { enabled, license, optInAt }` is now an **upgrade /
  opt-out** flag, not a gate:
  - `enabled === false` → excluded entirely (opt-out).
  - `license` is a CC key (`CC-BY-4.0`, `CC0-1.0`, …) → that open license.
  - otherwise (default) → In Copyright.
- Why not default to CC-BY: CC-BY grants *third parties* commercial reuse, which
  would undercut artists' own ability to sell. In Copyright preserves it.
- Privacy floor: never emit email, Auth0 `sub`, IP/boot telemetry, chat, or
  `verifications`. Only public handle + the four content types above.

A `prompt`-driven command (`linkdata cc-by` / `linkdata off`) is the natural
self-serve toggle later; for now it's set by hand in `@handles`.

---

## 6. The `data.aesthetic.computer` subdomain — wired like `at.aesthetic.computer`

The AT subdomain is the exact template (three layers):

1. **Landing page** — `at/landing-page.html` is a human-facing "here's what we
   offer" page (mission + top users + media feed). LOD gets the sibling
   `crm/landing-page.html` → served as static `root` for `data.aesthetic.computer/`:
   what linked open data is, the license, a live SPARQL box, and an example
   Getty-style federated query.
2. **Backend service reverse-proxied behind it** — the knot does
   `reverse_proxy localhost:5555` (`at/knot/infra/Caddyfile`). LOD does the same
   for Oxigraph: `data.aesthetic.computer/sparql → reverse_proxy localhost:7878`,
   and `/painting/* /piece/* /mood/* /@*` → `reverse_proxy localhost:8888`
   (the `crm.mjs` serializer on lith).
3. **Cloudflare DNS record + a `lith/Caddyfile` host block.** New `data` record
   in the `aesthetic.computer` zone (add to `lith/DNS.md`), and a block mirroring
   the existing `@give`/`@papers` pattern:

```caddyfile
@data host data.aesthetic.computer
handle @data {
    handle /sparql* { reverse_proxy localhost:7878 }      # Oxigraph
    handle /.well-known/void { reverse_proxy localhost:8888 }
    handle /painting/* { reverse_proxy localhost:8888 }   # crm.mjs serializer
    handle /piece/*    { reverse_proxy localhost:8888 }
    handle /mood/*     { reverse_proxy localhost:8888 }
    handle /@*         { reverse_proxy localhost:8888 }
    handle {                                               # static landing
        root * /opt/ac/system/public/data.aesthetic.computer
        file_server
    }
}
```

Stage 1 only needs layers 1 + 3 + the `localhost:8888` routes (no Oxigraph);
Stage 2 adds the `localhost:7878` proxy. Same staging as below.

## 7. Architecture — staged rollout

### Stage 1 — Subdomain + content negotiation → Linked Art (cheapest, no triple store)
- `data` DNS record + `@data` Caddy block (§6) + static `crm/landing-page.html`.
- New function `system/netlify/functions/crm.mjs` (routes `/painting/{code}`,
  `/piece/{code}`, `/mood/{handle}/{rkey}`, `/@{handle}`) + a serializer lib
  `system/backend/linked-art.mjs` (`paintingToLinkedArt`, `pieceToLinkedArt`,
  `moodToLinkedArt`, `personToLinkedArt`).
- Reuses current Mongo indexes (`paintings.code`, `pieces.code`,
  `moods.atproto.rkey`). **Pure serializer over data we already have. No triple
  store, no new process — just lith.**
- Validate output against linked.art tooling + a JSON-LD playground.

### Stage 2 — SPARQL endpoint ✅ LIVE
- **[Oxigraph](https://github.com/oxigraph/oxigraph)** v0.5.9 (single Rust
  binary at `/opt/oxigraph`) runs on **lith** bound to `127.0.0.1:7878` under
  `oxigraph.service`; Caddy proxies only `data.aesthetic.computer/sparql → /query`
  (read-only — `/update` + `/store` are never exposed; verified 404 publicly).
- **ETL** (`crm/build-graph.mjs`): walks MongoDB → runs every entity through the
  same serializers → expands to CIDOC CRM N-Triples via `rdf.mjs` (bundled
  linked.art context, offline) → atomic Graph-Store PUT replacing the default
  graph. `oxigraph-sync.timer` rebuilds every 30 min. Provisioned by
  `crm/deploy-sparql.fish`. (A full rebuild is ~15 min for ~7.7k entities; a
  `_firehose`-driven incremental update is a future optimization.)
- **Verified live:** 163,994 triples; authorship exact vs. Mongo ground truth
  (@jeffrey → 409 paintings = 409; distinct productions 2238 = handled-user
  paintings 2238). Example query on the landing page + VoID `void:sparqlEndpoint`.
- **Two bugs found + fixed during rollout:** (1) `jsonld.toRDF` restarts blank
  labels at `_:b0` per call → concatenation merged every work's production/
  time-span into one node → skolemize per doc; (2) the Mongo driver kept the
  event loop alive so the oneshot never exited (hung `activating`, blocking the
  timer) → `process.exit(0)` on success.

### Stage 3 — Discovery & images
- Sitemap of entity URIs; `robots`/`llms.txt` note pointing crawlers to the LOD.
- **IIIF Image API** over the painting CDN — Getty's image tooling is
  IIIF-native; this is high-leverage for paintings specifically.
- Optional: OAI-PMH feed for harvesters; federated-query examples in docs.

---

## 8. Decisions

**Resolved:**
- **Domain** → `data.aesthetic.computer` (mirrors `data.getty.edu`). ✓
- **Store** → Oxigraph on lith (`localhost:7878`). ✓
- **AAT terms** → all verified against the live Getty SPARQL endpoint
  (vocab.getty.edu) 2026-06-29. The first draft was almost entirely wrong
  (`300265727` "software" was actually *wanted posters*); corrected ids are now
  in `linked-art.mjs` and asserted in the test. No Getty term exists for "source
  code" or "checksum" — those carry unclassified. ✓

**Still open:**
1. **v1 allowlist vs. self-serve** — curated opt-in to start (per-handle
   `@handles.linkedData`, set by hand like now), open a `linkdata` prompt
   command later for self-serve.
2. **License menu** — which CC licenses to offer; default per entity type
   (paintings CC BY, pieces CC0?). `@jeffrey` is currently all CC-BY-4.0.

---

## 9. Build order

1. ✅ **Stage 0 + serializer lib** — `linkedData` consent gate (in `crm.mjs`) +
   `linked-art.mjs` with all four mappers + `node --test` fixtures (6 passing).
2. ✅ **Stage 1** — `@data` Caddy block + lith host rewrite + `crm.mjs` serving
   Linked Art + inline landing + VoID. *Pending:* Cloudflare `data` record +
   deploy + external validation against linked.art tooling.
3. ✅ **Stage 2** — Oxigraph on lith (`127.0.0.1:7878`) + `crm/build-graph.mjs`
   ETL + 30-min `oxigraph-sync.timer` + read-only `/sparql` + `void:sparqlEndpoint`.
4. ✅ **Stage 3** — `iiif.mjs` IIIF Presentation v3 Manifest + Image Level-0
   `info.json` per painting (dims from the PNG header; validates okay=0-errors on
   the official IIIF validator) + `/sitemap.xml` (7721 URIs) + Getty-federated
   query example. Landing redesigned (PALS beacon, "data" corner, auto light/dark,
   auto-scroll slideshow, Access Points as the pitch, footer link-backs). `data`
   routes from the AC prompt. All 2929 handles included (0 opt-outs).
