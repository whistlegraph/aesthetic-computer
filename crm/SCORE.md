# CRM — Linked Open Data for Aesthetic Computer

**Status:** Stage 1 implemented (serializer + endpoint + landing, on `main`) ·
**Owner:** @jeffrey · **Drafted:** 2026-06-28

**Stage 1 ships in these files:**
- `system/backend/linked-art.mjs` — pure Linked Art serializers (person/painting/piece/mood) + license registry.
- `system/netlify/functions/crm.mjs` — endpoint: path routing, Stage-0 consent gate, content negotiation, landing, VoID, 501 SPARQL stub.
- `system/tests/linked-art.test.mjs` — `node --test system/tests/linked-art.test.mjs` (6 passing).
- `lith/server.mjs` — host rewrite `data.aesthetic.computer/* → /api/crm/*`.
- `lith/Caddyfile` — `@data` host block → lith.

**To go live:** (1) create the Cloudflare `data` record in the `aesthetic.computer`
zone → lith origin `209.38.133.33` (add to `lith/DNS.md`); (2) deploy lith
(`fish lith/deploy.fish`); (3) opt a handle in:
`db['@handles'].updateOne({handle:'sat'}, {$set:{linkedData:{enabled:true, license:'CC-BY-4.0', optInAt:new Date()}}})`.
Until a handle opts in, every entity correctly 404s. In dev, `?preview=CC-BY-4.0`
bypasses the gate to validate serialization without touching live data.

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

## 5. Consent & rights — **the gate (Stage 0, non-negotiable)**

Getty pipelines *reject objects with no rights statement*, and broadcasting a
user's full corpus to the permanent semantic web demands explicit, revocable
consent. Mirror the Bluesky allowlist exactly:

- Add `@handles.linkedData = { enabled: bool, license: "CC-BY-4.0" | "CC0-1.0" | ... , optInAt: Date }`
  (or a `secrets.crm.handles` allowlist for a curated v1).
- **Nothing without a license.** No flag → entity 404s for `ld+json` and is
  absent from SPARQL.
- Revocation removes triples from the store and flips `ld+json` to 410 Gone.
- Privacy floor: never emit email, Auth0 `sub`, IP/boot telemetry, chat, or
  `verifications`. Only public handle + the four content types above.

A `prompt`-driven command (`linkdata on cc-by` / `linkdata off`) is the natural
user-facing toggle, parallel to how moods are posted.

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

### Stage 2 — SPARQL endpoint
- Stand up **[Oxigraph](https://github.com/oxigraph/oxigraph)** (Rust, single
  binary — runs on **lith** at `localhost:7878`; the `@data` block already
  proxies `/sparql` to it).
- ETL worker feeds it from the **existing `_firehose` change stream** (30-day
  TTL already in Mongo): on insert/update of an opted-in painting/piece/mood,
  map → triples → `INSERT`/`DELETE` in the store. This is the literal analog of
  the outbound Bluesky worker.
- Publish a VoID dataset description at `data.aesthetic.computer/.well-known/void`.

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

**Still open:**
1. **v1 allowlist vs. self-serve** — curated `secrets.crm.handles` to start
   (like Bluesky's `mirrorHandles`), open the `linkdata` command later.
2. **License menu** — which CC licenses to offer; default per entity type
   (paintings CC BY, pieces CC0?).
3. **AAT term review** — confirm the chosen Getty AAT ids with a CRM-literate
   reviewer before going public.

---

## 9. Build order

1. ✅ **Stage 0 + serializer lib** — `linkedData` consent gate (in `crm.mjs`) +
   `linked-art.mjs` with all four mappers + `node --test` fixtures (6 passing).
2. ✅ **Stage 1** — `@data` Caddy block + lith host rewrite + `crm.mjs` serving
   Linked Art + inline landing + VoID. *Pending:* Cloudflare `data` record +
   deploy + external validation against linked.art tooling.
3. ⬜ **Stage 2** — Oxigraph on lith (`localhost:7878`) + `_firehose` ETL +
   `/sparql` + flip the Caddy `/sparql*` handle to `:7878`.
4. ⬜ **Stage 3** — IIIF + sitemap + docs with a worked Getty-style federated query.
