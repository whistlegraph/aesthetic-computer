// crm.mjs — Linked Open Data endpoint for data.aesthetic.computer
// Serves opted-in AC works as Linked Art (JSON-LD profile of CIDOC CRM) so
// cultural-heritage researchers (Getty, museums, the American Art Collaborative)
// can discover, cite, and federate over them. Read-side sibling of the Bluesky
// mirror. Full design: crm/SCORE.md.
//
// Routing (host data.aesthetic.computer is rewritten to /api/crm/* by lith):
//   /                       → landing page (HTML)
//   /@{handle}              → Person (E21)
//   /painting/{code}        → DigitalObject (E22)
//   /piece/{code}           → DigitalObject (E73)
//   /mood/{handle}/{rkey}   → LinguisticObject (E33)
//   /.well-known/void       → VoID dataset description
//   /sparql                 → 501 until Stage 2 (Oxigraph)
//
// Consent gate (Stage 0): a work is only exposed if its owning handle has
// opted in — `@handles.linkedData = { enabled: true, license: "CC-BY-4.0" }`.
// No opt-in (or no license) → 404, so we never leak the existence of private work.
// 2026.06.29

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { handleFor } from "../../backend/authorization.mjs";
import {
  LICENSES,
  RIGHTS_STATEMENTS,
  DEFAULT_RIGHTS,
  isLicense,
  personToLinkedArt,
  paintingToLinkedArt,
  pieceToLinkedArt,
  moodToLinkedArt,
  DATA_BASE,
  WEB_BASE,
} from "../../backend/linked-art.mjs";

const dev = process.env.CONTEXT === "dev" || process.env.NETLIFY_DEV === "true";
const JSONLD = "application/ld+json";

// Strip the function prefix to get the entity path segments.
// "/api/crm/painting/Abc123" → ["painting", "Abc123"]
function segmentsFrom(path) {
  let p = (path || "/").split("?")[0];
  p = p.replace(/^\/+/, "");
  if (p.startsWith("api/crm")) p = p.slice("api/crm".length);
  else if (p.startsWith("crm")) p = p.slice("crm".length);
  return p.split("/").map(decodeURIComponent).filter(Boolean);
}

// Resolve the rights a handle's works are published under.
//   • handle doesn't exist                 → null (nothing to show)
//   • explicit opt-out (enabled === false) → null (excluded by the artist)
//   • opted in with a CC license           → that license (open reuse)
//   • any other public handle (default)    → "InC" In Copyright: the record is
//     open and citable, but copyright + the right to sell stay with the artist.
async function rightsFor(database, handle) {
  if (!handle) return null;
  const doc = await database.db
    .collection("@handles")
    .findOne({ handle: handle.replace(/^@/, "") });
  if (!doc) return null;
  const ld = doc.linkedData;
  if (ld?.enabled === false) return null;
  const license = isLicense(ld?.license) ? ld.license : DEFAULT_RIGHTS;
  return { handle: doc.handle, license };
}

// In dev, ?preview=CC-BY-4.0 bypasses the gate so we can validate serialization
// without flipping live data. Never active in production.
function previewLicense(event) {
  if (!dev) return null;
  const v = event.queryStringParameters?.preview;
  if (v && isLicense(v)) return v;
  if (v === "1" || v === "true") return "CC-BY-4.0";
  return null;
}

// Prefer HTML only when the client clearly asks for it over JSON-LD.
function wantsHtml(event) {
  if (event.queryStringParameters?.format === "jsonld") return false;
  if (event.queryStringParameters?.format === "html") return true;
  const accept = event.headers?.accept || event.headers?.Accept || "";
  return accept.includes("text/html") && !accept.includes(JSONLD);
}

function jsonld(doc) {
  return respond(200, doc, { "Content-Type": JSONLD });
}

// A minimal human view that points at both the JSON-LD and the apex page.
function htmlView(doc, webUrl) {
  const body = `<!doctype html><meta charset=utf-8><title>${doc._label}</title>
<style>body{font-family:monospace;max-width:48em;margin:3em auto;padding:0 1em;color:#111}a{color:#cd5c9b}</style>
<h1>${doc._label}</h1>
<p>Linked Art (CIDOC CRM) identity on <b>data.aesthetic.computer</b>.</p>
<ul>
<li><a href="${doc.id}?format=jsonld">JSON-LD representation</a></li>
<li><a href="${webUrl}">View on Aesthetic Computer →</a></li>
</ul>
<pre>${JSON.stringify(doc, null, 2).replace(/</g, "&lt;")}</pre>`;
  return respond(200, body, { "Content-Type": "text/html; charset=utf-8" });
}

export async function handler(event) {
  if (event.httpMethod !== "GET") return respond(405, { message: "Method Not Allowed" });

  const segs = segmentsFrom(event.path);

  // SPARQL is Stage 2.
  if (segs[0] === "sparql") {
    return respond(501, { message: "SPARQL endpoint not yet available (Stage 2)." });
  }

  // VoID dataset description.
  if (segs[0] === ".well-known" && segs[1] === "void") return voidDoc();

  let database;
  try {
    database = await connect();

    // Landing page — lists the opted-in people and their records.
    if (segs.length === 0) return await landing(database);

    // Person: /@handle
    if (segs[0].startsWith("@")) {
      const handle = segs[0].slice(1);
      const consent = previewLicense(event) ? { handle, license: previewLicense(event) } : await rightsFor(database, handle);
      if (!consent) return respond(404, { message: "Not found" });

      const latestMood = await database.db
        .collection("moods")
        .findOne({ user: await subForHandle(database, consent.handle), deleted: { $ne: true } }, { sort: { when: -1 } });

      const doc = personToLinkedArt({ handle: consent.handle, latestMood });
      return finish(event, doc, `${WEB_BASE}/@${consent.handle}`);
    }

    // Painting: /painting/{code}
    if (segs[0] === "painting" && segs[1]) {
      const p = await database.db.collection("paintings").findOne({ code: segs[1] });
      if (!p || p.nuked) return respond(404, { message: "Not found" });
      const handle = await handleFor(p.user);
      const consent = previewLicense(event) ? { handle, license: previewLicense(event) } : await rightsFor(database, handle);
      if (!consent || !handle) return respond(404, { message: "Not found" });

      const imageUrl = paintingImageUrl(handle, p.slug, p.user);
      const doc = paintingToLinkedArt({ code: p.code, handle, when: p.when, imageUrl, license: consent.license });
      return finish(event, doc, `${WEB_BASE}/painting/${p.code}`);
    }

    // Piece: /piece/{code}
    if (segs[0] === "piece" && segs[1]) {
      const piece =
        (await database.db.collection("pieces").findOne({ code: segs[1] })) ||
        (await database.db.collection("kidlisp").findOne({ code: segs[1] }));
      if (!piece || piece.user == null) return respond(404, { message: "Not found" });
      const handle = await handleFor(piece.user);
      const consent = previewLicense(event) ? { handle, license: previewLicense(event) } : await rightsFor(database, handle);
      if (!consent || !handle) return respond(404, { message: "Not found" });

      const doc = pieceToLinkedArt({
        code: piece.code,
        handle,
        when: piece.when,
        source: piece.source,
        hash: piece.hash,
        license: consent.license,
      });
      return finish(event, doc, `${WEB_BASE}/${piece.code}`);
    }

    // Mood: /mood/{handle}/{rkey}
    if (segs[0] === "mood" && segs[1] && segs[2]) {
      const handle = segs[1].replace(/^@/, "");
      const rkey = segs[2];
      const consent = previewLicense(event) ? { handle, license: previewLicense(event) } : await rightsFor(database, handle);
      if (!consent) return respond(404, { message: "Not found" });

      const sub = await subForHandle(database, consent.handle);
      const mood = await database.db
        .collection("moods")
        .findOne({ user: sub, "atproto.rkey": rkey, deleted: { $ne: true } });
      if (!mood) return respond(404, { message: "Not found" });

      const doc = moodToLinkedArt({
        handle: consent.handle,
        rkey,
        mood: mood.mood,
        when: mood.when,
        blueskyUri: mood.bluesky?.uri,
        license: consent.license,
      });
      return finish(event, doc, `${WEB_BASE}/moods~${consent.handle}~${rkey}`);
    }

    return respond(404, { message: "Not found" });
  } catch (error) {
    console.error("❌ crm error:", error);
    return respond(500, { message: error.message || String(error) });
  } finally {
    await database?.disconnect();
  }
}

// Resolve a handle (no @) back to its owning Auth0 sub.
async function subForHandle(database, handle) {
  const doc = await database.db.collection("@handles").findOne({ handle });
  return doc?._id;
}

function finish(event, doc, webUrl) {
  return wantsHtml(event) ? htmlView(doc, webUrl) : jsonld(doc);
}

function voidDoc() {
  const rights = [
    ...Object.values(RIGHTS_STATEMENTS).map(([id]) => id),
    ...Object.values(LICENSES).map(([id]) => id),
  ];
  return jsonld({
    "@context": { void: "http://rdfs.org/ns/void#", dcterms: "http://purl.org/dc/terms/" },
    "@id": `${DATA_BASE}/.well-known/void`,
    "@type": "void:Dataset",
    "dcterms:title": "Aesthetic Computer — Linked Open Data",
    "dcterms:description":
      "Public paintings, pieces, moods, and people from Aesthetic Computer, " +
      "expressed as Linked Art (a JSON-LD profile of CIDOC CRM). Records are " +
      "open and citable; works are In Copyright unless an artist has chosen a " +
      "Creative Commons license.",
    "dcterms:rights": rights,
    "void:uriSpace": `${DATA_BASE}/`,
    "void:rootResource": DATA_BASE,
  });
}

// Public image URL for a painting. Two slug shapes exist:
//   "{sub}/{kind}/{ts}" (kind varies: chat, painting, …) → strip the sub prefix
//   "{ts}" (older bare-timestamp slugs)                   → live under painting/{ts}
// Either way the URL keys off @handle, not the auth0 sub.
function paintingImageUrl(handle, slug, sub) {
  let mediaPath = String(slug || "");
  if (!mediaPath) return undefined;
  if (sub && mediaPath.startsWith(sub + "/")) {
    mediaPath = mediaPath.slice(sub.length + 1);
  } else if (!mediaPath.includes("/")) {
    mediaPath = "painting/" + mediaPath;
  }
  return `${WEB_BASE}/media/@${handle}/${mediaPath}.png`;
}

function esc(s) {
  return String(s ?? "").replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
}

function rightsLabel(key) {
  return key === DEFAULT_RIGHTS ? "in copyright" : key;
}

// Landing page — the human front door. Builds a recent-records feed across ALL
// public contributors (opt-outs excluded), each labeled with its rights, and
// points researchers at the standards + tooling that can consume the data.
// Modeled on at.aesthetic.computer's people+media feed.
async function landing(database) {
  const db = database.db;

  // Recent painting activity across all attributed (non-anonymous) users.
  const recent = await db
    .collection("paintings")
    .find({ nuked: { $ne: true }, user: { $ne: null } })
    .sort({ when: -1 })
    .limit(300)
    .toArray();

  // Distinct users in recency order, keeping up to 4 thumbnails each.
  const order = [];
  const bySub = new Map();
  for (const pt of recent) {
    if (!bySub.has(pt.user)) { bySub.set(pt.user, []); order.push(pt.user); }
    const arr = bySub.get(pt.user);
    if (arr.length < 4) arr.push(pt);
  }

  // Resolve handles + rights in one query; drop handleless + opted-out users.
  const handleDocs = await db.collection("@handles").find({ _id: { $in: order } }).toArray();
  const handleBySub = new Map(handleDocs.map((h) => [h._id, h]));
  const shownSubs = order
    .filter((sub) => {
      const h = handleBySub.get(sub);
      return h?.handle && h.linkedData?.enabled !== false; // exclude opt-outs
    })
    .slice(0, 24);

  // Total painting counts + latest mood per shown user — one aggregation each.
  const [countDocs, moodDocs] = await Promise.all([
    db.collection("paintings").aggregate([
      { $match: { user: { $in: shownSubs }, nuked: { $ne: true } } },
      { $group: { _id: "$user", n: { $sum: 1 } } },
    ]).toArray(),
    db.collection("moods").aggregate([
      { $match: { user: { $in: shownSubs }, deleted: { $ne: true } } },
      { $sort: { when: -1 } },
      { $group: { _id: "$user", mood: { $first: "$mood" }, rkey: { $first: "$atproto.rkey" } } },
    ]).toArray(),
  ]);
  const countBySub = new Map(countDocs.map((c) => [c._id, c.n]));
  const moodBySub = new Map(moodDocs.map((m) => [m._id, m]));

  const people = shownSubs.map((sub) => {
    const h = handleBySub.get(sub);
    const license = isLicense(h.linkedData?.license) ? h.linkedData.license : DEFAULT_RIGHTS;
    return {
      handle: h.handle,
      license,
      paintings: bySub.get(sub) || [],
      mood: moodBySub.get(sub),
      paintingCount: countBySub.get(sub) || 0,
    };
  });

  const cards = people
    .map((p) => {
      const thumbs = p.paintings
        .map((pt) => {
          const img = paintingImageUrl(p.handle, pt.slug, pt.user);
          return img
            ? `<a href="${DATA_BASE}/painting/${esc(pt.code)}" title="painting ${esc(pt.code)} — Linked Art record"><img loading=lazy src="${esc(img)}" alt="painting ${esc(pt.code)}"></a>`
            : "";
        })
        .join("");
      const moodLine = p.mood?.mood
        ? `<p class=mood>“${esc(p.mood.mood)}”${p.mood.rkey ? ` <a class=muted href="${DATA_BASE}/mood/${esc(p.handle)}/${esc(p.mood.rkey)}">↗ record</a>` : ""}</p>`
        : "";
      return `<section class=person>
<h3><a href="${DATA_BASE}/@${esc(p.handle)}">@${esc(p.handle)}</a>
<span class=muted>· ${p.paintingCount} painting${p.paintingCount === 1 ? "" : "s"} · ${esc(rightsLabel(p.license))}</span></h3>
${moodLine}
<div class=thumbs>${thumbs || '<span class=muted>no public paintings yet</span>'}</div>
</section>`;
    })
    .join("");

  const sample = people.find((p) => p.paintings[0])?.paintings[0];
  const sampleUrl = sample ? `${DATA_BASE}/painting/${sample.code}` : `${DATA_BASE}/@${people[0]?.handle || "jeffrey"}`;

  const body = `<!doctype html><html lang=en><meta charset=utf-8>
<meta name=viewport content="width=device-width, initial-scale=1">
<title>data · Aesthetic Computer</title>
<meta name=description content="Linked open data from Aesthetic Computer — paintings, pieces, moods, and people as Linked Art / CIDOC CRM, for cultural-heritage research.">
<style>
:root{color-scheme:light}
body{font-family:monospace;font-size:14px;line-height:1.5;max-width:48em;margin:0 auto;padding:2em 1em;color:#111;background:#f5f5f5}
a{color:#cd5c9b;text-decoration:none}a:hover{text-decoration:underline}
h1{border-bottom:2px solid #cd5c9b;padding-bottom:.3em}
h2{margin-top:2em}
code,pre{background:#eee;border-radius:4px}code{padding:.1em .3em}pre{padding:1em;overflow:auto;white-space:pre-wrap}
.muted{color:#666;font-weight:normal}
.person{border-top:1px solid #ddd;padding:.6em 0}
.person h3{margin:.2em 0;font-size:14px}
.mood{margin:.2em 0;color:#333}
.thumbs{display:flex;flex-wrap:wrap;gap:.4em;margin:.4em 0}
.thumbs img{width:88px;height:88px;object-fit:cover;border:1px solid #ccc;background:#fff;image-rendering:pixelated}
</style>
<h1>data.aesthetic.computer</h1>
<p>Aesthetic Computer publishes its community's public works as
<a href="https://linked.art">Linked Art</a> — the JSON-LD profile of
<a href="https://www.cidoc-crm.org">CIDOC CRM</a> used across the cultural-heritage
world. This makes paintings, pieces, moods, and people here discoverable and
citable by researchers (e.g. at the Getty) as linked open data.</p>

<h2>Recent records</h2>
<p class=muted>${people.length} recent contributor${people.length === 1 ? "" : "s"}. Each record is open and citable; reuse rights stay with the artist (<i>in copyright</i>) unless a Creative Commons license is shown. Thumbnails link to each Linked Art record.</p>
${cards || '<p class=muted>No public records yet.</p>'}

<h2>Entity identifiers</h2>
<ul>
<li><code>${DATA_BASE}/@{handle}</code> — a person <span class=muted>(CIDOC CRM E21)</span></li>
<li><code>${DATA_BASE}/painting/{code}</code> — a digital painting <span class=muted>(E22)</span></li>
<li><code>${DATA_BASE}/piece/{code}</code> — a KidLisp / JavaScript piece <span class=muted>(E73)</span></li>
<li><code>${DATA_BASE}/mood/{handle}/{rkey}</code> — a mood <span class=muted>(E33)</span></li>
</ul>
<p class=muted>Request <code>Accept: application/ld+json</code> (or append
<code>?format=jsonld</code>) for the Linked Art representation; a browser gets a
human-readable view that links back to the work on aesthetic.computer.</p>

<h2>Find these records elsewhere</h2>
<p>The same identifiers are designed to be consumed by standard cultural-heritage
and linked-data tooling:</p>
<ul>
<li><b>Fetch the JSON-LD</b> directly:<br>
<code>curl -H "Accept: application/ld+json" ${esc(sampleUrl)}</code></li>
<li><b>Inspect / expand it</b> in the <a href="https://json-ld.org/playground/">JSON-LD Playground</a> or the <a href="https://linkeddata.uriburner.com/">URIBurner</a> linked-data browser (paste a record URL).</li>
<li><b>Dataset description</b> for harvesters: <a href="${DATA_BASE}/.well-known/void">/.well-known/void</a> (<a href="https://www.w3.org/TR/void/">VoID</a>).</li>
<li><b>Vocabularies used:</b> <a href="https://www.getty.edu/research/tools/vocabularies/aat/">Getty AAT</a> for classification; <a href="https://rightsstatements.org/">rightsstatements.org</a> and <a href="https://creativecommons.org/licenses/">Creative Commons</a> for rights.</li>
<li><b>Also broadcast on the open social web</b> via <a href="https://at.aesthetic.computer">at.aesthetic.computer</a> (ATProto / Bluesky) — the same works, different protocol.</li>
</ul>

<h2>Coming soon</h2>
<ul>
<li><code>${DATA_BASE}/sparql</code> — SPARQL query endpoint, e.g. <i>“all digital paintings by @jeffrey in 2026”</i> (Stage 2).</li>
<li>IIIF image service over the painting CDN, for Getty-native image tooling (Stage 3).</li>
</ul>

<h2 id=optin>Rights &amp; opting out</h2>
<p>These records describe work that is already public on aesthetic.computer —
authorship, date, type, and a link to the image — much like a museum catalog
entry. By default each is marked
<a href="http://rightsstatements.org/vocab/InC/1.0/"><i>In Copyright</i></a>:
the record is open and citable, but copyright and the right to sell stay
entirely with the artist. Artists may opt in to an open
<a href="https://creativecommons.org/licenses/">Creative Commons</a> license to
allow reuse, or opt out to be excluded entirely — just ask.</p>`;
  return respond(200, body, { "Content-Type": "text/html; charset=utf-8" });
}
