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

// Read the consent + license for a handle (without the leading @).
async function consentFor(database, handle) {
  if (!handle) return null;
  const doc = await database.db
    .collection("@handles")
    .findOne({ handle: handle.replace(/^@/, "") });
  const ld = doc?.linkedData;
  if (!ld?.enabled || !isLicense(ld.license)) return null;
  return { handle: doc.handle, license: ld.license };
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

  // Landing page.
  if (segs.length === 0) return landing();

  // SPARQL is Stage 2.
  if (segs[0] === "sparql") {
    return respond(501, { message: "SPARQL endpoint not yet available (Stage 2)." });
  }

  // VoID dataset description.
  if (segs[0] === ".well-known" && segs[1] === "void") return voidDoc();

  let database;
  try {
    database = await connect();

    // Person: /@handle
    if (segs[0].startsWith("@")) {
      const handle = segs[0].slice(1);
      const consent = previewLicense(event) ? { handle, license: previewLicense(event) } : await consentFor(database, handle);
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
      const consent = previewLicense(event) ? { handle, license: previewLicense(event) } : await consentFor(database, handle);
      if (!consent || !handle) return respond(404, { message: "Not found" });

      // slug is "{sub}/{kind}/{ts}" (kind varies: chat, painting, …). Strip the
      // leading user-sub segment so the public image URL keys off @handle, not
      // the auth0 sub, and preserves the real storage subpath.
      let mediaPath = String(p.slug || "");
      if (p.user && mediaPath.startsWith(p.user + "/")) {
        mediaPath = mediaPath.slice(p.user.length + 1);
      }
      const imageUrl = mediaPath ? `${WEB_BASE}/media/@${handle}/${mediaPath}.png` : undefined;
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
      const consent = previewLicense(event) ? { handle, license: previewLicense(event) } : await consentFor(database, handle);
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
      const consent = previewLicense(event) ? { handle, license: previewLicense(event) } : await consentFor(database, handle);
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
  const licenses = Object.values(LICENSES).map(([id]) => id);
  return jsonld({
    "@context": { void: "http://rdfs.org/ns/void#", dcterms: "http://purl.org/dc/terms/" },
    "@id": `${DATA_BASE}/.well-known/void`,
    "@type": "void:Dataset",
    "dcterms:title": "Aesthetic Computer — Linked Open Data",
    "dcterms:description":
      "Opted-in paintings, pieces, moods, and people from Aesthetic Computer, " +
      "expressed as Linked Art (a JSON-LD profile of CIDOC CRM).",
    "dcterms:license": licenses,
    "void:uriSpace": `${DATA_BASE}/`,
    "void:rootResource": DATA_BASE,
  });
}

function landing() {
  const body = `<!doctype html><html lang=en><meta charset=utf-8>
<meta name=viewport content="width=device-width, initial-scale=1">
<title>data · Aesthetic Computer</title>
<meta name=description content="Linked open data from Aesthetic Computer — paintings, pieces, moods, and people as Linked Art / CIDOC CRM, for cultural-heritage research.">
<style>
:root{color-scheme:light}
body{font-family:monospace;font-size:14px;line-height:1.5;max-width:46em;margin:0 auto;padding:2em 1em;color:#111;background:#f5f5f5}
a{color:#cd5c9b;text-decoration:none}a:hover{text-decoration:underline}
h1{border-bottom:2px solid #cd5c9b;padding-bottom:.3em}
code,pre{background:#eee;border-radius:4px}code{padding:.1em .3em}pre{padding:1em;overflow:auto}
.muted{color:#666}
</style>
<h1>data.aesthetic.computer</h1>
<p>Aesthetic Computer publishes its community's opted-in works as
<a href="https://linked.art">Linked Art</a> — the JSON-LD profile of
<a href="https://www.cidoc-crm.org">CIDOC CRM</a> used across the cultural-heritage
world. This makes paintings, pieces, moods, and people here discoverable and
citable by researchers (e.g. at the Getty) as linked open data.</p>

<h2>Entity identifiers</h2>
<ul>
<li><code>${DATA_BASE}/@{handle}</code> — a person <span class=muted>(E21)</span></li>
<li><code>${DATA_BASE}/painting/{code}</code> — a digital painting <span class=muted>(E22)</span></li>
<li><code>${DATA_BASE}/piece/{code}</code> — a KidLisp / JavaScript piece <span class=muted>(E73)</span></li>
<li><code>${DATA_BASE}/mood/{handle}/{rkey}</code> — a mood <span class=muted>(E33)</span></li>
</ul>
<p class=muted>Request <code>Accept: application/ld+json</code> (or append
<code>?format=jsonld</code>) for the Linked Art representation; a browser gets a
human-readable view that links back to the work on aesthetic.computer.</p>

<h2>Consent &amp; rights</h2>
<p>Only works whose author has opted in — with an explicit Creative Commons
license — are published here. Everything else returns 404.</p>

<h2>Coming soon</h2>
<ul>
<li><code>${DATA_BASE}/sparql</code> — SPARQL query endpoint (Stage 2)</li>
<li><a href="${DATA_BASE}/.well-known/void">/.well-known/void</a> — dataset description</li>
</ul>`;
  return respond(200, body, { "Content-Type": "text/html; charset=utf-8" });
}
