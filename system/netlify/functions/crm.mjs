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
  paintingImageUrl,
  DATA_BASE,
  WEB_BASE,
} from "../../backend/linked-art.mjs";
import { paintingManifest, imageInfo, pngDimensions } from "../../backend/iiif.mjs";

const dev = process.env.CONTEXT === "dev" || process.env.NETLIFY_DEV === "true";
const JSONLD = "application/ld+json";

// The AC wordmark — "Aesthetic.Computer" with a pink dot (HTML; body use only).
const AC = `Aesthetic<span class=dot>.</span>Computer`;

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
  return { handle: doc.handle, license, colors: doc.colors || [] };
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

// Render a handle as its AC sigil: each glyph of "@handle" tinted by the
// per-character colors stored in @handles.colors.
function sigil(handle, colors) {
  if (!handle) return "";
  const clean = handle.replace(/^@/, "");
  const glyphs = [...`@${clean}`]
    .map((ch, i) => {
      const c = colors?.[i];
      const col = c ? `rgb(${c.r},${c.g},${c.b})` : "var(--accent)";
      return `<span style="color:${col}">${esc(ch)}</span>`;
    })
    .join("");
  return `<a class=sigil href="${DATA_BASE}/@${esc(clean)}">${glyphs}</a>`;
}

// A rich human view of a record — the handle sigil, the painting image, rights,
// and links to the JSON-LD / IIIF / apex. Styled to match the landing (auto
// light/dark). Machines still get JSON-LD via content negotiation.
function htmlView(doc, webUrl, extras = {}) {
  const { kind, handle, colors, imageUrl, code } = extras;
  const rights = doc.subject_to?.[0];
  const rightsHtml = rights
    ? `<p class=muted>Rights: <a href="${esc(rights.classified_as?.[0]?.id)}">${esc(rights._label)}</a></p>`
    : "";
  const img = imageUrl
    ? `<a href="${esc(imageUrl)}"><img class=hero loading=lazy src="${esc(imageUrl)}" alt="${esc(doc._label)}"></a>`
    : "";
  const quote = doc.type === "LinguisticObject" && doc.content ? `<blockquote>“${esc(doc.content)}”</blockquote>` : "";
  const iiif =
    kind === "painting"
      ? `<li><a href="${DATA_BASE}/iiif/${esc(code)}/manifest">IIIF manifest</a> · <a href="https://projectmirador.org/embed/?iiif-content=${encodeURIComponent(`${DATA_BASE}/iiif/${code}/manifest`)}">open in Mirador ↗</a></li>`
      : "";
  const body = `<!doctype html><html lang=en><meta charset=utf-8>
<meta name=viewport content="width=device-width,initial-scale=1">
<title>${esc(doc._label)} · data</title>
<link rel="icon" href="https://aesthetic.computer/purple-pals.svg">
<link rel="alternate" type="application/ld+json" href="${doc.id}?format=jsonld">
<style>
:root{--bg:#f4f4f6;--fg:#141414;--muted:#666;--accent:#cd5c9b;--card:#fff;--border:#d0d0d4;color-scheme:light dark}
@media(prefers-color-scheme:dark){:root{--bg:#0c0c0f;--fg:#eaeaea;--muted:#8a8a90;--accent:#ef86c0;--card:#17171b;--border:#2c2c32}}
*{box-sizing:border-box}
body{font-family:monospace;font-size:14px;line-height:1.55;max-width:44em;margin:0 auto;padding:2.4em 1em 4em;color:var(--fg);background:var(--bg)}
a{color:var(--accent);text-decoration:none}a:hover{text-decoration:underline}
.corner{position:fixed;top:.55em;left:.7em;font-weight:bold;color:var(--accent)}
.dot{color:var(--accent)}
.sigil{font-weight:bold;font-size:1.5em;letter-spacing:.03em}
h1{font-size:1.05em;font-weight:normal;color:var(--muted);margin:.4em 0 .8em}
img.hero{max-width:100%;border:1px solid var(--border);background:var(--card);image-rendering:pixelated;display:block;margin:1em 0;border-radius:4px}
blockquote{border-left:3px solid var(--accent);margin:1em 0;padding:.3em 0 .3em 1em;font-size:1.2em}
ul{padding-left:1.2em}
footer{margin-top:2.5em;border-top:1px solid var(--border);padding-top:1em;color:var(--muted);display:flex;gap:1.2em;flex-wrap:wrap}
</style>
<a class=corner href="https://aesthetic.computer" title="back to Aesthetic.Computer">data</a>
${handle ? `<div>${sigil(handle, colors)}</div>` : ""}
<h1>${esc(doc._label)}</h1>
${img}
${quote}
${rightsHtml}
<ul>
<li><a href="${doc.id}?format=jsonld">Linked Art JSON-LD (CIDOC CRM)</a></li>
${iiif}
<li><a href="${webUrl}">View on ${AC} ↗</a></li>
</ul>
<footer><a href="/">← data.aesthetic.computer</a><a href="https://aesthetic.computer">aesthetic.computer</a></footer>`;
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

    // Sitemap of every entity URI, for crawlers + harvesters.
    if (segs[0] === "sitemap.xml") return await sitemap(database);

    // Person: /@handle
    if (segs[0].startsWith("@")) {
      const handle = segs[0].slice(1);
      const consent = previewLicense(event) ? { handle, license: previewLicense(event) } : await rightsFor(database, handle);
      if (!consent) return respond(404, { message: "Not found" });

      const latestMood = await database.db
        .collection("moods")
        .findOne({ user: await subForHandle(database, consent.handle), deleted: { $ne: true } }, { sort: { when: -1 } });

      const doc = personToLinkedArt({ handle: consent.handle, latestMood });
      return finish(event, doc, `${WEB_BASE}/@${consent.handle}`, {
        kind: "person", handle: consent.handle, colors: consent.colors,
      });
    }

    // Painting: /painting/{code}
    if (segs[0] === "painting" && segs[1]) {
      const pt = await loadPainting(database, segs[1], event);
      if (!pt) return respond(404, { message: "Not found" });
      const doc = paintingToLinkedArt({ code: pt.code, handle: pt.handle, when: pt.when, imageUrl: pt.imageUrl, license: pt.license });
      return finish(event, doc, `${WEB_BASE}/painting/${pt.code}`, {
        kind: "painting", handle: pt.handle, colors: pt.colors, imageUrl: pt.imageUrl, code: pt.code,
      });
    }

    // IIIF surfaces: /iiif/{code}/manifest | /info.json | /full/max/0/default.png
    if (segs[0] === "iiif" && segs[1]) {
      const pt = await loadPainting(database, segs[1], event);
      if (!pt) return respond(404, { message: "Not found" });

      // The full-image request (IIIF Level 0) redirects to the CDN.
      if (segs[2] === "full") {
        return respond(302, "", { Location: pt.imageUrl, "Content-Type": "text/plain" });
      }

      const dims = (await pngDimensions(pt.imageUrl)) || { width: 2048, height: 2048 };
      if (segs[2] === "info.json") {
        return respond(200, imageInfo({ code: pt.code, ...dims }), { "Content-Type": JSONLD });
      }
      // Default (…/manifest or bare /iiif/{code}) → Presentation Manifest.
      const manifest = paintingManifest({
        code: pt.code, handle: pt.handle, when: pt.when, license: pt.license, ...dims,
      });
      return respond(200, manifest, { "Content-Type": JSONLD });
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
      return finish(event, doc, `${WEB_BASE}/${piece.code}`, {
        kind: "piece", handle, colors: consent.colors, code: piece.code,
      });
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
      return finish(event, doc, `${WEB_BASE}/moods~${consent.handle}~${rkey}`, {
        kind: "mood", handle: consent.handle, colors: consent.colors,
      });
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

// Load a painting + its owner's handle/rights/image URL, honoring the rights
// gate. Returns null for missing, nuked, handleless, or opted-out. Shared by the
// Linked Art record route and the IIIF surfaces.
async function loadPainting(database, code, event) {
  const p = await database.db.collection("paintings").findOne({ code });
  if (!p || p.nuked) return null;
  const handle = await handleFor(p.user);
  const consent = previewLicense(event)
    ? { handle, license: previewLicense(event) }
    : await rightsFor(database, handle);
  if (!consent || !handle) return null;
  return {
    code: p.code,
    handle,
    when: p.when,
    license: consent.license,
    colors: consent.colors,
    imageUrl: paintingImageUrl(handle, p.slug, p.user),
  };
}

// XML sitemap of every published entity URI (persons, paintings, pieces, moods),
// so crawlers and cultural-heritage harvesters can discover the whole dataset.
async function sitemap(database) {
  const db = database.db;
  const handles = await db.collection("@handles").find({}).toArray();
  const bySub = new Map();
  const urls = [];
  for (const h of handles) {
    if (!h.handle || h.linkedData?.enabled === false) continue;
    bySub.set(h._id, h.handle);
    urls.push(`${DATA_BASE}/@${h.handle}`);
  }
  const push = (u) => urls.push(u);
  for await (const p of db.collection("paintings").find({ nuked: { $ne: true }, user: { $ne: null } }, { projection: { code: 1, user: 1 } })) {
    if (bySub.has(p.user)) push(`${DATA_BASE}/painting/${p.code}`);
  }
  for await (const pc of db.collection("pieces").find({ user: { $ne: null } }, { projection: { code: 1, user: 1 } })) {
    if (bySub.has(pc.user)) push(`${DATA_BASE}/piece/${pc.code}`);
  }
  for await (const m of db
    .collection("moods")
    .find({ deleted: { $ne: true }, "atproto.rkey": { $exists: true }, user: { $ne: null } }, { projection: { user: 1, atproto: 1 } })) {
    const handle = bySub.get(m.user);
    if (handle) push(`${DATA_BASE}/mood/${handle}/${m.atproto.rkey}`);
  }
  const body =
    `<?xml version="1.0" encoding="UTF-8"?>\n` +
    `<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">\n` +
    urls.map((u) => `  <url><loc>${u.replace(/&/g, "&amp;")}</loc></url>`).join("\n") +
    `\n</urlset>\n`;
  return respond(200, body, { "Content-Type": "application/xml; charset=utf-8" });
}

function finish(event, doc, webUrl, extras = {}) {
  return wantsHtml(event) ? htmlView(doc, webUrl, extras) : jsonld(doc);
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
    "void:sparqlEndpoint": `${DATA_BASE}/sparql`,
  });
}

function esc(s) {
  return String(s ?? "").replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
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

  // Big animated slideshow — every recent painting with a handled owner.
  const slides = [];
  for (const pt of recent) {
    const h = handleBySub.get(pt.user);
    if (!h?.handle || h.linkedData?.enabled === false) continue;
    const img = paintingImageUrl(h.handle, pt.slug, pt.user);
    if (!img) continue;
    slides.push(
      `<a href="${DATA_BASE}/painting/${esc(pt.code)}" title="painting ${esc(pt.code)} by @${esc(h.handle)}"><img loading=lazy src="${esc(img)}" alt=""></a>`,
    );
    if (slides.length >= 40) break;
  }
  const track = slides.join("");

  // Compact people chips beneath the slideshow.
  const chips = people
    .map(
      (p) =>
        `<a class=chip href="${DATA_BASE}/@${esc(p.handle)}">@${esc(p.handle)}<span class=muted> ${p.paintingCount}</span></a>`,
    )
    .join("");

  const totalPeople = await db
    .collection("@handles")
    .countDocuments({ handle: { $exists: true }, "linkedData.enabled": { $ne: false } });

  const sample = slides.length ? recent.find((pt) => handleBySub.get(pt.user)?.handle) : null;
  const sampleUrl = sample ? `${DATA_BASE}/painting/${sample.code}` : `${DATA_BASE}/@jeffrey`;

  const body = `<!doctype html><html lang=en><meta charset=utf-8>
<meta name=viewport content="width=device-width, initial-scale=1">
<title>data · Aesthetic Computer</title>
<meta name=description content="Linked open data from Aesthetic Computer — paintings, pieces, moods, and people as Linked Art / CIDOC CRM, for cultural-heritage research.">
<link rel="icon" href="https://aesthetic.computer/purple-pals.svg">
<link rel="sitemap" href="${DATA_BASE}/sitemap.xml">
<style>
:root{--bg:#f4f4f6;--fg:#141414;--muted:#666;--accent:#cd5c9b;--card:#fff;--border:#d0d0d4;color-scheme:light dark}
@media (prefers-color-scheme:dark){:root{--bg:#0c0c0f;--fg:#eaeaea;--muted:#8a8a90;--accent:#ef86c0;--card:#17171b;--border:#2c2c32}}
*{box-sizing:border-box}
body{font-family:monospace;font-size:14px;line-height:1.55;max-width:50em;margin:0 auto;padding:2.4em 1em 4em;color:var(--fg);background:var(--bg)}
a{color:var(--accent);text-decoration:none}a:hover{text-decoration:underline}
.corner{position:fixed;top:.55em;left:.7em;font-weight:bold;color:var(--accent);z-index:10}
.dot{color:var(--accent)}
h1{font-size:1.5em;margin:.2em 0}
h2{margin-top:2.2em;border-bottom:1px solid var(--border);padding-bottom:.2em}
.muted{color:var(--muted);font-weight:normal}
code,pre{background:var(--card);border:1px solid var(--border);border-radius:5px}
code{padding:.1em .35em}pre{padding:1em;overflow:auto;white-space:pre-wrap}
header{text-align:center;margin-bottom:1em}
#pals-beacon{display:inline-flex}
.pals-logo-container{position:relative;display:inline-block}
.pals-logo,.pals-logo-pink{width:72px;height:auto;display:block}
.pals-logo{filter:grayscale(100%) opacity(.3)}
.pals-logo-pink{position:absolute;top:0;left:0;filter:hue-rotate(-30deg) saturate(1.5) brightness(1.2) drop-shadow(0 0 8px rgba(255,100,200,.8));animation:pulse 3s ease-in-out infinite}
@keyframes pulse{0%,100%{opacity:.5}50%{opacity:.95}}
.slideshow{overflow:hidden;margin:1.4em -50vw;padding:.4em 0;width:100vw;position:relative;left:50%;right:50%;margin-left:-50vw;margin-right:-50vw;-webkit-mask-image:linear-gradient(90deg,transparent,#000 6%,#000 94%,transparent);mask-image:linear-gradient(90deg,transparent,#000 6%,#000 94%,transparent)}
.track{display:flex;gap:.6em;width:max-content;animation:scroll 90s linear infinite}
.slideshow:hover .track{animation-play-state:paused}
.track img{height:190px;width:auto;border:1px solid var(--border);background:var(--card);image-rendering:pixelated;display:block}
@keyframes scroll{from{transform:translateX(0)}to{transform:translateX(-50%)}}
@media (prefers-reduced-motion:reduce){.track{animation:none}.pals-logo-pink{animation:none;opacity:.8}}
.access{display:grid;grid-template-columns:repeat(auto-fit,minmax(210px,1fr));gap:.7em;margin:1em 0}
.access .card{border:1px solid var(--border);border-radius:9px;padding:.8em;background:var(--card)}
.access .card b{color:var(--accent);font-size:1.05em}
.chips{display:flex;flex-wrap:wrap;gap:.4em;margin:.6em 0}
.chip{border:1px solid var(--border);border-radius:999px;padding:.15em .7em;background:var(--card);font-size:.92em}
footer{margin-top:3em;border-top:1px solid var(--border);padding-top:1em;color:var(--muted);display:flex;gap:1.2em;flex-wrap:wrap}
</style>
<a class=corner href="https://aesthetic.computer" title="back to Aesthetic.Computer">data</a>
<header>
  <a id="pals-beacon" href="https://aesthetic.computer" aria-label="aesthetic.computer">
    <span class="pals-logo-container">
      <img src="https://aesthetic.computer/purple-pals.svg" alt="" class="pals-logo">
      <img src="https://aesthetic.computer/purple-pals.svg" alt="" class="pals-logo-pink">
    </span>
  </a>
  <h1>data.aesthetic.computer</h1>
  <div class="muted">${AC} as linked open data</div>
</header>

<p>Every public painting, piece, mood, and person on
<a href="https://aesthetic.computer">${AC}</a> is published here as
<a href="https://linked.art">Linked Art</a> — the JSON-LD profile of
<a href="https://www.cidoc-crm.org">CIDOC CRM</a> — so cultural-heritage
researchers (e.g. at the <a href="https://www.getty.edu">Getty</a>) can
discover, query, and cite it with standard tools.</p>

<div class=slideshow><div class=track>${track}${track}</div></div>
<p class=muted style="text-align:center">${totalPeople.toLocaleString()} people · all works discoverable. Hover to pause; click any painting for its record.</p>

<h2>Access points</h2>
<div class=access>
<div class=card><b>SPARQL</b><br><a href="${DATA_BASE}/sparql">/sparql</a><br><span class=muted>query the whole CIDOC CRM graph</span></div>
<div class=card><b>Linked Art</b><br><code>/painting/{code}</code><br><span class=muted>JSON-LD via <code>Accept: ld+json</code></span></div>
<div class=card><b>IIIF</b><br><code>/iiif/{code}/manifest</code><br><span class=muted>Presentation v3 + Level-0 image</span></div>
<div class=card><b>Sitemap</b><br><a href="${DATA_BASE}/sitemap.xml">/sitemap.xml</a><br><span class=muted>every entity URI</span></div>
<div class=card><b>VoID</b><br><a href="${DATA_BASE}/.well-known/void">/.well-known/void</a><br><span class=muted>dataset description</span></div>
<div class=card><b>Bluesky</b><br><a href="https://at.aesthetic.computer">at.aesthetic.computer</a><br><span class=muted>same works over ATProto</span></div>
</div>

<h2>Example queries</h2>
<p>Every painting by @jeffrey:</p>
<pre>curl -s '${DATA_BASE}/sparql' --data-urlencode 'query=
 PREFIX crm: &lt;http://www.cidoc-crm.org/cidoc-crm/&gt;
 SELECT ?work WHERE {
   ?work crm:P108i_was_produced_by/crm:P14_carried_out_by
         &lt;${DATA_BASE}/@jeffrey&gt; }'</pre>
<p>Because classifications are shared <a href="https://www.getty.edu/research/tools/vocabularies/aat/">Getty AAT</a> URIs, our data <b>federates with the Getty's own</b> — e.g. enrich each type with the Getty's preferred label (run from a client that federates both endpoints):</p>
<pre>PREFIX crm: &lt;http://www.cidoc-crm.org/cidoc-crm/&gt;
PREFIX gvp: &lt;http://vocab.getty.edu/ontology#&gt;
PREFIX xl:  &lt;http://www.w3.org/2008/05/skos-xl#&gt;
SELECT DISTINCT ?type ?gettyLabel WHERE {
  SERVICE &lt;${DATA_BASE}/sparql&gt; { ?work crm:P2_has_type ?type
    FILTER(STRSTARTS(STR(?type),"http://vocab.getty.edu/aat/")) }
  SERVICE &lt;https://vocab.getty.edu/sparql&gt; {
    ?type gvp:prefLabelGVP/xl:literalForm ?gettyLabel . } }</pre>
<p class=muted>Or fetch one record's JSON-LD directly:
<code>curl -H "Accept: application/ld+json" ${esc(sampleUrl)}</code> — inspect it in the
<a href="https://json-ld.org/playground/">JSON-LD Playground</a>.</p>

<h2>People</h2>
<div class=chips>${chips || '<span class=muted>none yet</span>'}</div>

<h2 id=optin>Rights &amp; opting out</h2>
<p>These records describe work that is already public on aesthetic.computer —
authorship, date, type, and a link to the image — much like a museum catalog
entry. By default each is marked
<a href="http://rightsstatements.org/vocab/InC/1.0/"><i>In Copyright</i></a>:
the record is open and citable, but copyright and the right to sell stay
entirely with the artist. Artists may opt in to an open
<a href="https://creativecommons.org/licenses/">Creative Commons</a> license to
allow reuse, or opt out to be excluded — just ask.</p>

<footer>
<a href="https://aesthetic.computer">aesthetic.computer</a>
<a href="https://at.aesthetic.computer">at · bluesky</a>
<a href="https://github.com/whistlegraph/aesthetic-computer">github</a>
<a href="${DATA_BASE}/sitemap.xml">sitemap</a>
<a href="${DATA_BASE}/.well-known/void">void</a>
</footer>`;
  return respond(200, body, { "Content-Type": "text/html; charset=utf-8" });
}
