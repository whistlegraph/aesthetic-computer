// sitemap — the people and paintings of Aesthetic Computer, for crawlers.
//
//   GET /api/sitemap/index.xml         a sitemap index of the files below
//   GET /api/sitemap/handles.xml       every profile:  /@handle
//   GET /api/sitemap/paintings-N.xml   every painting: /painting~CODE
//
// sitemap.xml and sitemap-pieces.xml are static files and list only the
// built-in pieces. What people made was reachable by following a link and no
// other way, so a search or AI crawler could not find a painting it had not
// already been told about. robots.txt advertises the index.
//
// Who is listed follows the rule data.aesthetic.computer already publishes
// by (crm.mjs): an account with a handle, unless it set
// `linkedData.enabled: false`. Anonymous paintings, removed ones and
// unfinished drafts stay out.

import { connect } from "../../backend/database.mjs";

const ORIGIN = "https://aesthetic.computer";
const PER_FILE = 40000; // The protocol allows 50,000 URLs a file.
const FRESH_MS = 60 * 60 * 1000;

const xmlHeaders = {
  "Content-Type": "application/xml; charset=utf-8",
  "Cache-Control": "public, max-age=3600",
  "Access-Control-Allow-Origin": "*",
};
const reply = (statusCode, body, headers = xmlHeaders) => ({ statusCode, headers, body });
const escape = (s) => String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");

export const urlset = (urls) =>
  `<?xml version="1.0" encoding="UTF-8"?>\n<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">\n` +
  urls.map((u) => `  <url><loc>${escape(u)}</loc></url>`).join("\n") + `\n</urlset>\n`;

export const sitemapIndex = (files) =>
  `<?xml version="1.0" encoding="UTF-8"?>\n<sitemapindex xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">\n` +
  files.map((f) => `  <sitemap><loc>${escape(`${ORIGIN}/api/sitemap/${f}`)}</loc></sitemap>`).join("\n") + `\n</sitemapindex>\n`;

// Which file a request path names: "index", "handles", or a paintings page.
export function sitemapFile(path = "") {
  const name = path.split("?")[0].split("/").filter(Boolean).pop() || "";
  if (name === "sitemap" || name === "index.xml") return { kind: "index" };
  if (name === "handles.xml") return { kind: "handles" };
  const page = /^paintings-([1-9]\d{0,3})\.xml$/.exec(name);
  return page ? { kind: "paintings", page: Number(page[1]) } : null;
}

// Every listed URL, read once an hour: the whole set is two small queries,
// and a crawler asks for several files in a row.
let listing = null;
async function readListing() {
  if (listing && Date.now() - listing.at < FRESH_MS) return listing;
  const database = await connect();
  try {
    const db = database.db;
    const handleBySub = new Map();
    for await (const h of db.collection("@handles").find({}, { projection: { handle: 1, "linkedData.enabled": 1 } })) {
      if (h.handle && h.linkedData?.enabled !== false) handleBySub.set(h._id, h.handle);
    }
    const paintings = [];
    const cursor = db.collection("paintings")
      .find({ nuked: { $ne: true }, status: { $ne: "wip" }, user: { $ne: null }, code: { $type: "string" } }, { projection: { _id: 0, code: 1, user: 1 } })
      .sort({ _id: 1 });
    for await (const p of cursor) {
      if (handleBySub.has(p.user) && /^[A-Za-z0-9]{3,12}$/.test(p.code)) paintings.push(`${ORIGIN}/painting~${p.code}`);
    }
    const handles = [...new Set(handleBySub.values())].sort().map((h) => `${ORIGIN}/@${encodeURIComponent(h)}`);
    listing = { at: Date.now(), handles, paintings };
    return listing;
  } finally {
    try { await database.disconnect(); } catch (_e) { /* already closed */ }
  }
}

export async function handler(event) {
  if (event.httpMethod !== "GET" && event.httpMethod !== "HEAD") return reply(405, "Method not allowed", { "Content-Type": "text/plain" });
  const file = sitemapFile(event.path || "");
  if (!file) return reply(404, "No such sitemap", { "Content-Type": "text/plain" });
  try {
    const { handles, paintings } = await readListing();
    const pages = Math.max(1, Math.ceil(paintings.length / PER_FILE));
    if (file.kind === "index") {
      return reply(200, sitemapIndex(["handles.xml", ...Array.from({ length: pages }, (_, i) => `paintings-${i + 1}.xml`)]));
    }
    if (file.kind === "handles") return reply(200, urlset(handles));
    if (file.page > pages) return reply(404, "No such sitemap", { "Content-Type": "text/plain" });
    return reply(200, urlset(paintings.slice((file.page - 1) * PER_FILE, file.page * PER_FILE)));
  } catch (err) {
    console.error("🗺️ sitemap failed:", err?.message || err);
    return reply(503, "Sitemap unavailable", { "Content-Type": "text/plain", "Retry-After": "600" });
  }
}
