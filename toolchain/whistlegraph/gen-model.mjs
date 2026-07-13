// whistlegraph model — emit the normalized {works, posts} data model
//
//   node gen-model.mjs [--dry]
//
// Two entities (per @jeffrey's taxonomy):
//
//   WHISTLEGRAPH (a work — a drawing you sing) → graphs.json
//     code · title · by · year · kind · score assets · summary counts
//
//   POST (a published appearance of one or more whistlegraphs) → posts.json
//     id · platform · media · url · date · views/likes/… · thumb · graphs:[codes]
//
// A post is tagged with the whistlegraph code(s) it performs (many-to-many;
// today one code each, but the array is ready for a video that holds several).
// A record page is one work + the gallery of posts whose graphs[] include it.
//
// Curation stability: the live graphs.json carries hand-curation the cluster
// pass can't reproduce (Alex's attributions, renamed-code assets, the curated
// ten). We keep every live work verbatim and keep its CODE stable — a cluster
// that shares clips with a live work inherits that work's code (so deep links
// and CDN asset keys never churn). Everything else (new songs + all talks +
// other — now ALL visible) is emitted fresh from CODES.json + the naming pass.

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const D = join(HERE, "downloads");
const SITE = join(HERE, "..", "..", "system", "public", "whistlegraph.org");
const DRY = process.argv.includes("--dry");
const rd = (p) => JSON.parse(readFileSync(p, "utf8"));

const codes = rd(join(D, "CODES.json")).songs;
const songs = rd(join(D, "SONGS.json")).songs;
const catalog = rd(join(D, "CATALOG.json")).videos;
const live = rd(join(SITE, "graphs.json"));
const priorSnap = existsSync("/tmp/CODES-before.json") ? rd("/tmp/CODES-before.json").songs : [];

const catById = new Map(catalog.map((v) => [v.id, v]));
const songByGlyph = new Map(); // cluster glyph id → its SONGS cluster (for clip metadata)
for (const s of songs) {
  const top = [...s.clips].sort((a, b) => (b.views || 0) - (a.views || 0))[0];
  if (top) songByGlyph.set(top.id, s);
}
const clipMeta = new Map(); // clip id → {date, views, text} from SONGS
for (const s of songs) for (const c of s.clips) clipMeta.set(c.id, c);

// Live works are the curation to preserve. Map each live code to the clips it
// historically owned (via the pre-ingest CODES snapshot) so a re-clustered
// version can reclaim the same code.
const priorByCode = new Map(priorSnap.map((p) => [p.code, p]));
const clipToLiveCode = new Map(); // historical clip id → live code
for (const e of live.graphs) {
  const prior = priorByCode.get(e.code);
  if (!prior) continue;
  for (const id of prior.clips) clipToLiveCode.set(id, e.code);
}

// Audio-only posts (no video stream) can't carry a poster frame — flag media.
const audioOnly = new Set(existsSync("/tmp/wg-audio-only.json") ? rd("/tmp/wg-audio-only.json") : []);
const hasGlyph = (id) => existsSync(join(D, "glyphs", `${id}.jpg`));

const IDX = "https://assets.aesthetic.computer/whistlegraph/index";

// Resolve each cluster's SITE code: reclaim a live code when clips overlap a
// live work (majority wins), else use the cluster's own (fresh) code.
const clusterSite = codes.map((cl) => {
  const votes = new Map();
  for (const id of cl.clips) {
    const lc = clipToLiveCode.get(id);
    if (lc) votes.set(lc, (votes.get(lc) || 0) + 1);
  }
  let siteCode = cl.code, best = 0;
  for (const [lc, n] of votes) if (n > best) { best = n; siteCode = lc; }
  return { cl, siteCode, reclaimed: best > 0 };
});

// ── posts.json ──────────────────────────────────────────────────────────
// One post per clip, tagged with its cluster's site code. (A clip lives in
// exactly one cluster today, so graphs[] holds one code — the array is the
// forward-compatible shape for multi-whistlegraph posts.)
const postsById = new Map();
for (const { cl, siteCode } of clusterSite) {
  for (const id of cl.clips) {
    const cat = catById.get(id) || {};
    const cm = clipMeta.get(id) || {};
    let post = postsById.get(id);
    if (!post) {
      post = {
        id,
        platform: "tiktok",
        media: audioOnly.has(id) ? "audio" : "video",
        url: cat.url || `https://www.tiktok.com/@whistlegraph/video/${id}`,
        date: cat.date || cm.date || null,
        views: cat.views ?? cm.views ?? null,
        likes: cat.likes ?? null,
        comments: cat.comments ?? null,
        reposts: cat.reposts ?? null,
        saves: cat.saves ?? null,
        duration: cat.duration ?? null,
        desc: cat.desc || "",
        src: `${IDX}/posts/${id}.mp4`,
        thumb: audioOnly.has(id) || !hasGlyph(id) ? null : `${IDX}/posts/${id}.jpg`,
        graphs: [],
      };
      postsById.set(id, post);
    }
    if (!post.graphs.includes(siteCode)) post.graphs.push(siteCode);
  }
}
const posts = [...postsById.values()].sort((a, b) => (b.views || 0) - (a.views || 0));

// ── graphs.json (works) ─────────────────────────────────────────────────
// Preserve every live work verbatim; append every non-reclaimed cluster as a
// fresh work (all kinds visible). Curated ten stay first; the rest sort by
// reach. Each work carries its kind + a summary post-count and total views.
const year = (span) => Number(String(span?.[0] ?? "").slice(0, 4)) || null;
const kindByLiveCode = new Map(clusterSite.filter((c) => c.reclaimed).map((c) => [c.siteCode, c.cl.kind]));

// "experiment" was a retired tab — normalize it back to a graph.
const normKind = (k) => (k === "experiment" ? "graph" : k);
const liveWorks = live.graphs.map((e) => ({ ...e, kind: normKind(e.kind || kindByLiveCode.get(e.code) || "graph") }));
const liveCodeSet = new Set(liveWorks.map((e) => e.code));

const freshWorks = clusterSite
  .filter((c) => !c.reclaimed && !liveCodeSet.has(c.cl.code))
  .map(({ cl }) => ({
    code: cl.code,
    title: cl.title,
    by: "Whistlegraph",
    year: year(cl.span),
    kind: cl.kind,
    views: cl.views,
    perf: cl.performances,
    c: "#b44887",
    ...(audioOnly.size && cl.clips.every((id) => !hasGlyph(id)) ? { noGlyph: true } : {}),
  }));

const curated = liveWorks.slice(0, live.curated);
const rest = [...liveWorks.slice(live.curated), ...freshWorks].sort((a, b) => b.views - a.views);
const works = [...curated, ...rest];

const graphsOut = {
  generated: new Date().toISOString().slice(0, 10),
  count: works.length,
  curated: live.curated,
  graphs: works,
};
const postsOut = { generated: new Date().toISOString().slice(0, 10), count: posts.length, posts };

const kinds = works.reduce((m, w) => ((m[w.kind] = (m[w.kind] || 0) + 1), m), {});
console.log(`works: ${works.length} (${live.graphs.length} live kept, ${freshWorks.length} fresh) — kinds ${JSON.stringify(kinds)}`);
console.log(`posts: ${posts.length} (multi-tagged: ${posts.filter((p) => p.graphs.length > 1).length}, audio: ${posts.filter((p) => p.media === "audio").length})`);
if (DRY) { console.log("(dry — not written)"); process.exit(0); }
writeFileSync(join(SITE, "graphs.json"), JSON.stringify(graphsOut));
writeFileSync(join(SITE, "posts.json"), JSON.stringify(postsOut));
console.log(`wrote graphs.json + posts.json → ${SITE}`);
