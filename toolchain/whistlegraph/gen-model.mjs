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
// Curation anchor is the ORIGINAL 264-work file (the curated ten, Alex's
// attributions, renamed-code assets, the Longest film) — a stable snapshot,
// NOT the live graphs.json. Reading the live file would re-absorb whatever the
// last run wrote, so a re-cluster could never drop a stale/duplicate work.
// Everything outside these 264 is derived fresh from CODES.json each run.
const live = rd(join(D, "curation-seed.json"));
const priorSnap = existsSync("/tmp/CODES-before.json") ? rd("/tmp/CODES-before.json").songs : [];
// Hand-curation the clustering can't derive (the trio's broad-strokes notes):
//   renames  code → canonical title (title-only songs whose hook is never sung)
//   merges   sourceCode → targetCode (fold separately-clustered takes into one
//            whistlegraph; the source's posts retag to the target, the source
//            work drops out, and /sourceCode falls back to the index)
const overrides = existsSync(join(D, "overrides.json")) ? rd(join(D, "overrides.json")) : {};
const renames = overrides.renames || {};
const authors = overrides.authors || {}; // code → attribution ("Alex Freundlich", …)
const recodes = overrides.recodes || {};
const merges = overrides.merges || {};
// twins: a video can hold two whistlegraphs — mirror one cluster's posts under a
// NEW code+title so the same takes appear as a second work. [{code, title, from,
// by?}] where `from` is the source cluster's own code (its original clips).
const twins = overrides.twins || [];
// postTags: keep an extra badge on ONE specific post after it folds into another
// work — e.g. a talk video that merges into a song-graph but is still a "talk".
// { postId → ["talk", …] }. Survives merges because it's keyed on the post id.
const postTags = overrides.postTags || {};
const crossTags = overrides.crossTags || {}; // srcCode → [alsoUnderCode, …]
const mergeSources = new Set(Object.keys(merges));
// Resolve a raw cluster code to the slug it appears under: apply a recode
// (slug change) first, then a merge (fold into another work). Posts and the
// rollup both go through this so counts land on the final code.
const resolve = (code) => {
  let c = recodes[code] || code;
  for (let i = 0; i < 8 && merges[c]; i += 1) c = merges[c]; // follow merge chains (A→B→C), cycle-guarded
  return c;
};

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
// The id list lives durably under downloads/ (survives /tmp wipes on reboot);
// the old /tmp path is a fallback for an un-migrated tree.
const audioOnlyPath = existsSync(join(D, "audio-only.json"))
  ? join(D, "audio-only.json")
  : "/tmp/wg-audio-only.json";
const audioOnly = new Set(existsSync(audioOnlyPath) ? rd(audioOnlyPath) : []);
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
  return { cl, siteCode, reclaimed: best > 0, best };
});

// De-merge safeguard: once a song is split apart (Scared of Stairs out of
// Butterfly), both halves may still overlap the old merged live work and try
// to reclaim its code — which would silently re-merge them here. Let only the
// strongest claimant keep a given live code; the rest become their own works.
const strongest = new Map();
for (const cs of clusterSite) if (cs.reclaimed) {
  const cur = strongest.get(cs.siteCode);
  if (!cur || cs.best > cur.best) strongest.set(cs.siteCode, cs);
}
for (const cs of clusterSite) if (cs.reclaimed && strongest.get(cs.siteCode) !== cs) {
  cs.siteCode = cs.cl.code; cs.reclaimed = false;
}

// When a live code ends up hosting a seed-KNOWN song, that song's canonical
// title is ground truth — it corrects a stale live label (code "soda" was
// titled "My Neighbor" while actually holding Empty Soda Cup). Unseeded works
// keep their curated live title (turt "Turtle One Line", srce "The Source").
const seedTitle = new Map();
for (const [code, cs] of strongest) if (cs.reclaimed && cs.cl.known) seedTitle.set(code, cs.cl.title);

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
// Apply merge overrides: retag each post's codes to their merge target so the
// rollup (and every record page) aggregates the folded-in takes under one work.
for (const post of postsById.values()) {
  post.graphs = [...new Set(post.graphs.map(resolve))];
}
// Twin overrides: add a second code to the posts of a source cluster's OWN
// clips, so those multi-whistlegraph videos also appear under the twin work.
// (Uses the source cluster's original clips, not the post-merge set, so a fold
// into the source doesn't leak into the twin.)
const codeByCluster = new Map(codes.map((c) => [c.code, c]));
for (const t of twins) {
  const src = codeByCluster.get(t.from);
  if (!src) { console.warn(`twin ${t.code}: source cluster ${t.from} not found`); continue; }
  for (const id of src.clips) {
    const p = postsById.get(id);
    if (p && !p.graphs.includes(t.code)) p.graphs.push(t.code);
  }
}
// crossTags: a work that ALSO belongs under another existing work but keeps its
// own standing too (unlike merges, which drop the source). { sourceCode →
// [otherCode, …] } — the source's own clips gain the extra codes, so those posts
// surface on both record pages while the source work stays in the index.
for (const [srcCode, extra] of Object.entries(crossTags)) {
  const src = codeByCluster.get(srcCode);
  if (!src) { console.warn(`crossTags: source cluster ${srcCode} not found`); continue; }
  for (const id of src.clips) {
    const p = postsById.get(id);
    if (!p) continue;
    for (const c of extra) if (!p.graphs.includes(c)) p.graphs.push(c);
  }
}
// Per-post tags: stamp the extra badges (kept even when the post's work is a
// graph, so a folded-in talk video still reads as TALK on its card).
for (const [id, tags] of Object.entries(postTags)) {
  const p = postsById.get(id);
  if (p && Array.isArray(tags) && tags.length) p.tags = tags;
  else if (!p) console.warn(`postTags: post ${id} not found`);
}
const posts = [...postsById.values()].sort((a, b) => (b.views || 0) - (a.views || 0));

// Roll the posts up per code so a work's headline count/reach/thumb is the
// truth of what's actually tagged to it — the index and the record page then
// agree (they read from the same posts), and the split shows real numbers.
const rollup = new Map(); // code → {n, views, thumb}
for (const p of posts) for (const code of p.graphs) {
  const r = rollup.get(code) || { n: 0, views: 0, thumb: null };
  r.n += 1; r.views += p.views || 0;
  if (!r.thumb && p.thumb) r.thumb = p.thumb; // posts are view-sorted → top post's thumb
  rollup.set(code, r);
}

// ── graphs.json (works) ─────────────────────────────────────────────────
// Preserve every live work verbatim; append every non-reclaimed cluster as a
// fresh work (all kinds visible). Curated ten stay first; the rest sort by
// reach. Each work carries its kind + a summary post-count and total views.
const year = (span) => Number(String(span?.[0] ?? "").slice(0, 4)) || null;
const kindByLiveCode = new Map(clusterSite.filter((c) => c.reclaimed).map((c) => [c.siteCode, c.cl.kind]));

// "experiment" was a retired tab — normalize it back to a graph.
const normKind = (k) => (k === "experiment" ? "graph" : k);
const liveWorks = live.graphs.filter((e) => !mergeSources.has(e.code)).map((e) => {
  const code = recodes[e.code] || e.code; // a recode changes a live work's slug too
  const r = rollup.get(code);
  // Title precedence: an explicit rename wins, else a canonical seed title
  // corrects a stale live label, else keep the curated title.
  const title = renames[code] ?? (seedTitle.has(e.code) ? seedTitle.get(e.code) : undefined);
  return {
    ...e,
    code,
    ...(title !== undefined ? { title } : {}),
    ...(authors[code] ? { by: authors[code] } : {}), // durable attribution override
    kind: normKind(e.kind || kindByLiveCode.get(e.code) || "graph"),
    ...(r ? { views: r.views, perf: r.n } : {}), // accurate reach/count from tagged posts
    ...(r?.thumb ? { thumb: r.thumb } : {}),
  };
});
// Exclude a fresh cluster only if its EMITTED (post-recode) slug collides with a
// live work's emitted slug. Deduping on the emitted code — not the original —
// still catches the still-cdh case (fresh cdh emits bugy, which collides) while
// allowing a code swap (trip→tppl frees trip for bmhd→trip: fresh Triangle People
// emits tppl, no collision, so it survives instead of being dropped as "trip").
const liveCodeSet = new Set(liveWorks.map((e) => e.code));

const freshWorks = clusterSite
  .filter((c) => !c.reclaimed && !liveCodeSet.has(recodes[c.cl.code] || c.cl.code) && !mergeSources.has(c.cl.code))
  .map(({ cl }) => {
    const code = recodes[cl.code] || cl.code; // a recode changes the slug
    const r = rollup.get(code) || { n: cl.performances, views: cl.views, thumb: null };
    return {
      code,
      title: renames[code] ?? cl.title,
      by: authors[code] || "Whistlegraph", // durable attribution override; else collective credit
      year: year(cl.span),
      kind: cl.kind,
      views: r.views,
      perf: r.n,
      c: "#b44887",
      ...(r.thumb ? { thumb: r.thumb } : { noGlyph: true }),
    };
  });

// Twin works — a new entry per twin, its reach rolled up from the shared posts.
const twinWorks = twins.map((t) => {
  const r = rollup.get(t.code) || { n: 0, views: 0, thumb: null };
  const src = codeByCluster.get(t.from);
  return {
    code: t.code,
    title: t.title,
    by: authors[t.code] || t.by || "Whistlegraph",
    year: year(src?.span),
    kind: "graph",
    views: r.views,
    perf: r.n,
    c: "#b44887",
    ...(r.thumb ? { thumb: r.thumb } : { noGlyph: true }),
  };
});

const curated = liveWorks.slice(0, live.curated);
const rest = [...liveWorks.slice(live.curated), ...freshWorks, ...twinWorks].sort((a, b) => b.views - a.views);
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
