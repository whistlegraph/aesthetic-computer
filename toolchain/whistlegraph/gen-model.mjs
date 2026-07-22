// whistlegraph model — emit the normalized {works, posts} data model
//
//   node gen-model.mjs [--dry]
//
// Two entities plus an explicit relationship layer (approved by Alex +
// @jeffrey, 2026-07-17):
//
//   WHISTLEGRAPH (a confirmed work — a drawing you sing) → graphs.json:works
//     code · title · by · year · score assets · contribution rollups
//
//   POST (a published appearance of one or more whistlegraphs) → posts.json
//     id · kind · platform · media · url · date · views/likes/… · thumb
//
//   RELATIONSHIP (many-to-many) → post.relationships:[{work,role}]
//     only explicit `contributes` edges enter a work's count/reach.
//
// Automatic graph-like clusters stay candidates until curation confirms them.
// Talk/other cluster codes remain in graphs.json:legacy and post.graphs only as
// a compatibility bridge for old URLs; they are not commands or work codes.
//
// Curation stability: the live graphs.json carries hand-curation the cluster
// pass can't reproduce (Alex's attributions, renamed-code assets, the curated
// ten). We keep every live work verbatim and keep its CODE stable — a cluster
// that shares clips with a live work inherits that work's code (so deep links
// and CDN asset keys never churn). Everything else is emitted as an archive
// post and, when graph-like, a curation candidate from CODES.json.

import { readFileSync, writeFileSync, existsSync, readdirSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { commandDescriptions } from "../../system/public/aesthetic.computer/lib/prompt-commands.mjs";

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
// postPlots: narrative events that cut across works without becoming works.
// { postId → [plot title, …] }. A post can still stand alone and contribute to
// every whistlegraph in `graphs`; plots are a parallel descriptive layer.
const postPlots = overrides.postPlots || {};
const crossTags = overrides.crossTags || {}; // srcCode → [alsoUnderCode, …]
// The automatic cluster pass can suggest a composition, but only curation can
// promote it into the public work/code namespace. Seed works and known songs
// are confirmed automatically; these lists handle the exceptions Alex and the
// trio have explicitly identified during the archive pass.
const workModel = overrides.works || {};
const workIncludes = new Set(workModel.include || []);
const workExcludes = new Set(workModel.exclude || []);
// splits: pull specific posts OUT of a work into a NEW standalone work — for a
// take that got mis-clustered (e.g. a different song folded under Kitty Head).
// { newCode: { title, from, by?, ids:[postId, …] } }. Each listed post drops its
// `from` code and gains newCode; a fresh work is emitted from those posts.
const splits = overrides.splits || {};
// postEdits: per-post code surgery, keyed on post id. { id: {add:[…], remove:[…]} }
const postEdits = overrides.postEdits || {};
// manualPosts: durable non-TikTok archival media supplied by the trio. These
// records enter the same post model after clustering without being erased by a
// future TikTok catalog refresh.
const manualPosts = overrides.manualPosts || [];
const manualWorks = overrides.manualWorks || [];
const mergeSources = new Set(Object.keys(merges));
// Resolve a raw cluster code to the slug it appears under: apply a recode
// (slug change) first, then a merge (fold into another work). Posts and the
// rollup both go through this so counts land on the final code.
const resolve = (code) => {
  let c = recodes[code] || code;
  for (let i = 0; i < 8 && merges[c]; i += 1) c = merges[c]; // follow merge chains (A→B→C), cycle-guarded
  return c;
};
// Every durable title, attribution, recode, or merge target records a human
// curation decision about a composition. Preserve those decisions as confirmed
// works across model rebuilds; otherwise the candidate classifier can silently
// demote entries Alex just named or coded (the July 17 migration dropped
// "All These Fingers" this way). Explicit works.exclude still wins below.
const curatedWorkCodes = new Set(workIncludes);
for (const code of [
  ...Object.keys(renames),
  ...Object.keys(authors),
  ...Object.values(recodes),
  ...Object.values(merges),
]) curatedWorkCodes.add(resolve(code));

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
// Seed one post for every public account video, including posts that have not
// entered a song cluster. Cluster membership then adds site codes. This keeps
// Desk's Posts view a complete account-history surface instead of silently
// dropping talks, process clips, and still-unclassified performances.
const postsById = new Map();
const makePost = (id) => {
  const cat = catById.get(id) || {};
  const cm = clipMeta.get(id) || {};
  return {
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
};
for (const cat of catalog) postsById.set(cat.id, makePost(cat.id));

// A clustered clip lives in exactly one cluster today, so graphs[] usually
// holds one code — the array is the forward-compatible shape for posts that
// contribute to multiple whistlegraphs.
for (const { cl, siteCode } of clusterSite) {
  for (const id of cl.clips) {
    let post = postsById.get(id);
    if (!post) {
      post = makePost(id);
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
for (const [id, plots] of Object.entries(postPlots)) {
  const p = postsById.get(id);
  if (p && Array.isArray(plots) && plots.length) p.plots = [...new Set(plots)];
  else if (!p) console.warn(`postPlots: post ${id} not found`);
}
// splits: move the listed posts from their `from` work into the new code. Applied
// last so it overrides earlier tagging — a mis-clustered take lands only on the
// new work (and its old work's rollup count drops accordingly).
for (const [newCode, spec] of Object.entries(splits)) {
  for (const id of spec.ids || []) {
    const p = postsById.get(id);
    if (!p) { console.warn(`splits ${newCode}: post ${id} not found`); continue; }
    p.graphs = [...new Set([...p.graphs.filter((c) => c !== spec.from), newCode])];
  }
}
// postEdits: per-VIDEO surgery — one post's own code list, not a whole work.
// { postId: { add:[code…], remove:[code…] } }. Runs last so it can reference
// codes made by splits/twins. add = also appears under those works; remove =
// drops from that work (removing every code orphans the post → shown nowhere).
// Handles move (remove old + add new) in one entry.
for (const [id, edit] of Object.entries(postEdits)) {
  const p = postsById.get(id);
  if (!p) { console.warn(`postEdits: post ${id} not found`); continue; }
  const remove = new Set(edit.remove || []);
  p.graphs = p.graphs.filter((c) => !remove.has(c));
  for (const c of edit.add || []) if (!p.graphs.includes(c)) p.graphs.push(c);
}
for (const record of manualPosts) {
  const id = String(record?.id || "");
  if (!/^\d+$/.test(id) || postsById.has(id)) {
    console.warn(`manualPosts: invalid or duplicate post ${id || "(missing id)"}`);
    continue;
  }
  postsById.set(id, {
    id,
    platform: record.platform || "archive",
    media: record.media || "video",
    url: record.url || `${IDX}/posts/${id}.mp4`,
    date: record.date || null,
    views: record.views ?? null,
    likes: record.likes ?? null,
    comments: record.comments ?? null,
    reposts: record.reposts ?? null,
    saves: record.saves ?? null,
    duration: record.duration ?? null,
    desc: record.desc || "",
    src: record.src || `${IDX}/posts/${id}.mp4`,
    thumb: record.thumb || `${IDX}/posts/${id}.jpg`,
    graphs: [...new Set(record.works || [])],
  });
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
// fresh curation record. Curated ten stay first; the rest sort by reach. Only
// confirmed records enter `works` and the public command/code namespace.
const year = (span) => Number(String(span?.[0] ?? "").slice(0, 4)) || null;
const kindByLiveCode = new Map(clusterSite.filter((c) => c.reclaimed).map((c) => [c.siteCode, c.cl.kind]));
// A recognized whistlegraph stays a composition even when its cluster also
// contains a tutorial, reply, or behind-the-scenes post. Post-level media type
// must not promote the entire work into Talks / Other Posts.
const knownCompositionCodes = new Set(
  clusterSite.filter(({ cl }) => cl.known).map(({ siteCode }) => resolve(siteCode)),
);

// "experiment" was a retired tab — normalize it back to a graph.
const normKind = (k) => (k === "experiment" ? "graph" : k);
const workKind = (code, fallback) =>
  curatedWorkCodes.has(code) || knownCompositionCodes.has(code) ? "graph" : normKind(fallback);
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
    kind: workKind(code, e.kind || kindByLiveCode.get(e.code) || "graph"),
    status: workExcludes.has(code) ? "archived" : "confirmed",
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
      kind: workKind(code, cl.kind),
      status: workExcludes.has(code)
        ? "archived"
        : curatedWorkCodes.has(code) || cl.known
          ? "confirmed"
          : normKind(cl.kind) === "graph" ? "candidate" : "archived",
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
    status: workExcludes.has(t.code) ? "archived" : "confirmed",
    views: r.views,
    perf: r.n,
    c: "#b44887",
    ...(r.thumb ? { thumb: r.thumb } : { noGlyph: true }),
  };
});

// Split works — a new standalone work per split, rolled up from its moved posts,
// its year taken from the earliest moved post.
const splitYear = (ids) => {
  const ds = (ids || []).map((id) => postsById.get(id)?.date).filter(Boolean).sort();
  return ds.length ? Number(ds[0].slice(0, 4)) || null : null;
};
const splitWorks = Object.entries(splits).map(([code, spec]) => {
  const r = rollup.get(code) || { n: 0, views: 0, thumb: null };
  return {
    code,
    title: spec.title,
    by: authors[code] || spec.by || "Whistlegraph",
    year: splitYear(spec.ids),
    kind: "graph",
    status: workExcludes.has(code) ? "archived" : "confirmed",
    views: r.views,
    perf: r.n,
    c: "#b44887",
    ...(r.thumb ? { thumb: r.thumb } : { noGlyph: true }),
  };
});

const manualWorkRecords = manualWorks.map((spec) => {
  const r = rollup.get(spec.code) || { n: 0, views: 0, thumb: null };
  return {
    code: spec.code,
    title: spec.title,
    by: spec.by || "Whistlegraph",
    year: spec.year || null,
    kind: "graph",
    status: "confirmed",
    views: r.views,
    perf: r.n,
    c: spec.c || "#b44887",
    ...(spec.asset ? { asset: spec.asset } : {}),
    ...(r.thumb ? { thumb: r.thumb } : { noGlyph: true }),
  };
});

const curated = liveWorks.slice(0, live.curated);
const rest = [...liveWorks.slice(live.curated), ...freshWorks, ...twinWorks, ...splitWorks, ...manualWorkRecords].sort((a, b) => b.views - a.views);
const records = [...curated, ...rest];
const works = records.filter((w) => w.status === "confirmed");
const candidates = records.filter((w) => w.status === "candidate");
const legacy = records.filter((w) => w.status !== "confirmed");
const workCodes = new Set(works.map((w) => w.code));
const recordByCode = new Map(records.map((w) => [w.code, w]));
// Preserve old deep links after a recode or merge. The target must still be a
// confirmed work; aliases never reintroduce retired codes into commands.json.
const aliases = Object.fromEntries(
  [...Object.entries(recodes), ...Object.entries(merges)]
    .filter(([source, target]) => source !== target && workCodes.has(target)),
);

// Normalize the many-to-many relationship layer without breaking the old
// `graphs` field. `works` is the canonical set of compositions this post
// contributes to; `relationships` leaves room for discussion/source roles.
// Old cluster codes remain in `graphs` solely so legacy /<code> URLs can still
// resolve while the archive is being curated.
for (const p of posts) {
  p.works = p.graphs.filter((code) => workCodes.has(code));
  p.relationships = p.works.map((work) => ({ work, role: "contributes" }));
  const taggedKind = (p.tags || []).find((tag) => tag === "talk" || tag === "other");
  const recordKinds = p.graphs
    .map((code) => recordByCode.get(code)?.kind)
    .filter(Boolean);
  // Post type is independent of work confirmation: a performance in a
  // graph-like candidate remains a performance in the Archive even before the
  // composition is promoted into the coded Works index.
  p.kind = taggedKind || (p.works.length || recordKinds.includes("graph")
    ? "performance"
    : recordKinds.includes("talk") ? "talk" : "other");
}

const graphsOut = {
  generated: new Date().toISOString().slice(0, 10),
  count: works.length,
  candidateCount: candidates.length,
  legacyCount: legacy.length,
  curated: works.filter((w) => curated.includes(w)).length,
  works,
  candidates,
  legacy,
  aliases,
  // Compatibility for the OG endpoint and any old data clients. New code must
  // consume `works`; this full record list is the migration bridge.
  graphs: records,
};
const postsOut = { generated: new Date().toISOString().slice(0, 10), count: posts.length, posts };

// Prompt command feed. Whistlegraph codes share AC's bare command namespace,
// so publish collision metadata with the live index instead of teaching every
// client a second, inevitably stale list. A colliding work remains available
// everywhere as `wg <code>` and at /wg/<code>; non-colliding codes can also be
// entered bare (`imab`).
const commandConflicts = new Map();
const reserve = (code, reason) => {
  if (!commandConflicts.has(code)) commandConflicts.set(code, []);
  if (!commandConflicts.get(code).includes(reason)) commandConflicts.get(code).push(reason);
};
const pieceNames = (dir, reason) => {
  for (const file of readdirSync(dir)) {
    if (!file.endsWith(".mjs") && !file.endsWith(".lisp")) continue;
    reserve(file.replace(/\.(mjs|lisp)$/, ""), reason);
  }
};
pieceNames(join(SITE, "..", "aesthetic.computer", "disks"), "web-piece");
pieceNames(join(HERE, "..", "..", "fedac", "native", "pieces"), "native-piece");
for (const code of Object.keys(commandDescriptions)) reserve(code.toLowerCase(), "command");

const commandsOut = {
  generated: graphsOut.generated,
  count: works.length,
  commands: works.map((w) => {
    const conflicts = commandConflicts.get(w.code) || [];
    const bare = conflicts.length === 0;
    return {
      code: w.code,
      command: bare ? w.code : `wg ${w.code}`,
      bare,
      ...(conflicts.length ? { conflicts } : {}),
      title: w.title,
      by: w.by,
      kind: "graph",
      url: `https://whistlegraph.org/${w.code}`,
      ac: `https://aesthetic.computer/wg/${w.code}`,
    };
  }),
};

const archiveKinds = posts.reduce((m, p) => ((m[p.kind] = (m[p.kind] || 0) + 1), m), {});
console.log(`works: ${works.length} confirmed · ${candidates.length} candidates · ${legacy.length} legacy archive records`);
console.log(`posts: ${posts.length} — kinds ${JSON.stringify(archiveKinds)} (multi-work: ${posts.filter((p) => p.works.length > 1).length}, audio: ${posts.filter((p) => p.media === "audio").length})`);
if (DRY) { console.log("(dry — not written)"); process.exit(0); }
writeFileSync(join(SITE, "graphs.json"), JSON.stringify(graphsOut));
writeFileSync(join(SITE, "posts.json"), JSON.stringify(postsOut));
writeFileSync(join(SITE, "commands.json"), JSON.stringify(commandsOut));
console.log(`wrote graphs.json + posts.json + commands.json → ${SITE}`);
