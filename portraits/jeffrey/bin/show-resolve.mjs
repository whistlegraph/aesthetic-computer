#!/usr/bin/env node
// Resolve a "show" definition to the ordered list of @whistlegraph posts that
// should be visible on the public IG grid. A show is a small JSON file under
// portraits/jeffrey/shows/<slug>.json with optional selectors and manual
// include/exclude lists.
//
// Selectors (AND'd together):
//   { type: "face_match",    subject: "jeffrey", min_similarity?: 0.6 }
//   { type: "face_excludes", subject: "jeffrey" }
//   { type: "date_range",    from?: "YYYY-MM-DD", to?: "YYYY-MM-DD" }
//   { type: "recent_count",  n: 30 }
//   { type: "recent_days",   n: 30 }
//   { type: "media_kind",    values: ["post"]|["story"]|["post","story"] }
//   { type: "top_n_by",      metric: "likes"|"comments", n: 100 }
//
// Reads:
//   portraits/jeffrey/ig-archive/<account>-index.json   (universe)
//   portraits/jeffrey/curated/<account>-meta.jsonl      (typename+caption+likes+comments)
//   portraits/jeffrey/curated/jeffrey-match.jsonl       (face-match data)
//   portraits/jeffrey/shows/<slug>.json                 (show definition)
//
// Writes resolved JSON to stdout.
//
// Usage:
//   node bin/show-resolve.mjs <slug> [--account=whistlegraph] [--with-media-ids]

import { readFileSync, existsSync } from "fs";
import { join } from "path";
import { execSync } from "child_process";

const HERE = new URL(".", import.meta.url).pathname;
const REPO_ROOT = join(HERE, "..", "..", "..");

const args = process.argv.slice(2);
const slug = args.find((a) => !a.startsWith("--"));
const accountArg = args.find((a) => a.startsWith("--account="));
const account = accountArg ? accountArg.split("=")[1] : "whistlegraph";
const withMediaIds = args.includes("--with-media-ids");

if (!slug) {
  console.error(
    "usage: show-resolve.mjs <slug> [--account=whistlegraph] [--with-media-ids]",
  );
  process.exit(64);
}

const showPath = join(REPO_ROOT, "portraits/jeffrey/shows", `${slug}.json`);
const indexPath = join(
  REPO_ROOT,
  "portraits/jeffrey/ig-archive",
  `${account}-index.json`,
);
const metaPath = join(
  REPO_ROOT,
  "portraits/jeffrey/curated",
  `${account}-meta.jsonl`,
);
const matchPath = join(
  REPO_ROOT,
  "portraits/jeffrey/curated",
  "jeffrey-match.jsonl",
);
const archiveDir = join(REPO_ROOT, "portraits/jeffrey/ig-archive", account);

const POST_TYPES = new Set(["GraphImage", "GraphVideo", "GraphSidecar"]);
const STORY_TYPES = new Set(["GraphStoryImage", "GraphStoryVideo"]);

function kindOf(typename) {
  if (POST_TYPES.has(typename)) return "post";
  if (STORY_TYPES.has(typename)) return "story";
  return "other";
}

if (!existsSync(showPath)) {
  console.error(`show not found: ${showPath}`);
  process.exit(1);
}
if (!existsSync(indexPath)) {
  console.error(`index not found: ${indexPath}`);
  process.exit(1);
}

const show = JSON.parse(readFileSync(showPath, "utf8"));
const index = JSON.parse(readFileSync(indexPath, "utf8"));

// Load post metadata catalog (one row per shortcode).
const metaByShortcode = new Map();
if (existsSync(metaPath)) {
  for (const line of readFileSync(metaPath, "utf8").split("\n")) {
    if (!line) continue;
    let row;
    try {
      row = JSON.parse(line);
    } catch {
      continue;
    }
    if (row.error) continue;
    metaByShortcode.set(row.shortcode, {
      typename: row.typename,
      kind: kindOf(row.typename),
      caption: row.caption ?? null,
      likes: row.likes ?? null,
      comments: row.comments ?? null,
      taken_at: row.taken_at ?? null,
      is_video: !!row.is_video,
    });
  }
}

// Aggregate face-match results per shortcode (any image in the post matching
// is enough for the post to count as a match).
const faceByShortcode = new Map();
if (existsSync(matchPath)) {
  for (const line of readFileSync(matchPath, "utf8").split("\n")) {
    if (!line) continue;
    let row;
    try {
      row = JSON.parse(line);
    } catch {
      continue;
    }
    const cur = faceByShortcode.get(row.shortcode);
    const sim = row.best_similarity ?? 0;
    const isMatch = !!row.is_jeffrey;
    if (!cur) {
      faceByShortcode.set(row.shortcode, {
        is_jeffrey: isMatch,
        best_similarity: sim,
        n_faces: row.n_faces ?? 0,
      });
    } else {
      cur.is_jeffrey = cur.is_jeffrey || isMatch;
      cur.best_similarity = Math.max(cur.best_similarity, sim);
      cur.n_faces = Math.max(cur.n_faces, row.n_faces ?? 0);
    }
  }
}

const universe = index.posts
  .map((p) => ({
    shortcode: p.shortcode,
    date: p.date,
    ...(metaByShortcode.get(p.shortcode) ?? {
      typename: null,
      kind: "other",
      caption: null,
      likes: null,
      comments: null,
      taken_at: null,
      is_video: false,
    }),
    ...(faceByShortcode.get(p.shortcode) ?? {
      is_jeffrey: false,
      best_similarity: 0,
      n_faces: 0,
    }),
  }))
  .sort((a, b) => a.date.localeCompare(b.date));

function applySelector(set, sel) {
  switch (sel.type) {
    case "face_match": {
      const minSim = sel.min_similarity ?? 0;
      if (sel.subject !== "jeffrey")
        throw new Error(`face_match subject "${sel.subject}" not yet supported`);
      return set.filter((p) => p.is_jeffrey && p.best_similarity >= minSim);
    }
    case "face_excludes": {
      if (sel.subject !== "jeffrey")
        throw new Error(`face_excludes subject "${sel.subject}" not yet supported`);
      return set.filter((p) => !p.is_jeffrey);
    }
    case "date_range": {
      return set.filter(
        (p) =>
          (!sel.from || p.date >= sel.from) && (!sel.to || p.date <= sel.to),
      );
    }
    case "recent_count":
      return set.slice(-sel.n);
    case "recent_days": {
      const cutoff = new Date(Date.now() - sel.n * 86400_000)
        .toISOString()
        .slice(0, 10);
      return set.filter((p) => p.date >= cutoff);
    }
    case "media_kind": {
      const allowed = new Set(sel.values ?? ["post"]);
      return set.filter((p) => allowed.has(p.kind));
    }
    case "top_n_by": {
      const metric = sel.metric;
      if (metric !== "likes" && metric !== "comments")
        throw new Error(`top_n_by metric must be "likes" or "comments"`);
      const sorted = set
        .filter((p) => p[metric] !== null && p[metric] !== undefined)
        .sort((a, b) => (b[metric] ?? 0) - (a[metric] ?? 0));
      return sorted.slice(0, sel.n ?? 100);
    }
    default:
      throw new Error(`unknown selector type: ${sel.type}`);
  }
}

let matched = universe;
for (const sel of show.selectors ?? []) {
  matched = applySelector(matched, sel);
}

const includeSet = new Set(matched.map((p) => p.shortcode));
for (const sc of show.manual_include ?? []) includeSet.add(sc);
for (const sc of show.manual_exclude ?? []) includeSet.delete(sc);

const order = show.order ?? "date";
const universeByShortcode = new Map(universe.map((p) => [p.shortcode, p]));
let resolved = [...includeSet]
  .map((sc) => universeByShortcode.get(sc))
  .filter(Boolean);

const cmps = {
  date: (a, b) => a.date.localeCompare(b.date),
  date_desc: (a, b) => b.date.localeCompare(a.date),
  likes_desc: (a, b) => (b.likes ?? 0) - (a.likes ?? 0) || a.date.localeCompare(b.date),
  comments_desc: (a, b) =>
    (b.comments ?? 0) - (a.comments ?? 0) || a.date.localeCompare(b.date),
};
if (!cmps[order]) throw new Error(`unknown order: ${order}`);
resolved.sort(cmps[order]);

if (withMediaIds) {
  for (const p of resolved) {
    const meta = join(archiveDir, `${p.date}_${p.shortcode}.json.xz`);
    if (existsSync(meta)) {
      try {
        const json = JSON.parse(
          execSync(`xzcat "${meta}"`, {
            encoding: "utf8",
            maxBuffer: 32 * 1024 * 1024,
          }),
        );
        p.media_id = json?.node?.id ?? null;
      } catch {
        p.media_id = null;
      }
    } else {
      p.media_id = null;
    }
  }
}

const output = {
  show,
  account,
  resolved_at: new Date().toISOString(),
  source_index: indexPath.replace(REPO_ROOT + "/", ""),
  stats: {
    universe: universe.length,
    selected_by_selectors: matched.length,
    manual_include: (show.manual_include ?? []).length,
    manual_exclude: (show.manual_exclude ?? []).length,
    resolved: resolved.length,
    with_media_ids: withMediaIds,
  },
  posts: resolved,
};

console.log(JSON.stringify(output, null, 2));
