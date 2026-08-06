// Fail-closed snapshot of human-reviewed Whistlegraph identity and membership.
// Generated counts, reach, thumbnails, and candidate/archive rows intentionally
// stay outside the lock; confirmed work metadata and explicit post→work edges do
// not change unless a person reviews and accepts a new snapshot.

import { readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
export const DEFAULT_LOCK_PATH = join(HERE, "downloads", "curation-lock.json");
const SITE = join(HERE, "..", "..", "system", "public", "whistlegraph.org");

const keep = (value) => Object.fromEntries(Object.entries(value).filter(([, v]) => v !== undefined));

export function curationSnapshot(works = [], posts = []) {
  return {
    version: 1,
    works: works
      .map((work) => keep({
        code: work.code,
        title: work.title,
        by: work.by,
        year: work.year,
        kind: work.kind,
        status: work.status,
        slug: work.slug,
        asset: work.asset,
        canonical: work.canonical,
        wide: work.wide,
        film: work.film,
        c: work.c,
        versions: work.versions,
      }))
      .sort((a, b) => a.code.localeCompare(b.code)),
    postWorks: posts
      .filter((post) => (post.works || []).length)
      .map((post) => ({ id: String(post.id), works: [...post.works].sort() }))
      .sort((a, b) => a.id.localeCompare(b.id)),
  };
}

function rowsBy(items, key) {
  return new Map((items || []).map((item) => [String(item[key]), item]));
}

export function curationChanges(expected, actual) {
  const changes = [];
  for (const [label, key] of [["work", "code"], ["post relationship", "id"]]) {
    const field = label === "work" ? "works" : "postWorks";
    const before = rowsBy(expected?.[field], key);
    const after = rowsBy(actual?.[field], key);
    for (const id of [...new Set([...before.keys(), ...after.keys()])].sort()) {
      if (!before.has(id)) changes.push(`${label} added: ${id}`);
      else if (!after.has(id)) changes.push(`${label} removed: ${id}`);
      else if (JSON.stringify(before.get(id)) !== JSON.stringify(after.get(id))) changes.push(`${label} changed: ${id}`);
    }
  }
  return changes;
}

export function assertCurationLock(expected, works, posts) {
  const actual = curationSnapshot(works, posts);
  const changes = curationChanges(expected, actual);
  if (changes.length) {
    const shown = changes.slice(0, 20).map((change) => `  - ${change}`).join("\n");
    const rest = changes.length > 20 ? `\n  - …and ${changes.length - 20} more` : "";
    throw new Error(
      `Human curation lock rejected generated changes:\n${shown}${rest}\n` +
      "Review the work metadata and post relationships, then rerun gen-model.mjs with --accept-curation.",
    );
  }
  return actual;
}

export function writeCurationLock(path, works, posts) {
  const snapshot = curationSnapshot(works, posts);
  writeFileSync(path, `${JSON.stringify(snapshot, null, 2)}\n`);
  return snapshot;
}

if (import.meta.url === pathToFileURL(process.argv[1] || "").href) {
  if (!process.argv.includes("--accept")) {
    console.error("Usage: node toolchain/whistlegraph/curation-lock.mjs --accept");
    process.exit(2);
  }
  const graphs = JSON.parse(readFileSync(join(SITE, "graphs.json"), "utf8"));
  const archive = JSON.parse(readFileSync(join(SITE, "posts.json"), "utf8"));
  const snapshot = writeCurationLock(DEFAULT_LOCK_PATH, graphs.works || [], archive.posts || []);
  console.log(`accepted ${snapshot.works.length} works and ${snapshot.postWorks.length} post relationships → ${DEFAULT_LOCK_PATH}`);
}
