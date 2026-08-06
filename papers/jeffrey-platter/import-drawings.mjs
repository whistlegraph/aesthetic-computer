#!/usr/bin/env node

// Snapshot the public, legacy drawings.rey.sc Rails index into a small,
// reproducible platter manifest. The image files remain on the original site;
// this records their chronology, titles, stable routes, and derivative URLs so
// the history remains legible even after the Rails UI becomes hard to run.

import { mkdir, writeFile } from "node:fs/promises";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO_ROOT = join(HERE, "..", "..");
const ORIGIN = "https://drawings-rey-sc.jas.life";
const PAGE_COUNT = 96;
const CONCURRENCY = 4;
const TARGET = join(
  REPO_ROOT,
  "system/public/papers.aesthetic.computer/platter/jeffrey/drawings/manifest.json",
);

const months = new Map(
  [
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December",
  ].map((month, index) => [month, String(index + 1).padStart(2, "0")]),
);

function decodeHtml(value = "") {
  return value
    .replaceAll("&#39;", "'")
    .replaceAll("&quot;", '"')
    .replaceAll("&amp;", "&")
    .replaceAll("&lt;", "<")
    .replaceAll("&gt;", ">")
    .replaceAll("&nbsp;", " ");
}

async function get(path) {
  let lastError;
  for (let attempt = 1; attempt <= 4; attempt += 1) {
    try {
      const response = await fetch(`${ORIGIN}${path}`, {
        headers: { "user-agent": "Aesthetic Computer Jeffrey platter archiver/1.0" },
      });
      if (!response.ok) throw new Error(`${path}: HTTP ${response.status}`);
      return response.text();
    } catch (error) {
      lastError = error;
      if (attempt < 4) await new Promise((resolve) => setTimeout(resolve, attempt * 750));
    }
  }
  throw lastError;
}

function attr(fragment, name) {
  const match = fragment.match(new RegExp(`${name}=(?:"([^"]*)"|'([^']*)'|([^\\s>]+))`));
  return decodeHtml(match?.[1] ?? match?.[2] ?? match?.[3] ?? "");
}

function derivative(path, size) {
  return `${ORIGIN}${path.replace(/\/thumb_/, `/${size}_`)}`;
}

function original(path) {
  return `${ORIGIN}${path.replace(/\/thumb_/, "/")}`;
}

function parsePage(html, page) {
  const rows = [];
  let year = null;
  let month = null;
  const event = /<div class=time-info>\s*<h2>(\d{4})<\/h2>\s*<h3>([^<]+)<\/h3>[\s\S]*?<\/div>\s*<\/div>\s*<\/div>|<a\s+data-drawing=[\s\S]*?<\/a>/g;

  for (const match of html.matchAll(event)) {
    const fragment = match[0];
    if (match[1]) {
      year = Number(match[1]);
      month = decodeHtml(match[2]);
      continue;
    }

    const slug = attr(fragment, "data-drawing");
    const title = attr(fragment, "title") || "Untitled";
    const href = attr(fragment, "href");
    const image = attr(fragment, "src");
    const id = image.match(/^\/drawings\/(\d+)\//)?.[1];
    const assetStamp = image.match(/thumb_(\d{14})/)?.[1] ?? null;
    if (!slug || !href || !image || !id || !year || !month) {
      throw new Error(`page ${page}: could not parse drawing near ${fragment.slice(0, 160)}`);
    }

    rows.push({
      id: Number(id),
      slug,
      title,
      completed: `${year}-${months.get(month)}`,
      completedYear: year,
      completedMonth: month,
      source: `${ORIGIN}${href}`,
      thumb: `${ORIGIN}${image}`,
      medium: derivative(image, "medium"),
      large: derivative(image, "large"),
      original: original(image),
      assetStamp,
    });
  }
  return rows;
}

function parseIndex(html, resource) {
  const matches = [...html.matchAll(new RegExp(`href="/${resource}/([^"]+)"[^>]*>([\\s\\S]*?)</a>`, "g"))];
  return matches
    .map((match) => ({
      slug: match[1],
      name: decodeHtml(match[2].replace(/<[^>]+>/g, " ").replace(/\s+/g, " ")).trim(),
    }))
    .filter((row) => row.slug !== "new" && row.name)
    .filter((row, index, all) => all.findIndex((other) => other.slug === row.slug) === index)
    .map((row) => ({ ...row, source: `${ORIGIN}/${resource}/${row.slug}` }));
}

async function mapLimit(items, limit, fn) {
  const output = new Array(items.length);
  let cursor = 0;
  async function worker() {
    while (cursor < items.length) {
      const index = cursor++;
      output[index] = await fn(items[index], index);
    }
  }
  await Promise.all(Array.from({ length: limit }, worker));
  return output;
}

const pages = Array.from({ length: PAGE_COUNT }, (_, index) => index + 1);
const pageRows = await mapLimit(pages, CONCURRENCY, async (page) => {
  const html = await get(page === 1 ? "/drawings" : `/drawings?page=${page}`);
  const rows = parsePage(html, page);
  if (page === 1 || page === PAGE_COUNT || page % 12 === 0) {
    process.stderr.write(`page ${String(page).padStart(2, "0")}/${PAGE_COUNT}: ${rows.length}\n`);
  }
  return rows;
});

const [groupsHtml, locationsHtml, collaboratorsHtml, homeHtml] = await Promise.all([
  get("/groups"), get("/locations"), get("/collaborators"), get("/"),
]);

const rows = pageRows.flat();
const seen = new Set();
for (const row of rows) {
  if (seen.has(row.slug)) throw new Error(`duplicate drawing slug: ${row.slug}`);
  seen.add(row.slug);
}
if (rows.length !== 3996) throw new Error(`expected 3996 drawings, found ${rows.length}`);

const groups = parseIndex(groupsHtml, "groups");
const locations = parseIndex(locationsHtml, "locations");
const collaborators = parseIndex(collaboratorsHtml, "collaborators");
const version = decodeHtml(
  homeHtml.match(/<span class=version><small><i>([^<]+)<\/i>/)?.[1] ?? "unknown",
);

const manifest = {
  title: "Jeffrey's Drawings",
  description: "A chronological index of Jeffrey Alan Scudder's Rails-era drawing archive.",
  source: ORIGIN,
  sourceCode: "https://github.com/whistlegraph/drawings",
  generated: new Date().toISOString(),
  archiveVersion: version,
  recordCount: rows.length,
  completionRange: {
    newest: rows[0].completed,
    oldest: rows.at(-1).completed,
  },
  groups,
  locations,
  collaborators,
  rows,
};

await mkdir(dirname(TARGET), { recursive: true });
await writeFile(TARGET, `${JSON.stringify(manifest)}\n`, "utf8");
console.log(`wrote ${TARGET.replace(`${REPO_ROOT}/`, "")}`);
console.log(`${rows.length} drawings · ${groups.length} groups · ${locations.length} locations · ${collaborators.length} collaborators`);
