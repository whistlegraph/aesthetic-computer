#!/usr/bin/env node

import { mkdir, readFile, readdir, writeFile } from "node:fs/promises";
import { dirname, extname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const ROOT = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const MANIFESTS = join(ROOT, "system/public/aesthetic.computer/disks/pop");
const DISKS = join(ROOT, "system/public/aesthetic.computer/disks");
const RELEASES = join(ROOT, "pop/releases");
const SITE = join(ROOT, "system/public/pop.aesthetic.computer");
const OUTPUT = join(SITE, "releases");
const PUBLIC = "https://pop.aesthetic.computer";

function assetUrl(value) {
  if (!value) return null;
  return value.startsWith("/assets/")
    ? `https://assets.aesthetic.computer${value.slice("/assets".length)}`
    : value;
}

function publicAsset(asset = {}) {
  const { localPath: _localPath, packetUri: _packetUri, md5: _md5, ...safe } = asset;
  return { ...safe, publicUrl: assetUrl(safe.publicUrl) };
}

export function publicProjection(record) {
  return {
    schemaVersion: record.schemaVersion,
    slug: record.slug,
    ddex: record.ddex,
    release: record.release,
    recording: record.recording,
    artwork: record.artwork,
    assets: {
      audio: publicAsset(record.assets?.audio),
      cover: publicAsset(record.assets?.cover),
    },
    links: record.links ?? {},
    delivery: {
      state: "evaluation",
      messageControlType: "TestMessage",
      transmitted: false,
    },
  };
}

async function json(path) {
  return JSON.parse(await readFile(path, "utf8"));
}

async function releaseRecords() {
  const records = new Map();
  for (const entry of await readdir(RELEASES, { withFileTypes: true })) {
    if (!entry.isDirectory()) continue;
    const path = join(RELEASES, entry.name, "release.json");
    try {
      const record = publicProjection(await json(path));
      records.set(record.slug, record);
    } catch (error) {
      if (error?.code !== "ENOENT") throw error;
    }
  }
  return records;
}

export async function buildPopCatalog({ output = OUTPUT } = {}) {
  const records = await releaseRecords();
  const tracks = [];
  const wrappers = new Set(await readdir(DISKS));
  const files = (await readdir(MANIFESTS)).filter((file) => extname(file) === ".json").sort();
  for (const file of files) {
    const manifest = await json(join(MANIFESTS, file));
    // A track has a piece only when its disks/<slug>.mjs wrapper exists;
    // section-less manifests (no illy campaign) are catalog-only.
    const hasPiece = wrappers.has(`${manifest.slug}.mjs`) && manifest.sections?.length > 0;
    tracks.push({
      slug: manifest.slug,
      title: manifest.title,
      artist: manifest.artist,
      album: manifest.album,
      bpm: manifest.bpm,
      key: manifest.key,
      meter: manifest.meter,
      duration: manifest.duration,
      released: manifest.released ?? null,
      status: manifest.status ?? "released",
      cover: assetUrl(manifest.cover),
      thumb: `${PUBLIC}/thumbs/${manifest.slug}.jpg`, // 112², from pop/bin/catalog-art.mjs
      art: `${PUBLIC}/art/${manifest.slug}.jpg`, // 480²
      audio: assetUrl(manifest.audio),
      piece: hasPiece ? `https://aesthetic.computer/${manifest.slug}` : null,
      links: manifest.links ?? {},
      credits: manifest.credits,
      releaseData: records.has(manifest.slug) ? `/releases/${manifest.slug}` : null,
    });
  }

  // Newest release first; undated manifests sink to the bottom.
  tracks.sort((a, b) => (b.released ?? "").localeCompare(a.released ?? "") || a.slug.localeCompare(b.slug));

  await mkdir(output, { recursive: true });
  for (const record of records.values()) {
    await writeFile(join(output, `${record.slug}.json`), `${JSON.stringify(record, null, 2)}\n`);
  }
  const catalog = {
    schemaVersion: 1,
    artist: "Aesthetic Dot Computer",
    tracks,
  };
  await writeFile(join(output, "catalog.json"), `${JSON.stringify(catalog, null, 2)}\n`);
  await writeFile(join(output, "..", "feed.xml"), await feed(tracks));
  return catalog;
}

// RSS with audio enclosures — podcast apps and feed readers both take it.
async function feed(tracks) {
  const esc = (s) => String(s ?? "").replace(/[<>&"]/g, (c) => ({ "<": "&lt;", ">": "&gt;", "&": "&amp;", '"': "&quot;" })[c]);
  const items = [];
  for (const t of tracks) {
    let length = 0;
    try { length = Number((await fetch(t.audio, { method: "HEAD" })).headers.get("content-length")) || 0; } catch {}
    const date = t.released ? new Date(`${t.released}T12:00:00Z`).toUTCString() : "";
    items.push(`    <item>
      <title>${esc(t.title)}</title>
      <link>${esc(t.piece ?? `${PUBLIC}/?t=${t.slug}`)}</link>
      <guid isPermaLink="false">pop:${esc(t.slug)}</guid>
      ${date ? `<pubDate>${date}</pubDate>` : ""}
      <description>${esc(`${t.artist} · ${t.album}. ${t.credits ?? ""}`.trim())}</description>
      <enclosure url="${esc(t.audio)}" length="${length}" type="audio/mpeg" />
      <itunes:duration>${Math.round(t.duration ?? 0)}</itunes:duration>
      <itunes:image href="${esc(t.cover)}" />
    </item>`);
  }
  return `<?xml version="1.0" encoding="UTF-8"?>
<rss version="2.0" xmlns:itunes="http://www.itunes.com/dtds/podcast-1.0.dtd" xmlns:atom="http://www.w3.org/2005/Atom">
  <channel>
    <title>pop · Aesthetic Computer</title>
    <link>${PUBLIC}/</link>
    <atom:link href="${PUBLIC}/feed.xml" rel="self" type="application/rss+xml" />
    <description>Singles made inside Aesthetic Computer, newest first.</description>
    <language>en</language>
    <itunes:author>Aesthetic Dot Computer</itunes:author>
    <itunes:image href="${PUBLIC}/pop-og.jpg" />
${items.join("\n")}
  </channel>
</rss>
`;
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const catalog = await buildPopCatalog();
  console.log(`Published ${catalog.tracks.length} tracks to ${OUTPUT}`);
}
