#!/usr/bin/env node
// feed.mjs — build the "Aesthetic Computer — Readings" podcast feed.
//
// Reads every episode metadata sidecar (out/<slug>.json written by produce.mjs)
// and emits:
//   out/index.json  — catalog (newest first), for a web player
//   out/feed.xml    — RSS 2.0 + iTunes namespace, for podcast apps
//   out/cover.png   — the series/channel cover (generated once)
//
// Enclosure + image URLs point at the public asset host. Nothing is uploaded
// here; publish.mjs does that.
//
// Usage: node bin/feed.mjs   [--base https://assets.aesthetic.computer/podcast]

import { readFileSync, writeFileSync, readdirSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { renderCover } from "./cover.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const OUT = resolve(ROOT, "out");

const argv = process.argv.slice(2);
const baseFlag = argv.indexOf("--base");
const BASE = (baseFlag >= 0 ? argv[baseFlag + 1] : null)
  || process.env.PODCAST_BASE
  || "https://assets.aesthetic.computer/podcast";

const CHANNEL = {
  title: "Aesthetic Computer — Readings",
  link: "https://aesthetic.computer",
  language: "en-us",
  author: "@jeffrey",
  ownerName: "Jeffrey Scudder",
  ownerEmail: "mail@aesthetic.computer",
  description:
    "Readings of essays from Aesthetic Computer, in @jeffrey's voice. Each " +
    "episode is a single essay, read start to finish like a lesson — a little " +
    "bell to open, the length announced, and a fixed closing.",
};

const xml = (s) => String(s)
  .replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;")
  .replace(/"/g, "&quot;").replace(/'/g, "&apos;");

function hms(sec) {
  const h = Math.floor(sec / 3600);
  const m = Math.floor((sec % 3600) / 60);
  const s = Math.round(sec % 60);
  const p = (n) => String(n).padStart(2, "0");
  return h > 0 ? `${h}:${p(m)}:${p(s)}` : `${m}:${p(s)}`;
}

// ── gather episodes ────────────────────────────────────────────────────
const sidecars = readdirSync(OUT)
  .filter((f) => f.endsWith(".json") && f !== "index.json")
  .map((f) => { try { return JSON.parse(readFileSync(resolve(OUT, f), "utf8")); } catch { return null; } })
  .filter((e) => e && e.slug && e.audio);

sidecars.sort((a, b) => new Date(b.pubDate) - new Date(a.pubDate)); // newest first

// Local slug → hosted basename on the CDN (matches the papers siteName so the
// same mp3 backs both the papers "podcast" link and this feed).
const HOSTED = {
  "may-26": "aesthetic-may-26-essay",
  "june-26": "aesthetic-june-26-essay",
};
const host = (e) => HOSTED[e.slug] || e.slug;

// HOSTED is also the PUBLISH ALLOWLIST — only episodes listed here go in the
// public feed. This keeps local-only / confidential readings (e.g. the
// REGARDE-adjacent "named-markets") out of the feed no matter what's in out/.
for (let i = sidecars.length - 1; i >= 0; i--) {
  if (!HOSTED[sidecars[i].slug]) {
    console.log(`  · excluding ${sidecars[i].slug} (not in publish allowlist)`);
    sidecars.splice(i, 1);
  }
}

if (!sidecars.length) {
  console.error("✗ no episodes found in out/ — run produce.mjs first.");
  process.exit(1);
}

// ── series cover (generated once) ──────────────────────────────────────
const seriesCover = resolve(OUT, "cover.png");
if (!existsSync(seriesCover)) {
  const { full } = renderCover(
    { slug: "series", title: "Readings", author: "@jeffrey", date: "" }, OUT,
  );
  // renderCover writes <slug>-cover.png; adopt it as the channel cover.
  writeFileSync(seriesCover, readFileSync(full));
}

// ── index.json ─────────────────────────────────────────────────────────
const index = {
  ...CHANNEL,
  base: BASE,
  cover: `${BASE}/cover.png`,
  feed: `${BASE}/feed.xml`,
  updated: sidecars[0].pubDate,
  episodes: sidecars.map((e) => ({
    slug: e.slug,
    title: e.title,
    date: e.date,
    length: e.lengthText,
    durationSec: e.durationSec,
    bytes: e.bytes,
    words: e.wordCount,
    audio: `${BASE}/${host(e)}.mp3`,
    cover: `${BASE}/${host(e)}-cover.png`,
    description: e.description,
    pubDate: e.pubDate,
  })),
};
writeFileSync(resolve(OUT, "index.json"), JSON.stringify(index, null, 2) + "\n");

// ── feed.xml (RSS 2.0 + iTunes) ────────────────────────────────────────
const items = sidecars.map((e) => `    <item>
      <title>${xml(e.title)}</title>
      <link>${xml(`https://papers.aesthetic.computer/${host(e)}.pdf`)}</link>
      <guid isPermaLink="false">ac-reading-${xml(e.slug)}</guid>
      <pubDate>${xml(e.pubDate)}</pubDate>
      <description>${xml(e.description)}</description>
      <itunes:summary>${xml(e.description)}</itunes:summary>
      <itunes:author>${xml(CHANNEL.author)}</itunes:author>
      <itunes:duration>${hms(e.durationSec)}</itunes:duration>
      <itunes:image href="${xml(`${BASE}/${host(e)}-cover.png`)}"/>
      <itunes:explicit>false</itunes:explicit>
      <enclosure url="${xml(`${BASE}/${host(e)}.mp3`)}" length="${e.bytes}" type="audio/mpeg"/>
    </item>`).join("\n");

const feed = `<?xml version="1.0" encoding="UTF-8"?>
<rss version="2.0" xmlns:itunes="http://www.itunes.com/dtds/podcast-1.0.dtd" xmlns:content="http://purl.org/rss/1.0/modules/content/">
  <channel>
    <title>${xml(CHANNEL.title)}</title>
    <link>${xml(CHANNEL.link)}</link>
    <language>${xml(CHANNEL.language)}</language>
    <description>${xml(CHANNEL.description)}</description>
    <lastBuildDate>${xml(sidecars[0].pubDate)}</lastBuildDate>
    <itunes:author>${xml(CHANNEL.author)}</itunes:author>
    <itunes:summary>${xml(CHANNEL.description)}</itunes:summary>
    <itunes:type>episodic</itunes:type>
    <itunes:explicit>false</itunes:explicit>
    <itunes:owner>
      <itunes:name>${xml(CHANNEL.ownerName)}</itunes:name>
      <itunes:email>${xml(CHANNEL.ownerEmail)}</itunes:email>
    </itunes:owner>
    <itunes:image href="${xml(`${BASE}/cover.png`)}"/>
    <itunes:category text="Arts">
      <itunes:category text="Books"/>
    </itunes:category>
    <itunes:category text="Technology"/>
    <image>
      <url>${xml(`${BASE}/cover.png`)}</url>
      <title>${xml(CHANNEL.title)}</title>
      <link>${xml(CHANNEL.link)}</link>
    </image>
${items}
  </channel>
</rss>
`;
writeFileSync(resolve(OUT, "feed.xml"), feed);

console.log(`✓ ${sidecars.length} episode${sidecars.length === 1 ? "" : "s"} · base ${BASE}`);
console.log(`  out/index.json`);
console.log(`  out/feed.xml`);
console.log(`  out/cover.png`);
for (const e of sidecars) console.log(`   · ${e.slug} — ${e.title} (${e.lengthText})`);
