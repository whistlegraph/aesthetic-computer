#!/usr/bin/env node

import { existsSync, mkdirSync, readFileSync, readdirSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import sharp from "sharp";
import { released } from "../lib/released.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const OUT = resolve(ROOT, "out");
const includeDrafts = process.argv.includes("--include-drafts");
const base = "https://assets.aesthetic.computer/klokkentales";
mkdirSync(OUT, { recursive: true });

const metas = readdirSync(OUT)
  .filter((name) => name.endsWith(".json") && !name.endsWith(".buzzsprout.json") && name !== "index.json")
  .map((name) => JSON.parse(readFileSync(resolve(OUT, name), "utf8")))
  .filter((meta) => meta.slug && existsSync(resolve(OUT, meta.audio)))
  .filter((meta) => includeDrafts || released(meta.slug))
  .sort((a, b) => Date.parse(b.pubDate || b.date) - Date.parse(a.pubDate || a.date));

const seriesCover = resolve(OUT, "cover-1400.png");
await sharp(resolve(ROOT, "assets", "cover.svg")).png().toFile(seriesCover);

const index = {
  title: "Klokkentales",
  description: "Dramatic storybook dispatches from the public Lær Klokken chat.",
  disclosure: "Episodes may contain synthetic performances made with each narrator's permission.",
  feed: `${base}/feed.xml`,
  cover: `${base}/cover-1400.png`,
  episodes: metas.map((meta) => ({
    slug: meta.slug,
    title: meta.title,
    date: meta.date,
    description: meta.description,
    disclosure: meta.disclosure,
    cast: meta.cast,
    durationSec: meta.durationSec,
    audio: `${base}/${meta.audio}`,
    cover: `${base}/${meta.cover}`,
  })),
};
writeFileSync(resolve(OUT, "index.json"), JSON.stringify(index, null, 2) + "\n");

const xml = (value = "") => String(value)
  .replaceAll("&", "&amp;")
  .replaceAll("<", "&lt;")
  .replaceAll(">", "&gt;")
  .replaceAll('"', "&quot;")
  .replaceAll("'", "&apos;");
const hms = (seconds) => {
  const s = Math.max(0, Math.round(seconds || 0));
  return `${Math.floor(s / 3600)}:${String(Math.floor((s % 3600) / 60)).padStart(2, "0")}:${String(s % 60).padStart(2, "0")}`;
};

const items = metas.map((meta) => `    <item>
      <title>${xml(meta.title)}</title>
      <link>https://aesthetic.computer/klokkentales</link>
      <guid isPermaLink="false">klokkentales-${xml(meta.slug)}</guid>
      <pubDate>${xml(meta.pubDate)}</pubDate>
      <description>${xml(`${meta.description} ${meta.disclosure}`)}</description>
      <itunes:duration>${hms(meta.durationSec)}</itunes:duration>
      <itunes:explicit>false</itunes:explicit>
      <itunes:image href="${xml(`${base}/${meta.cover}`)}"/>
      <enclosure url="${xml(`${base}/${meta.audio}`)}" length="${meta.bytes}" type="audio/mpeg"/>
    </item>`).join("\n");

const lastBuildDate = metas[0]?.pubDate || new Date().toUTCString();
writeFileSync(resolve(OUT, "feed.xml"), `<?xml version="1.0" encoding="UTF-8"?>
<rss version="2.0" xmlns:itunes="http://www.itunes.com/dtds/podcast-1.0.dtd">
  <channel>
    <title>Klokkentales</title>
    <link>https://aesthetic.computer/klokkentales</link>
    <language>en</language>
    <description>Dramatic storybook dispatches from the public Lær Klokken chat.</description>
    <lastBuildDate>${xml(lastBuildDate)}</lastBuildDate>
    <itunes:author>Jeffrey &amp; Prutti</itunes:author>
    <itunes:type>episodic</itunes:type>
    <itunes:explicit>false</itunes:explicit>
    <itunes:image href="${xml(`${base}/cover-1400.png`)}"/>
    <itunes:category text="Arts"><itunes:category text="Books"/></itunes:category>
${items}
  </channel>
</rss>
`);

console.log(`${metas.length} episode${metas.length === 1 ? "" : "s"} -> ${resolve(OUT, "index.json")}`);
if (includeDrafts) console.log("included local drafts; do not publish this catalog");

