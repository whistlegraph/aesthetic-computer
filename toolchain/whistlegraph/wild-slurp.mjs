#!/usr/bin/env node
// wild-slurp.mjs — archive the media behind every /wild sighting so the
// field guide survives link rot. Reads the page's wild.json, fetches each
// entry's media by platform, stores it in the assets tree, and writes the
// media pointers back into wild.json for the page to render.
//
//   node toolchain/whistlegraph/wild-slurp.mjs [--force] [--only <substr>]
//
// Strategies:
//   youtube.com / youtu.be  → yt-dlp mp4 (≤720p, ≤300 MB) + jpg poster
//   reddit.com              → Pullpush submission JSON (reddit.com blocks
//                             plain fetches) + first preview image
//   everything else        → full HTML snapshot + og:image poster
//
// Media lands in system/public/assets/whistlegraph/wild/ (git-ignored) and
// ships via `npm run assets:sync:up` to assets.aesthetic.computer. wild.json
// only ever gains CDN URLs, so the site repo stays light.
import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import fs from "node:fs";
import path from "node:path";

const REPO = path.resolve(path.dirname(new URL(import.meta.url).pathname), "..", "..");
const WILD_JSON = path.join(REPO, "system/public/whistlegraph.org/wild/wild.json");
const MEDIA_DIR = path.join(REPO, "system/public/assets/whistlegraph/wild");
const CDN = "https://assets.aesthetic.computer/whistlegraph/wild";
const STAMP = new Date().toISOString().slice(0, 10);
const FORCE = process.argv.includes("--force");
const ONLY = process.argv.includes("--only")
  ? process.argv[process.argv.indexOf("--only") + 1]
  : null;
const UA = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0 Safari/537.36";

fs.mkdirSync(MEDIA_DIR, { recursive: true });
const data = JSON.parse(fs.readFileSync(WILD_JSON, "utf8"));

const entryId = (e) => {
  const hash = createHash("md5").update(e.url).digest("hex").slice(0, 6);
  const host = new URL(e.url).hostname.replace(/^www\./, "").split(".")[0];
  return `${(e.date || "undated").slice(0, 10)}-${host}-${hash}`;
};

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

async function grab(url, opts = {}) {
  const res = await fetch(url, {
    headers: { "user-agent": UA, ...(opts.headers || {}) },
    signal: AbortSignal.timeout(opts.timeout || 20000),
    redirect: "follow",
  });
  if (!res.ok) throw new Error(`HTTP ${res.status}`);
  return res;
}

function shrinkPoster(file) {
  // Cap posters at 900 px wide so the CDN payload stays light.
  try {
    execFileSync("sips", ["--resampleWidth", "900", file], { stdio: "ignore" });
  } catch { /* sips absent or image too small — keep as fetched */ }
}

async function savePoster(url, id) {
  const res = await grab(url);
  const buf = Buffer.from(await res.arrayBuffer());
  if (buf.length < 2000) throw new Error("poster too small to be real");
  const file = path.join(MEDIA_DIR, `${id}.jpg`);
  fs.writeFileSync(file, buf);
  shrinkPoster(file);
  return `${CDN}/${id}.jpg`;
}

async function slurpYouTube(e, id) {
  const out = path.join(MEDIA_DIR, `${id}.%(ext)s`);
  execFileSync("yt-dlp", [
    "-f", "bv*[height<=720]+ba/b[height<=720]/b",
    "--merge-output-format", "mp4",
    "--max-filesize", "300m",
    "--write-thumbnail", "--convert-thumbnails", "jpg",
    "--no-playlist", "--no-progress", "-q",
    "-o", out, e.url,
  ], { stdio: ["ignore", "inherit", "inherit"], timeout: 600000 });
  const video = path.join(MEDIA_DIR, `${id}.mp4`);
  if (!fs.existsSync(video)) throw new Error("yt-dlp produced no mp4 (filesize cap?)");
  const media = { kind: "video", video: `${CDN}/${id}.mp4`, slurped: STAMP };
  if (fs.existsSync(path.join(MEDIA_DIR, `${id}.jpg`))) media.poster = `${CDN}/${id}.jpg`;
  media.bytes = fs.statSync(video).size;
  return media;
}

async function slurpReddit(e, id) {
  const m = e.url.match(/\/comments\/([a-z0-9]+)/i);
  if (!m) throw new Error("no thread id in url");
  const res = await grab(
    `https://api.pullpush.io/reddit/search/submission/?ids=${m[1]}`,
    { timeout: 30000 },
  );
  const body = await res.json();
  const post = body?.data?.[0];
  if (!post) throw new Error("pullpush returned no submission");
  fs.writeFileSync(
    path.join(MEDIA_DIR, `${id}.reddit.json`),
    JSON.stringify(post, null, 1),
  );
  const media = { kind: "thread", archive: `${CDN}/${id}.reddit.json`, slurped: STAMP };
  const img =
    post.preview?.images?.[0]?.source?.url?.replaceAll("&amp;", "&") ||
    (/\.(jpg|jpeg|png|webp)$/i.test(post.url_overridden_by_dest || "")
      ? post.url_overridden_by_dest
      : null);
  if (img) {
    try { media.poster = await savePoster(img, id); } catch { /* thread text survives regardless */ }
  }
  await sleep(1200); // pullpush is a communal resource
  return media;
}

async function slurpPage(e, id) {
  const res = await grab(e.url);
  const html = await res.text();
  fs.writeFileSync(path.join(MEDIA_DIR, `${id}.html`), html);
  const media = { kind: "page", archive: `${CDN}/${id}.html`, slurped: STAMP };
  const og =
    html.match(/property=["']og:image["'][^>]*content=["']([^"']+)["']/i)?.[1] ||
    html.match(/content=["']([^"']+)["'][^>]*property=["']og:image["']/i)?.[1];
  if (og) {
    try {
      media.poster = await savePoster(new URL(og, e.url).href, id);
    } catch { /* snapshot alone still counts */ }
  }
  return media;
}

async function slurpFandom(e, id) {
  // fandom.com 403s plain page fetches, but its MediaWiki API is open.
  const u = new URL(e.url);
  const page = decodeURIComponent(u.pathname.replace(/^\/wiki\//, ""));
  const api = `${u.origin}/api.php`;
  const res = await grab(`${api}?action=parse&page=${encodeURIComponent(page)}&format=json&prop=text`);
  const body = await res.json();
  const html = body?.parse?.text?.["*"];
  if (!html) throw new Error("mediawiki parse returned no text");
  fs.writeFileSync(path.join(MEDIA_DIR, `${id}.html`),
    `<!doctype html><meta charset="utf-8"><title>${body.parse.title}</title>\n${html}`);
  const media = { kind: "page", archive: `${CDN}/${id}.html`, slurped: STAMP };
  try {
    const qi = await (await grab(
      `${api}?action=query&titles=${encodeURIComponent(page)}&prop=pageimages&piprop=original&format=json`,
    )).json();
    const pages = qi?.query?.pages || {};
    const orig = Object.values(pages)[0]?.original?.source;
    if (orig) media.poster = await savePoster(orig, id);
  } catch { /* text archive stands on its own */ }
  return media;
}

const isYouTube = (u) => /(^|\.)((youtube\.com)|(youtu\.be))$/.test(new URL(u).hostname.replace(/^www\./, ""));
const isReddit = (u) => /(^|\.)reddit\.com$/.test(new URL(u).hostname.replace(/^www\./, ""));
const isFandom = (u) => /(^|\.)fandom\.com$/.test(new URL(u).hostname.replace(/^www\./, ""));

let ok = 0, skipped = 0, failed = 0;
const failures = [];
for (const section of data.sections || []) {
  for (const e of section.entries || []) {
    if (ONLY && !e.url.includes(ONLY) && !e.title.includes(ONLY)) continue;
    if (e.media && !FORCE) { skipped += 1; continue; }
    const id = entryId(e);
    process.stdout.write(`· ${id}  ${e.title.slice(0, 56)}\n`);
    try {
      if (isYouTube(e.url)) e.media = await slurpYouTube(e, id);
      else if (isReddit(e.url)) e.media = await slurpReddit(e, id);
      else if (isFandom(e.url)) e.media = await slurpFandom(e, id);
      else e.media = await slurpPage(e, id);
      ok += 1;
    } catch (err) {
      failed += 1;
      failures.push(`${id}  ${e.url}  →  ${err.message}`);
      process.stdout.write(`  ✗ ${err.message}\n`);
    }
  }
}

fs.writeFileSync(WILD_JSON, JSON.stringify(data, null, 1) + "\n");
console.log(`\nslurped ${ok} · skipped ${skipped} (already archived) · failed ${failed}`);
if (failures.length) {
  console.log("failures:");
  for (const f of failures) console.log("  " + f);
}
console.log(`media → ${MEDIA_DIR}\nnext: npm run assets:sync:up  (then commit wild.json + deploy)`);
