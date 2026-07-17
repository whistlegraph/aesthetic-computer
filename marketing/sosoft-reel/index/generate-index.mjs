#!/usr/bin/env node
// Rebuild the Social Software media catalog without altering source media.
import fs from "node:fs";
import path from "node:path";
import os from "node:os";
import crypto from "node:crypto";
import { execFileSync } from "node:child_process";

const downloads = "/Users/jas/Downloads";
const here = path.dirname(new URL(import.meta.url).pathname);
const thumbs = path.join(here, "thumbs");
const archiveName = "Social Software — Cycle 2 — Scores for Social Software-1-001.zip";
const archivePath = path.join(downloads, archiveName);
const temp = fs.mkdtempSync(path.join(os.tmpdir(), "sosoft-index-"));
fs.mkdirSync(thumbs, { recursive: true });

const run = (cmd, args) => execFileSync(cmd, args, { encoding: "utf8", maxBuffer: 64 * 1024 * 1024 });
// Use the streaming system utility so multi-gigabyte source videos never enter JS memory.
const sha = file => run("shasum", ["-a", "256", file]).trim().split(/\s+/)[0];
const csv = value => `"${String(value ?? "").replaceAll('"', '""')}"`;
const esc = value => String(value ?? "").replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;").replaceAll('"', "&quot;");

function exif(file) {
  try { return JSON.parse(run("exiftool", ["-json", "-n", file]))[0] ?? {}; }
  catch { return {}; }
}

function probe(file) {
  try {
    const data = JSON.parse(run("ffprobe", ["-v", "error", "-show_entries", "format=duration:stream=codec_type,width,height,r_frame_rate", "-of", "json", file]));
    const video = data.streams?.find(s => s.codec_type === "video") ?? {};
    return { width: video.width, height: video.height, durationSeconds: Number(data.format?.duration || 0) || null };
  } catch { return {}; }
}

function thumbImage(source, destination) {
  if (fs.existsSync(destination)) return;
  run("magick", [source, "-auto-orient", "-thumbnail", "480x480>", "-strip", "-quality", "76", destination]);
}

function thumbVideo(source, destination, duration) {
  if (fs.existsSync(destination)) return;
  const at = String(Math.min(Math.max((duration || 3) * 0.12, 0.5), 8));
  run("ffmpeg", ["-v", "error", "-ss", at, "-i", source, "-frames:v", "1", "-vf", "scale=480:480:force_original_aspect_ratio=decrease", "-q:v", "4", "-y", destination]);
}

const loose = fs.readdirSync(downloads).filter(name =>
  /^IMG_.*\.(heic|mov)$/i.test(name) ||
  /^ChellyJin_SoSoftPresentation(?: \(\d+\))?\.mp4$/i.test(name) ||
  ["barking.MP4.mp4", "Biophonia_SoSoft_Fuser_V2.mp4"].includes(name)
).sort((a, b) => a.localeCompare(b, undefined, { numeric: true }));

// macOS Archive Utility's `ditto` tolerates the ZIP's malformed legacy filename encoding.
run("ditto", ["-x", "-k", archivePath, temp]);
const zipImages = [];
function walk(dir) {
  for (const ent of fs.readdirSync(dir, { withFileTypes: true })) {
    const full = path.join(dir, ent.name);
    if (ent.isDirectory()) walk(full);
    else if (/\.jpe?g$/i.test(ent.name)) zipImages.push(full);
  }
}
walk(temp);
zipImages.sort((a, b) => path.basename(a).localeCompare(path.basename(b), undefined, { numeric: true }));

const items = [];
let sequence = 1;
function add(source, provenance, archiveMember = null) {
  const name = path.basename(source);
  const isVideo = /\.(mov|mp4)$/i.test(name);
  const metadata = exif(source);
  const av = isVideo ? probe(source) : {};
  const digest = sha(source);
  const thumbName = `${String(sequence).padStart(3, "0")}-${name.replace(/[^a-z0-9._-]/gi, "-").replace(/\.[^.]+$/, "")}.jpg`;
  const thumbPath = path.join(thumbs, thumbName);
  if (isVideo) thumbVideo(source, thumbPath, av.durationSeconds);
  else thumbImage(source, thumbPath);
  const captureDate = metadata.DateTimeOriginal || metadata.CreateDate || metadata.MediaCreateDate || null;
  items.push({
    id: `SS-${String(sequence).padStart(3, "0")}`,
    filename: name,
    mediaType: isVideo ? "video" : "image",
    provenance,
    source: archiveMember ? archiveName : source,
    archiveMember,
    captureDate,
    width: av.width || metadata.ImageWidth || metadata.ExifImageWidth || null,
    height: av.height || metadata.ImageHeight || metadata.ExifImageHeight || null,
    durationSeconds: av.durationSeconds || metadata.Duration || null,
    bytes: fs.statSync(source).size,
    sha256: digest,
    duplicateOf: null,
    thumbnail: `thumbs/${thumbName}`,
    canonicalWork: "",
    status: "unreviewed",
    notes: ""
  });
  sequence++;
}

for (const name of loose) add(path.join(downloads, name), "today-loose-shoot");
for (const file of zipImages) add(file, "fuser-presentation-zip", path.relative(temp, file));

const firstByHash = new Map();
for (const item of items) {
  if (firstByHash.has(item.sha256)) item.duplicateOf = firstByHash.get(item.sha256);
  else firstByHash.set(item.sha256, item.id);
}

const sourceCounts = Object.fromEntries([...new Set(items.map(x => x.provenance))].map(group => [group, items.filter(x => x.provenance === group).length]));
const manifest = {
  schemaVersion: 1,
  title: "Scores for Social Software — media index",
  generatedAt: new Date().toISOString(),
  sourceRoot: downloads,
  sources: {
    "today-loose-shoot": { description: "Loose Downloads media from the current unboxing/documentation shoot", count: sourceCounts["today-loose-shoot"] },
    "fuser-presentation-zip": { description: "Photographs from the presentation at Fuser offices a few weeks earlier", archive: archiveName, archiveSha256: sha(archivePath), count: sourceCounts["fuser-presentation-zip"] }
  },
  counts: { total: items.length, images: items.filter(x => x.mediaType === "image").length, videos: items.filter(x => x.mediaType === "video").length, exactDuplicates: items.filter(x => x.duplicateOf).length },
  items
};
fs.writeFileSync(path.join(here, "manifest.json"), JSON.stringify(manifest, null, 2) + "\n");

const headers = ["id", "filename", "mediaType", "provenance", "source", "archiveMember", "captureDate", "width", "height", "durationSeconds", "bytes", "sha256", "duplicateOf", "thumbnail", "canonicalWork", "status", "notes"];
fs.writeFileSync(path.join(here, "manifest.csv"), [headers.map(csv).join(","), ...items.map(item => headers.map(h => csv(item[h])).join(","))].join("\n") + "\n");

const cards = items.map(item => `<article class="card" data-group="${esc(item.provenance)}" data-search="${esc(`${item.id} ${item.filename} ${item.captureDate || ""}`.toLowerCase())}">
  <img loading="lazy" src="${esc(item.thumbnail)}" alt="Thumbnail for ${esc(item.filename)}">
  <div class="meta"><b>${esc(item.id)}</b> ${esc(item.filename)}<br><small>${esc(item.provenance)} · ${esc(item.width)}×${esc(item.height)}${item.durationSeconds ? ` · ${Number(item.durationSeconds).toFixed(1)}s` : ""}<br>${esc(item.captureDate || "date unavailable")}</small></div>
  <dl><dt>Canonical work</dt><dd class="blank">—</dd><dt>Status</dt><dd>${esc(item.status)}</dd><dt>Notes</dt><dd class="blank">—</dd></dl>
</article>`).join("\n");

const html = `<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width"><title>${esc(manifest.title)}</title><style>
:root{color-scheme:light dark;font:15px/1.35 system-ui,sans-serif}body{margin:0;background:#111;color:#eee}header{position:sticky;z-index:2;top:0;padding:16px 22px;background:#111e;border-bottom:1px solid #555;backdrop-filter:blur(12px)}h1{font-size:22px;margin:0 0 6px}.summary{color:#bbb}.tools{display:flex;gap:8px;margin-top:10px;flex-wrap:wrap}input,select{padding:8px;border-radius:5px;border:1px solid #777;background:#222;color:#fff}main{display:grid;grid-template-columns:repeat(auto-fill,minmax(220px,1fr));gap:12px;padding:16px}.card{background:#202020;border:1px solid #3b3b3b;border-radius:8px;overflow:hidden}.card img{width:100%;height:220px;object-fit:contain;background:#080808}.meta,dl{padding:9px 11px;margin:0}.meta{overflow-wrap:anywhere}small{color:#aaa}dl{display:grid;grid-template-columns:7em 1fr;font-size:12px;border-top:1px solid #3b3b3b}dt{color:#999}dd{margin:0}.blank{color:#666}.hidden{display:none}a{color:#8bd}</style></head><body>
<header><h1>${esc(manifest.title)}</h1><div class="summary">${manifest.counts.total} assets: ${manifest.counts.images} images, ${manifest.counts.videos} videos · ${sourceCounts["today-loose-shoot"]} loose shoot · ${sourceCounts["fuser-presentation-zip"]} presentation archive · ${manifest.counts.exactDuplicates} exact duplicates</div><div class="tools"><input id="q" type="search" placeholder="Search ID, filename, date"><select id="group"><option value="">All sources</option><option value="today-loose-shoot">Today / loose shoot</option><option value="fuser-presentation-zip">Fuser presentation ZIP</option></select><a href="manifest.json">JSON</a><a href="manifest.csv">CSV</a></div></header><main>${cards}</main>
<script>const q=document.querySelector('#q'),g=document.querySelector('#group'),cards=[...document.querySelectorAll('.card')];function filter(){const s=q.value.toLowerCase();for(const c of cards)c.classList.toggle('hidden',!!((s&&!c.dataset.search.includes(s))||(g.value&&c.dataset.group!==g.value)))}q.oninput=g.onchange=filter;</script></body></html>`;
fs.writeFileSync(path.join(here, "index.html"), html);
fs.rmSync(temp, { recursive: true, force: true });
console.log(JSON.stringify(manifest.counts));
