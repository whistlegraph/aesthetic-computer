#!/usr/bin/env node
// Build a one-off Kunaki jewel-case CD kit from a physical-release manifest.

import { createHash } from "node:crypto";
import { execFileSync } from "node:child_process";
import {
  copyFileSync,
  existsSync,
  mkdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { basename, dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { CD_SPECS } from "../../../marketing/podcast/lib/kunaki.mjs";
import { proof } from "../lib/kit.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "../../..");
const argv = process.argv.slice(2);
const value = (name, fallback) => {
  const index = argv.indexOf(`--${name}`);
  return index === -1 ? fallback : argv[index + 1];
};
const force = argv.includes("--force");
const manifestPath = resolve(value("manifest", resolve(HERE, "../pixsies-so-far.json")));
const source = JSON.parse(readFileSync(manifestPath, "utf8"));
const out = resolve(value("out", resolve(HERE, "../out/pixsies-so-far-kunaki-cd")));
// The masters pipeline is the preferred audio: Red Book WAVs, no second
// encode between the release master and the pressed disc.
const mastersDir = resolve(value("masters", resolve(HERE, "../masters")));
const mastersManifest = resolve(mastersDir, "manifest.json");
const masters = argv.includes("--mp3") || !existsSync(mastersManifest)
  ? null
  : JSON.parse(readFileSync(mastersManifest, "utf8"));
const audioDir = resolve(out, "audio");
const coverDir = resolve(out, ".covers");
rmSync(audioDir, { recursive: true, force: true });
mkdirSync(audioDir, { recursive: true });
mkdirSync(coverDir, { recursive: true });

const esc = (text) => String(text).replace(/[&<>"']/g, (character) => ({
  "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&apos;",
}[character]));

async function download(url, path) {
  if (!force && existsSync(path)) return;
  const response = await fetch(url);
  if (!response.ok) throw new Error(`${response.status} downloading ${url}`);
  writeFileSync(path, Buffer.from(await response.arrayBuffer()));
}

const duration = (path) => Number(execFileSync("ffprobe", [
  "-v", "error",
  "-show_entries", "format=duration",
  "-of", "default=nw=1:nk=1",
  path,
], { encoding: "utf8" }).trim());

const clock = (seconds) => {
  const rounded = Math.round(seconds);
  return `${Math.floor(rounded / 60)}:${String(rounded % 60).padStart(2, "0")}`;
};

const digest = (path) => createHash("sha256").update(readFileSync(path)).digest("hex");

const coverFiles = [];
const tracks = [];
for (const [index, track] of source.tracks.entries()) {
  const number = String(index + 1).padStart(2, "0");
  const master = masters?.tracks.find((entry) => entry.slug === track.slug);
  if (masters && !master) throw new Error(`No master for ${track.slug}; rebuild with bin/masters.mjs`);
  const audioPath = resolve(audioDir, master ? master.file : `${number}-${track.slug}.mp3`);
  const coverPath = resolve(coverDir, `${number}-${track.slug}.jpg`);
  process.stdout.write(`\r${number}/${String(source.tracks.length).padStart(2, "0")} ${track.slug.padEnd(22)}`);
  if (master) copyFileSync(resolve(mastersDir, master.file), audioPath);
  else await download(track.audio, audioPath);
  await download(track.cover, coverPath);
  const seconds = duration(audioPath);
  if (!(seconds > 0)) throw new Error(`Unreadable audio: ${audioPath}`);
  tracks.push({
    position: index + 1,
    slug: track.slug,
    title: track.title,
    file: `audio/${basename(audioPath)}`,
    source: master ? master.from : "released-mp3",
    lossy: master ? master.lossy : true,
    sourceUrl: master ? undefined : track.audio,
    durationSeconds: seconds,
    sha256: digest(audioPath),
  });
  coverFiles.push(coverPath);
}
process.stdout.write("\n");

const gapSeconds = Math.max(0, tracks.length - 1) * CD_SPECS.audio.gapSeconds;
const audioSeconds = tracks.reduce((sum, track) => sum + track.durationSeconds, 0);
const programSeconds = audioSeconds + gapSeconds;
if (tracks.length > CD_SPECS.audio.maxTracks) {
  throw new Error(`${tracks.length} tracks exceed Kunaki's ${CD_SPECS.audio.maxTracks}-track limit`);
}
if (programSeconds > CD_SPECS.audio.maxMinutes * 60) {
  throw new Error(`${clock(programSeconds)} exceeds Kunaki's ${CD_SPECS.audio.maxMinutes}-minute limit`);
}

// Keep the SVGs comfortably below libxml's single-line size ceiling. The source
// covers are commonly 3000px JPEGs, while no mosaic cell exceeds 600px.
const embeddedCovers = coverFiles.map((path, index) => {
  const previewPath = resolve(coverDir, `.mosaic-${String(index + 1).padStart(2, "0")}.jpg`);
  if (force || !existsSync(previewPath)) {
    execFileSync("magick", [path, "-resize", "640x640^", "-gravity", "center", "-extent", "640x640", "-quality", "88", previewPath]);
  }
  return `data:image/jpeg;base64,${readFileSync(previewPath).toString("base64")}`;
});
const palette = ["#ff6b9d", "#4ecdc4", "#ffe66d", "#8ce99a"];
const textPalette = ["#e0447c", "#1f9d94", "#b07d00", "#3f9a55"];

function mosaic(width, height, opacity = 1) {
  const cells = [];
  const columns = 3;
  const rows = 3;
  const cellWidth = width / columns;
  const cellHeight = height / rows;
  for (let index = 0; index < embeddedCovers.length; index++) {
    const x = (index % columns) * cellWidth;
    const y = Math.floor(index / columns) * cellHeight;
    cells.push(`<svg x="${x}" y="${y}" width="${cellWidth + 1}" height="${cellHeight + 1}" overflow="hidden"><image href="${embeddedCovers[index]}" width="100%" height="100%" preserveAspectRatio="xMidYMid slice" opacity="${opacity}"/></svg>`);
  }
  return cells.join("");
}

function svg(width, height, body) {
  return `<svg xmlns="http://www.w3.org/2000/svg" width="${width}" height="${height}" viewBox="0 0 ${width} ${height}"><rect width="100%" height="100%" fill="#fff9fc"/>${body}</svg>`;
}

function render(name, spec, body) {
  const sourcePath = resolve(out, `.${name}.svg`);
  const pngPath = resolve(out, `.${name}.png`);
  const jpgPath = resolve(out, `${name}.jpg`);
  writeFileSync(sourcePath, svg(spec.width, spec.height, body));
  execFileSync("rsvg-convert", ["--width", String(spec.width), "--height", String(spec.height), "--output", pngPath, sourcePath]);
  execFileSync("magick", [pngPath, "-units", "PixelsPerInch", "-density", String(CD_SPECS.artwork.dpi), "-quality", "96", jpgPath]);
  return basename(jpgPath);
}

const title = esc(source.title);
const artist = esc(source.artist);
const front = render("front-cover", CD_SPECS.artwork.frontCover, `${mosaic(CD_SPECS.artwork.frontCover.width, CD_SPECS.artwork.frontCover.height)}
  <rect x="118" y="510" width="1187" height="390" rx="8" fill="#fff9fc" opacity=".94"/>
  <text x="711.5" y="660" text-anchor="middle" font-family="monospace" font-size="82" font-weight="700" fill="#282430">${title}</text>
  <text x="711.5" y="760" text-anchor="middle" font-family="monospace" font-size="36" fill="#b44887">${artist}</text>`);

const insertRows = tracks.map((track, index) => {
  const x = index < 5 ? 94 : 770;
  const y = 250 + (index % 5) * 166;
  return `<text x="${x}" y="${y}" font-family="monospace" font-size="34" fill="#282430"><tspan fill="${textPalette[index % textPalette.length]}">${String(track.position).padStart(2, "0")}</tspan><tspan dx="22">${esc(track.title)}</tspan></text><text x="${x + 560}" y="${y}" text-anchor="end" font-family="monospace" font-size="28" fill="#777">${clock(track.durationSeconds)}</text>`;
}).join("");
const insert = render("insert", CD_SPECS.artwork.insert, `
  <rect x="0" y="0" width="356" height="24" fill="#ff6b9d"/><rect x="356" y="0" width="356" height="24" fill="#4ecdc4"/><rect x="712" y="0" width="356" height="24" fill="#ffe66d"/><rect x="1068" y="0" width="355" height="24" fill="#8ce99a"/>
  <text x="94" y="110" font-family="monospace" font-size="48" font-weight="700" fill="#282430">${title}</text>
  ${insertRows}
  <text x="94" y="1295" font-family="monospace" font-size="26" fill="#777">aesthetic.computer/pop · ${esc(source.asOf)}</text>`);

const trayRows = tracks.map((track, index) => {
  const y = 245 + index * 92;
  return `<text x="170" y="${y}" font-family="monospace" font-size="38" fill="#282430"><tspan fill="${textPalette[index % textPalette.length]}">${String(track.position).padStart(2, "0")}</tspan><tspan dx="28">${esc(track.title)}</tspan></text><text x="1580" y="${y}" text-anchor="end" font-family="monospace" font-size="32" fill="#777">${clock(track.durationSeconds)}</text>`;
}).join("");
const tray = render("tray-card", CD_SPECS.artwork.trayCard, `
  <rect width="74" height="1385" fill="#282430"/><rect x="1698" width="74" height="1385" fill="#282430"/>
  <text x="45" y="692" text-anchor="middle" transform="rotate(-90 45 692)" font-family="monospace" font-size="28" fill="#fff9fc">${artist} · ${title}</text>
  <text x="1727" y="692" text-anchor="middle" transform="rotate(90 1727 692)" font-family="monospace" font-size="28" fill="#fff9fc">${artist} · ${title}</text>
  <text x="170" y="116" font-family="monospace" font-size="52" font-weight="700" fill="#282430">${artist}</text>
  ${trayRows}
  <text x="170" y="1244" font-family="monospace" font-size="24" fill="#777">© ℗ 2026 Aesthetic Dot Computer · @jeffrey / aesthetic.computer</text>`);

const discSpec = CD_SPECS.artwork.disc;
const disc = render("disc", discSpec, `${mosaic(discSpec.width, discSpec.height, 0.72)}
  <circle cx="697" cy="697" r="372" fill="#fff9fc" opacity=".96"/>
  <circle cx="697" cy="697" r="132" fill="#4ecdc4" opacity=".88"/>
  <circle cx="697" cy="697" r="54" fill="#fff9fc"/>
  <text x="697" y="594" text-anchor="middle" font-family="monospace" font-size="60" font-weight="700" fill="#282430">${title}</text>
  <text x="697" y="654" text-anchor="middle" font-family="monospace" font-size="27" fill="#b44887">${artist}</text>`);

const production = {
  schemaVersion: 1,
  state: "prepared",
  preparedAt: new Date().toISOString(),
  title: source.title,
  artist: source.artist,
  asOf: source.asOf,
  sourceManifest: basename(manifestPath),
  sourceAudio: masters
    ? `Red Book masters from bin/masters.mjs (${masters.lossless} of ${masters.tracks.length} lossless).`
    : "Public 320 kbps MP3 release artifacts.",
  program: {
    trackCount: tracks.length,
    audioSeconds,
    gapSeconds,
    totalSeconds: programSeconds,
    displayDuration: clock(programSeconds),
    tracks,
  },
  artwork: { front, insert, tray, disc, proof: proof(out, [front, insert, tray, disc]) },
  vendor: {
    name: "kunaki",
    productType: "audio-cd-jewel-case",
    productId: null,
    productCreation: "manual-browser-upload",
    fulfillment: "http-api",
    publishedUnitPriceUsd: CD_SPECS.priceUsd,
  },
  specs: CD_SPECS,
};
writeFileSync(resolve(out, "manifest.json"), `${JSON.stringify(production, null, 2)}\n`);
writeFileSync(resolve(out, "README.md"), `# ${source.title}\n\nUpload the numbered ${masters ? "WAV" : "MP3"} files in \`audio/\` as an Audio CD, in filename order. Select a jewel case and upload \`${front}\`, \`${insert}\`, \`${tray}\`, and \`${disc}\`. Choose no barcode for this one-off proof. Inspect the virtual disc before publishing.\n\nKunaki product creation is the one manual step. Put the resulting 10-character product ID in \`manifest.json\` under \`vendor.productId\`. Keep recipient and credential files outside the repository. Then request live shipping options and submit the approved order through the existing client:\n\n\`\`\`sh\nnode marketing/podcast/bin/kunaki.mjs shipping /private/path/shipping.json\nKUNAKI_USER_ID=... KUNAKI_PASSWORD=... node marketing/podcast/bin/kunaki.mjs order /private/path/order.json\n\`\`\`\n\nOrders default to test mode. A live API order additionally requires \`KUNAKI_ALLOW_LIVE=1\`. Kunaki requires the account to be funded separately and does not allow funded orders to be cancelled.\n`);

console.log(`${out}\n${tracks.length} tracks · ${clock(programSeconds)} including ${gapSeconds}s of vendor-inserted gaps`);
