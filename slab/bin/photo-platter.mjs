#!/usr/bin/env node
// photo-platter.mjs — read-only, local-first search over Apple Photos.

import { DatabaseSync } from "node:sqlite";
import { execFile } from "node:child_process";
import { access, chmod, mkdir, readFile, writeFile } from "node:fs/promises";
import { constants as fsConstants } from "node:fs";
import { homedir } from "node:os";
import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

process.umask(0o077);

const pexec = promisify(execFile);
const SCRIPT_DIR = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(SCRIPT_DIR, "../..");
const DEFAULT_LIBRARY = join(homedir(), "Pictures", "Photos Library.photoslibrary");
const DEFAULT_STATE = join(homedir(), "Library", "Application Support", "Aesthetic Computer", "photo-platter");
const BRIEFS_PATH = join(REPO, "pop", "photo-cover-briefs.json");
const APPLE_EPOCH_MS = 978307200000;
const SAFE_CATEGORIES = new Set([
  14,   // body of water
  1200, // user keywords
  1201, // user title
  1202, // user description
  1500, // Apple scene label
  1600, // activity
  1700, // venue
  1701, // venue type
]);

function arg(name, fallback = null) {
  const i = process.argv.indexOf(`--${name}`);
  return i >= 0 && process.argv[i + 1] ? process.argv[i + 1] : fallback;
}

function flag(name) { return process.argv.includes(`--${name}`); }

async function exists(path) {
  try { await access(path, fsConstants.F_OK); return true; }
  catch { return false; }
}

function paths(options = {}) {
  const library = resolve(options.library || process.env.AC_PHOTOS_LIBRARY || DEFAULT_LIBRARY);
  const state = resolve(options.state || process.env.AC_PHOTO_PLATTER_STATE || DEFAULT_STATE);
  return {
    library,
    state,
    photosDb: join(library, "database", "Photos.sqlite"),
    searchDb: join(library, "database", "search", "psi.sqlite"),
  };
}

function int64le(value) {
  let n = BigInt(value);
  if (n < 0n) n += 1n << 64n;
  const bytes = Buffer.alloc(8);
  bytes.writeBigUInt64LE(n);
  return bytes;
}

function intsToUuid(a, b) {
  const hex = Buffer.concat([int64le(a), int64le(b)]).toString("hex");
  return `${hex.slice(0, 8)}-${hex.slice(8, 12)}-${hex.slice(12, 16)}-${hex.slice(16, 20)}-${hex.slice(20)}`.toUpperCase();
}

function uuidToInts(uuid) {
  const bytes = Buffer.from(String(uuid).replaceAll("-", ""), "hex");
  const signed = (offset) => {
    const value = bytes.readBigUInt64LE(offset);
    return value >= 1n << 63n ? value - (1n << 64n) : value;
  };
  return [signed(0), signed(8)];
}

function termsFrom(query) {
  return [...new Set(String(query || "").toLowerCase().match(/[\p{L}\p{N}][\p{L}\p{N}'-]*/gu) || [])]
    .filter((term) => term.length > 1)
    .slice(0, 24);
}

function openDatabases(options = {}) {
  const p = paths(options);
  const photos = new DatabaseSync(p.photosDb, { readOnly: true });
  const search = new DatabaseSync(p.searchDb, { readOnly: true });
  return { ...p, photos, search };
}

function appleDate(seconds) {
  if (seconds === null || seconds === undefined) return null;
  return new Date(APPLE_EPOCH_MS + Number(seconds) * 1000).toISOString();
}

function chunk(values, size = 400) {
  const out = [];
  for (let i = 0; i < values.length; i += size) out.push(values.slice(i, i + size));
  return out;
}

function compactCandidate(row, match, library) {
  const originalPath = row.ZDIRECTORY && row.ZFILENAME
    ? join(library, "originals", row.ZDIRECTORY, row.ZFILENAME)
    : null;
  const previewPath = row.ZUUID
    ? join(library, "resources", "derivatives", "masters", row.ZUUID[0], `${row.ZUUID}_4_5005_c.jpeg`)
    : null;
  const aspect = row.ZWIDTH && row.ZHEIGHT ? Number(row.ZWIDTH) / Number(row.ZHEIGHT) : null;
  const cropPenalty = aspect ? Math.abs(Math.log(aspect)) : 2;
  const aesthetics = Number(row.ZOVERALLAESTHETICSCORE || 0);
  const coverage = match.terms.size;
  const semantic = [...match.termWeights.values()].reduce((sum, value) => sum + value, 0);
  const rank = semantic + coverage * 3 + aesthetics * 8 - cropPenalty * 1.4 + (row.ZFAVORITE ? 1.5 : 0);
  return {
    id: row.ZUUID,
    date: appleDate(row.ZDATECREATED),
    width: Number(row.ZWIDTH || 0),
    height: Number(row.ZHEIGHT || 0),
    aspect: aspect ? Number(aspect.toFixed(3)) : null,
    favorite: Boolean(row.ZFAVORITE),
    aesthetics: Number(aesthetics.toFixed(3)),
    rank: Number(rank.toFixed(3)),
    matchedTerms: [...match.terms].sort(),
    matchedLabels: [...match.labels].sort().slice(0, 12),
    originalPath,
    previewPath,
    importBundle: row.ZIMPORTBUNDLE || null,
    path: originalPath,
  };
}

function isCaptureSource(candidate) {
  if (!candidate.importBundle) return true;
  return /snapbridge|imagecapture|photos/i.test(candidate.importBundle);
}

function selfOnlyCandidates(candidates, photos) {
  const me = photos.prepare(`
    SELECT ZPERSONUUID FROM ZPERSON
    WHERE coalesce(ZISMECONFIDENCE,0)>0
    ORDER BY ZISMECONFIDENCE DESC, ZFACECOUNT DESC
    LIMIT 1
  `).get();
  if (!me?.ZPERSONUUID) throw new Error("Apple Photos has no local ‘me’ face cluster.");
  const safety = new Map();
  for (const ids of chunk(candidates.map((candidate) => candidate.id))) {
    const placeholders = ids.map(() => "?").join(",");
    const rows = photos.prepare(`
      SELECT asset.ZUUID,
        count(face.Z_PK) AS faceCount,
        sum(CASE WHEN person.ZPERSONUUID=? THEN 1 ELSE 0 END) AS selfFaceCount
      FROM ZASSET asset
      LEFT JOIN ZDETECTEDFACE face
        ON face.ZASSETFORFACE=asset.Z_PK
        AND coalesce(face.ZHIDDEN,0)=0
        AND coalesce(face.ZISINTRASH,0)=0
      LEFT JOIN ZPERSON person ON person.Z_PK=face.ZPERSONFORFACE
      WHERE asset.ZUUID IN (${placeholders})
      GROUP BY asset.ZUUID
    `).all(me.ZPERSONUUID, ...ids);
    for (const row of rows) safety.set(row.ZUUID, {
      faceCount: Number(row.faceCount || 0),
      selfFaceCount: Number(row.selfFaceCount || 0),
    });
  }
  return candidates.flatMap((candidate) => {
    const counts = safety.get(candidate.id) || { faceCount: 0, selfFaceCount: 0 };
    if (counts.faceCount !== 0 && counts.faceCount !== counts.selfFaceCount) return [];
    return [{ ...candidate, people: counts.faceCount ? "self-only" : "none-detected" }];
  });
}

function excludeUnverifiedPeople(candidates, search) {
  const assetQuery = search.prepare("SELECT rowid FROM assets WHERE uuid_0=? AND uuid_1=?");
  const labelQuery = search.prepare(`
    SELECT DISTINCT groups.content_string AS label
    FROM ga JOIN groups ON groups.rowid=ga.groupid
    WHERE ga.assetid=? AND groups.category=1500
  `);
  const humanLabel = /^(people|person|portrait|self-portrait|crowd|swimming|watersports)$/i;
  return candidates.filter((candidate) => {
    if (candidate.people === "self-only") return true;
    const asset = assetQuery.get(...uuidToInts(candidate.id));
    if (!asset) return true;
    return !labelQuery.all(asset.rowid)
      .some((row) => humanLabel.test(String(row.label || "").replaceAll("\0", "").trim()));
  });
}

export async function photoStatus(options = {}) {
  const db = openDatabases(options);
  try {
    const assets = db.photos.prepare(`
      SELECT count(*) AS total,
        sum(CASE WHEN ZKIND=0 THEN 1 ELSE 0 END) AS photos,
        sum(CASE WHEN ZKIND=1 THEN 1 ELSE 0 END) AS videos,
        sum(CASE WHEN ZFAVORITE=1 THEN 1 ELSE 0 END) AS favorites,
        sum(CASE WHEN ZISDETECTEDSCREENSHOT=1 THEN 1 ELSE 0 END) AS screenshots
      FROM ZASSET WHERE coalesce(ZTRASHEDSTATE,0)=0
    `).get();
    const labeled = db.search.prepare("SELECT count(DISTINCT assetid) AS n FROM ga JOIN groups ON groups.rowid=ga.groupid WHERE groups.category=1500").get();
    return { library: db.library, state: db.state, ...assets, labeled: labeled.n, privacy: "local/read-only; people, OCR, and precise location excluded" };
  } finally { db.photos.close(); db.search.close(); }
}

export async function searchPhotos(query, options = {}) {
  const terms = Array.isArray(query) ? query.flatMap(termsFrom) : termsFrom(query);
  if (!terms.length) throw new Error("A search query is required.");
  const boostedTerms = new Set((options.boostTerms || []).flatMap(termsFrom));
  const limit = Math.max(1, Math.min(100, Number(options.limit) || 20));
  const db = openDatabases(options);
  try {
    const matches = new Map();
    const groupQuery = db.search.prepare(`
      SELECT rowid, category, content_string, normalized_string, coalesce(score,0) AS score
      FROM groups
      WHERE normalized_string = ? OR normalized_string LIKE ?
      LIMIT 1200
    `);
    const assetQuery = db.search.prepare(`
      SELECT assets.uuid_0, assets.uuid_1
      FROM ga JOIN assets ON assets.rowid=ga.assetid
      WHERE ga.groupid=?
      LIMIT 12000
    `);
    assetQuery.setReadBigInts(true);
    for (const term of terms) {
      const groups = groupQuery.all(term, `%${term}%`).filter((row) => SAFE_CATEGORIES.has(Number(row.category)));
      for (const group of groups) {
        const category = Number(group.category);
        const categoryWeight = category === 1500 ? 4 : category >= 1200 && category <= 1202 ? 5 : 3;
        for (const row of assetQuery.all(group.rowid)) {
          const id = intsToUuid(row.uuid_0, row.uuid_1);
          const match = matches.get(id) || { terms: new Set(), labels: new Set(), termWeights: new Map() };
          match.terms.add(term);
          if (group.content_string) match.labels.add(String(group.content_string).replaceAll("\0", ""));
          const boost = boostedTerms.has(term) ? 2.5 : 1;
          match.termWeights.set(term, Math.max(match.termWeights.get(term) || 0, (categoryWeight + Math.max(0, Number(group.score || 0))) * boost));
          matches.set(id, match);
        }
      }
    }
    if (!matches.size) return [];

    const candidates = [];
    for (const ids of chunk([...matches.keys()])) {
      const placeholders = ids.map(() => "?").join(",");
      const rows = db.photos.prepare(`
        SELECT ZUUID,ZDATECREATED,ZWIDTH,ZHEIGHT,ZFAVORITE,
          ZOVERALLAESTHETICSCORE,ZDIRECTORY,ZFILENAME,
          (SELECT ZIMPORTEDBYBUNDLEIDENTIFIER
            FROM ZADDITIONALASSETATTRIBUTES
            WHERE Z_PK=ZASSET.ZADDITIONALATTRIBUTES) AS ZIMPORTBUNDLE
        FROM ZASSET
        WHERE ZUUID IN (${placeholders})
          AND ZKIND=0
          AND coalesce(ZTRASHEDSTATE,0)=0
          AND coalesce(ZHIDDEN,0)=0
          AND coalesce(ZISDETECTEDSCREENSHOT,0)=0
          AND ZWIDTH >= 1000 AND ZHEIGHT >= 1000
      `).all(...ids);
      for (const row of rows) candidates.push(compactCandidate(row, matches.get(row.ZUUID), db.library));
    }
    const checked = await Promise.all(candidates.map(async (candidate) => {
      const originalLocal = Boolean(candidate.originalPath && await exists(candidate.originalPath));
      const previewLocal = Boolean(candidate.previewPath && await exists(candidate.previewPath));
      return {
        ...candidate,
        path: originalLocal ? candidate.originalPath : previewLocal ? candidate.previewPath : null,
        local: originalLocal || previewLocal,
        originalLocal,
        previewLocal,
      };
    }));
    const faceSafe = options.selfOnly ? selfOnlyCandidates(checked, db.photos) : checked;
    const peopleSafe = options.selfOnly ? excludeUnverifiedPeople(faceSafe, db.search) : faceSafe;
    const sorted = peopleSafe
      .filter((candidate) => candidate.local || options.includeCloud)
      .filter((candidate) => !options.captureOnly || isCaptureSource(candidate))
      .sort((a, b) => b.rank - a.rank || b.aesthetics - a.aesthetics || String(b.date).localeCompare(String(a.date)));
    const diverse = [];
    const overflow = [];
    const dayCounts = new Map();
    const maxPerDay = Math.max(1, Number(options.maxPerDay) || 2);
    for (const candidate of sorted) {
      const day = candidate.date?.slice(0, 10) || candidate.id;
      const count = dayCounts.get(day) || 0;
      if (count < maxPerDay) {
        diverse.push(candidate);
        dayCounts.set(day, count + 1);
      } else overflow.push(candidate);
    }
    return [...diverse, ...overflow].slice(0, limit);
  } finally { db.photos.close(); db.search.close(); }
}

async function secureDir(path) {
  await mkdir(path, { recursive: true, mode: 0o700 });
  await chmod(path, 0o700).catch(() => {});
}

function safeSlug(value) {
  return String(value || "search").toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "").slice(0, 80) || "search";
}

export async function makeSheet(query, options = {}) {
  const candidates = options.candidates || await searchPhotos(query, options);
  if (!candidates.length) throw new Error(`No local photo candidates match “${Array.isArray(query) ? query.join(" ") : query}”.`);
  const p = paths(options);
  const slug = safeSlug(options.slug || (Array.isArray(query) ? query.join("-") : query));
  const dir = join(p.state, "sheets", slug);
  await secureDir(dir);
  const tiles = [];
  for (let i = 0; i < candidates.length; i++) {
    const candidate = candidates[i];
    const tile = join(dir, `${String(i + 1).padStart(2, "0")}.jpg`);
    const year = candidate.date ? candidate.date.slice(0, 10) : "undated";
    await pexec("magick", [
      candidate.path,
      "-auto-orient", "-thumbnail", "600x600^", "-gravity", "center", "-extent", "600x600",
      "-background", "#111111", "-gravity", "south", "-splice", "0x64",
      "-font", "/System/Library/Fonts/SFNS.ttf", "-fill", "white", "-pointsize", "30", "-annotate", "+0+16", `${String(i + 1).padStart(2, "0")} · ${year}`,
      "-quality", "88", tile,
    ], { maxBuffer: 8 * 1024 * 1024 });
    await chmod(tile, 0o600).catch(() => {});
    tiles.push(tile);
  }
  const columns = Math.min(3, candidates.length);
  const sheet = join(p.state, "sheets", `${slug}.jpg`);
  await pexec("magick", ["montage", ...tiles, "-font", "/System/Library/Fonts/SFNS.ttf", "-tile", `${columns}x`, "-geometry", "+18+18", "-background", "#111111", sheet], { maxBuffer: 8 * 1024 * 1024 });
  await chmod(sheet, 0o600).catch(() => {});
  const manifest = join(p.state, "sheets", `${slug}.json`);
  await writeFile(manifest, `${JSON.stringify({ query, createdAt: new Date().toISOString(), sheet, candidates }, null, 2)}\n`, { mode: 0o600 });
  return { query, sheet, manifest, candidates };
}

export async function songBriefs() {
  return JSON.parse(await readFile(BRIEFS_PATH, "utf8"));
}

export async function songCandidates(song, options = {}) {
  const briefs = await songBriefs();
  const wanted = String(song || "").toLowerCase();
  const brief = briefs.songs.find((item) => item.slug.toLowerCase() === wanted || item.title.toLowerCase() === wanted);
  if (!brief) throw new Error(`Unknown released song “${song}”. Choices: ${briefs.songs.map((item) => item.slug).join(", ")}`);
  const candidates = await searchPhotos(brief.terms, { ...options, boostTerms: brief.primaryTerms });
  return { brief, candidates };
}

async function uniqueVisualCandidates(candidates, limit) {
  const unique = [];
  const signatures = new Set();
  for (const candidate of candidates) {
    const { stdout } = await pexec("magick", [
      candidate.path, "-auto-orient", "-thumbnail", "64x64^", "-gravity", "center",
      "-extent", "64x64", "-format", "%#", "info:",
    ], { maxBuffer: 1024 * 1024 });
    const signature = stdout.trim();
    if (signatures.has(signature)) continue;
    signatures.add(signature);
    unique.push(candidate);
    if (unique.length >= limit) break;
  }
  return unique;
}

async function vividCandidates(candidates) {
  const scored = [];
  for (const candidate of candidates) {
    const { stdout } = await pexec("magick", [
      candidate.path, "-auto-orient", "-thumbnail", "96x96^", "-gravity", "center",
      "-extent", "96x96", "-colorspace", "HSL",
      "-format", "%[fx:mean.g],%[fx:standard_deviation.b]", "info:",
    ], { maxBuffer: 1024 * 1024 });
    const [saturation = 0, contrast = 0] = stdout.trim().split(",").map(Number);
    const coverRank = candidate.rank + saturation * 25 + contrast * 10;
    scored.push({
      ...candidate,
      saturation: Number(saturation.toFixed(3)),
      contrast: Number(contrast.toFixed(3)),
      coverRank: Number(coverRank.toFixed(3)),
    });
  }
  return scored.sort((a, b) => b.coverRank - a.coverRank || b.rank - a.rank);
}

export async function allSongSheets(options = {}) {
  const briefs = await songBriefs();
  const results = [];
  const requestedLimit = Math.max(1, Math.min(100, Number(options.limit) || 20));
  const rejected = new Set(options.rejectIds || []);
  for (const brief of briefs.songs) {
    const poolLimit = options.dedupeVisual ? Math.min(100, requestedLimit * 3) : requestedLimit;
    const found = (await searchPhotos(brief.terms, { ...options, limit: poolLimit, boostTerms: brief.primaryTerms }))
      .filter((candidate) => !rejected.has(candidate.id));
    const ranked = options.vivid ? await vividCandidates(found) : found;
    const candidates = options.dedupeVisual
      ? await uniqueVisualCandidates(ranked, requestedLimit)
      : ranked.slice(0, requestedLimit);
    if (!candidates.length) {
      results.push({ brief, error: "no local candidates" });
      continue;
    }
    results.push({ brief, ...(await makeSheet(brief.terms, { ...options, slug: brief.slug, candidates })) });
  }
  const p = paths(options);
  const report = join(p.state, "pop-cover-candidates.json");
  await writeFile(report, `${JSON.stringify({ createdAt: new Date().toISOString(), results }, null, 2)}\n`, { mode: 0o600 });
  return { report, results };
}

export async function makeApprovalPdfs(options = {}) {
  const p = paths(options);
  const approvalDir = join(p.state, "approval-pdfs");
  const workDir = join(approvalDir, "work");
  await secureDir(approvalDir);
  await secureDir(workDir);

  const batch = await allSongSheets({
    ...options,
    maxPerDay: 1,
    dedupeVisual: true,
    captureOnly: true,
    selfOnly: true,
    vivid: true,
  });
  const pages = [];
  for (const result of batch.results) {
    if (result.error || !result.candidates?.length) continue;
    const songDir = join(workDir, result.brief.slug);
    await secureDir(songDir);
    const tiles = [];
    const selected = result.candidates.slice(0, Math.max(1, Number(options.limit) || 9));
    for (let i = 0; i < selected.length; i++) {
      const tile = join(songDir, `${String(i + 1).padStart(2, "0")}.jpg`);
      await pexec("magick", [
        selected[i].path,
        "-auto-orient", "-thumbnail", "700x700^", "-gravity", "center", "-extent", "700x700",
        "-modulate", "102,138,100", "-sigmoidal-contrast", "3x50%",
        "-background", "#111111", "-gravity", "south", "-splice", "0x110",
        "-font", "/System/Library/Fonts/SFNS.ttf", "-fill", "white", "-pointsize", "64",
        "-annotate", "+0+24", String(i + 1).padStart(2, "0"),
        "-quality", "92", tile,
      ], { maxBuffer: 8 * 1024 * 1024 });
      await chmod(tile, 0o600).catch(() => {});
      tiles.push(tile);
    }

    const grid = join(songDir, "grid.jpg");
    const columns = Math.min(3, selected.length);
    const rows = Math.ceil(selected.length / columns);
    await pexec("magick", [
      "montage", ...tiles, "-font", "/System/Library/Fonts/SFNS.ttf",
      "-tile", `${columns}x${rows}`, "-geometry", "+24+24", "-background", "#111111", grid,
    ], { maxBuffer: 8 * 1024 * 1024 });
    const pageImage = join(approvalDir, `${result.brief.slug}-contact-sheet.jpg`);
    await pexec("magick", [
      grid, "-background", "#111111", "-gravity", "north", "-splice", "0x180",
      "-font", "/System/Library/Fonts/SFNS.ttf", "-fill", "white", "-pointsize", "96",
      "-annotate", "+0+46", result.brief.title, "-quality", "94", pageImage,
    ], { maxBuffer: 8 * 1024 * 1024 });
    const pdf = join(approvalDir, `${result.brief.slug}-contact-sheet.pdf`);
    await pexec("sips", ["-s", "format", "pdf", pageImage, "--out", pdf], { maxBuffer: 8 * 1024 * 1024 });
    await Promise.all([chmod(pageImage, 0o600).catch(() => {}), chmod(pdf, 0o600).catch(() => {})]);
    pages.push({ slug: result.brief.slug, title: result.brief.title, pdf, pageImage, candidates: selected.map((candidate) => candidate.id) });
  }
  if (!pages.length) throw new Error("No approval pages were generated.");

  const combined = join(approvalDir, "all-pop-cover-contact-sheets.pdf");
  await pexec("pdfunite", [...pages.map((page) => page.pdf), combined], { maxBuffer: 8 * 1024 * 1024 });
  await chmod(combined, 0o600).catch(() => {});
  const manifest = join(approvalDir, "approval-index.json");
  await writeFile(manifest, `${JSON.stringify({ createdAt: new Date().toISOString(), combined, pages }, null, 2)}\n`, { mode: 0o600 });
  return { combined, manifest, pages };
}

function publicCandidate(candidate) {
  return { ...candidate, path: candidate.local ? candidate.path : null };
}

async function main() {
  const command = process.argv[2] || "status";
  const options = {
    limit: Number(arg("limit", 20)),
    library: arg("library"),
    state: arg("state"),
    includeCloud: flag("include-cloud"),
    rejectIds: String(arg("reject", "")).split(",").filter(Boolean),
  };
  if (command === "status") console.log(JSON.stringify(await photoStatus(options), null, 2));
  else if (command === "search") {
    const query = process.argv.slice(3).filter((value, i, all) => !value.startsWith("--") && (i === 0 || !all[i - 1].startsWith("--"))).join(" ");
    console.log(JSON.stringify((await searchPhotos(query, options)).map(publicCandidate), null, 2));
  } else if (command === "sheet") {
    const query = process.argv.slice(3).filter((value, i, all) => !value.startsWith("--") && (i === 0 || !all[i - 1].startsWith("--"))).join(" ");
    console.log(JSON.stringify(await makeSheet(query, options), null, 2));
  } else if (command === "song") {
    const song = process.argv[3];
    console.log(JSON.stringify(await songCandidates(song, options), null, 2));
  } else if (command === "songs") console.log(JSON.stringify(await allSongSheets(options), null, 2));
  else if (command === "approval") console.log(JSON.stringify(await makeApprovalPdfs(options), null, 2));
  else throw new Error(`Unknown command “${command}”. Use status, search, sheet, song, songs, or approval.`);
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  main().catch((error) => { console.error(error.stack || error.message || error); process.exitCode = 1; });
}
