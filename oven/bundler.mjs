// bundler.mjs - HTML bundle generation for Aesthetic Computer pieces
// Ported from system/netlify/functions/bundle-html.js (CJS → ESM)
//
// Generates self-contained HTML bundles on-demand via Express routes.
// Supports KidLisp pieces ($code) and JavaScript pieces (piece=name).
//
// Core system files (platform code, fonts) are cached in-memory per git
// commit and pre-warmed on deploy so that per-piece bundling is fast.

import { promises as fs } from "fs";
import fsSync from "fs";
import path from "path";
import { gzipSync, brotliCompressSync, constants as zlibConstants } from "zlib";
import { execSync } from "child_process";
import { MongoClient } from "mongodb";

// ─── Configuration ──────────────────────────────────────────────────

// Source directory for aesthetic.computer platform files.
// In production this is rsynced by deploy.sh to /opt/oven/ac-source/
// In dev, override with AC_SOURCE_DIR env var.
const AC_SOURCE_DIR = process.env.AC_SOURCE_DIR
  || path.resolve(process.cwd(), "ac-source");

function getGitCommit() {
  if (process.env.OVEN_VERSION && process.env.OVEN_VERSION !== "unknown")
    return process.env.OVEN_VERSION;
  try {
    const hash = execSync("git rev-parse --short HEAD", { encoding: "utf8" }).trim();
    const isDirty = execSync("git status --porcelain", { encoding: "utf8" }).trim().length > 0;
    return isDirty ? `${hash} (dirty)` : hash;
  } catch {
    return "unknown";
  }
}

let GIT_COMMIT = getGitCommit();

// Re-read git commit (called after deploy rsync updates source).
export function refreshGitCommit() {
  GIT_COMMIT = getGitCommit();
  return GIT_COMMIT;
}

// ─── In-memory cache ────────────────────────────────────────────────

let coreBundleCache = null;
let coreBundleCacheCommit = null;
let skipMinification = false;

// ─── Brotli WASM decoder (loaded once at startup) ──────────────────

let brotliWasmGzBase64 = null; // gzip'd + base64'd WASM binary for inline embedding

function loadBrotliWasmDecoder() {
  try {
    const wasmPath = path.resolve(process.cwd(), "node_modules/brotli-dec-wasm/pkg/brotli_dec_wasm_bg.wasm");
    const wasmBytes = fsSync.readFileSync(wasmPath);
    const gzipped = gzipSync(wasmBytes, { level: 9 });
    brotliWasmGzBase64 = gzipped.toString("base64");
    console.log(`[bundler] brotli WASM decoder loaded (${Math.round(brotliWasmGzBase64.length / 1024)} KB gzip+base64)`);
  } catch (err) {
    console.warn(`[bundler] brotli WASM decoder not available: ${err.message}`);
  }
}

// Load on import
loadBrotliWasmDecoder();

// ─── Constants ──────────────────────────────────────────────────────

const ESSENTIAL_FILES = [
  "boot.mjs", "bios.mjs",
  "lib/loop.mjs", "lib/disk.mjs", "lib/parse.mjs",
  "lib/kidlisp.mjs",
  "lib/graph.mjs", "lib/geo.mjs", "lib/2d.mjs", "lib/pen.mjs",
  "lib/num.mjs", "lib/gl.mjs", "lib/webgl-blit.mjs",
  "lib/helpers.mjs", "lib/logs.mjs", "lib/store.mjs",
  "lib/platform.mjs", "lib/pack-mode.mjs",
  "lib/keyboard.mjs", "lib/gamepad.mjs", "lib/motion.mjs",
  "lib/speech.mjs", "lib/help.mjs", "lib/midi.mjs", "lib/usb.mjs",
  "lib/headers.mjs", "lib/glaze.mjs", "lib/ui.mjs",
  "disks/common/tape-player.mjs",
  "lib/sound/sound-whitelist.mjs",
  "lib/speaker-bundled.mjs",
  "dep/gl-matrix/common.mjs", "dep/gl-matrix/vec2.mjs",
  "dep/gl-matrix/vec3.mjs", "dep/gl-matrix/vec4.mjs",
  "dep/gl-matrix/mat3.mjs", "dep/gl-matrix/mat4.mjs",
  "dep/gl-matrix/quat.mjs",
  "lib/glazes/uniforms.js",
];

const SKIP_FILES = [];

// ─── Helpers ────────────────────────────────────────────────────────

function timestamp(date = new Date()) {
  const pad = (n, digits = 2) => n.toString().padStart(digits, "0");
  return `${date.getFullYear()}.${date.getMonth() + 1}.${date.getDate()}.${date.getHours()}.${date.getMinutes()}.${date.getSeconds()}.${pad(date.getMilliseconds(), 3)}`;
}

// CSS color names recognized by KidLisp (must match kidlisp.mjs KIDLISP_COLORS)
const KIDLISP_COLORS = new Set([
  "red", "green", "blue", "yellow", "orange", "purple", "pink", "cyan", "magenta",
  "black", "white", "gray", "grey", "brown", "lime", "navy", "teal", "olive",
  "maroon", "aqua", "fuchsia", "silver", "gold", "coral", "salmon", "khaki",
  "indigo", "violet", "turquoise", "tomato", "crimson", "lavender", "beige",
  "plum", "orchid", "tan", "chocolate", "sienna", "peru", "wheat",
]);

// Extract the first color word from KidLisp source for the page background.
// KidLisp shorthand puts a bare color name as the first token, e.g. "purple, ink, line"
function extractFirstColor(source) {
  if (!source) return null;
  // Get the first token (split on whitespace or comma)
  const firstToken = source.trim().split(/[\s,()]+/)[0]?.toLowerCase();
  if (firstToken && KIDLISP_COLORS.has(firstToken)) return firstToken;
  // Also check inside (wipe "color") or (wipe color)
  const wipeMatch = source.match(/\(\s*wipe\s+["']?([a-z]+)["']?\s*\)/i);
  if (wipeMatch && KIDLISP_COLORS.has(wipeMatch[1].toLowerCase())) return wipeMatch[1].toLowerCase();
  return null;
}

function extractPaintingCodes(source) {
  const codes = [];
  const regex = /#([a-zA-Z0-9]{3})\b/g;
  let match;
  while ((match = regex.exec(source)) !== null) {
    if (!codes.includes(match[1])) codes.push(match[1]);
  }
  return codes;
}

async function resolvePaintingCode(code) {
  try {
    const response = await fetch(`https://aesthetic.computer/api/painting-code?code=${code}`);
    if (!response.ok) return null;
    const data = await response.json();
    return { code, handle: data.handle || "anon", slug: data.slug };
  } catch {
    return null;
  }
}

async function fetchPaintingImage(handle, slug) {
  const handlePath = handle === "anon" ? "" : `@${handle}/`;
  const url = `https://aesthetic.computer/media/${handlePath}painting/${slug}.png`;
  try {
    const response = await fetch(url);
    if (!response.ok) return null;
    const buffer = await response.arrayBuffer();
    return Buffer.from(buffer).toString("base64");
  } catch {
    return null;
  }
}

// ─── Author info (MongoDB) ──────────────────────────────────────────

let mongoClient = null;

async function getDb() {
  const uri = process.env.MONGODB_CONNECTION_STRING;
  const dbName = process.env.MONGODB_NAME;
  if (!uri || !dbName) return null;
  if (!mongoClient) {
    mongoClient = await MongoClient.connect(uri);
  }
  return mongoClient.db(dbName);
}

async function fetchAuthorInfo(userId) {
  if (!userId) return { handle: null, userCode: null };
  let acHandle = null;
  let userCode = null;

  try {
    const response = await fetch(
      `https://aesthetic.computer/handle?for=${encodeURIComponent(userId)}`
    );
    if (response.ok) {
      const data = await response.json();
      if (data.handle) acHandle = data.handle;
    }
  } catch { /* ignore */ }

  try {
    const db = await getDb();
    if (db) {
      const user = await db.collection("users").findOne(
        { _id: userId }, { projection: { code: 1 } }
      );
      if (user?.code) userCode = user.code;
    }
  } catch { /* ignore */ }

  return { handle: acHandle, userCode };
}

// ─── KidLisp source fetching ────────────────────────────────────────

async function fetchKidLispFromAPI(pieceName) {
  const cleanName = pieceName.replace("$", "");
  const response = await fetch(
    `https://aesthetic.computer/api/store-kidlisp?code=${cleanName}`
  );
  const data = await response.json();
  if (data.error || !data.source) {
    throw new Error(`Piece '$${cleanName}' not found`);
  }
  return { source: data.source, userId: data.user || null };
}

function extractKidLispRefs(source) {
  const refs = [];
  const regex = /\$[a-z0-9_-]+/gi;
  for (const match of source.matchAll(regex)) {
    const ref = match[0].toLowerCase();
    if (!refs.includes(ref)) refs.push(ref);
  }
  return refs;
}

async function getKidLispSourceWithDeps(pieceName) {
  const allSources = {};
  const toProcess = [pieceName];
  const processed = new Set();
  let mainPieceUserId = null;

  while (toProcess.length > 0) {
    const current = toProcess.shift();
    const cleanName = current.replace("$", "");
    if (processed.has(cleanName)) continue;
    processed.add(cleanName);

    const { source, userId } = await fetchKidLispFromAPI(cleanName);
    allSources[cleanName] = source;

    if (cleanName === pieceName.replace("$", "") && userId) {
      mainPieceUserId = userId;
    }

    const refs = extractKidLispRefs(source);
    for (const ref of refs) {
      const refName = ref.replace("$", "");
      if (!processed.has(refName)) toProcess.push(refName);
    }
  }

  let authorHandle = "anon";
  let userCode = null;
  if (mainPieceUserId) {
    const info = await fetchAuthorInfo(mainPieceUserId);
    if (info.handle) authorHandle = info.handle;
    if (info.userCode) userCode = info.userCode;
  }

  return { sources: allSources, authorHandle, userCode };
}

// ─── Import rewriting ───────────────────────────────────────────────

function resolvePath(base, relative) {
  if (!relative.startsWith(".")) return relative;
  let dir = path.dirname(base);
  const parts = dir === "." ? [] : dir.split("/").filter((p) => p);
  const relParts = relative.split("/");
  for (const part of relParts) {
    if (part === "..") parts.pop();
    else if (part !== "." && part !== "") parts.push(part);
  }
  return parts.join("/");
}

function rewriteImports(code, filepath) {
  code = code.replace(
    /from\s*['"]aesthetic\.computer\/disks\/([^'"]+)['"]/g,
    (match, p) => "from 'ac/disks/" + p + "'"
  );
  code = code.replace(
    /import\s*\((['"]aesthetic\.computer\/disks\/([^'"]+)['")])\)/g,
    (match, fullPath, p) => "import('ac/disks/" + p + "')"
  );
  code = code.replace(
    /from\s*['"](\.\.\/[^'"]+|\.\/[^'"]+)(\?[^'"]+)?['"]/g,
    (match, p) => {
      const resolved = resolvePath(filepath, p);
      return 'from"' + resolved + '"';
    }
  );
  code = code.replace(
    /import\s*\((['"](\.\.\/[^'"]+|\.\/[^'"]+)(\?[^'"]+)?['")])\)/g,
    (match, fullPath, p) => {
      const resolved = resolvePath(filepath, p);
      return 'import("' + resolved + '")';
    }
  );
  code = code.replace(
    /import\s*\(\`(\.\.\/[^\`]+|\.\/[^\`]+)\`\)/g,
    (match, p) => {
      const clean = p.split("?")[0];
      const resolved = resolvePath(filepath, clean);
      return 'import("' + resolved + '")';
    }
  );
  code = code.replace(
    /\(\s*['"](\.\.?\/[^'"]+\.m?js)(\?[^'"]+)?['"]\s*\)/g,
    (match, p) => {
      const resolved = resolvePath(filepath, p);
      return '("' + resolved + '")';
    }
  );
  return code;
}

// ─── Minification ───────────────────────────────────────────────────

async function minifyJS(content, relativePath) {
  const ext = path.extname(relativePath);
  if (ext !== ".mjs" && ext !== ".js") return content;

  let processed = rewriteImports(content, relativePath);
  if (skipMinification) return processed;

  try {
    const { minify } = await import("terser");
    const result = await minify(processed, {
      compress: {
        dead_code: true,
        drop_console: true,
        drop_debugger: true,
        unused: true,
        passes: 3,
        pure_getters: true,
        unsafe: true,
        unsafe_math: true,
        unsafe_proto: true,
      },
      mangle: true,
      module: true,
      format: { comments: false, ascii_only: false, ecma: 2020 },
    });
    return result.code || processed;
  } catch (err) {
    console.error(`[bundler] minify failed ${relativePath}:`, err.message);
    return processed;
  }
}

// ─── Dependency discovery ───────────────────────────────────────────

async function discoverDependencies(acDir, essentialFiles, skipFiles) {
  const discovered = new Set(essentialFiles);
  const toProcess = [...essentialFiles];

  while (toProcess.length > 0) {
    const file = toProcess.shift();
    const fullPath = path.join(acDir, file);
    if (!fsSync.existsSync(fullPath)) continue;

    try {
      const content = await fs.readFile(fullPath, "utf8");
      const importRegex = /from\s+["'](\.\.[^"']+|\.\/[^"']+)["']/g;
      const dynamicRegex = /import\s*\(\s*["'](\.\.[^"']+|\.\/[^"']+)["']\s*\)/g;

      let match;
      while ((match = importRegex.exec(content)) !== null) {
        const resolved = resolvePath(file, match[1]);
        if (skipFiles.some((s) => resolved.includes(s))) continue;
        if (!discovered.has(resolved)) { discovered.add(resolved); toProcess.push(resolved); }
      }
      while ((match = dynamicRegex.exec(content)) !== null) {
        const resolved = resolvePath(file, match[1]);
        if (skipFiles.some((s) => resolved.includes(s))) continue;
        if (!discovered.has(resolved)) { discovered.add(resolved); toProcess.push(resolved); }
      }
    } catch { /* ignore */ }
  }

  return Array.from(discovered);
}

// ─── Core bundle (cached per git commit) ────────────────────────────

async function getCoreBundle(onProgress = () => {}, forceRefresh = false) {
  const acDir = AC_SOURCE_DIR;

  if (!forceRefresh && coreBundleCache && coreBundleCacheCommit === GIT_COMMIT) {
    console.log(`[bundler] cache hit for ${GIT_COMMIT}`);
    onProgress({ stage: "cache-hit", message: "Using cached core files..." });
    return coreBundleCache;
  }

  console.log(`[bundler] building core bundle for ${GIT_COMMIT}...`);
  const coreFiles = {};

  onProgress({ stage: "discover", message: "Discovering dependencies..." });
  const allFiles = await discoverDependencies(acDir, ESSENTIAL_FILES, SKIP_FILES);

  onProgress({ stage: "minify", message: `Minifying ${allFiles.length} files...` });

  // Parallel minification in batches
  let minified = 0;
  const BATCH = 10;
  for (let i = 0; i < allFiles.length; i += BATCH) {
    const batch = allFiles.slice(i, i + BATCH);
    const results = await Promise.all(
      batch.map(async (file) => {
        const full = path.join(acDir, file);
        try {
          if (!fsSync.existsSync(full)) return null;
          let content = await fs.readFile(full, "utf8");
          content = await minifyJS(content, file);
          return { file, content };
        } catch { return null; }
      })
    );
    for (const r of results) {
      if (r) {
        coreFiles[r.file] = { content: r.content, binary: false, type: path.extname(r.file).slice(1) };
        minified++;
      }
    }
    onProgress({ stage: "minify", message: `Minified ${minified}/${allFiles.length} files...` });
  }

  // nanoid
  const nanoidPath = "dep/nanoid/index.js";
  const nanoidFull = path.join(acDir, nanoidPath);
  if (fsSync.existsSync(nanoidFull)) {
    let c = await fs.readFile(nanoidFull, "utf8");
    c = await minifyJS(c, nanoidPath);
    coreFiles[nanoidPath] = { content: c, binary: false, type: "js" };
  }

  onProgress({ stage: "fonts", message: "Loading fonts..." });

  // Font glyphs
  const font1Dir = path.join(acDir, "disks/drawings/font_1");
  for (const category of ["lowercase", "uppercase", "numbers", "symbols"]) {
    const catDir = path.join(font1Dir, category);
    try {
      if (fsSync.existsSync(catDir)) {
        for (const f of fsSync.readdirSync(catDir).filter((n) => n.endsWith(".json"))) {
          const content = await fs.readFile(path.join(catDir, f), "utf8");
          coreFiles[`disks/drawings/font_1/${category}/${f}`] = { content, binary: false, type: "json" };
        }
      }
    } catch { /* skip */ }
  }

  coreBundleCache = coreFiles;
  coreBundleCacheCommit = GIT_COMMIT;
  console.log(`[bundler] cached ${Object.keys(coreFiles).length} core files`);
  return coreFiles;
}

// ─── KidLisp bundle ─────────────────────────────────────────────────

export async function createBundle(pieceName, onProgress = () => {}, nocompress = false, density = null, brotli = false) {
  const PIECE_NAME_NO_DOLLAR = pieceName.replace(/^\$/, "");
  const PIECE_NAME = "$" + PIECE_NAME_NO_DOLLAR;

  onProgress({ stage: "fetch", message: `Fetching $${PIECE_NAME_NO_DOLLAR}...` });

  const { sources: kidlispSources, authorHandle, userCode } = await getKidLispSourceWithDeps(PIECE_NAME_NO_DOLLAR);
  const mainSource = kidlispSources[PIECE_NAME_NO_DOLLAR];
  const depCount = Object.keys(kidlispSources).length - 1;

  onProgress({ stage: "deps", message: `Found ${depCount} dependenc${depCount === 1 ? "y" : "ies"}` });

  const packTime = Date.now();
  const packDate = new Date().toLocaleString("en-US", {
    timeZone: "America/Los_Angeles", year: "numeric", month: "long", day: "numeric",
    hour: "numeric", minute: "2-digit", second: "2-digit", hour12: true,
  });
  const bundleTimestamp = timestamp();

  const coreFiles = await getCoreBundle(onProgress);
  const files = { ...coreFiles };

  // Paintings
  const allKidlispSource = Object.values(kidlispSources).join("\n");
  const paintingCodes = extractPaintingCodes(allKidlispSource);
  const paintingData = {};

  if (paintingCodes.length > 0) {
    onProgress({ stage: "paintings", message: `Embedding ${paintingCodes.length} painting${paintingCodes.length === 1 ? "" : "s"}...` });
  }

  for (const code of paintingCodes) {
    const resolved = await resolvePaintingCode(code);
    if (resolved) {
      paintingData[code] = resolved;
      const img = await fetchPaintingImage(resolved.handle, resolved.slug);
      if (img) files[`paintings/${code}.png`] = { content: img, binary: true, type: "png" };
    }
  }

  for (const [name, source] of Object.entries(kidlispSources)) {
    files[`disks/${name}.lisp`] = { content: source, binary: false, type: "lisp" };
  }

  onProgress({ stage: "generate", message: "Generating HTML bundle..." });

  const filename = `$${PIECE_NAME_NO_DOLLAR}-${authorHandle}-${bundleTimestamp}.lisp.html`;

  const bgColor = extractFirstColor(mainSource);

  const htmlContent = generateHTMLBundle({
    PIECE_NAME, PIECE_NAME_NO_DOLLAR, mainSource, kidlispSources,
    files, paintingData, authorHandle, packDate, packTime,
    gitVersion: GIT_COMMIT, filename, density, bgColor,
  });

  const method = nocompress ? "none" : brotli ? "brotli" : "gzip";
  onProgress({ stage: "compress", message: nocompress ? "Skipping compression..." : `Compressing (${method})...` });

  if (nocompress) {
    return { html: htmlContent, filename, sizeKB: Math.round(htmlContent.length / 1024), mainSource, authorHandle, userCode, packDate, depCount };
  }

  const htmlBuf = Buffer.from(htmlContent, "utf-8");
  let finalHtml;

  if (brotli && brotliWasmGzBase64) {
    const compressed = brotliCompressSync(htmlBuf, {
      params: { [zlibConstants.BROTLI_PARAM_MODE]: zlibConstants.BROTLI_MODE_TEXT, [zlibConstants.BROTLI_PARAM_QUALITY]: 11 },
    });
    finalHtml = generateSelfExtractingBrotliHTML(PIECE_NAME, compressed.toString("base64"), bgColor);
  } else {
    const compressed = gzipSync(htmlBuf, { level: 9 });
    finalHtml = generateSelfExtractingHTML(PIECE_NAME, compressed.toString("base64"), bgColor);
  }

  return { html: finalHtml, filename, sizeKB: Math.round(finalHtml.length / 1024), mainSource, authorHandle, userCode, packDate, depCount };
}

// ─── JS piece bundle ────────────────────────────────────────────────

export async function createJSPieceBundle(pieceName, onProgress = () => {}, nocompress = false, density = null, brotli = false) {
  const acDir = AC_SOURCE_DIR;
  onProgress({ stage: "init", message: `Bundling ${pieceName}...` });

  const packTime = Date.now();
  const packDate = new Date().toLocaleString("en-US", {
    timeZone: "America/Los_Angeles", year: "numeric", month: "long", day: "numeric",
    hour: "numeric", minute: "2-digit", second: "2-digit", hour12: true,
  });
  const bundleTimestamp = timestamp();

  const coreFiles = await getCoreBundle(onProgress);
  const files = { ...coreFiles };

  const piecePath = `disks/${pieceName}.mjs`;
  const pieceFullPath = path.join(acDir, piecePath);
  if (!fsSync.existsSync(pieceFullPath)) {
    throw new Error(`Piece '${pieceName}' not found at ${piecePath}`);
  }

  onProgress({ stage: "piece", message: `Loading ${pieceName}.mjs...` });

  const pieceContent = await fs.readFile(pieceFullPath, "utf8");
  files[piecePath] = { content: rewriteImports(pieceContent, piecePath), binary: false, type: "mjs" };

  const pieceDepFiles = await discoverDependencies(acDir, [piecePath], SKIP_FILES);
  onProgress({ stage: "deps", message: `Found ${pieceDepFiles.length} dependencies...` });

  for (const depFile of pieceDepFiles) {
    if (files[depFile]) continue;
    const depFullPath = path.join(acDir, depFile);
    try {
      if (!fsSync.existsSync(depFullPath)) continue;
      let content = await fs.readFile(depFullPath, "utf8");
      content = depFile.startsWith("disks/") ? rewriteImports(content, depFile) : await minifyJS(content, depFile);
      files[depFile] = { content, binary: false, type: path.extname(depFile).slice(1) };
    } catch { /* skip */ }
  }

  onProgress({ stage: "generate", message: "Generating HTML bundle..." });

  const htmlContent = generateJSPieceHTMLBundle({ pieceName, files, packDate, packTime, gitVersion: GIT_COMMIT });
  const filename = `${pieceName}-${bundleTimestamp}.html`;

  const method = nocompress ? "none" : brotli ? "brotli" : "gzip";
  onProgress({ stage: "compress", message: nocompress ? "Skipping compression..." : `Compressing (${method})...` });

  if (nocompress) return { html: htmlContent, filename, sizeKB: Math.round(htmlContent.length / 1024) };

  const htmlBuf = Buffer.from(htmlContent, "utf-8");
  let finalHtml;

  if (brotli && brotliWasmGzBase64) {
    const compressed = brotliCompressSync(htmlBuf, {
      params: { [zlibConstants.BROTLI_PARAM_MODE]: zlibConstants.BROTLI_MODE_TEXT, [zlibConstants.BROTLI_PARAM_QUALITY]: 11 },
    });
    finalHtml = generateSelfExtractingBrotliHTML(pieceName, compressed.toString("base64"));
  } else {
    const compressed = gzipSync(htmlBuf, { level: 9 });
    finalHtml = generateSelfExtractingHTML(pieceName, compressed.toString("base64"));
  }

  return { html: finalHtml, filename, sizeKB: Math.round(finalHtml.length / 1024) };
}

// ─── M4D (Max for Live) ─────────────────────────────────────────────

const M4L_HEADER_INSTRUMENT = Buffer.from(
  "ampf\x04\x00\x00\x00iiiimeta\x04\x00\x00\x00\x00\x00\x00\x00ptch", "binary"
);

function generateM4DPatcher(pieceName, dataUri, width = 400, height = 200) {
  return {
    patcher: {
      fileversion: 1,
      appversion: { major: 9, minor: 0, revision: 7, architecture: "x64", modernui: 1 },
      classnamespace: "box",
      rect: [134, 174, 800, 600],
      openrect: [0, 0, width, height],
      openinpresentation: 1,
      gridsize: [15, 15],
      enablehscroll: 0, enablevscroll: 0,
      devicewidth: width,
      description: `Aesthetic Computer ${pieceName} (offline)`,
      boxes: [
        { box: { disablefind: 0, id: "obj-jweb", latency: 0, maxclass: "jweb~", numinlets: 1, numoutlets: 3, outlettype: ["signal","signal",""], patching_rect: [10,50,width,height], presentation: 1, presentation_rect: [0,0,width+1,height+1], rendermode: 1, url: dataUri } },
        { box: { id: "obj-plugout", maxclass: "newobj", numinlets: 2, numoutlets: 0, patching_rect: [10,280,75,22], text: "plugout~ 1 2" } },
        { box: { id: "obj-thisdevice", maxclass: "newobj", numinlets: 1, numoutlets: 3, outlettype: ["bang","int","int"], patching_rect: [350,50,85,22], text: "live.thisdevice" } },
        { box: { id: "obj-print", maxclass: "newobj", numinlets: 1, numoutlets: 0, patching_rect: [350,80,150,22], text: `print [AC-${pieceName.toUpperCase()}]` } },
        { box: { id: "obj-route", maxclass: "newobj", numinlets: 1, numoutlets: 2, outlettype: ["",""], patching_rect: [350,140,60,22], text: "route ready" } },
        { box: { id: "obj-activate", maxclass: "message", numinlets: 2, numoutlets: 1, outlettype: [""], patching_rect: [350,170,60,22], text: "activate 1" } },
        { box: { id: "obj-jweb-print", maxclass: "newobj", numinlets: 1, numoutlets: 0, patching_rect: [350,110,90,22], text: "print [AC-JWEB]" } },
        { box: { id: "obj-route-logs", maxclass: "newobj", numinlets: 1, numoutlets: 4, outlettype: ["","","",""], patching_rect: [470,140,120,22], text: "route log error warn" } },
        { box: { id: "obj-udpsend", maxclass: "newobj", numinlets: 1, numoutlets: 0, patching_rect: [470,210,160,22], text: "udpsend 127.0.0.1 7777" } },
        { box: { id: "obj-prepend-log", maxclass: "newobj", numinlets: 1, numoutlets: 1, outlettype: [""], patching_rect: [470,170,55,22], text: "prepend log" } },
        { box: { id: "obj-prepend-error", maxclass: "newobj", numinlets: 1, numoutlets: 1, outlettype: [""], patching_rect: [530,170,65,22], text: "prepend error" } },
        { box: { id: "obj-prepend-warn", maxclass: "newobj", numinlets: 1, numoutlets: 1, outlettype: [""], patching_rect: [600,170,60,22], text: "prepend warn" } },
      ],
      lines: [
        { patchline: { destination: ["obj-plugout",0], source: ["obj-jweb",0] } },
        { patchline: { destination: ["obj-plugout",1], source: ["obj-jweb",1] } },
        { patchline: { destination: ["obj-print",0], source: ["obj-thisdevice",0] } },
        { patchline: { destination: ["obj-jweb-print",0], source: ["obj-jweb",2] } },
        { patchline: { destination: ["obj-route",0], source: ["obj-jweb",2] } },
        { patchline: { destination: ["obj-activate",0], source: ["obj-route",0] } },
        { patchline: { destination: ["obj-jweb",0], source: ["obj-activate",0] } },
        { patchline: { destination: ["obj-route-logs",0], source: ["obj-jweb",2] } },
        { patchline: { destination: ["obj-prepend-log",0], source: ["obj-route-logs",0] } },
        { patchline: { destination: ["obj-prepend-error",0], source: ["obj-route-logs",1] } },
        { patchline: { destination: ["obj-prepend-warn",0], source: ["obj-route-logs",2] } },
        { patchline: { destination: ["obj-udpsend",0], source: ["obj-prepend-log",0] } },
        { patchline: { destination: ["obj-udpsend",0], source: ["obj-prepend-error",0] } },
        { patchline: { destination: ["obj-udpsend",0], source: ["obj-prepend-warn",0] } },
      ],
      dependency_cache: [], latency: 0, is_mpe: 0,
      external_mpe_tuning_enabled: 0, minimum_live_version: "",
      minimum_max_version: "", platform_compatibility: 0, autosave: 0,
    },
  };
}

function packAMXD(patcher) {
  const json = Buffer.from(JSON.stringify(patcher));
  const len = Buffer.alloc(4);
  len.writeUInt32LE(json.length, 0);
  return Buffer.concat([M4L_HEADER_INSTRUMENT, len, json]);
}

export async function createM4DBundle(pieceName, isJSPiece, onProgress = () => {}, density = null) {
  onProgress({ stage: "fetch", message: `Building M4L device for ${pieceName}...` });

  const bundleResult = isJSPiece
    ? await createJSPieceBundle(pieceName, onProgress, false, density)
    : await createBundle(pieceName, onProgress, false, density);

  onProgress({ stage: "generate", message: "Embedding bundle in M4L device..." });

  const dataUri = `data:text/html;base64,${Buffer.from(bundleResult.html).toString("base64")}`;
  const patcher = generateM4DPatcher(pieceName, dataUri);

  onProgress({ stage: "compress", message: "Packing .amxd binary..." });

  const binary = packAMXD(patcher);
  const filename = `AC ${pieceName} (offline).amxd`;

  return { binary, filename, sizeKB: Math.round(binary.length / 1024) };
}

// ─── Self-extracting HTML wrapper ───────────────────────────────────

function generateSelfExtractingHTML(title, gzipBase64, bgColor = null) {
  const bgRule = bgColor ? `background:${bgColor};` : "";
  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>${title} · Aesthetic Computer</title>
  <style>body{margin:0;${bgRule}overflow:hidden}</style>
</head>
<body>
  <script>
    fetch('data:application/gzip;base64,${gzipBase64}')
      .then(r=>r.blob())
      .then(b=>b.stream().pipeThrough(new DecompressionStream('gzip')))
      .then(s=>new Response(s).text())
      .then(h=>{document.open();document.write(h);document.close();})
      .catch(e=>{document.body.style.color='#fff';document.body.textContent='Bundle error: '+e.message;});
  </script>
</body>
</html>`;
}

function generateSelfExtractingBrotliHTML(title, brotliBase64, bgColor = null) {
  if (!brotliWasmGzBase64) throw new Error("Brotli WASM decoder not loaded");
  const bgRule = bgColor ? `background:${bgColor};` : "";
  // The inline script:
  // 1. Decompresses the WASM decoder binary using browser's native gzip DecompressionStream
  // 2. Instantiates the brotli-dec-wasm WASM module with minimal glue
  // 3. Decompresses the brotli payload
  // 4. Writes the resulting HTML
  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>${title} · Aesthetic Computer</title>
  <style>body{margin:0;${bgRule}overflow:hidden}</style>
</head>
<body>
  <script>(async()=>{
    const d=s=>Uint8Array.from(atob(s),c=>c.charCodeAt(0));
    const gz=async b=>new Uint8Array(await new Response(new Blob([b]).stream().pipeThrough(new DecompressionStream('gzip'))).arrayBuffer());
    const wb=await gz(d("${brotliWasmGzBase64}"));
    let w,m=null,V=0,dv=null;
    const gM=()=>{if(!m||m.byteLength===0)m=new Uint8Array(w.memory.buffer);return m;};
    const gD=()=>{if(!dv||dv.buffer!==w.memory.buffer)dv=new DataView(w.memory.buffer);return dv;};
    const td=new TextDecoder('utf-8',{ignoreBOM:true,fatal:true});td.decode();
    const te=new TextEncoder();
    const gs=(p,l)=>td.decode(gM().subarray(p>>>0,(p>>>0)+l));
    const imp={wbg:{}};
    imp.wbg.__wbg_Error_52673b7de5a0ca89=(a,b)=>Error(gs(a,b));
    imp.wbg.__wbg___wbindgen_throw_dd24417ed36fc46e=(a,b)=>{throw new Error(gs(a,b));};
    imp.wbg.__wbg_error_7534b8e9a36f1ab4=(a,b)=>{try{console.error(gs(a,b));}finally{w.__wbindgen_free(a,b,1);}};
    imp.wbg.__wbg_new_8a6f238a6ece86ea=()=>new Error();
    imp.wbg.__wbg_stack_0ed75d68575b0f3c=(a,b)=>{const s=b.stack;const buf=te.encode(s);const p=w.__wbindgen_malloc(buf.length,1)>>>0;gM().subarray(p,p+buf.length).set(buf);gD().setInt32(a+4,buf.length,true);gD().setInt32(a,p,true);};
    imp.wbg.__wbindgen_init_externref_table=()=>{const t=w.__wbindgen_externrefs;const o=t.grow(4);t.set(0,undefined);t.set(o,undefined);t.set(o+1,null);t.set(o+2,true);t.set(o+3,false);};
    const{instance:inst}=await WebAssembly.instantiate(wb,imp);
    w=inst.exports;m=null;dv=null;w.__wbindgen_start();
    const br=d("${brotliBase64}");
    const p0=w.__wbindgen_malloc(br.length,1)>>>0;gM().set(br,p0);V=br.length;
    const r=w.decompress(p0,V);
    if(r[3]){const v=w.__wbindgen_externrefs.get(r[2]);w.__externref_table_dealloc(r[2]);throw v;}
    const out=gM().subarray(r[0]>>>0,(r[0]>>>0)+r[1]).slice();
    w.__wbindgen_free(r[0],r[1],1);
    const html=new TextDecoder().decode(out);
    document.open();document.write(html);document.close();
  })().catch(e=>{document.body.style.color='#fff';document.body.textContent='Bundle error: '+e.message;});<\/script>
</body>
</html>`;
}

// ─── HTML templates ─────────────────────────────────────────────────
// These are large template literals — kept as-is from the Netlify version.

function generateHTMLBundle(opts) {
  const {
    PIECE_NAME, PIECE_NAME_NO_DOLLAR, mainSource, kidlispSources,
    files, paintingData, authorHandle, packDate, packTime, gitVersion, filename, density, bgColor,
  } = opts;

  const bgRule = bgColor ? `background: ${bgColor}; ` : "";

  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no">
  <title>${PIECE_NAME} · Aesthetic Computer</title>
  <style>
    body { margin: 0; padding: 0; ${bgRule}overflow: hidden; }
    canvas { display: block; image-rendering: pixelated; }
  </style>
</head>
<body>
  <script type="module">
    window.acPACK_MODE = true;
    window.KIDLISP_SUPPRESS_SNAPSHOT_LOGS = true;
    window.__acKidlispConsoleEnabled = false;
    window.acKEEP_MODE = true;
    window.acSTARTING_PIECE = "${PIECE_NAME}";
    window.acPACK_PIECE = "${PIECE_NAME}";
    window.acPACK_DATE = "${packDate}";
    window.acPACK_GIT = "${gitVersion}";
    window.acKIDLISP_SOURCE = ${JSON.stringify(mainSource)};
    ${density ? `window.acPACK_DENSITY = ${density};` : "// No density override"}
    window.acPACK_COLOPHON = {
      piece: { name: '${PIECE_NAME_NO_DOLLAR}', sourceCode: ${JSON.stringify(mainSource)}, isKidLisp: true },
      build: { author: '${authorHandle}', packTime: ${packTime}, gitCommit: '${gitVersion}', gitIsDirty: false, fileCount: ${Object.keys(files).length}, filename: '${filename}' }
    };
    window.acPAINTING_CODE_MAP = ${JSON.stringify(paintingData)};
    window.VFS = ${JSON.stringify(files).replace(/<\/script>/g, "<\\/script>")};
    window.acEMBEDDED_PAINTING_BITMAPS = {};
    window.acPAINTING_BITMAPS_READY = false;
    async function decodePaintingToBitmap(code, base64Data) {
      return new Promise((resolve, reject) => {
        const img = new Image();
        img.onload = function() {
          const canvas = document.createElement('canvas');
          canvas.width = img.width; canvas.height = img.height;
          const ctx = canvas.getContext('2d');
          ctx.drawImage(img, 0, 0);
          const imageData = ctx.getImageData(0, 0, canvas.width, canvas.height);
          resolve({ width: imageData.width, height: imageData.height, pixels: imageData.data });
        };
        img.onerror = reject;
        img.src = 'data:image/png;base64,' + base64Data;
      });
    }
    window.acDecodePaintingsPromise = (async function() {
      const promises = [];
      for (const [code, info] of Object.entries(window.acPAINTING_CODE_MAP || {})) {
        const vfsPath = 'paintings/' + code + '.png';
        if (window.VFS && window.VFS[vfsPath]) {
          promises.push(decodePaintingToBitmap(code, window.VFS[vfsPath].content).then(bitmap => {
            window.acEMBEDDED_PAINTING_BITMAPS['#' + code] = bitmap;
            window.acEMBEDDED_PAINTING_BITMAPS[code] = bitmap;
          }).catch(() => {}));
        }
      }
      await Promise.all(promises);
      window.acPAINTING_BITMAPS_READY = true;
    })();
    window.EMBEDDED_KIDLISP_SOURCE = ${JSON.stringify(mainSource)};
    window.EMBEDDED_KIDLISP_PIECE = '${PIECE_NAME_NO_DOLLAR}';
    window.objktKidlispCodes = ${JSON.stringify(kidlispSources)};
    window.acPREFILL_CODE_CACHE = ${JSON.stringify(kidlispSources)};
    const originalAppendChild = Element.prototype.appendChild;
    Element.prototype.appendChild = function(child) {
      if (child.tagName === 'LINK' && child.rel === 'stylesheet' && child.href && child.href.includes('.css')) return child;
      return originalAppendChild.call(this, child);
    };
    const originalBodyAppend = HTMLBodyElement.prototype.append;
    HTMLBodyElement.prototype.append = function(...nodes) {
      return originalBodyAppend.call(this, ...nodes.filter(n => !(n.tagName === 'LINK' && n.rel === 'stylesheet')));
    };
    window.VFS_BLOB_URLS = {};
    window.modulePaths = [];
    Object.entries(window.VFS).forEach(([path, file]) => {
      if (path.endsWith('.mjs') || path.endsWith('.js')) {
        const blob = new Blob([file.content], { type: 'application/javascript' });
        const blobUrl = URL.createObjectURL(blob);
        window.VFS_BLOB_URLS[path] = blobUrl;
        window.modulePaths.push(path);
      }
    });
    const importMapEntries = {};
    for (const fp of window.modulePaths) {
      if (window.VFS_BLOB_URLS[fp]) {
        importMapEntries[fp] = window.VFS_BLOB_URLS[fp];
        importMapEntries['/' + fp] = window.VFS_BLOB_URLS[fp];
        importMapEntries[\`./\${fp}\`] = window.VFS_BLOB_URLS[fp];
      }
    }
    const importMap = { imports: importMapEntries };
    const importMapScript = document.createElement('script');
    importMapScript.type = 'importmap';
    importMapScript.textContent = JSON.stringify(importMap);
    document.head.appendChild(importMapScript);
    const originalFetch = window.fetch;
    window.fetch = function(url, options) {
      const urlStr = typeof url === 'string' ? url : url.toString();
      if (urlStr.includes('/api/painting-code')) {
        const m = urlStr.match(/[?&]code=([^&]+)/);
        if (m) {
          const info = window.acPAINTING_CODE_MAP[m[1]];
          if (info) return Promise.resolve(new Response(JSON.stringify({ code: info.code, handle: info.handle, slug: info.slug }), { status: 200, headers: { 'Content-Type': 'application/json' } }));
        }
        return Promise.resolve(new Response(JSON.stringify({ error: 'Not found' }), { status: 404 }));
      }
      let vfsPath = decodeURIComponent(urlStr).replace(/^https?:\\/\\/[^\\/]+\\//g, '').replace(/^aesthetic\\.computer\\//g, '').replace(/#.*$/g, '').replace(/\\?.*$/g, '');
      vfsPath = vfsPath.replace(/^\\.\\.\\/+/g, '').replace(/^\\.\\//g, '').replace(/^\\//g, '').replace(/^aesthetic\\.computer\\//g, '');
      if (urlStr.includes('/media/') && urlStr.includes('/painting/')) {
        for (const [code, info] of Object.entries(window.acPAINTING_CODE_MAP || {})) {
          if (urlStr.includes(info.slug)) {
            const pvfs = 'paintings/' + code + '.png';
            if (window.VFS[pvfs]) {
              const f = window.VFS[pvfs]; const bs = atob(f.content); const bytes = new Uint8Array(bs.length);
              for (let i = 0; i < bs.length; i++) bytes[i] = bs.charCodeAt(i);
              return Promise.resolve(new Response(bytes, { status: 200, headers: { 'Content-Type': 'image/png' } }));
            }
          }
        }
      }
      if (window.VFS[vfsPath]) {
        const file = window.VFS[vfsPath]; let content; let ct = 'text/plain';
        if (file.binary) {
          const bs = atob(file.content); const bytes = new Uint8Array(bs.length);
          for (let i = 0; i < bs.length; i++) bytes[i] = bs.charCodeAt(i);
          content = bytes;
          if (file.type === 'png') ct = 'image/png'; else if (file.type === 'jpg' || file.type === 'jpeg') ct = 'image/jpeg';
        } else {
          content = file.content;
          if (file.type === 'mjs' || file.type === 'js') ct = 'application/javascript'; else if (file.type === 'json') ct = 'application/json';
        }
        return Promise.resolve(new Response(content, { status: 200, headers: { 'Content-Type': ct } }));
      }
      if (vfsPath.includes('disks/drawings/font_') || vfsPath.endsWith('.mjs') || vfsPath.includes('cursors/') || vfsPath.endsWith('.svg') || vfsPath.endsWith('.css') || urlStr.includes('/type/webfonts/')) {
        return Promise.resolve(new Response('', { status: 200, headers: { 'Content-Type': 'text/css' } }));
      }
      return originalFetch.call(this, url, options);
    };
    // Telemetry
    (function initTelemetry() {
      const origin = window.location?.origin || '';
      if (!origin.includes('aesthetic.computer') && !origin.includes('localhost')) return;
      const bootStart = performance.now();
      const sid = Math.random().toString(36).slice(2) + Date.now().toString(36);
      const tUrl = 'https://aesthetic.computer/api/bundle-telemetry';
      function send(type, data) {
        try {
          const body = JSON.stringify({ type, data: { sessionId: sid, piece: window.acPACK_PIECE || 'unknown', density: window.acPACK_DENSITY || 2, screenWidth: innerWidth, screenHeight: innerHeight, devicePixelRatio: devicePixelRatio || 1, userAgent: navigator.userAgent, ...data } });
          if (navigator.sendBeacon && navigator.sendBeacon(tUrl, body)) return;
          fetch(tUrl, { method: 'POST', body, headers: {'Content-Type':'application/json'}, keepalive: true }).catch(() => {});
        } catch {}
      }
      addEventListener('load', () => { send('boot', { bootTime: Math.round(performance.now() - bootStart), vfsFileCount: Object.keys(window.VFS || {}).length, blobUrlCount: Object.keys(window.VFS_BLOB_URLS || {}).length }); });
      let lastFT = performance.now(), fc = 0, sc = 0; const ps = [], ms = 60;
      function mf() {
        fc++; const now = performance.now();
        if (now - lastFT >= 1000) { const fps = fc; fc = 0; lastFT = now; if (sc < ms) { ps.push({t:Math.round(now-bootStart),fps}); sc++; if (sc%10===0) send('perf',{samples:ps.slice(-10)}); } }
        requestAnimationFrame(mf);
      }
      requestAnimationFrame(mf);
      addEventListener('error', (e) => { send('error', { message: e.message, filename: e.filename, lineno: e.lineno, colno: e.colno }); });
    })();
    (async function() {
      if (window.acDecodePaintingsPromise) await window.acDecodePaintingsPromise;
      import(window.VFS_BLOB_URLS['boot.mjs']).catch(err => { document.body.style.color='#fff'; document.body.textContent='Boot failed: '+err.message; });
    })();
  </script>
</body>
</html>`;
}

function generateJSPieceHTMLBundle(opts) {
  const { pieceName, files, packDate, packTime, gitVersion } = opts;

  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no">
  <title>${pieceName} · Aesthetic Computer</title>
  <style>
    body { margin: 0; padding: 0; overflow: hidden; }
    canvas { display: block; image-rendering: pixelated; }
  </style>
</head>
<body>
  <script type="module">
    window.acPACK_MODE = true;
    window.KIDLISP_SUPPRESS_SNAPSHOT_LOGS = true;
    window.__acKidlispConsoleEnabled = false;
    window.acSTARTING_PIECE = "${pieceName}";
    window.acPACK_PIECE = "${pieceName}";
    window.acPACK_DATE = "${packDate}";
    window.acPACK_GIT = "${gitVersion}";
    window.acPACK_COLOPHON = {
      piece: { name: '${pieceName}', isKidLisp: false },
      build: { author: '@jeffrey', packTime: ${packTime}, gitCommit: '${gitVersion}', gitIsDirty: false, fileCount: ${Object.keys(files).length} }
    };
    window.VFS = ${JSON.stringify(files).replace(/<\/script>/g, "<\\/script>")};
    const originalAppendChild = Element.prototype.appendChild;
    Element.prototype.appendChild = function(child) {
      if (child.tagName === 'LINK' && child.rel === 'stylesheet' && child.href && child.href.includes('.css')) return child;
      return originalAppendChild.call(this, child);
    };
    const originalBodyAppend = HTMLBodyElement.prototype.append;
    HTMLBodyElement.prototype.append = function(...nodes) {
      return originalBodyAppend.call(this, ...nodes.filter(n => !(n.tagName === 'LINK' && n.rel === 'stylesheet')));
    };
    window.VFS_BLOB_URLS = {};
    window.modulePaths = [];
    Object.entries(window.VFS).forEach(([path, file]) => {
      if (path.endsWith('.mjs') || path.endsWith('.js')) {
        const blob = new Blob([file.content], { type: 'application/javascript' });
        window.VFS_BLOB_URLS[path] = URL.createObjectURL(blob);
        window.modulePaths.push(path);
      }
    });
    const entries = {};
    for (const fp of window.modulePaths) {
      if (window.VFS_BLOB_URLS[fp]) {
        entries[fp] = window.VFS_BLOB_URLS[fp];
        entries['/' + fp] = window.VFS_BLOB_URLS[fp];
        entries[\`aesthetic.computer/\${fp}\`] = window.VFS_BLOB_URLS[fp];
        entries[\`/aesthetic.computer/\${fp}\`] = window.VFS_BLOB_URLS[fp];
        entries[\`./aesthetic.computer/\${fp}\`] = window.VFS_BLOB_URLS[fp];
        entries[\`https://aesthetic.computer/\${fp}\`] = window.VFS_BLOB_URLS[fp];
        entries[\`./\${fp}\`] = window.VFS_BLOB_URLS[fp];
        entries[\`../\${fp}\`] = window.VFS_BLOB_URLS[fp];
        entries[\`../../\${fp}\`] = window.VFS_BLOB_URLS[fp];
      }
    }
    const s = document.createElement('script');
    s.type = 'importmap';
    s.textContent = JSON.stringify({ imports: entries });
    document.head.appendChild(s);
    const originalFetch = window.fetch;
    window.fetch = function(url, options) {
      const urlStr = typeof url === 'string' ? url : url.toString();
      let vfsPath = decodeURIComponent(urlStr).replace(/^https?:\\/\\/[^\\/]+\\//g, '').replace(/^aesthetic\\.computer\\//g, '').replace(/#.*$/g, '').replace(/\\?.*$/g, '');
      vfsPath = vfsPath.replace(/^\\.\\.\\/+/g, '').replace(/^\\.\\//g, '').replace(/^\\//g, '').replace(/^aesthetic\\.computer\\//g, '');
      if (window.VFS[vfsPath]) {
        const file = window.VFS[vfsPath]; let content; let ct = 'text/plain';
        if (file.binary) {
          const bs = atob(file.content); const bytes = new Uint8Array(bs.length);
          for (let i = 0; i < bs.length; i++) bytes[i] = bs.charCodeAt(i);
          content = bytes;
          if (file.type === 'png') ct = 'image/png'; else if (file.type === 'jpg' || file.type === 'jpeg') ct = 'image/jpeg';
        } else {
          content = file.content;
          if (file.type === 'mjs' || file.type === 'js') ct = 'application/javascript'; else if (file.type === 'json') ct = 'application/json';
        }
        return Promise.resolve(new Response(content, { status: 200, headers: { 'Content-Type': ct } }));
      }
      if (vfsPath.includes('disks/drawings/font_') || vfsPath.includes('cursors/') || vfsPath.endsWith('.svg') || vfsPath.endsWith('.css') || urlStr.includes('/type/webfonts/')) {
        return Promise.resolve(new Response('', { status: 200, headers: { 'Content-Type': 'text/css' } }));
      }
      if (vfsPath.endsWith('.mjs')) console.warn('[VFS] Missing .mjs file:', vfsPath);
      return originalFetch.call(this, url, options);
    };
    (async function() {
      import(window.VFS_BLOB_URLS['boot.mjs']).catch(err => { document.body.style.color='#fff'; document.body.textContent='Boot failed: '+err.message; });
    })();
  </script>
</body>
</html>`;
}

// ─── Device mode (simple iframe wrapper) ────────────────────────────

export function generateDeviceHTML(pieceCode, density) {
  const densityParam = density ? `?density=${density}` : "";
  const pieceUrl = `https://aesthetic.computer/${pieceCode}${densityParam}`;
  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no">
  <title>${pieceCode} · Aesthetic Computer (Device)</title>
  <style>*{margin:0;padding:0;box-sizing:border-box}html,body{width:100%;height:100%;overflow:hidden;background:black}iframe{width:100%;height:100%;border:none}</style>
</head>
<body><iframe src="${pieceUrl}" allow="autoplay; fullscreen"></iframe></body>
</html>`;
}

// ─── Public API for server.mjs ──────────────────────────────────────

export async function prewarmCache() {
  const start = Date.now();
  refreshGitCommit();
  console.log(`[bundler] prewarming core bundle (commit ${GIT_COMMIT})...`);
  await getCoreBundle((p) => console.log(`[bundler] ${p.stage}: ${p.message}`), true);
  const elapsed = Date.now() - start;
  console.log(`[bundler] prewarm complete in ${elapsed}ms`);
  return { commit: GIT_COMMIT, elapsed, fileCount: Object.keys(coreBundleCache).length };
}

export function getCacheStatus() {
  return {
    cached: !!coreBundleCache,
    commit: coreBundleCacheCommit,
    fileCount: coreBundleCache ? Object.keys(coreBundleCache).length : 0,
    acSourceDir: AC_SOURCE_DIR,
    acSourceExists: fsSync.existsSync(AC_SOURCE_DIR),
  };
}

export { skipMinification };
export function setSkipMinification(val) { skipMinification = val; }
