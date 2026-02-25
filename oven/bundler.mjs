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
import { gzipSync, gunzipSync, brotliCompressSync, constants as zlibConstants } from "zlib";
import { execSync } from "child_process";
import { MongoClient } from "mongodb";
import sharp from "sharp";

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

const SKIP_FILES = [
  "dep/wasmboy/", // GameBoy emulator (~424 KB) — only needed if piece uses GB features
  "disks/prompt.mjs", // 377 KB — stubbed below (only FUNDING_MODE/FUNDING_SEVERITY needed)
  "disks/chat.mjs",   // 184 KB — stubbed below (chat UI, not functional in PACK_MODE)
];

// Lightweight stubs injected into VFS to satisfy static import chains
// without bundling the full 560+ KB of source.
// lib/chat.mjs imports { FUNDING_MODE } from "../disks/prompt.mjs"
// lib/disk.mjs dynamically imports "../disks/chat.mjs" (dead code: chatEnabled = false)
const VFS_STUBS = {
  "disks/prompt.mjs": `export const FUNDING_SEVERITY="off";export const FUNDING_MODE=false;`,
  "disks/chat.mjs": `export const CHAT_FONTS={};export function boot(){}export function paint(){}export function act(){}export function sim(){}export function receive(){}`,
};

// ─── Helpers ────────────────────────────────────────────────────────

// Parse BDF font file and extract glyphs for the given code point set.
// Returns a map of hex-padded keys (e.g. "0041") to glyph data objects.
function parseBDFGlyphs(bdfText, codePointSet) {
  const lines = bdfText.split(/\r?\n/);
  const map = {};
  let fontAscent = 8, fontDescent = 0;
  let inChar = false, inBitmap = false;
  let charCode = -1, bbx = null, dwidth = null, bitmapLines = [];

  for (const line of lines) {
    if (line.startsWith("FONT_ASCENT")) {
      fontAscent = parseInt(line.split(" ")[1]);
    } else if (line.startsWith("FONT_DESCENT")) {
      fontDescent = parseInt(line.split(" ")[1]);
    } else if (line.startsWith("STARTCHAR")) {
      inChar = true; inBitmap = false;
      charCode = -1; bbx = null; dwidth = null; bitmapLines = [];
    } else if (inChar && line.startsWith("ENCODING")) {
      charCode = parseInt(line.split(" ")[1]);
    } else if (inChar && line.startsWith("DWIDTH")) {
      const p = line.split(" ");
      dwidth = { x: parseInt(p[1]), y: parseInt(p[2]) };
    } else if (inChar && line.startsWith("BBX")) {
      const p = line.split(" ");
      bbx = { width: parseInt(p[1]), height: parseInt(p[2]), xOffset: parseInt(p[3]), yOffset: parseInt(p[4]) };
    } else if (inChar && line.trim() === "BITMAP") {
      inBitmap = true;
    } else if (inChar && line.startsWith("ENDCHAR")) {
      if (codePointSet.has(charCode) && bbx && bbx.width > 0 && bbx.height > 0) {
        // Parse bitmap into point commands
        const points = [];
        for (let r = 0; r < bbx.height && r < bitmapLines.length; r++) {
          let hex = bitmapLines[r].trim();
          if (hex.length % 2 !== 0) hex += "0";
          const bytes = Buffer.from(hex, "hex");
          for (let c = 0; c < bbx.width; c++) {
            const bi = Math.floor(c / 8);
            if (bi < bytes.length && (bytes[bi] >> (7 - (c % 8))) & 1) {
              points.push({ name: "point", args: [c, r] });
            }
          }
        }
        // Optimize consecutive points into line commands
        const commands = optimizeBDFPoints(points);
        const hexKey = charCode.toString(16).toUpperCase().padStart(4, "0");
        map[hexKey] = {
          resolution: [bbx.width, bbx.height],
          offset: [bbx.xOffset, bbx.yOffset],
          baselineOffset: [bbx.xOffset, fontAscent - bbx.height - bbx.yOffset],
          advance: dwidth ? dwidth.x : 4,
          bbx: { width: bbx.width, height: bbx.height, xOffset: bbx.xOffset, yOffset: bbx.yOffset },
          dwidth, fontMetrics: { ascent: fontAscent, descent: fontDescent },
          commands,
        };
      }
      inChar = false; inBitmap = false;
    } else if (inChar && inBitmap && bbx && bitmapLines.length < bbx.height) {
      bitmapLines.push(line.trim());
    }
  }
  return map;
}

// Merge adjacent horizontal/vertical points into line commands for smaller JSON.
function optimizeBDFPoints(points) {
  if (points.length === 0) return [];
  const unique = new Map();
  for (const p of points) {
    const k = `${p.args[0]},${p.args[1]}`;
    if (!unique.has(k)) unique.set(k, { x: p.args[0], y: p.args[1] });
  }
  const pts = Array.from(unique.values()).sort((a, b) => a.y !== b.y ? a.y - b.y : a.x - b.x);
  const used = new Set();
  const cmds = [];
  for (let i = 0; i < pts.length; i++) {
    if (used.has(i)) continue;
    const p = pts[i];
    // Try horizontal run
    const hRun = [i];
    for (let j = i + 1; j < pts.length; j++) {
      if (used.has(j) || pts[j].y !== p.y) break;
      if (pts[j].x === p.x + hRun.length) hRun.push(j); else break;
    }
    if (hRun.length >= 2) {
      cmds.push({ name: "line", args: [p.x, p.y, pts[hRun[hRun.length - 1]].x, p.y] });
      hRun.forEach(idx => used.add(idx));
      continue;
    }
    // Try vertical run
    const vRun = [i];
    for (let j = i + 1; j < pts.length; j++) {
      if (used.has(j)) continue;
      if (pts[j].x === p.x && pts[j].y === p.y + vRun.length) vRun.push(j);
    }
    if (vRun.length >= 2) {
      cmds.push({ name: "line", args: [p.x, p.y, p.x, pts[vRun[vRun.length - 1]].y] });
      vRun.forEach(idx => used.add(idx));
      continue;
    }
    cmds.push({ name: "point", args: [p.x, p.y] });
    used.add(i);
  }
  return cmds;
}

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
        drop_debugger: true,
        unused: true,
        passes: 3,
        pure_getters: "strict",
        toplevel: true,
      },
      mangle: true,
      module: true,
      toplevel: true,
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

  // BDF glyph maps — parse directly from BDF font files for comprehensive coverage
  onProgress({ stage: "glyphs", message: "Parsing BDF font files..." });
  const bdfGlyphs = {};
  const prodTypeDir = path.resolve(process.cwd(), "assets-type");
  const devTypeDir = path.resolve(acDir, "../assets/type");
  const assetsTypeDir = fsSync.existsSync(prodTypeDir) ? prodTypeDir : devTypeDir;

  // Character ranges to embed: ASCII printable + Latin-1 Supplement + common symbols
  const EMBED_RANGES = [
    [0x0020, 0x007E], // ASCII printable (95 chars)
    [0x00A0, 0x00FF], // Latin-1 Supplement (96 chars)
    [0x0100, 0x017F], // Latin Extended-A (128 chars)
    [0x2000, 0x206F], // General Punctuation (112 chars)
    [0x2190, 0x21FF], // Arrows (112 chars)
    [0x2500, 0x257F], // Box Drawing (128 chars)
    [0x25A0, 0x25FF], // Geometric Shapes (96 chars)
    [0x2600, 0x26FF], // Misc Symbols (256 chars)
  ];
  const embedSet = new Set();
  for (const [lo, hi] of EMBED_RANGES) {
    for (let cp = lo; cp <= hi; cp++) embedSet.add(cp);
  }

  for (const { fontName, bdfFile, key } of [
    { fontName: "MatrixChunky8", bdfFile: "MatrixChunky8.bdf", key: "MatrixChunky8" },
    { fontName: "unifont-16.0.03", bdfFile: "unifont-16.0.03.bdf", key: "unifont" },
  ]) {
    const bdfPath = path.join(assetsTypeDir, bdfFile);
    const bdfGzPath = bdfPath + ".gz";
    let bdfText = null;
    try {
      if (fsSync.existsSync(bdfPath)) {
        bdfText = fsSync.readFileSync(bdfPath, "utf8");
      } else if (fsSync.existsSync(bdfGzPath)) {
        bdfText = gunzipSync(fsSync.readFileSync(bdfGzPath)).toString("utf8");
      }
    } catch (err) {
      console.error(`[bundler] failed to read BDF ${bdfFile}:`, err.message);
    }
    if (!bdfText) {
      // Fallback: read pre-cached JSON files
      const fontDir = path.join(assetsTypeDir, fontName);
      if (fsSync.existsSync(fontDir)) {
        const map = {};
        try {
          for (const f of fsSync.readdirSync(fontDir).filter((n) => n.endsWith(".json"))) {
            const hex = f.replace(".json", "").toUpperCase();
            const paddedHex = hex.length < 4 ? hex.padStart(4, "0") : hex;
            map[paddedHex] = JSON.parse(await fs.readFile(path.join(fontDir, f), "utf8"));
          }
        } catch { /* skip */ }
        bdfGlyphs[key] = map;
        console.log(`[bundler] fallback: loaded ${Object.keys(map).length} ${key} glyphs from cache`);
      }
      continue;
    }

    const map = parseBDFGlyphs(bdfText, embedSet);
    bdfGlyphs[key] = map;
    console.log(`[bundler] parsed ${Object.keys(map).length} ${key} glyphs from BDF`);
  }
  coreFiles.__bdfGlyphs = bdfGlyphs;

  coreBundleCache = coreFiles;
  coreBundleCacheCommit = GIT_COMMIT;
  console.log(`[bundler] cached ${Object.keys(coreFiles).length} core files`);
  return coreFiles;
}

// ─── KidLisp bundle ─────────────────────────────────────────────────

export async function createBundle(pieceName, onProgress = () => {}, nocompress = false, density = null, brotli = false, noboxart = false) {
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

  // Inject lightweight stubs for skipped files
  for (const [stubPath, stubContent] of Object.entries(VFS_STUBS)) {
    files[stubPath] = { content: stubContent, binary: false, type: "mjs" };
  }

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

  // Extract BDF glyph maps (not part of VFS)
  const bdfGlyphs = files.__bdfGlyphs || {};
  delete files.__bdfGlyphs;

  const boxArtPNG = noboxart ? null : await generateBoxArtPNG(PIECE_NAME, authorHandle, bgColor).catch(() => null);

  const htmlContent = generateHTMLBundle({
    PIECE_NAME, PIECE_NAME_NO_DOLLAR, mainSource, kidlispSources,
    files, paintingData, authorHandle, packDate, packTime,
    gitVersion: GIT_COMMIT, filename, density, bgColor, bdfGlyphs, boxArtPNG,
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
    finalHtml = generateSelfExtractingBrotliHTML(PIECE_NAME, compressed.toString("base64"), bgColor, boxArtPNG);
  } else {
    const compressed = gzipSync(htmlBuf, { level: 9 });
    finalHtml = generateSelfExtractingHTML(PIECE_NAME, compressed.toString("base64"), bgColor, boxArtPNG);
  }

  return { html: finalHtml, filename, sizeKB: Math.round(finalHtml.length / 1024), mainSource, authorHandle, userCode, packDate, depCount };
}

// ─── JS piece bundle ────────────────────────────────────────────────

export async function createJSPieceBundle(pieceName, onProgress = () => {}, nocompress = false, density = null, brotli = false, noboxart = false) {
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

  // Inject lightweight stubs for skipped files
  for (const [stubPath, stubContent] of Object.entries(VFS_STUBS)) {
    files[stubPath] = { content: stubContent, binary: false, type: "mjs" };
  }

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

  // Extract BDF glyph maps (not part of VFS)
  const bdfGlyphs = files.__bdfGlyphs || {};
  delete files.__bdfGlyphs;

  const boxArtPNG = noboxart ? null : await generateBoxArtPNG(pieceName, null, null).catch(() => null);

  const htmlContent = generateJSPieceHTMLBundle({ pieceName, files, packDate, packTime, gitVersion: GIT_COMMIT, bdfGlyphs, boxArtPNG });
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
    finalHtml = generateSelfExtractingBrotliHTML(pieceName, compressed.toString("base64"), null, boxArtPNG);
  } else {
    const compressed = gzipSync(htmlBuf, { level: 9 });
    finalHtml = generateSelfExtractingHTML(pieceName, compressed.toString("base64"), null, boxArtPNG);
  }

  return { html: finalHtml, filename, sizeKB: Math.round(finalHtml.length / 1024) };
}

// ─── M4D (Max for Live) ─────────────────────────────────────────────

const M4L_HEADER_INSTRUMENT = Buffer.from(
  "ampf\x04\x00\x00\x00iiiimeta\x04\x00\x00\x00\x00\x00\x00\x00ptch", "binary"
);

function generateM4DPatcher(pieceName, html, width = 400, height = 200) {
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
        { box: { disablefind: 0, id: "obj-jweb", latency: 0, maxclass: "jweb~", numinlets: 1, numoutlets: 3, outlettype: ["signal","signal",""], patching_rect: [10,50,width,height], presentation: 1, presentation_rect: [0,0,width+1,height+1], rendermode: 1, url: "" } },
        { box: { id: "obj-loadbang", maxclass: "newobj", numinlets: 1, numoutlets: 1, outlettype: ["bang"], patching_rect: [10,10,58,22], text: "loadbang" } },
        { box: { id: "obj-loadhtml", maxclass: "message", numinlets: 2, numoutlets: 1, outlettype: [""], patching_rect: [10,30,400,22], text: `loadhtml ${html}` } },
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
        { patchline: { destination: ["obj-loadhtml",0], source: ["obj-loadbang",0] } },
        { patchline: { destination: ["obj-jweb",0], source: ["obj-loadhtml",0] } },
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

  const patcher = generateM4DPatcher(pieceName, bundleResult.html);

  onProgress({ stage: "compress", message: "Packing .amxd binary..." });

  const binary = packAMXD(patcher);
  const filename = `AC ${pieceName} (offline).amxd`;

  return { binary, filename, sizeKB: Math.round(binary.length / 1024) };
}

// ─── Self-extracting HTML wrapper ───────────────────────────────────

function generateSelfExtractingHTML(title, gzipBase64, bgColor = null, boxArtPNG = null) {
  const bgRule = bgColor ? `background:${bgColor};` : "";
  const boxArtTag = boxArtPNG
    ? `<img id="ac-box-art" src="data:image/png;base64,${boxArtPNG}" alt="${title}" style="position:fixed;inset:0;width:100%;height:100%;object-fit:cover;pointer-events:none;">`
    : "";
  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>${title} · Aesthetic Computer</title>
  <style>body{margin:0;${bgRule}overflow:hidden}</style>
</head>
<body>
  ${boxArtTag}
  <script>
    // Use blob: URL instead of data: URL for CSP compatibility (objkt sandboxing)
    const b64='${gzipBase64}';
    const bin=atob(b64);
    const bytes=new Uint8Array(bin.length);
    for(let i=0;i<bin.length;i++)bytes[i]=bin.charCodeAt(i);
    const blob=new Blob([bytes],{type:'application/gzip'});
    const url=URL.createObjectURL(blob);
    fetch(url)
      .then(r=>r.blob())
      .then(b=>b.stream().pipeThrough(new DecompressionStream('gzip')))
      .then(s=>new Response(s).text())
      .then(h=>{URL.revokeObjectURL(url);document.open();document.write(h);document.close();})
      .catch(e=>{document.body.style.cssText='color:#fff;background:#000;padding:20px;font-family:monospace';document.body.textContent='Bundle error: '+e.message;});
  </script>
</body>
</html>`;
}

function generateSelfExtractingBrotliHTML(title, brotliBase64, bgColor = null, boxArtPNG = null) {
  if (!brotliWasmGzBase64) throw new Error("Brotli WASM decoder not loaded");
  const bgRule = bgColor ? `background:${bgColor};` : "";
  const boxArtTag = boxArtPNG
    ? `<img id="ac-box-art" src="data:image/png;base64,${boxArtPNG}" alt="${title}" style="position:fixed;inset:0;width:100%;height:100%;object-fit:cover;pointer-events:none;">`
    : "";
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
  ${boxArtTag}
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
  })().catch(e=>{document.body.style.cssText='color:#fff;background:#000;padding:20px;font-family:monospace';document.body.textContent='Bundle error: '+e.message;});<\/script>
</body>
</html>`;
}

// ─── Box art generation ──────────────────────────────────────────────

function escapeXml(str) {
  return String(str)
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&apos;");
}

function boxArtTextColor(bgColor) {
  if (!bgColor) return "white";
  try {
    const hex = bgColor.match(/^#([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})$/i);
    if (hex) {
      const lum = (0.299 * parseInt(hex[1], 16) + 0.587 * parseInt(hex[2], 16) + 0.114 * parseInt(hex[3], 16)) / 255;
      return lum > 0.55 ? "#111111" : "white";
    }
    const rgb = bgColor.match(/rgb\((\d+),\s*(\d+),\s*(\d+)\)/i);
    if (rgb) {
      const lum = (0.299 * +rgb[1] + 0.587 * +rgb[2] + 0.114 * +rgb[3]) / 255;
      return lum > 0.55 ? "#111111" : "white";
    }
  } catch { /* ignore */ }
  return "white";
}

async function generateBoxArtPNG(pieceName, authorHandle, bgColor) {
  // AC theme: deep purple → purple gradient, hot pink accents, white type
  const W = 800, H = 1000; // portrait 4:5

  const display = pieceName.startsWith("$") ? pieceName.slice(1) : pieceName;
  const len = display.length;
  const namePx = len <= 5 ? 136 : len <= 8 ? 110 : len <= 12 ? 86 : len <= 16 ? 66 : len <= 22 ? 50 : 38;
  const author = authorHandle && authorHandle !== "unknown" ? escapeXml(authorHandle) : "";
  const nameY = H / 2 + (author ? -namePx * 0.55 : 0);
  const authorY = nameY + namePx * 0.65 + 52;

  const svg = `<svg xmlns="http://www.w3.org/2000/svg" width="${W}" height="${H}" viewBox="0 0 ${W} ${H}">
  <defs>
    <linearGradient id="bg" x1="0" y1="0" x2="0.4" y2="1">
      <stop offset="0%" stop-color="#1a0635"/>
      <stop offset="100%" stop-color="#3b0d72"/>
    </linearGradient>
  </defs>
  <rect width="${W}" height="${H}" fill="url(#bg)"/>
  <rect x="0" y="88" width="${W}" height="7" fill="#ff64c8"/>
  <rect x="0" y="${H - 95}" width="${W}" height="7" fill="#ff64c8"/>
  <text x="${W / 2}" y="${nameY}"
    font-family="'Courier New',Courier,monospace"
    font-size="${namePx}" font-weight="bold"
    fill="#ffffff" text-anchor="middle" dominant-baseline="middle">${escapeXml(display)}</text>
  ${author ? `<text x="${W / 2}" y="${authorY}"
    font-family="'Courier New',Courier,monospace"
    font-size="38" fill="#ff64c8"
    text-anchor="middle" dominant-baseline="middle">${author}</text>` : ""}
  <text x="${W / 2}" y="${H - 46}"
    font-family="'Courier New',Courier,monospace"
    font-size="22" letter-spacing="4"
    fill="rgba(255,100,200,0.55)" text-anchor="middle" dominant-baseline="middle">aesthetic.computer</text>
</svg>`;

  const buf = await sharp(Buffer.from(svg)).png({ compressionLevel: 9 }).toBuffer();
  return buf.toString("base64");
}

// ─── HTML templates ─────────────────────────────────────────────────
// These are large template literals — kept as-is from the Netlify version.

function generateHTMLBundle(opts) {
  const {
    PIECE_NAME, PIECE_NAME_NO_DOLLAR, mainSource, kidlispSources,
    files, paintingData, authorHandle, packDate, packTime, gitVersion, filename, density, bgColor, bdfGlyphs, boxArtPNG,
  } = opts;

  const bgRule = bgColor ? `background: ${bgColor}; ` : "";
  const boxArtImg = boxArtPNG
    ? `<img id="ac-box-art" src="data:image/png;base64,${boxArtPNG}" alt="${PIECE_NAME}">`
    : "";

  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no">
  <title>${PIECE_NAME} · Aesthetic Computer</title>
  <style>
    body { margin: 0; padding: 0; ${bgRule}overflow: hidden; }
    canvas { display: block; image-rendering: pixelated; }
    #ac-box-art { position: fixed; inset: 0; width: 100%; height: 100%; object-fit: cover; object-position: center; pointer-events: none; }
  </style>
</head>
<body>
  ${boxArtImg}
  <script>
    // Phase 1: Setup VFS, blob URLs, import map, and fetch interception.
    // This MUST run in a regular <script> (not type="module") so the import map
    // is in the DOM BEFORE any <script type="module"> executes.
    var _ba = document.getElementById('ac-box-art'); if (_ba) _ba.style.display = 'none';
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
    window.acBUNDLED_GLYPHS = ${JSON.stringify(bdfGlyphs)};
    window.acOBJKT_MATRIX_CHUNKY_GLYPHS = window.acBUNDLED_GLYPHS.MatrixChunky8 || {};
    window.acEMBEDDED_PAINTING_BITMAPS = {};
    window.acPAINTING_BITMAPS_READY = false;
    function decodePaintingToBitmap(code, base64Data) {
      return new Promise(function(resolve, reject) {
        var img = new Image();
        img.onload = function() {
          var canvas = document.createElement('canvas');
          canvas.width = img.width; canvas.height = img.height;
          var ctx = canvas.getContext('2d');
          ctx.drawImage(img, 0, 0);
          var imageData = ctx.getImageData(0, 0, canvas.width, canvas.height);
          resolve({ width: imageData.width, height: imageData.height, pixels: imageData.data });
        };
        img.onerror = reject;
        img.src = 'data:image/png;base64,' + base64Data;
      });
    }
    window.acDecodePaintingsPromise = (function() {
      var promises = [];
      var map = window.acPAINTING_CODE_MAP || {};
      var codes = Object.keys(map);
      for (var i = 0; i < codes.length; i++) {
        var code = codes[i];
        var vfsPath = 'paintings/' + code + '.png';
        if (window.VFS && window.VFS[vfsPath]) {
          promises.push(decodePaintingToBitmap(code, window.VFS[vfsPath].content).then(function(c) { return function(bitmap) {
            window.acEMBEDDED_PAINTING_BITMAPS['#' + c] = bitmap;
            window.acEMBEDDED_PAINTING_BITMAPS[c] = bitmap;
          }; }(code)).catch(function() {}));
        }
      }
      return Promise.all(promises).then(function() { window.acPAINTING_BITMAPS_READY = true; });
    })();
    window.EMBEDDED_KIDLISP_SOURCE = ${JSON.stringify(mainSource)};
    window.EMBEDDED_KIDLISP_PIECE = '${PIECE_NAME_NO_DOLLAR}';
    window.objktKidlispCodes = ${JSON.stringify(kidlispSources)};
    window.acPREFILL_CODE_CACHE = ${JSON.stringify(kidlispSources)};
    var originalAppendChild = Element.prototype.appendChild;
    Element.prototype.appendChild = function(child) {
      if (child.tagName === 'LINK' && child.rel === 'stylesheet' && child.href && child.href.includes('.css')) return child;
      return originalAppendChild.call(this, child);
    };
    var originalBodyAppend = HTMLBodyElement.prototype.append;
    HTMLBodyElement.prototype.append = function() {
      var args = []; for (var i = 0; i < arguments.length; i++) { var n = arguments[i]; if (!(n.tagName === 'LINK' && n.rel === 'stylesheet')) args.push(n); }
      return originalBodyAppend.apply(this, args);
    };
    window.VFS_BLOB_URLS = {};
    window.modulePaths = [];
    Object.entries(window.VFS).forEach(function(entry) {
      var path = entry[0], file = entry[1];
      if (path.endsWith('.mjs') || path.endsWith('.js')) {
        var blob = new Blob([file.content], { type: 'application/javascript' });
        window.VFS_BLOB_URLS[path] = URL.createObjectURL(blob);
        window.modulePaths.push(path);
      }
    });
    var importMapEntries = {};
    for (var i = 0; i < window.modulePaths.length; i++) {
      var fp = window.modulePaths[i];
      if (window.VFS_BLOB_URLS[fp]) {
        importMapEntries[fp] = window.VFS_BLOB_URLS[fp];
        importMapEntries['/' + fp] = window.VFS_BLOB_URLS[fp];
        importMapEntries['./' + fp] = window.VFS_BLOB_URLS[fp];
      }
    }
    var importMapScript = document.createElement('script');
    importMapScript.type = 'importmap';
    importMapScript.textContent = JSON.stringify({ imports: importMapEntries });
    document.head.appendChild(importMapScript);
    var originalFetch = window.fetch;
    window.fetch = function(url, options) {
      var urlStr = typeof url === 'string' ? url : url.toString();
      if (urlStr.includes('/api/')) {
        if (urlStr.includes('/api/bdf-glyph') && window.acBUNDLED_GLYPHS) {
          var fontM = urlStr.match(/[?&]font=([^&]+)/);
          var charsM = urlStr.match(/[?&]chars?=([^&]+)/);
          var fontKey = fontM ? decodeURIComponent(fontM[1]) : 'unifont';
          if (fontKey === 'unifont-16.0.03') fontKey = 'unifont';
          var fontMap = window.acBUNDLED_GLYPHS[fontKey] || {};
          var glyphs = {};
          if (charsM) { for (var c of charsM[1].split(',')) { var hex = c.trim().toUpperCase(); if (fontMap[hex]) glyphs[c.trim()] = fontMap[hex]; } }
          return Promise.resolve(new Response(JSON.stringify({ glyphs: glyphs }), { status: 200, headers: { 'Content-Type': 'application/json' } }));
        }
        if (urlStr.includes('/api/painting-code')) {
          var m = urlStr.match(/[?&]code=([^&]+)/);
          if (m) {
            var info = window.acPAINTING_CODE_MAP[m[1]];
            if (info) return Promise.resolve(new Response(JSON.stringify({ code: info.code, handle: info.handle, slug: info.slug }), { status: 200, headers: { 'Content-Type': 'application/json' } }));
          }
          return Promise.resolve(new Response(JSON.stringify({ error: 'Not found' }), { status: 404 }));
        }
        return Promise.resolve(new Response('{}', { status: 200, headers: { 'Content-Type': 'application/json' } }));
      }
      var vfsPath = decodeURIComponent(urlStr).replace(/^https?:\\/\\/[^\\/]+\\//g, '').replace(/^aesthetic\\.computer\\//g, '').replace(/#.*$/g, '').replace(/\\?.*$/g, '');
      vfsPath = vfsPath.replace(/^\\.\\.\\/+/g, '').replace(/^\\.\\//g, '').replace(/^\\//g, '').replace(/^aesthetic\\.computer\\//g, '');
      if (urlStr.includes('/media/') && urlStr.includes('/painting/')) {
        var paintKeys = Object.keys(window.acPAINTING_CODE_MAP || {});
        for (var pi = 0; pi < paintKeys.length; pi++) {
          var pcode = paintKeys[pi], pinfo = window.acPAINTING_CODE_MAP[pcode];
          if (urlStr.includes(pinfo.slug)) {
            var pvfs = 'paintings/' + pcode + '.png';
            if (window.VFS[pvfs]) {
              var f = window.VFS[pvfs]; var bs = atob(f.content); var bytes = new Uint8Array(bs.length);
              for (var j = 0; j < bs.length; j++) bytes[j] = bs.charCodeAt(j);
              return Promise.resolve(new Response(bytes, { status: 200, headers: { 'Content-Type': 'image/png' } }));
            }
          }
        }
      }
      if (window.VFS[vfsPath]) {
        var file = window.VFS[vfsPath]; var content; var ct = 'text/plain';
        if (file.binary) {
          var bs2 = atob(file.content); var bytes2 = new Uint8Array(bs2.length);
          for (var j2 = 0; j2 < bs2.length; j2++) bytes2[j2] = bs2.charCodeAt(j2);
          content = bytes2;
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
      var origin = window.location ? window.location.origin : '';
      if (origin.indexOf('aesthetic.computer') < 0 && origin.indexOf('localhost') < 0) return;
      var bootStart = performance.now();
      var sid = Math.random().toString(36).slice(2) + Date.now().toString(36);
      var tUrl = 'https://aesthetic.computer/api/bundle-telemetry';
      function send(type, data) {
        try {
          var body = JSON.stringify({ type: type, data: Object.assign({ sessionId: sid, piece: window.acPACK_PIECE || 'unknown', density: window.acPACK_DENSITY || 2, screenWidth: innerWidth, screenHeight: innerHeight, devicePixelRatio: devicePixelRatio || 1, userAgent: navigator.userAgent }, data) });
          if (navigator.sendBeacon && navigator.sendBeacon(tUrl, body)) return;
          fetch(tUrl, { method: 'POST', body: body, headers: {'Content-Type':'application/json'}, keepalive: true }).catch(function(){});
        } catch(e) {}
      }
      addEventListener('load', function() { send('boot', { bootTime: Math.round(performance.now() - bootStart), vfsFileCount: Object.keys(window.VFS || {}).length, blobUrlCount: Object.keys(window.VFS_BLOB_URLS || {}).length }); });
      var lastFT = performance.now(), fc = 0, sc = 0, ps = [], ms = 60;
      function mf() {
        fc++; var now = performance.now();
        if (now - lastFT >= 1000) { var fps = fc; fc = 0; lastFT = now; if (sc < ms) { ps.push({t:Math.round(now-bootStart),fps:fps}); sc++; if (sc%10===0) send('perf',{samples:ps.slice(-10)}); } }
        requestAnimationFrame(mf);
      }
      requestAnimationFrame(mf);
      addEventListener('error', function(e) { send('error', { message: e.message, filename: e.filename, lineno: e.lineno, colno: e.colno }); });
    })();
  </script>
  <script type="module">
    // Phase 2: Boot the app. Import map is already in the DOM from Phase 1.
    // Top-level await is valid in module scripts.
    if (window.acDecodePaintingsPromise) await window.acDecodePaintingsPromise;
    try {
      await import(window.VFS_BLOB_URLS['boot.mjs']);
    } catch (err) {
      document.body.style.cssText='color:#fff;background:#000;padding:20px;font-family:monospace';
      document.body.textContent='Boot failed: '+err.message;
    }
  </script>
</body>
</html>`;
}

function generateJSPieceHTMLBundle(opts) {
  const { pieceName, files, packDate, packTime, gitVersion, bdfGlyphs, boxArtPNG } = opts;

  const boxArtImg = boxArtPNG
    ? `<img id="ac-box-art" src="data:image/png;base64,${boxArtPNG}" alt="${pieceName}">`
    : "";

  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no">
  <title>${pieceName} · Aesthetic Computer</title>
  <style>
    body { margin: 0; padding: 0; overflow: hidden; }
    canvas { display: block; image-rendering: pixelated; }
    #ac-box-art { position: fixed; inset: 0; width: 100%; height: 100%; object-fit: cover; object-position: center; pointer-events: none; }
  </style>
</head>
<body>
  ${boxArtImg}
  <script>
    // Phase 1: Setup VFS, blob URLs, import map, and fetch interception.
    // This MUST run in a regular <script> (not type="module") so the import map
    // is in the DOM BEFORE any <script type="module"> executes.
    var _ba = document.getElementById('ac-box-art'); if (_ba) _ba.style.display = 'none';
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
    window.acBUNDLED_GLYPHS = ${JSON.stringify(bdfGlyphs)};
    window.acOBJKT_MATRIX_CHUNKY_GLYPHS = window.acBUNDLED_GLYPHS.MatrixChunky8 || {};
    var originalAppendChild = Element.prototype.appendChild;
    Element.prototype.appendChild = function(child) {
      if (child.tagName === 'LINK' && child.rel === 'stylesheet' && child.href && child.href.includes('.css')) return child;
      return originalAppendChild.call(this, child);
    };
    var originalBodyAppend = HTMLBodyElement.prototype.append;
    HTMLBodyElement.prototype.append = function() {
      var args = []; for (var i = 0; i < arguments.length; i++) { var n = arguments[i]; if (!(n.tagName === 'LINK' && n.rel === 'stylesheet')) args.push(n); }
      return originalBodyAppend.apply(this, args);
    };
    window.VFS_BLOB_URLS = {};
    window.modulePaths = [];
    Object.entries(window.VFS).forEach(function(entry) {
      var path = entry[0], file = entry[1];
      if (path.endsWith('.mjs') || path.endsWith('.js')) {
        var blob = new Blob([file.content], { type: 'application/javascript' });
        window.VFS_BLOB_URLS[path] = URL.createObjectURL(blob);
        window.modulePaths.push(path);
      }
    });
    var entries = {};
    for (var i = 0; i < window.modulePaths.length; i++) {
      var fp = window.modulePaths[i];
      if (window.VFS_BLOB_URLS[fp]) {
        entries[fp] = window.VFS_BLOB_URLS[fp];
        entries['/' + fp] = window.VFS_BLOB_URLS[fp];
        entries['aesthetic.computer/' + fp] = window.VFS_BLOB_URLS[fp];
        entries['/aesthetic.computer/' + fp] = window.VFS_BLOB_URLS[fp];
        entries['./aesthetic.computer/' + fp] = window.VFS_BLOB_URLS[fp];
        entries['https://aesthetic.computer/' + fp] = window.VFS_BLOB_URLS[fp];
        entries['./' + fp] = window.VFS_BLOB_URLS[fp];
        entries['../' + fp] = window.VFS_BLOB_URLS[fp];
        entries['../../' + fp] = window.VFS_BLOB_URLS[fp];
      }
    }
    var s = document.createElement('script');
    s.type = 'importmap';
    s.textContent = JSON.stringify({ imports: entries });
    document.head.appendChild(s);
    var originalFetch = window.fetch;
    window.fetch = function(url, options) {
      var urlStr = typeof url === 'string' ? url : url.toString();
      if (urlStr.includes('/api/')) {
        if (urlStr.includes('/api/bdf-glyph') && window.acBUNDLED_GLYPHS) {
          var fontM = urlStr.match(/[?&]font=([^&]+)/);
          var charsM = urlStr.match(/[?&]chars?=([^&]+)/);
          var fontKey = fontM ? decodeURIComponent(fontM[1]) : 'unifont';
          if (fontKey === 'unifont-16.0.03') fontKey = 'unifont';
          var fontMap = window.acBUNDLED_GLYPHS[fontKey] || {};
          var glyphs = {};
          if (charsM) { for (var c of charsM[1].split(',')) { var hex = c.trim().toUpperCase(); if (fontMap[hex]) glyphs[c.trim()] = fontMap[hex]; } }
          return Promise.resolve(new Response(JSON.stringify({ glyphs: glyphs }), { status: 200, headers: { 'Content-Type': 'application/json' } }));
        }
        return Promise.resolve(new Response('{}', { status: 200, headers: { 'Content-Type': 'application/json' } }));
      }
      var vfsPath = decodeURIComponent(urlStr).replace(/^https?:\\/\\/[^\\/]+\\//g, '').replace(/^aesthetic\\.computer\\//g, '').replace(/#.*$/g, '').replace(/\\?.*$/g, '');
      vfsPath = vfsPath.replace(/^\\.\\.\\/+/g, '').replace(/^\\.\\//g, '').replace(/^\\//g, '').replace(/^aesthetic\\.computer\\//g, '');
      if (window.VFS[vfsPath]) {
        var file = window.VFS[vfsPath]; var content; var ct = 'text/plain';
        if (file.binary) {
          var bs = atob(file.content); var bytes = new Uint8Array(bs.length);
          for (var j = 0; j < bs.length; j++) bytes[j] = bs.charCodeAt(j);
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
  </script>
  <script type="module">
    // Phase 2: Boot the app. The import map is already in the DOM from Phase 1.
    import(window.VFS_BLOB_URLS['boot.mjs']).catch(err => { document.body.style.cssText='color:#fff;background:#000;padding:20px;font-family:monospace'; document.body.textContent='Boot failed: '+err.message; });
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
